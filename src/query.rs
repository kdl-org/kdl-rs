use std::{collections::VecDeque, str::FromStr, sync::Arc};

use crate::{query_parser::KdlQueryParser, KdlDocument, KdlError, KdlNode, KdlValue};

#[derive(Debug, Clone, PartialEq)]
pub(crate) struct KdlQuery(pub(crate) Vec<KdlQuerySelector>);

impl FromStr for KdlQuery {
    type Err = KdlError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let parser = KdlQueryParser::new(s);
        parser.parse(crate::query_parser::query(&parser))
    }
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) struct KdlQuerySelector(pub(crate) Vec<KdlQuerySelectorSegment>);

impl KdlQuerySelector {
    fn matches(&self, crumb: Arc<Breadcrumb<'_>>, scope: &KdlDocument) -> bool {
        let scoped = self.0.first().map(|s| s.is_scope()).unwrap_or(false);

        if self.0.len() == 1 && scoped {
            // `scope()` matches if our node's immediate parent is the scope.
            return std::ptr::eq(scope, crumb.parent_doc);
        }

        if self.0.is_empty() {
            // I don't think this is possible, but just in case.
            return false;
        }

        let mut segments = self.0.iter().rev();
        let end = segments
            .next()
            .expect("This should've had at least one item.");

        if !end.matcher.matches(crumb.node) {
            // If the final segment doesn't even match the node, don't bother
            // looking any further.
            return false;
        } else if end.is_scope() && std::ptr::eq(scope, crumb.parent_doc) {
            // If the final segment is `scope()`, we're already at the top,
            // just see if this node's direct parent is the scope.
            return true;
        }

        let mut node = crumb.node;
        let mut next = crumb.next.clone();
        let mut parent_doc = crumb.parent_doc;
        'segments: for segment in segments {
            use KdlSegmentCombinator::*;
            match segment.op.as_ref().expect("This should've had an op.") {
                Child | Descendant => {
                    while let Some(crumb) = next.clone() {
                        if segment.matcher.matches(crumb.node) {
                            continue 'segments;
                        }

                        // We only loop once if the op is `Child`. Otherwise,
                        // we keep going up the tree!
                        if segment.op == Some(Child) {
                            break;
                        }

                        next = crumb.next.clone();
                        if let Some(crumb) = &next {
                            node = crumb.node;
                        }
                        parent_doc = crumb.parent_doc;
                    }

                    if segment.is_scope() && std::ptr::eq(scope, parent_doc) {
                        return true;
                    }

                    return false;
                }
                Neighbor | Sibling => {
                    for n in parent_doc
                        .nodes()
                        .iter()
                        .rev()
                        .skip_while(|n| !std::ptr::eq(*n, node))
                        .skip(1)
                    {
                        if segment.matcher.matches(n) {
                            node = n;
                            continue 'segments;
                        }
                        if segment.op == Some(Neighbor) {
                            break;
                        }
                    }

                    return false;
                }
            }
        }

        true
    }
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) struct KdlQuerySelectorSegment {
    pub(crate) op: Option<KdlSegmentCombinator>,
    pub(crate) matcher: KdlQueryMatcher,
}

impl KdlQuerySelectorSegment {
    fn is_scope(&self) -> bool {
        self.matcher.0.len() == 1 && self.matcher.0[0].accessor == KdlQueryMatcherAccessor::Scope
    }
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) enum KdlSegmentCombinator {
    Child,
    Descendant,
    Neighbor,
    Sibling,
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) struct KdlQueryMatcher(pub(crate) Vec<KdlQueryMatcherDetails>);

impl KdlQueryMatcher {
    pub(crate) fn matches(&self, node: &KdlNode) -> bool {
        self.0.iter().all(|m| m.matches(node))
    }
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) struct KdlQueryMatcherDetails {
    pub(crate) accessor: KdlQueryMatcherAccessor,
    pub(crate) op: KdlQueryAttributeOp,
    pub(crate) value: Option<KdlValue>,
}

impl KdlQueryMatcherDetails {
    pub(crate) fn matches(&self, node: &KdlNode) -> bool {
        use KdlQueryAttributeOp::*;
        use KdlQueryMatcherAccessor::*;

        match (&self.accessor, &self.op, &self.value) {
            (Scope, _, _) => false,
            (Tag | Node, op, Some(KdlValue::String(s) | KdlValue::RawString(s))) => {
                let lhs = match &self.accessor {
                    Tag => node.ty().map(|ty| ty.value()),
                    Node => Some(node.name().value()),
                    _ => unreachable!(),
                };
                let ss = Some(&s[..]);
                match op {
                    Equal => lhs == ss,
                    NotEqual => lhs != ss,
                    Gt => lhs > ss,
                    Gte => lhs >= ss,
                    Lt => lhs < ss,
                    Lte => lhs <= ss,
                    StartsWith => lhs.map(|lhs| lhs.starts_with(s)).unwrap_or(false),
                    EndsWith => lhs.map(|lhs| lhs.ends_with(s)).unwrap_or(false),
                    Contains => lhs.map(|lhs| lhs.contains(s)).unwrap_or(false),
                }
            }
            (Tag | Node, _op, Some(_)) => false,
            // I don't think this ever actually happens, but it should be just fine like this.
            (Tag | Node, _, None) => true,
            (Arg(_) | Prop(_), op, val @ Some(_)) => {
                let val = val.as_ref();
                let lhs = match &self.accessor {
                    Arg(Some(idx)) => node.get(*idx),
                    Arg(None) => node.get(0),
                    Prop(name) => node.get(&name[..]),
                    _ => unreachable!(),
                };
                match &op {
                    Equal => lhs == val,
                    NotEqual => lhs != val,
                    Gt => lhs > val,
                    Gte => lhs >= val,
                    Lt => lhs < val,
                    Lte => lhs <= val,
                    StartsWith | EndsWith | Contains => {
                        unreachable!("This should have been caught by the parser")
                    }
                }
            }
            (Arg(_) | Prop(_), _op, None) => match &self.accessor {
                Arg(Some(idx)) => node.get(*idx).is_some(),
                Arg(None) => node.get(0).is_some(),
                Prop(name) => node.get(&name[..]).is_some(),
                _ => unreachable!(),
            },
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) enum KdlQueryAttributeOp {
    Equal,
    NotEqual,
    Gt,
    Gte,
    Lt,
    Lte,
    StartsWith,
    EndsWith,
    Contains,
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) enum KdlQueryMatcherAccessor {
    Scope,
    Node,
    Tag,
    Arg(Option<usize>),
    Prop(String),
}
#[derive(Debug, Clone)]
struct Breadcrumb<'a> {
    node: &'a KdlNode,
    parent_doc: &'a KdlDocument,
    next: Option<Arc<Breadcrumb<'a>>>,
}

/// Iterator for results of a KDL query over a [`KdlDocument`].
#[derive(Debug, Clone)]
pub struct KdlQueryIterator<'a> {
    scope: Option<&'a KdlDocument>,
    query: KdlQuery,
    q: VecDeque<Arc<Breadcrumb<'a>>>,
}

impl<'a> KdlQueryIterator<'a> {
    pub(crate) fn new(scope: Option<&'a KdlDocument>, query: KdlQuery) -> Self {
        let mut q = VecDeque::new();
        if let Some(scope) = scope {
            for node in scope.nodes() {
                q.push_front(Arc::new(Breadcrumb {
                    node,
                    parent_doc: scope,
                    next: None,
                }));
            }
        }
        Self { scope, query, q }
    }
}

impl<'a> Iterator for KdlQueryIterator<'a> {
    type Item = &'a KdlNode;

    fn next(&mut self) -> Option<Self::Item> {
        let scope = self.scope?;

        while let Some(crumb) = self.q.pop_back() {
            if let Some(children) = crumb.node.children() {
                for node in children.nodes().iter().rev() {
                    self.q.push_back(Arc::new(Breadcrumb {
                        node,
                        parent_doc: children,
                        next: Some(crumb.clone()),
                    }));
                }
            }
            for selector in &self.query.0 {
                if selector.matches(crumb.clone(), scope) {
                    return Some(crumb.node);
                }
            }
        }

        // Otherwise, we're done! Just return None and the iterator's done.
        None
    }
}
