use std::{collections::VecDeque, str::FromStr, sync::Arc};

use crate::{query_parser::KdlQueryParser, KdlDocument, KdlError, KdlNode, KdlValue};

/// A parsed KQL query. For details on the syntax, see the [KQL
/// spec](https://github.com/kdl-org/kdl/blob/main/QUERY-SPEC.md).
#[derive(Debug, Clone, PartialEq)]
pub struct KdlQuery(pub(crate) Vec<KdlQuerySelector>);

impl FromStr for KdlQuery {
    type Err = KdlError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let parser = KdlQueryParser::new(s);
        parser.parse(crate::query_parser::query(&parser))
    }
}

/// A trait that tries to convert something into a [`KdlQuery`].
pub trait IntoKdlQuery: IntoQuerySealed {}

impl IntoKdlQuery for KdlQuery {}
impl IntoKdlQuery for String {}
impl<'a> IntoKdlQuery for &'a str {}
impl<'a> IntoKdlQuery for &'a String {}

#[doc(hidden)]
pub trait IntoQuerySealed {
    fn into_query(self) -> Result<KdlQuery, KdlError>;
}

impl IntoQuerySealed for KdlQuery {
    fn into_query(self) -> Result<KdlQuery, KdlError> {
        Ok(self)
    }
}

impl IntoQuerySealed for &str {
    fn into_query(self) -> Result<KdlQuery, KdlError> {
        self.parse()
    }
}

impl IntoQuerySealed for String {
    fn into_query(self) -> Result<KdlQuery, KdlError> {
        self.parse()
    }
}

impl IntoQuerySealed for &String {
    fn into_query(self) -> Result<KdlQuery, KdlError> {
        self.parse()
    }
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) struct KdlQuerySelector(pub(crate) Vec<KdlQuerySelectorSegment>);

impl KdlQuerySelector {
    fn matches(&self, crumb: Arc<Breadcrumb<'_>>, scope: Option<&KdlNode>) -> bool {
        if self.0.is_empty() {
            // I don't think this is possible, but just in case.
            return false;
        }

        let mut segments = self.0.iter().rev();
        let end = segments
            .next()
            .expect("This should've had at least one item.");

        if end.is_scope() {
            if let Some(scope) = &scope {
                if crumb.node == *scope {
                    return true;
                }
            } else if crumb.next.is_none() && scope.is_none() {
                return false;
            }
        }

        if !end.matcher.matches(crumb.node) {
            // If the final segment doesn't even match the node, don't bother
            // looking any further.
            return false;
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

                    if segment.is_scope() {
                        return next.map(|crumb| crumb.node) == scope;
                    }

                    return false;
                }
                Neighbor | Sibling => {
                    if let Some(parent) = &parent_doc {
                        for n in parent
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
            (Annotation | Node, op, Some(KdlValue::String(s) | KdlValue::RawString(s))) => {
                let lhs = match &self.accessor {
                    Annotation => node.ty().map(|ty| ty.value()),
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
            (Annotation | Node, _op, Some(_)) => false,
            // This is `()blah`.
            (Annotation, _, None) => node.ty().is_some(),
            // This is `[]`.
            (Node, _, None) => true,
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
    Annotation,
    Arg(Option<usize>),
    Prop(String),
}
#[derive(Debug, Clone)]
struct Breadcrumb<'a> {
    node: &'a KdlNode,
    parent_doc: Option<&'a KdlDocument>,
    next: Option<Arc<Breadcrumb<'a>>>,
}

/// Iterator for results of a KDL query over a [`KdlDocument`].
#[derive(Debug, Clone)]
pub struct KdlQueryIterator<'a> {
    scope: Option<&'a KdlNode>,
    query: KdlQuery,
    q: VecDeque<Arc<Breadcrumb<'a>>>,
}

impl<'a> KdlQueryIterator<'a> {
    pub(crate) fn new(
        scope: Option<&'a KdlNode>,
        ctx_doc: Option<&'a KdlDocument>,
        query: KdlQuery,
    ) -> Self {
        let mut q = VecDeque::new();
        if let Some(scope) = scope {
            q.push_back(Arc::new(Breadcrumb {
                node: scope,
                parent_doc: None,
                next: None,
            }));
        } else if let Some(doc) = ctx_doc {
            for node in doc.nodes() {
                q.push_front(Arc::new(Breadcrumb {
                    node,
                    parent_doc: Some(doc),
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
        while let Some(crumb) = self.q.pop_back() {
            if let Some(children) = crumb.node.children() {
                for node in children.nodes().iter().rev() {
                    self.q.push_back(Arc::new(Breadcrumb {
                        node,
                        parent_doc: Some(children),
                        next: Some(crumb.clone()),
                    }));
                }
            }
            for selector in &self.query.0 {
                if selector.matches(crumb.clone(), self.scope) {
                    return Some(crumb.node);
                }
            }
        }

        // Otherwise, we're done! Just return None and the iterator's done.
        None
    }
}
