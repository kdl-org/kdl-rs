use std::{
    collections::{HashSet, VecDeque},
    str::FromStr,
};

use crate::{query_parser::KdlQueryParser, KdlDocument, KdlError, KdlNode, KdlValue};

#[derive(Debug, Clone, PartialEq)]
pub(crate) struct KdlQuery(pub(crate) Vec<KdlQuerySelector>);

#[derive(Debug, Clone, PartialEq)]
pub(crate) struct KdlQuerySelector(pub(crate) Vec<KdlQuerySelectorSegment>);

#[derive(Debug, Clone, PartialEq)]
pub(crate) struct KdlQuerySelectorSegment {
    pub(crate) op: Option<KdlSegmentCombinator>,
    pub(crate) matcher: KdlQueryMatcher,
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

impl KdlQuery {
    pub(crate) fn run<'a>(&self, scopedoc: &'a KdlDocument, single: bool) -> Vec<&'a KdlNode> {
        let scope = KdlQuerySelectorSegment {
            op: None,
            matcher: KdlQueryMatcher(vec![KdlQueryMatcherDetails {
                accessor: KdlQueryMatcherAccessor::Scope,
                op: KdlQueryAttributeOp::Equal,
                value: None,
            }]),
        };
        let mut nodes = VecDeque::new();
        let mut nodes_hash = HashSet::new();
        let mut add_node = |node| {
            if !nodes_hash.contains(node) {
                nodes_hash.insert(node);
                nodes.push_front(node);
            }
        };
        let mut q = VecDeque::new();
        for selector in &self.0 {
            // (selectors, current_doc, parent_doc, target_node)
            q.push_back((&selector.0[..], scopedoc, None, None));
        }
        while let Some((selector, doc, _parent, node_idx)) = q.pop_back() {
            // Check for scope() special case
            if selector.first() == Some(&scope) && selector.len() == 1 {
                for node in doc.nodes().iter().rev() {
                    if single {
                        return vec![node];
                    } else {
                        add_node(node);
                    }
                }
            } else if selector.first() == Some(&scope) {
                q.push_back((&selector[1..], scopedoc, None, None));
            } else if selector.is_empty() {
                if let Some(idx) = node_idx {
                    let val = &doc.nodes()[idx];
                    if single {
                        return vec![val];
                    } else {
                        add_node(val);
                    }
                }
            } else if !selector.is_empty() {
                let segment = selector.first().expect("Selector shouldn't be empty?");
                let is_last = selector.len() == 1;
                use KdlSegmentCombinator::*;
                match segment.op {
                    Some(Child) | Some(Descendant) | None => {
                        for (idx, node) in doc.nodes().iter().enumerate() {
                            if segment.matcher.matches(node) {
                                if is_last {
                                    q.push_back((&selector[1..], doc, None, Some(idx)));
                                } else if let Some(newdoc) = node.children() {
                                    q.push_back((&selector[1..], newdoc, Some(doc), Some(idx)));
                                }
                            }
                            if segment.op != Some(Child) {
                                if let Some(newdoc) = node.children() {
                                    // We're looking for descendants. Keep
                                    // applying this selector to successive
                                    // children.
                                    q.push_back((selector, newdoc, Some(doc), Some(idx)));
                                }
                            }
                        }
                    }
                    Some(Neighbor) | Some(Sibling) => {
                        todo!()
                    }
                }
            }
        }

        nodes.into_iter().collect()
    }
}

impl FromStr for KdlQuery {
    type Err = KdlError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let parser = KdlQueryParser::new(s);
        parser.parse(crate::query_parser::query(&parser))
    }
}
