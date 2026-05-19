//! Serde deserializer for KDL documents.
//!
//! This module provides [`from_str`] for deserializing Rust types from KDL text.
//!
//! # KDL → Serde Data Model Mapping
//!
//! KDL documents are mapped to serde's data model as follows:
//!
//! - A **KDL document** is treated as a **map** (or struct), where each top-level
//!   node name is a key and the node's content is the value.
//! - A **KDL node with only a single argument** and no properties/children is
//!   treated as a **scalar value** (the argument itself).
//! - A **KDL node with multiple arguments** is treated as a **sequence** of those arguments.
//! - A **KDL node with properties** is treated as a **map** of property names to values.
//! - A **KDL node with children** is treated as a **map** (or struct), where child
//!   node names are keys.
//! - A **KDL node with both properties and children** merges them into a single map.
//! - **Repeated node names** at the same level are collected into a **sequence**.
//!
//! # Example
//!
//! ```rust
//! use serde::Deserialize;
//!
//! #[derive(Deserialize, Debug, PartialEq)]
//! struct Config {
//!     name: String,
//!     port: u16,
//! }
//!
//! let kdl = r#"
//! name "my-app"
//! port 8080
//! "#;
//!
//! let config: Config = kdl::de::from_str(kdl).unwrap();
//! assert_eq!(config, Config { name: "my-app".into(), port: 8080 });
//! ```

use std::borrow::Cow;
use std::fmt;

use serde::de::value::StringDeserializer;
use serde::de::{self, DeserializeSeed, Deserializer, MapAccess, SeqAccess, Visitor};
use serde::Deserialize;

use crate::{KdlDocument, KdlEntry, KdlNode, KdlValue};

/// Errors that can occur during KDL deserialization.
#[derive(Debug, Clone)]
pub struct Error {
    msg: String,
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.msg)
    }
}

impl std::error::Error for Error {}

impl de::Error for Error {
    fn custom<T: fmt::Display>(msg: T) -> Self {
        Error {
            msg: msg.to_string(),
        }
    }
}

impl From<crate::KdlError> for Error {
    fn from(e: crate::KdlError) -> Self {
        Error {
            msg: format!("{}", e),
        }
    }
}

/// Helper to produce a `StrDeserializer` with our error type.
fn str_deserializer(s: &str) -> de::value::StrDeserializer<'_, Error> {
    de::IntoDeserializer::into_deserializer(s)
}

/// Deserialize a type from a KDL string.
///
/// # Example
///
/// ```rust
/// use serde::Deserialize;
///
/// #[derive(Deserialize, Debug, PartialEq)]
/// struct Config {
///     name: String,
///     port: u16,
/// }
///
/// let kdl = r#"
/// name "my-app"
/// port 8080
/// "#;
///
/// let config: Config = kdl::de::from_str(kdl).unwrap();
/// assert_eq!(config, Config { name: "my-app".into(), port: 8080 });
/// ```
pub fn from_str<'a, T>(input: &'a str) -> Result<T, Error>
where
    T: Deserialize<'a>,
{
    let doc: KdlDocument = input.parse().map_err(Error::from)?;
    let de = DocumentDeserializer { doc: &doc };
    T::deserialize(de)
}

struct ValueDeserializer<'a> {
    value: &'a KdlValue,
}

impl<'de, 'a> de::Deserializer<'de> for ValueDeserializer<'a> {
    type Error = Error;

    fn deserialize_any<V: Visitor<'de>>(self, visitor: V) -> Result<V::Value, Self::Error> {
        match self.value {
            KdlValue::String(s) => visitor.visit_str(s),
            KdlValue::Integer(n) => {
                if let Ok(i) = i64::try_from(*n) {
                    visitor.visit_i64(i)
                } else if let Ok(u) = u64::try_from(*n) {
                    visitor.visit_u64(u)
                } else {
                    visitor.visit_i128(*n)
                }
            }
            KdlValue::Float(f) => visitor.visit_f64(*f),
            KdlValue::Bool(b) => visitor.visit_bool(*b),
            KdlValue::Null => visitor.visit_unit(),
        }
    }

    fn deserialize_option<V: Visitor<'de>>(self, visitor: V) -> Result<V::Value, Self::Error> {
        match self.value {
            KdlValue::Null => visitor.visit_none(),
            _ => visitor.visit_some(self),
        }
    }

    fn deserialize_newtype_struct<V: Visitor<'de>>(
        self,
        _name: &'static str,
        visitor: V,
    ) -> Result<V::Value, Self::Error> {
        visitor.visit_newtype_struct(self)
    }

    fn deserialize_enum<V: Visitor<'de>>(
        self,
        _name: &'static str,
        _variants: &'static [&'static str],
        visitor: V,
    ) -> Result<V::Value, Self::Error> {
        match self.value {
            KdlValue::String(s) => visitor.visit_enum(str_deserializer(s.as_str())),
            _ => Err(de::Error::custom("expected a string for unit enum variant")),
        }
    }

    fn deserialize_string<V: Visitor<'de>>(self, visitor: V) -> Result<V::Value, Self::Error> {
        match self.value {
            KdlValue::String(s) => visitor.visit_string(s.clone()),
            _ => self.deserialize_any(visitor),
        }
    }

    fn deserialize_str<V: Visitor<'de>>(self, visitor: V) -> Result<V::Value, Self::Error> {
        match self.value {
            KdlValue::String(s) => visitor.visit_str(s),
            _ => self.deserialize_any(visitor),
        }
    }

    serde::forward_to_deserialize_any! {
        bool i8 i16 i32 i64 i128 u8 u16 u32 u64 u128 f32 f64 char
        bytes byte_buf unit unit_struct seq tuple
        tuple_struct map struct identifier ignored_any
    }
}

struct DocumentDeserializer<'a> {
    doc: &'a KdlDocument,
}

impl<'de, 'a> de::Deserializer<'de> for DocumentDeserializer<'a> {
    type Error = Error;

    fn deserialize_any<V: Visitor<'de>>(self, visitor: V) -> Result<V::Value, Self::Error> {
        self.deserialize_map(visitor)
    }

    fn deserialize_map<V: Visitor<'de>>(self, visitor: V) -> Result<V::Value, Self::Error> {
        visitor.visit_map(DocumentMapAccess::new(self.doc))
    }

    fn deserialize_struct<V: Visitor<'de>>(
        self,
        _name: &'static str,
        _fields: &'static [&'static str],
        visitor: V,
    ) -> Result<V::Value, Self::Error> {
        self.deserialize_map(visitor)
    }

    fn deserialize_seq<V: Visitor<'de>>(self, visitor: V) -> Result<V::Value, Self::Error> {
        visitor.visit_seq(NodeListSeqAccess {
            iter: self.doc.nodes().iter(),
        })
    }

    fn deserialize_option<V: Visitor<'de>>(self, visitor: V) -> Result<V::Value, Self::Error> {
        if self.doc.nodes().is_empty() {
            visitor.visit_none()
        } else {
            visitor.visit_some(self)
        }
    }

    fn deserialize_newtype_struct<V: Visitor<'de>>(
        self,
        _name: &'static str,
        visitor: V,
    ) -> Result<V::Value, Self::Error> {
        visitor.visit_newtype_struct(self)
    }

    fn deserialize_unit<V: Visitor<'de>>(self, visitor: V) -> Result<V::Value, Self::Error> {
        visitor.visit_unit()
    }

    fn deserialize_unit_struct<V: Visitor<'de>>(
        self,
        _name: &'static str,
        visitor: V,
    ) -> Result<V::Value, Self::Error> {
        visitor.visit_unit()
    }

    fn deserialize_enum<V: Visitor<'de>>(
        self,
        _name: &'static str,
        _variants: &'static [&'static str],
        visitor: V,
    ) -> Result<V::Value, Self::Error> {
        // For enums at document level, treat as a single-node document where
        // the node name is the variant.
        let nodes = self.doc.nodes();
        if nodes.len() == 1 {
            visitor.visit_enum(NodeEnumAccess { node: &nodes[0] })
        } else {
            Err(de::Error::custom(
                "expected exactly one node for enum deserialization",
            ))
        }
    }

    serde::forward_to_deserialize_any! {
        bool i8 i16 i32
        i64 i128 u8 u16
        u32 u64 u128 f32
        f64 char str string
        bytes byte_buf tuple
        tuple_struct identifier ignored_any
    }
}

/// Groups nodes by name, producing deduplicated keys. Nodes that share a name
/// are collected into a Vec so they can be deserialized as sequences.
struct DocumentMapAccess<'a> {
    /// Ordered unique keys (node names in first-seen order).
    keys: Vec<&'a str>,

    /// All nodes grouped by name, preserving order within each group.
    groups: std::collections::HashMap<&'a str, Vec<&'a KdlNode>>,

    /// Current index into `keys`.
    idx: usize,
}

impl<'a> DocumentMapAccess<'a> {
    fn new(doc: &'a KdlDocument) -> Self {
        let mut keys = Vec::new();
        let mut groups: std::collections::HashMap<&'a str, Vec<&'a KdlNode>> =
            std::collections::HashMap::new();
        for node in doc.nodes() {
            let name = node.name().value();
            if !groups.contains_key(name) {
                keys.push(name);
            }
            groups.entry(name).or_default().push(node);
        }
        DocumentMapAccess {
            keys,
            groups,
            idx: 0,
        }
    }
}

impl<'de, 'a> MapAccess<'de> for DocumentMapAccess<'a> {
    type Error = Error;

    fn next_key_seed<K: DeserializeSeed<'de>>(
        &mut self,
        seed: K,
    ) -> Result<Option<K::Value>, Self::Error> {
        if self.idx >= self.keys.len() {
            return Ok(None);
        }
        let key = self.keys[self.idx];
        seed.deserialize(str_deserializer(key)).map(Some)
    }

    fn next_value_seed<V: DeserializeSeed<'de>>(
        &mut self,
        seed: V,
    ) -> Result<V::Value, Self::Error> {
        let key = self.keys[self.idx];
        self.idx += 1;
        let nodes = self.groups.get(key).unwrap();
        if nodes.len() == 1 {
            seed.deserialize(NodeDeserializer { node: nodes[0] })
        } else {
            // Multiple nodes with same name → sequence
            seed.deserialize(NodeGroupDeserializer { nodes })
        }
    }
}

struct NodeDeserializer<'a> {
    node: &'a KdlNode,
}

impl<'a> NodeDeserializer<'a> {
    fn args(&self) -> Vec<&'a KdlEntry> {
        self.node
            .entries()
            .iter()
            .filter(|e| e.name().is_none())
            .collect()
    }

    fn props(&self) -> Vec<&'a KdlEntry> {
        self.node
            .entries()
            .iter()
            .filter(|e| e.name().is_some())
            .collect()
    }

    /// Returns true if this node has only a single argument and nothing else.
    fn is_scalar(&self) -> bool {
        let args = self.args();
        let props = self.props();
        args.len() == 1 && props.is_empty() && self.node.children().is_none()
    }

    /// Returns true if this node is "empty" (no args, no props, no children).
    fn is_empty(&self) -> bool {
        self.node.entries().is_empty() && self.node.children().is_none()
    }
}

impl<'de, 'a> de::Deserializer<'de> for NodeDeserializer<'a> {
    type Error = Error;

    fn deserialize_any<V: Visitor<'de>>(self, visitor: V) -> Result<V::Value, Self::Error> {
        if self.is_empty() {
            // Node with no data → null/unit
            return visitor.visit_unit();
        }
        if self.is_scalar() {
            // Single argument → deserialize as scalar
            let entry = self.args()[0];
            return ValueDeserializer {
                value: entry.value(),
            }
            .deserialize_any(visitor);
        }

        let args = self.args();
        let props = self.props();
        let has_children = self.node.children().is_some();

        if !args.is_empty() && props.is_empty() && !has_children {
            // Only positional args → sequence
            return visitor.visit_seq(ArgSeqAccess {
                iter: args.into_iter(),
            });
        }

        // Has properties and/or children → map
        self.deserialize_map(visitor)
    }

    fn deserialize_bool<V: Visitor<'de>>(self, visitor: V) -> Result<V::Value, Self::Error> {
        if self.is_scalar() {
            ValueDeserializer {
                value: self.args()[0].value(),
            }
            .deserialize_any(visitor)
        } else {
            Err(de::Error::custom("expected a boolean value"))
        }
    }

    fn deserialize_i8<V: Visitor<'de>>(self, visitor: V) -> Result<V::Value, Self::Error> {
        self.deserialize_scalar(visitor)
    }
    fn deserialize_i16<V: Visitor<'de>>(self, visitor: V) -> Result<V::Value, Self::Error> {
        self.deserialize_scalar(visitor)
    }
    fn deserialize_i32<V: Visitor<'de>>(self, visitor: V) -> Result<V::Value, Self::Error> {
        self.deserialize_scalar(visitor)
    }
    fn deserialize_i64<V: Visitor<'de>>(self, visitor: V) -> Result<V::Value, Self::Error> {
        self.deserialize_scalar(visitor)
    }
    fn deserialize_i128<V: Visitor<'de>>(self, visitor: V) -> Result<V::Value, Self::Error> {
        self.deserialize_scalar(visitor)
    }
    fn deserialize_u8<V: Visitor<'de>>(self, visitor: V) -> Result<V::Value, Self::Error> {
        self.deserialize_scalar(visitor)
    }
    fn deserialize_u16<V: Visitor<'de>>(self, visitor: V) -> Result<V::Value, Self::Error> {
        self.deserialize_scalar(visitor)
    }
    fn deserialize_u32<V: Visitor<'de>>(self, visitor: V) -> Result<V::Value, Self::Error> {
        self.deserialize_scalar(visitor)
    }
    fn deserialize_u64<V: Visitor<'de>>(self, visitor: V) -> Result<V::Value, Self::Error> {
        self.deserialize_scalar(visitor)
    }
    fn deserialize_u128<V: Visitor<'de>>(self, visitor: V) -> Result<V::Value, Self::Error> {
        self.deserialize_scalar(visitor)
    }
    fn deserialize_f32<V: Visitor<'de>>(self, visitor: V) -> Result<V::Value, Self::Error> {
        self.deserialize_scalar(visitor)
    }
    fn deserialize_f64<V: Visitor<'de>>(self, visitor: V) -> Result<V::Value, Self::Error> {
        self.deserialize_scalar(visitor)
    }
    fn deserialize_char<V: Visitor<'de>>(self, visitor: V) -> Result<V::Value, Self::Error> {
        self.deserialize_scalar(visitor)
    }
    fn deserialize_str<V: Visitor<'de>>(self, visitor: V) -> Result<V::Value, Self::Error> {
        self.deserialize_scalar(visitor)
    }
    fn deserialize_string<V: Visitor<'de>>(self, visitor: V) -> Result<V::Value, Self::Error> {
        self.deserialize_scalar(visitor)
    }
    fn deserialize_bytes<V: Visitor<'de>>(self, visitor: V) -> Result<V::Value, Self::Error> {
        self.deserialize_scalar(visitor)
    }
    fn deserialize_byte_buf<V: Visitor<'de>>(self, visitor: V) -> Result<V::Value, Self::Error> {
        self.deserialize_scalar(visitor)
    }

    fn deserialize_option<V: Visitor<'de>>(self, visitor: V) -> Result<V::Value, Self::Error> {
        if self.is_empty() {
            visitor.visit_none()
        } else if self.is_scalar() {
            let val = self.args()[0].value();
            if matches!(val, KdlValue::Null) {
                visitor.visit_none()
            } else {
                visitor.visit_some(self)
            }
        } else {
            visitor.visit_some(self)
        }
    }

    fn deserialize_unit<V: Visitor<'de>>(self, visitor: V) -> Result<V::Value, Self::Error> {
        visitor.visit_unit()
    }

    fn deserialize_unit_struct<V: Visitor<'de>>(
        self,
        _name: &'static str,
        visitor: V,
    ) -> Result<V::Value, Self::Error> {
        visitor.visit_unit()
    }

    fn deserialize_newtype_struct<V: Visitor<'de>>(
        self,
        _name: &'static str,
        visitor: V,
    ) -> Result<V::Value, Self::Error> {
        visitor.visit_newtype_struct(self)
    }

    fn deserialize_seq<V: Visitor<'de>>(self, visitor: V) -> Result<V::Value, Self::Error> {
        let args = self.args();

        if !args.is_empty() && self.node.children().is_none() {
            // Arguments → sequence
            visitor.visit_seq(ArgSeqAccess {
                iter: args.into_iter(),
            })
        } else if let Some(children) = self.node.children() {
            // Children → sequence of nodes
            visitor.visit_seq(NodeListSeqAccess {
                iter: children.nodes().iter(),
            })
        } else {
            // Empty → empty sequence
            visitor.visit_seq(ArgSeqAccess {
                iter: Vec::new().into_iter(),
            })
        }
    }

    fn deserialize_tuple<V: Visitor<'de>>(
        self,
        _len: usize,
        visitor: V,
    ) -> Result<V::Value, Self::Error> {
        self.deserialize_seq(visitor)
    }

    fn deserialize_tuple_struct<V: Visitor<'de>>(
        self,
        _name: &'static str,
        _len: usize,
        visitor: V,
    ) -> Result<V::Value, Self::Error> {
        self.deserialize_seq(visitor)
    }

    fn deserialize_map<V: Visitor<'de>>(self, visitor: V) -> Result<V::Value, Self::Error> {
        visitor.visit_map(NodeMapAccess::new(self.node))
    }

    fn deserialize_struct<V: Visitor<'de>>(
        self,
        _name: &'static str,
        _fields: &'static [&'static str],
        visitor: V,
    ) -> Result<V::Value, Self::Error> {
        self.deserialize_map(visitor)
    }

    fn deserialize_enum<V: Visitor<'de>>(
        self,
        _name: &'static str,
        _variants: &'static [&'static str],
        visitor: V,
    ) -> Result<V::Value, Self::Error> {
        // If the node is a scalar string, treat it as a unit variant name.
        if self.is_scalar() {
            if let KdlValue::String(s) = self.args()[0].value() {
                return visitor.visit_enum(str_deserializer(s.as_str()));
            }
        }

        // Otherwise, try treating the node as an externally-tagged enum:
        // if there's exactly one child node, use its name as variant, body as data.
        if let Some(children) = self.node.children() {
            let child_nodes = children.nodes();
            if child_nodes.len() == 1 {
                return visitor.visit_enum(NodeEnumAccess {
                    node: &child_nodes[0],
                });
            }
        }

        // For nodes with properties but no children, try MAP-STYLE
        let props = self.props();
        if props.len() == 1 && self.args().is_empty() && self.node.children().is_none() {
            let prop = props[0];
            let name = prop.name().unwrap().value();
            return visitor.visit_enum(PropertyEnumAccess {
                key: name,
                value: prop.value(),
            });
        }

        // Fall back: just node name is variant
        visitor.visit_enum(NodeEnumAccess { node: self.node })
    }

    fn deserialize_identifier<V: Visitor<'de>>(self, visitor: V) -> Result<V::Value, Self::Error> {
        self.deserialize_str(visitor)
    }

    fn deserialize_ignored_any<V: Visitor<'de>>(self, visitor: V) -> Result<V::Value, Self::Error> {
        visitor.visit_unit()
    }
}

impl<'a> NodeDeserializer<'a> {
    fn deserialize_scalar<'de, V: Visitor<'de>>(self, visitor: V) -> Result<V::Value, Error> {
        if self.is_scalar() {
            ValueDeserializer {
                value: self.args()[0].value(),
            }
            .deserialize_any(visitor)
        } else {
            self.deserialize_any(visitor)
        }
    }
}

struct ArgSeqAccess<'a> {
    iter: std::vec::IntoIter<&'a KdlEntry>,
}

impl<'de, 'a> SeqAccess<'de> for ArgSeqAccess<'a> {
    type Error = Error;

    fn next_element_seed<T: DeserializeSeed<'de>>(
        &mut self,
        seed: T,
    ) -> Result<Option<T::Value>, Self::Error> {
        match self.iter.next() {
            Some(entry) => seed
                .deserialize(ValueDeserializer {
                    value: entry.value(),
                })
                .map(Some),
            None => Ok(None),
        }
    }
}

struct NodeListSeqAccess<'a> {
    iter: std::slice::Iter<'a, KdlNode>,
}

impl<'de, 'a> SeqAccess<'de> for NodeListSeqAccess<'a> {
    type Error = Error;

    fn next_element_seed<T: DeserializeSeed<'de>>(
        &mut self,
        seed: T,
    ) -> Result<Option<T::Value>, Self::Error> {
        match self.iter.next() {
            Some(node) => seed.deserialize(NodeDeserializer { node }).map(Some),
            None => Ok(None),
        }
    }
}

struct NodeGroupDeserializer<'a> {
    nodes: &'a [&'a KdlNode],
}

impl<'de, 'a> de::Deserializer<'de> for NodeGroupDeserializer<'a> {
    type Error = Error;

    fn deserialize_any<V: Visitor<'de>>(self, visitor: V) -> Result<V::Value, Self::Error> {
        self.deserialize_seq(visitor)
    }

    fn deserialize_seq<V: Visitor<'de>>(self, visitor: V) -> Result<V::Value, Self::Error> {
        visitor.visit_seq(NodeGroupSeqAccess {
            iter: self.nodes.iter(),
        })
    }

    serde::forward_to_deserialize_any! {
        bool i8 i16 i32 i64 i128 u8 u16 u32 u64 u128 f32 f64 char str string
        bytes byte_buf option unit unit_struct newtype_struct tuple
        tuple_struct map struct enum identifier ignored_any
    }
}

struct NodeGroupSeqAccess<'a> {
    iter: std::slice::Iter<'a, &'a KdlNode>,
}

impl<'de, 'a> SeqAccess<'de> for NodeGroupSeqAccess<'a> {
    type Error = Error;

    fn next_element_seed<T: DeserializeSeed<'de>>(
        &mut self,
        seed: T,
    ) -> Result<Option<T::Value>, Self::Error> {
        match self.iter.next() {
            Some(node) => seed.deserialize(NodeDeserializer { node }).map(Some),
            None => Ok(None),
        }
    }
}

struct NodeMapAccess<'a> {
    /// Properties and children combined as (key, value_source).
    entries: Vec<(Cow<'a, str>, NodeMapValue<'a>)>,
    idx: usize,
}

enum NodeMapValue<'a> {
    Arg(&'a KdlValue),
    Args(Vec<&'a KdlEntry>),
    SingleNode(&'a KdlNode),
    MultiNode(Vec<&'a KdlNode>),
}

impl<'a> NodeMapAccess<'a> {
    fn new(node: &'a KdlNode) -> Self {
        let mut entries: Vec<(Cow<'a, str>, NodeMapValue<'a>)> = Vec::new();

        let args: Vec<_> = node
            .entries()
            .iter()
            .filter(|e| e.name().is_none())
            .collect();
        let props: Vec<_> = node
            .entries()
            .iter()
            .filter(|e| e.name().is_some())
            .collect();

        for (i, arg) in args.iter().enumerate() {
            entries.push((
                format!("$argument{}", i + 1).into(),
                NodeMapValue::Arg(arg.value()),
            ));
        }

        if !args.is_empty() {
            entries.push(("$arguments".into(), NodeMapValue::Args(args.clone())));
        }

        for prop in &props {
            let name = prop.name().unwrap().value();
            entries.push((format!("@{}", name).into(), NodeMapValue::Arg(prop.value())));
            entries.push((name.into(), NodeMapValue::Arg(prop.value())));
        }

        // Add children (grouped by node name)
        if let Some(children) = node.children() {
            let mut child_keys: Vec<&str> = Vec::new();
            let mut child_groups: std::collections::HashMap<&str, Vec<&KdlNode>> =
                std::collections::HashMap::new();
            for child in children.nodes() {
                let name = child.name().value();
                if !child_groups.contains_key(name) {
                    child_keys.push(name);
                }
                child_groups.entry(name).or_default().push(child);
            }
            for key in child_keys {
                let group = child_groups.remove(key).unwrap();
                if group.len() == 1 {
                    entries.push((key.into(), NodeMapValue::SingleNode(group[0])));
                } else {
                    entries.push((key.into(), NodeMapValue::MultiNode(group)));
                }
            }
        }

        NodeMapAccess { entries, idx: 0 }
    }
}

impl<'de, 'a> MapAccess<'de> for NodeMapAccess<'a> {
    type Error = Error;

    fn next_key_seed<K: DeserializeSeed<'de>>(
        &mut self,
        seed: K,
    ) -> Result<Option<K::Value>, Self::Error> {
        if self.idx >= self.entries.len() {
            return Ok(None);
        }
        match &self.entries[self.idx].0 {
            Cow::Borrowed(s) => seed.deserialize(str_deserializer(s)),
            Cow::Owned(s) => seed.deserialize(StringDeserializer::new(s.clone())),
        }
        .map(Some)
    }

    fn next_value_seed<V: DeserializeSeed<'de>>(
        &mut self,
        seed: V,
    ) -> Result<V::Value, Self::Error> {
        let (_, ref value) = self.entries[self.idx];
        self.idx += 1;
        match value {
            NodeMapValue::Arg(v) => seed.deserialize(ValueDeserializer { value: v }),
            NodeMapValue::Args(entries) => seed.deserialize(ArgsSeqDeserializer {
                entries: entries.clone(),
            }),
            NodeMapValue::SingleNode(node) => seed.deserialize(NodeDeserializer { node }),
            NodeMapValue::MultiNode(nodes) => seed.deserialize(NodeGroupDeserializer { nodes }),
        }
    }
}

struct ArgsSeqDeserializer<'a> {
    entries: Vec<&'a KdlEntry>,
}

impl<'de, 'a> Deserializer<'de> for ArgsSeqDeserializer<'a> {
    type Error = Error;

    fn deserialize_any<V: Visitor<'de>>(self, visitor: V) -> Result<V::Value, Self::Error> {
        visitor.visit_seq(ArgSeqAccess {
            iter: self.entries.into_iter(),
        })
    }

    fn deserialize_seq<V: Visitor<'de>>(self, visitor: V) -> Result<V::Value, Self::Error> {
        visitor.visit_seq(ArgSeqAccess {
            iter: self.entries.into_iter(),
        })
    }

    fn deserialize_option<V: Visitor<'de>>(self, visitor: V) -> Result<V::Value, Self::Error> {
        if self.entries.is_empty() {
            visitor.visit_none()
        } else {
            visitor.visit_some(self)
        }
    }

    fn deserialize_newtype_struct<V: Visitor<'de>>(
        self,
        _: &'static str,
        visitor: V,
    ) -> Result<V::Value, Self::Error> {
        visitor.visit_newtype_struct(self)
    }

    serde::forward_to_deserialize_any! {
        bool i8 i16 i32 i64 i128 u8 u16 u32 u64 u128 f32 f64 char str string
        bytes byte_buf unit unit_struct tuple
        tuple_struct map struct enum identifier ignored_any
    }
}

struct NodeEnumAccess<'a> {
    node: &'a KdlNode,
}

impl<'de, 'a> de::EnumAccess<'de> for NodeEnumAccess<'a> {
    type Error = Error;
    type Variant = NodeVariantAccess<'a>;

    fn variant_seed<V: DeserializeSeed<'de>>(
        self,
        seed: V,
    ) -> Result<(V::Value, Self::Variant), Self::Error> {
        let variant_name = self.node.name().value();
        let val = seed.deserialize(str_deserializer(variant_name))?;
        Ok((val, NodeVariantAccess { node: self.node }))
    }
}

struct NodeVariantAccess<'a> {
    node: &'a KdlNode,
}

impl<'de, 'a> de::VariantAccess<'de> for NodeVariantAccess<'a> {
    type Error = Error;

    fn unit_variant(self) -> Result<(), Self::Error> {
        Ok(())
    }

    fn newtype_variant_seed<T: DeserializeSeed<'de>>(
        self,
        seed: T,
    ) -> Result<T::Value, Self::Error> {
        seed.deserialize(NodeDeserializer { node: self.node })
    }

    fn tuple_variant<V: Visitor<'de>>(
        self,
        _len: usize,
        visitor: V,
    ) -> Result<V::Value, Self::Error> {
        de::Deserializer::deserialize_seq(NodeDeserializer { node: self.node }, visitor)
    }

    fn struct_variant<V: Visitor<'de>>(
        self,
        _fields: &'static [&'static str],
        visitor: V,
    ) -> Result<V::Value, Self::Error> {
        de::Deserializer::deserialize_map(NodeDeserializer { node: self.node }, visitor)
    }
}

struct PropertyEnumAccess<'a> {
    key: &'a str,
    value: &'a KdlValue,
}

impl<'de, 'a> de::EnumAccess<'de> for PropertyEnumAccess<'a> {
    type Error = Error;
    type Variant = PropertyVariantAccess<'a>;

    fn variant_seed<V: DeserializeSeed<'de>>(
        self,
        seed: V,
    ) -> Result<(V::Value, Self::Variant), Self::Error> {
        let val = seed.deserialize(str_deserializer(self.key))?;
        Ok((val, PropertyVariantAccess { value: self.value }))
    }
}

struct PropertyVariantAccess<'a> {
    value: &'a KdlValue,
}

impl<'de, 'a> de::VariantAccess<'de> for PropertyVariantAccess<'a> {
    type Error = Error;

    fn unit_variant(self) -> Result<(), Self::Error> {
        Ok(())
    }

    fn newtype_variant_seed<T: DeserializeSeed<'de>>(
        self,
        seed: T,
    ) -> Result<T::Value, Self::Error> {
        seed.deserialize(ValueDeserializer { value: self.value })
    }

    fn tuple_variant<V: Visitor<'de>>(
        self,
        _len: usize,
        _visitor: V,
    ) -> Result<V::Value, Self::Error> {
        Err(de::Error::custom(
            "tuple variants not supported from KDL properties",
        ))
    }

    fn struct_variant<V: Visitor<'de>>(
        self,
        _fields: &'static [&'static str],
        _visitor: V,
    ) -> Result<V::Value, Self::Error> {
        Err(de::Error::custom(
            "struct variants not supported from KDL properties",
        ))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use serde::Deserialize;

    #[test]
    fn simple_struct() {
        #[derive(Deserialize, Debug, PartialEq)]
        struct Config {
            name: String,
            port: u16,
        }

        let kdl = r#"
name "my-app"
port 8080
"#;
        let config: Config = from_str(kdl).unwrap();
        assert_eq!(
            config,
            Config {
                name: "my-app".into(),
                port: 8080,
            }
        );
    }

    #[test]
    fn nested_struct() {
        #[derive(Deserialize, Debug, PartialEq)]
        struct Config {
            server: Server,
        }

        #[derive(Deserialize, Debug, PartialEq)]
        struct Server {
            host: String,
            port: u16,
        }

        let kdl = r#"
server {
    host "localhost"
    port 8080
}
"#;
        let config: Config = from_str(kdl).unwrap();
        assert_eq!(
            config,
            Config {
                server: Server {
                    host: "localhost".into(),
                    port: 8080,
                },
            }
        );
    }

    #[test]
    fn node_with_properties() {
        #[derive(Deserialize, Debug, PartialEq)]
        struct Config {
            server: Server,
        }

        #[derive(Deserialize, Debug, PartialEq)]
        struct Server {
            host: String,
            port: u16,
        }

        let kdl = r#"server host="localhost" port=8080"#;
        let config: Config = from_str(kdl).unwrap();
        assert_eq!(
            config,
            Config {
                server: Server {
                    host: "localhost".into(),
                    port: 8080,
                },
            }
        );
    }

    #[test]
    fn sequence_of_args() {
        #[derive(Deserialize, Debug, PartialEq)]
        struct Config {
            values: Vec<i32>,
        }

        let kdl = r#"values 1 2 3"#;
        let config: Config = from_str(kdl).unwrap();
        assert_eq!(
            config,
            Config {
                values: vec![1, 2, 3]
            }
        );
    }

    #[test]
    fn repeated_nodes_as_seq() {
        #[derive(Deserialize, Debug, PartialEq)]
        struct Config {
            route: Vec<String>,
        }

        let kdl = r#"
route "/api/users"
route "/api/posts"
route "/api/comments"
"#;
        let config: Config = from_str(kdl).unwrap();
        assert_eq!(
            config,
            Config {
                route: vec![
                    "/api/users".into(),
                    "/api/posts".into(),
                    "/api/comments".into(),
                ],
            }
        );
    }

    #[test]
    fn boolean_and_null() {
        #[derive(Deserialize, Debug, PartialEq)]
        struct Config {
            enabled: bool,
            disabled: bool,
            nothing: Option<String>,
        }

        let kdl = r#"
enabled #true
disabled #false
nothing #null
"#;
        let config: Config = from_str(kdl).unwrap();
        assert_eq!(
            config,
            Config {
                enabled: true,
                disabled: false,
                nothing: None,
            }
        );
    }

    #[test]
    fn option_some() {
        #[derive(Deserialize, Debug, PartialEq)]
        struct Config {
            name: Option<String>,
        }

        let kdl = r#"name "hello""#;
        let config: Config = from_str(kdl).unwrap();
        assert_eq!(
            config,
            Config {
                name: Some("hello".into()),
            }
        );
    }

    #[test]
    fn float_values() {
        #[derive(Deserialize, Debug, PartialEq)]
        struct Config {
            ratio: f64,
        }

        let kdl = r#"ratio 3.14"#;
        let config: Config = from_str(kdl).unwrap();
        assert!((config.ratio - 3.14).abs() < f64::EPSILON);
    }

    #[test]
    fn enum_unit_variant() {
        #[derive(Deserialize, Debug, PartialEq)]
        enum Color {
            Red,
            Green,
            Blue,
        }

        #[derive(Deserialize, Debug, PartialEq)]
        struct Config {
            color: Color,
        }

        let kdl = r#"color "Red""#;
        let config: Config = from_str(kdl).unwrap();
        assert_eq!(config, Config { color: Color::Red });
    }

    #[test]
    fn deeply_nested() {
        #[derive(Deserialize, Debug, PartialEq)]
        struct Root {
            level1: Level1,
        }

        #[derive(Deserialize, Debug, PartialEq)]
        struct Level1 {
            level2: Level2,
        }

        #[derive(Deserialize, Debug, PartialEq)]
        struct Level2 {
            value: i32,
        }

        let kdl = r#"
level1 {
    level2 {
        value 42
    }
}
"#;
        let root: Root = from_str(kdl).unwrap();
        assert_eq!(
            root,
            Root {
                level1: Level1 {
                    level2: Level2 { value: 42 },
                },
            }
        );
    }

    #[test]
    fn children_as_seq() {
        #[derive(Deserialize, Debug, PartialEq)]
        struct Item {
            name: String,
        }

        let kdl = r#"
items {
    item {
        name "a"
    }
    item {
        name "b"
    }
}
"#;

        #[derive(Deserialize, Debug, PartialEq)]
        struct Config {
            items: Items,
        }

        #[derive(Deserialize, Debug, PartialEq)]
        struct Items {
            item: Vec<Item>,
        }

        let config: Config = from_str(kdl).unwrap();
        assert_eq!(
            config,
            Config {
                items: Items {
                    item: vec![Item { name: "a".into() }, Item { name: "b".into() },],
                },
            }
        );
    }

    #[test]
    fn mixed_props_and_children() {
        #[derive(Deserialize, Debug, PartialEq)]
        struct Server {
            host: String,
            port: u16,
            routes: Routes,
        }

        #[derive(Deserialize, Debug, PartialEq)]
        struct Routes {
            path: Vec<String>,
        }

        #[derive(Deserialize, Debug, PartialEq)]
        struct Config {
            server: Server,
        }

        let kdl = r#"
server host="localhost" port=8080 {
    routes {
        path "/a"
        path "/b"
    }
}
"#;
        let config: Config = from_str(kdl).unwrap();
        assert_eq!(
            config,
            Config {
                server: Server {
                    host: "localhost".into(),
                    port: 8080,
                    routes: Routes {
                        path: vec!["/a".into(), "/b".into()],
                    },
                },
            }
        );
    }

    #[test]
    fn empty_document() {
        #[derive(Deserialize, Debug, PartialEq)]
        struct Config {}

        let config: Config = from_str("").unwrap();
        assert_eq!(config, Config {});
    }

    #[test]
    fn hashmap() {
        use std::collections::HashMap;

        let kdl = r#"
alpha 1
beta 2
gamma 3
"#;
        let map: HashMap<String, i32> = from_str(kdl).unwrap();
        assert_eq!(map.get("alpha"), Some(&1));
        assert_eq!(map.get("beta"), Some(&2));
        assert_eq!(map.get("gamma"), Some(&3));
    }

    #[test]
    fn newtype_struct() {
        #[derive(Deserialize, Debug, PartialEq)]
        struct Port(u16);

        #[derive(Deserialize, Debug, PartialEq)]
        struct Config {
            port: Port,
        }

        let kdl = r#"port 8080"#;
        let config: Config = from_str(kdl).unwrap();
        assert_eq!(config, Config { port: Port(8080) });
    }

    #[test]
    fn integer_types() {
        #[derive(Deserialize, Debug, PartialEq)]
        struct Config {
            a: i8,
            b: i16,
            c: i32,
            d: i64,
            e: u8,
            f: u16,
            g: u32,
            h: u64,
        }

        let kdl = r#"
a 1
b 2
c 3
d 4
e 5
f 6
g 7
h 8
"#;
        let config: Config = from_str(kdl).unwrap();
        assert_eq!(
            config,
            Config {
                a: 1,
                b: 2,
                c: 3,
                d: 4,
                e: 5,
                f: 6,
                g: 7,
                h: 8,
            }
        );
    }

    #[test]
    fn rename_props() {
        #[derive(Deserialize, Debug, PartialEq)]
        struct Server {
            #[serde(rename = "@host")]
            host: String,
            #[serde(rename = "@port")]
            port: u16,
        }

        #[derive(Deserialize, Debug, PartialEq)]
        struct Config {
            server: Server,
        }

        let kdl = "server host=localhost port=8080";
        let config: Config = from_str(kdl).unwrap();
        assert_eq!(
            config,
            Config {
                server: Server {
                    host: "localhost".into(),
                    port: 8080,
                },
            }
        );
    }

    #[test]
    fn rename_args() {
        #[derive(Deserialize, Debug, PartialEq)]
        struct Server {
            #[serde(rename = "$argument1")]
            host: String,
            #[serde(rename = "@port")]
            port: u16,
            #[serde(rename = "$arguments")]
            all: Vec<String>,
        }

        #[derive(Deserialize, Debug, PartialEq)]
        struct Config {
            server: Server,
        }

        let kdl = "server localhost port=8080 remote";
        let config: Config = from_str(kdl).unwrap();
        assert_eq!(
            config,
            Config {
                server: Server {
                    host: "localhost".into(),
                    port: 8080,
                    all: Vec::from(["localhost".into(), "remote".into()])
                },
            }
        );
    }

    #[test]
    fn rename_all_args() {
        #[derive(Deserialize, Debug, PartialEq)]
        struct Command {
            #[serde(rename = "@name")]
            name: String,
            #[serde(rename = "$arguments")]
            args: Vec<String>,
        }

        #[derive(Deserialize, Debug, PartialEq)]
        struct Config {
            command: Command,
        }

        let kdl = "command name=run --verbose --output result.txt";
        let config: Config = from_str(kdl).unwrap();
        assert_eq!(
            config,
            Config {
                command: Command {
                    name: "run".into(),
                    args: vec!["--verbose".into(), "--output".into(), "result.txt".into(),],
                },
            }
        );
    }

    #[test]
    fn rename_children_args() {
        #[derive(Deserialize, Debug, PartialEq)]
        struct Server {
            #[serde(rename = "@host")]
            host: String,
            #[serde(rename = "$arguments")]
            ports: Vec<u16>,
        }

        #[derive(Deserialize, Debug, PartialEq)]
        struct Config {
            server: Server,
        }

        let kdl = "server host=localhost 8080 8081 8082";
        let config: Config = from_str(kdl).unwrap();
        assert_eq!(
            config,
            Config {
                server: Server {
                    host: "localhost".into(),
                    ports: vec![8080, 8081, 8082],
                },
            }
        );
    }
}
