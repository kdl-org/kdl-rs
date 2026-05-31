//! Serde serializer for KDL documents.
//!
//! This module provides [`to_string`] for serializing Rust types to KDL text.
//!
//! # KDL Serialization Model
//!
//! Rust types are mapped to KDL as follows:
//!
//! - **Structs**: Each field becomes a KDL node at the current level, where the
//!   node name is the field name and the node's first argument is the field value.
//! - **Maps**: Same as structs — each key becomes a node name.
//! - **Sequences/Tuples**: Serialized as child nodes named `-` (the KDL dash convention).
//! - **Enums**: Unit variants are serialized as bare string arguments. Newtype, tuple,
//!   and struct variants use the variant name as a node name.
//! - **Options**: `None` is serialized as `#null`, `Some(v)` serializes `v` directly.
//! - **Booleans and Null**: Serialized as KDL flags (child present if true, missing if false).
//! - **Scalars**: Serialized as KDL values (strings, integers, floats).
//!
//! # Example
//!
//! ```rust
//! use serde::Serialize;
//!
//! #[derive(Serialize)]
//! struct Config {
//!     name: String,
//!     port: u16,
//! }
//!
//! let config = Config { name: "my-app".into(), port: 8080 };
//! let kdl = kdl::se::to_string(&config).unwrap();
//! assert_eq!(kdl, "name my-app\nport 8080\n");
//! ```
//! You can refer to the [crate::de] module to further customize the
//! (de)serialization model.

use serde::ser::{self, Serialize};
use std::{collections::HashSet, fmt};

use crate::{KdlDocument, KdlEntry, KdlNode, KdlValue};

/// Errors that can occur during KDL serialization.
#[derive(Debug)]
pub struct Error {
    msg: String,
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.msg)
    }
}

impl std::error::Error for Error {}

impl ser::Error for Error {
    fn custom<T: fmt::Display>(msg: T) -> Self {
        Error {
            msg: msg.to_string(),
        }
    }
}

/// Serialize a value to a KDL string.
///
/// # Example
///
/// ```rust
/// use serde::Serialize;
///
/// #[derive(Serialize)]
/// struct Config {
///     name: String,
///     port: u16,
/// }
///
/// let config = Config { name: "my-app".into(), port: 8080 };
/// let kdl = kdl::se::to_string(&config).unwrap();
/// assert_eq!(kdl, "name my-app\nport 8080\n");
/// ```
pub fn to_string<T: Serialize>(value: &T) -> Result<String, Error> {
    let doc = to_document(value)?;
    Ok(doc.to_string())
}

/// Serialize a value to a [`KdlDocument`].
///
/// This is useful if you want to manipulate the document before converting
/// it to a string.
pub fn to_document<T: Serialize>(value: &T) -> Result<KdlDocument, Error> {
    let mut ser = DocumentSerializer {
        doc: KdlDocument::new(),
    };
    value.serialize(&mut ser)?;
    Ok(ser.doc)
}

struct DocumentSerializer {
    doc: KdlDocument,
}

impl<'a> ser::Serializer for &'a mut DocumentSerializer {
    type Ok = ();
    type Error = Error;

    type SerializeSeq = SeqNodeSerializer<'a>;
    type SerializeTuple = SeqNodeSerializer<'a>;
    type SerializeTupleStruct = SeqNodeSerializer<'a>;
    type SerializeTupleVariant = SeqNodeSerializer<'a>;
    type SerializeMap = MapNodeSerializer<'a>;
    type SerializeStruct = MapNodeSerializer<'a>;
    type SerializeStructVariant = StructVariantSerializer<'a>;

    fn serialize_bool(self, v: bool) -> Result<Self::Ok, Self::Error> {
        self.doc.nodes_mut().push(value_node("-", v.into()));
        Ok(())
    }

    fn serialize_i8(self, v: i8) -> Result<Self::Ok, Self::Error> {
        self.serialize_i64(v as i64)
    }
    fn serialize_i16(self, v: i16) -> Result<Self::Ok, Self::Error> {
        self.serialize_i64(v as i64)
    }
    fn serialize_i32(self, v: i32) -> Result<Self::Ok, Self::Error> {
        self.serialize_i64(v as i64)
    }
    fn serialize_i64(self, v: i64) -> Result<Self::Ok, Self::Error> {
        self.doc
            .nodes_mut()
            .push(value_node("-", KdlValue::Integer(v as i128)));
        Ok(())
    }

    fn serialize_u8(self, v: u8) -> Result<Self::Ok, Self::Error> {
        self.serialize_u64(v as u64)
    }
    fn serialize_u16(self, v: u16) -> Result<Self::Ok, Self::Error> {
        self.serialize_u64(v as u64)
    }
    fn serialize_u32(self, v: u32) -> Result<Self::Ok, Self::Error> {
        self.serialize_u64(v as u64)
    }
    fn serialize_u64(self, v: u64) -> Result<Self::Ok, Self::Error> {
        self.doc
            .nodes_mut()
            .push(value_node("-", KdlValue::Integer(v as i128)));
        Ok(())
    }

    fn serialize_f32(self, v: f32) -> Result<Self::Ok, Self::Error> {
        self.serialize_f64(v as f64)
    }
    fn serialize_f64(self, v: f64) -> Result<Self::Ok, Self::Error> {
        self.doc
            .nodes_mut()
            .push(value_node("-", KdlValue::Float(v)));
        Ok(())
    }

    fn serialize_char(self, v: char) -> Result<Self::Ok, Self::Error> {
        self.serialize_str(&v.to_string())
    }

    fn serialize_str(self, v: &str) -> Result<Self::Ok, Self::Error> {
        let entry = KdlEntry::new(KdlValue::String(v.to_string()));
        let mut node = KdlNode::new("-");
        node.entries_mut().push(entry);
        self.doc.nodes_mut().push(node);
        Ok(())
    }

    fn serialize_bytes(self, v: &[u8]) -> Result<Self::Ok, Self::Error> {
        use serde::ser::SerializeSeq;
        let mut seq = self.serialize_seq(Some(v.len()))?;
        for b in v {
            seq.serialize_element(b)?;
        }
        seq.end()
    }

    fn serialize_none(self) -> Result<Self::Ok, Self::Error> {
        self.serialize_unit()
    }

    fn serialize_some<T: ?Sized + Serialize>(self, value: &T) -> Result<Self::Ok, Self::Error> {
        value.serialize(self)
    }

    fn serialize_unit(self) -> Result<Self::Ok, Self::Error> {
        // Empty document for unit
        Ok(())
    }

    fn serialize_unit_struct(self, _name: &'static str) -> Result<Self::Ok, Self::Error> {
        self.serialize_unit()
    }

    fn serialize_unit_variant(
        self,
        _name: &'static str,
        _variant_index: u32,
        variant: &'static str,
    ) -> Result<Self::Ok, Self::Error> {
        self.doc
            .nodes_mut()
            .push(value_node("-", KdlValue::String(variant.to_string())));
        Ok(())
    }

    fn serialize_newtype_struct<T: ?Sized + Serialize>(
        self,
        _name: &'static str,
        value: &T,
    ) -> Result<Self::Ok, Self::Error> {
        value.serialize(self)
    }

    fn serialize_newtype_variant<T: ?Sized + Serialize>(
        self,
        _name: &'static str,
        _variant_index: u32,
        variant: &'static str,
        value: &T,
    ) -> Result<Self::Ok, Self::Error> {
        let mut node = KdlNode::new(variant);
        let kdl_val = to_kdl_value(value)?;
        node.entries_mut().push(KdlEntry::new(kdl_val));
        self.doc.nodes_mut().push(node);
        Ok(())
    }

    fn serialize_seq(self, _len: Option<usize>) -> Result<Self::SerializeSeq, Self::Error> {
        Ok(SeqNodeSerializer {
            nodes: &mut self.doc.nodes,
            node_name: "-",
        })
    }

    fn serialize_tuple(self, len: usize) -> Result<Self::SerializeTuple, Self::Error> {
        self.serialize_seq(Some(len))
    }

    fn serialize_tuple_struct(
        self,
        _name: &'static str,
        len: usize,
    ) -> Result<Self::SerializeTupleStruct, Self::Error> {
        self.serialize_seq(Some(len))
    }

    fn serialize_tuple_variant(
        self,
        _name: &'static str,
        _variant_index: u32,
        variant: &'static str,
        _len: usize,
    ) -> Result<Self::SerializeTupleVariant, Self::Error> {
        Ok(SeqNodeSerializer {
            nodes: &mut self.doc.nodes,
            node_name: variant,
        })
    }

    fn serialize_map(self, _len: Option<usize>) -> Result<Self::SerializeMap, Self::Error> {
        Ok(MapNodeSerializer {
            nodes: &mut self.doc.nodes,
            current_key: None,
        })
    }

    fn serialize_struct(
        self,
        _name: &'static str,
        _len: usize,
    ) -> Result<Self::SerializeStruct, Self::Error> {
        Ok(MapNodeSerializer {
            nodes: &mut self.doc.nodes,
            current_key: None,
        })
    }

    fn serialize_struct_variant(
        self,
        _name: &'static str,
        _variant_index: u32,
        variant: &'static str,
        _len: usize,
    ) -> Result<Self::SerializeStructVariant, Self::Error> {
        Ok(StructVariantSerializer {
            parent_nodes: &mut self.doc.nodes,
            variant,
            children: KdlDocument::new(),
            used_indices: Default::default(),
        })
    }
}

struct SeqNodeSerializer<'a> {
    nodes: &'a mut Vec<KdlNode>,
    node_name: &'a str,
}

impl<'a> ser::SerializeSeq for SeqNodeSerializer<'a> {
    type Ok = ();
    type Error = Error;

    fn serialize_element<T: ?Sized + Serialize>(&mut self, value: &T) -> Result<(), Self::Error> {
        let mut node = KdlNode::new(self.node_name);
        serialize_value_into_node(&mut node, value)?;
        self.nodes.push(node);
        Ok(())
    }

    fn end(self) -> Result<Self::Ok, Self::Error> {
        Ok(())
    }
}

impl<'a> ser::SerializeTuple for SeqNodeSerializer<'a> {
    type Ok = ();
    type Error = Error;

    fn serialize_element<T: ?Sized + Serialize>(&mut self, value: &T) -> Result<(), Self::Error> {
        ser::SerializeSeq::serialize_element(self, value)
    }

    fn end(self) -> Result<Self::Ok, Self::Error> {
        ser::SerializeSeq::end(self)
    }
}

impl<'a> ser::SerializeTupleStruct for SeqNodeSerializer<'a> {
    type Ok = ();
    type Error = Error;

    fn serialize_field<T: ?Sized + Serialize>(&mut self, value: &T) -> Result<(), Self::Error> {
        ser::SerializeSeq::serialize_element(self, value)
    }

    fn end(self) -> Result<Self::Ok, Self::Error> {
        ser::SerializeSeq::end(self)
    }
}

impl<'a> ser::SerializeTupleVariant for SeqNodeSerializer<'a> {
    type Ok = ();
    type Error = Error;

    fn serialize_field<T: ?Sized + Serialize>(&mut self, value: &T) -> Result<(), Self::Error> {
        ser::SerializeSeq::serialize_element(self, value)
    }

    fn end(self) -> Result<Self::Ok, Self::Error> {
        ser::SerializeSeq::end(self)
    }
}

struct MapNodeSerializer<'a> {
    nodes: &'a mut Vec<KdlNode>,
    current_key: Option<String>,
}

impl<'a> ser::SerializeMap for MapNodeSerializer<'a> {
    type Ok = ();
    type Error = Error;

    fn serialize_key<T: ?Sized + Serialize>(&mut self, key: &T) -> Result<(), Self::Error> {
        self.current_key = Some(key_to_string(key)?);
        Ok(())
    }

    fn serialize_value<T: ?Sized + Serialize>(&mut self, value: &T) -> Result<(), Self::Error> {
        let key = self
            .current_key
            .take()
            .ok_or_else(|| ser::Error::custom("serialize_value without serialize_key"))?;
        let mut node = KdlNode::new(key);
        serialize_value_into_node(&mut node, value)?;
        self.nodes.push(node);
        Ok(())
    }

    fn end(self) -> Result<Self::Ok, Self::Error> {
        Ok(())
    }
}

impl<'a> ser::SerializeStruct for MapNodeSerializer<'a> {
    type Ok = ();
    type Error = Error;

    fn serialize_field<T: ?Sized + Serialize>(
        &mut self,
        key: &'static str,
        value: &T,
    ) -> Result<(), Self::Error> {
        let mut node = KdlNode::new(key);
        serialize_value_into_node(&mut node, value)?;
        match node.get(0) {
            Some(KdlValue::Bool(false)) | Some(KdlValue::Null) => {
                // Skip adding this node
            }
            _ => {
                self.nodes.push(node);
            }
        }
        Ok(())
    }

    fn end(self) -> Result<Self::Ok, Self::Error> {
        Ok(())
    }
}

struct StructVariantSerializer<'a> {
    parent_nodes: &'a mut Vec<KdlNode>,
    variant: &'static str,
    used_indices: HashSet<usize>,
    children: KdlDocument,
}

impl<'a> ser::SerializeStructVariant for StructVariantSerializer<'a> {
    type Ok = ();
    type Error = Error;

    fn serialize_field<T: ?Sized + Serialize>(
        &mut self,
        key: &'static str,
        value: &T,
    ) -> Result<(), Self::Error> {
        let mut node = KdlNode::new(key);
        if let Some(attr_name) = key.strip_prefix("#@") {
            let kdl_val = to_kdl_value(value)?;
            node.entries_mut()
                .push(KdlEntry::new_prop(attr_name, kdl_val));
        } else if key == "#args" {
            let mut ser = ArgsSerializer { node: &mut node };
            value.serialize(&mut ser)?;
        } else if key == "#rest" {
            let mut ser = RestSerializer {
                node: &mut node,
                used_indices: &self.used_indices,
            };
            value.serialize(&mut ser)?;
        } else if key.starts_with("#") {
            let idx: usize = key.strip_prefix('#').unwrap().parse().map_err(|e| Error {
                msg: format!("Failed to parse index rename {key}: {e}"),
            })?;
            self.used_indices.insert(idx);
            let kdl_val = to_kdl_value(value)?;
            node.entries_mut().push(KdlEntry::new(kdl_val));
        } else {
            let mut child = KdlNode::new(key);
            let mut child_ser = NodeValueSerializer { node: &mut child };
            value.serialize(&mut child_ser)?;
            let children = node.ensure_children();
            match child.get(0) {
                Some(KdlValue::Bool(false)) | Some(KdlValue::Null) => {
                    // Skip adding this node
                }
                _ => {
                    children.nodes_mut().push(child);
                }
            }
        }
        Ok(())
    }

    fn end(self) -> Result<Self::Ok, Self::Error> {
        let mut node = KdlNode::new(self.variant);
        node.set_children(self.children);
        self.parent_nodes.push(node);
        Ok(())
    }
}

/// Serializes a value into a node, adding it as an argument, or as children
/// if the value is complex (struct/map/seq).
fn serialize_value_into_node<T: ?Sized + Serialize>(
    node: &mut KdlNode,
    value: &T,
) -> Result<(), Error> {
    let mut ser = NodeValueSerializer { node };
    value.serialize(&mut ser)
}

/// Serializer that writes into a KdlNode.
struct NodeValueSerializer<'a> {
    node: &'a mut KdlNode,
}

impl<'a> ser::Serializer for &'a mut NodeValueSerializer<'a> {
    type Ok = ();
    type Error = Error;

    type SerializeSeq = NodeSeqSerializer<'a>;
    type SerializeTuple = NodeSeqSerializer<'a>;
    type SerializeTupleStruct = NodeSeqSerializer<'a>;
    type SerializeTupleVariant = NodeChildSeqSerializer<'a>;
    type SerializeMap = NodeChildMapSerializer<'a>;
    type SerializeStruct = NodeChildMapSerializer<'a>;
    type SerializeStructVariant = NodeChildStructVariantSerializer<'a>;

    fn serialize_bool(self, v: bool) -> Result<Self::Ok, Self::Error> {
        self.node.entries_mut().push(KdlEntry::new(v));
        Ok(())
    }

    fn serialize_i8(self, v: i8) -> Result<Self::Ok, Self::Error> {
        self.serialize_i64(v as i64)
    }
    fn serialize_i16(self, v: i16) -> Result<Self::Ok, Self::Error> {
        self.serialize_i64(v as i64)
    }
    fn serialize_i32(self, v: i32) -> Result<Self::Ok, Self::Error> {
        self.serialize_i64(v as i64)
    }
    fn serialize_i64(self, v: i64) -> Result<Self::Ok, Self::Error> {
        self.node
            .entries_mut()
            .push(KdlEntry::new(KdlValue::Integer(v as i128)));
        Ok(())
    }

    fn serialize_u8(self, v: u8) -> Result<Self::Ok, Self::Error> {
        self.serialize_u64(v as u64)
    }
    fn serialize_u16(self, v: u16) -> Result<Self::Ok, Self::Error> {
        self.serialize_u64(v as u64)
    }
    fn serialize_u32(self, v: u32) -> Result<Self::Ok, Self::Error> {
        self.serialize_u64(v as u64)
    }
    fn serialize_u64(self, v: u64) -> Result<Self::Ok, Self::Error> {
        self.node
            .entries_mut()
            .push(KdlEntry::new(KdlValue::Integer(v as i128)));
        Ok(())
    }

    fn serialize_f32(self, v: f32) -> Result<Self::Ok, Self::Error> {
        self.serialize_f64(v as f64)
    }
    fn serialize_f64(self, v: f64) -> Result<Self::Ok, Self::Error> {
        self.node
            .entries_mut()
            .push(KdlEntry::new(KdlValue::Float(v)));
        Ok(())
    }

    fn serialize_char(self, v: char) -> Result<Self::Ok, Self::Error> {
        self.serialize_str(&v.to_string())
    }

    fn serialize_str(self, v: &str) -> Result<Self::Ok, Self::Error> {
        let entry = KdlEntry::new(KdlValue::String(v.to_string()));
        self.node.entries_mut().push(entry);
        Ok(())
    }

    fn serialize_bytes(self, v: &[u8]) -> Result<Self::Ok, Self::Error> {
        use serde::ser::SerializeSeq;
        let mut seq = self.serialize_seq(Some(v.len()))?;
        for b in v {
            seq.serialize_element(b)?;
        }
        seq.end()
    }

    fn serialize_none(self) -> Result<Self::Ok, Self::Error> {
        self.node.entries_mut().push(KdlEntry::new(KdlValue::Null));
        Ok(())
    }

    fn serialize_some<T: ?Sized + Serialize>(self, value: &T) -> Result<Self::Ok, Self::Error> {
        value.serialize(self)
    }

    fn serialize_unit(self) -> Result<Self::Ok, Self::Error> {
        self.node.entries_mut().push(KdlEntry::new(KdlValue::Null));
        Ok(())
    }

    fn serialize_unit_struct(self, _name: &'static str) -> Result<Self::Ok, Self::Error> {
        self.serialize_unit()
    }

    fn serialize_unit_variant(
        self,
        _name: &'static str,
        _variant_index: u32,
        variant: &'static str,
    ) -> Result<Self::Ok, Self::Error> {
        self.serialize_str(variant)
    }

    fn serialize_newtype_struct<T: ?Sized + Serialize>(
        self,
        _name: &'static str,
        value: &T,
    ) -> Result<Self::Ok, Self::Error> {
        value.serialize(self)
    }

    fn serialize_newtype_variant<T: ?Sized + Serialize>(
        self,
        _name: &'static str,
        _variant_index: u32,
        variant: &'static str,
        value: &T,
    ) -> Result<Self::Ok, Self::Error> {
        let mut child = KdlNode::new(variant);
        let kdl_val = to_kdl_value(value)?;
        child.entries_mut().push(KdlEntry::new(kdl_val));
        let children = self.node.ensure_children();
        children.nodes_mut().push(child);
        Ok(())
    }

    fn serialize_seq(self, _len: Option<usize>) -> Result<Self::SerializeSeq, Self::Error> {
        Ok(NodeSeqSerializer { node: self.node })
    }

    fn serialize_tuple(self, len: usize) -> Result<Self::SerializeTuple, Self::Error> {
        self.serialize_seq(Some(len))
    }

    fn serialize_tuple_struct(
        self,
        _name: &'static str,
        len: usize,
    ) -> Result<Self::SerializeTupleStruct, Self::Error> {
        self.serialize_seq(Some(len))
    }

    fn serialize_tuple_variant(
        self,
        _name: &'static str,
        _variant_index: u32,
        variant: &'static str,
        _len: usize,
    ) -> Result<Self::SerializeTupleVariant, Self::Error> {
        Ok(NodeChildSeqSerializer {
            parent: self.node,
            variant_name: Some(variant),
            items: Vec::new(),
        })
    }

    fn serialize_map(self, _len: Option<usize>) -> Result<Self::SerializeMap, Self::Error> {
        Ok(NodeChildMapSerializer {
            node: self.node,
            current_key: None,
        })
    }

    fn serialize_struct(
        self,
        _name: &'static str,
        _len: usize,
    ) -> Result<Self::SerializeStruct, Self::Error> {
        Ok(NodeChildMapSerializer {
            node: self.node,
            current_key: None,
        })
    }

    fn serialize_struct_variant(
        self,
        _name: &'static str,
        _variant_index: u32,
        variant: &'static str,
        _len: usize,
    ) -> Result<Self::SerializeStructVariant, Self::Error> {
        Ok(NodeChildStructVariantSerializer {
            parent: self.node,
            variant,
            children: KdlDocument::new(),
        })
    }
}

struct NodeSeqSerializer<'a> {
    node: &'a mut KdlNode,
}

impl<'a> ser::SerializeSeq for NodeSeqSerializer<'a> {
    type Ok = ();
    type Error = Error;

    fn serialize_element<T: ?Sized + Serialize>(&mut self, value: &T) -> Result<(), Self::Error> {
        let kdl_val = to_kdl_value(value)?;
        self.node.entries_mut().push(KdlEntry::new(kdl_val));
        Ok(())
    }

    fn end(self) -> Result<Self::Ok, Self::Error> {
        Ok(())
    }
}

impl<'a> ser::SerializeTuple for NodeSeqSerializer<'a> {
    type Ok = ();
    type Error = Error;

    fn serialize_element<T: ?Sized + Serialize>(&mut self, value: &T) -> Result<(), Self::Error> {
        ser::SerializeSeq::serialize_element(self, value)
    }

    fn end(self) -> Result<Self::Ok, Self::Error> {
        ser::SerializeSeq::end(self)
    }
}

impl<'a> ser::SerializeTupleStruct for NodeSeqSerializer<'a> {
    type Ok = ();
    type Error = Error;

    fn serialize_field<T: ?Sized + Serialize>(&mut self, value: &T) -> Result<(), Self::Error> {
        ser::SerializeSeq::serialize_element(self, value)
    }

    fn end(self) -> Result<Self::Ok, Self::Error> {
        ser::SerializeSeq::end(self)
    }
}

struct NodeChildSeqSerializer<'a> {
    parent: &'a mut KdlNode,
    variant_name: Option<&'static str>,
    items: Vec<KdlValue>,
}

impl<'a> ser::SerializeTupleVariant for NodeChildSeqSerializer<'a> {
    type Ok = ();
    type Error = Error;

    fn serialize_field<T: ?Sized + Serialize>(&mut self, value: &T) -> Result<(), Self::Error> {
        let kdl_val = to_kdl_value(value)?;
        self.items.push(kdl_val);
        Ok(())
    }

    fn end(self) -> Result<Self::Ok, Self::Error> {
        let name = self.variant_name.unwrap_or("-");
        let mut node = KdlNode::new(name);
        for item in self.items {
            node.entries_mut().push(KdlEntry::new(item));
        }
        let children = self.parent.ensure_children();
        children.nodes_mut().push(node);
        Ok(())
    }
}

struct NodeChildMapSerializer<'a> {
    node: &'a mut KdlNode,
    current_key: Option<String>,
}

impl<'a> ser::SerializeMap for NodeChildMapSerializer<'a> {
    type Ok = ();
    type Error = Error;

    fn serialize_key<T: ?Sized + Serialize>(&mut self, key: &T) -> Result<(), Self::Error> {
        self.current_key = Some(key_to_string(key)?);
        Ok(())
    }

    fn serialize_value<T: ?Sized + Serialize>(&mut self, value: &T) -> Result<(), Self::Error> {
        let key = self
            .current_key
            .take()
            .ok_or_else(|| ser::Error::custom("serialize_value without serialize_key"))?;
        let mut child = KdlNode::new(key);
        let mut child_ser = NodeValueSerializer { node: &mut child };
        value.serialize(&mut child_ser)?;
        match child.get(0) {
            Some(KdlValue::Bool(false)) | Some(KdlValue::Null) => {
                // Skip adding this node
            }
            _ => {
                let children = self.node.ensure_children();
                children.nodes_mut().push(child);
            }
        }
        Ok(())
    }

    fn end(self) -> Result<Self::Ok, Self::Error> {
        Ok(())
    }
}

impl<'a> ser::SerializeStruct for NodeChildMapSerializer<'a> {
    type Ok = ();
    type Error = Error;

    fn serialize_field<T: ?Sized + Serialize>(
        &mut self,
        key: &'static str,
        value: &T,
    ) -> Result<(), Self::Error> {
        if let Some(attr_name) = key.strip_prefix("#@") {
            let kdl_val = to_kdl_value(value)?;
            self.node
                .entries_mut()
                .push(KdlEntry::new_prop(attr_name, kdl_val));
        } else if key == "#args" {
            let mut ser = ArgsSerializer { node: self.node };
            value.serialize(&mut ser)?;
        } else if key == "#rest" {
            // TODO(@zkat): do this properly. Need to keep track of what args have been picked out.
            let mut ser = ArgsSerializer { node: self.node };
            value.serialize(&mut ser)?;
        } else if key.starts_with("#") {
            // TODO(@zkat): How do we get the ordering here?... This will just
            // insert stuff as we discover it.
            let kdl_val = to_kdl_value(value)?;
            self.node.entries_mut().push(KdlEntry::new(kdl_val));
        } else {
            let mut child = KdlNode::new(key);
            let mut child_ser = NodeValueSerializer { node: &mut child };
            value.serialize(&mut child_ser)?;
            let children = self.node.ensure_children();
            match child.get(0) {
                Some(KdlValue::Bool(false)) | Some(KdlValue::Null) => {
                    // Skip adding this node
                }
                _ => {
                    children.nodes_mut().push(child);
                }
            }
        }
        Ok(())
    }

    fn end(self) -> Result<Self::Ok, Self::Error> {
        Ok(())
    }
}

struct NodeChildStructVariantSerializer<'a> {
    parent: &'a mut KdlNode,
    variant: &'static str,
    children: KdlDocument,
}

impl<'a> ser::SerializeStructVariant for NodeChildStructVariantSerializer<'a> {
    type Ok = ();
    type Error = Error;

    fn serialize_field<T: ?Sized + Serialize>(
        &mut self,
        key: &'static str,
        value: &T,
    ) -> Result<(), Self::Error> {
        let mut node = KdlNode::new(key);
        if let Some(attr_name) = key.strip_prefix("#@") {
            let kdl_val = to_kdl_value(value)?;
            self.parent
                .entries_mut()
                .push(KdlEntry::new_prop(attr_name, kdl_val));
        } else if key == "#args" {
            let mut ser = ArgsSerializer { node: &mut node };
            value.serialize(&mut ser)?;
        } else if key == "#rest" {
            // TODO(@zkat): do this properly. Need to keep track of what args have been picked out.
            let mut ser = ArgsSerializer { node: &mut node };
            value.serialize(&mut ser)?;
        } else if key.starts_with("#") {
            // TODO(@zkat): How do we get the ordering here?... This will just
            // insert stuff as we discover it.
            let kdl_val = to_kdl_value(value)?;
            self.parent.entries_mut().push(KdlEntry::new(kdl_val));
        } else {
            let mut child_ser = NodeValueSerializer { node: &mut node };
            value.serialize(&mut child_ser)?;
            match node.get(0) {
                Some(KdlValue::Bool(false)) | Some(KdlValue::Null) => {
                    // Skip adding this node
                }
                _ => {
                    self.children.nodes_mut().push(node);
                }
            }
        }
        Ok(())
    }

    fn end(self) -> Result<Self::Ok, Self::Error> {
        let mut node = KdlNode::new(self.variant);
        node.set_children(self.children);
        let parent_children = self.parent.ensure_children();
        parent_children.nodes_mut().push(node);
        Ok(())
    }
}

/// Helper for a regular node with just a name and just an argument value
fn value_node(name: &str, value: KdlValue) -> KdlNode {
    let mut node = KdlNode::new(name);
    node.entries_mut().push(KdlEntry::new(value));
    node
}

/// Convert a serde key to a string.
fn key_to_string<T: ?Sized + Serialize>(key: &T) -> Result<String, Error> {
    key.serialize(KeySerializer)
}

/// Restricted serializer that only produces strings (for map keys / node names).
struct KeySerializer;

impl ser::Serializer for KeySerializer {
    type Ok = String;
    type Error = Error;

    type SerializeSeq = ser::Impossible<String, Error>;
    type SerializeTuple = ser::Impossible<String, Error>;
    type SerializeTupleStruct = ser::Impossible<String, Error>;
    type SerializeTupleVariant = ser::Impossible<String, Error>;
    type SerializeMap = ser::Impossible<String, Error>;
    type SerializeStruct = ser::Impossible<String, Error>;
    type SerializeStructVariant = ser::Impossible<String, Error>;

    fn serialize_bool(self, v: bool) -> Result<Self::Ok, Self::Error> {
        Ok(if v {
            "true".to_string()
        } else {
            "false".to_string()
        })
    }

    fn serialize_i8(self, v: i8) -> Result<Self::Ok, Self::Error> {
        Ok(v.to_string())
    }
    fn serialize_i16(self, v: i16) -> Result<Self::Ok, Self::Error> {
        Ok(v.to_string())
    }
    fn serialize_i32(self, v: i32) -> Result<Self::Ok, Self::Error> {
        Ok(v.to_string())
    }
    fn serialize_i64(self, v: i64) -> Result<Self::Ok, Self::Error> {
        Ok(v.to_string())
    }
    fn serialize_u8(self, v: u8) -> Result<Self::Ok, Self::Error> {
        Ok(v.to_string())
    }
    fn serialize_u16(self, v: u16) -> Result<Self::Ok, Self::Error> {
        Ok(v.to_string())
    }
    fn serialize_u32(self, v: u32) -> Result<Self::Ok, Self::Error> {
        Ok(v.to_string())
    }
    fn serialize_u64(self, v: u64) -> Result<Self::Ok, Self::Error> {
        Ok(v.to_string())
    }

    fn serialize_f32(self, v: f32) -> Result<Self::Ok, Self::Error> {
        Ok(v.to_string())
    }
    fn serialize_f64(self, v: f64) -> Result<Self::Ok, Self::Error> {
        Ok(v.to_string())
    }

    fn serialize_char(self, v: char) -> Result<Self::Ok, Self::Error> {
        Ok(v.to_string())
    }

    fn serialize_str(self, v: &str) -> Result<Self::Ok, Self::Error> {
        Ok(v.to_string())
    }

    fn serialize_bytes(self, _v: &[u8]) -> Result<Self::Ok, Self::Error> {
        Err(ser::Error::custom("bytes cannot be used as KDL node names"))
    }

    fn serialize_none(self) -> Result<Self::Ok, Self::Error> {
        Err(ser::Error::custom("None cannot be used as a KDL node name"))
    }

    fn serialize_some<T: ?Sized + Serialize>(self, value: &T) -> Result<Self::Ok, Self::Error> {
        value.serialize(self)
    }

    fn serialize_unit(self) -> Result<Self::Ok, Self::Error> {
        Err(ser::Error::custom("unit cannot be used as a KDL node name"))
    }

    fn serialize_unit_struct(self, name: &'static str) -> Result<Self::Ok, Self::Error> {
        Ok(name.to_string())
    }

    fn serialize_unit_variant(
        self,
        _name: &'static str,
        _variant_index: u32,
        variant: &'static str,
    ) -> Result<Self::Ok, Self::Error> {
        Ok(variant.to_string())
    }

    fn serialize_newtype_struct<T: ?Sized + Serialize>(
        self,
        _name: &'static str,
        value: &T,
    ) -> Result<Self::Ok, Self::Error> {
        value.serialize(self)
    }

    fn serialize_newtype_variant<T: ?Sized + Serialize>(
        self,
        _name: &'static str,
        _variant_index: u32,
        _variant: &'static str,
        _value: &T,
    ) -> Result<Self::Ok, Self::Error> {
        Err(ser::Error::custom(
            "newtype variants cannot be used as KDL node names",
        ))
    }

    fn serialize_seq(self, _len: Option<usize>) -> Result<Self::SerializeSeq, Self::Error> {
        Err(ser::Error::custom(
            "sequences cannot be used as KDL node names",
        ))
    }

    fn serialize_tuple(self, _len: usize) -> Result<Self::SerializeTuple, Self::Error> {
        Err(ser::Error::custom(
            "tuples cannot be used as KDL node names",
        ))
    }

    fn serialize_tuple_struct(
        self,
        _name: &'static str,
        _len: usize,
    ) -> Result<Self::SerializeTupleStruct, Self::Error> {
        Err(ser::Error::custom(
            "tuple structs cannot be used as KDL node names",
        ))
    }

    fn serialize_tuple_variant(
        self,
        _name: &'static str,
        _variant_index: u32,
        _variant: &'static str,
        _len: usize,
    ) -> Result<Self::SerializeTupleVariant, Self::Error> {
        Err(ser::Error::custom(
            "tuple variants cannot be used as KDL node names",
        ))
    }

    fn serialize_map(self, _len: Option<usize>) -> Result<Self::SerializeMap, Self::Error> {
        Err(ser::Error::custom("maps cannot be used as KDL node names"))
    }

    fn serialize_struct(
        self,
        _name: &'static str,
        _len: usize,
    ) -> Result<Self::SerializeStruct, Self::Error> {
        Err(ser::Error::custom(
            "structs cannot be used as KDL node names",
        ))
    }

    fn serialize_struct_variant(
        self,
        _name: &'static str,
        _variant_index: u32,
        _variant: &'static str,
        _len: usize,
    ) -> Result<Self::SerializeStructVariant, Self::Error> {
        Err(ser::Error::custom(
            "struct variants cannot be used as KDL node names",
        ))
    }
}

/// Try to convert a serde-serializable value to a KdlValue.
/// Retreats a to String for complex types.
fn to_kdl_value<T: ?Sized + Serialize>(value: &T) -> Result<KdlValue, Error> {
    value.serialize(KdlValueSerializer)
}

/// Serializer that produces a KdlValue from a scalar.
struct KdlValueSerializer;

impl ser::Serializer for KdlValueSerializer {
    type Ok = KdlValue;
    type Error = Error;

    type SerializeSeq = ser::Impossible<KdlValue, Error>;
    type SerializeTuple = ser::Impossible<KdlValue, Error>;
    type SerializeTupleStruct = ser::Impossible<KdlValue, Error>;
    type SerializeTupleVariant = ser::Impossible<KdlValue, Error>;
    type SerializeMap = ser::Impossible<KdlValue, Error>;
    type SerializeStruct = ser::Impossible<KdlValue, Error>;
    type SerializeStructVariant = ser::Impossible<KdlValue, Error>;

    fn serialize_bool(self, v: bool) -> Result<Self::Ok, Self::Error> {
        Ok(KdlValue::Bool(v))
    }

    fn serialize_i8(self, v: i8) -> Result<Self::Ok, Self::Error> {
        Ok(KdlValue::Integer(v as i128))
    }
    fn serialize_i16(self, v: i16) -> Result<Self::Ok, Self::Error> {
        Ok(KdlValue::Integer(v as i128))
    }
    fn serialize_i32(self, v: i32) -> Result<Self::Ok, Self::Error> {
        Ok(KdlValue::Integer(v as i128))
    }
    fn serialize_i64(self, v: i64) -> Result<Self::Ok, Self::Error> {
        Ok(KdlValue::Integer(v as i128))
    }

    fn serialize_u8(self, v: u8) -> Result<Self::Ok, Self::Error> {
        Ok(KdlValue::Integer(v as i128))
    }
    fn serialize_u16(self, v: u16) -> Result<Self::Ok, Self::Error> {
        Ok(KdlValue::Integer(v as i128))
    }
    fn serialize_u32(self, v: u32) -> Result<Self::Ok, Self::Error> {
        Ok(KdlValue::Integer(v as i128))
    }
    fn serialize_u64(self, v: u64) -> Result<Self::Ok, Self::Error> {
        Ok(KdlValue::Integer(v as i128))
    }

    fn serialize_f32(self, v: f32) -> Result<Self::Ok, Self::Error> {
        Ok(KdlValue::Float(v as f64))
    }
    fn serialize_f64(self, v: f64) -> Result<Self::Ok, Self::Error> {
        Ok(KdlValue::Float(v))
    }

    fn serialize_char(self, v: char) -> Result<Self::Ok, Self::Error> {
        Ok(KdlValue::String(v.to_string()))
    }

    fn serialize_str(self, v: &str) -> Result<Self::Ok, Self::Error> {
        Ok(KdlValue::String(v.to_string()))
    }

    fn serialize_bytes(self, _v: &[u8]) -> Result<Self::Ok, Self::Error> {
        Err(ser::Error::custom(
            "bytes cannot be directly represented as a KDL value",
        ))
    }

    fn serialize_none(self) -> Result<Self::Ok, Self::Error> {
        Ok(KdlValue::Null)
    }

    fn serialize_some<T: ?Sized + Serialize>(self, value: &T) -> Result<Self::Ok, Self::Error> {
        value.serialize(self)
    }

    fn serialize_unit(self) -> Result<Self::Ok, Self::Error> {
        Ok(KdlValue::Null)
    }

    fn serialize_unit_struct(self, _name: &'static str) -> Result<Self::Ok, Self::Error> {
        Ok(KdlValue::Null)
    }

    fn serialize_unit_variant(
        self,
        _name: &'static str,
        _variant_index: u32,
        variant: &'static str,
    ) -> Result<Self::Ok, Self::Error> {
        Ok(KdlValue::String(variant.to_string()))
    }

    fn serialize_newtype_struct<T: ?Sized + Serialize>(
        self,
        _name: &'static str,
        value: &T,
    ) -> Result<Self::Ok, Self::Error> {
        value.serialize(self)
    }

    fn serialize_newtype_variant<T: ?Sized + Serialize>(
        self,
        _name: &'static str,
        _variant_index: u32,
        _variant: &'static str,
        _value: &T,
    ) -> Result<Self::Ok, Self::Error> {
        Err(ser::Error::custom(
            "newtype variants cannot be represented as a single KDL value",
        ))
    }

    fn serialize_seq(self, _len: Option<usize>) -> Result<Self::SerializeSeq, Self::Error> {
        Err(ser::Error::custom(
            "sequences cannot be represented as a single KDL value",
        ))
    }

    fn serialize_tuple(self, _len: usize) -> Result<Self::SerializeTuple, Self::Error> {
        Err(ser::Error::custom(
            "tuples cannot be represented as a single KDL value",
        ))
    }

    fn serialize_tuple_struct(
        self,
        _name: &'static str,
        _len: usize,
    ) -> Result<Self::SerializeTupleStruct, Self::Error> {
        Err(ser::Error::custom(
            "tuple structs cannot be represented as a single KDL value",
        ))
    }

    fn serialize_tuple_variant(
        self,
        _name: &'static str,
        _variant_index: u32,
        _variant: &'static str,
        _len: usize,
    ) -> Result<Self::SerializeTupleVariant, Self::Error> {
        Err(ser::Error::custom(
            "tuple variants cannot be represented as a single KDL value",
        ))
    }

    fn serialize_map(self, _len: Option<usize>) -> Result<Self::SerializeMap, Self::Error> {
        Err(ser::Error::custom(
            "maps cannot be represented as a single KDL value",
        ))
    }

    fn serialize_struct(
        self,
        _name: &'static str,
        _len: usize,
    ) -> Result<Self::SerializeStruct, Self::Error> {
        Err(ser::Error::custom(
            "structs cannot be represented as a single KDL value",
        ))
    }

    fn serialize_struct_variant(
        self,
        _name: &'static str,
        _variant_index: u32,
        _variant: &'static str,
        _len: usize,
    ) -> Result<Self::SerializeStructVariant, Self::Error> {
        Err(ser::Error::custom(
            "struct variants cannot be represented as a single KDL value",
        ))
    }
}

struct RestSerializer<'a> {
    node: &'a mut KdlNode,
    used_indices: &'a HashSet<usize>,
}

impl<'a> ser::Serializer for &'a mut RestSerializer<'a> {
    type Ok = ();
    type Error = Error;

    type SerializeSeq = RestSeqSerializer<'a>;
    type SerializeTuple = RestSeqSerializer<'a>;
    type SerializeTupleStruct = RestSeqSerializer<'a>;
    type SerializeTupleVariant = ser::Impossible<(), Error>;
    type SerializeMap = ser::Impossible<(), Error>;
    type SerializeStruct = ser::Impossible<(), Error>;
    type SerializeStructVariant = ser::Impossible<(), Error>;

    fn serialize_seq(self, _: Option<usize>) -> Result<Self::SerializeSeq, Self::Error> {
        Ok(RestSeqSerializer {
            node: self.node,
            used_indices: self.used_indices,
            current_idx: 0,
        })
    }

    fn serialize_tuple(self, len: usize) -> Result<Self::SerializeTuple, Self::Error> {
        self.serialize_seq(Some(len))
    }

    fn serialize_tuple_struct(
        self,
        _: &'static str,
        len: usize,
    ) -> Result<Self::SerializeTupleStruct, Self::Error> {
        self.serialize_seq(Some(len))
    }

    fn serialize_bool(self, v: bool) -> Result<Self::Ok, Self::Error> {
        self.node.entries_mut().push(KdlEntry::new(v));
        Ok(())
    }

    fn serialize_i8(self, v: i8) -> Result<Self::Ok, Self::Error> {
        self.serialize_i64(v as i64)
    }
    fn serialize_i16(self, v: i16) -> Result<Self::Ok, Self::Error> {
        self.serialize_i64(v as i64)
    }
    fn serialize_i32(self, v: i32) -> Result<Self::Ok, Self::Error> {
        self.serialize_i64(v as i64)
    }
    fn serialize_i64(self, v: i64) -> Result<Self::Ok, Self::Error> {
        self.node
            .entries_mut()
            .push(KdlEntry::new(KdlValue::Integer(v as i128)));
        Ok(())
    }

    fn serialize_u8(self, v: u8) -> Result<Self::Ok, Self::Error> {
        self.serialize_u64(v as u64)
    }
    fn serialize_u16(self, v: u16) -> Result<Self::Ok, Self::Error> {
        self.serialize_u64(v as u64)
    }
    fn serialize_u32(self, v: u32) -> Result<Self::Ok, Self::Error> {
        self.serialize_u64(v as u64)
    }
    fn serialize_u64(self, v: u64) -> Result<Self::Ok, Self::Error> {
        self.node
            .entries_mut()
            .push(KdlEntry::new(KdlValue::Integer(v as i128)));
        Ok(())
    }

    fn serialize_f32(self, v: f32) -> Result<Self::Ok, Self::Error> {
        self.serialize_f64(v as f64)
    }
    fn serialize_f64(self, v: f64) -> Result<Self::Ok, Self::Error> {
        self.node
            .entries_mut()
            .push(KdlEntry::new(KdlValue::Float(v)));
        Ok(())
    }

    fn serialize_char(self, v: char) -> Result<Self::Ok, Self::Error> {
        self.serialize_str(&v.to_string())
    }

    fn serialize_str(self, v: &str) -> Result<Self::Ok, Self::Error> {
        self.node
            .entries_mut()
            .push(KdlEntry::new(KdlValue::String(v.to_string())));
        Ok(())
    }

    fn serialize_bytes(self, _: &[u8]) -> Result<Self::Ok, Self::Error> {
        Err(ser::Error::custom(
            "bytes cannot be represented as KDL arguments",
        ))
    }

    fn serialize_none(self) -> Result<Self::Ok, Self::Error> {
        self.node.entries_mut().push(KdlEntry::new(KdlValue::Null));
        Ok(())
    }

    fn serialize_some<T: ?Sized + Serialize>(self, value: &T) -> Result<Self::Ok, Self::Error> {
        value.serialize(self)
    }

    fn serialize_unit(self) -> Result<Self::Ok, Self::Error> {
        self.node.entries_mut().push(KdlEntry::new(KdlValue::Null));
        Ok(())
    }

    fn serialize_unit_struct(self, _: &'static str) -> Result<Self::Ok, Self::Error> {
        self.serialize_unit()
    }

    fn serialize_unit_variant(
        self,
        _: &'static str,
        _: u32,
        variant: &'static str,
    ) -> Result<Self::Ok, Self::Error> {
        self.serialize_str(variant)
    }

    fn serialize_newtype_struct<T: ?Sized + Serialize>(
        self,
        _: &'static str,
        value: &T,
    ) -> Result<Self::Ok, Self::Error> {
        value.serialize(self)
    }

    fn serialize_newtype_variant<T: ?Sized + Serialize>(
        self,
        _: &'static str,
        _: u32,
        _: &'static str,
        _: &T,
    ) -> Result<Self::Ok, Self::Error> {
        Err(ser::Error::custom(
            "newtype variants cannot be represented as KDL arguments",
        ))
    }

    fn serialize_map(self, _: Option<usize>) -> Result<Self::SerializeMap, Self::Error> {
        Err(ser::Error::custom(
            "maps are cannot be represented as KDL arguments",
        ))
    }

    fn serialize_struct(
        self,
        _: &'static str,
        _: usize,
    ) -> Result<Self::SerializeStruct, Self::Error> {
        Err(ser::Error::custom(
            "structs are cannot be represented as KDL arguments",
        ))
    }

    fn serialize_struct_variant(
        self,
        _: &'static str,
        _: u32,
        _: &'static str,
        _: usize,
    ) -> Result<Self::SerializeStructVariant, Self::Error> {
        Err(ser::Error::custom(
            "struct variants cannot be represented as KDL arguments",
        ))
    }

    fn serialize_tuple_variant(
        self,
        _: &'static str,
        _: u32,
        _: &'static str,
        _: usize,
    ) -> Result<Self::SerializeTupleVariant, Self::Error> {
        Err(ser::Error::custom(
            "tuple variants cannot be represented as KDL arguments",
        ))
    }
}

struct RestSeqSerializer<'a> {
    node: &'a mut KdlNode,
    used_indices: &'a HashSet<usize>,
    current_idx: usize,
}

impl<'a> ser::SerializeSeq for RestSeqSerializer<'a> {
    type Ok = ();
    type Error = Error;

    fn serialize_element<T: ?Sized + Serialize>(&mut self, value: &T) -> Result<(), Self::Error> {
        if !self.used_indices.contains(&self.current_idx) {
            let kdl_val = to_kdl_value(value)?;
            self.node.entries_mut().push(KdlEntry::new(kdl_val));
        }
        self.current_idx += 1;
        Ok(())
    }

    fn end(self) -> Result<Self::Ok, Self::Error> {
        Ok(())
    }
}

impl<'a> ser::SerializeTuple for RestSeqSerializer<'a> {
    type Ok = ();
    type Error = Error;

    fn serialize_element<T: ?Sized + Serialize>(&mut self, value: &T) -> Result<(), Self::Error> {
        ser::SerializeSeq::serialize_element(self, value)
    }

    fn end(self) -> Result<Self::Ok, Self::Error> {
        ser::SerializeSeq::end(self)
    }
}

impl<'a> ser::SerializeTupleStruct for RestSeqSerializer<'a> {
    type Ok = ();
    type Error = Error;

    fn serialize_field<T: ?Sized + Serialize>(&mut self, value: &T) -> Result<(), Self::Error> {
        ser::SerializeSeq::serialize_element(self, value)
    }

    fn end(self) -> Result<Self::Ok, Self::Error> {
        ser::SerializeSeq::end(self)
    }
}

struct ArgsSerializer<'a> {
    node: &'a mut KdlNode,
}

impl<'a> ser::Serializer for &'a mut ArgsSerializer<'a> {
    type Ok = ();
    type Error = Error;

    type SerializeSeq = ArgsSeqSerializer<'a>;
    type SerializeTuple = ArgsSeqSerializer<'a>;
    type SerializeTupleStruct = ArgsSeqSerializer<'a>;
    type SerializeTupleVariant = ser::Impossible<(), Error>;
    type SerializeMap = ser::Impossible<(), Error>;
    type SerializeStruct = ser::Impossible<(), Error>;
    type SerializeStructVariant = ser::Impossible<(), Error>;

    fn serialize_seq(self, _: Option<usize>) -> Result<Self::SerializeSeq, Self::Error> {
        Ok(ArgsSeqSerializer { node: self.node })
    }

    fn serialize_tuple(self, len: usize) -> Result<Self::SerializeTuple, Self::Error> {
        self.serialize_seq(Some(len))
    }

    fn serialize_tuple_struct(
        self,
        _: &'static str,
        len: usize,
    ) -> Result<Self::SerializeTupleStruct, Self::Error> {
        self.serialize_seq(Some(len))
    }

    fn serialize_bool(self, v: bool) -> Result<Self::Ok, Self::Error> {
        self.node.entries_mut().push(KdlEntry::new(v));
        Ok(())
    }

    fn serialize_i8(self, v: i8) -> Result<Self::Ok, Self::Error> {
        self.serialize_i64(v as i64)
    }
    fn serialize_i16(self, v: i16) -> Result<Self::Ok, Self::Error> {
        self.serialize_i64(v as i64)
    }
    fn serialize_i32(self, v: i32) -> Result<Self::Ok, Self::Error> {
        self.serialize_i64(v as i64)
    }
    fn serialize_i64(self, v: i64) -> Result<Self::Ok, Self::Error> {
        self.node
            .entries_mut()
            .push(KdlEntry::new(KdlValue::Integer(v as i128)));
        Ok(())
    }

    fn serialize_u8(self, v: u8) -> Result<Self::Ok, Self::Error> {
        self.serialize_u64(v as u64)
    }
    fn serialize_u16(self, v: u16) -> Result<Self::Ok, Self::Error> {
        self.serialize_u64(v as u64)
    }
    fn serialize_u32(self, v: u32) -> Result<Self::Ok, Self::Error> {
        self.serialize_u64(v as u64)
    }
    fn serialize_u64(self, v: u64) -> Result<Self::Ok, Self::Error> {
        self.node
            .entries_mut()
            .push(KdlEntry::new(KdlValue::Integer(v as i128)));
        Ok(())
    }

    fn serialize_f32(self, v: f32) -> Result<Self::Ok, Self::Error> {
        self.serialize_f64(v as f64)
    }
    fn serialize_f64(self, v: f64) -> Result<Self::Ok, Self::Error> {
        self.node
            .entries_mut()
            .push(KdlEntry::new(KdlValue::Float(v)));
        Ok(())
    }

    fn serialize_char(self, v: char) -> Result<Self::Ok, Self::Error> {
        self.serialize_str(&v.to_string())
    }

    fn serialize_str(self, v: &str) -> Result<Self::Ok, Self::Error> {
        self.node
            .entries_mut()
            .push(KdlEntry::new(KdlValue::String(v.to_string())));
        Ok(())
    }

    fn serialize_bytes(self, _: &[u8]) -> Result<Self::Ok, Self::Error> {
        Err(ser::Error::custom(
            "bytes cannot be represented as KDL arguments",
        ))
    }

    fn serialize_none(self) -> Result<Self::Ok, Self::Error> {
        self.node.entries_mut().push(KdlEntry::new(KdlValue::Null));
        Ok(())
    }

    fn serialize_some<T: ?Sized + Serialize>(self, value: &T) -> Result<Self::Ok, Self::Error> {
        value.serialize(self)
    }

    fn serialize_unit(self) -> Result<Self::Ok, Self::Error> {
        self.node.entries_mut().push(KdlEntry::new(KdlValue::Null));
        Ok(())
    }

    fn serialize_unit_struct(self, _: &'static str) -> Result<Self::Ok, Self::Error> {
        self.serialize_unit()
    }

    fn serialize_unit_variant(
        self,
        _: &'static str,
        _: u32,
        variant: &'static str,
    ) -> Result<Self::Ok, Self::Error> {
        self.serialize_str(variant)
    }

    fn serialize_newtype_struct<T: ?Sized + Serialize>(
        self,
        _: &'static str,
        value: &T,
    ) -> Result<Self::Ok, Self::Error> {
        value.serialize(self)
    }

    fn serialize_newtype_variant<T: ?Sized + Serialize>(
        self,
        _: &'static str,
        _: u32,
        _: &'static str,
        _: &T,
    ) -> Result<Self::Ok, Self::Error> {
        Err(ser::Error::custom(
            "newtype variants cannot be represented as KDL arguments",
        ))
    }

    fn serialize_map(self, _: Option<usize>) -> Result<Self::SerializeMap, Self::Error> {
        Err(ser::Error::custom(
            "maps are cannot be represented as KDL arguments",
        ))
    }

    fn serialize_struct(
        self,
        _: &'static str,
        _: usize,
    ) -> Result<Self::SerializeStruct, Self::Error> {
        Err(ser::Error::custom(
            "structs are cannot be represented as KDL arguments",
        ))
    }

    fn serialize_struct_variant(
        self,
        _: &'static str,
        _: u32,
        _: &'static str,
        _: usize,
    ) -> Result<Self::SerializeStructVariant, Self::Error> {
        Err(ser::Error::custom(
            "struct variants cannot be represented as KDL arguments",
        ))
    }

    fn serialize_tuple_variant(
        self,
        _: &'static str,
        _: u32,
        _: &'static str,
        _: usize,
    ) -> Result<Self::SerializeTupleVariant, Self::Error> {
        Err(ser::Error::custom(
            "tuple variants cannot be represented as KDL arguments",
        ))
    }
}

struct ArgsSeqSerializer<'a> {
    node: &'a mut KdlNode,
}

impl<'a> ser::SerializeSeq for ArgsSeqSerializer<'a> {
    type Ok = ();
    type Error = Error;

    fn serialize_element<T: ?Sized + Serialize>(&mut self, value: &T) -> Result<(), Self::Error> {
        let kdl_val = to_kdl_value(value)?;
        self.node.entries_mut().push(KdlEntry::new(kdl_val));
        Ok(())
    }

    fn end(self) -> Result<Self::Ok, Self::Error> {
        Ok(())
    }
}

impl<'a> ser::SerializeTuple for ArgsSeqSerializer<'a> {
    type Ok = ();
    type Error = Error;

    fn serialize_element<T: ?Sized + Serialize>(&mut self, value: &T) -> Result<(), Self::Error> {
        ser::SerializeSeq::serialize_element(self, value)
    }

    fn end(self) -> Result<Self::Ok, Self::Error> {
        ser::SerializeSeq::end(self)
    }
}

impl<'a> ser::SerializeTupleStruct for ArgsSeqSerializer<'a> {
    type Ok = ();
    type Error = Error;

    fn serialize_field<T: ?Sized + Serialize>(&mut self, value: &T) -> Result<(), Self::Error> {
        ser::SerializeSeq::serialize_element(self, value)
    }

    fn end(self) -> Result<Self::Ok, Self::Error> {
        ser::SerializeSeq::end(self)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use serde::Serialize;

    #[test]
    fn simple_struct() {
        #[derive(Serialize)]
        struct Config {
            name: String,
            port: u16,
        }

        let config = Config {
            name: "my-app".into(),
            port: 8080,
        };
        let kdl = to_string(&config).unwrap();
        assert_eq!(kdl, "name my-app\nport 8080\n");
    }

    #[test]
    fn nested_struct() {
        #[derive(Serialize)]
        struct Config {
            server: Server,
        }

        #[derive(Serialize)]
        struct Server {
            host: String,
            port: u16,
        }

        let config = Config {
            server: Server {
                host: "localhost".into(),
                port: 8080,
            },
        };
        let kdl = to_string(&config).unwrap();

        assert!(kdl.contains("server"));
        assert!(kdl.contains("host localhost"));
        assert!(kdl.contains("port 8080"));
    }

    #[test]
    fn boolean_and_null() {
        #[derive(Serialize)]
        struct Config {
            enabled: bool,
            disabled: bool,
            something: Option<usize>,
            nothing: Option<usize>,
        }

        let config = Config {
            enabled: true,
            disabled: false,
            something: Some(1),
            nothing: None,
        };
        let kdl = to_string(&config).unwrap();
        assert!(kdl.contains("enabled"));
        assert!(!kdl.contains("disabled"));
        assert!(kdl.contains("something 1"));
        assert!(!kdl.contains("nothing"));
    }

    #[test]
    fn option_some() {
        #[derive(Serialize)]
        struct Config {
            name: Option<String>,
        }

        let config = Config {
            name: Some("hello".into()),
        };
        let kdl = to_string(&config).unwrap();
        assert!(kdl.contains("name hello"));
    }

    #[test]
    fn unit_enum() {
        #[derive(Serialize)]
        enum Color {
            Red,
        }

        #[derive(Serialize)]
        struct Config {
            color: Color,
        }

        let config = Config { color: Color::Red };
        let kdl = to_string(&config).unwrap();
        assert!(kdl.contains("color Red"));
    }

    #[test]
    fn float_value() {
        #[derive(Serialize)]
        struct Config {
            ratio: f64,
        }

        let config = Config { ratio: 3.14 };
        let kdl = to_string(&config).unwrap();
        assert!(kdl.contains("ratio"));
        assert!(kdl.contains("3.14"));
    }

    #[test]
    fn hashmap() {
        use std::collections::BTreeMap;

        let mut map = BTreeMap::new();
        map.insert("alpha".to_string(), 1);
        map.insert("beta".to_string(), 2);
        let kdl = to_string(&map).unwrap();
        assert!(kdl.contains("alpha 1"));
        assert!(kdl.contains("beta 2"));
    }

    #[test]
    fn newtype_struct() {
        #[derive(Serialize)]
        struct Port(u16);

        #[derive(Serialize)]
        struct Config {
            port: Port,
        }

        let config = Config { port: Port(8080) };
        let kdl = to_string(&config).unwrap();
        assert!(kdl.contains("port 8080"));
    }

    #[test]
    fn to_document_roundtrip() {
        #[derive(Serialize)]
        struct Config {
            name: String,
            port: u16,
        }

        let config = Config {
            name: "test".into(),
            port: 3000,
        };
        let doc = to_document(&config).unwrap();
        assert_eq!(doc.nodes().len(), 2);
        assert_eq!(doc.get("name").unwrap().get(0), Some(&"test".into()));
        assert_eq!(doc.get("port").unwrap().get(0), Some(&3000.into()));
    }

    #[test]
    fn rename_props() {
        #[derive(Serialize)]
        struct Server {
            #[serde(rename = "#@host")]
            host: String,
            #[serde(rename = "#@port")]
            port: u16,
        }

        #[derive(Serialize)]
        struct Config {
            server: Server,
        }

        let config = Config {
            server: Server {
                host: "localhost".into(),
                port: 8080,
            },
        };
        let kdl = to_string(&config).unwrap();
        assert!(kdl.contains("server"));
        assert!(kdl.contains("host=localhost"));
        assert!(kdl.contains("port=8080"));
    }

    #[test]
    fn rename_args() {
        #[derive(Serialize)]
        struct Server {
            #[serde(rename = "#0")]
            host: String,
            #[serde(rename = "#@port")]
            port: u16,
        }

        #[derive(Serialize)]
        struct Config {
            server: Server,
        }

        let config = Config {
            server: Server {
                host: "localhost".into(),
                port: 8080,
            },
        };
        let kdl = to_string(&config).unwrap();
        assert!(kdl.contains("server"));
        assert!(kdl.contains("localhost"));
        assert!(kdl.contains("port=8080"));
    }

    #[test]
    fn rename_all_args() {
        #[derive(Serialize)]
        struct Command {
            #[serde(rename = "#0")]
            name: String,
            #[serde(rename = "#args")]
            args: Vec<String>,
            #[serde(rename = "#rest")]
            rest: Vec<String>,
        }

        #[derive(Serialize)]
        struct Config {
            command: Command,
        }

        let config = Config {
            command: Command {
                name: "run".into(),
                args: vec!["run".into(), "--verbose".into(), "--output".into()],
                rest: vec!["--verbose".into(), "--output".into()],
            },
        };
        let kdl = to_string(&config).unwrap();
        assert!(kdl.contains("command"));
        assert!(kdl.contains("run"));
        assert!(kdl.contains("--verbose"));
        assert!(kdl.contains("--output"));
    }

    #[test]
    fn complex_enum() {
        #[derive(Serialize)]
        struct Config {
            command: Command,
        }

        #[derive(Serialize)]
        #[serde(rename_all = "kebab-case")]
        enum Command {
            #[serde(rename_all = "kebab-case")]
            Up {
                #[serde(rename = "#0")]
                towards: String,
                how_high: Option<usize>,
                enabled: bool,
            },
            Down(usize),
            Left(usize, usize),
            Right,
        }

        let config = Config {
            command: Command::Up {
                towards: "sky".into(),
                how_high: Some(1),
                enabled: false,
            },
        };
        let kdl = to_string(&config).unwrap();
        assert!(kdl.contains("command sky"));
        assert!(kdl.contains("up"));
        assert!(kdl.contains("how-high 1"));

        let config = Config {
            command: Command::Down(4),
        };
        let kdl = to_string(&config).unwrap();
        assert!(kdl.contains("down 4"));

        let config = Config {
            command: Command::Left(1, 2),
        };
        let kdl = to_string(&config).unwrap();
        assert!(kdl.contains("left 1 2"));

        let config = Config {
            command: Command::Right,
        };
        let kdl = to_string(&config).unwrap();
        assert_eq!(kdl, "command right\n");
    }
}

#[test]
fn rename_rest_args() {}
