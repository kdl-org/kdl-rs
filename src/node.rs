use std::{
    fmt::Display,
    ops::{Index, IndexMut},
    str::FromStr,
};

use crate::{parser, KdlDocument, KdlEntry, KdlError, KdlIdentifier, KdlValue};

/// Represents an individual KDL
/// [`Node`](https://github.com/kdl-org/kdl/blob/main/SPEC.md#node) inside a
/// KDL Document.
#[derive(Debug, Clone, PartialEq)]
pub struct KdlNode {
    pub(crate) leading: Option<String>,
    pub(crate) ty: Option<KdlIdentifier>,
    pub(crate) name: KdlIdentifier,
    // TODO: consider using `hashlink` for this instead, later.
    pub(crate) entries: Vec<KdlEntry>,
    pub(crate) before_children: Option<String>,
    pub(crate) children: Option<KdlDocument>,
    pub(crate) trailing: Option<String>,
}

impl KdlNode {
    /// Creates a new KdlNode with a given name.
    pub fn new(name: impl Into<KdlIdentifier>) -> Self {
        Self {
            name: name.into(),
            leading: None,
            ty: None,
            entries: Vec::new(),
            before_children: None,
            children: None,
            trailing: None,
        }
    }

    /// Gets this node's name.
    pub fn name(&self) -> &KdlIdentifier {
        &self.name
    }

    /// Gets a mutable reference to this node's name.
    pub fn name_mut(&mut self) -> &mut KdlIdentifier {
        &mut self.name
    }

    /// Sets this node's name.
    pub fn set_name(&mut self, name: impl Into<KdlIdentifier>) {
        self.name = name.into();
    }

    /// Gets the node's type identifier, if any.
    pub fn ty(&self) -> Option<&KdlIdentifier> {
        self.ty.as_ref()
    }

    /// Gets a mutable reference to the node's type identifier.
    pub fn ty_mut(&mut self) -> &mut Option<KdlIdentifier> {
        &mut self.ty
    }

    /// Sets the node's type identifier.
    pub fn set_ty(&mut self, ty: impl Into<KdlIdentifier>) {
        self.ty = Some(ty.into());
    }

    /// Returns a reference to this node's entries (arguments and properties).
    pub fn entries(&self) -> &[KdlEntry] {
        &self.entries
    }

    /// Returns a mutable reference to this node's entries (arguments and
    /// properties).
    pub fn entries_mut(&mut self) -> &mut Vec<KdlEntry> {
        &mut self.entries
    }

    /// Gets leading text (whitespace, comments) for this node.
    pub fn leading(&self) -> Option<&str> {
        self.leading.as_deref()
    }

    /// Sets leading text (whitespace, comments) for this node.
    pub fn set_leading(&mut self, leading: impl Into<String>) {
        self.leading = Some(leading.into());
    }

    /// Gets text (whitespace, comments) right before the children block's starting `{`.
    pub fn before_children(&self) -> Option<&str> {
        self.before_children.as_deref()
    }

    /// Gets text (whitespace, comments) right before the children block's starting `{`.
    pub fn set_before_children(&mut self, before: impl Into<String>) {
        self.before_children = Some(before.into());
    }

    /// Gets trailing text (whitespace, comments) for this node.
    pub fn trailing(&self) -> Option<&str> {
        self.trailing.as_deref()
    }

    /// Sets trailing text (whitespace, comments) for this node.
    pub fn set_trailing(&mut self, trailing: impl Into<String>) {
        self.trailing = Some(trailing.into());
    }

    /// Length of this node when rendered as a string.
    pub fn len(&self) -> usize {
        format!("{}", self).len()
    }

    /// Returns true if this node is completely empty (including whitespace).
    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    /// Clears leading and trailing text (whitespace, comments), as well as
    /// the space before the children block, if any. Individual entries and
    /// their formatting will be preserved.
    ///
    /// If you want to clear formatting on all children and entries as well,
    /// use [`Self::clear_fmt_recursive`].
    pub fn clear_fmt(&mut self) {
        self.leading = None;
        self.trailing = None;
        self.before_children = None;
    }

    /// Clears leading and trailing text (whitespace, comments), as well as
    /// the space before the children block, if any. Individual entries and
    /// children formatting will also be cleared.
    pub fn clear_fmt_recursive(&mut self) {
        self.clear_fmt();
        self.name.clear_fmt();
        if let Some(children) = &mut self.children {
            children.clear_fmt_recursive();
        }
        for entry in self.entries.iter_mut() {
            entry.clear_fmt();
        }
    }

    /// Fetches an entry by key. Number keys will look up arguments, strings
    /// will look up properties.
    pub fn get(&self, key: impl Into<NodeKey>) -> Option<&KdlEntry> {
        self.get_impl(key.into())
    }

    fn get_impl(&self, key: NodeKey) -> Option<&KdlEntry> {
        match key {
            NodeKey::Key(key) => {
                let mut current = None;
                for entry in &self.entries {
                    if entry.name.is_some()
                        && entry.name.as_ref().map(|i| i.value()) == Some(key.value())
                    {
                        current = Some(entry);
                    }
                }
                current
            }
            NodeKey::Index(idx) => {
                let mut current_idx = 0;
                for entry in &self.entries {
                    if entry.name.is_none() {
                        if current_idx == idx {
                            return Some(entry);
                        }
                        current_idx += 1;
                        if current_idx > idx + 1 {
                            return None;
                        }
                    }
                }
                None
            }
        }
    }

    /// Fetches a mutable referene to an entry by key. Number keys will look
    /// up arguments, strings will look up properties.
    pub fn get_mut(&mut self, key: impl Into<NodeKey>) -> Option<&mut KdlEntry> {
        self.get_mut_impl(key.into())
    }

    fn get_mut_impl(&mut self, key: NodeKey) -> Option<&mut KdlEntry> {
        match key {
            NodeKey::Key(key) => {
                let mut current = None;
                for entry in &mut self.entries {
                    if entry.name.is_some()
                        && entry.name.as_ref().map(|i| i.value()) == Some(key.value())
                    {
                        current = Some(entry);
                    }
                }
                current
            }
            NodeKey::Index(idx) => {
                let mut current_idx = 0;
                for entry in &mut self.entries {
                    if entry.name.is_none() {
                        if current_idx >= idx {
                            return Some(entry);
                        }
                        current_idx += 1;
                        if current_idx >= idx {
                            return None;
                        }
                    }
                }
                None
            }
        }
    }

    /// Inserts an entry into this node. If an entry already exists with the
    /// same key, it will be replaced and the previous entry will be returned.
    ///
    /// Numerical keys will insert arguments, string keys will insert
    /// properties.
    pub fn insert(
        &mut self,
        key: impl Into<NodeKey>,
        entry: impl Into<KdlEntry>,
    ) -> Option<KdlEntry> {
        self.insert_impl(key.into(), entry.into())
    }

    fn insert_impl(&mut self, key: NodeKey, mut entry: KdlEntry) -> Option<KdlEntry> {
        match key {
            NodeKey::Key(ref key_val) => {
                if entry.name.is_none() {
                    entry.name = Some(key_val.clone());
                }
                if entry.name.as_ref().map(|i| i.value()) != Some(key_val.value()) {
                    panic!("Property name mismatch");
                }
                if let Some(existing) = self.get_mut(key) {
                    std::mem::swap(existing, &mut entry);
                    Some(entry)
                } else {
                    self.entries.push(entry);
                    None
                }
            }
            NodeKey::Index(idx) => {
                if entry.name.is_some() {
                    panic!("Cannot insert property with name under a numerical key");
                }
                if let Some(existing) = self.get_mut(key) {
                    std::mem::swap(existing, &mut entry);
                    Some(entry)
                } else {
                    let mut current_idx = 0;
                    for existing in &mut self.entries {
                        if existing.name.is_none() {
                            if current_idx == idx {
                                std::mem::swap(existing, &mut entry);
                                return Some(entry);
                            }
                            current_idx += 1;
                            if current_idx >= idx {
                                break;
                            }
                        }
                    }
                    if idx > current_idx {
                        panic!(
                            "Insertion index (is {}) should be <= len (is {})",
                            idx, current_idx
                        );
                    } else {
                        self.entries.push(entry);
                        None
                    }
                }
            }
        }
    }

    /// Removes an entry from this node. If an entry already exists with the
    /// same key, it will be returned.
    ///
    /// Numerical keys will remove arguments, string keys will remove
    /// properties.
    pub fn remove(&mut self, key: impl Into<NodeKey>) -> Option<KdlEntry> {
        self.remove_impl(key.into())
    }

    fn remove_impl(&mut self, key: NodeKey) -> Option<KdlEntry> {
        match key {
            NodeKey::Key(key) => {
                for (idx, entry) in self.entries.iter_mut().enumerate() {
                    if entry.name.is_some() && entry.name.as_ref() == Some(&key) {
                        return Some(self.entries.remove(idx));
                    }
                }
                None
            }
            NodeKey::Index(idx) => {
                let mut current_idx = 0;
                for entry in &mut self.entries {
                    if entry.name.is_none() {
                        if current_idx == idx {
                            return Some(self.entries.remove(idx));
                        }
                        current_idx += 1;
                        if current_idx >= idx {
                            return None;
                        }
                    }
                }
                None
            }
        }
    }

    /// Shorthand for `self.entries_mut().push(entry)`.
    pub fn push(&mut self, entry: impl Into<KdlEntry>) {
        self.entries.push(entry.into());
    }

    /// Shorthand for `self.entries_mut().clear()`
    pub fn clear_entries(&mut self) {
        self.entries.clear();
    }

    /// Returns a reference to this node's children, if any.
    pub fn children(&self) -> Option<&KdlDocument> {
        self.children.as_ref()
    }

    /// Returns a mutable reference to this node's children, if any.
    pub fn children_mut(&mut self) -> &mut Option<KdlDocument> {
        &mut self.children
    }

    /// Sets the KdlDocument representing this node's children.
    pub fn set_children(&mut self, children: KdlDocument) {
        self.children = Some(children);
    }

    /// Removes this node's children completely.
    pub fn clear_children(&mut self) {
        self.children = None;
    }

    /// Returns a mutable reference to this node's children [`KdlDocument`],
    /// creating one first if one does not already exist.
    pub fn ensure_children(&mut self) -> &mut KdlDocument {
        if self.children.is_none() {
            self.children = Some(KdlDocument::new());
        }
        self.children_mut().as_mut().unwrap()
    }

    /// Auto-formats this node and its contents.
    pub fn fmt(&mut self) {
        self.fmt_impl(0, false);
    }

    /// Auto-formats this node and its contents, stripping comments.
    pub fn fmt_no_comments(&mut self) {
        self.fmt_impl(0, true);
    }
}

/// Represents a [`KdlNode`]'s entry key.
#[derive(Debug, Clone, PartialEq)]
pub enum NodeKey {
    /// Key for a node property entry.
    Key(KdlIdentifier),
    /// Index for a node argument entry (positional value).
    Index(usize),
}

impl From<&str> for NodeKey {
    fn from(key: &str) -> Self {
        NodeKey::Key(key.into())
    }
}

impl From<String> for NodeKey {
    fn from(key: String) -> Self {
        NodeKey::Key(key.into())
    }
}

impl From<usize> for NodeKey {
    fn from(key: usize) -> Self {
        NodeKey::Index(key)
    }
}

impl Index<usize> for KdlNode {
    type Output = KdlValue;

    fn index(&self, index: usize) -> &Self::Output {
        self.get(index).expect("Argument out of range.").value()
    }
}

impl IndexMut<usize> for KdlNode {
    fn index_mut(&mut self, index: usize) -> &mut Self::Output {
        self.get_mut(index)
            .expect("Argument out of range.")
            .value_mut()
    }
}

impl Index<&str> for KdlNode {
    type Output = KdlValue;

    fn index(&self, key: &str) -> &Self::Output {
        self.get(key).expect("No such property.").value()
    }
}

impl IndexMut<&str> for KdlNode {
    fn index_mut(&mut self, key: &str) -> &mut Self::Output {
        if self.get(key).is_none() {
            self.push((key, KdlValue::Null));
        }
        self.get_mut(key)
            .expect("Something went wrong.")
            .value_mut()
    }
}

impl FromStr for KdlNode {
    type Err = KdlError;

    fn from_str(input: &str) -> Result<Self, Self::Err> {
        parser::parse(input, parser::node)
    }
}

impl Display for KdlNode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.stringify(f, 0)
    }
}

impl KdlNode {
    pub(crate) fn fmt_impl(&mut self, indent: usize, no_comments: bool) {
        if let Some(s) = self.leading.as_mut() {
            crate::fmt::fmt_leading(s, indent, no_comments);
        }
        if let Some(s) = self.trailing.as_mut() {
            crate::fmt::fmt_trailing(s, no_comments);
            if s.starts_with(';') {
                s.remove(0);
            }
            if let Some(c) = s.chars().next() {
                if !c.is_whitespace() {
                    s.insert(0, ' ');
                }
            }
            s.push('\n');
        }
        self.before_children = None;
        self.name.clear_fmt();
        if let Some(ty) = self.ty.as_mut() {
            ty.clear_fmt()
        }
        for entry in &mut self.entries {
            entry.fmt();
        }
        if let Some(children) = self.children.as_mut() {
            children.fmt_impl(indent + 4, no_comments);
            if let Some(leading) = children.leading.as_mut() {
                leading.push('\n');
            }
            if let Some(trailing) = children.trailing.as_mut() {
                trailing.push_str(format!("{:indent$}", "", indent = indent).as_str());
            }
        }
    }

    pub(crate) fn stringify(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        indent: usize,
    ) -> std::fmt::Result {
        if let Some(leading) = &self.leading {
            write!(f, "{}", leading)?;
        } else {
            write!(f, "{:indent$}", "", indent = indent)?;
        }
        if let Some(ty) = &self.ty {
            write!(f, "({})", ty)?;
        }
        write!(f, "{}", self.name)?;
        let mut space_before_children = true;
        for entry in &self.entries {
            if entry.leading.is_none() {
                write!(f, " ")?;
            }
            write!(f, "{}", entry)?;
            space_before_children = entry.trailing.is_none();
        }
        if let Some(children) = &self.children {
            if let Some(before) = self.before_children() {
                write!(f, "{}", before)?;
            } else if space_before_children {
                write!(f, " ")?;
            }
            write!(f, "{{")?;
            if children.leading.is_none() {
                writeln!(f)?;
            }
            children.stringify(f, indent + 4)?;
            write!(f, "}}")?;
        }
        if let Some(trailing) = &self.trailing {
            write!(f, "{}", trailing)?;
        }
        Ok(())
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn parsing() -> miette::Result<()> {
        let node: KdlNode = "\n\t  (\"ty\")\"node\" 0xDEADbeef;\n".parse()?;
        assert_eq!(node.leading(), Some("\n\t  "));
        assert_eq!(node.trailing(), Some(";\n"));
        assert_eq!(node.ty(), Some(&"\"ty\"".parse()?));
        assert_eq!(node.name(), &"\"node\"".parse()?);
        assert_eq!(node.get(0), Some(&"0xDEADbeef".parse()?));

        r#"
        node "test" {
            link "blah" anything="self"
        }"#
        .parse::<KdlNode>()?;
        Ok(())
    }

    #[test]
    fn indexing() {
        let mut node = KdlNode::new("foo");
        node.push("bar");
        node["foo"] = 1.into();

        assert_eq!(node[0], "bar".into());
        assert_eq!(node["foo"], 1.into());

        node[0] = false.into();
        node["foo"] = KdlValue::Null;

        assert_eq!(node[0], false.into());
        assert_eq!(node["foo"], KdlValue::Null);

        node.entries_mut().push(KdlEntry::new_prop("x", 1));
        node.entries_mut().push(KdlEntry::new_prop("x", 2));
        assert_eq!(&node["x"], &2.into())
    }
}
