use std::{fmt::Display, str::FromStr};

use crate::{parser, KdlError, KdlIdentifier, KdlValue};

/// KDL Entries are the "arguments" to KDL nodes: either a (positional)
/// [`Argument`](https://github.com/kdl-org/kdl/blob/main/SPEC.md#argument) or
/// a (key/value)
/// [`Property`](https://github.com/kdl-org/kdl/blob/main/SPEC.md#property)
#[derive(Debug, Clone, PartialEq)]
pub struct KdlEntry {
    pub(crate) leading: Option<String>,
    pub(crate) ty: Option<KdlIdentifier>,
    pub(crate) value: KdlValue,
    pub(crate) value_repr: Option<String>,
    pub(crate) name: Option<KdlIdentifier>,
    pub(crate) trailing: Option<String>,
}

impl KdlEntry {
    /// Creates a new Argument (positional) KdlEntry.
    pub fn new(value: impl Into<KdlValue>) -> Self {
        KdlEntry {
            leading: None,
            ty: None,
            value: value.into(),
            value_repr: None,
            name: None,
            trailing: None,
        }
    }

    /// Gets a reference to this entry's name, if it's a property entry.
    pub fn name(&self) -> Option<&KdlIdentifier> {
        self.name.as_ref()
    }

    /// Gets the entry's value.
    pub fn value(&self) -> &KdlValue {
        &self.value
    }

    /// Gets a mutable reference to this entry's value.
    pub fn value_mut(&mut self) -> &mut KdlValue {
        &mut self.value
    }

    /// Sets the entry's value.
    pub fn set_value(&mut self, value: impl Into<KdlValue>) {
        self.value = value.into();
    }

    /// Gets the entry's type.
    pub fn ty(&self) -> Option<&KdlIdentifier> {
        self.ty.as_ref()
    }

    /// Gets a mutable reference to this entry's type.
    pub fn ty_mut(&mut self) -> Option<&mut KdlIdentifier> {
        self.ty.as_mut()
    }

    /// Sets the entry's type.
    pub fn set_ty(&mut self, ty: impl Into<KdlIdentifier>) {
        self.ty = Some(ty.into());
    }

    /// Creates a new Property (key/value) KdlEntry.
    pub fn new_prop(key: impl Into<KdlIdentifier>, value: impl Into<KdlValue>) -> Self {
        KdlEntry {
            leading: None,
            ty: None,
            value: value.into(),
            value_repr: None,
            name: Some(key.into()),
            trailing: None,
        }
    }

    /// Gets leading text (whitespace, comments) for this KdlEntry.
    pub fn leading(&self) -> Option<&str> {
        self.leading.as_deref()
    }

    /// Gets leading text (whitespace, comments) for this KdlEntry,
    /// and starts tracking it if it was not already being tracked.
    pub fn leading_mut(&mut self) -> &mut String {
        self.leading.get_or_insert(String::new())
    }

    /// Sets leading text (whitespace, comments) for this KdlEntry.
    pub fn set_leading(&mut self, leading: impl Into<String>) {
        self.leading = Some(leading.into());
    }

    /// Gets trailing text (whitespace, comments) for this KdlEntry.
    pub fn trailing(&self) -> Option<&str> {
        self.trailing.as_deref()
    }

    /// Gets trailing text (whitespace, comments) for this KdlEntry,
    /// and starts tracking it if it was not already being tracked.
    pub fn trailing_mut(&mut self) -> &mut String {
        self.trailing.get_or_insert(String::new())
    }

    /// Sets trailing text (whitespace, comments) for this KdlEntry.
    pub fn set_trailing(&mut self, trailing: impl Into<String>) {
        self.trailing = Some(trailing.into());
    }

    /// Clears leading and trailing text (whitespace, comments), as well as
    /// resetting this entry's value to its default representation.
    pub fn clear_fmt(&mut self) {
        self.leading = None;
        self.trailing = None;
        self.value_repr = None;
        if let Some(ty) = &mut self.ty {
            ty.clear_fmt();
        }
        if let Some(name) = &mut self.name {
            name.clear_fmt();
        }
    }

    /// Gets the custom string representation for this KdlEntry's [`KdlValue`].
    pub fn value_repr(&self) -> Option<&str> {
        self.value_repr.as_deref()
    }

    /// Gets the custom string representation for this KdlEntry's [`KdlValue`],
    /// and starts tracking it if it was not already being tracked.
    pub fn value_repr_mut(&mut self) -> &mut String {
        self.value_repr
            .get_or_insert_with(|| format!("{}", self.value))
    }

    /// Sets a custom string representation for this KdlEntry's [`KdlValue`].
    pub fn set_value_repr(&mut self, repr: impl Into<String>) {
        self.value_repr = Some(repr.into());
    }

    /// Length of this entry when rendered as a string.
    pub fn len(&self) -> usize {
        format!("{}", self).len()
    }

    /// Returns true if this entry is completely empty (including whitespace).
    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    /// Auto-formats this entry.
    pub fn fmt(&mut self) {
        self.leading = Some(" ".into());
        self.trailing = Some(String::new());
        self.value_repr = None;
        if let Some(name) = &mut self.name {
            name.fmt();
        }
    }
}

impl Display for KdlEntry {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Some(leading) = self.leading() {
            write!(f, "{}", leading)?;
        }
        if let Some(name) = self.name() {
            write!(f, "{}=", name)?;
        }
        if let Some(ty) = self.ty() {
            write!(f, "({})", ty)?;
        }
        if let Some(repr) = self.value_repr() {
            write!(f, "{}", repr)?;
        } else {
            write!(f, "{}", self.value)?;
        }
        if let Some(trailing) = self.trailing() {
            write!(f, "{}", trailing)?;
        }
        Ok(())
    }
}

impl<T> From<T> for KdlEntry
where
    T: Into<KdlValue>,
{
    fn from(value: T) -> Self {
        KdlEntry::new(value)
    }
}

impl<K, V> From<(K, V)> for KdlEntry
where
    K: Into<KdlIdentifier>,
    V: Into<KdlValue>,
{
    fn from((key, value): (K, V)) -> Self {
        KdlEntry::new_prop(key, value)
    }
}

impl FromStr for KdlEntry {
    type Err = KdlError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        parser::parse(s, parser::entry_with_trailing)
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use pretty_assertions::assert_eq;

    #[test]
    fn reset_value_repr() -> miette::Result<()> {
        let mut left_entry: KdlEntry = "   name=1.03e2".parse()?;
        let mut right_entry: KdlEntry = "   name=103.0".parse()?;
        assert_ne!(left_entry, right_entry);
        left_entry.clear_fmt();
        right_entry.clear_fmt();
        assert_eq!(left_entry, right_entry);
        Ok(())
    }

    #[test]
    fn new() {
        let entry = KdlEntry::new(42);
        assert_eq!(
            entry,
            KdlEntry {
                leading: None,
                ty: None,
                value: KdlValue::Base10(42),
                value_repr: None,
                name: None,
                trailing: None,
            }
        );

        let entry = KdlEntry::new_prop("name", 42);
        assert_eq!(
            entry,
            KdlEntry {
                leading: None,
                ty: None,
                value: KdlValue::Base10(42),
                value_repr: None,
                name: Some("name".into()),
                trailing: None,
            }
        );
    }

    #[test]
    fn parsing() -> miette::Result<()> {
        let entry: KdlEntry = " \\\n (\"m\\\"eh\")0xDEADbeef\t\\\n".parse()?;
        assert_eq!(
            entry,
            KdlEntry {
                leading: Some(" \\\n ".into()),
                ty: Some("\"m\\\"eh\"".parse()?),
                value: KdlValue::Base16(0xdeadbeef),
                value_repr: Some("0xDEADbeef".into()),
                name: None,
                trailing: Some("\t\\\n".into()),
            }
        );

        let entry: KdlEntry = " \\\n \"foo\"=(\"m\\\"eh\")0xDEADbeef\t\\\n".parse()?;
        assert_eq!(
            entry,
            KdlEntry {
                leading: Some(" \\\n ".into()),
                ty: Some("\"m\\\"eh\"".parse()?),
                value: KdlValue::Base16(0xdeadbeef),
                value_repr: Some("0xDEADbeef".into()),
                name: Some("\"foo\"".parse()?),
                trailing: Some("\t\\\n".into()),
            }
        );

        Ok(())
    }

    #[test]
    fn display() {
        let entry = KdlEntry::new(KdlValue::Base10(42));
        assert_eq!(format!("{}", entry), "42");

        let entry = KdlEntry::new_prop("name", KdlValue::Base10(42));
        assert_eq!(format!("{}", entry), "name=42");
    }
}
