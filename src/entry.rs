#[cfg(feature = "span")]
use miette::SourceSpan;
use std::{fmt::Display, str::FromStr};

use crate::{v2_parser, KdlIdentifier, KdlParseFailure, KdlValue};

/// KDL Entries are the "arguments" to KDL nodes: either a (positional)
/// [`Argument`](https://github.com/kdl-org/kdl/blob/main/SPEC.md#argument) or
/// a (key/value)
/// [`Property`](https://github.com/kdl-org/kdl/blob/main/SPEC.md#property)
#[derive(Debug, Clone, Eq)]
pub struct KdlEntry {
    pub(crate) ty: Option<KdlIdentifier>,
    pub(crate) value: KdlValue,
    pub(crate) name: Option<KdlIdentifier>,
    pub(crate) format: Option<KdlEntryFormat>,
    #[cfg(feature = "span")]
    pub(crate) span: SourceSpan,
}

impl PartialEq for KdlEntry {
    fn eq(&self, other: &Self) -> bool {
        self.ty == other.ty
            && self.value == other.value
            && self.name == other.name
            && self.format == other.format
        // intentionally omitted: self.span == other.span
    }
}

impl std::hash::Hash for KdlEntry {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.ty.hash(state);
        self.value.hash(state);
        self.name.hash(state);
        self.format.hash(state);
        // intentionally omitted: self.span.hash(state)
    }
}

impl KdlEntry {
    /// Creates a new Argument (positional) KdlEntry.
    pub fn new(value: impl Into<KdlValue>) -> Self {
        KdlEntry {
            ty: None,
            value: value.into(),
            name: None,
            format: None,
            #[cfg(feature = "span")]
            span: (0..0).into(),
        }
    }

    /// Gets a reference to this entry's name, if it's a property entry.
    pub fn name(&self) -> Option<&KdlIdentifier> {
        self.name.as_ref()
    }

    /// Gets a mutable reference to this node's name.
    pub fn name_mut(&mut self) -> Option<&mut KdlIdentifier> {
        self.name.as_mut()
    }

    /// Sets this node's name.
    pub fn set_name(&mut self, name: Option<impl Into<KdlIdentifier>>) {
        self.name = name.map(|x| x.into());
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

    /// Gets this entry's span.
    ///
    /// This value will be properly initialized when created via [`KdlDocument::parse`]
    /// but may become invalidated if the document is mutated. We do not currently
    /// guarantee this to yield any particularly consistent results at that point.
    #[cfg(feature = "span")]
    pub fn span(&self) -> SourceSpan {
        self.span
    }

    /// Sets this entry's span.
    #[cfg(feature = "span")]
    pub fn set_span(&mut self, span: impl Into<SourceSpan>) {
        self.span = span.into();
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

    /// Gets the formatting details for this entry.
    pub fn format(&self) -> Option<&KdlEntryFormat> {
        self.format.as_ref()
    }

    /// Gets a mutable reference to this entry's formatting details.
    pub fn format_mut(&mut self) -> Option<&mut KdlEntryFormat> {
        self.format.as_mut()
    }

    /// Sets the formatting details for this entry.
    pub fn set_format(&mut self, format: KdlEntryFormat) {
        self.format = Some(format);
    }

    /// Creates a new Property (key/value) KdlEntry.
    pub fn new_prop(key: impl Into<KdlIdentifier>, value: impl Into<KdlValue>) -> Self {
        KdlEntry {
            ty: None,
            value: value.into(),
            name: Some(key.into()),
            format: None,
            #[cfg(feature = "span")]
            span: SourceSpan::from(0..0),
        }
    }

    /// Clears leading and trailing text (whitespace, comments), as well as
    /// resetting this entry's value to its default representation.
    pub fn clear_format(&mut self) {
        self.format = None;
        if let Some(ty) = &mut self.ty {
            ty.clear_format();
        }
        if let Some(name) = &mut self.name {
            name.clear_format();
        }
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
    pub fn autoformat(&mut self) {
        self.format = None;
        if let Some(name) = &mut self.name {
            name.autoformat();
        }
    }
}

impl Display for KdlEntry {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Some(KdlEntryFormat { leading, .. }) = &self.format {
            write!(f, "{}", leading)?;
        }
        if let Some(name) = &self.name {
            write!(f, "{}", name)?;
            if let Some(KdlEntryFormat {
                after_key,
                eq,
                after_eq,
                ..
            }) = &self.format
            {
                write!(f, "{}{}{}", after_key, eq, after_eq)?;
            } else {
                write!(f, "=")?;
            }
        }
        if let Some(ty) = &self.ty {
            write!(f, "(")?;
            if let Some(KdlEntryFormat { before_ty_name, .. }) = &self.format {
                write!(f, "{}", before_ty_name)?;
            }
            write!(f, "{}", ty)?;
            if let Some(KdlEntryFormat { after_ty_name, .. }) = &self.format {
                write!(f, "{}", after_ty_name)?;
            }
            write!(f, ")")?;
        }
        if let Some(KdlEntryFormat {
            after_ty,
            value_repr,
            ..
        }) = &self.format
        {
            write!(f, "{}{}", after_ty, value_repr)?;
        } else {
            write!(f, "{}", self.value)?;
        }
        if let Some(KdlEntryFormat { trailing, .. }) = &self.format {
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
    type Err = KdlParseFailure;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let (maybe_val, errs) = v2_parser::try_parse(v2_parser::padded_node_entry, s);
        if let (Some(Some(v)), true) = (maybe_val, errs.is_empty()) {
            Ok(v)
        } else {
            Err(v2_parser::failure_from_errs(errs, s))
        }
    }
}

/// Formatting details for [`KdlEntry`]s.
#[derive(Debug, Default, Clone, Eq, PartialEq, Hash)]
pub struct KdlEntryFormat {
    /// The actual text representation of the entry's value.
    pub value_repr: String,
    /// Whitespace and comments preceding the entry itself.
    pub leading: String,
    /// Whitespace and comments following the entry itself.
    pub trailing: String,
    /// Whitespace and comments after the entry's type annotation's closing
    /// `)`, before its value.
    pub after_ty: String,
    /// Whitespace and comments between the opening `(` of an entry's type
    /// annotation and its actual type name.
    pub before_ty_name: String,
    /// Whitespace and comments between the actual type name and the closing
    /// `)` in an entry's type annotation.
    pub after_ty_name: String,
    /// Whitespace and comments between an entry's key name and its equals sign.
    pub after_key: String,
    /// Whitespace and comments between an entry's equals sign and its value.
    pub after_eq: String,
    /// Actual text used as an entry's equals sign.
    pub eq: String,
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn reset_value_repr() -> miette::Result<()> {
        let mut left_entry: KdlEntry = "   name=1.03e2".parse()?;
        let mut right_entry: KdlEntry = "   name=103.0".parse()?;
        assert_ne!(left_entry, right_entry);
        left_entry.clear_format();
        right_entry.clear_format();
        assert_eq!(left_entry, right_entry);
        Ok(())
    }

    #[test]
    fn new() {
        let entry = KdlEntry::new(42);
        assert_eq!(
            entry,
            KdlEntry {
                ty: None,
                value: KdlValue::Integer(42),
                name: None,
                format: None,
                #[cfg(feature = "span")]
                span: SourceSpan::from(0..0),
            }
        );

        let entry = KdlEntry::new_prop("name", 42);
        assert_eq!(
            entry,
            KdlEntry {
                ty: None,
                value: KdlValue::Integer(42),
                name: Some("name".into()),
                format: None,
                #[cfg(feature = "span")]
                span: SourceSpan::from(0..0),
            }
        );
    }

    #[test]
    fn parsing() -> miette::Result<()> {
        let entry: KdlEntry = "foo".parse()?;
        assert_eq!(
            entry,
            KdlEntry {
                ty: None,
                value: KdlValue::from("foo"),
                name: None,
                format: Some(KdlEntryFormat {
                    value_repr: "foo".into(),
                    ..Default::default()
                }),
                #[cfg(feature = "span")]
                span: SourceSpan::from(0..3),
            }
        );

        let entry: KdlEntry = "foo=bar".parse()?;
        assert_eq!(
            entry,
            KdlEntry {
                ty: None,
                value: KdlValue::from("bar"),
                name: Some("foo".parse()?),
                format: Some(KdlEntryFormat {
                    value_repr: "bar".into(),
                    eq: "=".into(),
                    ..Default::default()
                }),
                #[cfg(feature = "span")]
                span: SourceSpan::from(0..7),
            }
        );

        let entry: KdlEntry = " \\\n (\"m\\\"eh\")0xDEADbeef\t\\\n".parse()?;
        let mut ty: KdlIdentifier = "\"m\\\"eh\"".parse()?;
        ty.span = (5..12).into();
        assert_eq!(
            entry,
            KdlEntry {
                ty: Some(ty),
                value: KdlValue::Integer(0xdeadbeef),
                name: None,
                format: Some(KdlEntryFormat {
                    leading: " \\\n ".into(),
                    trailing: "\t\\\n".into(),
                    value_repr: "0xDEADbeef".into(),
                    ..Default::default()
                }),
                #[cfg(feature = "span")]
                span: SourceSpan::from(0..26),
            }
        );

        let entry: KdlEntry = " \\\n \"foo\"=(\"m\\\"eh\")0xDEADbeef\t\\\n".parse()?;
        assert_eq!(
            entry,
            KdlEntry {
                format: Some(KdlEntryFormat {
                    leading: " \\\n ".into(),
                    trailing: "\t\\\n".into(),
                    value_repr: "0xDEADbeef".into(),
                    before_ty_name: "".into(),
                    after_ty_name: "".into(),
                    after_ty: "".into(),
                    after_key: "".into(),
                    after_eq: "".into(),
                    eq: "=".into(),
                }),
                ty: Some("\"m\\\"eh\"".parse()?),
                value: KdlValue::Integer(0xdeadbeef),
                name: Some("\"foo\"".parse()?),
                #[cfg(feature = "span")]
                span: SourceSpan::from(0..0),
            }
        );

        Ok(())
    }

    #[test]
    fn display() {
        let entry = KdlEntry::new(KdlValue::Integer(42));
        assert_eq!(format!("{}", entry), "42");

        let entry = KdlEntry::new_prop("name", KdlValue::Integer(42));
        assert_eq!(format!("{}", entry), "name=42");
    }
}
