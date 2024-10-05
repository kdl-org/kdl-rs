#[cfg(feature = "span")]
use miette::SourceSpan;
use std::{fmt::Display, str::FromStr};

use crate::{v2_parser, KdlParseFailure, KdlValue};

/// Represents a KDL
/// [Identifier](https://github.com/kdl-org/kdl/blob/main/SPEC.md#identifier).
#[derive(Debug, Clone, Eq)]
pub struct KdlIdentifier {
    pub(crate) value: String,
    pub(crate) repr: Option<String>,
    #[cfg(feature = "span")]
    pub(crate) span: SourceSpan,
}

impl PartialEq for KdlIdentifier {
    fn eq(&self, other: &Self) -> bool {
        self.value == other.value && self.repr == other.repr
        // intentionally omitted: self.span == other.span
    }
}

impl std::hash::Hash for KdlIdentifier {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.value.hash(state);
        self.repr.hash(state);
        // Intentionally omitted: self.span.hash(state);
    }
}

impl KdlIdentifier {
    /// Gets the string value for this identifier.
    pub fn value(&self) -> &str {
        &self.value
    }

    /// Sets the string value for this identifier.
    pub fn set_value(&mut self, value: impl Into<String>) {
        self.value = value.into();
    }

    /// Gets this identifier's span.
    ///
    /// This value will be properly initialized when created via [`KdlDocument::parse`]
    /// but may become invalidated if the document is mutated. We do not currently
    /// guarantee this to yield any particularly consistent results at that point.
    #[cfg(feature = "span")]
    pub fn span(&self) -> SourceSpan {
        self.span
    }

    /// Sets this identifier's span.
    #[cfg(feature = "span")]
    pub fn set_span(&mut self, span: impl Into<SourceSpan>) {
        self.span = span.into();
    }

    /// Gets the custom string representation for this identifier, if any.
    pub fn repr(&self) -> Option<&str> {
        self.repr.as_deref()
    }

    /// Sets a custom string representation for this identifier.
    pub fn set_repr(&mut self, repr: impl Into<String>) {
        self.repr = Some(repr.into());
    }

    /// Length of this identifier when rendered as a string.
    pub fn len(&self) -> usize {
        format!("{}", self).len()
    }

    /// Returns true if this identifier is completely empty.
    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    /// Resets this identifier to its default representation. It will attempt
    /// to make it an unquoted identifier, and fall back to a string
    /// representation if that would be invalid.
    pub fn clear_format(&mut self) {
        self.repr = None;
    }

    /// Auto-formats this identifier.
    pub fn autoformat(&mut self) {
        self.repr = None;
    }
}

impl Display for KdlIdentifier {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Some(repr) = &self.repr {
            write!(f, "{}", repr)
        } else {
            write!(f, "{}", KdlValue::String(self.value().into()))
        }
    }
}

impl From<&str> for KdlIdentifier {
    fn from(value: &str) -> Self {
        KdlIdentifier {
            value: value.to_string(),
            repr: None,
            #[cfg(feature = "span")]
            span: SourceSpan::from(0..0),
        }
    }
}

impl From<String> for KdlIdentifier {
    fn from(value: String) -> Self {
        KdlIdentifier {
            value,
            repr: None,
            #[cfg(feature = "span")]
            span: SourceSpan::from(0..0),
        }
    }
}

impl From<KdlIdentifier> for String {
    fn from(value: KdlIdentifier) -> Self {
        value.value
    }
}

impl FromStr for KdlIdentifier {
    type Err = KdlParseFailure;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let (maybe_val, errs) = v2_parser::try_parse(v2_parser::identifier, s);
        if let Some(v) = maybe_val {
            Ok(v)
        } else {
            Err(v2_parser::failure_from_errs(errs, s))
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn parsing() -> miette::Result<()> {
        let plain = "foo";
        assert_eq!(
            plain.parse::<KdlIdentifier>()?,
            KdlIdentifier {
                value: plain.to_string(),
                repr: Some(plain.to_string()),
                #[cfg(feature = "span")]
                span: SourceSpan::from(0..3),
            }
        );

        let quoted = r#""foo\"bar""#;
        assert_eq!(
            quoted.parse::<KdlIdentifier>()?,
            KdlIdentifier {
                value: "foo\"bar".to_string(),
                repr: Some(quoted.to_string()),
                #[cfg(feature = "span")]
                span: SourceSpan::from(0..0),
            }
        );

        let invalid = "123";
        assert!(invalid.parse::<KdlIdentifier>().is_err());

        let invalid = "   space   ";
        assert!(invalid.parse::<KdlIdentifier>().is_err());

        let invalid = "\"x";
        assert!(invalid.parse::<KdlIdentifier>().is_err());

        Ok(())
    }

    #[test]
    fn formatting() {
        let plain = KdlIdentifier::from("foo");
        assert_eq!(format!("{}", plain), "foo");

        let quoted = KdlIdentifier::from("foo\"bar");
        assert_eq!(format!("{}", quoted), r#""foo\"bar""#);

        let mut custom_repr = KdlIdentifier::from("foo");
        custom_repr.set_repr(r#""foo/bar""#.to_string());
        assert_eq!(format!("{}", custom_repr), r#""foo/bar""#);
    }
}
