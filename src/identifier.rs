#[cfg(feature = "span")]
use miette::SourceSpan;
use std::{fmt::Display, str::FromStr};

use crate::{parser, KdlError};

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
    pub fn clear_fmt(&mut self) {
        self.repr = None;
    }

    /// Auto-formats this identifier.
    pub fn fmt(&mut self) {
        self.repr = None;
    }
}

impl Display for KdlIdentifier {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Some(repr) = &self.repr {
            write!(f, "{}", repr)
        } else if self.plain_value() {
            write!(f, "{}", self.value)
        } else {
            write!(f, "{:?}", self.value)
        }
    }
}

impl KdlIdentifier {
    pub(crate) fn is_identifier_char(c: char) -> bool {
        !((c as u32) < 0x20
            || (c as u32) > 0x10ffff
            || matches!(
                c,
                '\\' | '/'
                    | '('
                    | ')'
                    | '{'
                    | '}'
                    | '<'
                    | '>'
                    | ';'
                    | '['
                    | ']'
                    | '='
                    | ','
                    | '"'
                    // Newlines
                    | '\r'
                    | '\n'
                    | '\u{0085}'
                    | '\u{000C}'
                    | '\u{2028}'
                    | '\u{2029}'
                    // Whitespace
                    | ' '
                    | '\t'
                    | '\u{FEFF}'
                    | '\u{00A0}'
                    | '\u{1680}'
                    | '\u{2000}'
                    | '\u{2001}'
                    | '\u{2002}'
                    | '\u{2003}'
                    | '\u{2004}'
                    | '\u{2005}'
                    | '\u{2006}'
                    | '\u{2007}'
                    | '\u{2008}'
                    | '\u{2009}'
                    | '\u{200A}'
                    | '\u{202F}'
                    | '\u{205F}'
                    | '\u{3000}'
            ))
    }

    pub(crate) fn is_initial_char(c: char) -> bool {
        !c.is_numeric() && Self::is_identifier_char(c)
    }

    fn plain_value(&self) -> bool {
        let mut iter = self.value.chars();
        if let Some(c) = iter.next() {
            if !Self::is_initial_char(c) {
                return false;
            }
        } else {
            return false;
        }
        for char in iter {
            if !Self::is_identifier_char(char) {
                return false;
            }
        }
        true
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
    type Err = KdlError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let kdl_parser = crate::parser::KdlParser::new(s);
        kdl_parser.parse(parser::identifier(&kdl_parser))
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

        let quoted = "\"foo\\\"bar\"";
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
