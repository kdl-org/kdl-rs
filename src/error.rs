use std::num::{ParseFloatError, ParseIntError};

use miette::{Diagnostic, SourceSpan};
use nom::error::{ContextError, ErrorKind, FromExternalError, ParseError};
use thiserror::Error;

#[cfg(doc)]
use {
    crate::KdlNode,
    std::convert::{TryFrom, TryInto},
};

/// An error that occurs when parsing a KDL document.
///
/// This error implements [`miette::Diagnostic`] and can be used to display
/// detailed, pretty-printed diagnostic messages when using [`miette::Result`]
/// and the `"fancy"` feature flag for `miette`:
///
/// ```no_run
/// fn main() -> miette::Result<()> {
///     "foo 1.".parse::<kdl::KdlDocument>()?;
///     Ok(())
/// }
/// ```
///
/// This will display a message like:
/// ```text
/// Error:
///   × Expected valid value.
///    ╭────
///  1 │ foo 1.
///    ·     ─┬
///    ·      ╰── invalid float
///    ╰────
///   help: Floating point numbers must be base 10, and have numbers after the decimal point.
/// ```
#[derive(Debug, Diagnostic, Clone, Eq, PartialEq, Error)]
#[error("{kind}")]
pub struct KdlError {
    /// Source string for the KDL document that failed to parse.
    #[source_code]
    pub input: String,

    /// Offset in chars of the error.
    #[label("{}", label.unwrap_or("here"))]
    pub span: SourceSpan,

    /// Label text for this span. Defaults to `"here"`.
    pub label: Option<&'static str>,

    /// Suggestion for fixing the parser error.
    #[help]
    pub help: Option<&'static str>,

    /// Specific error kind for this parser error.
    pub kind: KdlErrorKind,
}

/// A type reprenting additional information specific to the type of error being returned.
#[derive(Debug, Diagnostic, Clone, Eq, PartialEq, Error)]
pub enum KdlErrorKind {
    /// An error occurred while parsing an integer.
    #[error(transparent)]
    #[diagnostic(code(kdl::parse_int))]
    ParseIntError(ParseIntError),

    /// An error occurred while parsing a floating point number.
    #[error(transparent)]
    #[diagnostic(code(kdl::parse_float))]
    ParseFloatError(ParseFloatError),

    /// Generic parsing error. The given context string denotes the component
    /// that failed to parse.
    #[error("Expected {0}.")]
    #[diagnostic(code(kdl::parse_component))]
    Context(&'static str),

    /// Generic unspecified error. If this is returned, the call site should
    /// be annotated with context, if possible.
    #[error("An unspecified error occurred.")]
    #[diagnostic(code(kdl::other))]
    Other,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub(crate) struct KdlParseError<I> {
    pub(crate) input: I,
    pub(crate) context: Option<&'static str>,
    pub(crate) len: usize,
    pub(crate) label: Option<&'static str>,
    pub(crate) help: Option<&'static str>,
    pub(crate) kind: Option<KdlErrorKind>,
    pub(crate) touched: bool,
}

impl<I> ParseError<I> for KdlParseError<I> {
    fn from_error_kind(input: I, _kind: nom::error::ErrorKind) -> Self {
        Self {
            input,
            len: 0,
            label: None,
            help: None,
            context: None,
            kind: None,
            touched: false,
        }
    }

    fn append(_input: I, _kind: nom::error::ErrorKind, other: Self) -> Self {
        other
    }
}

impl<I> ContextError<I> for KdlParseError<I> {
    fn add_context(_input: I, ctx: &'static str, mut other: Self) -> Self {
        other.context = other.context.or(Some(ctx));
        other
    }
}

impl<'a> FromExternalError<&'a str, ParseIntError> for KdlParseError<&'a str> {
    fn from_external_error(input: &'a str, _kind: ErrorKind, e: ParseIntError) -> Self {
        KdlParseError {
            input,
            len: 0,
            label: None,
            help: None,
            context: None,
            kind: Some(KdlErrorKind::ParseIntError(e)),
            touched: false,
        }
    }
}

impl<'a> FromExternalError<&'a str, ParseFloatError> for KdlParseError<&'a str> {
    fn from_external_error(input: &'a str, _kind: ErrorKind, e: ParseFloatError) -> Self {
        KdlParseError {
            input,
            len: 0,
            label: None,
            help: None,
            context: None,
            kind: Some(KdlErrorKind::ParseFloatError(e)),
            touched: false,
        }
    }
}
