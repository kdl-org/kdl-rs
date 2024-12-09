use std::sync::Arc;

use miette::{Diagnostic, SourceSpan};
use thiserror::Error;

#[cfg(doc)]
use {
    crate::KdlNode,
    std::convert::{TryFrom, TryInto},
};

/// The toplevel `Error` type for KDL: this is returned when a KDL document
/// failed to parse entirely.
///
/// This diagnostic implements [`miette::Diagnostic`] and can be used to
/// display detailed, pretty-printed diagnostic messages when using
/// [`miette::Result`] and the `"fancy"` feature flag for `miette`:
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
#[error("Failed to parse KDL document")]
pub struct KdlParseFailure {
    /// Original input that this failure came from.
    #[source_code]
    pub input: Arc<String>,

    /// Sub-diagnostics for this failure.
    #[related]
    pub diagnostics: Vec<KdlDiagnostic>,
}

/// An individual diagnostic message for a KDL parsing issue.
///
/// While generally signifying errors, they can also be treated as warnings.
#[derive(Debug, Diagnostic, Clone, Eq, PartialEq, Error)]
#[error("{}", message.clone().unwrap_or_else(|| "Unexpected error".into()))]
pub struct KdlDiagnostic {
    /// Shared source for the diagnostic.
    #[source_code]
    pub input: Arc<String>,

    /// Offset in chars of the error.
    #[label("{}", label.clone().unwrap_or_else(|| "here".into()))]
    pub span: SourceSpan,

    /// Message for the error itself.
    pub message: Option<String>,

    /// Label text for this span. Defaults to `"here"`.
    pub label: Option<String>,

    /// Suggestion for fixing the parser error.
    #[help]
    pub help: Option<String>,

    /// Severity level for the Diagnostic.
    #[diagnostic(severity)]
    pub severity: miette::Severity,
}
