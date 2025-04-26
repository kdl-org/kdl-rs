use std::{error::Error, fmt::Display, sync::Arc};

use miette::{Diagnostic, SourceSpan};

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
#[derive(Debug, Diagnostic, Clone, Eq, PartialEq)]
pub struct KdlError {
    /// Original input that this failure came from.
    #[source_code]
    pub input: Arc<String>,

    /// Sub-diagnostics for this failure.
    #[related]
    pub diagnostics: Vec<KdlDiagnostic>,
}

impl Display for KdlError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Failed to parse KDL document")
    }
}
impl Error for KdlError {}

/// An individual diagnostic message for a KDL parsing issue.
///
/// While generally signifying errors, they can also be treated as warnings.
#[derive(Debug, Diagnostic, Clone, Eq, PartialEq)]
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

impl Display for KdlDiagnostic {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let message = self
            .message
            .clone()
            .unwrap_or_else(|| "Unexpected error".into());
        write!(f, "{message}")
    }
}
impl Error for KdlDiagnostic {}

#[cfg(feature = "v1")]
impl From<kdlv1::KdlError> for KdlError {
    fn from(value: kdlv1::KdlError) -> Self {
        let input = Arc::new(value.input);
        KdlError {
            input: input.clone(),
            diagnostics: vec![KdlDiagnostic {
                input,
                span: SourceSpan::new(value.span.offset().into(), value.span.len()),
                message: Some(format!("{}", value.kind)),
                label: value.label.map(|x| x.into()),
                help: value.help.map(|x| x.into()),
                severity: miette::Severity::Error,
            }],
        }
    }
}

#[cfg(test)]
mod tests {
    use std::error::Error;

    use super::*;

    #[test]
    fn kdl_error() {
        let kdl_error = KdlError {
            input: Default::default(),
            diagnostics: Default::default(),
        };

        assert_eq!(kdl_error.to_string(), "Failed to parse KDL document");
        assert!(kdl_error.source().is_none());
    }

    #[test]
    fn kdl_diagnostic() {
        let mut kdl_diagnostic = KdlDiagnostic {
            input: Default::default(),
            span: SourceSpan::new(0.into(), 1),
            message: Default::default(),
            label: Default::default(),
            help: Default::default(),
            severity: Default::default(),
        };

        assert_eq!(kdl_diagnostic.to_string(), "Unexpected error");
        assert!(kdl_diagnostic.source().is_none());

        kdl_diagnostic.message = Some("mega bad news, kiddo".to_owned());

        assert_eq!(kdl_diagnostic.to_string(), "mega bad news, kiddo");
        assert!(kdl_diagnostic.source().is_none());
    }
}
