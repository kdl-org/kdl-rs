use std::{error::Error, fmt::Display, iter, sync::Arc};

use miette::{Diagnostic, LabeledSpan, Severity, SourceSpan};

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
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct KdlError {
    /// Original input that this failure came from.
    pub input: Arc<String>,

    /// Sub-diagnostics for this failure.
    pub diagnostics: Vec<KdlDiagnostic>,
}

impl Display for KdlError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Failed to parse KDL document")
    }
}
impl Error for KdlError {}

impl Diagnostic for KdlError {
    fn source_code(&self) -> Option<&dyn miette::SourceCode> {
        Some(&self.input)
    }

    fn related<'a>(&'a self) -> Option<Box<dyn Iterator<Item = &'a dyn Diagnostic> + 'a>> {
        Some(Box::new(
            self.diagnostics.iter().map(|d| d as &dyn Diagnostic),
        ))
    }
}

/// An individual diagnostic message for a KDL parsing issue.
///
/// While generally signifying errors, they can also be treated as warnings.
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct KdlDiagnostic {
    /// Shared source for the diagnostic.
    pub input: Arc<String>,

    /// Offset in chars of the error.
    pub span: SourceSpan,

    /// Message for the error itself.
    pub message: Option<String>,

    /// Label text for this span. Defaults to `"here"`.
    pub label: Option<String>,

    /// Suggestion for fixing the parser error.
    pub help: Option<String>,

    /// Severity level for the Diagnostic.
    pub severity: Severity,
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

impl Diagnostic for KdlDiagnostic {
    fn source_code(&self) -> Option<&dyn miette::SourceCode> {
        Some(&self.input)
    }

    fn severity(&self) -> Option<Severity> {
        Some(self.severity)
    }

    fn help<'a>(&'a self) -> Option<Box<dyn Display + 'a>> {
        self.help.as_ref().map(|s| Box::new(s) as Box<dyn Display>)
    }

    fn labels(&self) -> Option<Box<dyn Iterator<Item = miette::LabeledSpan> + '_>> {
        let label = self.label.clone().unwrap_or_else(|| "here".to_owned());
        let labeled_span = LabeledSpan::new_with_span(Some(label), self.span);

        Some(Box::new(iter::once(labeled_span)))
    }
}

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
                severity: Severity::Error,
            }],
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn kdl_error() {
        let kdl_diagnostic = KdlDiagnostic {
            input: Default::default(),
            span: SourceSpan::new(0.into(), 0),
            message: Default::default(),
            label: Default::default(),
            help: Default::default(),
            severity: Default::default(),
        };

        let kdl_error = KdlError {
            input: Arc::new("bark? i guess?".to_owned()),
            diagnostics: vec![kdl_diagnostic.clone(), kdl_diagnostic],
        };

        // Test `Error` impl
        assert_eq!(kdl_error.to_string(), "Failed to parse KDL document");
        assert!(kdl_error.source().is_none());

        // Test `Diagnostic` impl
        let related: Vec<_> = kdl_error.related().unwrap().collect();
        assert_eq!(related.len(), 2);
        assert_eq!(
            kdl_error
                .source_code()
                .unwrap()
                .read_span(&SourceSpan::new(0.into(), 5), 0, 0)
                .unwrap()
                .data(),
            b"bark?"
        );
    }

    #[test]
    fn kdl_diagnostic() {
        let mut kdl_diagnostic = KdlDiagnostic {
            input: Arc::new("Catastrophic failure!!!".to_owned()),
            span: SourceSpan::new(0.into(), 3),
            message: None,
            label: Some("cute".to_owned()),
            help: Some("try harder?".to_owned()),
            severity: Severity::Error,
        };

        // Test `Error` impl
        assert_eq!(kdl_diagnostic.to_string(), "Unexpected error");
        assert!(kdl_diagnostic.source().is_none());

        kdl_diagnostic.message = Some("mega bad news, kiddo".to_owned());

        assert_eq!(kdl_diagnostic.to_string(), "mega bad news, kiddo");
        assert!(kdl_diagnostic.source().is_none());

        // Test `Diagnostic` impl
        let labels: Vec<_> = kdl_diagnostic.labels().unwrap().collect();
        assert_eq!(labels.len(), 1);
        assert_eq!(labels[0].label().unwrap(), "cute");
        assert_eq!(
            kdl_diagnostic
                .source_code()
                .unwrap()
                .read_span(labels[0].inner(), 0, 0)
                .unwrap()
                .data(),
            b"Cat"
        );
        assert_eq!(kdl_diagnostic.help().unwrap().to_string(), "try harder?");
        assert_eq!(kdl_diagnostic.severity().unwrap(), Severity::Error);
    }
}
