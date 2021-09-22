#![doc(html_logo_url = "https://kdl.dev/logo.svg")]
#![doc = include_str!("../README.md")]

use nom::combinator::all_consuming;
use nom::Finish;

pub use crate::error::{KdlError, KdlErrorKind, TryFromKdlNodeValueError};
pub use crate::node::{KdlNode, KdlValue};

mod error;
mod node;
mod nom_compat;
mod parser;

/// Parse a KDL document from a string into a list of [`KdlNode`]s.
///
/// ```
/// use kdl::{KdlNode, KdlValue};
/// use std::collections::HashMap;
///
/// assert_eq!(
///     kdl::parse_document("node 1 key=true").unwrap(),
///     vec![
///         KdlNode {
///             name: String::from("node"),
///             values: vec![KdlValue::Int(1)],
///             properties: {
///                 let mut temp = HashMap::new();
///                 temp.insert(String::from("key"), KdlValue::Boolean(true));
///                 temp
///             },
///             children: vec![],
///         }
///     ]
/// )
/// ```
pub fn parse_document<I>(input: I) -> Result<Vec<KdlNode>, KdlError>
where
    I: AsRef<str>,
{
    let input = input.as_ref();
    all_consuming(parser::nodes)(input)
        .finish()
        .map(|(_, arg)| arg)
        .map_err(|e| {
            let prefix = &input[..(input.len() - e.input.len())];
            let (line, column) = calculate_line_column(prefix);
            KdlError {
                input: input.into(),
                offset: prefix.chars().count(),
                line,
                column,
                kind: if let Some(kind) = e.kind {
                    kind
                } else if let Some(ctx) = e.context {
                    KdlErrorKind::Context(ctx)
                } else {
                    KdlErrorKind::Other
                },
            }
        })
}

/// Calculates the line and column of the end of a `&str`.
///
/// If the line ends on a newline, the (line, column) pair is placed on the previous line instead.
fn calculate_line_column(input: &str) -> (usize, usize) {
    let (input, skipped_lines) = parser::count_leading_lines(input);
    let input = parser::strip_trailing_newline(input);
    (skipped_lines + 1, input.len() + 1) // +1 as we're 1-based
}
