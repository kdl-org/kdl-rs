#![doc(html_logo_url = "https://kdl.dev/logo.svg")]
/// `kdl` is a "document-oriented" parser and API. That means that, unlike
/// serde-based implementations, it's meant to preserve formatting when editing,
/// as well as inserting values with custom formatting. This is useful when
/// working with human-maintained KDL files.
///
/// You can think of this crate as
/// [`toml_edit`](https://crates.io/crates/toml_edit), but for KDL.
///
/// ### Example
///
/// ```rust
/// use kdl::KdlDocument;
///
/// let doc: KdlDocument = r#"
/// hello 1 2 3
/// world prop="value" {
///     child 1
///     child 2
/// }
/// "#.parse().expect("failed to parse KDL");
///
/// assert_eq!(doc.get_args("hello"), vec![&1.into(), &2.into(), &3.into()]);
/// assert_eq!(doc.get("world").map(|node| &node["prop"]), Some(&"value".into()));
/// ```
///
/// ## License
///
/// The code in this repository is covered by [the Apache-2.0 License](LICENSE.md).
pub use document::*;
pub use entry::*;
pub use error::*;
pub use identifier::*;
pub use node::*;
pub use value::*;

mod document;
mod entry;
mod error;
mod identifier;
mod node;
mod nom_compat;
mod parser;
mod value;
