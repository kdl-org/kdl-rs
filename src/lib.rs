//! `kdl` is a "document-oriented" parser and API for the [KDL Document
//! Language](https://kdl.dev), a node-based, human-friendly configuration and
//! serialization format. Unlike serde-based implementations, this crate
//! preserves formatting when editing, as well as when inserting or changing
//! values with custom formatting. This is most useful when working with
//! human-maintained KDL files.
//!
//! You can think of this crate as
//! [`toml_edit`](https://crates.io/crates/toml_edit), but for KDL.
//!
//! If you don't care about formatting or programmatic manipulation, you might
//! check out [`knuffel`](https://crates.io/crates/knuffel) or
//! [`kaydle`](https://crates.io/crates/kaydle) instead for serde (or
//! serde-like) parsing.
//!
//! ## Example
//!
//! ```rust
//! use kdl::KdlDocument;
//!
//! let doc_str = r#"
//! hello 1 2 3
//!
//! world prop="value" {
//!     child 1
//!     child 2
//! }
//! "#;
//!
//! let doc: KdlDocument = doc_str.parse().expect("failed to parse KDL");
//!
//! assert_eq!(
//!     doc.get_args("hello"),
//!     vec![&1.into(), &2.into(), &3.into()]
//! );
//!
//! assert_eq!(
//!     doc.get("world").map(|node| &node["prop"]),
//!     Some(&"value".into())
//! );
//!
//! // Documents fully roundtrip:
//! assert_eq!(doc.to_string(), doc_str);
//! ```
//!
//! ## Controlling Formatting
//!
//! By default, everything is created with default formatting. You can parse
//! items manually to provide custom representations, comments, etc:
//!
//! ```rust
//! let node_str = r#"
//!   // indented comment
//!   "formatted" 1 /* comment */ \
//!     2;
//! "#;
//!
//! let mut doc = kdl::KdlDocument::new();
//! doc.nodes_mut().push(node_str.parse().unwrap());
//!
//! assert_eq!(&doc.to_string(), node_str);
//! ```
//!
//! [`KdlDocument`], [`KdlNode`], [`KdlEntry`], and [`KdlIdentifier`] can all
//! be parsed and managed this way.
//!
//! ### Query Engine
//!
//! `kdl` includes a query engine for
//! [KQL](https://github.com/kdl-org/kdl/blob/main/QUERY-SPEC.md), which lets you
//! pick out nodes from a document using a CSS Selectors-style syntax.
//!
//! Queries can be done from either a [`KdlDocument`] or a [`KdlNode`], with
//! mostly the same semantics.
//!
//! ```rust
//! use kdl::KdlDocument;
//!
//! let doc = r#"
//! a {
//!     b 1
//!     c 2
//!     d 3 {
//!         e prop="hello"
//!     }
//! }
//! "#.parse::<KdlDocument>().expect("failed to parse KDL");
//!
//! let results = doc.query("a > b").expect("failed to parse query");
//! assert_eq!(results, Some(&doc.nodes()[0].children().unwrap().nodes()[0]));
//!
//! let results = doc.query_get("e", "prop").expect("failed to parse query");
//! assert_eq!(results, Some(&"hello".into()));
//!
//! let results = doc.query_get_all("a > []", 0).expect("failed to parse query").collect::<Vec<_>>();
//! assert_eq!(results, vec![&1.into(), &2.into(), &3.into()]);
//! ```
//!
//! ## Error Reporting
//!
//! [`KdlError`] implements [`miette::Diagnostic`] and can be used to display
//! detailed, pretty-printed diagnostic messages when using [`miette::Result`]
//! and the `"fancy"` feature flag for `miette`:
//!
//! ```toml
//! # Cargo.toml
//! [dependencies]
//! miette = { version = "x.y.z", features = ["fancy"] }
//! ```
//!
//! ```no_run
//! fn main() -> miette::Result<()> {
//!     "foo 1.".parse::<kdl::KdlDocument>()?;
//!     Ok(())
//! }
//! ```
//!
//! This will display a message like:
//! ```text
//! Error:
//!   × Expected valid value.
//!    ╭────
//!  1 │ foo 1.
//!    ·     ─┬
//!    ·      ╰── invalid float
//!    ╰────
//!   help: Floating point numbers must be base 10, and have numbers after the decimal point.
//! ```
//!
//! ## Quirks
//!
//! ### Properties
//!
//! Multiple properties with the same name are allowed, and all duplicated
//! **will be preserved**, meaning those documents will correctly round-trip.
//! When using `node.get()`/`node["key"]` & company, the _last_ property with
//! that name's value will be returned.
//!
//! ### Numbers
//!
//! KDL itself does not specify a particular representation for numbers and
//! accepts just about anything valid, no matter how large and how small. This
//! means a few things:
//!
//! * Numbers without a decimal point are interpreted as [`u64`].
//! * Numbers with a decimal point are interpreted as [`f64`].
//! * Floating point numbers that evaluate to [`f64::INFINITY`] or
//!   [`f64::NEG_INFINITY`] or NaN will be represented as such in the values,
//!   instead of the original numbers.
//! * A similar restriction applies to overflowed [`u64`] values.
//! * The original _representation_ of these numbers will be preserved, unless
//!   you [`KdlDocument::fmt`] in which case the original representation will be
//!   thrown away and the actual value will be used when serializing.
//!
//! ## License
//!
//! The code in this repository is covered by [the Apache-2.0
//! License](LICENSE.md).

#![deny(missing_debug_implementations, nonstandard_style)]
#![warn(missing_docs, unreachable_pub, rust_2018_idioms, unreachable_pub)]
#![cfg_attr(test, deny(warnings))]
#![doc(html_favicon_url = "https://kdl.dev/favicon.ico")]
#![doc(html_logo_url = "https://kdl.dev/logo.svg")]

pub use document::*;
pub use entry::*;
pub use error::*;
pub use identifier::*;
pub use node::*;
pub use query::*;
pub use value::*;

mod document;
mod entry;
mod error;
mod fmt;
mod identifier;
mod node;
mod nom_compat;
mod parser;
mod query;
mod query_parser;
mod value;
