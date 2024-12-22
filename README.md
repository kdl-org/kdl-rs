# `kdl`

`kdl` is a "document-oriented" parser and API for the [KDL Document
Language](https://kdl.dev), a node-based, human-friendly configuration and
serialization format.

Unlike serde-based implementations, this crate preserves formatting when
editing, as well as when inserting or changing values with custom
formatting. This is most useful when working with human-maintained KDL
files.

You can think of this crate as
[`toml_edit`](https://crates.io/crates/toml_edit), but for KDL.

This crate supports both KDL v2.0.0 and v1.0.0 (when using the non-default
`v1` feature). It also supports converting documents between either format.

There is also a `v1-fallback` feature that may be enabled in order to have
the various `Kdl*::parse` methods try to parse their input as v2, and, if
that fails, try again as v1. In either case, a dedicated `Kdl*::parse_v1`
method is available for v1-exclusive parsing, as long as either `v1` or
`v1-fallback` are enabled.

### Example

```rust
use kdl::{KdlDocument, KdlValue};

let doc_str = r#"
hello 1 2 3

// Comment
world prop=string-value {
    child 1
    child 2
    child #inf
}
"#;

let doc: KdlDocument = doc_str.parse().expect("failed to parse KDL");

assert_eq!(
    doc.iter_args("hello").collect::<Vec<&KdlValue>>(),
    vec![&1.into(), &2.into(), &3.into()]
);

assert_eq!(
    doc.get("world").map(|node| &node["prop"]),
    Some(&"string-value".into())
);

// Documents fully roundtrip:
assert_eq!(doc.to_string(), doc_str);
```

### Controlling Formatting

By default, everything is created with default formatting. You can parse
items manually to provide custom representations, comments, etc:

```rust
let node_str = r#"
  // indented comment
  "formatted" 1 /* comment */ \
    2;
"#;

let mut doc = kdl::KdlDocument::new();
doc.nodes_mut().push(node_str.parse().unwrap());

assert_eq!(&doc.to_string(), node_str);
```

[`KdlDocument`], [`KdlNode`], [`KdlEntry`], and [`KdlIdentifier`] can all
be parsed and managed this way.

### Error Reporting

[`KdlError`] implements [`miette::Diagnostic`] and can be used to display
detailed, pretty-printed diagnostic messages when using [`miette::Result`]
and the `"fancy"` feature flag for `miette`:

```toml
# Cargo.toml
[dependencies]
miette = { version = "x.y.z", features = ["fancy"] }
```

```rust
fn main() -> miette::Result<()> {
    "foo 1.".parse::<kdl::KdlDocument>()?;
    Ok(())
}
```

This will display a message like:
```
Error:
  × Expected valid value.
   ╭────
 1 │ foo 1.
   ·     ─┬
   ·      ╰── invalid float
   ╰────
  help: Floating point numbers must be base 10, and have numbers after the decimal point.
```

### Features

* `span` (default) - Includes spans in the various document-related structs.
* `v1` - Adds support for v1 parsing. This will pull in the entire previous
    version of `kdl-rs`, and so may be fairly heavy.
* `v1-fallback` - Implies `v1`. Makes it so the various `*::parse()` and
    `FromStr` implementations try to parse their inputs as `v2`, and, if that
    fails, try again with `v1`. Errors will only be reported as if the input was
    `v2`. To manage this more precisely, you can use the `*::parse_v2` and
    `*::parse_v1` methods.

### Quirks

#### Properties

Multiple properties with the same name are allowed, and all duplicated
**will be preserved**, meaning those documents will correctly round-trip.
When using `node.get()`/`node["key"]` & company, the _last_ property with
that name's value will be returned (as per spec).

#### Numbers

KDL itself does not specify a particular representation for numbers and
accepts just about anything valid, no matter how large and how small. This
means a few things:

* Numbers without a decimal point are interpreted as [`i128`].
* Numbers with a decimal point are interpreted as [`f64`].
* The keywords `#inf`, `#-inf`, and `#nan` evaluate to [`f64::INFINITY`],
  [`f64::NEG_INFINITY`], and [`f64::NAN`].
* The original _representation/text_ of these numbers will be preserved,
  unless you [`KdlDocument::autoformat`] in which case the original
  representation will be thrown away and the actual value will be used when
  serializing.

### Minimum Supported Rust Version

You must be at least `1.70.0` tall to get on this ride.

### License

The code in this repository is covered by [the Apache-2.0
License](LICENSE).

[`KdlDocument`]: https://docs.rs/kdl/latest/kdl/struct.KdlDocument.html
[`KdlNode`]: https://docs.rs/kdl/latest/kdl/struct.KdlNode.html
[`KdlEntry`]: https://docs.rs/kdl/latest/kdl/struct.KdlEntry.html
[`KdlIdentifier`]: https://docs.rs/kdl/latest/kdl/struct.KdlIdentifier.html
[`KdlError`]: https://docs.rs/kdl/latest/kdl/struct.KdlError.html
[`miette::Diagnostic`]: https://docs.rs/miette/latest/miette/trait.Diagnostic.html
[`miette::Result`]: https://docs.rs/miette/latest/miette/type.Result.html
