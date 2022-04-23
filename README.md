# `kdl`

`kdl` is a "document-oriented" parser and API for the [KDL Document
Language](https://kdl.dev), a node-based, human-friendly configuration and
serialization format. Unlike serde-based implementations, this crate
preserves formatting when editing, as well as when inserting or changing
values with custom formatting. This is most useful when working with
human-maintained KDL files.

You can think of this crate as
[`toml_edit`](https://crates.io/crates/toml_edit), but for KDL.

If you don't care about formatting or programmatic manipulation, you might
check out [`knuffel`](https://crates.io/crates/knuffel) or
[`kaydle`](https://crates.io/crates/kaydle) instead for serde (or
serde-like) parsing.

### Example

```rust
use kdl::KdlDocument;

let doc_str = r#"
hello 1 2 3

world prop="value" {
    child 1
    child 2
}
"#;

let doc: KdlDocument = doc_str.parse().expect("failed to parse KDL");

assert_eq!(
    doc.get_args("hello"),
    vec![&1.into(), &2.into(), &3.into()]
);

assert_eq!(
    doc.get("world").map(|node| &node["prop"]),
    Some(&"value".into())
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


This error implements [`miette::Diagnostic`] and can be used to display
detailed, pretty-printed diagnostic messages when using [`miette::Result`]
and the `"pretty"` feature flag for `miette`:

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
### License

The code in this repository is covered by [the Apache-2.0
License](LICENSE.md).

[`KdlDocument`]: https://docs.rs/kdl/3.0.1-alpha.0/kdl/struct.KdlDocument.html
[`KdlNode`]: https://docs.rs/kdl/3.0.1-alpha.0/kdl/struct.KdlNode.html
[`KdlEntry`]: https://docs.rs/kdl/3.0.1-alpha.0/kdl/struct.KdlEntry.html
[`KdlIdentifier`]: https://docs.rs/kdl/3.0.1-alpha.0/kdl/struct.KdlIdentifier.html
