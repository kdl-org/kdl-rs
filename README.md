# `kdl`

`kdl` is a "document-oriented" parser and API. That means that, unlike
serde-based implementations, it's meant to preserve formatting when
editing, as well as inserting values with custom formatting. This is
useful when working with human-maintained KDL files.

You can think of this crate as
[`toml_edit`](https://crates.io/crates/toml_edit), but for KDL.

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

### License

The code in this repository is covered by [the Apache-2.0
License](LICENSE.md).

[`KdlDocument`]: https://docs.rs/kdl/3.0.1-alpha.0/kdl/struct.KdlDocument.html
[`KdlNode`]: https://docs.rs/kdl/3.0.1-alpha.0/kdl/struct.KdlNode.html
[`KdlEntry`]: https://docs.rs/kdl/3.0.1-alpha.0/kdl/struct.KdlEntry.html
[`KdlIdentifier`]: https://docs.rs/kdl/3.0.1-alpha.0/kdl/struct.KdlIdentifier.html
