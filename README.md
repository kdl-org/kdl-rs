# KDL - The KDL Document Language

[KDL](https://github.com/kdl-org/kdl) is a document language with xml-like
semantics that looks like you're invoking a bunch of CLI commands!

It's meant to be used both as a serialization format and a configuration
language, and is relatively light on syntax compared to XML.

There's a living
[specification](https://github.com/kdl-org/kdl/blob/main/SPEC.md), as well as
[various implementations](https://github.com/kdl-org/kdl#implementations). The language is based on
[SDLang](https://sdlang.org), with a number of modifications and
clarifications on its syntax and behavior.

This repository is the official/reference implementation in Rust, and
corresponds to [the kdl crate](https://crates.io/crates/kdl)

## Design and Discussion

KDL is still extremely new, and discussion about the format should happen over
on the [spec repo's discussions
page](https://github.com/kdoclang/kdl/discussions). Feel free to jump in and
give us your 2 cents!

## Example KDL File

```text
author "Alex Monad" email="alex@example.com" active=true

contents {
  section "First section" {
    paragraph "This is the first paragraph"
    paragraph "This is the second paragraph"
  }
}

// unicode! comments!
π 3.14159
```

## Basic Library Example

```
use kdl::{KdlNode, KdlValue};
use std::collections::HashMap;

assert_eq!(
    kdl::parse_document("node 1 key=true").unwrap(),
    vec![
        KdlNode {
            name: String::from("node"),
            values: vec![KdlValue::Int(1)],
            properties: {
                let mut temp = HashMap::new();
                temp.insert(String::from("key"), KdlValue::Boolean(true));
                temp
            },
            children: vec![],
        }
    ]
)
```

## License

The code in this repository is covered by [the Apache-2.0 License](LICENSE.md).
