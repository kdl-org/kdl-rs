//! Tests the kdl files in the examples directory.

use kdl::*;
use std::collections::HashMap;

/// Helper for constructing nodes.
///
/// This takes input that's similar to KDL itself, but each node must be terminated with either
/// a semicolon or a braced block. Nodes whose name contains characters not valid in Rust
/// identifiers must be written as a string literal instead.
macro_rules! nodes {
    ([$v:ident]:name) => {};
    ([$v:ident]:name $name:ident $($tt:tt)*) => {
        nodes!([$v]:values stringify!($name); {} {} $($tt)*)
    };
    ([$v:ident]:name $name:literal $($tt:tt)*) => {
        nodes!([$v]:values $name; {} {} $($tt)*)
    };
    ([$v:ident]:values $name:expr; {$($value:literal,)*} $props:tt $new_value:literal $($tt:tt)*) => {
        nodes!([$v]:values $name; {$($value,)* $new_value,} $props $($tt)*)
    };
    ([$v:ident]:values $name:expr; $values:tt {$($key:ident=$prop:literal,)*} $new_key:ident=$new_prop:literal $($tt:tt)*) => {
        nodes!([$v]:values $name; $values {$($key=$prop,)* $new_key=$new_prop,} $($tt)*)
    };
    ([$v:ident]:values $name:expr; $values:tt $props:tt $(; $($tt:tt)*)?) => {
        nodes!([$v]:values $name; $values $props {} $($($tt)*)?)
    };
    ([$v:ident]:values $name:expr; {$($value:literal,)*} {$($key:ident=$prop:literal,)*} {$($child:tt)*} $($tail:tt)*) => {
        $v.push(KdlNode {
            name: $name.to_owned(),
            values: vec![$( $value.to_owned().into() ),*],
            properties: {
                #[allow(unused_mut)]
                let mut map = HashMap::new();
                $(
                    map.insert(stringify!($key).to_owned(), $prop.to_owned().into());
                )*
                map
            },
            children: nodes!($($child)*),
        });
        nodes!([$v]:name $($tail)*);
    };
    // Explicitly match literal and ident at the start instead of $($tt:tt)*
    // so we get better errors than "recursion limit exceeded" if we fail to match.
    (:start $($tt:tt)+) => {{
        let mut v = Vec::new();
        nodes!([v]:name $($tt)+);
        v
    }};
    ($name:literal $($tt:tt)*) => {
        nodes!(:start $name $($tt)*)
    };
    ($name:ident $($tt:tt)*) => {
        nodes!(:start $name $($tt)*)
    };
    () => { vec![] }
}

#[test]
fn test_ci() {
    let doc = parse_document(include_str!("../examples/ci.kdl"));
    let nodes = nodes! {
        name "CI";
        on "push" "pull_request";
        env {
            RUSTFLAGS "-Dwarnings"
        }
        jobs {
            fmt_and_docs "Check fmt & build docs" {
                "runs-on" "ubuntu-latest";
                steps {
                    step uses="actions/checkout@v1";
                    step "Install Rust" uses="actions-rs/toolchain@v1" {
                        profile "minimal";
                        toolchain "stable";
                        components "rustfmt";
                        override true;
                    }
                    step "rustfmt" run="cargo fmt --all -- --check";
                    step "docs" run="cargo doc --no-deps";
                }
            }
            build_and_test "Build & Test" {
                "runs-on" "${{ matrix.os }}";
                strategy {
                    matrix {
                        rust "1.46.0" "stable";
                        os "ubuntu-latest" "macOS-latest" "windows-latest";
                    }
                }

                steps {
                    step uses="actions/checkout@v1";
                    step "Install Rust" uses="actions-rs/toolchain@v1" {
                        profile "minimal";
                        toolchain "${{ matrix.rust }}";
                        components "clippy";
                        override true;
                    }
                    step "Clippy" run="cargo clippy --all -- -D warnings";
                    step "Run tests" run="cargo test --all --verbose";
                }
            }
        }
    };
    assert_eq!(doc, Ok(nodes));
}

#[test]
fn test_cargo() {
    let doc = parse_document(include_str!("../examples/cargo.kdl"));
    let nodes = nodes! {
        package {
            name "kdl";
            version "0.0.0";
            description "kat's document language";
            authors "Kat Marchán <kzm@zkat.tech>";
            "license-file" "LICENSE.md";
            edition "2018";
        }
        dependencies {
            nom "6.0.1";
            thiserror "1.0.22";
        }
    };
    assert_eq!(doc, Ok(nodes));
}
