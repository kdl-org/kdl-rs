use kdl::{FormatConfig, KdlDocument, KdlNode};

#[test]
fn build_and_format() {
    let mut c = KdlNode::new("c");
    c.ensure_children();
    let mut b = KdlNode::new("b");
    b.ensure_children().nodes_mut().push(c);
    let mut a = KdlNode::new("a");
    a.ensure_children().nodes_mut().push(b);

    let mut doc = KdlDocument::new();
    doc.nodes_mut().push(a);
    doc.autoformat();
    let fmt = doc.to_string();
    assert_eq!(
        fmt,
        r#"a {
    b {
        c {

        }
    }
}
"#
    );
}

#[test]
#[cfg(feature = "v1")]
fn ensure_v1_does_not_over_escape_forward_slash() {
    // Per the v1 spec, `\/` is a permitted escape but unescaped `/` is also
    // legal. ensure_v1 should not introduce unnecessary `\/` escapes.
    let input = "node \"a/b/c\"\n";
    let mut doc = KdlDocument::parse_v1(input).unwrap();
    doc.ensure_v1();
    assert_eq!(doc.to_string(), input);
}

#[test]
#[cfg(feature = "v1")]
fn ensure_v2_strips_escaped_forward_slash() {
    // `\/` is forbidden in v2, so ensure_v2 must convert it to a `/`.
    let input = "node \"a\\/b\"\n";
    let mut doc = KdlDocument::parse_v1(input).unwrap();
    doc.ensure_v2();
    assert_eq!(doc.to_string(), "node \"a/b\"\n");
}

#[test]
#[cfg(feature = "v1")]
fn ensure_v1_preserves_raw_string_with_backslash_slash() {
    // In a raw string, `\/` is two literal characters, and should be kept as is.
    let input = "node r#\"a\\/b\"#\n";
    let mut doc = KdlDocument::parse_v1(input).unwrap();
    doc.ensure_v1();
    assert_eq!(doc.to_string(), input);
}

/// Make sure that indentation rules for nodes that are programatically created
/// are applied correctly when using a custom config.
#[test]
fn format_fresh_nested_nodes_with_custom_indent() {
    let mut doc = KdlDocument::new();
    let mut parent = KdlNode::new("parent");
    let mut child = KdlNode::new("child");

    child
        .ensure_children()
        .nodes_mut()
        .push(KdlNode::new("innermost"));
    parent.ensure_children().nodes_mut().push(child);
    doc.nodes_mut().push(parent);

    // Create a custom indent config and make sure it's properly applied.
    let config = FormatConfig::builder().indent("  ").build();
    doc.autoformat_config(&config);
    let once = doc.to_string();
    assert_eq!(
        once,
        r#"parent {
  child {
    innermost
  }
}
"#
    );
}
