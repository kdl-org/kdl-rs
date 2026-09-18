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
