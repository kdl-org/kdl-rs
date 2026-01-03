use kdl::{KdlDocument, KdlNode};

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
fn multiline_string_never() -> miette::Result<()> {
    let input = r#"multi """
      a
      b
      c
      """
multi-explicit "a\nb\nc""#;

    let mut doc: KdlDocument = input.parse()?;
    doc.autoformat();

    let output = doc.to_string();
    assert_eq!(
        output,
        "multi \"a\\nb\\nc\"\nmulti-explicit \"a\\nb\\nc\"\n"
    );
    Ok(())
}

#[test]
fn multiline_string_triple_quotes() -> miette::Result<()> {
    let input = r#"multi """
      a
      b
      c
      """
multi-explicit "a\nb\nc""#;

    let mut doc: KdlDocument = input.parse()?;
    doc.autoformat_config(
        &kdl::FormatConfig::builder()
            .expand_multiline(kdl::MultilineStringExpansion::TripleQuotes)
            .build(),
    );

    let output = doc.to_string();
    assert!(output.contains("multi \"\"\""));
    assert!(output.contains("multi-explicit \"a\\nb\\nc\""));
    Ok(())
}

#[test]
fn multiline_string_always() -> miette::Result<()> {
    let input = r#"multi """
      a
      b
      c
      """
multi-explicit "a\nb\nc""#;

    let mut doc: KdlDocument = input.parse()?;
    doc.autoformat_config(
        &kdl::FormatConfig::builder()
            .expand_multiline(kdl::MultilineStringExpansion::Always)
            .build(),
    );

    let output = doc.to_string();
    assert!(output.contains("multi \"\"\""));
    assert!(output.contains("multi-explicit \"\"\""));
    Ok(())
}

#[test]
fn multiline_string_nested_triple_quotes() -> miette::Result<()> {
    let input = r#"some-node {
    multi """
        a
        b
        c
     """
     explicit "a\nb\nc"
}"#;

    let mut doc: KdlDocument = input.parse()?;
    doc.autoformat_config(
        &kdl::FormatConfig::builder()
            .expand_multiline(kdl::MultilineStringExpansion::TripleQuotes)
            .build(),
    );

    let output = doc.to_string();
    // Multiline string should be preserved
    assert!(output.contains("multi \"\"\""));
    // Regular string should be normalized
    assert!(output.contains("explicit \"a\\nb\\nc\""));
    // Indentation should be normalized to 4 spaces
    assert!(output.contains("    multi"));
    assert!(output.contains("    explicit"));
    Ok(())
}
