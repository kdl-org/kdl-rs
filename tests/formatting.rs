use kdl::{KdlDocument, KdlNode};

use pretty_assertions::assert_eq;

fn build_abc() -> KdlDocument {
    let mut c = KdlNode::new("c");
    c.ensure_children();
    let mut b = KdlNode::new("b");
    b.ensure_children().nodes_mut().push(c);
    let mut a = KdlNode::new("a");
    a.ensure_children().nodes_mut().push(b);
    let mut doc = KdlDocument::new();
    doc.nodes_mut().push(a);
    doc.fmt();
    doc
}

#[test]
fn build_and_format() {
    let fmt = build_abc().to_string();
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
fn build_and_parse() -> miette::Result<()> {
    let mut built = build_abc();
    let mut parsed: KdlDocument = build_abc().to_string().parse()?;
    built.fmt();
    parsed.fmt();
    assert_eq!(built, parsed);
    Ok(())
}
