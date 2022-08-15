use kdl::{KdlDocument, KdlNode, KdlValue};

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
    doc.fmt();
    let fmt = doc.to_string();
    println!("{fmt}");
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
