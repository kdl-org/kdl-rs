use kdl::{KdlDocument, KdlNode};
use miette::Result;
use pretty_assertions::assert_eq;

#[test]
fn scope_alone() -> Result<()> {
    let doc: KdlDocument = r#"
            foo {
                bar
                baz
            }
            bar
            baz
            "#
    .parse()?;

    // let results = doc.query_all("scope()")?.collect::<Vec<&KdlNode>>();

    // assert_eq!(results, Vec::<&KdlNode>::new());

    let results = doc.nodes()[0]
        .query_all("scope()")?
        .collect::<Vec<&KdlNode>>();

    assert_eq!(results, vec![&doc.nodes()[0]]);

    Ok(())
}

#[test]
fn scope_only_at_top() -> Result<()> {
    let doc: KdlDocument = r#"
            foo {
                bar
            }
            "#
    .parse()?;

    assert!(
        doc.query_all("foo >> scope()").is_err(),
        "scope() must be at the top level"
    );

    Ok(())
}

#[test]
fn any_descendants() -> Result<()> {
    let doc: KdlDocument = r#"
            foo
            bar
            baz
            "#
    .parse()?;

    let results = doc.query_all("bar")?.collect::<Vec<&KdlNode>>();

    assert_eq!(results, vec![&doc.nodes()[1]]);
    Ok(())
}

#[test]
fn prop_matcher() -> Result<()> {
    let doc: KdlDocument = r#"
            foo {
                bar p=1
                baz
            }
            bar p=1
            baz p=2 {
                foo {
                    bar p=1 {
                        bar p=2
                    }
                }
            }
            "#
    .parse()?;

    let results = doc.query_all("[p = 2]")?.collect::<Vec<&KdlNode>>();

    assert_eq!(
        results,
        vec![
            &doc.nodes()[2],
            &doc.nodes()[2].children().unwrap().nodes()[0]
                .children()
                .unwrap()
                .nodes()[0]
                .children()
                .unwrap()
                .nodes()[0]
        ]
    );

    let results = doc.query_all("[p = 1]")?.collect::<Vec<&KdlNode>>();

    assert_eq!(
        results,
        vec![
            &doc.nodes()[0].children().unwrap().nodes()[0],
            &doc.nodes()[1],
            &doc.nodes()[2].children().unwrap().nodes()[0]
                .children()
                .unwrap()
                .nodes()[0]
        ]
    );

    assert_eq!(
        doc.query_all("[prop(p) = 1]")?.collect::<Vec<&KdlNode>>(),
        results
    );

    let results = doc.query_all("[p]")?.collect::<Vec<&KdlNode>>();

    assert_eq!(
        results,
        vec![
            &doc.nodes()[0].children().unwrap().nodes()[0],
            &doc.nodes()[1],
            &doc.nodes()[2],
            &doc.nodes()[2].children().unwrap().nodes()[0]
                .children()
                .unwrap()
                .nodes()[0],
            &doc.nodes()[2].children().unwrap().nodes()[0]
                .children()
                .unwrap()
                .nodes()[0]
                .children()
                .unwrap()
                .nodes()[0]
        ]
    );

    assert_eq!(
        doc.query_all("[prop(p)]")?.collect::<Vec<&KdlNode>>(),
        results
    );

    Ok(())
}

#[test]
fn empty_arg_matcher() -> Result<()> {
    let doc: KdlDocument = r#"
            foo {
                bar 1
                baz
            }
            bar 2
            baz {
                foo {
                    bar 1 {
                        bar
                    }
                }
            }
            "#
    .parse()?;

    let results = doc.query_all("[arg() = 1]")?.collect::<Vec<&KdlNode>>();

    assert_eq!(
        results,
        vec![
            &doc.nodes()[0].children().unwrap().nodes()[0],
            &doc.nodes()[2].children().unwrap().nodes()[0]
                .children()
                .unwrap()
                .nodes()[0]
        ]
    );

    let results = doc.query_all("[arg()]")?.collect::<Vec<&KdlNode>>();

    assert_eq!(
        results,
        vec![
            &doc.nodes()[0].children().unwrap().nodes()[0],
            &doc.nodes()[1],
            &doc.nodes()[2].children().unwrap().nodes()[0]
                .children()
                .unwrap()
                .nodes()[0]
        ]
    );

    Ok(())
}

#[test]
fn indexed_arg_matcher() -> Result<()> {
    let doc: KdlDocument = r#"
            foo {
                bar 1 2
                baz
            }
            bar 2 1
            baz {
                foo {
                    bar 1 2 {
                        bar 1 3 2
                    }
                }
            }
            "#
    .parse()?;

    let results = doc.query_all("[arg(1) = 2]")?.collect::<Vec<&KdlNode>>();

    assert_eq!(
        results,
        vec![
            &doc.nodes()[0].children().unwrap().nodes()[0],
            &doc.nodes()[2].children().unwrap().nodes()[0]
                .children()
                .unwrap()
                .nodes()[0]
        ]
    );

    let results = doc.query_all("[arg(2) = 2]")?.collect::<Vec<&KdlNode>>();

    assert_eq!(
        results,
        vec![
            &doc.nodes()[2].children().unwrap().nodes()[0]
                .children()
                .unwrap()
                .nodes()[0]
                .children()
                .unwrap()
                .nodes()[0]
        ]
    );

    Ok(())
}

#[test]
fn type_annotation_matcher() -> Result<()> {
    let doc: KdlDocument = r#"
            foo {
                (here)bar
                baz
            }
            bar
            baz {
                (here)foo {
                    bar {
                        (here)bar
                    }
                }
            }
            "#
    .parse()?;

    let results = doc.query_all("(here)")?.collect::<Vec<&KdlNode>>();

    assert_eq!(
        results,
        vec![
            &doc.nodes()[0].children().unwrap().nodes()[0],
            &doc.nodes()[2].children().unwrap().nodes()[0],
            &doc.nodes()[2].children().unwrap().nodes()[0]
                .children()
                .unwrap()
                .nodes()[0]
                .children()
                .unwrap()
                .nodes()[0]
        ]
    );

    let results = doc
        .query_all("[type() = \"here\"]")?
        .collect::<Vec<&KdlNode>>();

    assert_eq!(
        results,
        vec![
            &doc.nodes()[0].children().unwrap().nodes()[0],
            &doc.nodes()[2].children().unwrap().nodes()[0],
            &doc.nodes()[2].children().unwrap().nodes()[0]
                .children()
                .unwrap()
                .nodes()[0]
                .children()
                .unwrap()
                .nodes()[0]
        ]
    );

    let results = doc.query_all("()")?.collect::<Vec<&KdlNode>>();

    assert_eq!(
        results,
        vec![
            &doc.nodes()[0].children().unwrap().nodes()[0],
            &doc.nodes()[2].children().unwrap().nodes()[0],
            &doc.nodes()[2].children().unwrap().nodes()[0]
                .children()
                .unwrap()
                .nodes()[0]
                .children()
                .unwrap()
                .nodes()[0]
        ]
    );

    Ok(())
}
