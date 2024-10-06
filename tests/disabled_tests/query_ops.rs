use kdl::{KdlDocument, KdlNode};
use miette::Result;
use pretty_assertions::assert_eq;

#[test]
fn scope_with_all_children() -> Result<()> {
    let doc: KdlDocument = r#"
            foo {
                bar
                baz
            }
            bar
            baz
            "#
    .parse()?;

    let results = doc.query_all("scope() > []")?.collect::<Vec<&KdlNode>>();

    assert_eq!(&results, &doc.nodes().iter().collect::<Vec<&KdlNode>>());
    Ok(())
}

#[test]
fn scope_child_by_name() -> Result<()> {
    let doc: KdlDocument = r#"
            foo {
                bar
                baz
            }
            bar {
                a
                b
            }
            baz
            "#
    .parse()?;

    let results = doc.query_all("scope() > bar")?.collect::<Vec<&KdlNode>>();

    assert_eq!(results, vec![&doc.nodes()[1]]);

    // Scope from a specific node.
    let results = results[0]
        .query_all("scope() > a")?
        .collect::<Vec<&KdlNode>>();

    assert_eq!(
        results,
        vec![&doc.nodes()[1].children().unwrap().nodes()[0]]
    );

    Ok(())
}

#[test]
fn scope_descendants() -> Result<()> {
    let doc: KdlDocument = r#"
            foo {
                bar
                baz
            }
            bar
            baz
            "#
    .parse()?;

    let results = doc.query_all("scope() >> bar")?.collect::<Vec<&KdlNode>>();

    assert_eq!(
        results,
        vec![
            &doc.nodes()[0].children().unwrap().nodes()[0],
            &doc.nodes()[1]
        ]
    );
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
            foo {
                bar
                baz
            }
            bar
            baz
            "#
    .parse()?;

    let results = doc.query_all("bar")?.collect::<Vec<&KdlNode>>();

    assert_eq!(
        results,
        vec![
            &doc.nodes()[0].children().unwrap().nodes()[0],
            &doc.nodes()[1]
        ]
    );
    Ok(())
}

#[test]
fn node_descendants() -> Result<()> {
    let doc: KdlDocument = r#"
            foo {
                bar
                baz
            }
            bar
            baz {
                foo {
                    bar {
                        bar
                    }
                }
            }
            "#
    .parse()?;

    let results = doc.query_all("foo >> bar")?.collect::<Vec<&KdlNode>>();

    assert_eq!(
        results,
        vec![
            &doc.nodes()[0].children().unwrap().nodes()[0],
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
    Ok(())
}

#[test]
fn node_children() -> Result<()> {
    let doc: KdlDocument = r#"
            foo {
                bar
                baz
            }
            bar
            baz {
                foo {
                    bar {
                        bar
                    }
                }
            }
            "#
    .parse()?;

    let results = doc.query_all("foo > bar")?.collect::<Vec<&KdlNode>>();

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
    Ok(())
}

#[test]
fn node_neighbor() -> Result<()> {
    let doc: KdlDocument = r#"
            foo {
                bar
                baz
            }
            bar
            baz
            "#
    .parse()?;

    let results = doc.query_all("foo + bar")?.collect::<Vec<&KdlNode>>();

    assert_eq!(results, vec![&doc.nodes()[1]]);

    let results = doc.query_all("foo + bar + baz")?.collect::<Vec<&KdlNode>>();

    assert_eq!(results, vec![&doc.nodes()[2]]);

    Ok(())
}

#[test]
fn node_sibling() -> Result<()> {
    let doc: KdlDocument = r#"
            foo {
                bar
                baz
            }
            bar
            baz
            quux
            other
            "#
    .parse()?;

    let results = doc.query_all("foo ++ bar")?.collect::<Vec<&KdlNode>>();

    assert_eq!(results, vec![&doc.nodes()[1]]);

    let results = doc.query_all("foo ++ baz")?.collect::<Vec<&KdlNode>>();

    assert_eq!(results, vec![&doc.nodes()[2]]);

    let results = doc
        .query_all("foo ++ bar ++ other")?
        .collect::<Vec<&KdlNode>>();

    assert_eq!(results, vec![&doc.nodes()[4]]);

    Ok(())
}

#[test]
fn multiple_selectors() -> Result<()> {
    let doc: KdlDocument = r#"
            foo {
                bar
                baz
            }
            bar
            baz {
                foo {
                    bar {
                        bar
                    }
                }
            }
            "#
    .parse()?;

    let results = doc.query_all("foo, baz")?.collect::<Vec<&KdlNode>>();

    assert_eq!(
        results,
        vec![
            &doc.nodes()[0],
            &doc.nodes()[0].children().unwrap().nodes()[1],
            &doc.nodes()[2],
            &doc.nodes()[2].children().unwrap().nodes()[0]
        ],
        "First match all the `foo`s, then all the `baz`s."
    );

    Ok(())
}

#[test]
fn all_combined() -> Result<()> {
    let doc: KdlDocument = r#"
            foo {
                bar {
                    baz {
                        foo {
                            bar {
                                bar
                            }
                        }
                        bar
                        baz
                        quux
                        other
                    }
                }
            }
            bar
            baz
            "#
    .parse()?;

    let results = doc
        .query_all("foo >> baz > foo + bar ++ other")?
        .collect::<Vec<&KdlNode>>();

    assert_eq!(
        results,
        vec![
            &doc.nodes()[0].children().unwrap().nodes()[0]
                .children()
                .unwrap()
                .nodes()[0]
                .children()
                .unwrap()
                .nodes()[4]
        ]
    );
    Ok(())
}
