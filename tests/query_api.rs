use kdl::{KdlDocument, KdlQuery};
use miette::Result;

#[test]
fn document_query_all() -> Result<()> {
    let doc = "foo\nbar\nbaz".parse::<KdlDocument>()?;
    let results = doc.query_all("bar")?;
    assert_eq!(results.count(), 1);
    let results = doc.query_all(String::from("bar"))?;
    assert_eq!(results.count(), 1);
    let results = doc.query_all(&String::from("bar"))?;
    assert_eq!(results.count(), 1);
    let results = doc.query_all("bar".parse::<KdlQuery>()?)?;
    assert_eq!(results.count(), 1);

    let results = doc.query_all("scope()")?;
    assert_eq!(
        results.count(),
        0,
        "scope() on its own doesn't return anything if querying from a doc."
    );

    Ok(())
}

#[test]
fn document_query() -> Result<()> {
    let doc = "foo\nbar\nbaz".parse::<KdlDocument>()?;

    assert!(doc.query("bar")?.is_some());
    assert!(doc.query(String::from("bar"))?.is_some());
    assert!(doc.query(&String::from("bar"))?.is_some());
    assert!(doc.query("bar".parse::<KdlQuery>()?)?.is_some());

    assert!(doc.query("scope()")?.is_none());

    Ok(())
}

#[test]
fn document_query_get() -> Result<()> {
    let doc = "foo\nbar true\nbaz".parse::<KdlDocument>()?;

    assert_eq!(doc.query_get("bar", 0)?, Some(&true.into()));
    assert_eq!(doc.query_get(String::from("bar"), 0)?, Some(&true.into()));
    assert_eq!(doc.query_get(&String::from("bar"), 0)?, Some(&true.into()));
    assert_eq!(
        doc.query_get("bar".parse::<KdlQuery>()?, 0)?,
        Some(&true.into())
    );

    Ok(())
}

#[test]
fn document_query_get_all() -> Result<()> {
    let doc = "foo\nbar true\nbaz false".parse::<KdlDocument>()?;

    assert_eq!(
        doc.query_get_all("[]", 0)?.collect::<Vec<_>>(),
        vec![&true.into(), &false.into()]
    );
    assert_eq!(doc.query_get_all(String::from("[]"), 0)?.count(), 2);
    assert_eq!(doc.query_get_all(&String::from("[]"), 0)?.count(), 2);
    assert_eq!(doc.query_get_all("[]".parse::<KdlQuery>()?, 0)?.count(), 2);

    Ok(())
}

#[test]
fn node_query_all() -> Result<()> {
    let doc = r#"
        foo
        bar {
            a {
                b
            }
        }
        baz
    "#
    .parse::<KdlDocument>()?;
    let node = doc.query("bar")?.unwrap();

    let results = node.query_all("b")?;
    assert_eq!(results.count(), 1);
    let results = node.query_all(String::from("b"))?;
    assert_eq!(results.count(), 1);
    let results = node.query_all(&String::from("b"))?;
    assert_eq!(results.count(), 1);
    let results = node.query_all("b".parse::<KdlQuery>()?)?;
    assert_eq!(results.count(), 1);

    let results = node.query_all("scope()")?.collect::<Vec<_>>();
    assert_eq!(results[0], node);

    let results = node.query_all("scope() > a".parse::<KdlQuery>()?)?;
    assert_eq!(results.count(), 1);

    let results = node.query_all("scope() > b".parse::<KdlQuery>()?)?;
    assert_eq!(results.count(), 0);

    Ok(())
}

#[test]
fn node_query() -> Result<()> {
    let doc = r#"
        foo
        bar {
            a {
                b
            }
        }
        baz
    "#
    .parse::<KdlDocument>()?;
    let node = doc.query("bar")?.unwrap();

    assert!(node.query("b")?.is_some());
    assert!(node.query(String::from("b"))?.is_some());
    assert!(node.query(&String::from("b"))?.is_some());
    assert!(node.query("b".parse::<KdlQuery>()?)?.is_some());

    assert_eq!(node.query("scope()")?, Some(node));
    assert!(node.query("scope() > a")?.is_some());
    assert!(node.query("scope() > b")?.is_none());

    Ok(())
}

#[test]
fn node_query_get() -> Result<()> {
    let doc = r#"
        foo
        bar 1 2 3 {
            a false {
                b true
            }
        }
        baz
    "#
    .parse::<KdlDocument>()?;
    let node = doc.query("bar")?.unwrap();

    assert_eq!(node.query_get("b", 0)?, Some(&true.into()));
    assert_eq!(node.query_get(String::from("b"), 0)?, Some(&true.into()));
    assert_eq!(node.query_get(&String::from("b"), 0)?, Some(&true.into()));
    assert_eq!(
        node.query_get("b".parse::<KdlQuery>()?, 0)?,
        Some(&true.into())
    );

    assert_eq!(node.query_get("scope()", 0)?, Some(&1.into()));
    assert_eq!(node.query_get("scope() > a", 0)?, Some(&false.into()));
    assert!(node.query_get("scope() > b", "prop")?.is_none());
    Ok(())
}

#[test]
fn node_query_get_all() -> Result<()> {
    let doc = r#"
        foo
        bar 1 2 3 {
            a false {
                b true
            }
        }
        baz
    "#
    .parse::<KdlDocument>()?;
    let node = doc.query("bar")?.unwrap();

    assert_eq!(
        node.query_get_all("[]", 0)?.collect::<Vec<_>>(),
        vec![&false.into(), &true.into()]
    );
    assert_eq!(node.query_get_all(String::from("[]"), 0)?.count(), 2);
    assert_eq!(node.query_get_all(&String::from("[]"), 0)?.count(), 2);
    assert_eq!(node.query_get_all("[]".parse::<KdlQuery>()?, 0)?.count(), 2);

    Ok(())
}
