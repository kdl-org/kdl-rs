// Shows how to maintain nodes sorted by value in a
// machine-generated kdl document

use kdl::{KdlDocument, KdlIdentifier, KdlNode};

fn sort_by_name(x: &KdlNode, y: &KdlNode) -> std::cmp::Ordering {
    x.name().value().cmp(y.name().value())
}

fn main() -> miette::Result<()> {
    let input = r#"
words {
    apple  // one a day keeps the doctor away
    orange
}
    "#;
    let mut doc: KdlDocument = input.parse()?;

    let words_section = doc.get_mut("words").expect("'words' section should exist");
    let children = words_section
        .children_mut()
        .as_mut()
        .expect("'words' section should have children");
    let word_nodes = children.nodes_mut();

    let identifier = KdlIdentifier::from("banana");
    let word_node = KdlNode::new(identifier);
    word_nodes.push(word_node);
    word_nodes.sort_by(sort_by_name);
    words_section.fmt();

    println!("{}", doc.to_string());

    // output:
    // words {
    //     apple // one a day keeps the doctor away
    //     banana
    //     orange
    // }

    Ok(())
}
