use std::{fmt::Display, str::FromStr};

use crate::{parser, KdlError, KdlNode, KdlValue};

/// Represents a KDL
/// [`Document`](https://github.com/kdl-org/kdl/blob/main/SPEC.md#document).
///
/// This type is also used to manage a [`KdlNode`]'s [`Children
/// Block`](https://github.com/kdl-org/kdl/blob/main/SPEC.md#children-block),
/// when present.
///
/// # Examples
///
/// The easiest way to create a `KdlDocument` is to parse it:
/// ```rust
/// # use kdl::KdlDocument;
/// let kdl: KdlDocument = "foo 1 2 3\nbar 4 5 6".parse().expect("parse failed");
/// ```
#[derive(Debug, Default, Clone, PartialEq)]
pub struct KdlDocument {
    pub(crate) leading: Option<String>,
    pub(crate) nodes: Vec<KdlNode>,
    pub(crate) trailing: Option<String>,
}

impl KdlDocument {
    /// Creates a new Document.
    pub fn new() -> Self {
        Default::default()
    }

    /// Gets the first child node with a matching name.
    pub fn get(&self, name: &str) -> Option<&KdlNode> {
        self.nodes.iter().find(move |n| n.name().value() == name)
    }

    /// Gets a reference to the first child node with a matching name.
    pub fn get_mut(&mut self, name: &str) -> Option<&mut KdlNode> {
        self.nodes
            .iter_mut()
            .find(move |n| n.name().value() == name)
    }

    /// Gets the first argument (value) of the first child node with a
    /// matching name. This is a shorthand utility for cases where a document
    /// is being used as a key/value store.
    ///
    /// # Examples
    ///
    /// Given a document like this:
    /// ```kdl
    /// foo 1
    /// bar false
    /// ```
    ///
    /// You can fetch the value of `foo` in a single call like this:
    /// ```rust
    /// # use kdl::{KdlDocument, KdlValue};
    /// # let doc: KdlDocument = "foo 1\nbar false".parse().unwrap();
    /// assert_eq!(doc.get_arg("foo"), Some(&1.into()));
    /// ```
    pub fn get_arg(&self, name: &str) -> Option<&KdlValue> {
        self.get(name)
            .and_then(|node| node.get(0))
            .map(|e| e.value())
    }

    /// Gets the all node arguments (value) of the first child node with a
    /// matching name. This is a shorthand utility for cases where a document
    /// is being used as a key/value store and the value is expected to be
    /// array-ish.
    ///
    /// If a node has no arguments, this will return an empty array.
    ///
    /// # Examples
    ///
    /// Given a document like this:
    /// ```kdl
    /// foo 1 2 3
    /// bar false
    /// ```
    ///
    /// You can fetch the arguments for `foo` in a single call like this:
    /// ```rust
    /// # use kdl::{KdlDocument, KdlValue};
    /// # let doc: KdlDocument = "foo 1 2 3\nbar false".parse().unwrap();
    /// assert_eq!(doc.get_args("foo"), vec![&1.into(), &2.into(), &3.into()]);
    /// ```
    pub fn get_args(&self, name: &str) -> Vec<&KdlValue> {
        self.get(name)
            .map(|n| n.entries())
            .unwrap_or_default()
            .iter()
            .filter(|e| e.name().is_none())
            .map(|e| e.value())
            .collect()
    }

    /// Gets a mutable reference to the first argument (value) of the first
    /// child node with a matching name. This is a shorthand utility for cases
    /// where a document is being used as a key/value store.
    pub fn get_arg_mut(&mut self, name: &str) -> Option<&mut KdlValue> {
        self.get_mut(name)
            .and_then(|node| node.get_mut(0))
            .map(|e| e.value_mut())
    }

    /// This utility makes it easy to interact with a KDL convention where
    /// child nodes named `-` are treated as array-ish values.
    ///
    /// # Examples
    ///
    /// Given a document like this:
    /// ```kdl
    /// foo {
    ///   - 1
    ///   - 2
    ///   - false
    /// }
    /// ```
    ///
    /// You can fetch the dashed child values of `foo` in a single call like this:
    /// ```rust
    /// # use kdl::{KdlDocument, KdlValue};
    /// # let doc: KdlDocument = "foo {\n - 1\n - 2\n - false\n}".parse().unwrap();
    /// assert_eq!(doc.get_dash_vals("foo"), vec![&1.into(), &2.into(), &false.into()]);
    /// ```
    pub fn get_dash_vals(&self, name: &str) -> Vec<&KdlValue> {
        self.get(name)
            .and_then(|n| n.children())
            .map(|doc| doc.nodes())
            .unwrap_or_default()
            .iter()
            .filter(|e| e.name().value() == "-")
            .map(|e| e.get(0))
            .filter(|v| v.is_some())
            .map(|v| v.unwrap().value())
            .collect()
    }

    /// Returns a reference to this document's child nodes.
    pub fn nodes(&self) -> &[KdlNode] {
        &self.nodes
    }

    /// Returns a mutable reference to this document's child nodes.
    pub fn nodes_mut(&mut self) -> &mut Vec<KdlNode> {
        &mut self.nodes
    }

    /// Gets leading text (whitespace, comments) for this KdlDocument.
    pub fn leading(&self) -> Option<&str> {
        self.leading.as_deref()
    }

    /// Sets leading text (whitespace, comments) for this KdlDocument.
    pub fn set_leading(&mut self, leading: impl Into<String>) {
        self.leading = Some(leading.into());
    }

    /// Gets trailing text (whitespace, comments) for this KdlDocument.
    pub fn trailing(&self) -> Option<&str> {
        self.trailing.as_deref()
    }

    /// Sets trailing text (whitespace, comments) for this KdlDocument.
    pub fn set_trailing(&mut self, trailing: impl Into<String>) {
        self.trailing = Some(trailing.into());
    }

    /// Length of this document when rendered as a string.
    pub fn len(&self) -> usize {
        format!("{}", self).len()
    }

    /// Returns true if this document is completely empty (including whitespace)
    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    /// Clears leading and trailing text (whitespace, comments). `KdlNode`s in
    /// this document will be unaffected.
    pub fn clear_fmt(&mut self) {
        self.leading = None;
        self.trailing = None;
    }

    /// Auto-formats this Document, making everything nice while preserving
    /// comments.
    pub fn fmt(&mut self) {
        self.fmt_impl(0, false);
    }

    /// Formats the document and removes all comments from the document.
    pub fn fmt_no_comments(&mut self) {
        self.fmt_impl(0, true);
    }
}

impl Display for KdlDocument {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.stringify(f, 0)
    }
}

impl KdlDocument {
    pub(crate) fn fmt_impl(&mut self, indent: usize, no_comments: bool) {
        if let Some(s) = self.leading.as_mut() {
            crate::fmt::fmt_leading(s, indent, no_comments);
        }
        let mut has_nodes = false;
        for node in &mut self.nodes {
            has_nodes = true;
            node.fmt_impl(indent, no_comments);
        }
        if let Some(s) = self.trailing.as_mut() {
            crate::fmt::fmt_trailing(s, no_comments);
            if !has_nodes {
                s.push('\n');
            }
        }
    }

    pub(crate) fn stringify(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        indent: usize,
    ) -> std::fmt::Result {
        if let Some(leading) = &self.leading {
            write!(f, "{}", leading)?;
        }
        for node in &self.nodes {
            node.stringify(f, indent)?;
            if node.trailing.is_none() {
                writeln!(f)?;
            }
        }
        if let Some(trailing) = &self.trailing {
            write!(f, "{}", trailing)?;
        }
        Ok(())
    }
}

impl IntoIterator for KdlDocument {
    type Item = KdlNode;
    type IntoIter = std::vec::IntoIter<Self::Item>;

    fn into_iter(self) -> Self::IntoIter {
        self.nodes.into_iter()
    }
}

impl FromStr for KdlDocument {
    type Err = KdlError;

    fn from_str(input: &str) -> Result<Self, Self::Err> {
        parser::parse(input, parser::document)
    }
}

#[cfg(test)]
mod test {
    use crate::{KdlEntry, KdlValue};

    use super::*;

    #[test]
    fn parsing() -> miette::Result<()> {
        let src = "
// This is the first node
foo 1 2 \"three\" null true bar=\"baz\" {
    - 1
    - 2
    - \"three\"
    (mytype)something (\"name\")\"else\"\r
}

null_id null_prop=null
true_id true_prop=null
+false true

         bar \"indented\" // trailing whitespace after this\t
/*
Some random comment
 */

a; b; c;
/-commented \"node\"

another /*foo*/ \"node\" /-1 /*bar*/ null;
final;";
        let mut doc: KdlDocument = src.parse()?;

        assert_eq!(doc.leading, Some("".into()));
        assert_eq!(doc.get_arg("foo"), Some(&1.into()));
        assert_eq!(
            doc.get_dash_vals("foo"),
            vec![&1.into(), &2.into(), &"three".into()]
        );

        let foo = doc.get("foo").expect("expected a foo node");
        assert_eq!(foo.leading, Some("\n// This is the first node\n".into()));
        assert_eq!(&foo[2], &"three".into());
        assert_eq!(&foo["bar"], &"baz".into());
        assert_eq!(
            foo.children().unwrap().get_arg("something"),
            Some(&"else".into())
        );
        assert_eq!(doc.get_arg("another"), Some(&"node".into()));

        let null = doc.get("null_id").expect("expected a null_id node");
        assert_eq!(&null["null_prop"], &KdlValue::Null);

        let tru = doc.get("true_id").expect("expected a true_id node");
        assert_eq!(&tru["true_prop"], &KdlValue::Null);

        let plusfalse = doc.get("+false").expect("expected a +false node");
        assert_eq!(&plusfalse[0], &KdlValue::Bool(true));

        let bar = doc.get("bar").expect("expected a bar node");
        assert_eq!(
            format!("{}", bar),
            "\n         bar \"indented\" // trailing whitespace after this\t\n"
        );

        let a = doc.get("a").expect("expected a node");
        assert_eq!(
            format!("{}", a),
            "/*\nSome random comment\n */\n\na; ".to_string()
        );

        let b = doc.get("b").expect("expected a node");
        assert_eq!(format!("{}", b), "b; ".to_string());

        // Round-tripping works.
        assert_eq!(format!("{}", doc), src);

        // Programmatic manipulation works.
        let mut node: KdlNode = "new\n".parse()?;
        // Manual entry parsing preserves formatting/reprs.
        node.push("\"blah\"=0xDEADbeef".parse::<KdlEntry>()?);
        doc.nodes_mut().push(node);

        assert_eq!(
            format!("{}", doc),
            format!("{}new \"blah\"=0xDEADbeef\n", src)
        );

        Ok(())
    }

    #[test]
    fn construction() {
        let mut doc = KdlDocument::new();
        doc.nodes_mut().push(KdlNode::new("foo"));

        let mut bar = KdlNode::new("bar");
        bar.insert("prop", "value");
        bar.push(1);
        bar.push(2);
        bar.push(false);
        bar.push(KdlValue::Null);

        let subdoc = bar.ensure_children();
        subdoc.nodes_mut().push(KdlNode::new("barchild"));
        doc.nodes_mut().push(bar);
        doc.nodes_mut().push(KdlNode::new("baz"));

        assert_eq!(
            r#"foo
bar prop="value" 1 2 false null {
    barchild
}
baz
"#,
            format!("{}", doc)
        );
    }

    #[test]
    fn fmt() -> miette::Result<()> {
        let mut doc: KdlDocument = r#"

        /* x */ foo    1 "bar"=0xDEADbeef {
    child1     1  ;

 // child 2 comment

        child2 2 // comment

               child3    "

   string\t" \
{
       /*


       multiline*/
                                    inner1    \
                    r"value" \
                    ;

        inner2      \ //comment
        {
            inner3
        }
    }
               }

        // trailing comment here

        "#
        .parse()?;

        KdlDocument::fmt(&mut doc);

        print!("{}", doc);
        assert_eq!(
            doc.to_string(),
            r#"/* x */
foo 1 bar=0xdeadbeef {
    child1 1
    // child 2 comment
    child2 2 // comment
    child3 "\n\n   string\t" {
        /*


       multiline*/
        inner1 r"value"
        inner2 {
            inner3
        }
    }
}
// trailing comment here"#
        );
        Ok(())
    }

    #[test]
    fn parse_examples() -> miette::Result<()> {
        include_str!("../examples/kdl-schema.kdl").parse::<KdlDocument>()?;
        include_str!("../examples/Cargo.kdl").parse::<KdlDocument>()?;
        include_str!("../examples/ci.kdl").parse::<KdlDocument>()?;
        include_str!("../examples/nuget.kdl").parse::<KdlDocument>()?;
        Ok(())
    }
}
