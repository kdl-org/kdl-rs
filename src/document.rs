#[cfg(feature = "span")]
use miette::SourceSpan;
use std::fmt::Display;

use crate::{KdlNode, KdlParseFailure, KdlValue};

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
#[derive(Debug, Clone, Eq)]
pub struct KdlDocument {
    pub(crate) nodes: Vec<KdlNode>,
    pub(crate) format: Option<KdlDocumentFormat>,
    #[cfg(feature = "span")]
    pub(crate) span: SourceSpan,
}

impl PartialEq for KdlDocument {
    fn eq(&self, other: &Self) -> bool {
        self.nodes == other.nodes && self.format == other.format
        // Intentionally omitted: self.span == other.span
    }
}

impl std::hash::Hash for KdlDocument {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.nodes.hash(state);
        self.format.hash(state);
        // Intentionally omitted: self.span.hash(state)
    }
}

impl Default for KdlDocument {
    fn default() -> Self {
        Self {
            nodes: Default::default(),
            format: Default::default(),
            #[cfg(feature = "span")]
            span: SourceSpan::from(0..0),
        }
    }
}

impl KdlDocument {
    /// Creates a new Document.
    pub fn new() -> Self {
        Default::default()
    }

    /// Gets this document's span.
    ///
    /// This value will be properly initialized when created via [`KdlDocument::parse`]
    /// but may become invalidated if the document is mutated. We do not currently
    /// guarantee this to yield any particularly consistent results at that point.
    #[cfg(feature = "span")]
    pub fn span(&self) -> SourceSpan {
        self.span
    }

    /// Sets this document's span.
    #[cfg(feature = "span")]
    pub fn set_span(&mut self, span: impl Into<SourceSpan>) {
        self.span = span.into();
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
    /// # let doc: KdlDocument = "foo 1\nbar #false".parse().unwrap();
    /// assert_eq!(doc.get_arg("foo"), Some(&1.into()));
    /// ```
    pub fn get_arg(&self, name: &str) -> Option<&KdlValue> {
        self.get(name).and_then(|node| node.get(0))
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
    /// bar #false
    /// ```
    ///
    /// You can fetch the arguments for `foo` in a single call like this:
    /// ```rust
    /// # use kdl::{KdlDocument, KdlValue};
    /// # let doc: KdlDocument = "foo 1 2 3\nbar #false".parse().unwrap();
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
        self.get_mut(name).and_then(|node| node.get_mut(0))
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
    ///   - #false
    /// }
    /// ```
    ///
    /// You can fetch the dashed child values of `foo` in a single call like this:
    /// ```rust
    /// # use kdl::{KdlDocument, KdlValue};
    /// # let doc: KdlDocument = "foo {\n - 1\n - 2\n - #false\n}".parse().unwrap();
    /// assert_eq!(doc.get_dash_args("foo"), vec![&1.into(), &2.into(), &false.into()]);
    /// ```
    pub fn get_dash_args(&self, name: &str) -> Vec<&KdlValue> {
        self.get(name)
            .and_then(|n| n.children())
            .map(|doc| doc.nodes())
            .unwrap_or_default()
            .iter()
            .filter(|e| e.name().value() == "-")
            .filter_map(|e| e.get(0))
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

    /// Gets the formatting details for this entry.
    pub fn format(&self) -> Option<&KdlDocumentFormat> {
        self.format.as_ref()
    }

    /// Gets a mutable reference to this entry's formatting details.
    pub fn format_mut(&mut self) -> Option<&mut KdlDocumentFormat> {
        self.format.as_mut()
    }

    /// Sets the formatting details for this entry.
    pub fn set_format(&mut self, format: KdlDocumentFormat) {
        self.format = Some(format);
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
    ///
    /// If you need to clear the `KdlNode`s, use [`Self::clear_fmt_recursive`].
    pub fn clear_format(&mut self) {
        self.format = None;
    }

    /// Clears leading and trailing text (whitespace, comments), also clearing
    /// all the `KdlNode`s in the document.
    pub fn clear_format_recursive(&mut self) {
        self.clear_format();
        for node in self.nodes.iter_mut() {
            node.clear_format_recursive();
        }
    }

    /// Auto-formats this Document, making everything nice while preserving
    /// comments.
    pub fn autoformat(&mut self) {
        self.autoformat_impl(0, false);
    }

    /// Formats the document and removes all comments from the document.
    pub fn autoformat_no_comments(&mut self) {
        self.autoformat_impl(0, true);
    }

    // TODO(@zkat): These should all be moved into the query module itself,
    // instead of being methods on the models
    //
    // /// Queries this Document's children according to the KQL query language,
    // /// returning an iterator over all matching nodes.
    // ///
    // /// # NOTE
    // ///
    // /// Any query selectors that try to select the toplevel `scope()` will
    // /// fail to match when using this method, since there's no [`KdlNode`] to
    // /// return in this case.
    // pub fn query_all(
    //     &self,
    //     query: impl IntoKdlQuery,
    // ) -> Result<KdlQueryIterator<'_>, KdlDiagnostic> {
    //     let parsed = query.into_query()?;
    //     Ok(KdlQueryIterator::new(None, Some(self), parsed))
    // }

    // /// Queries this Document's children according to the KQL query language,
    // /// returning the first match, if any.
    // ///
    // /// # NOTE
    // ///
    // /// Any query selectors that try to select the toplevel `scope()` will
    // /// fail to match when using this method, since there's no [`KdlNode`] to
    // /// return in this case.
    // pub fn query(&self, query: impl IntoKdlQuery) -> Result<Option<&KdlNode>, KdlDiagnostic> {
    //     let mut iter = self.query_all(query)?;
    //     Ok(iter.next())
    // }

    // /// Queries this Document's children according to the KQL query language,
    // /// picking the first match, and calling `.get(key)` on it, if the query
    // /// succeeded.
    // ///
    // /// # NOTE
    // ///
    // /// Any query selectors that try to select the toplevel `scope()` will
    // /// fail to match when using this method, since there's no [`KdlNode`] to
    // /// return in this case.
    // pub fn query_get(
    //     &self,
    //     query: impl IntoKdlQuery,
    //     key: impl Into<NodeKey>,
    // ) -> Result<Option<&KdlValue>, KdlDiagnostic> {
    //     Ok(self.query(query)?.and_then(|node| node.get(key)))
    // }

    // /// Queries this Document's children according to the KQL query language,
    // /// returning an iterator over all matching nodes, returning the requested
    // /// field from each of those nodes and filtering out nodes that don't have
    // /// it.
    // ///
    // /// # NOTE
    // ///
    // /// Any query selectors that try to select the toplevel `scope()` will
    // /// fail to match when using this method, since there's no [`KdlNode`] to
    // /// return in this case.
    // pub fn query_get_all(
    //     &self,
    //     query: impl IntoKdlQuery,
    //     key: impl Into<NodeKey>,
    // ) -> Result<impl Iterator<Item = &KdlValue>, KdlDiagnostic> {
    //     let key: NodeKey = key.into();
    //     Ok(self
    //         .query_all(query)?
    //         .filter_map(move |node| node.get(key.clone())))
    // }
}

impl std::str::FromStr for KdlDocument {
    type Err = KdlParseFailure;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        crate::v2_parser::try_parse(crate::v2_parser::document, s)
    }
}

impl Display for KdlDocument {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.stringify(f, 0)
    }
}

impl KdlDocument {
    pub(crate) fn autoformat_impl(&mut self, indent: usize, no_comments: bool) {
        if let Some(KdlDocumentFormat { leading, .. }) = self.format_mut() {
            crate::fmt::autoformat_leading(leading, indent, no_comments);
        }
        let mut has_nodes = false;
        for node in &mut self.nodes {
            has_nodes = true;
            node.autoformat_impl(indent, no_comments);
        }
        if let Some(KdlDocumentFormat { trailing, .. }) = self.format_mut() {
            crate::fmt::autoformat_trailing(trailing, no_comments);
            if !has_nodes {
                trailing.push('\n');
            }
        }
    }

    pub(crate) fn stringify(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        indent: usize,
    ) -> std::fmt::Result {
        if let Some(KdlDocumentFormat { leading, .. }) = self.format() {
            write!(f, "{}", leading)?;
        }
        for node in &self.nodes {
            node.stringify(f, indent)?;
        }
        if let Some(KdlDocumentFormat { trailing, .. }) = self.format() {
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

/// Formatting details for [`KdlDocument`]s.
#[derive(Debug, Clone, Default, Hash, Eq, PartialEq)]
pub struct KdlDocumentFormat {
    /// Whitespace and comments preceding the document's first node.
    pub leading: String,
    /// Whitespace and comments following the document's last node.
    pub trailing: String,
}

#[cfg(test)]
mod test {
    #[cfg(feature = "span")]
    use crate::KdlIdentifier;
    use crate::{KdlEntry, KdlValue};

    use super::*;

    #[test]
    fn canonical_clear_fmt() -> miette::Result<()> {
        let left_src = r#"
// There is a node here
first_node /*with cool comments, too */ param=1.03e2 /-"commented" "argument" {
    // With nested nodes too
    nested 1 2 3
    nested_2 "hi" "world" // this one is cool
}
second_node param=153 { nested one=1 two=2; }"#;
        let right_src = r#"
first_node param=103.0       "argument" {
        // Different indentation, because
        // Why not
        nested 1 2 3
        nested_2 "hi" /* actually, "hello" */ "world"
}
// There is a node here
second_node /* This time, the comment is here */ param=153 {
        nested one=1 two=2
}"#;
        let mut left_doc: KdlDocument = left_src.parse()?;
        let mut right_doc: KdlDocument = right_src.parse()?;
        assert_ne!(left_doc, right_doc);
        left_doc.clear_format_recursive();
        right_doc.clear_format_recursive();
        assert_eq!(left_doc, right_doc);
        Ok(())
    }

    #[test]
    fn basic_parsing() -> miette::Result<()> {
        let src = r#"
            // Hello, world!
            node 1
            node two
            node item="three";
            node {
                nested 1 2 3
                nested_2 hi "world"
            }
            (type)node ("type")what?
            +false #true
            null_id null_prop=#null
                    foo indented
            // normal comment?
            /- comment
            /* block comment */
            inline /*comment*/ here
            another /-comment there
            
            
            after some whitespace
            trailing /* multiline */
            trailing // single line
            "#;
        let _doc: KdlDocument = src.parse()?;
        Ok(())
    }

    #[test]
    fn parsing() -> miette::Result<()> {
        let src = "
// This is the first node
foo 1 2 three #null #true bar=\"baz\" {
    - 1
    - 2
    - three
    (mytype)something (\"name\")else\r
}

null_id null_prop=#null
true_id true_prop=#null
+false #true

         bar \"indented\" // trailing whitespace after this\t
/*
Some random comment
 */

a; b; c;
/-commented \"node\"

another /*foo*/ \"node\" /-1 /*bar*/ #null;
final;";
        let mut doc: KdlDocument = src.parse()?;

        assert_eq!(doc.get_arg("foo"), Some(&1.into()));
        assert_eq!(
            doc.get_dash_args("foo"),
            vec![&1.into(), &2.into(), &"three".into()]
        );
        assert_eq!(
            doc.format().map(|f| &f.leading[..]),
            Some("\n// This is the first node\n")
        );

        let foo = doc.get("foo").expect("expected a foo node");
        assert_eq!(foo.format().map(|f| &f.terminator[..]), Some("\n"));
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
            "/*\nSome random comment\n */\n\na;".to_string()
        );

        let b = doc.get("b").expect("expected a node");
        assert_eq!(format!("{}", b), " b;".to_string());

        // Round-tripping works.
        assert_eq!(format!("{}", doc), src);

        // Programmatic manipulation works.
        let mut node: KdlNode = "new\n".parse()?;
        // Manual entry parsing preserves formatting/reprs. Note that
        // if you're making KdlEntries this way, you need to inject
        // your own whitespace (or format the node)
        node.push(" \"blah\"=0xDEADbeef".parse::<KdlEntry>()?);
        dbg!(&node);
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

        doc.autoformat();

        assert_eq!(
            r#"foo
bar prop=value 1 2 #false #null {
    barchild
}
baz
"#,
            format!("{}", doc)
        );
    }

    #[ignore = "There's still issues around formatting comments and esclines."]
    #[test]
    fn autoformat() -> miette::Result<()> {
        let mut doc: KdlDocument = r##"

        /* x */ foo    1 "bar"=0xDEADbeef {
    child1     1  ;

 // child 2 comment

        child2 2 // comment

               child3    "

   string\t
   " \
{
       /*


       multiline*/
                                    inner1    \
                    #"value"# \
                    ;

        inner2      \ //comment
        {
            inner3
        }
    }
               }

        // trailing comment here

        "##
        .parse()?;

        KdlDocument::autoformat(&mut doc);

        assert_eq!(
            doc.to_string(),
            r#"/* x */
foo 1 bar=0xdeadbeef {
    child1 1
    // child 2 comment
    child2 2 // comment
    child3 "\nstring\t" {
        /*


       multiline*/
        inner1 value
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
    fn simple_autoformat() -> miette::Result<()> {
        let mut doc: KdlDocument = "a { b { c { }; }; }".parse().unwrap();
        KdlDocument::autoformat(&mut doc);
        assert_eq!(
            doc.to_string(),
            r#"a {
    b {
        c {

        }
    }
}
"#
        );
        Ok(())
    }

    #[cfg(feature = "span")]
    fn check_spans_for_doc(doc: &KdlDocument, source: &impl miette::SourceCode) {
        for node in doc.nodes() {
            check_spans_for_node(node, source);
        }
    }

    #[cfg(feature = "span")]
    fn check_spans_for_node(node: &KdlNode, source: &impl miette::SourceCode) {
        use crate::KdlEntryFormat;

        check_span_for_ident(node.name(), source);
        if let Some(ty) = node.ty() {
            check_span_for_ident(ty, source);
        }

        for entry in node.entries() {
            if let Some(name) = entry.name() {
                check_span_for_ident(name, source);
            }
            if let Some(ty) = entry.ty() {
                check_span_for_ident(ty, source);
            }
            if let Some(KdlEntryFormat { value_repr, .. }) = entry.format() {
                if entry.name().is_none() && entry.ty().is_none() {
                    check_span(value_repr, entry.span(), source);
                }
            }
        }
        if let Some(children) = node.children() {
            check_spans_for_doc(children, source);
        }
    }

    #[cfg(feature = "span")]
    #[track_caller]
    fn check_span_for_ident(ident: &KdlIdentifier, source: &impl miette::SourceCode) {
        if let Some(repr) = ident.repr() {
            check_span(repr, ident.span(), source);
        } else {
            check_span(ident.value(), ident.span(), source);
        }
    }

    #[cfg(feature = "span")]
    #[track_caller]
    fn check_span(expected: &str, span: SourceSpan, source: &impl miette::SourceCode) {
        let span = source.read_span(&span, 0, 0).unwrap();
        let span = std::str::from_utf8(span.data()).unwrap();
        assert_eq!(span, expected);
    }

    #[cfg(feature = "span")]
    #[test]
    fn span_test() -> miette::Result<()> {
        let input = r####"
this {
    is (a)"cool" document="to" read=(int)5 10.1 (u32)0x45
    and x="" {
        "it" /*shh*/ "has"="💯" ##"the"##
        Best🎊est
        "syntax ever"
    }
    "yknow?" 0x10
}
// that's
nice
inline { time; to; live "our" "dreams"; "y;all" }

"####;

        let doc: KdlDocument = input.parse()?;

        // First check that all the identity-spans are correct
        check_spans_for_doc(&doc, &input);

        // Now check some more interesting concrete spans

        // The whole document should be everything from the first node until the
        // last before_terminator whitespace.
        check_span(&input[1..(input.len() - 2)], doc.span(), &input);

        // This one-liner node should be the whole line without leading whitespace
        let is_node = doc
            .get("this")
            .unwrap()
            .children()
            .unwrap()
            .get("is")
            .unwrap();
        check_span(
            r##"is (a)"cool" document="to" read=(int)5 10.1 (u32)0x45"##,
            is_node.span(),
            &input,
        );

        // Some simple with/without type hints
        check_span(r#"(a)"cool""#, is_node.entry(0).unwrap().span(), &input);
        check_span(
            r#"read=(int)5"#,
            is_node.entry("read").unwrap().span(),
            &input,
        );
        check_span(r#"10.1"#, is_node.entry(1).unwrap().span(), &input);
        check_span(r#"(u32)0x45"#, is_node.entry(2).unwrap().span(), &input);

        // Now let's look at some messed up parts of that "and" node
        let and_node = doc
            .get("this")
            .unwrap()
            .children()
            .unwrap()
            .get("and")
            .unwrap();

        // The node is what you expect, the whole line and its two braces
        check_span(
            r####"and x="" {
        "it" /*shh*/ "has"="💯" ##"the"##
        Best🎊est
        "syntax ever"
    }"####,
            and_node.span(),
            &input,
        );

        // The child document is a little weird, it's the contents *inside* the braces
        // without the surrounding whitespace/comments. Just the actual contents.
        check_span(
            r####""it" /*shh*/ "has"="💯" ##"the"##
        Best🎊est
        "syntax ever""####,
            and_node.children().unwrap().span(),
            &input,
        );

        // Oh hey don't forget to check that "x" entry
        check_span(r#"x="""#, and_node.entry("x").unwrap().span(), &input);

        // Now the "it" node, more straightforward
        let it_node = and_node.children().unwrap().get("it").unwrap();
        check_span(
            r####""it" /*shh*/ "has"="💯" ##"the"##"####,
            it_node.span(),
            &input,
        );
        check_span(
            r#""has"="💯""#,
            it_node.entry("has").unwrap().span(),
            &input,
        );
        check_span(
            r####"##"the"##"####,
            it_node.entry(0).unwrap().span(),
            &input,
        );

        // Make sure inline nodes work ok
        let inline_node = doc.get("inline").unwrap();
        check_span(
            r#"inline { time; to; live "our" "dreams"; "y;all" }"#,
            inline_node.span(),
            &input,
        );

        let inline_children = inline_node.children().unwrap();
        check_span(
            r#"time; to; live "our" "dreams"; "y;all" "#,
            inline_children.span(),
            &input,
        );

        let inline_nodes = inline_children.nodes();
        check_span("time", inline_nodes[0].span(), &input);
        check_span("to", inline_nodes[1].span(), &input);
        check_span(r#"live "our" "dreams""#, inline_nodes[2].span(), &input);
        check_span(r#""y;all""#, inline_nodes[3].span(), &input);

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
