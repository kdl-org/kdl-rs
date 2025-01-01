use std::{
    num::{ParseFloatError, ParseIntError},
    sync::Arc,
};

use miette::{Severity, SourceSpan};

use num::CheckedMul;
use winnow::{
    ascii::{digit1, hex_digit1, oct_digit1, Caseless},
    combinator::{
        alt, cut_err, empty, eof, fail, not, opt, peek, preceded, repeat, repeat_till, separated,
        terminated, trace,
    },
    error::{AddContext, ErrMode, ErrorKind, FromExternalError, FromRecoverableError, ParserError},
    prelude::*,
    stream::{AsChar, Location, Recover, Recoverable, Stream},
    token::{any, none_of, one_of, take_while},
    Located,
};

use crate::{
    KdlDiagnostic, KdlDocument, KdlDocumentFormat, KdlEntry, KdlEntryFormat, KdlError,
    KdlIdentifier, KdlNode, KdlNodeFormat, KdlValue,
};

type Input<'a> = Recoverable<Located<&'a str>, KdlParseError>;
type PResult<T> = winnow::PResult<T, KdlParseError>;

pub(crate) fn try_parse<'a, P: Parser<Input<'a>, T, KdlParseError>, T>(
    mut parser: P,
    input: &'a str,
) -> Result<T, KdlError> {
    let (_, maybe_val, errs) = parser.recoverable_parse(Located::new(input));
    if let (Some(v), true) = (maybe_val, errs.is_empty()) {
        Ok(v)
    } else {
        Err(failure_from_errs(errs, input))
    }
}

pub(crate) fn failure_from_errs(errs: Vec<KdlParseError>, input: &str) -> KdlError {
    let src = Arc::new(String::from(input));
    KdlError {
        input: src.clone(),
        diagnostics: errs
            .into_iter()
            .map(|e| KdlDiagnostic {
                input: src.clone(),
                span: e.span.unwrap_or_else(|| (0usize..0usize).into()),
                message: e
                    .message
                    .or_else(|| e.label.clone().map(|l| format!("Expected {l}"))),
                label: e.label.map(|l| format!("not {l}")),
                help: e.help,
                severity: Severity::Error,
            })
            .collect(),
    }
}

#[derive(Debug, Clone, Default, Eq, PartialEq)]
struct KdlParseContext {
    message: Option<String>,
    label: Option<String>,
    help: Option<String>,
    severity: Option<Severity>,
}

impl KdlParseContext {
    fn msg(mut self, txt: impl AsRef<str>) -> Self {
        self.message = Some(txt.as_ref().to_string());
        self
    }

    fn lbl(mut self, txt: impl AsRef<str>) -> Self {
        self.label = Some(txt.as_ref().to_string());
        self
    }

    fn hlp(mut self, txt: impl AsRef<str>) -> Self {
        self.help = Some(txt.as_ref().to_string());
        self
    }

    // fn sev(mut self, severity: Severity) -> Self {
    //     self.severity = Some(severity);
    //     self
    // }
}

fn cx() -> KdlParseContext {
    Default::default()
}

#[derive(Debug, Default, Clone, Eq, PartialEq)]
pub(crate) struct KdlParseError {
    pub(crate) message: Option<String>,
    pub(crate) span: Option<SourceSpan>,
    pub(crate) label: Option<String>,
    pub(crate) help: Option<String>,
    pub(crate) severity: Option<Severity>,
}

impl<I: Stream> ParserError<I> for KdlParseError {
    fn from_error_kind(_input: &I, _kind: ErrorKind) -> Self {
        Self {
            message: None,
            span: None,
            label: None,
            help: None,
            severity: None,
        }
    }

    fn append(
        self,
        _input: &I,
        _token_start: &<I as Stream>::Checkpoint,
        _kind: ErrorKind,
    ) -> Self {
        self
    }
}

impl<I: Stream> AddContext<I, KdlParseContext> for KdlParseError {
    fn add_context(
        mut self,
        _input: &I,
        _token_start: &<I as Stream>::Checkpoint,
        ctx: KdlParseContext,
    ) -> Self {
        self.message = ctx.message.or(self.message);
        self.label = ctx.label.or(self.label);
        self.help = ctx.help.or(self.help);
        self.severity = ctx.severity.or(self.severity);
        self
    }
}

impl<'a> FromExternalError<Input<'a>, ParseIntError> for KdlParseError {
    fn from_external_error(_: &Input<'a>, _kind: ErrorKind, e: ParseIntError) -> Self {
        KdlParseError {
            span: None,
            message: Some(format!("{e}")),
            label: Some("invalid integer".into()),
            help: None,
            severity: Some(Severity::Error),
        }
    }
}

impl<'a> FromExternalError<Input<'a>, ParseFloatError> for KdlParseError {
    fn from_external_error(_input: &Input<'a>, _kind: ErrorKind, e: ParseFloatError) -> Self {
        KdlParseError {
            span: None,
            label: Some("invalid float".into()),
            help: None,
            message: Some(format!("{e}")),
            severity: Some(Severity::Error),
        }
    }
}

struct NegativeUnsignedError;

impl<'a> FromExternalError<Input<'a>, NegativeUnsignedError> for KdlParseError {
    fn from_external_error(
        _input: &Input<'a>,
        _kind: ErrorKind,
        _e: NegativeUnsignedError,
    ) -> Self {
        KdlParseError {
            span: None,
            message: Some("Tried to parse a negative number as an unsigned integer".into()),
            label: Some("negative unsigned int".into()),
            help: None,
            severity: Some(Severity::Error),
        }
    }
}

impl<I: Stream + Location> FromRecoverableError<I, Self> for KdlParseError {
    #[inline]
    fn from_recoverable_error(
        token_start: &<I as Stream>::Checkpoint,
        _err_start: &<I as Stream>::Checkpoint,
        input: &I,
        mut e: Self,
    ) -> Self {
        e.span = e
            .span
            .or_else(|| Some(span_from_checkpoint(input, token_start)));
        e
    }
}

fn span_from_checkpoint<I: Stream + Location>(
    input: &I,
    start: &<I as Stream>::Checkpoint,
) -> SourceSpan {
    let offset = input.offset_from(start);
    ((input.location() - offset)..input.location()).into()
}

// This is just like the standard .resume_after(), except we only resume on Cut errors.
fn resume_after_cut<Input, Output, Error, ParseNext, ParseRecover>(
    mut parser: ParseNext,
    mut recover: ParseRecover,
) -> impl Parser<Input, Option<Output>, Error>
where
    Input: Stream + Recover<Error>,
    Error: FromRecoverableError<Input, Error>,
    ParseNext: Parser<Input, Output, Error>,
    ParseRecover: Parser<Input, (), Error>,
{
    trace("resume_after_cut", move |input: &mut Input| {
        resume_after_cut_inner(&mut parser, &mut recover, input)
    })
}

fn resume_after_cut_inner<P, R, I, O, E>(
    parser: &mut P,
    recover: &mut R,
    i: &mut I,
) -> winnow::PResult<Option<O>, E>
where
    P: Parser<I, O, E>,
    R: Parser<I, (), E>,
    I: Stream,
    I: Recover<E>,
    E: FromRecoverableError<I, E>,
{
    let token_start = i.checkpoint();
    let mut err = match parser.parse_next(i) {
        Ok(o) => {
            return Ok(Some(o));
        }
        Err(ErrMode::Incomplete(e)) => return Err(ErrMode::Incomplete(e)),
        Err(ErrMode::Backtrack(e)) => return Err(ErrMode::Backtrack(e)),
        Err(err) => err,
    };
    let err_start = i.checkpoint();
    if recover.parse_next(i).is_ok() {
        if let Err(err_) = i.record_err(&token_start, &err_start, err) {
            err = err_;
        } else {
            return Ok(None);
        }
    }

    i.reset(&err_start);
    err = err.map(|err| E::from_recoverable_error(&token_start, &err_start, i, err));
    Err(err)
}

#[cfg(test)]
fn new_input(s: &str) -> Input<'_> {
    Recoverable::new(Located::new(s))
}

/// `document := bom? nodes`
pub(crate) fn document(input: &mut Input<'_>) -> PResult<KdlDocument> {
    let bom = opt(bom.take()).parse_next(input)?;
    let mut doc = nodes.parse_next(input)?;
    let badend = resume_after_cut(
        cut_err(eof).context(cx().lbl("EOF").msg("Expected end of document")),
        any.void(),
    )
    .parse_next(input)?
    .is_none();
    if badend {
        document.parse_next(input)?;
    }
    if let Some(bom) = bom {
        if let Some(fmt) = doc.format_mut() {
            fmt.leading = format!("{bom}{}", fmt.leading);
        }
    }
    Ok(doc)
}

/// `nodes := (line-space* node)* line-space*`
fn nodes(input: &mut Input<'_>) -> PResult<KdlDocument> {
    let leading = repeat(0.., alt((line_space.void(), (slashdash, base_node).void())))
        .map(|()| ())
        .take()
        .parse_next(input)?;
    let _start = input.checkpoint();
    let ns: Vec<KdlNode> = separated(
        0..,
        node,
        alt((node_terminator.void(), (eof.void(), any.void()).void())),
    )
    .parse_next(input)?;
    let _span = span_from_checkpoint(input, &_start);
    opt(node_terminator).parse_next(input)?;
    let trailing = repeat(0.., alt((line_space.void(), (slashdash, base_node).void())))
        .map(|()| ())
        .take()
        .parse_next(input)?;
    Ok(KdlDocument {
        nodes: ns,
        format: Some(KdlDocumentFormat {
            leading: leading.into(),
            trailing: trailing.into(),
        }),
        #[cfg(feature = "span")]
        span: _span,
    })
}

/// base-node := slashdash? type? node-space* string
///      (node-space+ slashdash? node-prop-or-arg)*
///      (node-space+ slashdash node-children)*
///      (node-space+ node-children)?
///      (node-space+ slashdash node-children)*
///      node-space*
/// node := base-node node-space* node-terminator
/// final-node := base-node node-space* node-terminator?
fn node(input: &mut Input<'_>) -> PResult<KdlNode> {
    let leading = repeat(0.., alt((line_space.void(), (slashdash, base_node).void())))
        .map(|()| ())
        .take()
        .parse_next(input)?;
    let mut nd = base_node.parse_next(input)?;
    if let Some(fmt) = nd.format_mut() {
        fmt.leading = leading.into();
    }
    Ok(nd)
}

fn base_node(input: &mut Input<'_>) -> PResult<KdlNode> {
    trace("children closing check", not(alt(("}".void(), eof.void())))).parse_next(input)?;
    let _start = input.checkpoint();
    let open_curly = resume_after_cut(
        cut_err(not("{").context(
            cx().msg("Found child block instead of node name")
                .lbl("node name")
                .hlp("Did you forget to add the node name itself? Or perhaps terminated the node before its child block?"))),
        "{".void(),
    )
    .parse_next(input)?;
    if open_curly.is_none() {
        // If we got a weird misplaced `{`, we consume the "child block" here,
        // because otherwise the error message is going to include the entire
        // child block as its span, but we only want to point to the offending
        // curly.
        input.reset(&_start);
        node_children.parse_next(input)?;
        opt(slashdashed_children).parse_next(input)?;
        peek(opt(node_terminator)).parse_next(input)?;
        // We also return a fake node here, for good measure.
        return Ok(KdlNode::new("<<BAD_NODE>>"));
    }
    let ty = opt(ty).parse_next(input)?;
    let after_ty = node_space0.take().parse_next(input)?;
    let _before_ident = input.checkpoint();
    let name = resume_after_cut(cut_err(identifier).context(
        cx().msg("Found invalid node name")
                                  .lbl("node name")
                                  .hlp("This can be any string type, including a quoted, raw, or multiline string, as well as a plain identifier string.")

    ), badval)
        .parse_next(input)?
        .unwrap_or_else(|| KdlIdentifier::from("/BAD_IDENT\\"));
    let name_is_valid = name.repr.as_ref().map(|s| s.is_empty()) != Some(true);
    // resume_after_cut() only picks up context from parsers passed into it. In
    // order to add an error that's more specific about us wanting a _node name_
    // here, we have to do some shenanigans with a "fake" parse here.
    // While this does result in double errors, I think it's still useful to get
    // _both_ the error message for a string/ident parser error _and_ the error
    // message for a node name being expected.
    if !name_is_valid {
        resume_after_cut(|input: &mut Input<'_>| -> PResult<()> {
                Err(ErrMode::Cut(KdlParseError {
                   span: Some(span_from_checkpoint(input, &_before_ident)),
                   ..Default::default()
                }))
            }.context(cx().msg("Found invalid node name")
                          .lbl("node name")
                          .hlp("This can be any string type, including a quoted, raw, or multiline string, as well as a plain identifier string.")),
        empty).parse_next(input)?;
    }
    let entries = repeat(
        0..,
        (peek(node_space1), node_entry).map(|(_, e): ((), _)| e),
    )
    .map(|e: Vec<Option<KdlEntry>>| e.into_iter().flatten().collect::<Vec<KdlEntry>>())
    .parse_next(input)?;
    let children = opt((
        before_node_children.take(),
        trace("node children", node_children),
    ))
    .parse_next(input)?;
    let (before_terminator, terminator) = if children.is_some() {
        (
            opt(slashdashed_children).take(),
            peek(opt(node_terminator).take()),
        )
            .parse_next(input)?
    } else {
        (
            before_node_children.take(),
            peek(opt(node_terminator).take()),
        )
            .parse_next(input)?
    };
    let (before_inner_ty, ty, after_inner_ty) = ty.unwrap_or_default();
    let (before_children, children) = children
        .map(|(before_children, children)| (before_children.into(), Some(children)))
        .unwrap_or(("".into(), None));
    Ok(KdlNode {
        ty,
        name,
        entries,
        children,
        format: Some(KdlNodeFormat {
            before_ty_name: before_inner_ty.into(),
            after_ty_name: after_inner_ty.into(),
            after_ty: after_ty.into(),
            before_children,
            before_terminator: before_terminator.into(),
            terminator: terminator.into(),
            ..Default::default()
        }),
        #[cfg(feature = "span")]
        span: span_from_checkpoint(input, &_start),
    })
}

#[cfg(test)]
#[test]
fn test_node() {
    assert_eq!(
        node.parse(new_input("foo")).unwrap(),
        KdlNode {
            ty: None,
            name: KdlIdentifier {
                value: "foo".into(),
                repr: Some("foo".into()),
                span: (0..3).into()
            },
            entries: vec![],
            children: None,
            format: Some(Default::default()),
            span: (0..7).into()
        }
    );

    assert_eq!(
        node.parse(new_input("foo bar")).unwrap(),
        KdlNode {
            ty: None,
            name: KdlIdentifier {
                value: "foo".into(),
                repr: Some("foo".into()),
                span: (0..3).into()
            },
            entries: vec![KdlEntry {
                ty: None,
                value: "bar".into(),
                name: None,
                format: Some(KdlEntryFormat {
                    value_repr: "bar".into(),
                    leading: " ".into(),
                    ..Default::default()
                }),
                span: SourceSpan::new(3.into(), 4)
            }],
            children: None,
            format: Some(KdlNodeFormat {
                ..Default::default()
            }),
            span: (0..8).into()
        }
    );
}

pub(crate) fn padded_node(input: &mut Input<'_>) -> PResult<KdlNode> {
    let ((mut node, _terminator, trailing), _span) = (
        node,
        opt(node_terminator),
        repeat(0.., alt((line_space, node_space)))
            .map(|_: ()| ())
            .take(),
    )
        .with_span()
        .parse_next(input)?;
    if let Some(fmt) = node.format_mut() {
        fmt.trailing = trailing.into();
    }
    #[cfg(feature = "span")]
    {
        node.span = _span.into();
    }
    Ok(node)
}

pub(crate) fn padded_node_entry(input: &mut Input<'_>) -> PResult<KdlEntry> {
    let ((leading, entry, trailing), _span) = (
        repeat(0.., line_space).map(|_: ()| ()).take(),
        trace("node entry", node_entry),
        repeat(0.., alt((line_space, node_space)))
            .map(|_: ()| ())
            .take(),
    )
        .with_span()
        .parse_next(input)?;
    if let Some(entry) = entry.map(|mut val| {
        if let Some(fmt) = val.format_mut() {
            fmt.leading = format!("{leading}{}", fmt.leading);
            fmt.trailing = format!("{}{trailing}", fmt.trailing);
        }
        #[cfg(feature = "span")]
        {
            val.span = _span.into();
        }
        val
    }) {
        Ok(entry)
    } else {
        fail.parse_next(input)?
    }
}

/// `node-prop-or-arg := prop | value`
/// `prop := string optional-node-space equals-sign optional-node-space value`
fn node_entry(input: &mut Input<'_>) -> PResult<Option<KdlEntry>> {
    let leading = (node_space0, opt((slashdashed_entries, node_space1)))
        .take()
        .parse_next(input)?;
    let _start = input.checkpoint();
    let maybe_ident = trace("prop name or string val", opt(identifier)).parse_next(input)?;
    let ident_was_parsed = maybe_ident.is_some();
    let after_key = if ident_was_parsed {
        opt((node_space0.take(), equals_sign))
            .parse_next(input)?
            .map(|(after_key, _)| after_key)
    } else {
        None
    };
    let entry = if let Some(after_key) = after_key {
        let (after_eq, value) = (
            node_space0.take(),
            cut_err(value.context(cx().lbl("property value"))),
        )
            .parse_next(input)?;
        value.map(|mut value| {
            value.name = maybe_ident;
            if let Some(fmt) = value.format_mut() {
                fmt.after_key = after_key.into();
                fmt.after_eq = after_eq.into();
            }
            #[cfg(feature = "span")]
            value
        })
    } else if let Some(ident) = maybe_ident {
        // It was ambiguous, but this ident is actually a value.
        Some(KdlEntry {
            format: Some(KdlEntryFormat {
                value_repr: ident.repr.unwrap_or_else(|| ident.value.clone()),
                ..Default::default()
            }),
            value: KdlValue::String(ident.value),
            name: None,
            ty: None,
            #[cfg(feature = "span")]
            span: (0..0).into(),
        })
    } else {
        trace("non-string value", resume_after_cut(value, badval))
            .parse_next(input)?
            .flatten()
    };
    Ok(entry.map(|mut value| {
        if let Some(fmt) = value.format_mut() {
            fmt.leading = leading.into();
        }
        #[cfg(feature = "span")]
        {
            value.span = span_from_checkpoint(input, &_start);
        }
        value
    }))
}

fn slashdashed_entries(input: &mut Input<'_>) -> PResult<()> {
    separated(1.., (slashdash, node_entry), node_space1)
        .map(|()| ())
        .take()
        .map(|x| x.to_string())
        .parse_next(input)?;
    Ok(())
}

#[cfg(test)]
#[test]
fn entry_test() {
    assert_eq!(
        node_entry.parse(new_input("foo=bar")).unwrap(),
        Some(KdlEntry {
            ty: None,
            value: KdlValue::String("bar".into()),
            name: Some("foo".parse().unwrap()),
            format: Some(KdlEntryFormat {
                value_repr: "bar".into(),
                ..Default::default()
            }),
            span: (0..7).into()
        })
    );

    assert_eq!(
        node_entry.parse(new_input("foo")).unwrap(),
        Some(KdlEntry {
            ty: None,
            value: KdlValue::String("foo".into()),
            name: None,
            format: Some(KdlEntryFormat {
                value_repr: "foo".into(),
                ..Default::default()
            }),
            span: (0..3).into()
        })
    );

    assert_eq!(
        node_entry.parse(new_input("/-foo bar")).unwrap(),
        Some(KdlEntry {
            ty: None,
            value: KdlValue::String("bar".into()),
            name: None,
            format: Some(KdlEntryFormat {
                value_repr: "bar".into(),
                leading: "/-foo ".into(),
                ..Default::default()
            }),
            span: (6..9).into()
        })
    );

    assert_eq!(
        node_entry.parse(new_input("/- foo=1 bar = 2")).unwrap(),
        Some(KdlEntry {
            ty: None,
            value: 2.into(),
            name: Some(KdlIdentifier {
                value: "bar".into(),
                repr: Some("bar".into()),
                span: (9..12).into(),
            }),
            format: Some(KdlEntryFormat {
                value_repr: "2".into(),
                leading: "/- foo=1 ".into(),
                after_key: " ".into(),
                after_eq: " ".into(),
                ..Default::default()
            }),
            span: (9..16).into()
        })
    );

    assert_eq!(
        node_entry.parse(new_input("/- \nfoo = 1 bar = 2")).unwrap(),
        Some(KdlEntry {
            ty: None,
            value: 2.into(),
            name: Some(KdlIdentifier {
                value: "bar".into(),
                repr: Some("bar".into()),
                span: (12..16).into(),
            }),
            format: Some(KdlEntryFormat {
                value_repr: "2".into(),
                leading: "/- \nfoo = 1 ".into(),
                after_key: " ".into(),
                after_eq: " ".into(),
                ..Default::default()
            }),
            span: (12..18).into()
        })
    );
}

fn before_node_children(input: &mut Input<'_>) -> PResult<()> {
    alt((
        (
            node_space1,
            slashdashed_entries,
            // This second one will fail if `node_entry_leading` is empty.
            node_space1,
            slashdashed_children,
        )
            .take(),
        (node_space1, slashdashed_entries).take(),
        (node_space1, slashdashed_children).take(),
        node_space0.take(),
    ))
    .void()
    .parse_next(input)?;
    node_space0.parse_next(input)?;
    Ok(())
}

#[cfg(test)]
#[test]
fn before_node_children_test() {
    assert!(before_node_children.parse(new_input(" /- { }")).is_ok());
    assert!(before_node_children.parse(new_input(" /- { bar }")).is_ok());
}

fn slashdashed_children(input: &mut Input<'_>) -> PResult<()> {
    node_space0.parse_next(input)?;
    trace(
        "slashdashed children",
        separated(
            1..,
            (slashdash.void(), node_children.void()).void(),
            node_space1,
        ),
    )
    .map(|()| ())
    .parse_next(input)
}

#[cfg(test)]
#[test]
fn around_children_test() {
    assert!(slashdashed_children.parse(new_input("/- { }")).is_ok());
    assert!(slashdashed_children.parse(new_input("/- { bar }")).is_ok());
}

/// `node-children := '{' nodes final-node? '}'`
fn node_children(input: &mut Input<'_>) -> PResult<KdlDocument> {
    let _before_open = input.checkpoint();
    let _before_open_loc = input.location();
    "{".parse_next(input)?;
    let _after_open_loc = input.location();
    let ns = trace("child nodes", nodes).parse_next(input)?;
    let _after_nodes = input.checkpoint();
    let _after_nodes_loc = input.location();
    let close_res: PResult<_> = cut_err("}")
        .context(cx().msg("No closing '}' for child block").lbl("closed"))
        .parse_next(input);
    if close_res.is_err() {
        return close_res
            .map(|_| KdlDocument::new())
            .or_else(|mut e: ErrMode<KdlParseError>| {
                e = match e {
                    ErrMode::Cut(mut pe) => {
                        #[cfg(feature = "span")]
                        {
                            pe.span = Some((_before_open_loc.._after_open_loc).into());
                        }
                        ErrMode::Cut(pe)
                    }
                    e => return Err(e),
                };
                input.record_err(&_before_open, &_before_open, e)?;
                if !ns.is_empty() {
                    input.record_err(
                        &_after_nodes,
                        &_after_nodes,
                        ErrMode::Cut(KdlParseError {
                            message: Some("Closing '}' was not found after nodes".into()),
                            span: Some((_after_open_loc.._after_nodes_loc).into()),
                            label: Some("closed".into()),
                            help: None,
                            severity: Some(Severity::Error),
                        }),
                    )?;
                }
                Ok(KdlDocument::new())
            });
    }
    Ok(ns)
}

/// `node-terminator := single-line-comment | newline | ';' | eof`
fn node_terminator(input: &mut Input<'_>) -> PResult<()> {
    trace(
        "node_terminator",
        alt((";".void(), newline, single_line_comment)),
    )
    .void()
    .parse_next(input)
}

/// `value := type? optional-node-space (string | number | keyword)`
fn value(input: &mut Input<'_>) -> PResult<Option<KdlEntry>> {
    let ((ty, (value, raw)), _span) = trace(
        "value",
        (
            opt((ty, node_space0.take())),
            alt((keyword.map(Some), number.map(Some), string)).with_taken(),
        ),
    )
    .with_span()
    .parse_next(input)?;
    let ((before_ty_name, ty, after_ty_name), after_ty) = ty.unwrap_or_default();
    Ok(value.map(|value| KdlEntry {
        ty,
        value,
        name: None,
        format: Some(KdlEntryFormat {
            value_repr: raw.into(),
            after_ty: after_ty.into(),
            before_ty_name: before_ty_name.into(),
            after_ty_name: after_ty_name.into(),
            ..Default::default()
        }),
        #[cfg(feature = "span")]
        span: _span.into(),
    }))
}

fn badval(input: &mut Input<'_>) -> PResult<()> {
    trace("badval", repeat_till(1.., any, peek(value_terminator)))
        .map(|((), _)| ())
        .parse_next(input)
}

fn value_terminator(input: &mut Input<'_>) -> PResult<()> {
    alt((
        eof.void(),
        "=".void(),
        ")".void(),
        "{".void(),
        "}".void(),
        node_space,
        node_terminator,
    ))
    .parse_next(input)
}

fn value_terminator_check(input: &mut Input<'_>) -> PResult<()> {
    trace("value terminator check", cut_err(peek(value_terminator).context(cx().hlp("A valid value was partially parsed, but was not followed by a value terminator. Did you want a space here?")))).parse_next(input)
}

/// `type := '(' optional-node-space string optional-node-space ')'`
fn ty<'s>(input: &mut Input<'s>) -> PResult<(&'s str, Option<KdlIdentifier>, &'s str)> {
    "(".parse_next(input)?;
    let (before_ty, ty, after_ty) = (
        node_space0.take(),
        resume_after_cut(
            cut_err(
                (identifier, peek(alt((node_space, ")".void())))).context(
                    cx().lbl("type name")
                        .msg("invalid contents inside type annotation"),
                ),
            ),
            repeat_till(1.., (not(badval_ty_char), any), peek(badval_ty_char)).map(|((), _)| ()),
        )
        .map(|opt| opt.map(|(i, _)| i)),
        node_space0.take(),
    )
        .parse_next(input)?;
    ")".parse_next(input)?;
    Ok((before_ty, ty, after_ty))
}

fn badval_ty_char(input: &mut Input<'_>) -> PResult<()> {
    alt((")".void(), "{".void(), node_space, node_terminator)).parse_next(input)
}

/// `line-space := newline | ws | single-line-comment`
fn line_space(input: &mut Input<'_>) -> PResult<()> {
    alt((node_space, newline, single_line_comment)).parse_next(input)
}

/// `node-space := ws* escline ws* | ws+`
fn node_space(input: &mut Input<'_>) -> PResult<()> {
    alt(((wss, escline, wss).void(), wsp)).parse_next(input)
}

fn node_space0(input: &mut Input<'_>) -> PResult<()> {
    repeat(0.., node_space).parse_next(input)
}

fn node_space1(input: &mut Input<'_>) -> PResult<()> {
    repeat(1.., node_space).parse_next(input)
}

/// string := identifier-string | quoted-string | raw-string ¶
pub(crate) fn string(input: &mut Input<'_>) -> PResult<Option<KdlValue>> {
    trace(
        "string",
        alt((
            resume_after_cut(
                (identifier_string, value_terminator_check).context(cx().lbl("identifier string")),
                badval,
            ),
            resume_after_cut(
                (raw_string, value_terminator_check).context(cx().lbl("raw string")),
                alt((raw_string_badval, badval)).void(),
            ),
            resume_after_cut(
                (quoted_string, value_terminator_check).context(cx().lbl("quoted string")),
                alt((quoted_string_badval, badval)).void(),
            ),
        )),
    )
    .map(|res| res.map(|(s, _)| s))
    .parse_next(input)
}

pub(crate) fn identifier(input: &mut Input<'_>) -> PResult<KdlIdentifier> {
    let mut bad_ident = false;
    let ((mut ident, raw), _span) = string
        .verify_map(|ident| {
            ident
                .or_else(|| {
                    // This is a sentinel we use later for better error messages
                    bad_ident = true;
                    Some(KdlValue::String("/BAD_IDENT\\".into()))
                })
                .and_then(|v| match v {
                    KdlValue::String(s) => Some(KdlIdentifier::from(s)),
                    _ => None,
                })
        })
        .with_taken()
        .with_span()
        .parse_next(input)?;
    ident.set_repr(if bad_ident { "" } else { raw });
    #[cfg(feature = "span")]
    {
        ident.set_span(_span);
    }
    Ok(ident)
}

/// `identifier-string := unambiguous-ident | signed-ident | dotted-ident`
fn identifier_string(input: &mut Input<'_>) -> PResult<KdlValue> {
    alt((unambiguous_ident, signed_ident, dotted_ident))
        .take()
        .map(|s| KdlValue::String(s.into()))
        .parse_next(input)
}

/// `unambiguous-ident := ((identifier-char - digit - sign - '.') identifier-char*) - 'true' - 'false' - 'null' - 'inf' - '-inf' - 'nan'`
fn unambiguous_ident(input: &mut Input<'_>) -> PResult<()> {
    not(alt((digit1.void(), alt(("-", "+")).void(), ".".void()))).parse_next(input)?;
    peek(identifier_char).parse_next(input)?;
    trace(
        "identifier chars",
        cut_err(
            repeat(1.., identifier_char)
                .verify_map(|s: String| {
                    if s == "true"
                        || s == "false"
                        || s == "null"
                        || s == "inf"
                        || s == "-inf"
                        || s == "nan"
                    {
                        None
                    } else {
                        Some(s)
                    }
                })
                .void(),
        ),
    )
    .parse_next(input)
}

/// `signed-ident := sign ((identifier-char - digit - '.') identifier-char*)?`
fn signed_ident(input: &mut Input<'_>) -> PResult<()> {
    alt(("+", "-")).parse_next(input)?;
    not(alt((digit1.void(), ".".void()))).parse_next(input)?;
    repeat(0.., identifier_char).parse_next(input)
}

/// `dotted-ident := sign? '.' ((identifier-char - digit) identifier-char*)?`
fn dotted_ident(input: &mut Input<'_>) -> PResult<()> {
    (
        opt(signum),
        ".",
        not(digit1),
        repeat(0.., identifier_char).map(|_: ()| ()),
    )
        .void()
        .parse_next(input)
}

static DISALLOWED_IDENT_CHARS: [char; 11] =
    ['\\', '/', '(', ')', '{', '}', '[', ']', ';', '"', '#'];

pub(crate) fn is_disallowed_ident_char(c: char) -> bool {
    DISALLOWED_IDENT_CHARS.iter().any(|ic| ic == &c)
        || NEWLINES.iter().copied().collect::<String>().contains(c)
        || UNICODE_SPACES.iter().any(|us| us == &c)
        || is_disallowed_unicode(c)
        || c == '='
}

/// `identifier-char := unicode - unicode-space - newline - [\\/(){};\[\]"#] - disallowed-literal-code-points - equals-sign`
fn identifier_char(input: &mut Input<'_>) -> PResult<char> {
    (
        not(alt((
            unicode_space,
            newline,
            disallowed_unicode,
            equals_sign,
        ))),
        none_of(DISALLOWED_IDENT_CHARS),
    )
        .map(|(_, c)| c)
        .parse_next(input)
}

/// `equals-sign := See Table ([Equals Sign](#equals-sign))`
fn equals_sign(input: &mut Input<'_>) -> PResult<()> {
    "=".void().parse_next(input)
}

/// ```text
/// quoted-string := '"' single-line-string-body '"' | '"""' newline multi-line-string-body newline (unicode-space | ('\' (unicode-space | newline)+)*) '"""'
/// single-line-string-body := (string-character - newline)*
/// multi-line-string-body := (('"' | '""')? string-character)*
/// ```
fn quoted_string(input: &mut Input<'_>) -> PResult<KdlValue> {
    let quotes =
        alt((
            (
                "\"\"\"",
                cut_err(newline).context(cx().lbl("multi-line string newline").msg(
                    "Multi-line string opening quotes must be immediately followed by a newline",
                )),
            )
                .take(),
            "\"",
        ))
        .parse_next(input)?;
    let is_multiline = quotes.len() > 1;
    let ml_prefix: Option<String> = if is_multiline {
        Some(
            cut_err(peek(preceded(
                repeat_till(
                    0..,
                    (
                        repeat(
                            0..,
                            (
                                not(newline),
                                alt((
                                    ws_escape.void(),
                                    trace(
                                        "valid string body char(s)",
                                        alt((
                                            ('\"', not("\"\"")).void(),
                                            ('\"', not("\"")).void(),
                                            string_char.void(),
                                        )),
                                    )
                                    .void(),
                                )),
                            ),
                        )
                        .map(|()| ()),
                        newline,
                    ),
                    peek(terminated(
                        repeat(0.., alt((ws_escape, unicode_space))).map(|()| ()),
                        "\"\"\"",
                    )),
                )
                .map(|((), ())| ()),
                terminated(
                    repeat(0.., alt((ws_escape.map(|_| ""), unicode_space.take())))
                        .map(|s: String| s),
                    "\"\"\"",
                ),
            )))
            .context(cx().lbl("multi-line string"))
            .parse_next(input)?,
        )
    } else {
        None
    };
    let body = if let Some(prefix) = ml_prefix {
        let parser = repeat_till(
            0..,
            (
                cut_err(alt(((&prefix[..]).void(), peek(empty_line).void())))
                    .context(cx().msg("matching multiline string prefix").lbl("bad prefix").hlp("Multi-line string bodies must be prefixed by the exact same whitespace as the leading whitespace before the closing '\"\"\"'")),
                alt((
                    empty_line.map(|s| s.to_string()),
                    repeat_till(
                        0..,
                        (
                            not(newline),
                            alt((
                                ws_escape.map(|_| None),
                                alt((
                                    ('\"', not("\"\"")).map(|(c, ())| Some(c)),
                                    ('\"', not("\"")).map(|(c, ())| Some(c)),
                                    string_char.map(Some),
                                ))
                            ))
                        ).map(|(_, c)| c),
                        newline,
                    )
                    // multiline string literal newlines are normalized to `\n`
                    .map(|(cs, _): (Vec<Option<char>>, _)| cs.into_iter().flatten().chain(vec!['\n']).collect::<String>()),
                )),
            )
                .map(|(_, s)| s),
            (
                &prefix[..],
                repeat(0.., ws_escape.void()).map(|()| ()),
                peek("\"\"\""),
            ),
        )
        .map(|(s, _): (Vec<String>, (_, _, _))| {
            let mut s = s.join("");
            // Slice off the `\n` at the end of the last line.
            s.truncate(s.len().saturating_sub(1));
            s
        })
        .context(cx().lbl("multi-line quoted string"));
        cut_err(parser).parse_next(input)?
    } else {
        let parser = repeat_till(
            0..,
            (
                cut_err(
                    not(newline).context(
                        cx().msg("Unexpected newline in single-line quoted string")
                            .hlp("You can make a string multi-line by wrapping it in '\"\"\"', with a newline immediately after the opening quotes."),
                    ),
                ),
                alt((
                    ws_escape.map(|_| None),
                    string_char.map(Some),
                ))
                ).map(|(_, c)| c),
            peek("\"")
        )
        .map(|(cs, _): (Vec<Option<char>>, _)| cs.into_iter().flatten().collect::<String>())
        .context(cx().lbl("quoted string"));
        cut_err(parser).parse_next(input)?
    };
    let closing_quotes = if is_multiline {
        "\"\"\"".context(cx().msg("missing multiline string closing quotes").hlp("Multiline strings must be closed by '\"\"\"' on a standalone line, only prefixed by whitespace."))
    } else {
        "\"".context(
            cx().msg("missing string closing quote")
                .hlp("Did you forget to escape something?"),
        )
    };
    cut_err(closing_quotes).parse_next(input)?;
    Ok(KdlValue::String(body))
}

fn empty_line(input: &mut Input<'_>) -> PResult<&'static str> {
    repeat(0.., alt((ws_escape.void(), unicode_space.void())))
        .map(|()| ())
        .parse_next(input)?;
    newline.parse_next(input)?;
    Ok("\n")
}

/// Like badval, but is able to slurp up invalid raw strings, which contain whitespace.
fn quoted_string_badval(input: &mut Input<'_>) -> PResult<()> {
    // TODO(@zkat): this should have different behavior based on whether we're
    // resuming a single or multi-line string. Right now, multi-liners end up
    // with silly errors.
    (
        repeat_till(
            0..,
            (not(quoted_string_terminator), any),
            quoted_string_terminator,
        ),
        quoted_string_terminator,
    )
        .map(|(((), _), _)| ())
        .parse_next(input)
}

fn quoted_string_terminator(input: &mut Input<'_>) -> PResult<()> {
    alt(("\"\"\"".void(), "\"".void(), peek(value_terminator))).parse_next(input)
}

/// ```text
/// string-character := '\' escape | [^\\"] - disallowed-literal-code-points
/// ```
fn string_char(input: &mut Input<'_>) -> PResult<char> {
    alt((
        trace("escaped char", escaped_char),
        trace(
            "regular string char",
            (not(disallowed_unicode), none_of(['\\', '"'])).map(|(_, c)| c),
        ),
    ))
    .parse_next(input)
}

fn ws_escape(input: &mut Input<'_>) -> PResult<()> {
    trace(
        "ws_escape",
        (
            "\\",
            repeat(1.., alt((unicode_space, newline))).map(|()| ()),
        ),
    )
    .void()
    .parse_next(input)
}

/// ```text
/// escape := ["\\bfnrts] | 'u{' hex-digit{1, 6} '}' | (unicode-space | newline)+
/// hex-digit := [0-9a-fA-F]
/// ```
fn escaped_char(input: &mut Input<'_>) -> PResult<char> {
    "\\".parse_next(input)?;
    alt((
        alt((
            "\\".value('\\'),
            "\"".value('\"'),
            "b".value('\u{0008}'),
            "f".value('\u{000C}'),
            "n".value('\n'),
            "r".value('\r'),
            "t".value('\t'),
            "s".value(' '),
        )),
        (
            "u{",
            cut_err(take_while(1..=6, AsChar::is_hex_digit)),
            cut_err("}"),
        )
            .context(cx().lbl("unicode escape char"))
            .verify_map(|(_, hx, _)| {
                let val = u32::from_str_radix(hx, 16)
                    .expect("Should have already been validated to be a hex string.");
                char::from_u32(val)
            }),
    ))
    .parse_next(input)
}

/// ```text
/// raw-string := '#' raw-string-quotes '#' | '#' raw-string '#'
/// raw-string-quotes := '"' single-line-raw-string-body '"' | '"""' newline multi-line-raw-string-body '"""'
/// single-line-raw-string-body := '' | (single-line-raw-string-char - '"') single-line-raw-string-char*? | '"' (single-line-raw-string-char - '"') single-line-raw-string-char*?
/// single-line-raw-string-char := unicode - newline - disallowed-literal-code-points
/// multi-line-raw-string-body := (unicode - disallowed-literal-code-points)*?
/// ```
fn raw_string(input: &mut Input<'_>) -> PResult<KdlValue> {
    let _start_loc = input.location();
    let hashes: String = repeat(1.., "#").parse_next(input)?;
    let quotes = alt((("\"\"\"", newline).take(), "\"")).parse_next(input)?;
    let is_multiline = quotes.len() > 1;
    let ml_prefix: Option<String> = if is_multiline {
        Some(
            peek(preceded(
                repeat_till(
                    0..,
                    (
                        repeat(
                            0..,
                            (
                                not(newline),
                                not(disallowed_unicode),
                                not(("\"\"\"", &hashes[..])),
                                any,
                            ),
                        )
                        .map(|()| ()),
                        newline,
                    ),
                    peek(terminated(
                        repeat(0.., unicode_space).map(|()| ()),
                        ("\"\"\"", &hashes[..]),
                    )),
                )
                .map(|((), ())| ()),
                terminated(
                    repeat(0.., unicode_space).map(|()| ()).take(),
                    ("\"\"\"", &hashes[..]),
                ),
            ))
            .parse_next(input)?
            .to_string(),
        )
    } else {
        None
    };
    let body = if let Some(prefix) = ml_prefix {
        repeat_till(
            0..,
            (
                cut_err(alt(((&prefix[..]).void(), peek(empty_line).void())))
                    .context(cx().lbl("matching multiline raw string prefix")),
                alt((
                    empty_line.map(|s| s.to_string()),
                    repeat_till(
                        0..,
                        (not(newline), not(("\"\"\"", &hashes[..])), any)
                            .map(|((), (), _)| ())
                            .take(),
                        newline,
                    )
                    // multiline string literal newlines are normalized to `\n`
                    .map(|(s, _): (Vec<&str>, _)| format!("{}\n", s.join(""))),
                )),
            )
                .map(|(_, s)| s),
            (
                &prefix[..],
                repeat(0.., unicode_space).map(|()| ()).take(),
                peek(("\"\"\"", &hashes[..])),
            ),
        )
        .map(|(s, _): (Vec<String>, (_, _, _))| {
            let mut s = s.join("");
            // Slice off the `\n` at the end of the last line.
            s.truncate(s.len().saturating_sub(1));
            s
        })
        .parse_next(input)?
    } else {
        repeat_till(
            0..,
            (
                not(disallowed_unicode),
                not(newline),
                not(("\"", &hashes[..])),
                any,
            )
                .map(|(_, _, _, s)| s),
            peek(("\"", &hashes[..])),
        )
        .map(|(s, _): (String, _)| s)
        .context(cx().lbl("raw string"))
        .parse_next(input)?
    };
    let closing_quotes = if is_multiline {
        "\"\"\"".context(cx().lbl("multiline raw string closing quotes"))
    } else {
        "\"".context(cx().lbl("raw string closing quotes"))
    };
    cut_err((closing_quotes, &hashes[..])).parse_next(input)?;
    if body == "\"" {
        Err(ErrMode::Cut(KdlParseError {
            message: Some("Single-line raw strings cannot look like multi-line ones".into()),
            span: Some((_start_loc..input.location()).into()),
            label: Some("triple quotes".into()),
            help: Some("Consider using a regular escaped string if all you want is a single quote: \"\\\"\"".into()),
            severity: Some(Severity::Error),
        }))
    } else {
        Ok(KdlValue::String(body))
    }
}

/// Like badval, but is able to slurp up invalid raw strings, which contain whitespace.
fn raw_string_badval(input: &mut Input<'_>) -> PResult<()> {
    repeat_till(
        0..,
        (not(alt(("#", "\""))), any),
        (alt(("#", "\"")), peek(alt((ws, newline, eof.void())))),
    )
    .map(|(v, _)| v)
    .parse_next(input)
}

#[cfg(test)]
mod string_tests {
    use super::*;

    #[test]
    fn identifier_string() {
        assert_eq!(
            string.parse(new_input("foo")).unwrap(),
            Some(KdlValue::String("foo".into()))
        );
        assert_eq!(
            string.parse(new_input(",")).unwrap(),
            Some(KdlValue::String(",".into()))
        );
    }

    #[test]
    fn single_line_quoted_string() {
        assert_eq!(
            string.parse(new_input("\"foo\"")).unwrap(),
            Some(KdlValue::String("foo".into()))
        );
        assert_eq!(
            string.parse(new_input("\"foo\\u{0a}\"")).unwrap(),
            Some(KdlValue::String("foo\u{0a}".into()))
        );
        assert_eq!(
            string.parse(new_input("\"\\u{10FFFF}\"")).unwrap(),
            Some(KdlValue::String("\u{10ffff}".into()))
        );
    }

    #[test]
    fn multiline_quoted_string() {
        assert_eq!(
            string
                .parse(new_input("\"\"\"\nfoo\nbar\nbaz\n\"\"\""))
                .unwrap(),
            Some(KdlValue::String("foo\nbar\nbaz".into()))
        );
        assert_eq!(
            string
                .parse(new_input("\"\"\"\n  foo\n    bar\n  baz\n  \"\"\""))
                .unwrap(),
            Some(KdlValue::String("foo\n  bar\nbaz".into()))
        );
        assert_eq!(
            string
                .parse(new_input("\"\"\"\nfoo\r\nbar\nbaz\n\"\"\""))
                .unwrap(),
            Some(KdlValue::String("foo\nbar\nbaz".into()))
        );
        assert_eq!(
            string
                .parse(new_input("\"\"\"\n  foo\n    bar\n   baz\n  \"\"\""))
                .unwrap(),
            Some(KdlValue::String("foo\n  bar\n baz".into()))
        );
        assert_eq!(
            string
                .parse(new_input(
                    "\"\"\"\n  \\     foo\n    \\  bar\n   \\ baz\n  \"\"\""
                ))
                .unwrap(),
            Some(KdlValue::String("foo\n  bar\n baz".into()))
        );
        assert_eq!(
            string
                .parse(new_input("\"\"\"\n\n    string\t\n    \"\"\""))
                .unwrap(),
            Some(KdlValue::String("\nstring\t".into())),
            "Empty line without any indentation"
        );
        assert_eq!(
            string
                .parse(new_input("\"\"\"\n 	 \\\n          \n 	 \"\"\""))
                .unwrap(),
            Some(KdlValue::String("".into())),
            "Escaped whitespace with proper prefix"
        );
        assert_eq!(
            string.parse(new_input("\"\"\"\n\\\"\"\"\n\"\"\"")).unwrap(),
            Some(KdlValue::String("\"\"\"".into()))
        );

        assert!(string
            .parse(new_input("\"\"\"\nfoo\n  bar\n  baz\n  \"\"\""))
            .is_err());
    }

    #[test]
    fn raw_string() {
        assert_eq!(
            string.parse(new_input("#\"foo\"#")).unwrap(),
            Some(KdlValue::String("foo".into()))
        );
        assert!(string.parse(new_input("#\"\"\"#")).is_err());
    }

    #[test]
    fn multiline_raw_string() {
        assert_eq!(
            string
                .parse(new_input("#\"\"\"\nfoo\nbar\nbaz\n\"\"\"#"))
                .unwrap(),
            Some(KdlValue::String("foo\nbar\nbaz".into()))
        );
        assert_eq!(
            string
                .parse(new_input("#\"\"\"\nfoo\r\nbar\nbaz\n\"\"\"#"))
                .unwrap(),
            Some(KdlValue::String("foo\nbar\nbaz".into()))
        );
        assert_eq!(
            string
                .parse(new_input("##\"\"\"\n  foo\n    bar\n  baz\n  \"\"\"##"))
                .unwrap(),
            Some(KdlValue::String("foo\n  bar\nbaz".into()))
        );
        assert_eq!(
            string
                .parse(new_input("#\"\"\"\n  foo\n    \\nbar\n   baz\n  \"\"\"#"))
                .unwrap(),
            Some(KdlValue::String("foo\n  \\nbar\n baz".into()))
        );
        assert!(string
            .parse(new_input("#\"\"\"\nfoo\n  bar\n  baz\n  \"\"\"#"))
            .is_err());

        assert!(string.parse(new_input("#\"\nfoo\nbar\nbaz\n\"#")).is_err());
        assert!(string.parse(new_input("\"\nfoo\nbar\nbaz\n\"")).is_err());
    }

    #[test]
    fn ident() {
        assert_eq!(
            identifier.parse(new_input("foo")).unwrap(),
            KdlIdentifier {
                value: "foo".into(),
                repr: Some("foo".into()),
                span: (0..3).into()
            }
        );
        assert_eq!(
            identifier.parse(new_input("+.")).unwrap(),
            KdlIdentifier {
                value: "+.".into(),
                repr: Some("+.".into()),
                span: (0..1).into()
            }
        )
    }
}

/// ```text
/// keyword := '#true' | '#false' | '#null'
/// keyword-number := '#inf' | '#-inf' | '#nan'
/// ````
fn keyword(input: &mut Input<'_>) -> PResult<KdlValue> {
    let _ = "#".parse_next(input)?;
    not(one_of(['#', '"'])).parse_next(input)?;
    cut_err(alt((
        "true".value(KdlValue::Bool(true)),
        "false".value(KdlValue::Bool(false)),
        "null".value(KdlValue::Null),
        "nan".value(KdlValue::Float(f64::NAN)),
        "inf".value(KdlValue::Float(f64::INFINITY)),
        "-inf".value(KdlValue::Float(f64::NEG_INFINITY)),
    )))
    .context(cx().lbl("keyword").hlp(
        "Available keywords in KDL are '#true', '#false', '#null', '#nan', '#inf', and '#-inf'; they are case-sensitive.",
    ))
    .parse_next(input)
}

/// `bom := '\u{FEFF}'`
fn bom(input: &mut Input<'_>) -> PResult<()> {
    "\u{FEFF}".void().parse_next(input)
}

pub(crate) fn is_disallowed_unicode(c: char) -> bool {
    matches!(c,
        '\u{0000}'..='\u{0008}'
        | '\u{000E}'..='\u{001F}'
        | '\u{200E}'..='\u{200F}'
        | '\u{202A}'..='\u{202E}'
        | '\u{2066}'..='\u{2069}'
        | '\u{FEFF}'
    )
}

/// `disallowed-literal-code-points := See Table (Disallowed Literal Code
/// Points)`
/// ```markdown
/// * The codepoints `U+0000-0008` or the codepoints `U+000E-001F`  (various
///   control characters).
/// * `U+007F` (the Delete control character).
/// * Any codepoint that is not a [Unicode Scalar
///   Value](https://unicode.org/glossary/#unicode_scalar_value) (`U+D800-DFFF`).
/// * `U+200E-200F`, `U+202A-202E`, and `U+2066-2069`, the [unicode
///   "direction control"
///   characters](https://www.w3.org/International/questions/qa-bidi-unicode-controls)
/// * `U+FEFF`, aka Zero-width Non-breaking Space (ZWNBSP)/Byte Order Mark (BOM),
///   except as the first code point in a document.
/// ```
fn disallowed_unicode(input: &mut Input<'_>) -> PResult<()> {
    take_while(1.., is_disallowed_unicode)
        .void()
        .parse_next(input)
}

/// `escline := '\\' ws* (single-line-comment | newline | eof)`
fn escline(input: &mut Input<'_>) -> PResult<()> {
    "\\".parse_next(input)?;
    wss.parse_next(input)?;
    alt((single_line_comment, newline, eof.void())).parse_next(input)?;
    wss.parse_next(input)
}

#[cfg(test)]
#[test]
fn escline_test() {
    let node = node.parse(new_input("foo bar\\\n   baz")).unwrap();
    assert_eq!(node.entries().len(), 2);
}

pub(crate) static NEWLINES: [&str; 8] = [
    "\u{000D}\u{000A}",
    "\u{000D}",
    "\u{000A}",
    "\u{0085}",
    "\u{000B}",
    "\u{000C}",
    "\u{2028}",
    "\u{2029}",
];

/// `newline := <See Table>`
fn newline(input: &mut Input<'_>) -> PResult<()> {
    alt(NEWLINES)
        .void()
        .context(cx().lbl("newline"))
        .parse_next(input)
}

fn wss(input: &mut Input<'_>) -> PResult<()> {
    repeat(0.., ws).parse_next(input)
}

fn wsp(input: &mut Input<'_>) -> PResult<()> {
    repeat(1.., ws).parse_next(input)
}

/// `ws := unicode-space | multi-line-comment``
fn ws(input: &mut Input<'_>) -> PResult<()> {
    alt((unicode_space, multi_line_comment)).parse_next(input)
}

static UNICODE_SPACES: [char; 18] = [
    '\u{0009}', '\u{0020}', '\u{00A0}', '\u{1680}', '\u{2000}', '\u{2001}', '\u{2002}', '\u{2003}',
    '\u{2004}', '\u{2005}', '\u{2006}', '\u{2007}', '\u{2008}', '\u{2009}', '\u{200A}', '\u{202F}',
    '\u{205F}', '\u{3000}',
];

/// `unicode-space := <See Table>`
fn unicode_space(input: &mut Input<'_>) -> PResult<()> {
    one_of(UNICODE_SPACES).void().parse_next(input)
}

/// `single-line-comment := '//' ^newline* (newline | eof)`
fn single_line_comment(input: &mut Input<'_>) -> PResult<()> {
    "//".parse_next(input)?;
    repeat_till(
        0..,
        (not(alt((newline, eof.void()))), any),
        alt((newline, eof.void())),
    )
    .map(|(_, _): ((), _)| ())
    .parse_next(input)
}

/// `multi-line-comment := '/*' commented-block`
fn multi_line_comment(input: &mut Input<'_>) -> PResult<()> {
    "/*".parse_next(input)?;
    cut_err(commented_block)
        .context(cx().lbl("closing of multi-line comment"))
        .parse_next(input)
}

/// `commented-block := '*/' | (multi-line-comment | '*' | '/' | [^*/]+) commented-block`
fn commented_block(input: &mut Input<'_>) -> PResult<()> {
    alt((
        "*/".void(),
        preceded(
            alt((
                multi_line_comment,
                "*".void(),
                "/".void(),
                repeat(1.., none_of(['*', '/'])).map(|()| ()),
            )),
            commented_block,
        ),
    ))
    .parse_next(input)
}

#[cfg(test)]
#[test]
fn multi_line_comment_test() {
    assert!(multi_line_comment.parse(new_input("/* foo */")).is_ok());
    assert!(multi_line_comment.parse(new_input("/**/")).is_ok());
    assert!(multi_line_comment.parse(new_input("/*\nfoo\n*/")).is_ok());
    assert!(multi_line_comment.parse(new_input("/*\nfoo*/")).is_ok());
    assert!(multi_line_comment.parse(new_input("/*foo\n*/")).is_ok());
    assert!(multi_line_comment.parse(new_input("/* foo\n*/")).is_ok());
    assert!(multi_line_comment
        .parse(new_input("/* /*bar*/ foo\n*/"))
        .is_ok());
}

/// slashdash := '/-' (node-space | line-space)*
fn slashdash(input: &mut Input<'_>) -> PResult<()> {
    (
        "/-",
        repeat(0.., alt((node_space, line_space))).map(|()| ()),
    )
        .void()
        .parse_next(input)
}

#[cfg(test)]
#[test]
fn slashdash_tests() {
    assert!(document.parse(new_input("/- foo bar")).is_ok());
    assert!(document.parse(new_input("/- foo bar;")).is_ok());
    assert!(document.parse(new_input("/-n 1;")).is_ok());
    assert!(node.parse(new_input("/- foo\nbar baz")).is_ok());
    assert!(node_entry.parse(new_input("/-commented tada")).is_ok());
    assert!(node.parse(new_input("foo /- { }")).is_ok());
    assert!(node.parse(new_input("foo /- { bar }")).is_ok());
    assert!(node
        .parse(new_input("/- foo bar\nnode /-1 2 { x }"))
        .is_ok());
    assert!(node
        .parse(new_input("/- foo bar\nnode 2 /-3 { x }"))
        .is_ok());
    assert!(node
        .parse(new_input("/- foo bar\nnode /-1 2 /-3 { x }"))
        .is_ok());
}

/// `number := keyword-number | hex | octal | binary | decimal`
fn number(input: &mut Input<'_>) -> PResult<KdlValue> {
    alt((float_value, integer_value)).parse_next(input)
}

/// ```text
/// decimal := sign? integer ('.' integer)? exponent?
/// exponent := ('e' | 'E') sign? integer
/// ```
fn float_value(input: &mut Input<'_>) -> PResult<KdlValue> {
    float.map(KdlValue::Float).parse_next(input)
}

fn float<T: ParseFloat>(input: &mut Input<'_>) -> PResult<T> {
    (
        alt((
            (
                decimal::<i128>,
                opt(preceded(
                    '.',
                    cut_err(
                        udecimal::<i128>.context(
                            cx().msg("Non-digit character found after the '.' of a float"),
                        ),
                    ),
                )),
                Caseless("e"),
                opt(one_of(['-', '+'])),
                cut_err(udecimal::<i128>.context(
                    cx().msg("Non-digit character found in the exponent part of a float").hlp("Floats with exponent parts should look like '2.0e123', or '43.3E-4'."),
                )),
            )
                .take(),
            (
                decimal::<i128>,
                '.',
                cut_err(
                    udecimal::<i128>
                        .context(cx().msg("Non-digit character found after the '.' of a float")),
                ),
            )
                .take(),
        )),
        value_terminator_check,
    )
        .try_map(|(float_str, _)| T::parse_float(&str::replace(float_str, "_", "")))
        .context(cx().lbl("float"))
        .parse_next(input)
}

#[cfg(test)]
#[test]
fn float_test() {
    use winnow::token::take;

    assert_eq!(
        float_value.parse(new_input("12_34.56")).unwrap(),
        KdlValue::Float(1234.56)
    );
    assert_eq!(
        float_value.parse(new_input("1234_.56")).unwrap(),
        KdlValue::Float(1234.56)
    );
    assert_eq!(
        (float_value, take(1usize))
            .parse(new_input("1234.56 "))
            .unwrap(),
        (KdlValue::Float(1234.56), " ")
    );
    assert!(float_value.parse(new_input("_1234.56")).is_err());
    assert!(float_value.parse(new_input("1234a.56")).is_err());
    assert_eq!(
        value
            .parse(new_input("2.5"))
            .unwrap()
            .map(|x| x.value().clone()),
        Some(KdlValue::Float(2.5))
    );
}

fn integer_value(input: &mut Input<'_>) -> PResult<KdlValue> {
    alt((
        (hex, value_terminator_check).context(cx().lbl("hexadecimal number")),
        (octal, value_terminator_check).context(cx().lbl("octal number")),
        (binary, value_terminator_check).context(cx().lbl("binary number")),
        (decimal, value_terminator_check).context(cx().lbl("integer")),
    ))
    .map(|(val, _)| KdlValue::Integer(val))
    .parse_next(input)
}

/// Non-float decimal
fn decimal<T: FromStrRadix + MaybeNegatable>(input: &mut Input<'_>) -> PResult<T> {
    let positive = signum.parse_next(input)?;
    udecimal::<T>
        .try_map(|x| {
            if positive {
                Ok(x)
            } else {
                x.negated().ok_or(NegativeUnsignedError)
            }
        })
        .parse_next(input)
}

#[cfg(test)]
#[test]
fn decimal_test() {
    assert_eq!(decimal::<i128>.parse(new_input("12_34")).unwrap(), 1234);
    assert_eq!(decimal::<i128>.parse(new_input("1234_")).unwrap(), 1234);
    assert!(decimal::<i128>.parse(new_input("_1234")).is_err());
    assert!(decimal::<i128>.parse(new_input("1234a")).is_err());
}

/// `integer := digit (digit | '_')*`
fn udecimal<T: FromStrRadix>(input: &mut Input<'_>) -> PResult<T> {
    (
        digit1,
        repeat(
            0..,
            alt(("_", take_while(1.., AsChar::is_dec_digit).take())),
        ),
    )
        .try_map(|(l, r): (&str, Vec<&str>)| {
            T::from_str_radix(&format!("{l}{}", str::replace(&r.join(""), "_", "")), 10)
        })
        .parse_next(input)
}

/// `hex := sign? '0x' hex-digit (hex-digit | '_')*`
fn hex<T: FromStrRadix + MaybeNegatable>(input: &mut Input<'_>) -> PResult<T> {
    let positive = signum.parse_next(input)?;
    uhex::<T>
        .try_map(|x| {
            if positive {
                Ok(x)
            } else {
                x.negated().ok_or(NegativeUnsignedError)
            }
        })
        .parse_next(input)
}

fn uhex<T: FromStrRadix>(input: &mut Input<'_>) -> PResult<T> {
    alt(("0x", "0X")).parse_next(input)?;
    cut_err((
        hex_digit1,
        repeat(
            0..,
            alt(("_", take_while(1.., AsChar::is_hex_digit).take())),
        ),
    ))
    .try_map(|(l, r): (&str, Vec<&str>)| {
        T::from_str_radix(&format!("{l}{}", str::replace(&r.join(""), "_", "")), 16)
    })
    .context(cx().lbl("hexadecimal"))
    .parse_next(input)
}

#[cfg(test)]
#[test]
fn test_hex() {
    assert_eq!(
        hex::<i128>.parse(new_input("0xdead_beef123")).unwrap(),
        0xdeadbeef123
    );
    assert_eq!(
        hex::<i128>.parse(new_input("0xDeAd_BeEf123")).unwrap(),
        0xdeadbeef123
    );
    assert_eq!(
        hex::<i128>.parse(new_input("0xdeadbeef123_")).unwrap(),
        0xdeadbeef123
    );
    assert!(
        hex::<i128>
            .parse(new_input("0xABCDEF0123456789abcdef0123456789"))
            .is_err(),
        "i128 overflow"
    );
    assert!(hex::<i128>.parse(new_input("0x_deadbeef123")).is_err());

    assert!(hex::<i128>.parse(new_input("0xbeefg1")).is_err());
}

/// `octal := sign? '0o' [0-7] [0-7_]*`
fn octal<T: FromStrRadix + MaybeNegatable>(input: &mut Input<'_>) -> PResult<T> {
    let positive = signum.parse_next(input)?;
    uoctal::<T>
        .try_map(|x| {
            if positive {
                Ok(x)
            } else {
                x.negated().ok_or(NegativeUnsignedError)
            }
        })
        .parse_next(input)
}

fn uoctal<T: FromStrRadix>(input: &mut Input<'_>) -> PResult<T> {
    alt(("0o", "0O")).parse_next(input)?;
    cut_err((
        oct_digit1,
        repeat(
            0..,
            alt(("_", take_while(1.., AsChar::is_oct_digit).take())),
        ),
    ))
    .try_map(|(l, r): (&str, Vec<&str>)| {
        T::from_str_radix(&format!("{l}{}", str::replace(&r.join(""), "_", "")), 8)
    })
    .context(cx().lbl("octal"))
    .parse_next(input)
}

#[cfg(test)]
#[test]
fn test_octal() {
    assert_eq!(octal::<i128>.parse(new_input("0o12_34")).unwrap(), 0o1234);
    assert_eq!(octal::<i128>.parse(new_input("0o1234_")).unwrap(), 0o1234);
    assert!(octal::<i128>.parse(new_input("0o_12_34")).is_err());
    assert!(octal::<i128>.parse(new_input("0o89")).is_err());
}

/// `binary := sign? '0b' ('0' | '1') ('0' | '1' | '_')*`
fn binary<T: FromStrRadix + MaybeNegatable>(input: &mut Input<'_>) -> PResult<T> {
    let positive = signum.parse_next(input)?;
    ubinary::<T>
        .try_map(|x| {
            if positive {
                Ok(x)
            } else {
                x.negated().ok_or(NegativeUnsignedError)
            }
        })
        .parse_next(input)
}

fn ubinary<T: FromStrRadix>(input: &mut Input<'_>) -> PResult<T> {
    alt(("0b", "0B")).parse_next(input)?;
    cut_err(
        (alt(("0", "1")), repeat(0.., alt(("0", "1", "_")))).try_map(
            move |(x, xs): (&str, Vec<&str>)| {
                T::from_str_radix(&format!("{x}{}", str::replace(&xs.join(""), "_", "")), 2)
            },
        ),
    )
    .context(cx().lbl("binary"))
    .parse_next(input)
}

#[cfg(test)]
#[test]
fn test_binary() {
    use winnow::token::take;

    assert_eq!(binary::<i128>.parse(new_input("0b10_01")).unwrap(), 0b1001);
    assert_eq!(binary::<i128>.parse(new_input("0b1001_")).unwrap(), 0b1001);
    assert!(binary::<i128>.parse(new_input("0b_10_01")).is_err());
    assert_eq!(
        (binary::<i128>, take(4usize))
            .parse(new_input("0b12389"))
            .unwrap(),
        (1, "2389")
    );
    assert!(binary::<i128>.parse(new_input("123")).is_err());
}

fn signum(input: &mut Input<'_>) -> PResult<bool> {
    let sign = opt(alt(('+', '-'))).parse_next(input)?;
    let mult = if let Some(sign) = sign {
        sign == '+'
    } else {
        true
    };
    Ok(mult)
}

trait FromStrRadix {
    fn from_str_radix(s: &str, radix: u32) -> Result<Self, ParseIntError>
    where
        Self: Sized;
}

macro_rules! impl_from_str_radix {
    ($($t:ty),*) => {
        $(
            impl FromStrRadix for $t {
                fn from_str_radix(s: &str, radix: u32) -> Result<Self, ParseIntError> {
                    <$t>::from_str_radix(s, radix)
                }
            }
        )*
    };
}

impl_from_str_radix!(i8, i16, i32, i64, i128, isize, u8, u16, u32, u64, u128, usize);

trait MaybeNegatable: CheckedMul {
    fn negated(&self) -> Option<Self>;
}

macro_rules! impl_negatable_signed {
    ($($t:ty),*) => {
        $(
            impl MaybeNegatable for $t {
                fn negated(&self) -> Option<Self> {
                    Some(self * -1)
                }
            }
        )*
    };
}

macro_rules! impl_negatable_unsigned {
    ($($t:ty),*) => {
        $(
            impl MaybeNegatable for $t {
                fn negated(&self) -> Option<Self> {
                    None
                }
            }
        )*
    };
}

trait ParseFloat {
    fn parse_float(input: &str) -> Result<Self, ParseFloatError>
    where
        Self: Sized;
}

impl ParseFloat for f32 {
    fn parse_float(input: &str) -> Result<Self, ParseFloatError> {
        input.parse()
    }
}
impl ParseFloat for f64 {
    fn parse_float(input: &str) -> Result<Self, ParseFloatError> {
        input.parse()
    }
}

impl_negatable_signed!(i8, i16, i32, i64, i128, isize);
impl_negatable_unsigned!(u8, u16, u32, u64, u128, usize);

#[cfg(test)]
mod failure_tests {
    use miette::Severity;

    use crate::{KdlDiagnostic, KdlDocument, KdlError};
    use std::sync::Arc;

    #[test]
    fn bad_node_name_test() -> miette::Result<()> {
        let input = Arc::new("foo { bar; { baz; }; }".to_string());
        let res: Result<KdlDocument, KdlError> = input.parse();
        // super::_print_diagnostic(res);
        // return Ok(());
        assert_eq!(
            res,
            Err(mkfail(
                input.clone(),
                vec![
                    KdlDiagnostic {
                        input: input.clone(),
                        span: (11..12).into(),
                        message: Some("Found child block instead of node name".into()),
                        label: Some("not node name".into()),
                        help: Some("Did you forget to add the node name itself? Or perhaps terminated the node before its child block?".into()),
                        severity: Severity::Error
                    }
                ]
            ))
        );
        let input = Arc::new("no/de 1 {\n    1 2 foo\n    bad#\n}".to_string());
        let res: Result<KdlDocument, KdlError> = input.parse();
        // super::_print_diagnostic(res);
        // return Ok(());
        assert_eq!(
            res,
            Err(mkfail(
                input.clone(),
                vec![
                    KdlDiagnostic {
                        input: input.clone(),
                        span: (0..5).into(),
                        message: Some("Expected identifier string".into()),
                        label: Some("not identifier string".into()),
                        help: Some("A valid value was partially parsed, but was not followed by a value terminator. Did you want a space here?".into()),
                        severity: Severity::Error
                    },
                    KdlDiagnostic {
                        input: input.clone(),
                        span: (0..5).into(),
                        message: Some("Found invalid node name".into()),
                        label: Some("not node name".into()),
                        help: Some("This can be any string type, including a quoted, raw, or multiline string, as well as a plain identifier string.".into()),
                        severity: Severity::Error
                    },
                    KdlDiagnostic {
                        input: input.clone(),
                        span: (14..15).into(),
                        message: Some("Found invalid node name".into()),
                        label: Some("not node name".into()),
                        help: Some("This can be any string type, including a quoted, raw, or multiline string, as well as a plain identifier string.".into()),
                        severity: Severity::Error
                    },
                    KdlDiagnostic {
                        input: input.clone(),
                        span: (26..30).into(),
                        message: Some("Expected identifier string".into()),
                        label: Some("not identifier string".into()),
                        help: Some("A valid value was partially parsed, but was not followed by a value terminator. Did you want a space here?".into()),
                        severity: Severity::Error
                    },
                    KdlDiagnostic {
                        input: input.clone(),
                        span: (26..30).into(),
                        message: Some("Found invalid node name".into()),
                        label: Some("not node name".into()),
                        help: Some("This can be any string type, including a quoted, raw, or multiline string, as well as a plain identifier string.".into()),
                        severity: Severity::Error
                    }
                ]
            ))
        );
        Ok(())
    }

    #[test]
    fn bad_entry_number_test() -> miette::Result<()> {
        let input = Arc::new("node 1asdf 2".to_string());
        let res: Result<KdlDocument, KdlError> = input.parse();
        // super::_print_diagnostic(res);
        // return Ok(());
        assert_eq!(
            res,
            Err(mkfail(
                input.clone(),
                vec![KdlDiagnostic {
                    input: input.clone(),
                    span: (5..10).into(),
                    message: Some("Expected integer".into()),
                    label: Some("not integer".into()),
                    severity: miette::Severity::Error,
                    help: Some("A valid value was partially parsed, but was not followed by a value terminator. Did you want a space here?".into()),
                }]
            ))
        );

        let input = Arc::new("node 0x1asdf 2".to_string());
        let res: Result<KdlDocument, KdlError> = input.parse();
        assert_eq!(
            res,
            Err(mkfail(
                input.clone(),
                vec![KdlDiagnostic {
                    input: input.clone(),
                    span: (5..12).into(),
                    message: Some("Expected hexadecimal number".into()),
                    label: Some("not hexadecimal number".into()),
                    severity: miette::Severity::Error,
                    help: Some("A valid value was partially parsed, but was not followed by a value terminator. Did you want a space here?".into()),
                }]
            ))
        );

        let input = Arc::new("node 0o1asdf 2".to_string());
        let res: Result<KdlDocument, KdlError> = input.parse();
        assert_eq!(
            res,
            Err(mkfail(
                input.clone(),
                vec![KdlDiagnostic {
                    input: input.clone(),
                    span: (5..12).into(),
                    message: Some("Expected octal number".into()),
                    label: Some("not octal number".into()),
                    severity: miette::Severity::Error,
                    help: Some("A valid value was partially parsed, but was not followed by a value terminator. Did you want a space here?".into()),
                }]
            ))
        );

        let input = Arc::new("node 0b1asdf 2".to_string());
        let res: Result<KdlDocument, KdlError> = input.parse();
        assert_eq!(
            res,
            Err(mkfail(
                input.clone(),
                vec![KdlDiagnostic {
                    input: input.clone(),
                    span: (5..12).into(),
                    message: Some("Expected binary number".into()),
                    label: Some("not binary number".into()),
                    severity: miette::Severity::Error,
                    help: Some("A valid value was partially parsed, but was not followed by a value terminator. Did you want a space here?".into()),
                }]
            ))
        );

        let input = Arc::new("node 1.0asdf 2".to_string());
        let res: Result<KdlDocument, KdlError> = input.parse();
        assert_eq!(
            res,
            Err(mkfail(
                input.clone(),
                vec![KdlDiagnostic {
                    input: input.clone(),
                    span: (5..12).into(),
                    message: Some("Expected float".into()),
                    label: Some("not float".into()),
                    severity: miette::Severity::Error,
                    help: Some("A valid value was partially parsed, but was not followed by a value terminator. Did you want a space here?".into()),
                }]
            ))
        );

        let input = Arc::new("node 1.asdf 2".to_string());
        let res: Result<KdlDocument, KdlError> = input.parse();
        assert_eq!(
            res,
            Err(mkfail(
                input.clone(),
                vec![KdlDiagnostic {
                    input: input.clone(),
                    span: (5..11).into(),
                    message: Some("Non-digit character found after the '.' of a float".into()),
                    label: Some("not float".into()),
                    severity: miette::Severity::Error,
                    help: None,
                }]
            ))
        );

        let input = Arc::new("node 1.0easdf 2".to_string());
        let res: Result<KdlDocument, KdlError> = input.parse();
        assert_eq!(
            res,
            Err(mkfail(
                input.clone(),
                vec![KdlDiagnostic {
                    input: input.clone(),
                    span: (5..13).into(),
                    message: Some(
                        "Non-digit character found in the exponent part of a float".into()
                    ),
                    label: Some("not float".into()),
                    severity: miette::Severity::Error,
                    help: Some(
                        "Floats with exponent parts should look like '2.0e123', or '43.3E-4'."
                            .into()
                    ),
                }]
            ))
        );
        Ok(())
    }

    #[test]
    fn bad_string_test() -> miette::Result<()> {
        let input = Arc::new("node \" 1".to_string());
        let res: Result<KdlDocument, KdlError> = input.parse();
        assert_eq!(
            res,
            Err(mkfail(
                input.clone(),
                vec![KdlDiagnostic {
                    input: input.clone(),
                    span: (5..8).into(),
                    severity: miette::Severity::Error,
                    message: Some("Expected quoted string".into()),
                    label: Some("not quoted string".into()),
                    help: None,
                }]
            ))
        );

        let input = Arc::new("node \"foo\"1".to_string());
        let res: Result<KdlDocument, KdlError> = input.parse();
        // if let Err(e) = res {
        //     println!("{:?}", miette::Report::from(e));
        // }
        assert_eq!(
            res,
            Err(mkfail(
                input.clone(),
                vec![KdlDiagnostic {
                    input: input.clone(),
                    span: (5..11).into(),
                    severity: miette::Severity::Error,
                    message: Some("Expected quoted string".into()),
                    label: Some("not quoted string".into()),
                    help: Some("A valid value was partially parsed, but was not followed by a value terminator. Did you want a space here?".into()),
                }]
            ))
        );

        let input = Arc::new("node \"\nlet's do multiline!\"".to_string());
        let res: Result<KdlDocument, KdlError> = input.parse();
        assert_eq!(
            res,
            Err(mkfail(
                input.clone(),
                vec![
                    KdlDiagnostic {
                        input: input.clone(),
                        span: (5..6).into(),
                        message: Some("Unexpected newline in single-line quoted string".into()),
                        label: Some("not quoted string".into()),
                        help: Some("You can make a string multi-line by wrapping it in '\"\"\"', with a newline immediately after the opening quotes.".into()),
                        severity: Severity::Error
                    },
                    KdlDiagnostic {
                        input: input.clone(),
                        span: (16..27).into(),
                        message: Some("Expected identifier string".into()),
                        label: Some("not identifier string".into()),
                        help: Some("A valid value was partially parsed, but was not followed by a value terminator. Did you want a space here?".into()),
                        severity: Severity::Error
                    }
                ]
            ))
        );
        Ok(())
    }

    #[test]
    fn bad_child_test() -> miette::Result<()> {
        let input = Arc::new("node {".to_string());
        let res: Result<KdlDocument, KdlError> = input.parse();
        // _print_diagnostic(res);
        // return Ok(());
        assert_eq!(
            res,
            Err(mkfail(
                input.clone(),
                vec![KdlDiagnostic {
                    input: input.clone(),
                    span: (5..6).into(),
                    severity: miette::Severity::Error,
                    message: Some("No closing '}' for child block".into()),
                    label: Some("not closed".into()),
                    help: None,
                }]
            ))
        );

        let input = Arc::new("node {}}".to_string());
        let res: Result<KdlDocument, KdlError> = input.parse();
        // _print_diagnostic(res);
        // return Ok(());
        // println!("{res:#?}");
        assert_eq!(
            res,
            Err(mkfail(
                input.clone(),
                vec![KdlDiagnostic {
                    input: input.clone(),
                    span: (7..8).into(),
                    message: Some("Expected end of document".into()),
                    label: Some("not EOF".into(),),
                    help: None,
                    severity: miette::Severity::Error,
                }]
            ))
        );

        let input = Arc::new("node }{".to_string());
        let res: Result<KdlDocument, KdlError> = input.parse();
        // _print_diagnostic(res);
        // return Ok(());
        assert_eq!(
            res,
            Err(mkfail(
                input.clone(),
                vec![
                    KdlDiagnostic {
                        input: input.clone(),
                        span: (5..6).into(),
                        message: Some(
                            "Expected end of document".into()
                        ),
                        label: Some(
                            "not EOF".into()
                        ),
                        help: None,
                        severity: Severity::Error,
                    },
                    KdlDiagnostic {
                        input: input.clone(),
                        span: (6..7).into(),
                        message: Some(
                            "Found child block instead of node name".into(),
                        ),
                        label: Some(
                            "not node name".into(),
                        ),
                        help: Some(
                            "Did you forget to add the node name itself? Or perhaps terminated the node before its child block?".into(),
                        ),
                        severity: Severity::Error,
                    },
                    KdlDiagnostic {
                        input: input.clone(),
                        span: (6..7).into(),
                        message: Some(
                            "No closing '}' for child block".into(),
                        ),
                        label: Some(
                            "not closed".into(),
                        ),
                        help: None,
                        severity: Severity::Error,
                    },
                ]
            ))
        );

        let input = Arc::new("node {\n".to_string());
        let res: Result<KdlDocument, KdlError> = input.parse();
        // _print_diagnostic(res);
        // return Ok(());
        assert_eq!(
            res,
            Err(mkfail(
                input.clone(),
                vec![
                    KdlDiagnostic {
                        input: input.clone(),
                        span: (5..6).into(),
                        message: Some("No closing '}' for child block".into()),
                        label: Some("not closed".into()),
                        help: None,
                        severity: Severity::Error,
                    },
                    KdlDiagnostic {
                        input: input.clone(),
                        span: (6..7).into(),
                        message: Some("Closing '}' was not found after nodes".into()),
                        label: Some("not closed".into()),
                        help: None,
                        severity: Severity::Error,
                    },
                ]
            ))
        );

        let input = Arc::new("node {\nnode2{{}}".to_string());
        let res: Result<KdlDocument, KdlError> = input.parse();
        // _print_diagnostic(res);
        // return Ok(());
        println!("{res:#?}");
        assert_eq!(
            res,
            Err(mkfail(
                input.clone(),
                vec![
                    KdlDiagnostic {
                        input: input.clone(),
                        span: (13..14).into(),
                        message: Some(
                            "Found child block instead of node name".into()
                        ),
                        label: Some(
                            "not node name".into()
                        ),
                        help: Some(
                            "Did you forget to add the node name itself? Or perhaps terminated the node before its child block?".into()
                        ),
                        severity: Severity::Error,
                    },
                    KdlDiagnostic {
                        input: input.clone(),
                        span: (5..6).into(),
                        message: Some(
                            "No closing '}' for child block".into(),
                        ),
                        label: Some(
                            "not closed".into(),
                        ),
                        help: None,
                        severity: Severity::Error,
                    },
                    KdlDiagnostic {
                        input: input.clone(),
                        span: (6..16).into(),
                        message: Some(
                            "Closing '}' was not found after nodes".into()
                        ),
                        label: Some(
                            "not closed".into()
                        ),
                        help: None,
                        severity: Severity::Error,
                    },
               ]
            ))
        );

        Ok(())
    }

    fn mkfail(input: Arc<String>, diagnostics: Vec<KdlDiagnostic>) -> KdlError {
        KdlError { input, diagnostics }
    }
}

#[cfg(test)]
fn _print_diagnostic<T>(res: Result<T, KdlError>) {
    if let Err(e) = res {
        println!("{:?}", miette::Report::from(e));
    }
}
