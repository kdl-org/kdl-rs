use std::{
    num::{ParseFloatError, ParseIntError},
    sync::Arc,
};

use miette::{Severity, SourceSpan};

use winnow::{
    ascii::{digit1, hex_digit1, oct_digit1, Caseless},
    combinator::{alt, cut_err, eof, not, opt, peek, preceded, repeat, repeat_till},
    error::{
        AddContext, ContextError, ErrorKind, FromExternalError, FromRecoverableError, ParserError,
        StrContext, StrContextValue,
    },
    prelude::*,
    stream::{AsChar, Location, Recoverable, Stream},
    token::{any, none_of, one_of, take_until, take_while},
    Located,
};

use crate::{
    KdlDiagnostic, KdlDocument, KdlDocumentFormat, KdlEntry, KdlEntryFormat, KdlErrorKind,
    KdlIdentifier, KdlNode, KdlNodeFormat, KdlParseFailure, KdlValue,
};

type Input<'a> = Recoverable<Located<&'a str>, KdlParseError>;
type PResult<T> = winnow::PResult<T, KdlParseError>;

impl std::str::FromStr for KdlDocument {
    type Err = KdlParseFailure;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let (maybe_val, errs) = try_parse(document, s);
        if let (Some(v), true) = (maybe_val, errs.is_empty()) {
            Ok(v)
        } else {
            Err(failure_from_errs(errs, s))
        }
    }
}

pub(crate) fn try_parse<'a, P: Parser<Input<'a>, T, KdlParseError>, T>(
    mut parser: P,
    input: &'a str,
) -> (Option<T>, Vec<KdlParseError>) {
    let (_, maybe_val, errs) = parser.recoverable_parse(Located::new(input));
    (maybe_val, errs)
}

pub(crate) fn failure_from_errs(errs: Vec<KdlParseError>, input: &str) -> KdlParseFailure {
    let src = Arc::new(String::from(input));
    KdlParseFailure {
        input: src.clone(),
        diagnostics: errs
            .into_iter()
            .map(|e| KdlDiagnostic {
                input: src.clone(),
                span: e.span.unwrap_or_else(|| (0usize..0usize).into()),
                label: e.label,
                help: e.help,
                severity: Severity::Error,
                kind: if let Some(ctx) = e.context {
                    KdlErrorKind::Context(ctx)
                } else {
                    KdlErrorKind::Other
                },
            })
            .collect(),
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub(crate) struct KdlParseError {
    pub(crate) context: Option<&'static str>,
    pub(crate) span: Option<SourceSpan>,
    pub(crate) label: Option<&'static str>,
    pub(crate) help: Option<&'static str>,
    pub(crate) kind: Option<KdlErrorKind>,
}

impl<I: Stream> ParserError<I> for KdlParseError {
    fn from_error_kind(_input: &I, _kind: ErrorKind) -> Self {
        Self {
            span: None,
            label: None,
            help: None,
            context: None,
            kind: None,
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

impl<I: Stream> AddContext<I> for KdlParseError {
    fn add_context(
        mut self,
        _input: &I,
        _token_start: &<I as Stream>::Checkpoint,
        ctx: &'static str,
    ) -> Self {
        self.context = self.context.or(Some(ctx));
        self
    }
}

impl<'a> FromExternalError<Input<'a>, ParseIntError> for KdlParseError {
    fn from_external_error(_: &Input<'a>, _kind: ErrorKind, e: ParseIntError) -> Self {
        KdlParseError {
            span: None,
            label: None,
            help: None,
            context: None,
            kind: Some(KdlErrorKind::ParseIntError(e)),
        }
    }
}

impl<'a> FromExternalError<Input<'a>, ParseFloatError> for KdlParseError {
    fn from_external_error(_input: &Input<'a>, _kind: ErrorKind, e: ParseFloatError) -> Self {
        KdlParseError {
            span: None,
            label: None,
            help: None,
            context: None,
            kind: Some(KdlErrorKind::ParseFloatError(e)),
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
        e.span = e.span.or_else(|| {
            Some((input.offset_from(token_start).saturating_sub(1)..input.location()).into())
        });
        e
    }
}

impl<I: Stream + Location> FromRecoverableError<I, ContextError> for KdlParseError {
    #[inline]
    fn from_recoverable_error(
        token_start: &<I as Stream>::Checkpoint,
        _err_start: &<I as Stream>::Checkpoint,
        input: &I,
        e: ContextError,
    ) -> Self {
        KdlParseError {
            span: Some((input.offset_from(token_start).saturating_sub(1)..input.location()).into()),
            label: None,
            help: None,
            context: e.context().next().and_then(|e| match e {
                StrContext::Label(l) => Some(*l),
                StrContext::Expected(StrContextValue::StringLiteral(s)) => Some(*s),
                StrContext::Expected(StrContextValue::Description(s)) => Some(*s),
                _ => None,
            }),
            kind: None,
        }
    }
}

/// Consumes the rest of a value we've cut_err on, so we can contine the parse.
// TODO: maybe use this for detecting invalid codepoints with useful errors?
fn badval<'s>(input: &mut Input<'s>) -> PResult<()> {
    repeat_till(
        0..,
        (
            not(alt((ws, node_terminator.void(), "{".void(), "}".void()))),
            any,
        ),
        alt((
            eof.void(),
            peek(alt((ws, node_terminator.void(), "{".void(), "}".void()))),
        )),
    )
    .map(|(_, _): ((), _)| ())
    .parse_next(input)
}

fn lbl(label: &'static str) -> &'static str {
    label
}

#[cfg(test)]
fn new_input<'a>(s: &'a str) -> Input<'a> {
    Recoverable::new(Located::new(s))
}

/// `document := bom? nodes`
fn document<'s>(input: &mut Input<'s>) -> PResult<KdlDocument> {
    let bom = opt(bom.take()).parse_next(input)?;
    let mut doc = nodes.parse_next(input)?;
    if let Some(bom) = bom {
        if let Some(fmt) = doc.format_mut() {
            fmt.leading = format!("{bom}{}", fmt.leading);
        }
    }
    Ok(doc)
}

/// `nodes := (line-space* node)* line-space*`
fn nodes<'s>(input: &mut Input<'s>) -> PResult<KdlDocument> {
    let ((leading, nodes, trailing), _span) = (
        repeat(0.., line_space).map(|()| ()).take(),
        repeat(0.., node),
        repeat(0.., line_space).map(|()| ()).take(),
    )
        .with_span()
        .parse_next(input)?;
    Ok(KdlDocument {
        nodes,
        format: Some(KdlDocumentFormat {
            leading: leading.into(),
            trailing: trailing.into(),
        }),
        #[cfg(feature = "span")]
        span: _span.into(),
    })
}

/// `base-node := type? optional-node-space string (required-node-space node-prop-or-arg)* (optional-node-space node-children)?`
fn base_node<'s>(input: &mut Input<'s>) -> PResult<KdlNode> {
    let ((ty, after_ty, name, entries, children), _span) = (
        opt(ty),
        optional_node_space.take(),
        identifier,
        repeat(
            0..,
            (peek(required_node_space), node_entry).map(|(_, e): ((), _)| e),
        )
        .map(|e: Vec<Option<KdlEntry>>| e.into_iter().filter_map(|e| e).collect::<Vec<KdlEntry>>()),
        opt((optional_node_space.take(), node_children)),
    )
        .with_span()
        .parse_next(input)?;
    let (before_inner_ty, ty, after_inner_ty) = if let Some(ty_info) = ty {
        ty_info
    } else {
        ("", None, "")
    };
    let (before_children, children) = children
        .map(|(before_children, children)| (before_children, Some(children)))
        .unwrap_or(("", None));
    Ok(KdlNode {
        ty,
        name,
        entries,
        children,
        format: Some(KdlNodeFormat {
            after_ty: after_ty.into(),
            before_ty_name: before_inner_ty.into(),
            after_ty_name: after_inner_ty.into(),
            before_children: before_children.into(),
            ..Default::default()
        }),
        #[cfg(feature = "span")]
        span: _span.into(),
    })
}

/// `node := base-node optional-node-space node-terminator`
fn node<'s>(input: &mut Input<'s>) -> PResult<KdlNode> {
    let ((leading, mut node, trailing, terminator), _span) = (
        repeat(0.., line_space).map(|()| ()).take(),
        base_node,
        optional_node_space.take(),
        node_terminator.take(),
    )
        .context("node")
        .with_span()
        .parse_next(input)?;
    if let Some(fmt) = node.format_mut() {
        fmt.leading = leading.into();
        fmt.trailing = format!("{trailing}{terminator}");
    }
    #[cfg(feature = "span")]
    {
        node.span = _span.into();
    }
    Ok(node)
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
            format: Some(KdlNodeFormat {
                after_ty: "".into(),
                before_ty_name: "".into(),
                after_ty_name: "".into(),
                before_children: "".into(),
                leading: "".into(),
                trailing: "".into()
            }),
            span: (0..7).into()
        }
    );

    assert_eq!(
        base_node.parse(new_input("foo bar")).unwrap(),
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
            span: (0..7).into()
        }
    );
}

pub(crate) fn padded_node<'s>(input: &mut Input<'s>) -> PResult<KdlNode> {
    let ((leading, mut node, trailing), _span) = (
        repeat(0.., alt((line_space, node_space)))
            .map(|_: ()| ())
            .take(),
        node,
        repeat(0.., alt((line_space, node_space)))
            .map(|_: ()| ())
            .take(),
    )
        .with_span()
        .parse_next(input)?;
    if let Some(fmt) = node.format_mut() {
        fmt.leading = format!("{leading}{}", fmt.leading);
        fmt.trailing = format!("{}{trailing}", fmt.trailing);
    }
    #[cfg(feature = "span")]
    {
        node.span = _span.into();
    }
    Ok(node)
}

/// `final-node := base-node optional-node-space node-terminator?`
fn final_node<'s>(input: &mut Input<'s>) -> PResult<KdlNode> {
    let node = base_node.parse_next(input)?;
    optional_node_space.parse_next(input)?;
    opt(node_terminator).parse_next(input)?;
    Ok(node)
}

pub(crate) fn padded_node_entry<'s>(input: &mut Input<'s>) -> PResult<Option<KdlEntry>> {
    let ((leading, entry, trailing), _span) = (
        repeat(0.., line_space).map(|_: ()| ()).take(),
        node_entry,
        repeat(0.., alt((line_space, node_space)))
            .map(|_: ()| ())
            .take(),
    )
        .with_span()
        .parse_next(input)?;
    Ok(entry.map(|mut val| {
        if let Some(fmt) = val.format_mut() {
            fmt.leading = format!("{leading}{}", fmt.leading);
            fmt.trailing = format!("{}{trailing}", fmt.trailing);
        }
        #[cfg(feature = "span")]
        {
            val.span = _span.into();
        }
        val
    }))
}

/// `node-prop-or-arg := prop | value`
fn node_entry<'s>(input: &mut Input<'s>) -> PResult<Option<KdlEntry>> {
    let ((leading, mut entry), _span) = (optional_node_space.take(), alt((prop, value)))
        .context(lbl("node entry"))
        .with_span()
        .parse_next(input)?;
    entry = entry.map(|mut e| {
        if let Some(fmt) = e.format_mut() {
            fmt.leading = leading.into();
        }
        #[cfg(feature = "span")]
        {
            e.set_span(_span);
        }
        e
    });
    Ok(entry)
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
                eq: "=".into(),
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
}

/// `node-children := '{' nodes final-node? '}'`
fn node_children<'s>(input: &mut Input<'s>) -> PResult<KdlDocument> {
    "{".parse_next(input)?;
    let _start = input.location();
    let mut ns = nodes.parse_next(input)?;
    let fin = opt(final_node).parse_next(input)?;
    if let Some(fin) = fin {
        ns.nodes.push(fin);
        #[cfg(feature = "span")]
        {
            ns.span = (_start..input.location()).into();
        }
    }
    cut_err("}").parse_next(input)?;
    Ok(ns)
}

/// `node-terminator := single-line-comment | newline | ';' | eof`
fn node_terminator<'s>(input: &mut Input<'s>) -> PResult<()> {
    alt((eof.void(), ";".void(), newline)).parse_next(input)
}

/// `prop := string optional-node-space equals-sign optional-node-space value`
fn prop<'s>(input: &mut Input<'s>) -> PResult<Option<KdlEntry>> {
    let ((key, after_key, eq, after_eq, value), _span) = (
        identifier,
        optional_node_space.take(),
        equals_sign.take(),
        optional_node_space.take(),
        cut_err(value).context("property value"),
    )
        .context("property")
        .with_span()
        .parse_next(input)?;
    Ok(value.map(|mut value| {
        value.name = Some(key);
        if let Some(fmt) = value.format_mut() {
            fmt.after_ty = after_key.into();
            fmt.eq = eq.into();
            fmt.after_eq = after_eq.into();
        }
        #[cfg(feature = "span")]
        {
            value.span = _span.into();
        }
        value
    }))
}

/// `value := type? optional-node-space (string | number | keyword)`
fn value<'s>(input: &mut Input<'s>) -> PResult<Option<KdlEntry>> {
    let ((ty, after_ty, (value, raw)), _span) = (
        opt(ty),
        optional_node_space.take(),
        alt((string, number.map(Some), keyword.map(Some)))
            .context(lbl("value"))
            .resume_after(badval)
            .with_taken(),
    )
        .with_span()
        .parse_next(input)?;
    let (before_ty_name, ty, after_ty_name) = if let Some(ty_info) = ty {
        ty_info
    } else {
        ("", None, "")
    };
    Ok(value.flatten().map(|value| KdlEntry {
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

/// `type := '(' optional-node-space string optional-node-space ')'`
fn ty<'s>(input: &mut Input<'s>) -> PResult<(&'s str, Option<KdlIdentifier>, &'s str)> {
    "(".parse_next(input)?;
    let (before_ty, ty, after_ty) = (
        optional_node_space.take(),
        cut_err(identifier.context(lbl("type name")))
            .resume_after((badval, peek(")").void(), badval).void()),
        optional_node_space.take(),
    )
        .parse_next(input)?;
    cut_err(")").parse_next(input)?;
    Ok((before_ty, ty, after_ty))
}

/// `plain-line-space := newline | ws | single-line-comment`
fn plain_line_space<'s>(input: &mut Input<'s>) -> PResult<()> {
    alt((newline, ws, single_line_comment)).parse_next(input)
}

/// `plain-node-space := ws* escline ws* | ws+`
fn plain_node_space<'s>(input: &mut Input<'s>) -> PResult<()> {
    alt(((wss, escline, wss).void(), wsp)).parse_next(input)
}

/// `line-space := plain-line-space+ | '/-' plain-node-space* node`
fn line_space<'s>(input: &mut Input<'s>) -> PResult<()> {
    alt((
        repeat(1.., plain_line_space).map(|_: ()| ()).void(),
        (
            "/-",
            repeat(0.., plain_node_space).map(|_: ()| ()),
            cut_err(node),
        )
            .void()
            .context(lbl("slashdashed node")),
    ))
    .parse_next(input)
}

/// `node-space := plain-node-space+ ('/-' plain-node-space* (node-prop-or-arg | node-children))?`
fn node_space<'s>(input: &mut Input<'s>) -> PResult<()> {
    repeat(1.., plain_node_space)
        .map(|_: ()| ())
        .parse_next(input)?;
    opt((
        "/-",
        repeat(0.., plain_node_space).map(|_: ()| ()),
        cut_err(alt((
            node_entry.void().context(lbl("slashdashed entry")),
            node_children.void().context(lbl("slashdashed children")),
        ))),
    ))
    .void()
    .parse_next(input)
}

/// `required-node-space := node-space* plain-node-space+`
fn required_node_space<'s>(input: &mut Input<'s>) -> PResult<()> {
    repeat(0.., (node_space, peek(plain_node_space)))
        .map(|_: ()| ())
        .parse_next(input)?;
    repeat(1.., plain_node_space).parse_next(input)
}

/// `optional-node-space := node-space*`
fn optional_node_space<'s>(input: &mut Input<'s>) -> PResult<()> {
    dbg!(&input);
    repeat(0.., node_space).parse_next(input)
}

/// `string := identifier-string | quoted-string | raw-string`
pub(crate) fn string<'s>(input: &mut Input<'s>) -> PResult<Option<KdlValue>> {
    alt((identifier_string, raw_string, quoted_string))
        .context("string")
        .parse_next(input)
}

pub(crate) fn identifier<'s>(input: &mut Input<'s>) -> PResult<KdlIdentifier> {
    let ((mut ident, raw), _span) = string
        .verify_map(|i| {
            i.and_then(|v| match v {
                KdlValue::String(s) => Some(KdlIdentifier::from(s)),
                _ => None,
            })
        })
        .with_taken()
        .with_span()
        .parse_next(input)?;
    ident.set_repr(raw);
    #[cfg(feature = "span")]
    {
        ident.set_span(_span);
    }
    Ok(ident)
}

/// `identifier-string := unambiguous-ident | signed-ident | dotted-ident`
fn identifier_string<'s>(input: &mut Input<'s>) -> PResult<Option<KdlValue>> {
    alt((unambiguous_ident, signed_ident, dotted_ident))
        .take()
        .map(|s| Some(KdlValue::String(s.into())))
        .parse_next(input)
}

/// `unambiguous-ident := ((identifier-char - digit - sign - '.') identifier-char*) - 'true' - 'false' - 'null' - 'inf' - '-inf' - 'nan'`
fn unambiguous_ident<'s>(input: &mut Input<'s>) -> PResult<()> {
    not(alt((digit1.void(), alt(("-", "+")).void(), ".".void()))).parse_next(input)?;
    repeat(1.., identifier_char)
        .verify_map(|s: String| {
            if s == "true" || s == "false" || s == "null" || s == "inf" || s == "-inf" || s == "nan"
            {
                None
            } else {
                Some(s)
            }
        })
        .void()
        .parse_next(input)
}

/// `signed-ident := sign ((identifier-char - digit - '.') identifier-char*)?`
fn signed_ident<'s>(input: &mut Input<'s>) -> PResult<()> {
    alt(("+", "-")).parse_next(input)?;
    not(alt((digit1.void(), ".".void()))).parse_next(input)?;
    repeat(0.., identifier_char).parse_next(input)
}

/// `dotted-ident := sign? '.' ((identifier-char - digit) identifier-char*)?`
fn dotted_ident<'s>(input: &mut Input<'s>) -> PResult<()> {
    (
        opt(sign),
        ".",
        not(digit1),
        repeat(1.., identifier_char).map(|_: ()| ()),
    )
        .void()
        .parse_next(input)
}

/// `identifier-char := unicode - unicode-space - newline - [\\/(){};\[\]"#] - disallowed-literal-code-points - equals-sign`
fn identifier_char<'s>(input: &mut Input<'s>) -> PResult<char> {
    (
        not(alt((
            unicode_space,
            newline,
            disallowed_unicode,
            equals_sign,
        ))),
        none_of(['\\', '/', '(', ')', '{', '}', '[', ']', ';', '"', '#']),
    )
        .map(|(_, c)| c)
        .parse_next(input)
}

/// `equals-sign := See Table ([Equals Sign](#equals-sign))`
fn equals_sign<'s>(input: &mut Input<'s>) -> PResult<()> {
    one_of(['=', '﹦', '＝', '🟰']).void().parse_next(input)
}

/// ```text
/// quoted-string := '"' (single-line-string-body | newline multi-line-string-body newline unicode-space*) '"'
/// single-line-string-body := (string-character - newline)*
/// multi-line-string-body := string-character*
/// ```
fn quoted_string<'s>(input: &mut Input<'s>) -> PResult<Option<KdlValue>> {
    "\"".parse_next(input)?;
    let is_multiline = opt(newline).parse_next(input)?.is_some();
    let ml_prefix: Option<String> = if is_multiline {
        Some(
            peek(repeat(0.., unicode_space).map(|_: ()| ()).take())
                .parse_next(input)?
                .to_string(),
        )
    } else {
        None
    };
    let body: Option<String> = if let Some(prefix) = ml_prefix {
        cut_err(repeat_till(
            0..,
            (&prefix[..], string_char, newline).map(|(_, s, _)| s),
            (&prefix[..], "\""),
        ))
        .map(|(s, _): (String, _)| s)
        .resume_after(quoted_string_badval)
        .parse_next(input)?
    } else {
        cut_err(repeat_till(
            0..,
            (not(newline), string_char).map(|(_, s)| s),
            "\"",
        ))
        .map(|(s, _): (String, _)| s)
        .resume_after(quoted_string_badval)
        .parse_next(input)?
    };
    Ok(body.map(|body| KdlValue::String(body)))
}

/// Like badval, but is able to slurp up invalid raw strings, which contain whitespace.
fn quoted_string_badval<'s>(input: &mut Input<'s>) -> PResult<()> {
    let terminator = ("\"", peek(alt((ws, newline, eof.void()))));
    let terminator2 = ("\"", peek(alt((ws, newline, eof.void()))));
    repeat_till(0.., (not(terminator), any), terminator2)
        .map(|(v, _)| v)
        .parse_next(input)
}
/// ```text
/// string-character := '\' escape | [^\\"] - disallowed-literal-code-points
/// ```
fn string_char<'s>(input: &mut Input<'s>) -> PResult<char> {
    alt((
        escaped_char,
        (not(disallowed_unicode), none_of(['\\', '"'])).map(|(_, c)| c),
    ))
    .parse_next(input)
}

/// ```text
/// escape := ["\\bfnrts] | 'u{' hex-digit{1, 6} '}' | (unicode-space | newline)+
/// hex-digit := [0-9a-fA-F]
/// ```
fn escaped_char<'s>(input: &mut Input<'s>) -> PResult<char> {
    alt((
        preceded(
            "\\",
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
        ),
        (
            "u{",
            cut_err(take_while(1..6, AsChar::is_hex_digit)),
            cut_err("}"),
        )
            .verify_map(|(_, hx, _)| {
                let val = u32::from_str_radix(hx, 16)
                    .expect("Should have already been validated to be a hex string.");
                char::from_u32(val)
            }),
        repeat(1.., alt((unicode_space, newline))).map(|_: ()| ' '),
    ))
    .parse_next(input)
}

/// `raw-string := '#' raw-string-quotes '#' | '#' raw-string '#'`
/// `raw-string-quotes := '"' (single-line-raw-string-body | newline multi-line-raw-string-body newline unicode-space*) '"'`
/// `single-line-raw-string-body := (unicode - newline - disallowed-literal-code-points)*`
/// `multi-line-raw-string-body := (unicode - disallowed-literal-code-points)`
fn raw_string<'s>(input: &mut Input<'s>) -> PResult<Option<KdlValue>> {
    let hashes: String = repeat(1.., "#").parse_next(input)?;
    "\"".parse_next(input)?;
    let is_multiline = opt(newline).parse_next(input)?.is_some();
    let ml_prefix: Option<String> = if is_multiline {
        Some(
            peek(repeat(0.., unicode_space).map(|_: ()| ()).take())
                .parse_next(input)?
                .to_string(),
        )
    } else {
        None
    };
    let body: Option<String> = if let Some(prefix) = ml_prefix {
        cut_err(repeat_till(
            0..,
            (
                &prefix[..],
                not(disallowed_unicode),
                not(("\"", &hashes[..])),
                any,
                newline,
            )
                .map(|(_, _, _, s, _)| s),
            (&prefix[..], "\"", &hashes[..]),
        ))
        .map(|(s, _): (String, _)| s)
        .resume_after(raw_string_badval)
        .parse_next(input)?
    } else {
        cut_err(repeat_till(
            0..,
            (
                not(disallowed_unicode),
                not(newline),
                not(("\"", &hashes[..])),
                any,
            )
                .map(|(_, _, _, s)| s),
            ("\"", &hashes[..]),
        ))
        .map(|(s, _): (String, _)| s)
        .resume_after(raw_string_badval)
        .parse_next(input)?
    };
    Ok(body.map(|body| KdlValue::String(body)))
}

/// Like badval, but is able to slurp up invalid raw strings, which contain whitespace.
fn raw_string_badval<'s>(input: &mut Input<'s>) -> PResult<()> {
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
    }

    #[test]
    fn quoted_string() {
        assert_eq!(
            string.parse(new_input("\"foo\"")).unwrap(),
            Some(KdlValue::String("foo".into()))
        );
    }

    #[test]
    fn raw_string() {
        assert_eq!(
            string.parse(new_input("#\"foo\"#")).unwrap(),
            Some(KdlValue::String("foo".into()))
        );
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
    }
}

/// ```text
/// keyword := '#true' | '#false' | '#null'
/// keyword-number := '#inf' | '#-inf' | '#nan'
/// ````
fn keyword<'s>(input: &mut Input<'s>) -> PResult<KdlValue> {
    let _ = "#".parse_next(input)?;
    let _ = not(one_of(['#', '"'])).parse_next(input)?;
    cut_err(alt((
        Caseless("true").value(KdlValue::Bool(true)),
        Caseless("false").value(KdlValue::Bool(false)),
        Caseless("null").value(KdlValue::Null),
        Caseless("nan").value(KdlValue::Base10Float(f64::NAN)),
        Caseless("inf").value(KdlValue::Base10Float(f64::INFINITY)),
        Caseless("-inf").value(KdlValue::Base10Float(f64::NEG_INFINITY)),
    )))
    .context(lbl("keyword"))
    .parse_next(input)
}

/// `bom := '\u{FEFF}'`
fn bom<'s>(input: &mut Input<'s>) -> PResult<()> {
    "\u{FEFF}".void().parse_next(input)
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
fn disallowed_unicode<'s>(input: &mut Input<'s>) -> PResult<()> {
    take_while(1.., |c| match c {
        '\u{0000}'..='\u{0008}' => true,
        '\u{000E}'..='\u{001F}' => true,
        '\u{200E}'..='\u{200F}' => true,
        '\u{202A}'..='\u{202E}' => true,
        '\u{2066}'..='\u{2069}' => true,
        '\u{FEFF}' => true,
        _ => false,
    })
    .void()
    .parse_next(input)
}

/// `escline := '\\' ws* (single-line-comment | newline | eof)`
fn escline<'s>(input: &mut Input<'s>) -> PResult<()> {
    "\\".parse_next(input)?;
    repeat(0.., ws).map(|_: ()| ()).parse_next(input)?;
    alt((single_line_comment, newline, eof.void())).parse_next(input)
}

/// `newline := <See Table>`
fn newline<'s>(input: &mut Input<'s>) -> PResult<()> {
    alt((
        "\u{000D}",
        "\u{000A}",
        "\u{0085}",
        "\u{000C}",
        "\u{2028}",
        "\u{2029}",
        "\u{000D}\u{000A}",
    ))
    .void()
    .context(lbl("newline"))
    .parse_next(input)
}

fn wss<'s>(input: &mut Input<'s>) -> PResult<()> {
    repeat(0.., ws).parse_next(input)
}

fn wsp<'s>(input: &mut Input<'s>) -> PResult<()> {
    repeat(1.., ws).parse_next(input)
}

/// `ws := unicode-space | multi-line-comment``
fn ws<'s>(input: &mut Input<'s>) -> PResult<()> {
    alt((unicode_space, multi_line_comment)).parse_next(input)
}

/// `unicode-space := <See Table>`
fn unicode_space<'s>(input: &mut Input<'s>) -> PResult<()> {
    alt((
        '\u{0009}', '\u{000B}', '\u{0020}', '\u{00A0}', '\u{1680}', '\u{2000}', '\u{2001}',
        '\u{2002}', '\u{2003}', '\u{2004}', '\u{2005}', '\u{2006}', '\u{2007}', '\u{2008}',
        '\u{2009}', '\u{200A}', '\u{202F}', '\u{205F}', '\u{3000}',
    ))
    .void()
    .parse_next(input)
}

/// `single-line-comment := '//' ^newline* (newline | eof)`
fn single_line_comment<'s>(input: &mut Input<'s>) -> PResult<()> {
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
fn multi_line_comment<'s>(input: &mut Input<'s>) -> PResult<()> {
    "/*".parse_next(input)?;
    cut_err(commented_block)
        .context(lbl("closing of multi-line comment"))
        .parse_next(input)
}

/// `commented-block := '*/' | (multi-line-comment | '*' | '/' | [^*/]+) commented-block`
fn commented_block<'s>(input: &mut Input<'s>) -> PResult<()> {
    alt((
        "*/".void(),
        preceded(
            alt((
                multi_line_comment,
                "*".void(),
                "/".void(),
                take_until(1.., "*/").void(),
            )),
            commented_block,
        ),
    ))
    .parse_next(input)
}

/// `multi-line-comment :`
/// `number := keyword-number | hex | octal | binary | decimal`
fn number<'s>(input: &mut Input<'s>) -> PResult<KdlValue> {
    alt((hex, octal, binary, float, integer)).parse_next(input)
}

/// ```text
/// decimal := sign? integer ('.' integer)? exponent?
/// exponent := ('e' | 'E') sign? integer
/// ```
fn float<'s>(input: &mut Input<'s>) -> PResult<KdlValue> {
    alt((
        (
            integer,
            opt(preceded('.', cut_err(integer_base))),
            Caseless("e"),
            opt(one_of(['-', '+'])),
            cut_err(integer_base),
        )
            .take(),
        (integer, '.', cut_err(integer_base)).take(),
    ))
    .try_map(|float_str| {
        str::replace(float_str, "_", "")
            .parse::<f64>()
            .map(KdlValue::Base10Float)
    })
    .context(lbl("float"))
    .parse_next(input)
}

#[cfg(test)]
#[test]
fn float_test() {
    use winnow::token::take;

    assert_eq!(
        float.parse(new_input("12_34.56")).unwrap(),
        KdlValue::Base10Float(1234.56)
    );
    assert_eq!(
        float.parse(new_input("1234_.56")).unwrap(),
        KdlValue::Base10Float(1234.56)
    );
    assert_eq!(
        (float, take(1usize)).parse(new_input("1234.56c")).unwrap(),
        (KdlValue::Base10Float(1234.56), "c")
    );
    assert!(float.parse(new_input("_1234.56")).is_err());
    assert!(float.parse(new_input("1234a.56")).is_err());
}

/// Non-float decimal
fn integer<'s>(input: &mut Input<'s>) -> PResult<KdlValue> {
    let mult = sign.parse_next(input)?;
    integer_base
        .map(|x| KdlValue::Base10(x * mult))
        .context(lbl("integer"))
        .parse_next(input)
}

#[cfg(test)]
#[test]
fn integer_test() {
    assert_eq!(
        integer.parse(new_input("12_34")).unwrap(),
        KdlValue::Base10(1234)
    );
    assert_eq!(
        integer.parse(new_input("1234_")).unwrap(),
        KdlValue::Base10(1234)
    );
    assert!(integer.parse(new_input("_1234")).is_err());
    assert!(integer.parse(new_input("1234a")).is_err());
}

/// `integer := digit (digit | '_')*`
fn integer_base<'s>(input: &mut Input<'s>) -> PResult<i64> {
    (
        digit1,
        cut_err(repeat(
            0..,
            alt(("_", take_while(1.., AsChar::is_dec_digit).take())),
        )),
    )
        .try_map(|(l, r): (&str, Vec<&str>)| {
            format!("{l}{}", str::replace(&r.join(""), "_", "")).parse()
        })
        .parse_next(input)
}

/// `hex := sign? '0x' hex-digit (hex-digit | '_')*`
fn hex<'s>(input: &mut Input<'s>) -> PResult<KdlValue> {
    let mult = sign.parse_next(input)?;
    alt(("0x", "0X")).parse_next(input)?;
    cut_err((
        hex_digit1,
        repeat(
            0..,
            alt(("_", take_while(1.., AsChar::is_hex_digit).take())),
        ),
    ))
    .try_map(|(l, r): (&str, Vec<&str>)| {
        i64::from_str_radix(&format!("{l}{}", str::replace(&r.join(""), "_", "")), 16)
            .map(|x| x * mult)
            .map(KdlValue::Base16)
    })
    .context(lbl("hexadecimal"))
    .parse_next(input)
}

#[cfg(test)]
#[test]
fn test_hex() {
    assert_eq!(
        hex.parse(new_input("0xdead_beef123")).unwrap(),
        KdlValue::Base16(0xdeadbeef123)
    );
    assert_eq!(
        hex.parse(new_input("0xDeAd_BeEf123")).unwrap(),
        KdlValue::Base16(0xdeadbeef123)
    );
    assert_eq!(
        hex.parse(new_input("0xdeadbeef123_")).unwrap(),
        KdlValue::Base16(0xdeadbeef123)
    );
    assert!(hex.parse(new_input("0x_deadbeef123")).is_err());
    assert!(hex.parse(new_input("0xbeefg1")).is_err());
}

/// `octal := sign? '0o' [0-7] [0-7_]*`
fn octal<'s>(input: &mut Input<'s>) -> PResult<KdlValue> {
    let mult = sign.parse_next(input)?;
    alt(("0o", "0O")).parse_next(input)?;
    cut_err((
        oct_digit1,
        repeat(
            0..,
            alt(("_", take_while(1.., AsChar::is_oct_digit).take())),
        ),
    ))
    .try_map(|(l, r): (&str, Vec<&str>)| {
        i64::from_str_radix(&format!("{l}{}", str::replace(&r.join(""), "_", "")), 8)
            .map(|x| x * mult)
            .map(KdlValue::Base8)
    })
    .context(lbl("octal"))
    .parse_next(input)
}

#[cfg(test)]
#[test]
fn test_octal() {
    assert_eq!(
        octal.parse(new_input("0o12_34")).unwrap(),
        KdlValue::Base8(0o1234)
    );
    assert_eq!(
        octal.parse(new_input("0o1234_")).unwrap(),
        KdlValue::Base8(0o1234)
    );
    assert!(octal.parse(new_input("0o_12_34")).is_err());
    assert!(octal.parse(new_input("0o89")).is_err());
}

/// `binary := sign? '0b' ('0' | '1') ('0' | '1' | '_')*`
fn binary<'s>(input: &mut Input<'s>) -> PResult<KdlValue> {
    let mult = sign.parse_next(input)?;
    alt(("0b", "0B")).parse_next(input)?;
    cut_err(
        (alt(("0", "1")), repeat(0.., alt(("0", "1", "_")))).try_map(
            move |(x, xs): (&str, Vec<&str>)| {
                i64::from_str_radix(&format!("{x}{}", str::replace(&xs.join(""), "_", "")), 2)
                    .map(|x| x * mult)
                    .map(KdlValue::Base2)
            },
        ),
    )
    .context(lbl("binary"))
    .parse_next(input)
}

#[cfg(test)]
#[test]
fn test_binary() {
    use winnow::token::take;

    assert_eq!(
        binary.parse(new_input("0b10_01")).unwrap(),
        KdlValue::Base2(0b1001)
    );
    assert_eq!(
        binary.parse(new_input("0b1001_")).unwrap(),
        KdlValue::Base2(0b1001)
    );
    assert!(binary.parse(new_input("0b_10_01")).is_err());
    assert_eq!(
        (binary, take(4usize)).parse(new_input("0b12389")).unwrap(),
        (KdlValue::Base2(1), "2389")
    );
    assert!(binary.parse(new_input("123")).is_err());
}

fn sign<'s>(input: &mut Input<'s>) -> PResult<i64> {
    let sign = opt(alt(('+', '-'))).parse_next(input)?;
    let mult = if let Some(sign) = sign {
        if sign == '+' {
            1
        } else {
            -1
        }
    } else {
        1
    };
    Ok(mult)
}
