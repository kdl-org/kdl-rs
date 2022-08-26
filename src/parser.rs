// A bunch of random variables/functions become dead when you disable
// span support and rather than turning the code into complete cfg
// swiss-cheese, it's simpler to just hush the compiler about it
#![cfg_attr(not(feature = "span"), allow(dead_code, unused_variables))]

use std::ops::RangeTo;

use crate::nom_compat::{many0, many1, many_till};
use miette::SourceSpan;
use nom::branch::alt;
use nom::bytes::complete::{tag, take_until, take_while, take_while_m_n};
use nom::character::complete::{anychar, char, none_of, one_of};
use nom::combinator::{all_consuming, cut, eof, map, map_opt, map_res, opt, peek, recognize};
use nom::error::{context, ParseError};
use nom::sequence::{delimited, preceded, terminated, tuple};
use nom::{Finish, IResult, Offset, Parser, Slice};

use crate::{
    KdlDocument, KdlEntry, KdlError, KdlErrorKind, KdlIdentifier, KdlNode, KdlParseError, KdlValue,
};

/// The parser for the entire input.
///
/// All of our parsing subroutines want to hold onto some global information
/// to generate things like spans, so instead of making them simple free
/// functions, we wrap their bodies in closures that take in a kdl_parser.
/// The free functions then becoming constructors that return those closures.
/// This is basically the same idea behind nom combinators like many0 which
/// take an input to configure the combinator and then return a function.
pub(crate) struct KdlParser<'a> {
    pub(crate) full_input: &'a str,
}

impl<'a> KdlParser<'a> {
    pub(crate) fn new(full_input: &'a str) -> Self {
        Self { full_input }
    }

    pub(crate) fn parse<T, P>(&self, parser: P) -> Result<T, KdlError>
    where
        P: Parser<&'a str, T, KdlParseError<&'a str>>,
    {
        all_consuming(parser)(self.full_input)
            .finish()
            .map(|(_, arg)| arg)
            .map_err(|e| {
                let span_substr = &e.input[..e.len];
                KdlError {
                    input: self.full_input.into(),
                    span: self.span_from_substr(span_substr),
                    help: e.help,
                    label: e.label,
                    kind: if let Some(kind) = e.kind {
                        kind
                    } else if let Some(ctx) = e.context {
                        KdlErrorKind::Context(ctx)
                    } else {
                        KdlErrorKind::Other
                    },
                }
            })
    }

    /// Creates a span for an item using two substrings of self.full_input:
    ///
    /// * before: the remainder of the input before parsing the item
    /// * after: the remainder input after parsing the item
    ///
    /// All we really care about are the addresses of the strings, the lengths don't matter
    fn span_from_before_and_after(&self, before: &str, after: &str) -> SourceSpan {
        let base_addr = self.full_input.as_ptr() as usize;
        let before_addr = before.as_ptr() as usize;
        let after_addr = after.as_ptr() as usize;
        assert!(
            before_addr >= base_addr,
            "tried to get the span of a non-substring!"
        );
        assert!(
            after_addr >= before_addr,
            "subslices were in wrong order for spanning!"
        );

        let start = before_addr - base_addr;
        let end = after_addr - base_addr;
        SourceSpan::from(start..end)
    }

    /// Creates a span for an item using a substring of self.full_input
    ///
    /// Note that substr must be a literal substring, as in it must be
    /// a pointer into the same string!
    fn span_from_substr(&self, substr: &str) -> SourceSpan {
        let base_addr = self.full_input.as_ptr() as usize;
        let substr_addr = substr.as_ptr() as usize;
        assert!(
            substr_addr >= base_addr,
            "tried to get the span of a non-substring!"
        );
        let start = substr_addr - base_addr;
        let end = start + substr.len();
        SourceSpan::from(start..end)
    }
}

fn set_details<'a>(
    mut err: nom::Err<KdlParseError<&'a str>>,
    start: &'a str,
    label: Option<&'static str>,
    help: Option<&'static str>,
) -> nom::Err<KdlParseError<&'a str>> {
    match &mut err {
        nom::Err::Error(e) | nom::Err::Failure(e) => {
            if !e.touched {
                e.len = start.offset(e.input);
                e.input = start;
                e.label = label;
                e.help = help;
                e.touched = true;
            }
        }
        _ => {}
    }
    err
}

pub(crate) fn document<'a: 'b, 'b>(
    kdl_parser: &'b KdlParser<'a>,
) -> impl Fn(&'a str) -> IResult<&'a str, KdlDocument, KdlParseError<&'a str>> + 'b {
    move |input| {
        let start = input;
        let (input, nodes) = many0(node(kdl_parser))(input)?;
        let (input, trailing) = all_whitespace(kdl_parser)(input)?;
        let mut doc = KdlDocument::new();
        doc.set_leading("");
        doc.set_trailing(trailing);
        *doc.nodes_mut() = nodes;
        #[cfg(feature = "span")]
        doc.set_span(kdl_parser.span_from_before_and_after(start, trailing));
        Ok((input, doc))
    }
}

pub(crate) fn node<'a: 'b, 'b>(
    kdl_parser: &'b KdlParser<'a>,
) -> impl Fn(&'a str) -> IResult<&'a str, KdlNode, KdlParseError<&'a str>> + 'b {
    |input| {
        let (input, leading) = all_whitespace(kdl_parser)(input)?;
        let start = input;
        let (input, ty) = opt(context(
            "valid node type annotation",
            annotation(kdl_parser),
        ))(input)?;
        let (input, name) = context("valid node name", identifier(kdl_parser))(input)?;
        let (input, entries) = many0(context("valid node entry", entry(kdl_parser)))(input)?;
        let (input, children) =
            opt(context("valid node children block", children(kdl_parser)))(input)?;
        let (input, trailing) = context(
            "valid node terminator",
            cut(recognize(preceded(
                many0(node_space(kdl_parser)),
                alt((
                    terminated(recognize(tag(";")), opt(alt((linespace, eof)))),
                    alt((newline, single_line_comment, eof)),
                )),
            ))),
        )(input)
        .map_err(|e| {
            set_details(
                e,
                start,
                Some("parsed node"),
                Some("Nodes can only be terminated by `;` or a valid line ending."),
            )
        })?;
        let mut node = KdlNode::new(name);
        node.set_leading(leading);
        node.set_trailing(trailing);
        #[cfg(feature = "span")]
        node.set_span(kdl_parser.span_from_before_and_after(start, trailing));
        node.ty = ty;
        let ents = node.entries_mut();
        *ents = entries;
        if let Some((before, children)) = children {
            let childs = node.children_mut();
            *childs = Some(children);
            node.set_before_children(before);
        }
        Ok((input, node))
    }
}

pub(crate) fn identifier<'a: 'b, 'b>(
    kdl_parser: &'b KdlParser<'a>,
) -> impl Fn(&'a str) -> IResult<&'a str, KdlIdentifier, KdlParseError<&'a str>> + 'b {
    move |input| alt((quoted_identifier(kdl_parser), plain_identifier(kdl_parser)))(input)
}

pub(crate) fn leading_comments<'a: 'b, 'b>(
    kdl_parser: &'b KdlParser<'a>,
) -> impl Fn(&'a str) -> IResult<&'a str, Vec<&'a str>, KdlParseError<&'a str>> + 'b {
    move |input| {
        terminated(
            many0(preceded(
                opt(many0(alt((newline, unicode_space)))),
                comment(kdl_parser),
            )),
            opt(many0(alt((newline, unicode_space, eof)))),
        )(input)
    }
}

pub(crate) fn trailing_comments<'a: 'b, 'b>(
    kdl_parser: &'b KdlParser<'a>,
) -> impl Fn(&'a str) -> IResult<&'a str, Vec<&'a str>, KdlParseError<&'a str>> + 'b {
    move |mut input| {
        let mut comments = vec![];
        loop {
            let (inp, _) = opt(many0(alt((newline, unicode_space, tag("\\")))))(input)?;
            let (inp, comment) = opt(comment(kdl_parser))(inp)?;
            if let Some(comment) = comment {
                comments.push(comment);
            }
            let (inp, _) = opt(many0(alt((newline, unicode_space, tag("\\"), tag(";")))))(inp)?;
            let (inp, end) = opt(eof)(inp)?;
            if end.is_some() {
                return Ok((inp, comments));
            }
            if input == inp {
                panic!("invalid trailing text");
            }
            input = inp;
        }
    }
}

fn plain_identifier<'a: 'b, 'b>(
    kdl_parser: &'b KdlParser<'a>,
) -> impl Fn(&'a str) -> IResult<&'a str, KdlIdentifier, KdlParseError<&'a str>> + 'b {
    move |input| {
        let start = input;
        let (input, name) = recognize(preceded(
            take_while_m_n(1, 1, KdlIdentifier::is_initial_char),
            cut(take_while(KdlIdentifier::is_identifier_char)),
        ))(input).map_err(|e| set_details(e, start, Some("invalid identifier character"), Some("See https://github.com/kdl-org/kdl/blob/main/SPEC.md#identifier for an explanation of valid KDL identifiers.")))?;
        match name {
            "false" | "true" | "null" => {
                return Err(nom::Err::Error(KdlParseError {
                    input,
                    context: Some("non-keyword identifier"),
                    len: name.len(),
                    label: Some("reserved keyword"),
                    help: Some("Reserved keywords cannot be used as identifiers."),
                    kind: None,
                    touched: false,
                }))
            }
            _ => {}
        }
        let mut ident = KdlIdentifier::from(name);
        ident.set_repr(name);
        #[cfg(feature = "span")]
        ident.set_span(kdl_parser.span_from_before_and_after(start, input));
        Ok((input, ident))
    }
}

fn quoted_identifier<'a: 'b, 'b>(
    kdl_parser: &'b KdlParser<'a>,
) -> impl Fn(&'a str) -> IResult<&str, KdlIdentifier, KdlParseError<&str>> + 'b {
    move |input| {
        let start = input;
        let (input, (raw, val)) = alt((string, raw_string))(input)?;
        let mut ident = KdlIdentifier::from(val.as_string().unwrap());
        ident.set_repr(raw);
        #[cfg(feature = "span")]
        ident.set_span(kdl_parser.span_from_before_and_after(start, input));
        Ok((input, ident))
    }
}

pub(crate) fn entry_with_trailing<'a: 'b, 'b>(
    kdl_parser: &'b KdlParser<'a>,
) -> impl Fn(&'a str) -> IResult<&'a str, KdlEntry, KdlParseError<&'a str>> + 'b {
    move |input| {
        let (input, mut leading) = recognize(many0(node_space(kdl_parser)))(input)?;
        if leading.is_empty() {
            leading = " ";
        };
        let (input, mut entry) = alt((property(kdl_parser), argument(kdl_parser)))(input)?;
        let (input, trailing) = recognize(many0(node_space(kdl_parser)))(input)?;
        entry.set_leading(leading);
        entry.set_trailing(trailing);
        Ok((input, entry))
    }
}

fn entry<'a: 'b, 'b>(
    kdl_parser: &'b KdlParser<'a>,
) -> impl Fn(&'a str) -> IResult<&'a str, KdlEntry, KdlParseError<&'a str>> + 'b {
    move |input| {
        let (input, leading) = recognize(many1(node_space(kdl_parser)))(input)?;
        let (input, mut entry) = alt((property(kdl_parser), argument(kdl_parser)))(input)?;
        entry.set_leading(leading);
        Ok((input, entry))
    }
}

fn entry_maybe_space<'a: 'b, 'b>(
    kdl_parser: &'b KdlParser<'a>,
) -> impl Fn(&'a str) -> IResult<&'a str, KdlEntry, KdlParseError<&'a str>> + 'b {
    move |input| {
        let (input, leading) = recognize(many0(node_space(kdl_parser)))(input)?;
        let (input, mut entry) = alt((property(kdl_parser), argument(kdl_parser)))(input)?;
        entry.set_leading(leading);
        Ok((input, entry))
    }
}

fn property<'a: 'b, 'b>(
    kdl_parser: &'b KdlParser<'a>,
) -> impl Fn(&'a str) -> IResult<&'a str, KdlEntry, KdlParseError<&'a str>> + 'b {
    move |input| {
        let start = input;
        let (input, name) = identifier(kdl_parser)(input)?;
        let (input, _) = context("'=' after property name", tag("="))(input)?;
        let (input, ty) = opt(annotation(kdl_parser))(input)?;
        let (input, (raw, value)) = context("property value", cut(value))(input).map_err(|e| set_details(e, input, Some("invalid value"), Some("Please refer to https://github.com/kdl-org/kdl/blob/main/SPEC.md#value for valid KDL value syntaxes.")))?;
        let mut entry = KdlEntry::new_prop(name, value);
        entry.ty = ty;
        entry.set_trailing("");
        entry.set_value_repr(raw);
        #[cfg(feature = "span")]
        entry.set_span(kdl_parser.span_from_before_and_after(start, input));
        Ok((input, entry))
    }
}

fn argument<'a: 'b, 'b>(
    kdl_parser: &'b KdlParser<'a>,
) -> impl Fn(&'a str) -> IResult<&'a str, KdlEntry, KdlParseError<&'a str>> + 'b {
    move |input| {
        let start = input;
        let (input, ty) = opt(annotation(kdl_parser))(input)?;
        let (input, (raw, value)) = if ty.is_some() {
            context("valid value", cut(value))(input)
        } else {
            context("valid value", value)(input)
        }?;
        let mut entry = KdlEntry::new(value);
        entry.ty = ty;
        entry.set_trailing("");
        entry.set_value_repr(raw);
        #[cfg(feature = "span")]
        entry.set_span(kdl_parser.span_from_before_and_after(start, input));
        Ok((input, entry))
    }
}

fn value(input: &str) -> IResult<&str, (String, KdlValue), KdlParseError<&str>> {
    alt((
        null,
        boolean,
        string,
        raw_string,
        hexadecimal,
        octal,
        binary,
        float,
        integer,
    ))(input)
}

fn children<'a: 'b, 'b>(
    kdl_parser: &'b KdlParser<'a>,
) -> impl Fn(&'a str) -> IResult<&'a str, (&'a str, KdlDocument), KdlParseError<&'a str>> + 'b {
    move |input| {
        let (input, before) = recognize(many0(node_space(kdl_parser)))(input)?;
        let start = input;
        let (input, _) = tag("{")(input)?;
        let (input, children) = document(kdl_parser)(input)?;
        let (input, _) = cut(context("closing '}' in node children block", tag("}")))(input)
            .map_err(|e| set_details(e, start, Some("children block body"), None))?;
        Ok((input, (before, children)))
    }
}

fn annotation<'a: 'b, 'b>(
    kdl_parser: &'b KdlParser<'a>,
) -> impl Fn(&'a str) -> IResult<&'a str, KdlIdentifier, KdlParseError<&'a str>> + 'b {
    move |input| {
        let start = input;
        let (input, _) = tag("(")(input)?;
        let (input, ty) = cut(identifier(kdl_parser))(input)?;
        let (input, _) = context("closing ')' for type annotation", cut(tag(")")))(input)
            .map_err(|e| set_details(e, start, Some("annotation"), Some("annotations can only be KDL identifiers (including string identifiers), and can't have any space inside the parentheses.")))?;
        Ok((input, ty))
    }
}

fn all_whitespace<'a: 'b, 'b>(
    kdl_parser: &'b KdlParser<'a>,
) -> impl Fn(&'a str) -> IResult<&'a str, &'a str, KdlParseError<&'a str>> + 'b {
    move |input| recognize(many0(alt((comment(kdl_parser), unicode_space, newline))))(input)
}

fn whitespace(input: &str) -> IResult<&str, &str, KdlParseError<&str>> {
    recognize(alt((unicode_space, multi_line_comment)))(input)
}

fn linespace(input: &str) -> IResult<&str, &str, KdlParseError<&str>> {
    recognize(alt((unicode_space, newline, single_line_comment)))(input)
}

fn node_space<'a: 'b, 'b>(
    kdl_parser: &'b KdlParser<'a>,
) -> impl Fn(&'a str) -> IResult<&'a str, &'a str, KdlParseError<&'a str>> + 'b {
    move |input| {
        context(
            "node space",
            recognize(alt((
                delimited(many0(whitespace), escline, many0(whitespace)),
                recognize(many1(whitespace)),
                node_slashdash(kdl_parser),
            ))),
        )(input)
    }
}

fn escline(input: &str) -> IResult<&str, &str, KdlParseError<&str>> {
    recognize(preceded(
        tag("\\"),
        context(
            "newline after line escape",
            cut(preceded(
                many0(whitespace),
                alt((single_line_comment, newline)),
            )),
        ),
    ))(input).map_err(|e| set_details(e, input, Some("line escape starts here"), Some("line escapes can only be followed by whitespace plus a newline (or single-line comment).")))
}

fn unicode_space(input: &str) -> IResult<&str, &str, KdlParseError<&str>> {
    alt((
        tag(" "),
        tag("\t"),
        tag("\u{FEFF}"), // BOM
        tag("\u{00A0}"),
        tag("\u{1680}"),
        tag("\u{2000}"),
        tag("\u{2001}"),
        tag("\u{2002}"),
        tag("\u{2003}"),
        tag("\u{2004}"),
        tag("\u{2005}"),
        tag("\u{2006}"),
        tag("\u{2007}"),
        tag("\u{2008}"),
        tag("\u{2009}"),
        tag("\u{200A}"),
        tag("\u{202F}"),
        tag("\u{205F}"),
        tag("\u{3000}"),
    ))(input)
}

/// `newline := All line-break unicode white_space
pub(crate) fn newline(input: &str) -> IResult<&str, &str, KdlParseError<&str>> {
    alt((
        tag("\r\n"),
        tag("\r"),
        tag("\n"),
        tag("\u{0085}"),
        tag("\u{000C}"),
        tag("\u{2028}"),
        tag("\u{2029}"),
    ))(input)
}

fn comment<'a: 'b, 'b>(
    kdl_parser: &'b KdlParser<'a>,
) -> impl Fn(&'a str) -> IResult<&'a str, &'a str, KdlParseError<&'a str>> + 'b {
    move |input| {
        alt((
            single_line_comment,
            multi_line_comment,
            slashdash_comment(kdl_parser),
        ))(input)
    }
}

/// `single-line-comment := '//' ('\r' [^\n] | [^\r\n])* (newline | eof)`
fn single_line_comment(input: &str) -> IResult<&str, &str, KdlParseError<&str>> {
    recognize(preceded(
        tag("//"),
        cut(many_till(
            anychar,
            context("newline or eof after //", alt((newline, eof))),
        )),
    ))(input)
    .map_err(|e| set_details(e, input, Some("comment"), None))
}

/// `multi-line-comment := '/*' commented-block
fn multi_line_comment(input: &str) -> IResult<&str, &str, KdlParseError<&str>> {
    recognize(preceded(
        tag("/*"),
        context("comment block body", cut(commented_block)),
    ))(input)
    .map_err(|e| set_details(e, input, Some("comment"), Some("multi-line comments must start with /* and be terminated with a matching */. They may be nested, but their */ must match.")))
}

/// `commented-block := '*/' | (multi-line-comment | '*' | '/' | [^*/]+) commented-block`
fn commented_block(input: &str) -> IResult<&str, &str, KdlParseError<&str>> {
    alt((
        tag("*/"),
        terminated(
            alt((
                multi_line_comment,
                tag("*"),
                tag("/"),
                recognize(many_till(anychar, peek(alt((tag("*"), tag("/")))))),
            )),
            commented_block,
        ),
    ))(input)
}

fn node_slashdash<'a: 'b, 'b>(
    kdl_parser: &'b KdlParser<'a>,
) -> impl Fn(&'a str) -> IResult<&'a str, &'a str, KdlParseError<&'a str>> + 'b {
    move |input| {
        recognize(preceded(
            tag("/-"),
            context(
                "node following a slashdash",
                cut(alt((
                    recognize(entry_maybe_space(kdl_parser)),
                    recognize(children(kdl_parser)),
                ))),
            ),
        ))(input)
        .map_err(|e| set_details(e, input, Some("slashdash"), None))
    }
}

fn slashdash_comment<'a: 'b, 'b>(
    kdl_parser: &'b KdlParser<'a>,
) -> impl Fn(&'a str) -> IResult<&'a str, &'a str, KdlParseError<&'a str>> + 'b {
    move |input| {
        recognize(preceded(tag("/-"), cut(node(kdl_parser))))(input)
            .map_err(|e| set_details(e, input, Some("slashdash"), None))
    }
}

fn boolean(input: &str) -> IResult<&str, (String, KdlValue), KdlParseError<&str>> {
    alt((
        map(tag("true"), |s: &str| (s.into(), KdlValue::Bool(true))),
        map(tag("false"), |s: &str| (s.into(), KdlValue::Bool(false))),
    ))(input)
}

fn null(input: &str) -> IResult<&str, (String, KdlValue), KdlParseError<&str>> {
    map(tag("null"), |s: &str| (s.into(), KdlValue::Null))(input)
}

/// `escaped-string := '"' character* '"'`
fn string(input: &str) -> IResult<&str, (String, KdlValue), KdlParseError<&str>> {
    let (input, _) = tag("\"")(input)?;
    let mut original = String::new();
    let mut value = String::new();
    original.push('"');
    let (input, chars) = many0(character)(input)?;
    for (raw, processed) in chars {
        original.push_str(raw);
        value.push(processed);
    }
    let (input, _) =
        cut(tag("\""))(input).map_err(|e| set_details(e, input, Some("string"), None))?;
    original.push('"');
    Ok((input, (original, KdlValue::String(value))))
}

/// `character := '\' escape | [^\"]`
fn character(input: &str) -> IResult<&str, (&str, char), KdlParseError<&str>> {
    with_raw(alt((preceded(char('\\'), cut(escape)), none_of("\\\""))))(input)
}

/// This is like `recognize`, but _also_ returns the actual value.
fn with_raw<I: Clone + Offset + Slice<RangeTo<usize>>, O, E: ParseError<I>, F>(
    mut parser: F,
) -> impl FnMut(I) -> IResult<I, (I, O), E>
where
    F: Parser<I, O, E>,
{
    move |input: I| {
        let i = input.clone();
        match parser.parse(i) {
            Ok((i, x)) => {
                let index = input.offset(&i);
                Ok((i, (input.slice(..index), x)))
            }
            Err(e) => Err(e),
        }
    }
}

/// `escape := ["\\/bfnrt] | 'u{' hex-digit{1, 6} '}'`
fn escape(input: &str) -> IResult<&str, char, KdlParseError<&str>> {
    alt((
        delimited(tag("u{"), cut(unicode), char('}')),
        map_opt(anychar, |c| match c {
            '"' => Some('"'),
            '\\' => Some('\\'),
            '/' => Some('/'),
            'b' => Some('\u{08}'),
            'f' => Some('\u{0C}'),
            'n' => Some('\n'),
            'r' => Some('\r'),
            't' => Some('\t'),
            _ => None,
        }),
    ))(input)
}

fn unicode(input: &str) -> IResult<&str, char, KdlParseError<&str>> {
    // TODO: This should only accept up to 0x10FFFF.
    map_opt(
        map_res(
            take_while_m_n(1, 6, |c: char| c.is_ascii_hexdigit()),
            |hex| u32::from_str_radix(hex, 16),
        ),
        std::char::from_u32,
    )(input)
}

/// `raw-string := 'r' raw-string-hash`
/// `raw-string-hash := '#' raw-string-hash '#' | raw-string-quotes`
/// `raw-string-quotes := '"' .* '"'`
fn raw_string(input: &str) -> IResult<&str, (String, KdlValue), KdlParseError<&str>> {
    let mut raw = String::new();
    let (input, _) = char('r')(input)?;
    raw.push('r');
    let (input, hashes) = recognize(many0(char('#')))(input)?;
    raw.push_str(hashes);
    let (input, _) = char('"')(input)?;
    raw.push('"');
    let close = format!("\"{}", hashes);
    let (input, value) = take_until(&close[..])(input)?;
    raw.push_str(value);
    let (input, _) = cut(tag(&close[..]))(input)?;
    raw.push_str(&close);
    Ok((input, (raw, KdlValue::RawString(value.into()))))
}

fn float(input: &str) -> IResult<&str, (String, KdlValue), KdlParseError<&str>> {
    map_res(
        with_raw(alt((
            recognize(tuple((
                integer,
                opt(preceded(char('.'), cut(integer))),
                one_of("eE"),
                opt(one_of("+-")),
                cut(integer),
            ))),
            recognize(tuple((integer, char('.'), cut(integer)))),
        ))),
        |(raw, x)| {
            str::replace(x, "_", "")
                .parse::<f64>()
                .map(|x| (raw.into(), KdlValue::Base10Float(x)))
        },
    )(input)
    .map_err(|e| {
        set_details(
            e,
            input,
            Some("invalid float"),
            Some(
                "Floating point numbers must be base 10, and have numbers after the decimal point.",
            ),
        )
    })
}

/// ```text
/// integer := sign? [1-9] [0-9_]*
/// sign := '+' | '-'
/// ```
fn integer(input: &str) -> IResult<&str, (String, KdlValue), KdlParseError<&str>> {
    let mut raw = String::new();
    let (input, (raw_sign, sign)) = with_raw(sign)(input)?;
    raw.push_str(raw_sign);
    map_res(
        with_raw(recognize(many1(terminated(
            one_of("0123456789"),
            many0(char('_')),
        )))),
        move |(raw_int, out)| {
            raw.push_str(raw_int);
            str::replace(out, "_", "")
                .parse::<i64>()
                .map(move |x| x * sign)
                .map(|x| (raw.clone(), KdlValue::Base10(x)))
        },
    )(input)
}

fn hexadecimal(input: &str) -> IResult<&str, (String, KdlValue), KdlParseError<&str>> {
    let mut raw = String::new();
    let (input, (raw_sign, sign)) = with_raw(sign)(input)?;
    raw.push_str(raw_sign);
    map_res(
        with_raw(preceded(
            alt((tag("0x"), tag("0X"))),
            context(
                "hexadecimal value",
                cut(recognize(many1(terminated(
                    one_of("0123456789abcdefABCDEF"),
                    many0(char('_')),
                )))),
            ),
        )),
        move |(raw_body, hex): (&str, &str)| {
            raw.push_str(raw_body);
            // TODO: Failure in case of int overflow!
            i64::from_str_radix(&str::replace(hex, "_", ""), 16)
                .map(|x| x * sign)
                .map(|x| (raw.clone(), KdlValue::Base16(x)))
        },
    )(input)
    .map_err(|e| set_details(e, input, Some("invalid hexadecimal"), Some("Hexadecimal values can only include the characters 0-9 and a-f (case-insensitive), with optional `_` separators.")))
}

/// `octal := sign? '0o' [0-7] [0-7_]*`
fn octal(input: &str) -> IResult<&str, (String, KdlValue), KdlParseError<&str>> {
    let mut raw = String::new();
    let (input, (raw_sign, sign)) = with_raw(sign)(input)?;
    raw.push_str(raw_sign);
    map_res(
        with_raw(preceded(
            alt((tag("0o"), tag("0O"))),
            context(
                "octal value",
                cut(recognize(many1(terminated(
                    one_of("01234567"),
                    many0(char('_')),
                )))),
            ),
        )),
        move |(raw_body, oct): (&str, &str)| {
            raw.push_str(raw_body);
            i64::from_str_radix(&str::replace(oct, "_", ""), 8)
                .map(|x| x * sign)
                .map(|x| (raw.clone(), KdlValue::Base8(x)))
        },
    )(input)
    .map_err(|e| {
        set_details(
            e,
            input,
            Some("invalid octal"),
            Some("octal values can only include the characters 0-7, with optional `_` separators."),
        )
    })
}

/// `binary := sign? '0b' ('0' | '1') ('0' | '1' | '_')*`
fn binary(input: &str) -> IResult<&str, (String, KdlValue), KdlParseError<&str>> {
    let mut raw = String::new();
    let (input, (raw_sign, sign)) = with_raw(sign)(input)?;
    raw.push_str(raw_sign);
    map_res(
        with_raw(preceded(
            alt((tag("0b"), tag("0B"))),
            context(
                "binary value",
                cut(recognize(many1(terminated(one_of("01"), many0(char('_')))))),
            ),
        )),
        move |(raw_body, binary): (&str, &str)| {
            raw.push_str(raw_body);
            i64::from_str_radix(&str::replace(binary, "_", ""), 2)
                .map(|x| x * sign)
                .map(|x| (raw.clone(), KdlValue::Base2(x)))
        },
    )(input)
    .map_err(|e| set_details(e, input, Some("invalid binary"), Some("Hexadecimal values can only include the characters 0 and 1, with optional `_` separators.")))
}

fn sign(input: &str) -> IResult<&str, i64, KdlParseError<&str>> {
    let (input, sign) = opt(alt((char('+'), char('-'))))(input)?;
    let mult = if let Some(sign) = sign {
        if sign == '+' {
            1
        } else {
            -1
        }
    } else {
        1
    };
    Ok((input, mult))
}

#[cfg(test)]
mod node_tests {
    use super::*;

    #[test]
    fn basic() {
        let input = "foo 1 \"bar\"=false";
        let kdl_parser = crate::parser::KdlParser::new(input);
        match node(&kdl_parser)(input) {
            Ok(("", parsed)) => {
                let mut ident = KdlIdentifier::from("foo");
                ident.set_repr("foo");
                assert_eq!(parsed.name(), &ident);

                let mut entries = parsed.entries().iter();

                let mut one = KdlEntry::new(1);
                one.set_leading(" ");
                one.set_trailing("");
                one.set_value_repr("1");
                assert_eq!(entries.next(), Some(&one));

                let mut ident = KdlIdentifier::from("bar");
                ident.set_repr("\"bar\"");
                let mut bar = KdlEntry::new_prop(ident, false);
                bar.set_leading(" ");
                bar.set_trailing("");
                bar.set_value_repr("false");
                assert_eq!(entries.next(), Some(&bar));
            }
            Ok(_) => panic!("unexpected success"),
            Err(e) => {
                panic!("failed to parse: {:?}", e);
            }
        };
    }
}

#[cfg(test)]
mod whitespace_tests {
    #[test]
    fn basic() {
        use super::all_whitespace;

        let input = " \t\n\r";
        let kdl_parser = crate::parser::KdlParser::new(input);
        assert_eq!(all_whitespace(&kdl_parser)(input), Ok(("", " \t\n\r")));
    }
}

#[cfg(test)]
mod comment_tests {
    use super::*;

    #[test]
    fn single_line() {
        let input = "// Hello world";
        let kdl_parser = crate::parser::KdlParser::new(input);
        assert_eq!(comment(&kdl_parser)(input), Ok(("", "// Hello world")));
    }

    #[test]
    fn multi_line() {
        let input = "/* Hello world */";
        let kdl_parser = crate::parser::KdlParser::new(input);
        assert_eq!(comment(&kdl_parser)(input), Ok(("", "/* Hello world */")));

        let input = "/* Hello /* world */ blah */";
        let kdl_parser = crate::parser::KdlParser::new(input);
        assert_eq!(
            comment(&kdl_parser)(input),
            Ok(("", "/* Hello /* world */ blah */"))
        );
    }

    #[test]
    fn slashdash() {
        let input = "/-foo 1 2";
        let kdl_parser = crate::parser::KdlParser::new(input);
        assert_eq!(comment(&kdl_parser)(input), Ok(("", "/-foo 1 2")));
    }

    #[test]
    fn surrounding() {
        // assert_eq!(trailing_comments("// foo"), Ok(("", vec!["// foo"])));
        // assert_eq!(trailing_comments("/* foo */"), Ok(("", vec!["/* foo */"])));
        // assert_eq!(trailing_comments("/* foo */ \n // bar"), Ok(("", vec!["/* foo */", "// bar"])));
    }
}

#[cfg(test)]
mod value_tests {
    use super::*;

    #[test]
    fn boolean_val() {
        assert_eq!(
            value("true"),
            Ok(("", ("true".into(), KdlValue::Bool(true))))
        );
        assert_eq!(
            value("false"),
            Ok(("", ("false".into(), KdlValue::Bool(false))))
        );
    }

    #[test]
    fn null_val() {
        assert_eq!(value("null"), Ok(("", ("null".into(), KdlValue::Null))));
    }

    #[test]
    fn binary_val() {
        assert_eq!(
            value("0b0101"),
            Ok(("", ("0b0101".into(), KdlValue::Base2(0b0101))))
        );
        assert_eq!(
            value("0b0101_1111"),
            Ok(("", ("0b0101_1111".into(), KdlValue::Base2(0b0101_1111))))
        );
        assert_eq!(
            value("-0b0101"),
            Ok(("", ("-0b0101".into(), KdlValue::Base2(-0b0101))))
        );
        assert_eq!(
            value("+0b0101"),
            Ok(("", ("+0b0101".into(), KdlValue::Base2(0b0101))))
        );
    }

    #[test]
    fn octal_val() {
        assert_eq!(
            value("0o01234567"),
            Ok(("", ("0o01234567".into(), KdlValue::Base8(0o01234567))))
        );
        assert_eq!(
            value("0o123_4567"),
            Ok(("", ("0o123_4567".into(), KdlValue::Base8(0o1234567))))
        );
        assert_eq!(
            value("-0o123"),
            Ok(("", ("-0o123".into(), KdlValue::Base8(-0o123))))
        );
        assert_eq!(
            value("+0o123"),
            Ok(("", ("+0o123".into(), KdlValue::Base8(0o123))))
        );
    }

    #[test]
    fn hexadecimal_val() {
        assert_eq!(
            value("0x0123456789abcdef"),
            Ok((
                "",
                (
                    "0x0123456789abcdef".into(),
                    KdlValue::Base16(0x0123456789abcdef)
                )
            ))
        );
        let input = "node 0x0123_4567_89ab_cdef";
        let kdl_parser = crate::parser::KdlParser::new(input);
        let (_, n) = node(&kdl_parser)(input).expect("failed to parse node");
        assert_eq!(&n[0], &KdlValue::Base16(0x0123456789abcdef));
        assert_eq!(
            value("0x123_4567"),
            Ok(("", ("0x123_4567".into(), KdlValue::Base16(0x1234567))))
        );
        assert_eq!(
            value("-0x123"),
            Ok(("", ("-0x123".into(), KdlValue::Base16(-0x123))))
        );
        assert_eq!(
            value("+0x123"),
            Ok(("", ("+0x123".into(), KdlValue::Base16(0x123))))
        );
    }

    #[test]
    fn integer_val() {
        assert_eq!(
            value("123_456"),
            Ok(("", ("123_456".into(), KdlValue::Base10(123456))))
        );
        assert_eq!(
            value("-123"),
            Ok(("", ("-123".into(), KdlValue::Base10(-123))))
        );
        assert_eq!(
            value("+123"),
            Ok(("", ("+123".into(), KdlValue::Base10(123))))
        );
    }

    #[test]
    fn float_val() {
        assert_eq!(
            value("123_456.789e-10"),
            Ok((
                "",
                (
                    "123_456.789e-10".into(),
                    KdlValue::Base10Float(123_456.789e-10)
                )
            ))
        );
        assert_eq!(
            value("-123.456"),
            Ok(("", ("-123.456".into(), KdlValue::Base10Float(-123.456))))
        );
        assert_eq!(
            value("+123.456"),
            Ok(("", ("+123.456".into(), KdlValue::Base10Float(123.456))))
        );
    }

    #[test]
    fn string_val() {
        assert_eq!(
            value(r#""Hello \n\u{2020}world""#),
            Ok((
                "",
                (
                    r#""Hello \n\u{2020}world""#.into(),
                    KdlValue::String("Hello \n\u{2020}world".into())
                )
            ))
        );
    }

    #[test]
    fn raw_string_val() {
        assert_eq!(
            value(r#"r"Hello \n\u{2020}world""#),
            Ok((
                "",
                (
                    r#"r"Hello \n\u{2020}world""#.into(),
                    KdlValue::RawString(r"Hello \n\u{2020}world".into())
                )
            ))
        );
        assert_eq!(
            value(r###"r##"Hello \n\u{2020}world"##"###),
            Ok((
                "",
                (
                    r###"r##"Hello \n\u{2020}world"##"###.into(),
                    KdlValue::RawString(r"Hello \n\u{2020}world".into())
                )
            ))
        );
    }
}
