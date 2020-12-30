use std::{collections::HashMap, iter::from_fn};

use nom::branch::alt;
use nom::bytes::complete::{tag, take_until, take_while_m_n};
use nom::character::complete::{alpha1, alphanumeric1, anychar, char, none_of, one_of};
use nom::combinator::{
    all_consuming, eof, iterator, map, map_opt, map_res, not, opt, recognize, value,
};
use nom::multi::{fold_many0, many0, many1, many_till};
use nom::sequence::{delimited, pair, preceded, terminated, tuple};
use nom::Finish;
use nom::IResult;

use crate::error::KdlParseError;
use crate::node::{KdlComment, KdlNode, KdlValue};

/// `nodes := linespace* (node nodes?)? linespace*`
pub(crate) fn nodes(input: &str) -> IResult<&str, Vec<KdlNode>, KdlParseError<&str>> {
    let (input, _) = many0(linespace)(input)?;
    let (input, nodes) = map(many0(terminated(node, many0(linespace))), |nodes| {
        nodes.into_iter().filter_map(|node| node).collect()
    })(input)?;
    let (input, _) = many0(linespace)(input)?;
    Ok((input, nodes))
}

// The following two functions exist for the purposes of translating offsets into line/column pairs
// for error reporting. We're doing this here so we can make use of our `newline` definition, to
// ensure line/column information is reported accurately based on our definition of newlines, even
// if we update our definition of newlines later.

/// Counts all lines in the input up to the final line.
///
/// This counts and skips past all lines terminated in `newline` with the exception of the final
/// line, regardless of whether it's newline-terminated. If the input only contains a single line,
/// the input will be returned unmodified with a count of `0`.
pub(crate) fn count_leading_lines(input: &str) -> (&str, usize) {
    let mut iter = iterator(
        input,
        terminated(many_till(value((), anychar), newline), not(eof)),
    );
    let count = (&mut iter).count();
    match iter.finish().finish() {
        Ok((input, _)) => (input, count),
        // I don't believe this particular parser can error, but we need to handle it anyway
        Err(e) => (e.input, count),
    }
}

/// Strips a single trailing `newline`, if present, from the input.
pub(crate) fn strip_trailing_newline(input: &str) -> &str {
    // Nom doesn't support parsing in reverse, but we want to reuse our newline definition. The
    // longest newline sequence is 2 characters, so we can just test the last char, and the
    // second-to-last char, and validate that the parser consumes the full input.
    let mut idx_iter = input.char_indices().map(|(idx, _)| idx);
    let mut last = idx_iter.next_back();
    let mut second_last = idx_iter.next_back();
    // Start with the second-to-last, otherwise \r\n will be parsed as just the \n.
    from_fn(|| second_last.take().or_else(|| last.take()))
        .find(|&idx| all_consuming(newline)(&input[idx..]).is_ok())
        .map(|idx| &input[..idx])
        .unwrap_or(input)
}

#[derive(Clone)]
enum NodeArg {
    Value(KdlValue),
    Property(String, KdlValue),
}

/// `node := ('/-' ws*)? identifier (node-space node-props-and-args)* (node-space* node-children ws*)? node-terminator`
pub(crate) fn node(input: &str) -> IResult<&str, Option<KdlNode>, KdlParseError<&str>> {
    let (input, comment) = opt(terminated(tag("/-"), many0(whitespace)))(input)?;
    let (input, tag) = identifier(input)?;
    let (input, args) = many0(preceded(node_space, node_prop_or_arg))(input)?;
    let (input, children) = opt(delimited(
        many0(node_space),
        node_children,
        many0(whitespace),
    ))(input)?;
    let (input, _) = node_terminator(input)?;
    if comment.is_some() {
        Ok((input, None))
    } else {
        let (values, properties): (Vec<NodeArg>, Vec<NodeArg>) = args
            .into_iter()
            .filter_map(|n| n)
            .partition(|arg| matches!(arg, NodeArg::Value(_)));
        Ok((
            input,
            Some(KdlNode {
                name: tag,
                children: children.unwrap_or_else(Vec::new),
                values: values
                    .into_iter()
                    .map(|arg| match arg {
                        NodeArg::Value(val) => val,
                        _ => unreachable!(),
                    })
                    .collect(),
                properties: properties.into_iter().fold(HashMap::new(), |mut acc, arg| {
                    match arg {
                        NodeArg::Property(key, value) => {
                            acc.insert(key, value);
                        }
                        _ => unreachable!(),
                    }
                    acc
                }),
            }),
        ))
    }
}

/// `bare_identifier := [a-zA-Z_] [a-zA-Z0-9!$%&'*+\-./:<>?@\^_|~]*`
pub(crate) fn bare_identifier(input: &str) -> IResult<&str, &str, KdlParseError<&str>> {
    recognize(pair(
        alt((alpha1, tag("_"))),
        many0(alt((alphanumeric1, recognize(one_of("~!@$%^&*-_+./:<>?"))))),
    ))(input)
}

/// `identifier := bare_identifier | string`
fn identifier(input: &str) -> IResult<&str, String, KdlParseError<&str>> {
    alt((map(bare_identifier, String::from), string))(input)
}

/// `node-props-and-args := ('/-' ws*)? (prop | value)`
fn node_prop_or_arg(input: &str) -> IResult<&str, Option<NodeArg>, KdlParseError<&str>> {
    let (input, comment) = opt(terminated(tag("/-"), many0(whitespace)))(input)?;
    let (input, proparg) = alt((
        map(property, |(key, val)| NodeArg::Property(key, val)),
        map(node_value, NodeArg::Value),
    ))(input)?;
    if comment.is_some() {
        Ok((input, None))
    } else {
        Ok((input, Some(proparg)))
    }
}

/// `prop := identifier '=' value`
fn property(input: &str) -> IResult<&str, (String, KdlValue), KdlParseError<&str>> {
    let (input, key) = identifier(input)?;
    let (input, _) = tag("=")(input)?;
    let (input, val) = node_value(input)?;
    Ok((input, (key, val)))
}

/// `value := string | raw_string | number | boolean | 'null'`
fn node_value(input: &str) -> IResult<&str, KdlValue, KdlParseError<&str>> {
    alt((
        map(string, KdlValue::String),
        map(raw_string, |s| KdlValue::String(s.into())),
        number,
        boolean,
        value(KdlValue::Null, tag("null")),
    ))(input)
}

/// node-terminator := single-line-comment | newline | ';' | eof
fn node_terminator(input: &str) -> IResult<&str, Option<KdlComment>, KdlParseError<&str>> {
    alt((
        value(None, eof),
        map(single_line_comment, Option::Some),
        value(None, newline),
        value(None, char(';')),
    ))(input)
}

/// `node-children := ('/-' ws*)? '{' nodes '}'`
fn node_children(input: &str) -> IResult<&str, Vec<KdlNode>, KdlParseError<&str>> {
    let (input, comment) = opt(terminated(tag("/-"), many0(whitespace)))(input)?;
    let (input, children) = delimited(tag("{"), nodes, tag("}"))(input)?;
    if comment.is_some() {
        Ok((input, Vec::new()))
    } else {
        Ok((input, children))
    }
}

/// `string := '"' character* '"'`
fn string(input: &str) -> IResult<&str, String, KdlParseError<&str>> {
    delimited(
        char('"'),
        fold_many0(character, String::new(), |mut acc, ch| {
            acc.push(ch);
            acc
        }),
        char('"'),
    )(input)
}

/// `character := '\' escape | [^\"]`
fn character(input: &str) -> IResult<&str, char, KdlParseError<&str>> {
    alt((preceded(char('\\'), escape), none_of("\\\"")))(input)
}

// creates a (map, inverse map) tuple
macro_rules! bimap {
    ($($x:expr => $y:expr),+) => {
        (phf::phf_map!($($x => $y),+), phf::phf_map!($($y => $x),+))
    }
}

/// a map and its inverse of escape-sequence<->char
pub(crate) static ESCAPE_CHARS: (phf::Map<char, char>, phf::Map<char, char>) = bimap! {
    '"' => '"',
    '\\' => '\\',
    '/' => '/',
    'b' => '\u{08}',
    'f' => '\u{0C}',
    'n' => '\n',
    'r' => '\r',
    't' => '\t'
};

/// `escape := ["\\/bfnrt] | 'u{' hex-digit{1, 6} '}'`
fn escape(input: &str) -> IResult<&str, char, KdlParseError<&str>> {
    alt((
        delimited(tag("u{"), unicode, char('}')),
        map_opt(anychar, |c| ESCAPE_CHARS.0.get(&c).copied()),
    ))(input)
}

fn unicode(input: &str) -> IResult<&str, char, KdlParseError<&str>> {
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
fn raw_string(input: &str) -> IResult<&str, &str, KdlParseError<&str>> {
    let (input, _) = char('r')(input)?;
    let (input, hashes) = recognize(many0(char('#')))(input)?;
    let (input, _) = char('"')(input)?;
    let close = format!("\"{}", hashes);
    let (input, string) = take_until(&close[..])(input)?;
    let (input, _) = tag(&close[..])(input)?;
    Ok((input, string))
}

/// `number := decimal | hex | octal | binary`
fn number(input: &str) -> IResult<&str, KdlValue, KdlParseError<&str>> {
    alt((
        map(integer, KdlValue::Int),
        map(hexadecimal, KdlValue::Int),
        map(octal, KdlValue::Int),
        map(binary, KdlValue::Int),
        map(float, KdlValue::Float),
    ))(input)
}

/// ```text
/// decimal := integer ('.' [0-9]+)? exponent?
/// exponent := ('e' | 'E') integer
/// integer := sign? [1-9] [0-9_]*
/// sign := '+' | '-'
/// ```
fn float(input: &str) -> IResult<&str, f64, KdlParseError<&str>> {
    map_res(
        alt((
            recognize(tuple((
                integer,
                opt(preceded(char('.'), integer)),
                one_of("eE"),
                opt(one_of("+-")),
                integer,
            ))),
            recognize(tuple((integer, char('.'), integer))),
        )),
        |x| str::replace(x, "_", "").parse::<f64>(),
    )(input)
}

/// ```text
/// decimal := integer ('.' [0-9]+)? exponent?
/// exponent := ('e' | 'E') integer
/// integer := sign? [1-9] [0-9_]*
/// sign := '+' | '-'
/// ```
fn integer(input: &str) -> IResult<&str, i64, KdlParseError<&str>> {
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
    map_res(
        recognize(many1(terminated(one_of("0123456789"), many0(char('_'))))),
        move |out: &str| {
            i64::from_str_radix(&str::replace(&out, "_", ""), 10).map(move |x| x * mult)
        },
    )(input)
}

/// `hex := '0x' [0-9a-fA-F] [0-9a-fA-F_]*`
fn hexadecimal(input: &str) -> IResult<&str, i64, KdlParseError<&str>> {
    map_res(
        preceded(
            alt((tag("0x"), tag("0X"))),
            recognize(many1(terminated(
                one_of("0123456789abcdefABCDEF"),
                many0(char('_')),
            ))),
        ),
        move |out: &str| i64::from_str_radix(&str::replace(&out, "_", ""), 16),
    )(input)
}

/// `octal := '0o' [0-7] [0-7_]*`
fn octal(input: &str) -> IResult<&str, i64, KdlParseError<&str>> {
    map_res(
        preceded(
            alt((tag("0o"), tag("0O"))),
            recognize(many1(terminated(one_of("01234567"), many0(char('_'))))),
        ),
        move |out: &str| i64::from_str_radix(&str::replace(&out, "_", ""), 8),
    )(input)
}

/// `binary := '0b' ('0' | '1') ('0' | '1' | '_')*`
fn binary(input: &str) -> IResult<&str, i64, KdlParseError<&str>> {
    map_res(
        preceded(
            alt((tag("0b"), tag("0B"))),
            recognize(many1(terminated(one_of("01"), many0(char('_'))))),
        ),
        move |out: &str| i64::from_str_radix(&str::replace(&out, "_", ""), 2),
    )(input)
}

/// `boolean := 'true' | 'false'`
fn boolean(input: &str) -> IResult<&str, KdlValue, KdlParseError<&str>> {
    alt((
        value(KdlValue::Boolean(true), tag("true")),
        value(KdlValue::Boolean(false), tag("false")),
    ))(input)
}

/// `node-space := ws* escline ws* | ws+`
fn node_space(input: &str) -> IResult<&str, (), KdlParseError<&str>> {
    alt((
        delimited(many0(whitespace), escline, many0(whitespace)),
        map(many1(whitespace), |_| ()),
    ))(input)
}

/// `single-line-comment := '//' ('\r' [^\n] | [^\r\n])* (newline | eof)`
fn single_line_comment(input: &str) -> IResult<&str, KdlComment, KdlParseError<&str>> {
    let (input, _) = tag("//")(input)?;
    let (input, _) = many_till(value((), anychar), alt((newline, value((), eof))))(input)?;
    Ok((input, KdlComment::Single))
}

/// `multi-line-comment := '/*' ('*' [^\/] | [^*])* '*/'`
fn multi_line_comment(input: &str) -> IResult<&str, KdlComment, KdlParseError<&str>> {
    delimited(
        tag("/*"),
        value(KdlComment::Multiline, take_until("*/")),
        tag("*/"),
    )(input)
}

/// `escline := '\\' ws* (single-line-comment | newline)`
fn escline(input: &str) -> IResult<&str, (), KdlParseError<&str>> {
    let (input, _) = tag("\\")(input)?;
    let (input, _) = many0(whitespace)(input)?;
    let (input, _) = alt((map(single_line_comment, Option::Some), value(None, newline)))(input)?;
    Ok((input, ()))
}

/// `linespace := newline | ws | single-line-comment`
fn linespace(input: &str) -> IResult<&str, Option<KdlComment>, KdlParseError<&str>> {
    alt((
        value(None, newline),
        value(None, whitespace),
        map(single_line_comment, Option::Some),
    ))(input)
}

/// `ws := bom | unicode-space | multi-line-comment`
fn whitespace(input: &str) -> IResult<&str, (), KdlParseError<&str>> {
    // TODO: bom?
    value(
        (),
        alt((
            tag("\u{FEFF}"),
            unicode_space,
            recognize(multi_line_comment),
        )),
    )(input)
}

fn unicode_space(input: &str) -> IResult<&str, &str, KdlParseError<&str>> {
    alt((
        tag(" "),
        tag("\t"),
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
fn newline(input: &str) -> IResult<&str, (), KdlParseError<&str>> {
    value(
        (),
        alt((
            tag("\r\n"),
            tag("\r"),
            tag("\n"),
            tag("\u{0085}"),
            tag("\u{000C}"),
            tag("\u{2028}"),
            tag("\u{2029}"),
        )),
    )(input)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_nodes() {
        assert_eq!(
            nodes("node"),
            Ok((
                "",
                vec![KdlNode {
                    name: "node".into(),
                    values: Vec::new(),
                    properties: HashMap::new(),
                    children: Vec::new(),
                }]
            ))
        );
        assert_eq!(
            nodes("node\n"),
            Ok((
                "",
                vec![KdlNode {
                    name: "node".into(),
                    values: Vec::new(),
                    properties: HashMap::new(),
                    children: Vec::new(),
                }]
            ))
        );
        assert_eq!(
            nodes("\nnode\n"),
            Ok((
                "",
                vec![KdlNode {
                    name: "node".into(),
                    values: Vec::new(),
                    properties: HashMap::new(),
                    children: Vec::new(),
                }]
            ))
        );
        assert_eq!(
            nodes("node1\nnode2"),
            Ok((
                "",
                vec![
                    KdlNode {
                        name: "node1".into(),
                        values: Vec::new(),
                        properties: HashMap::new(),
                        children: Vec::new(),
                    },
                    KdlNode {
                        name: "node2".into(),
                        values: Vec::new(),
                        properties: HashMap::new(),
                        children: Vec::new(),
                    }
                ]
            ))
        );
    }

    #[test]
    fn test_node() {
        assert_eq!(
            node("node"),
            Ok((
                "",
                Some(KdlNode {
                    name: "node".into(),
                    values: Vec::new(),
                    properties: HashMap::new(),
                    children: Vec::new(),
                })
            ))
        );
        assert_eq!(
            node("node\n"),
            Ok((
                "",
                Some(KdlNode {
                    name: "node".into(),
                    values: Vec::new(),
                    properties: HashMap::new(),
                    children: Vec::new(),
                })
            ))
        );
        assert_eq!(
            node("node;"),
            Ok((
                "",
                Some(KdlNode {
                    name: "node".into(),
                    values: Vec::new(),
                    properties: HashMap::new(),
                    children: Vec::new(),
                })
            ))
        );
        assert_eq!(
            node("node 1"),
            Ok((
                "",
                Some(KdlNode {
                    name: "node".into(),
                    values: vec![KdlValue::Int(1)],
                    properties: HashMap::new(),
                    children: Vec::new(),
                })
            ))
        );
        assert_eq!(
            node("node 1 2 \"3\" true false null"),
            Ok((
                "",
                Some(KdlNode {
                    name: "node".into(),
                    values: vec![
                        KdlValue::Int(1),
                        KdlValue::Int(2),
                        KdlValue::String("3".into()),
                        KdlValue::Boolean(true),
                        KdlValue::Boolean(false),
                        KdlValue::Null
                    ],
                    properties: HashMap::new(),
                    children: Vec::new(),
                })
            ))
        );

        assert_eq!(
            node("node {\n  node2\n}"),
            Ok((
                "",
                Some(KdlNode {
                    name: "node".into(),
                    values: Vec::new(),
                    properties: HashMap::new(),
                    children: vec![KdlNode {
                        name: "node2".into(),
                        values: Vec::new(),
                        properties: HashMap::new(),
                        children: Vec::new()
                    }]
                })
            ))
        );

        assert_eq!(
            node("node { node2; }"),
            Ok((
                "",
                Some(KdlNode {
                    name: "node".into(),
                    values: Vec::new(),
                    properties: HashMap::new(),
                    children: vec![KdlNode {
                        name: "node2".into(),
                        values: Vec::new(),
                        properties: HashMap::new(),
                        children: Vec::new()
                    }]
                })
            ))
        );
    }

    #[test]
    fn test_node_slashdash_comment() {
        assert_eq!(node("/-node"), Ok(("", None)));
        assert_eq!(node("/- node"), Ok(("", None)));
        assert_eq!(node("/- node\n"), Ok(("", None)));
        assert_eq!(node("/-node 1 2 3"), Ok(("", None)));
        assert_eq!(node("/-node key=false"), Ok(("", None)));
        assert_eq!(node("/-node{\nnode\n}"), Ok(("", None)));
        assert_eq!(
            node("/-node 1 2 3 key=\"value\" \\\n{\nnode\n}"),
            Ok(("", None))
        );
    }

    #[test]
    fn test_arg_slashdash_comment() {
        assert_eq!(
            node("node /-1"),
            Ok((
                "",
                Some(KdlNode {
                    name: "node".into(),
                    values: Vec::new(),
                    properties: HashMap::new(),
                    children: Vec::new(),
                })
            ))
        );
        assert_eq!(
            node("node /-1 2"),
            Ok((
                "",
                Some(KdlNode {
                    name: "node".into(),
                    values: vec![KdlValue::Int(2)],
                    properties: HashMap::new(),
                    children: Vec::new(),
                })
            ))
        );
        assert_eq!(
            node("node 1 /- 2 3"),
            Ok((
                "",
                Some(KdlNode {
                    name: "node".into(),
                    values: vec![KdlValue::Int(1), KdlValue::Int(3)],
                    properties: HashMap::new(),
                    children: Vec::new(),
                })
            ))
        );
        assert_eq!(
            node("node /--1"),
            Ok((
                "",
                Some(KdlNode {
                    name: "node".into(),
                    values: Vec::new(),
                    properties: HashMap::new(),
                    children: Vec::new(),
                })
            ))
        );
        assert_eq!(
            node("node /- -1"),
            Ok((
                "",
                Some(KdlNode {
                    name: "node".into(),
                    values: Vec::new(),
                    properties: HashMap::new(),
                    children: Vec::new(),
                })
            ))
        );
        assert_eq!(
            node("node \\\n/- -1"),
            Ok((
                "",
                Some(KdlNode {
                    name: "node".into(),
                    values: Vec::new(),
                    properties: HashMap::new(),
                    children: Vec::new(),
                })
            ))
        );
    }

    #[test]
    fn test_prop_slashdash_comment() {
        let mut properties = HashMap::new();
        properties.insert("key".to_owned(), KdlValue::Int(1));
        assert_eq!(
            node("node /-key=1"),
            Ok((
                "",
                Some(KdlNode {
                    name: "node".into(),
                    values: Vec::new(),
                    properties: HashMap::new(),
                    children: Vec::new(),
                })
            ))
        );
        assert_eq!(
            node("node /- key=1"),
            Ok((
                "",
                Some(KdlNode {
                    name: "node".into(),
                    values: Vec::new(),
                    properties: HashMap::new(),
                    children: Vec::new(),
                })
            ))
        );
        assert_eq!(
            node("node key=1 /-key2=2"),
            Ok((
                "",
                Some(KdlNode {
                    name: "node".into(),
                    values: Vec::new(),
                    properties,
                    children: Vec::new(),
                })
            ))
        );
    }

    #[test]
    fn test_children_slashdash_comment() {
        assert_eq!(
            node("node /-{}"),
            Ok((
                "",
                Some(KdlNode {
                    name: "node".into(),
                    values: Vec::new(),
                    properties: HashMap::new(),
                    children: Vec::new(),
                })
            ))
        );
        assert_eq!(
            node("node /- {}"),
            Ok((
                "",
                Some(KdlNode {
                    name: "node".into(),
                    values: Vec::new(),
                    properties: HashMap::new(),
                    children: Vec::new(),
                })
            ))
        );
        assert_eq!(
            node("node /-{\nnode2\n}"),
            Ok((
                "",
                Some(KdlNode {
                    name: "node".into(),
                    values: Vec::new(),
                    properties: HashMap::new(),
                    children: Vec::new(),
                })
            ))
        );
    }

    #[test]
    fn test_string() {
        assert_eq!(string("\"\""), Ok(("", "".into())));
        assert_eq!(string("\"hello\""), Ok(("", "hello".into())));
        assert_eq!(string("\"hello\nworld\""), Ok(("", "hello\nworld".into())));
        assert_eq!(string("\"\u{10FFF}\""), Ok(("", "\u{10FFF}".into())));
        assert_eq!(
            string(r#""\"\\\/\b\f\n\r\t""#),
            Ok(("", "\"\\/\u{08}\u{0C}\n\r\t".into()))
        );
        assert_eq!(string(r#""\u{10}""#), Ok(("", "\u{10}".into())));
        assert!(string(r#""\i""#).is_err());
        assert!(string(r#""\u{c0ffee}""#).is_err());
    }

    #[test]
    fn test_float() {
        assert_eq!(float("1.0"), Ok(("", 1.0f64)));
        assert_eq!(float("0.0"), Ok(("", 0.0f64)));
        assert_eq!(float("-1.0"), Ok(("", -1.0f64)));
        assert_eq!(float("+1.0"), Ok(("", 1.0f64)));
        assert_eq!(float("1.0e10"), Ok(("", 1.0e10f64)));
        assert_eq!(float("1.0e-10"), Ok(("", 1.0e-10f64)));
        assert_eq!(float("-1.0e-10"), Ok(("", -1.0e-10f64)));
        assert_eq!(float("123_456_789.0"), Ok(("", 123456789.0f64)));
        assert_eq!(float("123_456_789.0_"), Ok(("", 123456789.0f64)));
        assert!(float("?1.0").is_err());
        assert!(float("_1.0").is_err());
        assert!(float("1._0").is_err());
        assert!(float("1.").is_err());
        assert!(float(".0").is_err());
    }

    #[test]
    fn test_integer() {
        assert_eq!(integer("0"), Ok(("", 0)));
        assert_eq!(integer("0123456789"), Ok(("", 123456789)));
        assert_eq!(integer("0123_456_789"), Ok(("", 123456789)));
        assert_eq!(integer("0123_456_789_"), Ok(("", 123456789)));
        assert_eq!(integer("+0123456789"), Ok(("", 123456789)));
        assert_eq!(integer("-0123456789"), Ok(("", -123456789)));
        assert!(integer("?0123456789").is_err());
        assert!(integer("_0123456789").is_err());
        assert!(integer("a").is_err());
        assert!(integer("--").is_err());
    }

    #[test]
    fn test_hexadecimal() {
        assert_eq!(
            hexadecimal("0x0123456789abcdef"),
            Ok(("", 0x0123456789abcdef))
        );
        assert_eq!(
            hexadecimal("0x01234567_89abcdef"),
            Ok(("", 0x0123456789abcdef))
        );
        assert_eq!(
            hexadecimal("0x01234567_89abcdef_"),
            Ok(("", 0x0123456789abcdef))
        );
        assert!(hexadecimal("0x_123").is_err());
        assert!(hexadecimal("0xg").is_err());
        assert!(hexadecimal("0xx").is_err());
    }

    #[test]
    fn test_octal() {
        assert_eq!(octal("0o01234567"), Ok(("", 0o01234567)));
        assert_eq!(octal("0o0123_4567"), Ok(("", 0o01234567)));
        assert_eq!(octal("0o01234567_"), Ok(("", 0o01234567)));
        assert!(octal("0o_123").is_err());
        assert!(octal("0o8").is_err());
        assert!(octal("0oo").is_err());
    }

    #[test]
    fn test_binary() {
        assert_eq!(binary("0b0101"), Ok(("", 0b0101)));
        assert_eq!(binary("0b01_10"), Ok(("", 0b0110)));
        assert_eq!(binary("0b01___10"), Ok(("", 0b0110)));
        assert_eq!(binary("0b0110_"), Ok(("", 0b0110)));
        assert!(binary("0b_0110").is_err());
        assert!(binary("0b20").is_err());
        assert!(binary("0bb").is_err());
    }

    #[test]
    fn test_raw_string() {
        assert_eq!(raw_string(r#"r"foo""#), Ok(("", "foo")));
        assert_eq!(raw_string("r\"foo\nbar\""), Ok(("", "foo\nbar")));
        assert_eq!(raw_string(r##"r#"foo"#"##), Ok(("", "foo")));
        assert_eq!(raw_string(r###"r##"foo"##"###), Ok(("", "foo")));
        assert_eq!(raw_string(r#"r"\nfoo\r""#), Ok(("", r"\nfoo\r")));
        assert!(raw_string(r###"r##"foo"#"###).is_err());
    }

    #[test]
    fn test_boolean() {
        assert_eq!(boolean("true"), Ok(("", KdlValue::Boolean(true))));
        assert_eq!(boolean("false"), Ok(("", KdlValue::Boolean(false))));
        assert!(boolean("blah").is_err());
    }

    #[test]
    fn test_node_space() {
        assert_eq!(node_space(" "), Ok(("", ())));
        assert_eq!(node_space("\t "), Ok(("", ())));
        assert_eq!(node_space("\t \\ // hello\n "), Ok(("", ())));
        assert!(node_space("blah").is_err());
    }

    #[test]
    fn test_single_line_comment() {
        assert_eq!(single_line_comment("//hello"), Ok(("", KdlComment::Single)));
        assert_eq!(
            single_line_comment("// \thello"),
            Ok(("", KdlComment::Single))
        );
        assert_eq!(
            single_line_comment("//hello\n"),
            Ok(("", KdlComment::Single))
        );
        assert_eq!(
            single_line_comment("//hello\r\n"),
            Ok(("", KdlComment::Single))
        );
        assert_eq!(
            single_line_comment("//hello\n\r"),
            Ok(("\r", KdlComment::Single))
        );
        assert_eq!(
            single_line_comment("//hello\rworld"),
            Ok(("world", KdlComment::Single))
        );
        assert_eq!(
            single_line_comment("//hello\nworld\r\n"),
            Ok(("world\r\n", KdlComment::Single))
        );
    }

    #[test]
    fn test_multi_line_comment() {
        assert_eq!(
            multi_line_comment("/*hello*/"),
            Ok(("", KdlComment::Multiline))
        );
        assert_eq!(
            multi_line_comment("/*hello*/\n"),
            Ok(("\n", KdlComment::Multiline))
        );
        assert_eq!(
            multi_line_comment("/*\nhello\r\n*/"),
            Ok(("", KdlComment::Multiline))
        );
        assert_eq!(
            multi_line_comment("/*\nhello** /\n*/"),
            Ok(("", KdlComment::Multiline))
        );
        assert_eq!(
            multi_line_comment("/**\nhello** /\n*/"),
            Ok(("", KdlComment::Multiline))
        );
        assert_eq!(
            multi_line_comment("/*hello*/world"),
            Ok(("world", KdlComment::Multiline))
        );
    }

    #[test]
    fn test_escline() {
        assert_eq!(escline("\\\nfoo"), Ok(("foo", ())));
        assert_eq!(escline("\\\n  foo"), Ok(("  foo", ())));
        assert_eq!(escline("\\  \t \nfoo"), Ok(("foo", ())));
        assert_eq!(escline("\\ // test \nfoo"), Ok(("foo", ())));
        assert_eq!(escline("\\ // test \n  foo"), Ok(("  foo", ())));
    }

    #[test]
    fn test_whitespace() {
        assert_eq!(whitespace(" "), Ok(("", ())));
        assert_eq!(whitespace("\t"), Ok(("", ())));
        assert_eq!(whitespace("/* \nfoo\r\n */ etc"), Ok((" etc", ())));
        assert!(whitespace("hi").is_err())
    }

    #[test]
    fn test_newline() {
        assert_eq!(newline("\n"), Ok(("", ())));
        assert_eq!(newline("\r"), Ok(("", ())));
        assert_eq!(newline("\r\n"), Ok(("", ())));
        assert_eq!(newline("\n\n"), Ok(("\n", ())));
        assert!(newline("blah").is_err());
    }

    #[test]
    fn test_count_leading_lines() {
        assert_eq!(count_leading_lines(""), ("", 0));
        assert_eq!(count_leading_lines("foo"), ("foo", 0));
        assert_eq!(count_leading_lines("foo\n"), ("foo\n", 0));
        assert_eq!(count_leading_lines("foo\nbar"), ("bar", 1));
        assert_eq!(count_leading_lines("foo\nbar\n"), ("bar\n", 1));
        assert_eq!(count_leading_lines("\nfoo\n\nbar\n"), ("bar\n", 3));
        assert_eq!(count_leading_lines("foo\r\nbar\r\n"), ("bar\r\n", 1));
        assert_eq!(count_leading_lines("foo\nbar\rbaz"), ("baz", 2));
        assert_eq!(count_leading_lines("foo\nbar\n\n"), ("\n", 2));

        assert_eq!(
            count_leading_lines(
                r#"// This example is a GitHub Action if it used KDL syntax.
// See .github/workflows/ci.yml for the file this was based on.
name "CI"

on "push" "pull_request"

env {
  RUSTFLAGS "-Dwarnings"
"#
            ),
            ("  RUSTFLAGS \"-Dwarnings\"\n", 7)
        );
    }

    #[test]
    fn test_strip_trailing_newline() {
        assert_eq!(strip_trailing_newline(""), "");
        assert_eq!(strip_trailing_newline("foo"), "foo");
        assert_eq!(strip_trailing_newline("foo\n"), "foo");
        assert_eq!(strip_trailing_newline("foo\n\n"), "foo\n");
        assert_eq!(strip_trailing_newline("foo\nbar"), "foo\nbar");
        assert_eq!(strip_trailing_newline("foo\nbar\n"), "foo\nbar");
        assert_eq!(strip_trailing_newline("foo\r\n"), "foo");
        assert_eq!(strip_trailing_newline("\n"), "");
        assert_eq!(strip_trailing_newline("foo\r\n\r"), "foo\r\n");
        assert_eq!(strip_trailing_newline("foo\nx"), "foo\nx");
    }
}
