use crate::nom_compat::many0;
use crate::query::{
    KdlQuery, KdlQueryAttributeOp, KdlQueryMatcher, KdlQueryMatcherAccessor,
    KdlQueryMatcherDetails, KdlQuerySelector, KdlQuerySelectorSegment, KdlSegmentCombinator,
};
use crate::v1_parser::{value, KdlParser};
use crate::{KdlDiagnostic, KdlErrorKind, KdlParseError, KdlValue};
use miette::Severity;
use nom::branch::alt;
use nom::bytes::complete::tag;
use nom::combinator::{all_consuming, cut, map, opt, recognize};
use nom::error::context;
use nom::multi::separated_list1;
use nom::sequence::{delimited, preceded, terminated};
use nom::{Finish, IResult, Offset, Parser};

pub(crate) struct KdlQueryParser<'a>(KdlParser<'a>);

impl<'a> KdlQueryParser<'a> {
    pub(crate) fn new(full_input: &'a str) -> Self {
        Self(KdlParser::new(full_input))
    }

    pub(crate) fn parse<T, P>(&self, parser: P) -> Result<T, KdlDiagnostic>
    where
        P: Parser<&'a str, T, KdlParseError<&'a str>>,
    {
        all_consuming(parser)(self.0.full_input)
            .finish()
            .map(|(_, arg)| arg)
            .map_err(|e| {
                let span_substr = &e.input[..e.len];
                KdlDiagnostic {
                    input: self.0.full_input.into(),
                    span: self.0.span_from_substr(span_substr),
                    help: if let Some(help) = e.help {
                        Some(help)
                    } else if e.kind.is_none() && e.context.is_none() {
                        Some("The general syntax for queries is '(type)nodename[prop=value], anothernode, etc'. For more details, please see https://github.com/kdl-org/kdl/blob/main/QUERY-SPEC.md")
                    } else {
                        None
                    },
                    label: e.label,
                    kind: if let Some(kind) = e.kind {
                        kind
                    } else if let Some(ctx) = e.context {
                        KdlErrorKind::Context(ctx)
                    } else {
                        KdlErrorKind::Context("a valid KQL query")
                    },
                    severity: Severity::Error,
                }
            })
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

pub(crate) fn query<'a: 'b, 'b>(
    kdl_parser: &'b KdlQueryParser<'a>,
) -> impl FnMut(&'a str) -> IResult<&'a str, KdlQuery, KdlParseError<&'a str>> + 'b {
    map(
        separated_list1(
            delimited(whitespace, tag(","), whitespace),
            query_selector(kdl_parser),
        ),
        KdlQuery,
    )
}

fn query_selector<'a: 'b, 'b>(
    kdl_parser: &'b KdlQueryParser<'a>,
) -> impl Fn(&'a str) -> IResult<&'a str, KdlQuerySelector, KdlParseError<&'a str>> + 'b {
    move |input| {
        let mut segments = Vec::new();
        let mut is_scope = true;
        let mut input = input;
        loop {
            let (inp, _) = whitespace(input)?;
            input = inp;
            let (inp, matchers) = node_matchers(kdl_parser, is_scope)(input)?;
            input = inp;
            let (inp, _) = whitespace(input)?;
            input = inp;
            let (inp, op) = opt(segment_combinator)(input)?;
            input = inp;
            let is_last = op.is_none();
            segments.push(KdlQuerySelectorSegment {
                op,
                matcher: KdlQueryMatcher(matchers),
            });
            if is_last {
                break;
            }
            is_scope = false;
        }
        let (input, _) = whitespace(input)?;
        Ok((input, KdlQuerySelector(segments)))
    }
}

fn segment_combinator(input: &str) -> IResult<&str, KdlSegmentCombinator, KdlParseError<&str>> {
    alt((
        map(tag(">>"), |_| KdlSegmentCombinator::Descendant),
        map(tag(">"), |_| KdlSegmentCombinator::Child),
        map(tag("++"), |_| KdlSegmentCombinator::Sibling),
        map(tag("+"), |_| KdlSegmentCombinator::Neighbor),
    ))(input)
}

fn node_matchers<'a: 'b, 'b>(
    kdl_parser: &'b KdlQueryParser<'a>,
    is_scope: bool,
) -> impl Fn(&'a str) -> IResult<&'a str, Vec<KdlQueryMatcherDetails>, KdlParseError<&'a str>> + 'b
{
    move |input| {
        let mut matchers = Vec::new();

        let (input, _) = whitespace(input)?;

        let start = input;
        let (input, scope) = opt(scope_accessor)(input)?;
        if let Some(xsr) = scope {
            if is_scope {
                matchers.push(KdlQueryMatcherDetails {
                    op: KdlQueryAttributeOp::Equal,
                    accessor: xsr,
                    value: None,
                });
                return Ok((input, matchers));
            } else {
                return Err(nom::Err::Error(KdlParseError {
                    input: start,
                    len: start.len() - input.len(),
                    kind: None,
                    label: Some("scope()"),
                    help: Some("Make sure scope() precedes any other items within a (comma-separated) selector."),
                    touched: false,
                    context: Some("scope() to be the first item in this selector"),
                }));
            }
        }

        let (input, details) = opt(annotation_matcher(kdl_parser))(input)?;
        if let Some(details) = details {
            matchers.push(details);
            let start = input;
            let (input, typed) = opt(annotation_matcher(kdl_parser))(input)?;
            if typed.is_some() {
                return Err(nom::Err::Error(KdlParseError {
                    input: start,
                    len: start.len() - input.len(),
                    kind: None,
                    label: Some("type annotation"),
                    help: Some("The syntax for node selectors is (type)node[attribute=value]."),
                    touched: false,
                    context: Some("only one type annotation per selector"),
                }));
            }
        }

        let (input, node) = opt(crate::v1_parser::identifier(&kdl_parser.0))(input)?;
        if let Some(node) = node {
            matchers.push(KdlQueryMatcherDetails {
                op: KdlQueryAttributeOp::Equal,
                value: Some(KdlValue::String(node.value().to_owned())),
                accessor: KdlQueryMatcherAccessor::Node,
            });
        }

        let start = input;
        let (input, typed) = opt(annotation_matcher(kdl_parser))(input)?;
        if typed.is_some() {
            return Err(nom::Err::Error(KdlParseError {
                input: start,
                len: start.len() - input.len(),
                kind: None,
                label: Some("type annotation"),
                help: Some("The syntax for node selectors is (type)node[attribute=value]."),
                touched: false,
                context: Some("type annotation to not be used after a node name"),
            }));
        }

        let start = input;
        let (input, mut attribute_matchers) = many0(attribute_matcher(kdl_parser))(input)?;
        matchers.append(&mut attribute_matchers);

        if matchers.is_empty() {
            Err(nom::Err::Error(KdlParseError {
                input: start,
                len: 0,
                kind: None,
                label: Some("node matcher"),
                help: Some("node matcher must not be empty"),
                touched: false,
                context: Some("a valid node matcher"),
            }))
        } else {
            // Check for trailing type annotations.
            let start = input;
            let (end, typed) = opt(annotation_matcher(kdl_parser))(input)?;
            if typed.is_some() {
                return Err(nom::Err::Error(KdlParseError {
                    input: start,
                    len: start.len() - end.len(),
                    kind: None,
                    label: Some("type annotation"),
                    help: Some("The syntax for node selectors is (type)node[attribute=value]."),
                    touched: false,
                    context: Some("type annotation to come before attribute matcher(s)"),
                }));
            }

            // Check for trailing node name matcher.
            let (end, ident) = opt(crate::v1_parser::identifier(&kdl_parser.0))(input)?;
            if ident.is_some() {
                return Err(nom::Err::Error(KdlParseError {
                    input: start,
                    len: start.len() - end.len(),
                    kind: None,
                    label: Some("node name"),
                    help: Some("The syntax for node selectors is (type)node[attribute=value]."),
                    touched: false,
                    context: Some("node name to come before attribute matcher(s)"),
                }));
            }

            Ok((input, matchers))
        }
    }
}

fn attribute_matcher<'a: 'b, 'b>(
    kdl_parser: &'b KdlQueryParser<'a>,
) -> impl Fn(&'a str) -> IResult<&'a str, KdlQueryMatcherDetails, KdlParseError<&'a str>> + 'b {
    move |input| {
        let start = input;
        let (input, _) = tag("[")(input)?;
        let (input, _) = whitespace(input)?;
        let (input, matcher) = attribute_matcher_inner(kdl_parser)(input)?;
        let (input, _) = whitespace(input)?;
        let (input, _) = context("a closing ']' for this attribute matcher", cut(tag("]")))(input)
            .map_err(|e| set_details(e, start, Some("partial attribute matcher"), None))?;

        Ok((input, matcher))
    }
}

fn attribute_matcher_inner<'a: 'b, 'b>(
    kdl_parser: &'b KdlQueryParser<'a>,
) -> impl Fn(&'a str) -> IResult<&'a str, KdlQueryMatcherDetails, KdlParseError<&'a str>> + 'b {
    move |input| {
        let (input, xsr) = opt(accessor(kdl_parser))(input)?;
        if let Some(xsr) = xsr {
            let (input, _) = whitespace(input)?;
            let (input, op) = opt(attribute_op)(input)?;
            let (input, _) = whitespace(input)?;
            if let Some(op) = op {
                let prev = input;
                let (input, val) = opt(crate::v1_parser::value)(input)?;
                // Make sure it's a syntax error to try and use string
                // operators with non-string arguments.
                if let Some((_, value)) = val {
                    if matches!(
                        op,
                        KdlQueryAttributeOp::StartsWith
                            | KdlQueryAttributeOp::EndsWith
                            | KdlQueryAttributeOp::Contains
                    ) {
                        if value.is_string() {
                            Ok((
                                input,
                                KdlQueryMatcherDetails {
                                    op,
                                    value: Some(value),
                                    accessor: xsr,
                                },
                            ))
                        } else {
                            Err(nom::Err::Failure(KdlParseError {
                                input: prev,
                                len: prev.len() - input.len(),
                                kind: None,
                                label: Some("non-string operator value"),
                                help: Some("Only strings can be used as arguments for string-related operators (*=, ^=, $=)."),
                                touched: false,
                                context: Some("a string as an operator value"),
                            }))
                        }
                    } else {
                        Ok((
                            input,
                            KdlQueryMatcherDetails {
                                op,
                                value: Some(value),
                                accessor: xsr,
                            },
                        ))
                    }
                } else {
                    Err(nom::Err::Failure(KdlParseError {
                        input: prev,
                        len: 0,
                        kind: None,
                        label: Some("operator value"),
                        help: Some("Only valid KDL values can be used on the right hand side of attribute matcher operators."),
                        touched: false,
                        context: Some("a valid operator argument"),
                    }))
                }
            } else {
                Ok((
                    input,
                    KdlQueryMatcherDetails {
                        op: KdlQueryAttributeOp::Equal,
                        value: None,
                        accessor: xsr,
                    },
                ))
            }
        } else {
            Ok((
                input,
                KdlQueryMatcherDetails {
                    op: KdlQueryAttributeOp::Equal,
                    value: None,
                    accessor: KdlQueryMatcherAccessor::Node,
                },
            ))
        }
    }
}

fn attribute_op(input: &str) -> IResult<&str, KdlQueryAttributeOp, KdlParseError<&str>> {
    alt((
        map(tag("="), |_| KdlQueryAttributeOp::Equal),
        map(tag("!="), |_| KdlQueryAttributeOp::NotEqual),
        map(tag(">"), |_| KdlQueryAttributeOp::Gt),
        map(tag(">="), |_| KdlQueryAttributeOp::Gte),
        map(tag("<"), |_| KdlQueryAttributeOp::Lt),
        map(tag("<="), |_| KdlQueryAttributeOp::Lte),
        map(tag("^="), |_| KdlQueryAttributeOp::StartsWith),
        map(tag("$="), |_| KdlQueryAttributeOp::EndsWith),
        map(tag("*="), |_| KdlQueryAttributeOp::Contains),
    ))(input)
}

fn annotation_matcher<'a: 'b, 'b>(
    kdl_parser: &'b KdlQueryParser<'a>,
) -> impl Fn(&'a str) -> IResult<&'a str, KdlQueryMatcherDetails, KdlParseError<&'a str>> + 'b {
    move |input| {
        let start = input;
        let (input, _) = tag("(")(input)?;
        let (input, _) = whitespace(input)?;
        let (input, ty) = opt(crate::v1_parser::identifier(&kdl_parser.0))(input)?;
        let (input, _) = context("closing ')' for type annotation", cut(tag(")")))(input)
            .map_err(|e| set_details(e, start, Some("annotation"), Some("annotations can only be KDL identifiers (including string identifiers), and can't have any space inside the parentheses.")))?;
        Ok((
            input,
            KdlQueryMatcherDetails {
                op: KdlQueryAttributeOp::Equal,
                value: ty.map(|ident| KdlValue::String(ident.value().to_owned())),
                accessor: KdlQueryMatcherAccessor::Annotation,
            },
        ))
    }
}

fn scope_accessor(input: &str) -> IResult<&str, KdlQueryMatcherAccessor, KdlParseError<&str>> {
    let start = input;
    let (input, _) = tag("scope(")(input)?;
    let (input, _) = context(
        "a valid scope accessor",
        cut(preceded(whitespace, tag(")"))),
    )(input)
    .map_err(|e| set_details(e, start, Some("partial scope accessor"), None))?;
    Ok((input, KdlQueryMatcherAccessor::Scope))
}

fn accessor<'a: 'b, 'b>(
    kdl_parser: &'b KdlQueryParser<'a>,
) -> impl Fn(&'a str) -> IResult<&'a str, KdlQueryMatcherAccessor, KdlParseError<&'a str>> + 'b {
    move |input| {
        let (input, accessor) = alt((
            type_accessor,
            arg_accessor,
            prop_accessor(kdl_parser),
            prop_name_accessor(kdl_parser),
            bad_accessor(kdl_parser),
        ))(input)?;

        Ok((input, accessor))
    }
}

fn type_accessor(input: &str) -> IResult<&str, KdlQueryMatcherAccessor, KdlParseError<&str>> {
    let start = input;
    let (input, _) = tag("type")(input)?;
    let (input, _) = context(
        "an opening '(' for a 'type()' accessor",
        preceded(whitespace, tag("(")),
    )(input)
    .map_err(|e| set_details(e, start, Some("partial type accessor"), None))?;
    let (input, _) = context(
        "a closing ')' for this 'type()' accessor",
        cut(preceded(whitespace, tag(")"))),
    )(input)
    .map_err(|e| {
        set_details(
            e,
            start,
            Some("partial type accessor"),
            Some("type() accessors don't take any arguments. Use e.g. [type() = \"foo\"] instead."),
        )
    })?;
    Ok((input, KdlQueryMatcherAccessor::Annotation))
}

fn arg_accessor(input: &str) -> IResult<&str, KdlQueryMatcherAccessor, KdlParseError<&str>> {
    let (input, _) = tag("arg")(input)?;
    let (input, arg) = parenthesized_arg(input)?;
    if let Some(arg) = arg {
        if let Some(index) = arg
            .as_i64()
            .and_then(|arg| -> Option<usize> { arg.try_into().ok() })
        {
            Ok((input, KdlQueryMatcherAccessor::Arg(Some(index))))
        } else {
            Err(nom::Err::Error(KdlParseError {
                input,
                len: 0,
                kind: None,
                label: Some("arg accessor"),
                help: Some("arg accessor must be an integer"),
                touched: false,
                context: Some("a valid arg accessor"),
            }))
        }
    } else {
        Ok((input, KdlQueryMatcherAccessor::Arg(None)))
    }
}

fn prop_name_accessor<'a: 'b, 'b>(
    kdl_parser: &'b KdlQueryParser<'a>,
) -> impl Fn(&'a str) -> IResult<&'a str, KdlQueryMatcherAccessor, KdlParseError<&'a str>> + 'b {
    move |input| {
        let start = input;
        let (input, prop_name) = crate::v1_parser::identifier(&kdl_parser.0)(input)?;
        let (_, paren) = opt(preceded(whitespace, tag("(")))(input)?;
        if paren.is_some() {
            Err(nom::Err::Error(KdlParseError {
                input: start,
                len: 0,
                kind: None,
                label: Some("accessor"),
                help: Some("accessor must be one of: type(), arg(), prop(), propname"),
                touched: false,
                context: Some("a valid accessor"),
            }))
        } else {
            Ok((
                input,
                KdlQueryMatcherAccessor::Prop(prop_name.value().to_owned()),
            ))
        }
    }
}

fn prop_accessor<'a: 'b, 'b>(
    kdl_parser: &'b KdlQueryParser<'a>,
) -> impl Fn(&'a str) -> IResult<&'a str, KdlQueryMatcherAccessor, KdlParseError<&'a str>> + 'b {
    move |input| {
        let (input, _) = tag("prop")(input)?;
        let (input, val) = parenthesized_prop(kdl_parser)(input)?;
        Ok((input, KdlQueryMatcherAccessor::Prop(val)))
    }
}

fn parenthesized_arg(input: &str) -> IResult<&str, Option<KdlValue>, KdlParseError<&str>> {
    let (input, _) = tag("(")(input)?;
    let (input, maybe_value) = opt(value)(input)?;
    let (input, _) = tag(")")(input)?;

    if let Some((_, val)) = maybe_value {
        Ok((input, Some(val)))
    } else {
        Ok((input, None))
    }
}

fn parenthesized_prop<'a: 'b, 'b>(
    kdl_parser: &'b KdlQueryParser<'a>,
) -> impl Fn(&'a str) -> IResult<&'a str, String, KdlParseError<&'a str>> + 'b {
    move |input| {
        let (input, _) = tag("(")(input)?;
        let (input, prop) = crate::v1_parser::identifier(&kdl_parser.0)(input)?;
        let (input, _) = tag(")")(input)?;
        Ok((input, prop.value().to_owned()))
    }
}

fn bad_accessor<'a: 'b, 'b>(
    kdl_parser: &'b KdlQueryParser<'a>,
) -> impl Fn(&'a str) -> IResult<&'a str, KdlQueryMatcherAccessor, KdlParseError<&'a str>> + 'b {
    move |input| {
        let start = input;

        let (input, scope) = opt(preceded(
            tag("scope"),
            preceded(
                whitespace,
                opt(terminated(tag("("), opt(preceded(whitespace, tag(")"))))),
            ),
        ))(input)?;

        if scope.is_some() {
            return Err(nom::Err::Failure(KdlParseError {
                input: start,
                len: start.len() - input.len(),
                kind: None,
                label: Some("incorrect scope() accessor"),
                help: Some("Accessors must be one of: type(), arg(), prop(), propname"),
                touched: false,
                context: Some(
                    "'scope()' to be the first item only at the top level of the query selector",
                ),
            }));
        }

        let (input, ident) = opt(terminated(
            crate::v1_parser::identifier(&kdl_parser.0),
            preceded(
                whitespace,
                terminated(tag("("), opt(preceded(whitespace, tag(")")))),
            ),
        ))(input)?;

        if let Some(ident) = ident {
            match ident.value() {
                "type" | "arg" | "prop" | "val" => {}
                _ => {
                    return Err(nom::Err::Failure(KdlParseError {
                        input: start,
                        len: start.len() - input.len(),
                        kind: None,
                        label: Some("invalid attribute accessor"),
                        help: Some("Accessors must be one of: type(), arg(), prop(), propname"),
                        touched: false,
                        context: Some("a valid attribute accessor"),
                    }));
                }
            }
        }

        Err(nom::Err::Error(KdlParseError {
            input: start,
            len: 0,
            kind: None,
            label: Some("accessor"),
            help: Some("accessor must be one of: type(), arg(), prop(), propname"),
            touched: false,
            context: Some("a valid accessor"),
        }))
    }
}

fn whitespace(input: &str) -> IResult<&str, &str, KdlParseError<&str>> {
    recognize(many0(alt((
        crate::v1_parser::unicode_space,
        crate::v1_parser::newline,
    ))))(input)
}
