use crate::nom_compat::many0;
use crate::parser::{value, KdlParser};
use crate::query::{
    KdlQuery, KdlQueryAttributeOp, KdlQueryMatcher, KdlQueryMatcherAccessor,
    KdlQueryMatcherDetails, KdlQuerySelector, KdlQuerySelectorSegment, KdlSegmentCombinator,
};
use crate::{KdlError, KdlErrorKind, KdlParseError, KdlValue};
use nom::branch::alt;
use nom::bytes::complete::tag;
use nom::combinator::{all_consuming, map, opt, recognize};
use nom::multi::separated_list1;
use nom::sequence::delimited;
use nom::{Finish, IResult, Parser};

pub(crate) struct KdlQueryParser<'a>(KdlParser<'a>);

impl<'a> KdlQueryParser<'a> {
    pub(crate) fn new(full_input: &'a str) -> Self {
        Self(KdlParser::new(full_input))
    }

    pub(crate) fn parse<T, P>(&self, parser: P) -> Result<T, KdlError>
    where
        P: Parser<&'a str, T, KdlParseError<&'a str>>,
    {
        all_consuming(parser)(self.0.full_input)
            .finish()
            .map(|(_, arg)| arg)
            .map_err(|e| {
                let span_substr = &e.input[..e.len];
                KdlError {
                    input: self.0.full_input.into(),
                    span: self.0.span_from_substr(span_substr),
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

        let (input, scope) = opt(tag("scope()"))(input)?;
        if scope.is_some() {
            if is_scope {
                matchers.push(KdlQueryMatcherDetails {
                    op: KdlQueryAttributeOp::Equal,
                    accessor: KdlQueryMatcherAccessor::Scope,
                    value: None,
                });
                return Ok((input, matchers));
            } else {
                return Err(nom::Err::Error(KdlParseError {
                    input,
                    len: 0,
                    kind: None,
                    label: Some("scope()"),
                    help: Some("scope() can only be used as the first item in a query."),
                    touched: false,
                    context: Some("valid query"),
                }));
            }
        }

        let (mut input, typed) = opt(delimited(tag("("), whitespace, tag(")")))(input)?;
        if typed.is_some() {
            matchers.push(KdlQueryMatcherDetails {
                op: KdlQueryAttributeOp::Equal,
                value: None,
                accessor: KdlQueryMatcherAccessor::Annotation,
            });
        } else {
            let (inp, ty) = opt(crate::parser::annotation(&kdl_parser.0))(input)?;
            input = inp;
            if let Some(tag) = ty {
                matchers.push(KdlQueryMatcherDetails {
                    op: KdlQueryAttributeOp::Equal,
                    value: Some(KdlValue::String(tag.value().to_owned())),
                    accessor: KdlQueryMatcherAccessor::Annotation,
                });
            }
        }

        let (input, node) = opt(crate::parser::identifier(&kdl_parser.0))(input)?;
        if let Some(node) = node {
            matchers.push(KdlQueryMatcherDetails {
                op: KdlQueryAttributeOp::Equal,
                value: Some(KdlValue::String(node.value().to_owned())),
                accessor: KdlQueryMatcherAccessor::Node,
            });
        }

        let (input, mut attribute_matchers) = many0(attribute_matcher(kdl_parser))(input)?;
        matchers.append(&mut attribute_matchers);

        if matchers.is_empty() {
            Err(nom::Err::Error(KdlParseError {
                input,
                len: 0,
                kind: None,
                label: Some("node matcher"),
                help: Some("node matcher must not be empty"),
                touched: false,
                context: Some("a valid node matcher"),
            }))
        } else {
            Ok((input, matchers))
        }
    }
}

fn attribute_matcher<'a: 'b, 'b>(
    kdl_parser: &'b KdlQueryParser<'a>,
) -> impl Fn(&'a str) -> IResult<&'a str, KdlQueryMatcherDetails, KdlParseError<&'a str>> + 'b {
    move |input| {
        let (input, _) = tag("[")(input)?;
        let (input, _) = whitespace(input)?;
        let (input, matcher) = attribute_matcher_inner(kdl_parser)(input)?;
        let (input, _) = whitespace(input)?;
        let (input, _) = tag("]")(input)?;

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
                let (input, (_, value)) = crate::parser::value(input)?;
                // Make sure it's a syntax error to try and use string
                // operators with non-string arguments.
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
                        // TODO: underline bad value
                        Err(nom::Err::Error(KdlParseError {
                            input,
                            len: 0,
                            kind: None,
                            label: Some("operator value"),
                            help: Some("string operator value must be a string"),
                            touched: false,
                            context: Some("a valid operator value"),
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

fn accessor<'a: 'b, 'b>(
    kdl_parser: &'b KdlQueryParser<'a>,
) -> impl Fn(&'a str) -> IResult<&'a str, KdlQueryMatcherAccessor, KdlParseError<&'a str>> + 'b {
    move |input| {
        let (input, accessor) = alt((
            map(tag("type()"), |_| KdlQueryMatcherAccessor::Annotation),
            arg_accessor,
            prop_accessor(kdl_parser),
            prop_name_accessor(kdl_parser),
        ))(input)?;

        Ok((input, accessor))
    }
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
        let (input, prop_name) = crate::parser::identifier(&kdl_parser.0)(input)?;
        Ok((
            input,
            KdlQueryMatcherAccessor::Prop(prop_name.value().to_owned()),
        ))
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
        let (input, prop) = crate::parser::identifier(&kdl_parser.0)(input)?;
        let (input, _) = tag(")")(input)?;
        Ok((input, prop.value().to_owned()))
    }
}

fn whitespace(input: &str) -> IResult<&str, &str, KdlParseError<&str>> {
    recognize(many0(alt((
        crate::parser::unicode_space,
        crate::parser::newline,
    ))))(input)
}
