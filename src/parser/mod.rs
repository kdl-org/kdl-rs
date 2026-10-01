use std::{fmt::Display, ops::Range, sync::Arc};

use miette::{Severity, SourceSpan};
use winnow::{
    Parser,
    combinator::cut_err,
    error::{AddContext, ErrMode, FromRecoverableError, ModalError, ParserError},
    stream::{Recover, Stream},
};

use crate::{KdlDiagnostic, KdlError};

pub(crate) mod cursor;
pub(crate) mod kdl_v2;

type PResult<T> = Result<T, PError>;
type PError = ErrMode<KdlParseError>;

// More useful version of `Location`. Parser inputs over token streams
// have trouble interacting with offsets, so spanning needs to solely
// be done via external means. `Location` is the default but it doesn't
// allow an easy way to find a span in this manner. This is a somewhat
// awkward issue that winnow has with token streams.

/// Utilities for a parse stream which has a specific notion of "Text Offset/Location".
/// This bridges the gap between a parser that has an input of a sequence of tokens,
/// and a "Slice" that is the string text.
trait TextLocation: Stream {
    /// Return a text span from `start` to the current position of this stream.
    fn span_from_checkpoint(&self, start: &Self::Checkpoint) -> Range<usize>;
}

// Turn some non-postfix combinators into postfix combinators
trait CombinatorExt<I: Stream, O, E: ParserError<I>>: Parser<I, O, E> + Sized {
    #[inline(always)]
    fn trace(self, name: impl Display) -> impl Parser<I, O, E> {
        winnow::combinator::trace(name, self)
    }

    fn cut(self) -> impl Parser<I, O, E>
    where
        E: ModalError,
    {
        cut_err(self)
    }
}
impl<I: Stream, O, E: ParserError<I>, P: Parser<I, O, E>> CombinatorExt<I, O, E> for P {}

trait SubtokenParse<I: Stream + TextLocation + Recover<IE>, E, IE>:
    Parser<I, I::Slice, E> + Sized
{
    // Parse some text within a token. All errors are added to the recoverable errors,
    // and respanned to the source text.
    fn subtoken_parse<O>(
        self,
        parser: impl FnMut(I::Slice) -> Result<O, Vec<IE>>,
        recover_value: impl FnMut() -> O,
    ) -> impl Parser<I, O, E>;
}

impl<I: Stream + TextLocation + Recover<KdlParseError>, P> SubtokenParse<I, PError, KdlParseError>
    for P
where
    P: Parser<I, I::Slice, PError> + Sized,
{
    fn subtoken_parse<O>(
        mut self,
        mut parser: impl FnMut(<I as Stream>::Slice) -> Result<O, Vec<KdlParseError>>,
        mut recover_value: impl FnMut() -> O,
    ) -> impl Parser<I, O, PError> {
        move |input: &mut I| {
            let start = input.checkpoint();
            let slice = self.parse_next(input)?;
            match parser(slice) {
                Ok(o) => Ok(o),
                Err(errs) => {
                    for mut e in errs {
                        // Respan the internal error to start from the correct spot
                        // the length doesn't need to change.
                        if let Some(inner_span) = e.span {
                            e.span = Some(SourceSpan::from((
                                input.span_from_checkpoint(&start).start + inner_span.offset(),
                                inner_span.len(),
                            )));
                        }
                        input.record_err(&start, &start, e).unwrap();
                    }
                    Ok(recover_value())
                }
            }
        }
    }
}

#[derive(Debug, Clone, Default, Eq, PartialEq)]
struct KdlParseContext {
    message: Option<String>,
    label: Option<String>,
    help: Option<String>,
    severity: Option<Severity>,
}

#[allow(unused)]
impl KdlParseContext {
    fn msg(mut self, txt: impl Display) -> Self {
        self.message = Some(format!("{}", txt));
        self
    }

    fn lbl(mut self, txt: impl Display) -> Self {
        self.label = Some(format!("{}", txt));
        self
    }

    fn hlp(mut self, txt: impl Display) -> Self {
        self.help = Some(format!("{}", txt));
        self
    }

    fn sev(mut self, severity: Severity) -> Self {
        self.severity = Some(severity);
        self
    }
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

impl KdlParseError {
    pub(self) fn from_span_and_ctx(span: SourceSpan, ctx: KdlParseContext) -> Self {
        Self {
            message: ctx.message,
            span: Some(span),
            label: ctx.label,
            help: ctx.help,
            severity: ctx.severity,
        }
    }
}

impl<I: Stream + TextLocation> ParserError<I> for KdlParseError {
    type Inner = Self;
    fn from_input(input: &I) -> Self {
        let span = input.span_from_checkpoint(&input.checkpoint());
        Self::from_span_and_ctx(span.into(), cx())
    }

    fn append(self, _input: &I, _token_start: &<I as Stream>::Checkpoint) -> Self {
        self
    }

    fn into_inner(self) -> Result<Self, Self> {
        Ok(self)
    }
}

impl<I: Stream + TextLocation> AddContext<I, KdlParseContext> for KdlParseError {
    fn add_context(
        mut self,
        input: &I,
        token_start: &<I as Stream>::Checkpoint,
        ctx: KdlParseContext,
    ) -> Self {
        self.message = ctx.message.or(self.message);
        self.label = ctx.label.or(self.label);
        self.help = ctx.help.or(self.help);
        self.severity = ctx.severity.or(self.severity);
        self.span = Some(input.span_from_checkpoint(token_start).into());
        self
    }
}

impl<I: Stream + TextLocation> FromRecoverableError<I, Self> for KdlParseError {
    #[inline]
    fn from_recoverable_error(
        token_start: &<I as Stream>::Checkpoint,
        _err_start: &<I as Stream>::Checkpoint,
        input: &I,
        mut e: Self,
    ) -> Self {
        e.span = e
            .span
            .or_else(|| Some(input.span_from_checkpoint(token_start).into()));
        e
    }
}

pub(crate) fn failure_from_errs(
    errs: impl IntoIterator<Item = KdlParseError>,
    input: &str,
) -> KdlError {
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
                label: e.label,
                help: e.help,
                severity: Severity::Error,
            })
            .collect(),
    }
}
