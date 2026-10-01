use std::{
    mem,
    ops::{Bound, RangeBounds},
    str::Chars,
};

use miette::SourceSpan;

use crate::parser::{KdlParseContext, KdlParseError};

#[derive(Debug)]
pub(super) struct Cursor<'t> {
    chars: Chars<'t>,
    src: &'t str,
    errs: Vec<KdlParseError>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(super) struct CursorPos(usize);

#[allow(unused)]
impl<'t> Cursor<'t> {
    /// Creates a new [`Cursor`] over some `src` text.
    pub(super) fn new(src: &'t str) -> Self {
        Self {
            chars: src.chars(),
            src,
            errs: Vec::new(),
        }
    }
    /// Creates an `Ok` action context.
    fn ctx_ok<S, F>(&mut self, val: S) -> ActionContext<'_, 't, S, F> {
        ActionContext::new_ok(self, val)
    }
    /// Creates an `Err` action context.
    fn ctx_err<S, F>(
        &mut self,
        val: F,
        span: impl Into<SourceSpan>,
    ) -> ActionContext<'_, 't, S, F> {
        ActionContext::new_err(self, val, span.into())
    }
    /// Returns the stream's current [`position`].
    ///
    /// [`position`]: CursorPos
    pub(super) fn position(&self) -> CursorPos {
        CursorPos(self.offset())
    }
    /// Returns the offset in bytes that the cursor is
    /// into the `src` string.
    fn offset(&self) -> usize {
        let start = self.src.as_ptr().addr();
        let current = self.text().as_ptr().addr();
        // Make sure the result actually makes sense.
        debug_assert!(
            current
                .checked_sub(start)
                .is_some_and(|v| v <= self.src.len())
        );

        current - start
    }
    /// Returns the text from `pos` to the end of the stream.
    pub(super) fn text_from(&self, pos: CursorPos) -> &'t str {
        &self.src[pos.0..]
    }
    /// Returns the text from `start` to `end`.
    pub(super) fn text_between(&self, start: CursorPos, end: CursorPos) -> &'t str {
        &self.src[start.0..end.0]
    }
    /// Helper function to move a `CursorPos` one character forwards.
    fn next_pos(&self, mut pos: CursorPos) -> CursorPos {
        let next_len = self
            .text_from(pos)
            .chars()
            .next()
            .map(char::len_utf8)
            .unwrap_or(0);
        pos.0 += next_len;
        pos
    }
    /// Eat the string `s` from the stream, returning an error if it does not eat it.
    /// The error will contain the leading characters with a length corresponding
    /// to the length of `s` (or up until EOF).
    pub(super) fn check_str(&mut self, s: &str) -> ActionContext<'_, 't, (), &'t str> {
        if self.text().starts_with(s) {
            self.chars = self.text()[s.len()..].chars();
            self.ctx_ok(())
        } else {
            let text = self.text();
            let len = text.ceil_char_boundary(s.len());
            self.ctx_err(&text[..len], (self.offset(), len))
        }
    }
    /// Eat the character `c` from the stream, returning an error if it does not have it.
    /// The error will contain the leading character or `None` if EOF was reached.
    pub(super) fn check(&mut self, c: char) -> ActionContext<'_, 't, (), Option<char>> {
        if self.peek().is_some_and(|v| v == c) {
            self.eat();
            self.ctx_ok(())
        } else {
            let c = self.peek();
            self.ctx_err(c, (self.offset(), c.is_some() as usize))
        }
    }

    pub(super) fn check_fn(
        &mut self,
        f: impl FnOnce(char) -> bool,
    ) -> ActionContext<'_, 't, char, Option<char>> {
        if let Some(c) = self.peek().filter(|v| f(*v)) {
            self.eat();
            self.ctx_ok(c)
        } else {
            let c = self.peek();
            self.ctx_err(c, (self.offset(), c.is_some() as usize))
        }
    }
    /// Returns the text remaining in the stream.
    pub(super) fn text(&self) -> &'t str {
        self.chars.as_str()
    }

    pub(super) fn advance(&mut self, len: usize) -> &'t str {
        let text = self.text();
        let len = text.ceil_char_boundary(len);

        let text = &self.text()[..len];
        self.chars = self.text()[len..].chars();
        text
    }
    /// Returns the next character in the stream, or `None` if EOF was encountered.
    pub(super) fn peek(&self) -> Option<char> {
        self.chars.clone().next()
    }
    /// Consumes the next character in the stream and returns it,
    /// or `None` if EOF was encountered.
    pub(super) fn eat(&mut self) -> Option<char> {
        self.chars.next()
    }
    /// Consumes the next character in the stream and returns it, failing
    /// if EOF was encountered.
    pub(super) fn char(&mut self) -> ActionContext<'_, 't, char, ()> {
        match self.eat() {
            Some(c) => self.ctx_ok(c),
            None => self.ctx_err((), self.offset()),
        }
    }
    /// Returns `true` if the stream is exhausted.
    pub(super) fn eof(&self) -> bool {
        self.text().is_empty()
    }
    /// Eats chars while the predicate `f` returns true and returns the text matched.
    pub(super) fn eat_while(&mut self, mut f: impl FnMut(char) -> bool) -> &'t str {
        let start = self.position();
        while self.peek().is_some_and(&mut f) {
            self.eat();
        }
        self.text_between(start, self.position())
    }
    /// Eats chars until `c` is found. If `c` does not exist in the input stream,
    /// the entire input will be consumed. Returns
    pub(super) fn eat_until_char(&mut self, c: char) -> &'t str {
        let start = self.position();

        let text = self.text();
        let idx = text.find(c).unwrap_or(text.len());
        self.chars = text[idx..].chars();

        self.text_between(start, self.position())
    }
    /// Eats chars that match the predicate `f`, with a maximum of the upper bound of `range`.
    /// If the number of chars eaten is less than the lower bound of `range`, an error
    /// will be returned containing the chars eaten + the next char that failed to match.
    /// Otherwise, the chars eaten is returned.
    pub(super) fn eat_while_range(
        &mut self,
        range: impl RangeBounds<usize>,
        mut f: impl FnMut(char) -> bool,
    ) -> ActionContext<'_, 't, &'t str, &'t str> {
        let start = self.position();

        let start_bound = (range.start_bound().cloned(), Bound::Unbounded);
        let end_bound = (Bound::Unbounded, range.end_bound().cloned());

        let mut x = 0;
        loop {
            if !self.peek().is_some_and(&mut f) {
                break;
            }
            x += 1;
            if !end_bound.contains(&x) {
                break;
            }
            self.eat();
        }

        if !start_bound.contains(&x) {
            let end = self.next_pos(self.position());
            self.ctx_err(self.text_between(start, self.next_pos(self.position())), start.0..end.0)
        } else {
            self.ctx_ok(self.text_between(start, self.position()))
        }
    }
    /// Adds a [`KdlParseError`] to the cursor's error list.
    pub(super) fn add_raw_error(&mut self, error: KdlParseError) {
        self.errs.push(error);
    }
    /// Adds an error at the current cursor position with a length of `0`.
    pub(super) fn error(&mut self, cx: KdlParseContext) {
        self.error_at(self.position(), cx);
    }
    /// Adds an error at the `pos` with a length of `0`.
    pub(super) fn error_at(&mut self, pos: CursorPos, cx: KdlParseContext) {
        self.error_range(pos, pos, cx);
    }

    pub(super) fn error_from_prev(&mut self, char: char, cx: KdlParseContext) {
        let prev = self.offset() - char.len_utf8();
        self.error_from(CursorPos(prev), cx);
    }
    /// Adds an error from `start` to the current position.
    pub(super) fn error_from(&mut self, start: CursorPos, cx: KdlParseContext) {
        let end = self.position();
        self.error_range(start, end, cx);
    }
    /// Adds an error spanning from `start` to `end`.
    pub(super) fn error_range(&mut self, start: CursorPos, end: CursorPos, cx: KdlParseContext) {
        self.errs.push(KdlParseError {
            span: Some(SourceSpan::from((start.0, end.0 - start.0))),
            message: cx.message,
            label: cx.label,
            help: cx.help,
            severity: cx.severity,
        });
    }
    /// Finishes parsing with the cursor, returning `Ok` if no errors were added
    /// or the list of errors otherwise.
    pub(super) fn into_errors(self) -> Result<(), Vec<KdlParseError>> {
        if self.errs.is_empty() {
            Ok(())
        } else {
            Err(self.errs)
        }
    }
}

/// The result of some action by a [`Cursor`].
///
/// The context has either succeeded with a value of `S`,
/// or failed with a value of `F`. This struct has utility functions
/// to add errors to the cursor if the action failed, as well as other
/// error handling utilities.
pub(super) struct ActionContext<'c, 't, S, F> {
    cursor: &'c mut Cursor<'t>,
    result: Result<S, (F, SourceSpan)>,
}

#[allow(unused)]
impl<'c, 't, S, F> ActionContext<'c, 't, S, F> {
    fn new_ok(cursor: &'c mut Cursor<'t>, val: S) -> Self {
        Self {
            cursor,
            result: Ok(val),
        }
    }

    fn new_err(cursor: &'c mut Cursor<'t>, val: F, span: SourceSpan) -> Self {
        Self {
            cursor,
            result: Err((val, span)),
        }
    }

    /// Returns `true` if this action context succeeded.
    pub(super) fn is_ok(&self) -> bool {
        self.result.is_ok()
    }
    /// Returns `true` if this action context failed.
    pub(super) fn is_err(&self) -> bool {
        self.result.is_err()
    }
    /// Adds an error at the current position if this action context failed.
    pub(super) fn with_error(
        self,
        cx: impl FnOnce(F) -> KdlParseContext,
    ) -> ActionContext<'c, 't, S, ()> {
        match self.result {
            Ok(v) => ActionContext {
                cursor: self.cursor,
                result: Ok(v),
            },
            Err((e, span)) => {
                let error = KdlParseError::from_span_and_ctx(span, cx(e));
                self.cursor.add_raw_error(error);
                ActionContext::new_err(self.cursor, (), span)
            }
        }
    }
    /// Adds an error at `pos` if this action context failed.
    pub(super) fn with_error_at(
        self,
        pos: CursorPos,
        cx: impl FnOnce(F) -> KdlParseContext,
    ) -> ActionContext<'c, 't, S, ()> {
        self.with_error_range(pos, pos, cx)
    }
    /// Adds an error from `start` to the current position if this action context failed.
    pub(super) fn with_error_from(
        self,
        start: CursorPos,
        cx: impl FnOnce(F) -> KdlParseContext,
    ) -> ActionContext<'c, 't, S, ()> {
        let pos = self.cursor.position();
        self.with_error_range(start, pos, cx)
    }
    /// Adds an error from `start` to `end` if this action context failed.
    pub(super) fn with_error_range(
        self,
        start: CursorPos,
        end: CursorPos,
        cx: impl FnOnce(F) -> KdlParseContext,
    ) -> ActionContext<'c, 't, S, ()> {
        match self.result {
            Ok(v) => ActionContext {
                cursor: self.cursor,
                result: Ok(v),
            },
            Err((e, span)) => {
                self.cursor.error_range(start, end, cx(e));
                ActionContext::new_err(self.cursor, (), span)
            }
        }
    }
    /// Fails the current parse, returning the list of accumulated errors.
    /// The source [`Cursor`] should probably not be used after this.
    pub(super) fn fail(self) -> Result<S, Vec<KdlParseError>> {
        self.result.map_err(|_| mem::take(&mut self.cursor.errs))
    }
    /// Recovers the current parse, simply returning the inner result value.
    pub(super) fn pass(self) -> Result<S, F> {
        self.result.map_err(|(v, _)| v)
    }

    pub(super) fn inner(self) -> Result<S, (F, SourceSpan)> {
        self.result
    }
}
