// TODO: MAKE SURE TO REMOVE THIS AFTER DEV

use std::{
    fmt, iter, mem,
    num::{ParseFloatError, ParseIntError},
    ops::Range,
    slice,
};

use winnow::{
    Parser,
    combinator::{
        alt, delimited, dispatch, eof, fail, opt, peek, repeat, terminated, trace,
    },
    error::{ErrMode, Needed, ParserError},
    stream::{Accumulate, ContainsToken, Location, Offset, Recover, Stream, StreamIsPartial},
    token::{any, one_of},
};

use crate::{
    KdlDocument, KdlEntry, KdlEntryFormat, KdlError, KdlIdentifier, KdlNode, KdlNodeFormat,
    KdlValue,
    parser::{
        CombinatorExt, KdlParseError, PError, PResult, SubtokenParse, TextLocation,
        cursor::{Cursor, CursorPos},
        cx, failure_from_errs,
    },
};

pub(crate) fn parse_file(src: &str) -> Result<KdlDocument, KdlError> {
    let (tokens, errors) = tokenize_bom(src);
    eprintln!("{:?}", tokens);
    let stream = TokenStream::new(&tokens, src, errors);
    match recover_parse(stream, document) {
        Ok(v) => Ok(v),
        Err(e) => Err(failure_from_errs(e, src)),
    }
}

#[allow(unused)]
pub(crate) fn parse<O>(
    src: &str,
    parser: impl for<'src> Parser<TokenStream<'src>, O, PError>,
) -> Result<O, KdlError> {
    let (tokens, errors) = tokenize(src);
    let stream = TokenStream::new(&tokens, src, errors);
    match recover_parse(stream, parser) {
        Ok(v) => Ok(v),
        Err(e) => Err(failure_from_errs(e, src)),
    }
}

fn recover_parse<'src, O>(
    mut input: TokenStream<'src>,
    parser: impl Parser<TokenStream<'src>, O, PError>,
) -> Result<O, Vec<KdlParseError>> {
    match terminated(parser, eof).parse_next(&mut input) {
        Ok(o) if input.errors.is_empty() => return Ok(o),
        // Theres no support for incomplete parsing so unwraping is fine.
        Err(e) => input.errors.insert(0, e.into_inner().unwrap()),
        _ => (),
    };
    Err(input.errors)
}

// IMPORTANT
// token offsets and text offsets are two very different things
// these type aliases exist to clarify which `usize`s are which.
//
// TokenOffset is the nth token in the token stream
// TextOffset is the nth byte in the source text
//
// TokenOffsets are not useful outside of this source file.

type TokenOffset = usize;
type TextOffset = usize;

#[derive(Copy, Clone, PartialEq, Eq, Hash, Debug)]
pub enum TokenKind {
    True,
    False,
    Null,

    IdentString,
    QuotedString,
    MultiString,
    RawQuotedString,
    RawMultiString,

    HexInteger,
    OctInteger,
    BinInteger,
    DecInteger,
    DecFloat,
    Inf,
    NegInf,
    Nan,

    LParen,
    RParen,
    LCurly,
    RCurly,

    Equals,
    Semicolon,

    SingleComment,
    MultiComment,
    Slashdash,

    Space,
    Newline,
    Escline,
    Bom,

    Unknown,
}

impl fmt::Display for TokenKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "tk::{:?}", *self)
    }
}

use TokenKind::*;

#[derive(Copy, Clone, PartialEq, Eq, Hash)]
pub struct Token<'src> {
    pub kind: TokenKind,
    pub src: &'src str,
}

impl<'src> Token<'src> {
    pub fn new(kind: TokenKind, text: &'src str, span: Range<TextOffset>) -> Self {
        Self {
            kind,
            src: &text[span],
        }
    }

    fn from_cursor(kind: TokenKind, start: CursorPos, cursor: &mut Cursor<'src>) -> Self {
        Self {
            kind,
            src: cursor.text_between(start, cursor.position()),
        }
    }
}

impl<'src> fmt::Display for Token<'src> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.src)
    }
}

impl<'src> fmt::Debug for Token<'src> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", self.src)
    }
}

// Does this really make sense? A BOM is more part of the text encoding rather than the data format.
fn tokenize_bom<'src>(text: &'src str) -> (Vec<Token<'src>>, Vec<KdlParseError>) {
    let mut cursor = Cursor::new(text);
    let mut tokens = Vec::new();
    let start = cursor.position();
    if cursor.check('\u{FEFF}').is_ok() {
        tokens.push(Token::from_cursor(Bom, start, &mut cursor));
    }
    while let Some(token) = read_token(&mut cursor) {
        tokens.push(token);
    }
    let errors = cursor.into_errors().err().unwrap_or_default();
    (tokens, errors)
}

fn tokenize<'src>(text: &'src str) -> (Vec<Token<'src>>, Vec<KdlParseError>) {
    let mut cursor = Cursor::new(text);
    let mut tokens = Vec::new();
    while let Some(token) = read_token(&mut cursor) {
        tokens.push(token);
    }
    let errors = cursor.into_errors().err().unwrap_or_default();
    (tokens, errors)
}

// Better character set handling
#[macro_export]
#[doc(hidden)]
macro_rules! cs_kdlv2 {
    ($id1:ident | $($id:ident)|+) => {
        $crate::cs_kdlv2!($id1) | $($crate::cs_kdlv2!($id))|+
    };
    (invalid) => {
        '\u{0000}'..='\u{0008}' | '\u{007F}' | '\u{200E}' | '\u{200F}' |
        '\u{202A}'..='\u{202E}' | '\u{2066}'..='\u{2069}' | '\u{FEFF}'
    };
    (non_ident) => {
        '(' | ')' | '{' | '}' | '[' | ']' | '/' | '\\' | '"' | '#' | ';' | '=' |
        $crate::cs_kdlv2!(spaces) | $crate::cs_kdlv2!(newlines)
    };
    (spaces) => {
        '\u{0009}' | '\u{0020}' | '\u{00A0}' | '\u{1680}' | '\u{2000}'..='\u{200A}' |
        '\u{202F}' | '\u{205F}' | '\u{3000}'
    };
    (newlines) => {
        '\u{000A}'..='\u{000D}' | '\u{0085}' | '\u{2028}' | '\u{2029}'
    };
    (hex_start) => { 'a'..='f' | 'A'..='F' | '0'..='9' };
    (hex) => { $crate::cs_kdlv2!(hex_start) | '_' };
    (dec_start) => { '0'..='9' };
    (dec) => { $crate::cs_kdlv2!(dec_start) | '_' };
    (oct_start) => { '0'..='7' };
    (oct) => { $crate::cs_kdlv2!(oct_start) | '_' };
    (bin_start) => { '0' | '1' };
    (bin) => { $crate::cs_kdlv2!(bin_start) | '_' };
}

#[macro_export]
#[doc(hidden)]
macro_rules! in_cs_kdlv2 {
    ($($id:ident)|+ => $e:expr) => {
        matches!($e, $crate::cs_kdlv2!($($id)|+))
    };
    ($($id:ident)|+) => {
        |c: char| { matches!(c, $crate::cs_kdlv2!($($id)|+)) }
    };
}

pub(crate) use crate::cs_kdlv2 as charset;
pub(crate) use crate::in_cs_kdlv2 as in_charset;

fn read_token<'src>(cursor: &mut Cursor<'src>) -> Option<Token<'src>> {
    let invalid_codepoint = || cx().msg("TODO: Invalid literal codepoint");

    let start = cursor.position();

    let kind = match cursor.eat()? {
        ';' => Semicolon,
        '=' => Equals,
        '{' => LCurly,
        '}' => RCurly,
        '(' => LParen,
        ')' => RParen,
        '\\' => Escline,
        '/' => match cursor.peek() {
            Some('/') => {
                cursor.eat();
                while let Some(c) = cursor.eat() {
                    match c {
                        charset!(newlines) => break,
                        charset!(invalid) => {
                            cursor.error_from_prev(c, invalid_codepoint());
                        }
                        _ => (),
                    }
                }
                SingleComment
            }
            Some('*') => {
                cursor.eat();
                let mut nest = 1;
                while nest > 0 {
                    match cursor.eat() {
                        None => {
                            // needs rewording
                            cursor.error(cx().msg("TODO: Expected the multiline comment to end"));
                            break;
                        }
                        Some('/') if cursor.peek() == Some('*') => {
                            cursor.eat();
                            nest += 1;
                        }
                        Some('*') if cursor.peek() == Some('/') => {
                            cursor.eat();
                            nest -= 1;
                        }
                        Some(c @ charset!(invalid)) => {
                            cursor.error_from_prev(c, invalid_codepoint());
                        }
                        _ => (),
                    }
                }
                MultiComment
            }
            Some('-') => {
                cursor.eat();
                Slashdash
            }
            _ => {
                cursor.error_from_prev('/', cx().msg("TODO: Invalid token"));
                Unknown
            }
        },
        // Invalid characters shouldn't end a string because they're invalid anyways
        // and they never really show up in text so its better to just include them
        // and add an error to the stream.
        '"' => {
            if cursor.check_str("\"\"").is_ok() {
                while let Some(c) = cursor.eat() {
                    match c {
                        '\\' => {
                            cursor.eat();
                        }
                        '"' if cursor.check_str("\"\"").is_ok() => break,
                        charset!(invalid) => {
                            cursor.error_from_prev(c, invalid_codepoint());
                        }
                        _ => (),
                    };
                }
                MultiString
            } else {
                while let Some(c) = cursor.eat() {
                    match c {
                        '\\' => {
                            // Either eat a whitespace escape or a normal escape
                            if cursor.eat_while(in_charset!(newlines | spaces)).is_empty() {
                                cursor.eat();
                            }
                        }
                        '"' => break,
                        charset!(newlines) => break,
                        charset!(invalid) => {
                            cursor.error_from_prev(c, invalid_codepoint());
                        }
                        _ => (),
                    }
                }
                QuotedString
            }
        }
        // Either a keyword or a raw string
        '#' => {
            if cursor.peek().is_some_and(|c| matches!(c, '#' | '"')) {
                let hashes = 1 + cursor.eat_while(|c| c == '#').len();
                eprintln!("{hashes}");
                // This must be a multiline string, as #"""# is not a valid string
                if cursor.check_str("\"\"\"").is_ok() {
                    let mut quotes = 0;
                    while let Some(c) = cursor.eat() {
                        match c {
                            '"' => quotes += 1,
                            '#' if quotes >= 3
                                && cursor.eat_while(|c| c == '#').len() + 1 >= hashes =>
                            {
                                break;
                            }
                            charset!(invalid) => {
                                cursor.error_from_prev(c, invalid_codepoint());
                            }
                            _ => {
                                quotes = 0;
                            }
                        }
                    }
                    RawMultiString
                } else {
                    if cursor
                        .check_str("\"")
                        .with_error(|_| cx().msg("TODO: Expected opening quote"))
                        .is_ok()
                    {
                        while let Some(c) = cursor.eat() {
                            match c {
                                '"' if cursor.eat_while(|c| c == '#').len() >= hashes => break,
                                charset!(invalid) => {
                                    cursor.error_from_prev(c, invalid_codepoint());
                                }
                                _ => (),
                            }
                        }
                        RawQuotedString
                    } else {
                        Unknown
                    }
                }
            } else {
                // gives a good heuristic in case of invalid keywords. explicitly does not include
                // numbers because a keyword with a number is more likely something like #true123,
                // which is just missing whitespace
                let keyword = cursor.eat_while(|c| matches!(c, 'A'..='Z' | 'a'..='z' | '-' | '_'));
                match keyword {
                    "null" => Null,
                    "true" => True,
                    "false" => False,
                    "inf" => Inf,
                    "-inf" => NegInf,
                    "nan" => Nan,
                    _ => {
                        cursor.error_from(start, cx().msg("TODO: Invalid Keyword"));
                        Unknown
                    }
                }
            }
        }
        // (){}/\"#;= are already checked, so these are the remaining possible
        // non-ident characters left besides newlines and spaces
        '[' | ']' => {
            cursor.error_from(start, cx().msg("TODO: Invalid character"));
            Unknown
        }
        '\r' => {
            cursor.check('\n');
            Newline
        }
        charset!(invalid) => {
            cursor.eat_while(in_charset!(invalid));
            cursor.error_from(start, invalid_codepoint());
            Unknown
        }
        charset!(newlines) => Newline,
        charset!(spaces) => {
            cursor.eat_while(in_charset!(spaces));
            Space
        }
        // This is awkward.
        '0' => zero_number(cursor),
        '1'..='9' => rest_dec_number(cursor),
        '.' if matches!(cursor.peek(), Some('0'..='9')) => {
            cursor.eat_while(in_charset!(dec));
            dec_exponent(cursor);
            cursor.error_from(start, cx().msg("TODO: Numbers may not start with a ."));
            DecFloat
        }
        '-' | '+' => match cursor.peek() {
            Some('0') => {
                cursor.eat();
                zero_number(cursor)
            }
            Some('1'..='9') => {
                cursor.eat();
                rest_dec_number(cursor)
            }
            Some('.') => {
                cursor.eat();
                if matches!(cursor.peek(), Some('0'..='9')) {
                    cursor.eat_while(in_charset!(dec));
                    dec_exponent(cursor);
                    cursor.error_from(start, cx().msg("TODO: Numbers may not start with a ."));
                    DecFloat
                } else {
                    cursor.eat_while(|c| !in_charset!(non_ident => c));
                    IdentString
                }
            }
            _ => {
                cursor.eat_while(|c| !in_charset!(non_ident => c));
                IdentString
            }
        },
        _ => {
            cursor.eat_while(|c| !in_charset!(non_ident => c));
            IdentString
        }
    };
    Some(Token::from_cursor(kind, start, cursor))
}

fn zero_number(cursor: &mut Cursor<'_>) -> TokenKind {
    let initial_char = |c: Option<char>, ty: &str| {
        if c == Some('_') {
            cx().msg(format_args!("TODO: `_` is not allowed at the start"))
        } else {
            cx().msg(format_args!("TODO: Expected a string of {ty} digits"))
        }
    };
    match cursor.peek() {
        Some('x') => {
            cursor.eat();
            cursor
                .check_fn(in_charset!(hex_start))
                .with_error(|c| initial_char(c, "hexadecimal"));
            cursor.eat_while(in_charset!(hex));
            HexInteger
        }
        Some('o') => {
            cursor.eat();
            cursor
                .check_fn(in_charset!(hex_start))
                .with_error(|c| initial_char(c, "octal"));
            cursor.eat_while(in_charset!(hex));
            OctInteger
        }
        Some('b') => {
            cursor.eat();
            cursor
                .check_fn(in_charset!(hex_start))
                .with_error(|c| initial_char(c, "binary"));
            cursor.eat_while(in_charset!(hex));
            BinInteger
        }
        _ => rest_dec_number(cursor),
    }
}

fn rest_dec_number(cursor: &mut Cursor<'_>) -> TokenKind {
    let mut float = false;
    cursor.eat_while(in_charset!(dec));
    if cursor.check('.').is_ok() {
        match cursor.peek() {
            Some('_') => {
                cursor.error(cx().msg("TODO: The first digit in a string of digits cannot be `_`"));
            }
            Some(charset!(dec_start)) => (),
            _ => {
                cursor.error(cx().msg("TODO: Expected digits after the decimal point"));
            }
        }
        cursor.eat_while(in_charset!(dec));
        float = true;
    }
    float |= dec_exponent(cursor);

    if float { DecFloat } else { DecInteger }
}

fn dec_exponent(cursor: &mut Cursor<'_>) -> bool {
    if matches!(cursor.peek(), Some('e' | 'E')) {
        cursor.eat();
        cursor.check_fn(|c| matches!(c, '+' | '-'));
        match cursor.peek() {
            Some('_') => {
                cursor.error(cx().msg("TODO: The first digit in a string of digits cannot be `_`"));
            }
            Some(charset!(dec_start)) => (),
            _ => {
                cursor.error(cx().msg("TODO: Expected digits after the exponent"));
            }
        }
        cursor.eat_while(in_charset!(dec));
        true
    } else {
        false
    }
}

impl<'src> Parser<TokenStream<'src>, Token<'src>, PError> for TokenKind {
    fn parse_next(&mut self, input: &mut TokenStream<'src>) -> PResult<Token<'src>> {
        trace(*self, |input: &mut TokenStream<'src>| {
            input
                .next_token()
                .filter(|t| t.kind == *self)
                .ok_or_else(|| ParserError::from_input(input))
        })
        .parse_next(input)
    }
}

impl<'src> PartialEq<TokenKind> for Token<'src> {
    fn eq(&self, other: &TokenKind) -> bool {
        self.kind == *other
    }
}
impl<'src> PartialEq<Token<'src>> for TokenKind {
    fn eq(&self, other: &Token<'src>) -> bool {
        *self == other.kind
    }
}
impl<'src, const N: usize> ContainsToken<Token<'src>> for [TokenKind; N] {
    fn contains_token(&self, token: Token<'src>) -> bool {
        self.contains(&token.kind)
    }
}

// Essentially winnow's TokenSlice, but with the `Location` impl changed.
// The offsets in `Stream` generally refer to a static token offset,
// and not the variable text offset. To get a text offset, use
// `Location` or `SpanFromCheckpoint`.
#[derive(Clone, Debug)]
pub struct TokenStream<'src> {
    slice: &'src [Token<'src>],
    initial: &'src [Token<'src>],
    text: &'src str,
    errors: Vec<KdlParseError>,
}

#[derive(Copy, Clone, Debug)]
pub struct Checkpoint<'src>(&'src [Token<'src>]);

impl<'src> Offset for Checkpoint<'src> {
    fn offset_from(&self, start: &Self) -> TokenOffset {
        self.0.offset_from(&start.0)
    }
}

impl<'src> TokenStream<'src> {
    fn new(tokens: &'src [Token<'src>], text: &'src str, errors: Vec<KdlParseError>) -> Self {
        Self {
            slice: tokens,
            initial: tokens,
            text,
            errors,
        }
    }

    fn token_slice_to_str(&self, subslice: &'src [Token<'src>]) -> &'src str {
        if subslice.is_empty() {
            // Sort of awkward. Find the index where the empty subslice should be,
            // then find the string location associated with it
            let offset = subslice.offset_from(&self.initial);
            // If the subslice is the EOF subslice, return the EOF string subslice.
            // otherwise, return the empty string from the next token's src.
            self.initial
                .get(offset)
                .map(|s| &s.src[..0])
                .unwrap_or_else(|| &self.text[self.text.len()..])
        } else {
            let start = subslice.first().unwrap().src.offset_from(&self.text);
            let end = {
                let last_src = subslice.last().unwrap().src;
                last_src.offset_from(&self.text) + last_src.len()
            };
            &self.text[start..end]
        }
    }
}

impl<'src> TextLocation for TokenStream<'src> {
    fn span_from_checkpoint(&self, checkpoint: &Self::Checkpoint) -> Range<usize> {
        if checkpoint.0.is_empty() {
            return self.text.len()..self.text.len();
        }
        let start = checkpoint.0[0].src.offset_from(&self.text);
        let end = if let Some(next_tk) = self.slice.first() {
            next_tk.src.offset_from(&self.text)
        } else {
            self.text.len()
        };
        start..end
    }
    //
    // fn location_of(&self, token: &Self::Token) -> usize {
    //     token.src.offset_from(&self.text)
    // }
}

impl<'src> Stream for TokenStream<'src> {
    type Token = Token<'src>;

    type Slice = &'src str;

    type IterOffsets = iter::Enumerate<iter::Copied<slice::Iter<'src, Self::Token>>>;

    type Checkpoint = Checkpoint<'src>;

    fn iter_offsets(&self) -> Self::IterOffsets {
        self.slice.iter().copied().enumerate()
    }

    fn eof_offset(&self) -> TokenOffset {
        self.slice.eof_offset()
    }

    fn next_token(&mut self) -> Option<Self::Token> {
        self.slice.next_token()
    }

    fn peek_token(&self) -> Option<Self::Token> {
        self.slice.peek_token()
    }

    fn offset_for<P>(&self, predicate: P) -> Option<TokenOffset>
    where
        P: Fn(Self::Token) -> bool,
    {
        self.slice.offset_for(predicate)
    }

    fn offset_at(&self, tokens: TokenOffset) -> Result<TokenOffset, Needed> {
        self.slice.offset_at(tokens)
    }

    fn next_slice(&mut self, offset: TokenOffset) -> Self::Slice {
        let slice = self.slice.next_slice(offset);
        self.token_slice_to_str(slice)
    }

    fn peek_slice(&self, offset: TokenOffset) -> Self::Slice {
        self.token_slice_to_str(self.slice.peek_slice(offset))
    }

    fn checkpoint(&self) -> Self::Checkpoint {
        Checkpoint(self.slice)
    }

    fn reset(&mut self, checkpoint: &Self::Checkpoint) {
        self.slice = checkpoint.0;
    }

    fn trace(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.slice.trace(f)
    }
}

impl<'src> Recover<KdlParseError> for TokenStream<'src> {
    fn record_err(
        &mut self,
        _: &Self::Checkpoint,
        _: &Self::Checkpoint,
        err: KdlParseError,
    ) -> Result<(), KdlParseError> {
        self.errors.push(err);
        Ok(())
    }

    fn is_recovery_supported() -> bool {
        true
    }
}

impl<'src> StreamIsPartial for TokenStream<'src> {
    type PartialState = ();

    fn complete(&mut self) -> Self::PartialState {}

    fn restore_partial(&mut self, _: Self::PartialState) {}

    fn is_partial_supported() -> bool {
        false
    }
}

impl<'src> Location for TokenStream<'src> {
    fn previous_token_end(&self) -> TextOffset {
        // Tokens are always flush with eachother
        self.current_token_start()
    }

    fn current_token_start(&self) -> TextOffset {
        if let Some(token_text) = self.slice.first() {
            token_text.src.offset_from(&self.text)
        } else {
            self.text.len()
        }
    }
}

impl<'src> Offset<Checkpoint<'src>> for TokenStream<'src> {
    fn offset_from(&self, start: &Checkpoint<'_>) -> TokenOffset {
        self.slice.offset_from(&start.0)
    }
}

#[allow(unused)]
fn recover<'src, O>(
    mut parser: impl Parser<TokenStream<'src>, O, PError>,
    mut recover: impl Parser<TokenStream<'src>, (), PError>,
) -> impl Parser<TokenStream<'src>, Option<O>, PError> {
    move |input: &mut TokenStream<'src>| {
        let err = match parser.parse_next(input) {
            Ok(o) => return Ok(Some(o)),
            Err(ErrMode::Cut(e)) => e,
            Err(e) => return Err(e),
        };
        if recover.parse_next(input).is_ok() {
            input.errors.push(err);
            Ok(None)
        } else {
            Err(ErrMode::Cut(err))
        }
    }
}
fn peek_kind(input: &mut TokenStream<'_>) -> PResult<TokenKind> {
    input
        .peek_token()
        .map(|v| v.kind)
        .ok_or_else(|| ParserError::from_input(input))
}

pub(crate) struct KdlDocumentAcc {
    result: KdlDocument,
    trailing: String,
    start: Option<usize>,
    end: usize,
}

impl KdlDocumentAcc {
    pub(crate) fn finish(mut self) -> KdlDocument {
        #[cfg(feature = "span")]
        {
            self.result.span = (self.start.unwrap_or(self.end)..self.end).into();
        }
        if self.result.nodes.is_empty() {
            self.result.format_mut().unwrap().leading = self.trailing;
            self.result
        } else {
            self.result.format_mut().unwrap().trailing = self.trailing;
            self.result
        }
    }
}

impl<'src> Accumulate<AccEntry<'src, KdlNode>> for KdlDocumentAcc {
    fn initial(capacity: Option<usize>) -> Self {
        Self {
            result: KdlDocument {
                nodes: capacity.map(Vec::with_capacity).unwrap_or_default(),
                format: Some(Default::default()),
                #[cfg(feature = "span")]
                span: 0.into(),
            },
            start: None,
            end: 0,
            trailing: String::new(),
        }
    }

    fn accumulate(&mut self, acc: AccEntry<'src, KdlNode>) {
        match acc.kind {
            AccKind::Normal(mut node) => {
                // Put the acquired whitespace to the start of the
                // new node before adding it to the document.
                let fmt = node.format_mut().unwrap();
                self.trailing.push_str(&fmt.leading);
                fmt.leading = mem::take(&mut self.trailing);
                self.result.nodes.push(node);

                self.start = self.start.or(Some(acc.span.start));
                self.end = acc.span.end;
            }
            AccKind::Slashdash(_) | AccKind::Whitespace => {
                self.trailing.push_str(acc.text);
            }
        }
    }
}

fn document(input: &mut TokenStream<'_>) -> PResult<KdlDocument> {
    let bom = opt(Bom).parse_next(input)?;
    let mut doc = nodes.map(KdlDocumentAcc::finish).parse_next(input)?;
    if let Some((fmt, bom)) = doc.format_mut().zip(bom) {
        fmt.leading = format!("{bom}{}", fmt.leading);
    }
    Ok(doc)
}

#[test]
fn idk() -> miette::Result<()> {
    let text = r#####"
test
/-slashdash
test2
"#####;

    let (tokens, errors) = tokenize(text);

    println!("{:?}", tokens);

    let mut stream = TokenStream::new(&tokens, text, errors);
    let document = match document.parse_next(&mut stream) {
        Ok(d) => {
            if !stream.errors.is_empty() {
                return Err(failure_from_errs(stream.errors, text).into());
            }
            d
        }
        Err(e) => {
            stream.errors.insert(0, e.into_inner().unwrap());
            return Err(failure_from_errs(stream.errors, text).into());
        }
    };
    println!("{:?}", document);
    println!("\ndocument:\n{}\n\noriginal:\n{}\n", document, text);

    Ok(())
}

#[derive(Debug)]
pub(crate) struct AccEntry<'src, T> {
    span: Range<TextOffset>,
    text: &'src str,
    kind: AccKind<T>,
}

impl<'src, T> AccEntry<'src, T> {
    pub(crate) fn new_normal(text: &'src str, span: Range<TextOffset>, value: T) -> Self {
        Self {
            text,
            span,
            kind: AccKind::Normal(value),
        }
    }

    pub(crate) fn new_slashdash(text: &'src str, span: Range<TextOffset>, value: T) -> Self {
        Self {
            text,
            span,
            kind: AccKind::Slashdash(value),
        }
    }

    pub(crate) fn new_whitespace(text: &'src str, span: Range<TextOffset>) -> Self {
        Self {
            text,
            span,
            kind: AccKind::Whitespace,
        }
    }
}

#[derive(Debug)]
pub(crate) enum AccKind<T> {
    Normal(T),
    Slashdash(T),
    Whitespace,
}

impl<'src, T> From<((AccKind<T>, Range<TextOffset>), &'src str)> for AccEntry<'src, T> {
    fn from(((kind, span), text): ((AccKind<T>, Range<TextOffset>), &'src str)) -> Self {
        AccEntry { span, text, kind }
    }
}

pub(crate) fn padded_node(input: &mut TokenStream<'_>) -> PResult<KdlNode> {
    #[derive(Debug, Default)]
    struct Acc {
        node: Option<KdlNode>,
        trailing: String,
        error: Option<KdlParseError>,
    }
    impl<'src> Accumulate<AccEntry<'src, KdlNode>> for Acc {
        fn initial(_: Option<usize>) -> Self {
            Self::default()
        }

        fn accumulate(&mut self, acc: AccEntry<'src, KdlNode>) {
            match acc.kind {
                AccKind::Normal(mut node) => {
                    if self.node.is_none() {
                        node.format_mut().unwrap().leading = mem::take(&mut self.trailing);
                        self.node = Some(node);
                    } else if self.error.is_none() {
                        self.error = Some(KdlParseError::from_span_and_ctx(
                            acc.span.into(),
                            cx().msg("TODO: Only one node"),
                        ))
                    }
                }
                _ => self.trailing.push_str(acc.text),
            }
        }
    }
    let start = input.checkpoint();
    let result = nodes.map(|acc: Acc| acc).parse_next(input)?;
    let extra = opt(line_spaces).take().parse_next(input)?;
    if let Some(err) = result.error {
        Err(ErrMode::Cut(err))
    } else if let Some(mut node) = result.node {
        node.format_mut().unwrap().trailing = format!("{}{}", result.trailing, extra);
        Ok(node)
    } else {
        Err(ErrMode::Cut(KdlParseError::from_span_and_ctx(
            input.span_from_checkpoint(&start).into(),
            cx().msg("TODO: Need one node"),
        )))
    }
}

pub(crate) fn nodes<'src, A>(input: &mut TokenStream<'src>) -> PResult<A>
where
    A: Accumulate<AccEntry<'src, KdlNode>>,
{
    trace("nodes", |input: &mut TokenStream<'src>| {
        // This function is evil but everything is awkward otherwise.
        let mut acc = A::initial(None);

        // Normal inter-node whitespace
        let mut ws = opt(line_spaces)
            .map(|_| AccKind::<KdlNode>::Whitespace)
            .with_span()
            .with_taken()
            .map(AccEntry::from);
        // A base node followed by maybe pre-terminator whitespace.
        // The whitespace may be treated as trailing whitespace if
        // there is no terminator to follow.
        let mut nd = (
            alt((
                base_node
                    .with_span()
                    .with_taken()
                    .map(|((v, s), t)| (v, false, s, t)),
                slashdashed(base_node.cut())
                    .with_span()
                    .with_taken()
                    .map(|((v, s), t)| (v, true, s, t)),
            )),
            node_space0.span().with_taken(),
        );

        // Parse and commit an initial whitespace span
        acc.accumulate(ws.parse_next(input)?);
        // Now parse a node. Do not commit it until checks if there is a following terminator
        let mut checkpoint = input.checkpoint();
        let Some((mut node, mut trail)) = opt(nd.by_ref()).parse_next(input)? else {
            return Ok(acc);
        };
        loop {
            if let Some(term) = opt(node_terminator).parse_next(input)? {
                let fmt = node.0.format_mut().unwrap();
                fmt.before_terminator.push_str(trail.1);
                fmt.terminator = term.into();
                let span = input.span_from_checkpoint(&checkpoint);
                let text = &input.text[span.clone()];
                #[cfg(feature = "span")]
                {
                    node.0.span = span.clone().into();
                }
                acc.accumulate(if node.1 {
                    AccEntry::new_slashdash(text, span, node.0)
                } else {
                    AccEntry::new_normal(text, span, node.0)
                });
            } else {
                // No terminator was parsed, so end the node iteration.
                // This trailing whitespace should be sent to the accumulator
                // instead of the parsed node.
                acc.accumulate(if node.1 {
                    AccEntry::new_slashdash(node.3, node.2, node.0)
                } else {
                    AccEntry::new_normal(node.3, node.2, node.0)
                });
                acc.accumulate(AccEntry::new_whitespace(trail.1, trail.0));
                break;
            }

            // A node + terminator pair was just parsed and committed.
            // All that remains is try to parse new lines spaces + a new node.
            acc.accumulate(ws.parse_next(input)?);
            // TODO: maybe instead of opt'ing this, actually check for }/eof?
            checkpoint = input.checkpoint();
            if let Some((new_node, new_trail)) = opt(nd.by_ref()).parse_next(input)? {
                node = new_node;
                trail = new_trail;
            } else {
                break;
            }
        }

        // TODO: Maybe parse another set of trailing whitespace? It will only matter when
        // breaking due to not terminator, but if that happens, it *should* get eaten up
        // by the node-spaces + terminator.

        Ok(acc)
    })
    .parse_next(input)
}

/// A node up until the terminator. Should not be called by anything else
/// because the span and terminator data (which are logically part of the node)
/// are not actually filled in yet.
fn base_node(input: &mut TokenStream<'_>) -> PResult<KdlNode> {
    trace("base-node", |input: &mut TokenStream<'_>| {
        let _start = input.checkpoint();
        let (before_ty_name, ty, after_ty_name) = opt(annotation)
            .parse_next(input)?
            .map(|(b, i, a)| (b, Some(i), a))
            .unwrap_or_default();
        let after_ty = if ty.is_some() {
            node_space0.take().parse_next(input)?.into()
        } else {
            String::new()
        };
        let name = identifier.parse_next(input)?;
        let (entries, after_entries) = entries.map(KdlEntriesAcc::finish).parse_next(input)?;

        let (before_children, children, after_children) =
            children.map(KdlChildrenAcc::finish).parse_next(input)?;

        // TODO: This is for compliance with the current formatting, but I probably want to change
        // this to be more correct. before_children should always contain trailing slashdashed
        // entries and leading slashdashed children, and before_terminator should include trailing
        // slashdashed children.
        let (before_children, before_terminator) = if children.is_some() {
            (
                format!("{}{}", after_entries, before_children),
                after_children,
            )
        } else {
            (
                String::new(),
                format!("{}{}{}", after_entries, before_children, after_children),
            )
        };

        Ok(KdlNode {
            ty,
            name,
            entries,
            children,
            format: Some(KdlNodeFormat {
                leading: String::new(),
                before_ty_name: before_ty_name.into(),
                after_ty_name: after_ty_name.into(),
                after_ty,
                before_children,
                before_terminator,
                terminator: String::new(),
                trailing: String::new(),
            }),
            #[cfg(feature = "span")]
            span: input.span_from_checkpoint(&_start).into(),
        })
    })
    .parse_next(input)
}

fn node_terminator<'src>(input: &mut TokenStream<'src>) -> PResult<&'src str> {
    alt((line_terminator.void(), Semicolon.void()))
        .take()
        .parse_next(input)
}

pub(crate) fn identifier(input: &mut TokenStream<'_>) -> PResult<KdlIdentifier> {
    string
        .with_span()
        .with_taken()
        .map(|((v, s), r)| KdlIdentifier {
            value: v,
            repr: Some(r.into()),
            span: s.into(),
        })
        .parse_next(input)
}

fn annotation<'src>(
    input: &mut TokenStream<'src>,
) -> PResult<(&'src str, KdlIdentifier, &'src str)> {
    delimited(
        LParen,
        (node_space0.take(), identifier, node_space0.take()).cut(),
        RParen.cut(),
    )
    .trace("type")
    .parse_next(input)
}

pub(crate) struct KdlEntriesAcc {
    result: Vec<KdlEntry>,
    trailing: String,
}

impl KdlEntriesAcc {
    pub(crate) fn finish(self) -> (Vec<KdlEntry>, String) {
        (self.result, self.trailing)
    }
}

impl<'src> Accumulate<AccEntry<'src, KdlEntry>> for KdlEntriesAcc {
    fn initial(_: Option<usize>) -> Self {
        Self {
            result: Vec::new(),
            trailing: String::new(),
        }
    }

    fn accumulate(&mut self, acc: AccEntry<'src, KdlEntry>) {
        match acc.kind {
            AccKind::Normal(mut entry) => {
                // Put the acquired whitespace to the start of the
                // new entry.
                let fmt = entry.format_mut().unwrap();
                self.trailing.push_str(&fmt.leading);
                fmt.leading = mem::take(&mut self.trailing);

                self.result.push(entry);
            }
            AccKind::Slashdash(_) | AccKind::Whitespace => {
                self.trailing.push_str(acc.text);
            }
        }
    }
}

pub(crate) fn padded_entry(input: &mut TokenStream<'_>) -> PResult<KdlEntry> {
    #[derive(Debug, Default)]
    struct Acc {
        entry: Option<KdlEntry>,
        trailing: String,
        error: Option<KdlParseError>,
    }
    impl<'src> Accumulate<AccEntry<'src, KdlEntry>> for Acc {
        fn initial(_: Option<usize>) -> Self {
            Self::default()
        }

        fn accumulate(&mut self, acc: AccEntry<'src, KdlEntry>) {
            eprintln!("{:?}", acc);
            match acc.kind {
                AccKind::Normal(mut entry) => {
                    if self.entry.is_none() {
                        entry.format_mut().unwrap().leading = mem::take(&mut self.trailing);
                        self.entry = Some(entry);
                    } else if self.error.is_none() {
                        self.error = Some(KdlParseError::from_span_and_ctx(
                            acc.span.into(),
                            cx().msg("TODO: Only one entry"),
                        ))
                    }
                }
                _ => self.trailing.push_str(acc.text),
            }
        }
    }
    // Is it fair to not recognize slashdashed braced children?
    // This might need a rework
    let start = input.checkpoint();
    let result = entries_inner.map(|acc: Acc| acc).parse_next(input)?;
    let extra = node_space0.take().parse_next(input)?;
    if let Some(err) = result.error {
        Err(ErrMode::Cut(err))
    } else if let Some(mut entry) = result.entry {
        entry.format_mut().unwrap().trailing = format!("{}{}", result.trailing, extra);
        Ok(entry)
    } else {
        Err(ErrMode::Cut(KdlParseError::from_span_and_ctx(
            input.span_from_checkpoint(&start).into(),
            cx().msg("TODO: Need one entry"),
        )))
    }
}

// Entries but without a requirement on the initial whitespace
fn entries_inner<'src, A>(input: &mut TokenStream<'src>) -> PResult<A>
where
    A: Accumulate<AccEntry<'src, KdlEntry>>,
{
    // There has to be a better way to do this that can handle heterogenous tuples.
    let mut acc = A::initial(None);

    // Either a slashdash that we do not consume, or node spaces
    let mut ws = alt((
        peek_kind.verify(|c| *c == Slashdash).void(),
        node_space1.void(),
    ))
    .map(|_| AccKind::<KdlEntry>::Whitespace)
    .with_span()
    .with_taken()
    .map(AccEntry::from);

    let mut entry = alt((
        slashdashed(entry).map(AccKind::Slashdash),
        entry.map(AccKind::Normal),
    ))
    .with_span()
    .with_taken()
    .map(AccEntry::from);

    // Try to parse leading whitespace, but don't fail if it isn't present.
    let mut checkpoint = input.checkpoint();
    let mut entry_ws = opt(ws.by_ref()).parse_next(input)?;
    while let Some(entry) = opt(entry.by_ref()).parse_next(input)? {
        checkpoint = input.checkpoint();
        if let Some(w) = entry_ws {
            acc.accumulate(w);
        }
        acc.accumulate(entry);
        let Some(w) = opt(ws.by_ref()).parse_next(input)? else {
            break;
        };
        entry_ws = Some(w);
    }

    input.reset(&checkpoint);

    Ok(acc)
}

pub(crate) fn entries<'src, A>(input: &mut TokenStream<'src>) -> PResult<A>
where
    A: Accumulate<AccEntry<'src, KdlEntry>>,
{
    // Most usages of entries requires it to start with a slashdash or
    // node-space
    if peek(alt((Slashdash.void(), node_space.void())))
        .parse_next(input)
        .is_err()
    {
        return Ok(A::initial(None));
    }
    entries_inner.parse_next(input)
}

fn entry(input: &mut TokenStream<'_>) -> PResult<KdlEntry> {
    alt((
        (
            identifier.with_taken(),
            opt((
                node_space0.take(),
                Equals,
                node_space0.take(),
                opt((annotation, node_space0.take())),
                value.with_taken().cut(),
            )),
        )
            .with_span()
            .map(|((ident, val), _span)| {
                if let Some((after_key, _, after_eq, ty, val)) = val {
                    let (before_ty_name, ty, after_ty_name, after_ty) = ty
                        .map(|((b, i, a), at)| (b, Some(i), a, at))
                        .unwrap_or_default();
                    KdlEntry {
                        name: Some(ident.0),
                        ty,
                        value: val.0,
                        format: Some(KdlEntryFormat {
                            after_key: after_key.into(),
                            before_ty_name: before_ty_name.into(),
                            after_ty_name: after_ty_name.into(),
                            after_ty: after_ty.into(),
                            after_eq: after_eq.into(),
                            value_repr: val.1.into(),
                            ..Default::default()
                        }),
                        #[cfg(feature = "span")]
                        span: _span.into(),
                    }
                } else {
                    KdlEntry {
                        name: None,
                        ty: None,
                        value: KdlValue::String(ident.0.value),
                        format: Some(KdlEntryFormat {
                            value_repr: ident.1.into(),
                            ..Default::default()
                        }),
                        #[cfg(feature = "span")]
                        span: _span.into(),
                    }
                }
            }),
        (opt((annotation, node_space0.take())), value.with_taken())
            .with_span()
            .map(|((ty, val), _span)| {
                let (before_ty_name, ty, after_ty_name, after_ty) = ty
                    .map(|((b, i, a), at)| (b, Some(i), a, at))
                    .unwrap_or_default();
                KdlEntry {
                    name: None,
                    ty,
                    value: val.0,
                    format: Some(KdlEntryFormat {
                        before_ty_name: before_ty_name.into(),
                        after_ty_name: after_ty_name.into(),
                        after_ty: after_ty.into(),
                        value_repr: val.1.into(),
                        ..Default::default()
                    }),
                    #[cfg(feature = "span")]
                    span: _span.into(),
                }
            }),
    ))
    .trace("entry")
    .parse_next(input)
}

fn value(input: &mut TokenStream<'_>) -> PResult<KdlValue> {
    alt((
        string.map(KdlValue::String),
        number,
        True.map(|_| KdlValue::Bool(true)),
        False.map(|_| KdlValue::Bool(false)),
        Null.map(|_| KdlValue::Null),
    ))
    .parse_next(input)
}

pub(crate) struct KdlChildrenAcc {
    leading: String,
    document: Option<KdlDocument>,
    trailing: String,
}

impl KdlChildrenAcc {
    pub(crate) fn finish(self) -> (String, Option<KdlDocument>, String) {
        (self.leading, self.document, self.trailing)
    }
}

impl<'src> Accumulate<AccEntry<'src, KdlDocument>> for KdlChildrenAcc {
    fn initial(_: Option<usize>) -> Self {
        Self {
            leading: String::new(),
            document: None,
            trailing: String::new(),
        }
    }

    fn accumulate(&mut self, acc: AccEntry<'src, KdlDocument>) {
        match acc.kind {
            AccKind::Normal(entry) => {
                self.document = Some(entry);
            }
            AccKind::Whitespace | AccKind::Slashdash(_) => {
                if self.document.is_some() {
                    self.trailing.push_str(acc.text);
                } else {
                    self.leading.push_str(acc.text);
                }
            }
        }
    }
}

pub(crate) fn children<'src, A>(input: &mut TokenStream<'src>) -> PResult<A>
where
    A: Accumulate<AccEntry<'src, KdlDocument>>,
{
    trace("children", |input: &mut TokenStream<'src>| {
        let slashed_children = |acc: A, input: &mut TokenStream<'src>| {
            // Annoying workaround for `init` in `fold` needing to be FnMut.
            let mut acc = Some(acc);
            repeat(
                0..,
                (
                    node_space0
                        .map(|_| AccKind::<KdlDocument>::Whitespace)
                        .with_span()
                        .with_taken(),
                    slashdashed(braced_child_nodes.cut())
                        .map(AccKind::Slashdash)
                        .with_span()
                        .with_taken(),
                ),
            )
            .fold(
                || acc.take().unwrap(),
                |mut acc, (ws, cl)| {
                    acc.accumulate(ws.into());
                    acc.accumulate(cl.into());
                    acc
                },
            )
            .parse_next(input)
        };
        let mut acc = slashed_children(A::initial(None), input)?;

        let real_child = opt((
            node_space0
                .map(|_| AccKind::<KdlDocument>::Whitespace)
                .with_span()
                .with_taken(),
            braced_child_nodes
                .map(AccKind::Normal)
                .with_span()
                .with_taken(),
        ))
        .parse_next(input)?;

        if let Some((ws, cl)) = real_child {
            acc.accumulate(ws.into());
            acc.accumulate(cl.into());

            slashed_children(acc, input)
        } else {
            Ok(acc)
        }
    })
    .parse_next(input)
}

/// Specifically the { ... } bit. Children refers to a list of child_lists, slashdashed
/// and unslashdashed
fn braced_child_nodes(input: &mut TokenStream<'_>) -> PResult<KdlDocument> {
    // let _start = input.checkpoint();
    //
    let doc = delimited(
        LCurly,
        nodes.cut().map(KdlDocumentAcc::finish),
        RCurly.cut(),
    )
    .parse_next(input)?;
    // #[cfg(feature = "span")]
    // {
    //     doc.span = input.span_from_checkpoint(&_start).into();
    // }

    Ok(doc)
}

fn number(input: &mut TokenStream<'_>) -> PResult<KdlValue> {
    // lbl was weird here, but _name is present for ease of adding back
    let parse_int_error = |err: ParseIntError| cx().msg(format_args!("{err}"));
    let parse_float_error = |err: ParseFloatError| cx().msg(format_args!("{err}"));
    let parse_radix_int_error = |err: ParseIntError, _name: &str| cx().msg(format_args!("{err}"));

    let parse_nondec_number = |base: u32| {
        move |input: &str| {
            let (ch, name) = match base {
                16 => ('x', "hexadecimal"),
                8 => ('o', "octal"),
                2 => ('b', "binary"),
                _ => unreachable!(),
            };
            // The lexer reads `[-+]?0[xbo]<digits>`, meaning the invalid chars are
            // underscores and the [xbo]. This will leave a leading `0` but this
            // function is fine with that.
            let fixed = input.replace(['_', ch], "");
            i128::from_str_radix(&fixed, base)
                .map(KdlValue::Integer)
                .map_err(|e| {
                    vec![KdlParseError::from_span_and_ctx(
                        (0..input.len()).into(),
                        parse_radix_int_error(e, name),
                    )]
                })
        }
    };

    // Make these special values just in case?
    let bad_int = || KdlValue::Integer(0);
    let bad_float = || KdlValue::Float(0.0);

    match peek_kind.parse_next(input)? {
        Nan => any.map(|_| KdlValue::Float(f64::NAN)).parse_next(input),
        NegInf => any
            .map(|_| KdlValue::Float(f64::NEG_INFINITY))
            .parse_next(input),
        Inf => any
            .map(|_| KdlValue::Float(f64::INFINITY))
            .parse_next(input),
        HexInteger => any
            .take()
            .subtoken_parse(parse_nondec_number(16), bad_int)
            .parse_next(input),
        OctInteger => any
            .take()
            .subtoken_parse(parse_nondec_number(8), bad_int)
            .parse_next(input),
        BinInteger => any
            .take()
            .subtoken_parse(parse_nondec_number(2), bad_int)
            .parse_next(input),
        DecInteger => any
            .take()
            .subtoken_parse(
                |input: &str| {
                    input
                        .replace('_', "")
                        .parse()
                        .map(KdlValue::Integer)
                        .map_err(|e| {
                            vec![KdlParseError::from_span_and_ctx(
                                (0..input.len()).into(),
                                parse_int_error(e),
                            )]
                        })
                },
                bad_int,
            )
            .parse_next(input),
        DecFloat => any
            .take()
            .subtoken_parse(
                |input: &str| {
                    input
                        .replace('_', "")
                        .parse()
                        .map(KdlValue::Float)
                        .map_err(|e| {
                            vec![KdlParseError::from_span_and_ctx(
                                (0..input.len()).into(),
                                parse_float_error(e),
                            )]
                        })
                },
                bad_float,
            )
            .parse_next(input),
        _ => fail.parse_next(input),
    }
}

fn string(input: &mut TokenStream<'_>) -> PResult<String> {
    let bad_val = || String::from("<<INVALID STRING>>");
    dispatch! {peek_kind;
        IdentString => any.take().map(From::from),
        QuotedString => any.take().subtoken_parse(parse_quoted_string, bad_val),
        MultiString => any.take().subtoken_parse(parse_multiline_string, bad_val),
        RawQuotedString => any.take().subtoken_parse(parse_raw_quoted_string, bad_val),
        RawMultiString => any.take().subtoken_parse(parse_raw_multiline_string, bad_val),
        _ => fail,
    }
    .parse_next(input)
}

fn parse_quoted_string(input: &str) -> Result<String, Vec<KdlParseError>> {
    let bad_eof = |_| cx().msg("TODO: Expected closing quote");
    let no_esc = |_| cx().msg("TODO: Expected escape character");

    let mut cursor = Cursor::new(input);
    let mut buffer = String::with_capacity(input.len() - 2);
    // String token would never be lexed without a leading quote
    assert!(cursor.check('"').is_ok());

    loop {
        match cursor.char().with_error(bad_eof).fail()? {
            '"' => break,
            '\\' => {
                let start = cursor.position();
                buffer.push(match cursor.char().with_error(no_esc).fail()? {
                    // Simple escapes
                    'b' => '\u{0008}',
                    'n' => '\n',
                    'f' => '\u{000C}',
                    'r' => '\r',
                    't' => '\t',
                    's' => ' ',
                    '\\' => '\\',
                    '"' => '"',
                    // Unicode escaping
                    'u' => match parse_unicode_escape(&mut cursor) {
                        Some(c) => c,
                        None => continue,
                    },
                    // Whitespace escaping
                    charset!(spaces | newlines) => {
                        cursor.eat_while(in_charset!(spaces | newlines));
                        continue;
                    }
                    // Invalid escape character
                    _ => {
                        cursor.error_from(start, cx().msg("TODO: Invalid escape"));
                        continue;
                    }
                })
            }
            c => buffer.push(c),
        }
    }

    cursor.into_errors()?;

    Ok(buffer)
}

fn parse_raw_quoted_string(input: &str) -> Result<String, Vec<KdlParseError>> {
    let hashless_input = input.trim_matches('#');
    let start_hashes = hashless_input.as_ptr().addr() - input.as_ptr().addr();
    let end_hashes = input.len() - hashless_input.len() - start_hashes;

    let mut cursor = Cursor::new(&input[..(input.len() - end_hashes - 1)]);

    if start_hashes != end_hashes {
        cursor.add_raw_error(KdlParseError::from_span_and_ctx(
            (0..start_hashes).into(),
            cx().msg("TODO: Unbalanced hashes"),
        ));
    }
    if !hashless_input.ends_with("\"") {
        return Err(vec![KdlParseError {
            span: Some((0..(start_hashes + 1)).into()),
            message: Some("TODO: Unclosed raw string".into()),
            ..Default::default()
        }]);
    }
    // Move past the hashes + quote (these always get lexed correctly)
    cursor.advance(start_hashes + 1);

    let text = cursor.text();

    cursor.into_errors()?;

    Ok(text.into())
}

// Pretty sure this is correct, but the function is really complicated
fn parse_multiline_string(input: &str) -> Result<String, Vec<KdlParseError>> {
    let no_esc = |_| cx().msg("TODO: Expected escape character");
    let no_nl = |_| cx().msg("TODO: Multiline string must start with a newline");
    let bad_dedent = |_| cx().msg("TODO: Dedent error");
    // Do the initial unterminated string check. These tokens consumes basically
    // everything in their path, so most other errors will be nonsense.
    // The ending isn't very interesting as well, so just span it to the initial 3 quotes.
    // TODO: Maybe add another diagnostic with some help showing the end?
    if !input.ends_with("\"\"\"") {
        return Err(vec![KdlParseError {
            span: Some((0..3).into()),
            message: Some("TODO: Unclosed multiline string".into()),
            ..Default::default()
        }]);
    }

    // Just strip out the final quotes from the cursor because we already checked it.
    // The same can't be done for the initial part because of error spanning.
    let mut cursor = Cursor::new(&input[..(input.len() - 3)]);
    // TODO: Maybe don't bother with capacity? These strings are likely
    // to be significantly smaller than their actual token lengths.
    let mut buffer = String::with_capacity(input.len() - 6);
    // Two double quotes would just get lexed as an empty string,
    // and given that its a valid token heuristics can't be done.
    // Hence, this token is guaranteed to start with the 3 quotes.
    cursor.advance(3);

    // A multiline string needs to start with a newline after the quotes,
    // but its not "fatal" if it doesn't.
    let init_nl = cursor
        .check_fn(in_charset!(newlines))
        .with_error(no_nl)
        .pass();
    if init_nl == Ok('\r') && cursor.peek() == Some('\n') {
        cursor.eat();
    }

    // First we need to scan for the ending whitespace. If an invalid dedent is found,
    // just pretend there is no dedent so its possible to collect errors from the rest
    // of the string.
    let dedent = get_dedent_whitespace(&input[3..(input.len() - 3)]).unwrap_or_else(|mut err| {
        // An error with the initial newlines will cause a dedent error, but
        // that error is a bit more cryptic so no point in pushing it.
        if init_nl.is_err() {
            return "";
        }
        err.span = err.span.map(|v| (v.offset() + 3, v.len()).into());
        cursor.add_raw_error(err);
        ""
    });

    // The start of this loop is now in the context of a new line.
    while !cursor.eof() {
        let buffer_line_start = buffer.len();
        let mut only_whitespace = true;
        // Save the dedent error for later so it can be added only if this is not
        // a blank line
        let dedent_result = cursor.check_str(dedent).inner();

        while let Some(c) = cursor.eat() {
            match c {
                '\\' => {
                    only_whitespace = false;
                    let start = cursor.position();
                    buffer.push(match cursor.char().with_error(no_esc).fail()? {
                        // Simple escapes
                        'b' => '\u{0008}',
                        'n' => '\n',
                        'f' => '\u{000C}',
                        'r' => '\r',
                        't' => '\t',
                        's' => ' ',
                        '\\' => '\\',
                        '"' => '"',
                        // Unicode escaping
                        'u' => match parse_unicode_escape(&mut cursor) {
                            Some(c) => c,
                            None => continue,
                        },
                        // Whitespace escaping
                        charset!(spaces | newlines) => {
                            let res = cursor.eat_while(in_charset!(spaces | newlines));
                            eprintln!("ough {:?}", format!("{}{}", c, res));
                            continue;
                        }
                        // Invalid escape character
                        _ => {
                            cursor.error_from(start, cx().msg("TODO: Invalid escape"));
                            continue;
                        }
                    })
                }
                charset!(newlines) => {
                    buffer.push('\n');
                    break;
                }
                charset!(spaces) => buffer.push(c),
                c => {
                    buffer.push(c);
                    only_whitespace = false;
                }
            }
        }

        // If only whitespace were read from the line and actual data has been pushed,
        // clear the line and append a newline.
        if only_whitespace && buffer.len() > buffer_line_start {
            buffer.truncate(buffer_line_start);
            buffer.push('\n');
        } else if let Err((e, span)) = dedent_result {
            cursor.add_raw_error(KdlParseError::from_span_and_ctx(span, bad_dedent(e)));
        }
    }

    // In every valid multiline string, there is a final newline, so pop that.
    buffer.pop();

    cursor.into_errors()?;

    Ok(buffer)
}

fn parse_unicode_escape(cursor: &mut Cursor<'_>) -> Option<char> {
    // TODO: Might be useful to organize errors elsewhere?
    // They can get pretty cluttered when having good heuristics
    let no_lcur = |_| cx().msg("TODO: Missing {");
    let no_rcur = |c: Option<char>| match c {
        Some(c) if c.is_ascii_hexdigit() => {
            cx().msg("TODO: Unicode escape can only have 6 hex digits")
        }
        Some(c) => cx().msg(format_args!("TODO: '{c}' is not a hex digit")),
        None => cx().msg("TODO: Missing }"),
    };
    let bad_char = || cx().msg("TODO: Invalid char representation");
    let bad_hex = |s: &str| match s.chars().next_back() {
        Some('}') => cx().msg("TODO: Unicode escape must have at least 1 hex digit"),
        Some(c) if !c.is_ascii_hexdigit() => {
            cx().msg(format_args!("TODO: '{c}' is not a hex digit"))
        }
        _ => panic!(),
    };

    cursor.check('{').with_error(no_lcur).pass().ok()?;
    // Should this variable be inlined?
    let hex_start = cursor.position();
    let hex = cursor
        .eat_while_range(1..=6, |c| char::is_ascii_hexdigit(&c))
        .with_error(bad_hex)
        .pass()
        .ok()?;
    // While breaking here will not eat the trailing },
    // it doesn't really matter considering the string will
    // not be converted into a real value
    let hex_end = cursor.position();

    cursor.check('}').with_error(no_rcur).pass().ok()?;

    // Is this too weird?
    char::from_u32(u32::from_str_radix(hex, 16).unwrap()).or_else(|| {
        cursor.error_range(hex_start, hex_end, bad_char());
        None
    })
}

// Really convoluted function, but I'm 99% sure its *actually* spec compliant. Even the playground
// (at the time of writing) isn't totally spec compliant when dealing with some convoluted inputs.
fn get_dedent_whitespace(input: &str) -> Result<&str, KdlParseError> {
    // The input already has the quotes stripped.
    let mut chars = input[..].char_indices();

    let mut end_idx = input.len();
    let mut nl_idx = None;
    // `err_idx` is the index *after* the character the loop breaks on and can be used as an error
    // end location.
    let err_idx = loop {
        // Move to the first non-space character. If there is no remaining non-space character,
        // it means the start of the string was reached.
        let Some((idx, ch)) = chars.rfind(|v| !in_charset!(spaces => v.1)) else {
            break 0;
        };
        match ch {
            // This whitespace is maybe being ws escaped. We'll need to count
            // the number of consecutive backslashes to be sure.
            '\\' => {
                let mut count = 1;
                for (_, c) in chars.clone().rev() {
                    if c != '\\' {
                        break;
                    }
                    count += 1;
                }

                // If an even number of consecutive backslashes were found,
                // this is not actually a whitespace escape, and instead a bunch
                // backslashes and someone trying to limit test the parser.
                if count % 2 == 0 {
                    break idx + ch.len_utf8();
                }
                // This was a ws escape, so clear the found newline and reset the
                // end index.
                nl_idx = None;
                end_idx = idx;

                if count > 1 {
                    // This was a bunch of escapes *followed* by a ws escape. This construct
                    // is never valid, so just clear nl_idx and break from the loop.
                    // Also advance the char iterator to simulate finding the next
                    // non-special character.
                    break chars
                        .next_back()
                        .map(|(i, c)| i + c.len_utf8())
                        .unwrap_or(0);
                }
            }
            // If we found a newline, mark the important newline. Note that because this scans
            // backwards, \r\n newlines will get marked on the \n, which is correct.
            charset!(newlines) => {
                nl_idx = nl_idx.or(Some(idx));
            }
            // Otherwise, we found a non-whitespace character that cannot be a ws escape.
            _ => break idx + ch.len_utf8(),
        }
    };

    let Some(nl_idx) = nl_idx else {
        let line_start_idx = chars
            .rfind(|(_, c)| in_charset!(newlines => *c))
            .map(|(i, c)| i + c.len_utf8())
            .unwrap_or(0);
        // No valid newline index was found, which means no dedent whitespace exists.
        return Err(KdlParseError::from_span_and_ctx(
            (line_start_idx..err_idx).into(),
            cx().msg("The last line must be only whitespace"),
        ));
    };

    Ok(&input[(nl_idx + 1)..end_idx])
}

fn parse_raw_multiline_string(input: &str) -> Result<String, Vec<KdlParseError>> {
    let no_nl = |_| cx().msg("TODO: Multiline string must start with a newline");
    let bad_dedent = |_| cx().msg("TODO: Dedent error");
    // This kinda sucks
    let hashless_input = input.trim_matches('#');
    let start_hashes = hashless_input.as_ptr().addr() - input.as_ptr().addr();
    let end_hashes = input.len() - hashless_input.len() - start_hashes;

    let mut cursor = Cursor::new(&input[..(input.len() - end_hashes - 3)]);

    if start_hashes != end_hashes {
        cursor.add_raw_error(KdlParseError::from_span_and_ctx(
            (0..start_hashes).into(),
            cx().msg("TODO: Unbalanced hashes"),
        ));
        // Would be nice to add something to highlight the end hashes as well
    }
    if !hashless_input.ends_with("\"\"\"") {
        return Err(vec![KdlParseError {
            span: Some((0..(start_hashes + 3)).into()),
            message: Some("TODO: Unclosed multiline string".into()),
            ..Default::default()
        }]);
    }

    let mut buffer = String::with_capacity(hashless_input.len() - 6);
    // Move past the hashes + 3 quotes (these always get lexed correctly)
    cursor.advance(start_hashes + 3);
    // Check for the starting newline
    let init_nl = cursor
        .check_fn(in_charset!(newlines))
        .with_error(no_nl)
        .pass();
    if init_nl == Ok('\r') && cursor.peek() == Some('\n') {
        cursor.eat();
    }

    let dedent = get_raw_dedent_whitespace(&hashless_input[3..(hashless_input.len() - 3)])
        .unwrap_or_else(|mut err| {
            // An error with the initial newlines will cause a dedent error, but
            // that error is a bit more cryptic so no point in pushing it.
            if init_nl.is_err() {
                return "";
            }
            err.span = err
                .span
                .map(|v| (v.offset() + start_hashes + 3, v.len()).into());
            cursor.add_raw_error(err);
            ""
        });

    while !cursor.eof() {
        let buffer_line_start = buffer.len();
        let mut only_whitespace = true;
        let dedent_result = cursor.check_str(dedent).inner();

        while let Some(c) = cursor.eat() {
            match c {
                charset!(newlines) => {
                    buffer.push('\n');
                    break;
                }
                charset!(spaces) => (),
                _ => {
                    only_whitespace = false;
                }
            }
            buffer.push(c);
        }

        if only_whitespace && buffer.len() > buffer_line_start {
            buffer.truncate(buffer_line_start);
            buffer.push('\n');
        } else if let Err((e, span)) = dedent_result {
            cursor.add_raw_error(KdlParseError::from_span_and_ctx(span, bad_dedent(e)));
        }
    }

    buffer.pop();
    cursor.into_errors()?;

    Ok(buffer)
}

// Much simpler
fn get_raw_dedent_whitespace(input: &str) -> Result<&str, KdlParseError> {
    // The input already has the quotes stripped.
    let mut chars = input[..].char_indices();

    let (idx, c) = chars
        .rfind(|v| !in_charset!(spaces => v.1))
        .unwrap_or((0, '\0'));
    if !in_charset!(newlines => c) {
        let line_start_idx = chars
            .rfind(|(_, c)| in_charset!(newlines => *c))
            .map(|(i, c)| i + c.len_utf8())
            .unwrap_or(0);

        return Err(KdlParseError::from_span_and_ctx(
            (line_start_idx..(idx + c.len_utf8())).into(),
            cx().msg("The last line must be only whitespace"),
        ));
    }

    Ok(&input[(idx + c.len_utf8())..])
}

fn line_spaces(input: &mut TokenStream<'_>) -> PResult<()> {
    repeat(1.., alt((line_terminator, node_space))).parse_next(input)
}

fn line_terminator(input: &mut TokenStream<'_>) -> PResult<()> {
    one_of([Newline, SingleComment]).void().parse_next(input)
}

fn node_space(input: &mut TokenStream<'_>) -> PResult<()> {
    alt(((wsp, opt(escline)).void(), escline)).parse_next(input)
}

fn node_space1(input: &mut TokenStream<'_>) -> PResult<()> {
    repeat(1.., node_space).parse_next(input)
}

fn node_space0(input: &mut TokenStream<'_>) -> PResult<()> {
    repeat(0.., node_space).parse_next(input)
}

fn ws(input: &mut TokenStream<'_>) -> PResult<()> {
    one_of([Space, MultiComment]).void().parse_next(input)
}

fn wss(input: &mut TokenStream<'_>) -> PResult<()> {
    repeat(0.., ws).map(|()| ()).parse_next(input)
}

fn wsp(input: &mut TokenStream<'_>) -> PResult<()> {
    repeat(1.., ws).map(|()| ()).parse_next(input)
}

fn escline(input: &mut TokenStream<'_>) -> PResult<()> {
    (
        Escline,
        (wss, alt((line_terminator, eof.void())), wss).cut(),
    )
        .void()
        .trace("escline")
        .parse_next(input)
}

// The stuff following the slashdash is not cut because it would make working with child
// entries/nodes a bit more awkward. That being said, maybe its worth putting in that
// effort to make it cleaner?
fn slashdashed<'src, O>(
    mut parser: impl Parser<TokenStream<'src>, O, PError>,
) -> impl Parser<TokenStream<'src>, O, PError> {
    move |input: &mut TokenStream<'src>| {
        (Slashdash, opt(line_spaces), parser.by_ref())
            .trace("slashdash")
            .parse_next(input)
            .map(|(_, _, o)| o)
    }
}
