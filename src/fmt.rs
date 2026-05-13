use std::fmt::Write as _;

/// Formatting configuration for use with [`KdlDocument::autoformat_config`](`crate::KdlDocument::autoformat_config`)
/// and [`KdlNode::autoformat_config`](`crate::KdlNode::autoformat_config`).
#[non_exhaustive]
#[derive(Debug)]
pub struct FormatConfig<'a> {
    /// How deeply to indent the overall node or document,
    /// in repetitions of [`indent`](`FormatConfig::indent`).
    /// Defaults to `0`.
    pub indent_level: usize,

    /// The indentation to use at each level. Defaults to four spaces.
    pub indent: &'a str,

    /// Whether to remove comments. Defaults to `false`.
    pub no_comments: bool,

    /// Whether to keep individual entry formatting.
    pub entry_autoformate_keep: bool,
}

/// See field documentation for defaults.
impl Default for FormatConfig<'_> {
    fn default() -> Self {
        Self::builder().build()
    }
}

impl FormatConfig<'_> {
    /// Creates a new [`FormatConfigBuilder`] with default configuration.
    pub const fn builder() -> FormatConfigBuilder<'static> {
        FormatConfigBuilder::new()
    }
}

/// A [`FormatConfig`] builder.
///
/// Note that setters can be repeated.
#[derive(Debug, Default)]
pub struct FormatConfigBuilder<'a>(FormatConfig<'a>);

impl<'a> FormatConfigBuilder<'a> {
    /// Creates a new [`FormatConfig`] builder with default configuration.
    pub const fn new() -> Self {
        Self(FormatConfig {
            indent_level: 0,
            indent: "    ",
            no_comments: false,
            entry_autoformate_keep: false,
        })
    }

    /// How deeply to indent the overall node or document,
    /// in repetitions of [`indent`](`FormatConfig::indent`).
    /// Defaults to `0` iff not specified.
    pub const fn maybe_indent_level(mut self, indent_level: Option<usize>) -> Self {
        if let Some(indent_level) = indent_level {
            self.0.indent_level = indent_level;
        }
        self
    }

    /// How deeply to indent the overall node or document,
    /// in repetitions of [`indent`](`FormatConfig::indent`).
    /// Defaults to `0` iff not specified.
    pub const fn indent_level(mut self, indent_level: usize) -> Self {
        self.0.indent_level = indent_level;
        self
    }

    /// The indentation to use at each level.
    /// Defaults to four spaces iff not specified.
    pub const fn maybe_indent<'b, 'c>(self, indent: Option<&'b str>) -> FormatConfigBuilder<'c>
    where
        'a: 'b,
        'b: 'c,
    {
        if let Some(indent) = indent {
            self.indent(indent)
        } else {
            self
        }
    }

    /// The indentation to use at each level.
    /// Defaults to four spaces if not specified.
    pub const fn indent(self, indent: &str) -> FormatConfigBuilder<'_> {
        FormatConfigBuilder(FormatConfig { indent, ..self.0 })
    }

    /// Whether to remove comments.
    /// Defaults to `false` iff not specified.
    pub const fn maybe_no_comments(mut self, no_comments: Option<bool>) -> Self {
        if let Some(no_comments) = no_comments {
            self.0.no_comments = no_comments;
        }
        self
    }

    /// Whether to remove comments.
    /// Defaults to `false` iff not specified.
    pub const fn no_comments(mut self, no_comments: bool) -> Self {
        self.0.no_comments = no_comments;
        self
    }

    /// Builds the [`FormatConfig`].
    pub const fn build(self) -> FormatConfig<'a> {
        self.0
    }
}

pub(crate) fn autoformat_leading(leading: &mut String, config: &FormatConfig<'_>) {
    let mut result = String::new();
    if !config.no_comments {
        let input = leading.trim();
        if !input.is_empty() {
            for line in input.lines() {
                let trimmed = line.trim();
                if !trimmed.is_empty() {
                    for _ in 0..config.indent_level {
                        result.push_str(config.indent);
                    }
                    writeln!(result, "{trimmed}").unwrap();
                }
            }
        }
    }
    for _ in 0..config.indent_level {
        result.push_str(config.indent);
    }
    *leading = result;
}

pub(crate) fn autoformat_trailing(decor: &mut String, no_comments: bool) {
    if decor.is_empty() {
        return;
    }
    let starts_on_own_line = decor.starts_with(['\n', '\r']);
    // Preserve leading whitespace/newlines only for comments that are intended
    // to remain on their own lines. Same-line decor should still normalize to a
    // single separator before the comment.
    *decor = if starts_on_own_line {
        decor.trim_end()
    } else {
        decor.trim()
    }
    .to_string();
    let mut result = String::new();
    if !decor.is_empty() && !no_comments {
        if !starts_on_own_line && decor.trim_start() == &decor[..] {
            write!(result, " ").unwrap();
        }
        for comment in decor.lines() {
            writeln!(result, "{comment}").unwrap();
        }
    }
    *decor = result;
}

/// Removes parsed line-continuation backslashes before formatting node-boundary decor.
pub(crate) fn strip_leading_line_escape(decor: &mut String) {
    while let Some((start, end)) = line_escape_range(decor) {
        decor.replace_range(start..end, "");
    }
}

fn line_escape_range(decor: &str) -> Option<(usize, usize)> {
    let mut idx = 0;
    while idx < decor.len() {
        let rest = &decor[idx..];
        if rest.starts_with("/*") {
            idx = skip_multiline_comment(decor, idx)?;
            continue;
        }

        let ch = rest.chars().next()?;
        if ch == '\\' {
            if let Some(end) = line_escape_end(decor, idx + ch.len_utf8()) {
                return Some((idx, end));
            }
        }
        idx += ch.len_utf8();
    }
    None
}

fn line_escape_end(decor: &str, mut idx: usize) -> Option<usize> {
    while idx < decor.len() {
        let rest = &decor[idx..];
        if rest.starts_with("/*") {
            idx = skip_multiline_comment(decor, idx)?;
            continue;
        }

        let ch = rest.chars().next()?;
        if is_unicode_space(ch) {
            idx += ch.len_utf8();
            continue;
        }

        return newline_len(rest).map(|len| trim_start_idx(decor, idx + len));
    }
    None
}

fn skip_multiline_comment(decor: &str, start: usize) -> Option<usize> {
    let mut depth = 1;
    let mut idx = start + "/*".len();
    while idx < decor.len() {
        let rest = &decor[idx..];
        if rest.starts_with("/*") {
            depth += 1;
            idx += "/*".len();
        } else if rest.starts_with("*/") {
            depth -= 1;
            idx += "*/".len();
            if depth == 0 {
                return Some(idx);
            }
        } else {
            idx += rest.chars().next()?.len_utf8();
        }
    }
    None
}

fn newline_len(rest: &str) -> Option<usize> {
    [
        "\r\n", "\r", "\n", "\u{0085}", "\u{000B}", "\u{000C}", "\u{2028}", "\u{2029}",
    ]
    .iter()
    .find_map(|newline| rest.starts_with(newline).then_some(newline.len()))
}

fn trim_start_idx(decor: &str, mut idx: usize) -> usize {
    while idx < decor.len() {
        let rest = &decor[idx..];
        let Some(ch) = rest.chars().next() else {
            return idx;
        };
        if !is_unicode_space(ch) {
            return idx;
        }
        idx += ch.len_utf8();
    }
    idx
}

fn is_unicode_space(ch: char) -> bool {
    matches!(
        ch,
        '\u{0009}'
            | '\u{0020}'
            | '\u{00A0}'
            | '\u{1680}'
            | '\u{2000}'
            | '\u{2001}'
            | '\u{2002}'
            | '\u{2003}'
            | '\u{2004}'
            | '\u{2005}'
            | '\u{2006}'
            | '\u{2007}'
            | '\u{2008}'
            | '\u{2009}'
            | '\u{200A}'
            | '\u{202F}'
            | '\u{205F}'
            | '\u{3000}'
    )
}

pub(crate) fn autoformat_indented_trailing(decor: &mut String, config: &FormatConfig<'_>) {
    if decor.is_empty() {
        return;
    }
    *decor = decor.trim().to_string();
    if decor == "\\" {
        // A line escape only affects source layout. Once autoformatting has
        // normalized node boundaries, keeping it would emit a stray backslash.
        decor.clear();
        return;
    }
    *decor = format_indented_comment_lines(decor, config, "");
}

pub(crate) fn autoformat_node_terminator(terminator: &mut String, config: &FormatConfig<'_>) {
    if !terminator.starts_with('\n') {
        let decor = terminator.trim();
        // Same-line `//` comments are node terminators in v2; keep them on the
        // node line unless the caller is explicitly stripping comments.
        if !config.no_comments && decor.starts_with("//") {
            *terminator = format!(" {decor}\n");
            return;
        }
        *terminator = "\n".into();
        return;
    }

    let decor = terminator.trim();
    *terminator = format_indented_comment_lines(decor, config, "\n");
    if terminator.is_empty() {
        terminator.push('\n');
    }
}

fn format_indented_comment_lines(decor: &str, config: &FormatConfig<'_>, prefix: &str) -> String {
    let mut result = String::from(prefix);
    if decor.is_empty() || config.no_comments {
        return result;
    }

    for line in decor.lines() {
        let trimmed = line.trim();
        if !trimmed.is_empty() {
            push_indent(&mut result, config);
            writeln!(result, "{trimmed}").unwrap();
        }
    }
    result
}

fn push_indent(result: &mut String, config: &FormatConfig<'_>) {
    for _ in 0..config.indent_level {
        result.push_str(config.indent);
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn builder() -> miette::Result<()> {
        let built = FormatConfig::builder()
            .indent_level(12)
            .indent(" \t")
            .no_comments(true)
            .build();
        assert!(matches!(
            built,
            FormatConfig {
                indent_level: 12,
                indent: " \t",
                no_comments: true,
                entry_autoformate_keep: false,
            }
        ));
        Ok(())
    }
}
