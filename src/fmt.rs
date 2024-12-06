use std::fmt::Write as _;

/// Formatting configuration for use with [`KdlDocument::autoformat_config`](`crate::KdlDocument::autoformat_config`)
/// and [`KdlNode::autoformat_config`](`crate::KdlNode::autoformat_config`).
#[non_exhaustive]
#[derive(Debug, bon::Builder)]
pub struct FormatConfig<'a> {
    /// How deeply to indent the overall node or document,
    /// in repetitions of [`indent`](`FormatConfig::indent`).
    /// Defaults to `0`.
    #[builder(default = 0)]
    pub indent_level: usize,

    /// The indentation to use at each level. Defaults to four spaces.
    #[builder(default = "    ")]
    pub indent: &'a str,

    /// Whether to remove comments. Defaults to `false`.
    #[builder(default = false)]
    pub no_comments: bool,
}

/// See field documentation for defaults.
impl Default for FormatConfig<'_> {
    fn default() -> Self {
        Self::builder().build()
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
                    writeln!(result, "{}", trimmed).unwrap();
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
    *decor = decor.trim().to_string();
    let mut result = String::new();
    if !decor.is_empty() && !no_comments {
        if decor.trim_start() == &decor[..] {
            write!(result, " ").unwrap();
        }
        for comment in decor.lines() {
            writeln!(result, "{comment}").unwrap();
        }
    }
    *decor = result;
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
            }
        ));
        Ok(())
    }
}
