use ec4rs::property::IndentStyle;
use kdl::{FormatConfig, KdlDocument};

use crate::{error::KdlFmtError, kdl::parse_kdl};

#[derive(Debug, Clone)]
pub struct KdlFmtConfig {
    from_kdlfmt_file: bool,
    pub indent: String,
    pub use_tabs: bool,
}

impl Default for KdlFmtConfig {
    fn default() -> Self {
        Self {
            from_kdlfmt_file: false,
            indent: FormatConfig::default().indent.to_string(),
            use_tabs: false,
        }
    }
}

impl KdlFmtConfig {
    pub const fn filename() -> &'static str {
        "kdlfmt.kdl"
    }

    fn parse_config(config: &str) -> miette::Result<KdlDocument> {
        parse_kdl(config, None).map(|(doc, _version)| doc)
    }

    pub fn load(path: Option<&std::path::PathBuf>) -> Result<Self, KdlFmtError> {
        let mut config = Self::default();

        let config_result = path.map_or_else(
            || std::fs::read_to_string(Self::filename()),
            std::fs::read_to_string,
        );

        if let Ok(config_str) = config_result {
            // TODO: custom parse error
            let doc = Self::parse_config(&config_str)
                .map_err(|error| KdlFmtError::ParseKdl(None, error))?;

            if doc
                .get_arg(Self::use_tabs_key())
                .and_then(kdl::KdlValue::as_bool)
                == Some(true)
            {
                config.use_tabs = true;
                config.indent = Self::get_indent(1, true);
                config.from_kdlfmt_file = true;
            }

            if let Some(indent_size) = doc
                .get_arg(Self::indent_size_key())
                .and_then(kdl::KdlValue::as_integer)
            {
                config.indent = Self::get_indent(indent_size.max(1) as usize, config.use_tabs);
                config.from_kdlfmt_file = true;
            }
        }

        Ok(config)
    }

    pub fn get_indent(indent: usize, use_tabs: bool) -> String {
        let mut spaces = String::new();

        for _ in 0..indent {
            if use_tabs {
                spaces.push('\t');
            } else {
                spaces.push(' ');
            }
        }

        spaces
    }

    pub const fn indent_size_key() -> &'static str {
        "indent_size"
    }

    pub const fn use_tabs_key() -> &'static str {
        "use_tabs"
    }

    pub fn get_editorconfig_or_default(&self, path: &std::path::Path) -> Self {
        if !self.from_kdlfmt_file
            && let Ok(mut properties) = ec4rs::properties_of(path)
        {
            properties.use_fallbacks();

            let use_tabs = properties
                .get::<IndentStyle>()
                .is_ok_and(|indent_style| matches!(indent_style, IndentStyle::Tabs));

            let indent_size = properties.get::<ec4rs::property::IndentSize>().map_or(
                if use_tabs { 1 } else { self.indent.len() },
                |value| match value {
                    ec4rs::property::IndentSize::Value(value) => value,
                    ec4rs::property::IndentSize::UseTabWidth => {
                        if let Ok(ec4rs::property::TabWidth::Value(value)) =
                            properties.get::<ec4rs::property::TabWidth>()
                        {
                            value
                        } else {
                            1
                        }
                    }
                },
            );

            let indent = Self::get_indent(indent_size, use_tabs);

            return Self {
                use_tabs,
                indent,
                from_kdlfmt_file: false,
            };
        }

        self.clone()
    }

    pub fn get_formatter_config(&self) -> FormatConfig<'_> {
        kdl::FormatConfig::builder().indent(&self.indent).build()
    }
}
