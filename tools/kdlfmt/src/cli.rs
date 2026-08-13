use std::io::Read;

use clap::{Args, Parser, Subcommand};

const HELP_TEMPLATE: &str = "\
{before-help}{name} {version}
{about-with-newline}{author-with-newline}
{usage-heading} {usage}

{all-args}{after-help}
";

#[derive(Debug, Parser)]
#[command(author, version, about, long_about = None, propagate_version = true, help_template = HELP_TEMPLATE)]
pub struct Cli {
    #[command(subcommand)]
    pub command: Commands,

    #[arg(long, value_enum, global = true)]
    pub log_level: Option<LogLevel>,
}

#[derive(Debug, Subcommand)]
pub enum Commands {
    /// Format kdl files
    Format(FormatCommandArguments),

    /// Validate files are formatted
    Check(FormatCommandArguments),

    /// Initialize formatter config
    Init(InitCommandArguments),

    /// Generate shell completions
    Completions(ShellCompletionCommandArguments),
}

#[derive(Args, Debug)]
pub struct FormatCommandArguments {
    /// Path to file OR directory.
    ///
    /// Use "-" to read from stdin and print to stdout.
    #[arg()]
    pub input: Vec<String>,

    /// kdl specification to use.
    ///
    /// By default all versions are tried
    #[arg(long, value_enum)]
    pub kdl_version: Option<KdlVersion>,

    /// Read from stdin and print to stdout.
    #[arg(long)]
    pub stdin: bool,

    /// Path to config file.
    #[arg(long)]
    pub config: Option<std::path::PathBuf>,
}

#[derive(clap::ValueEnum, Clone, Copy, PartialEq, Eq, Debug)]
pub enum LogLevel {
    Trace,
    Debug,
    Info,
    Warn,
    Error,
    Off,
}

#[derive(Clone, Copy, PartialEq, Eq, Debug, Hash)]
pub enum Shell {
    /// Bourne Again `SHell` (bash).
    Bash,

    /// Elvish shell (elvish).
    Elvish,

    /// Friendly Interactive `SHell` (fish).
    Fish,

    /// `Nushell` (nushell).
    Nushell,

    /// `PowerShell` (powershell).
    Powershell,

    /// Z `SHell` (zsh).
    Zsh,
}

impl clap::ValueEnum for Shell {
    fn value_variants<'a>() -> &'a [Self] {
        &[
            Self::Bash,
            Self::Elvish,
            Self::Fish,
            Self::Nushell,
            Self::Powershell,
            Self::Zsh,
        ]
    }

    fn to_possible_value(&self) -> Option<clap::builder::PossibleValue> {
        Some(match self {
            Self::Bash => clap::builder::PossibleValue::new("bash"),
            Self::Elvish => clap::builder::PossibleValue::new("elvish"),
            Self::Fish => clap::builder::PossibleValue::new("fish"),
            Self::Nushell => clap::builder::PossibleValue::new("nushell"),
            Self::Powershell => clap::builder::PossibleValue::new("powershell"),
            Self::Zsh => clap::builder::PossibleValue::new("zsh"),
        })
    }
}

#[derive(Args, Debug)]
pub struct ShellCompletionCommandArguments {
    #[arg()]
    pub shell: Shell,
}

pub fn read_stdin() -> std::io::Result<String> {
    let stdin = std::io::stdin();

    let mut input = String::new();

    stdin.lock().read_to_string(&mut input)?;

    Ok(input)
}

#[derive(Args, Debug)]
pub struct InitCommandArguments {
    /// Create config even if one already exists in current directory.
    #[arg(long, default_value_t = false)]
    pub force: bool,

    /// kdl specification to use.
    ///
    /// By default all versions are tried
    #[arg(long, value_enum)]
    pub kdl_version: Option<KdlVersion>,
}

#[derive(clap::ValueEnum, Clone, Copy, PartialEq, Eq, Debug, Default)]
pub enum KdlVersion {
    V1,
    #[default]
    V2,
}
