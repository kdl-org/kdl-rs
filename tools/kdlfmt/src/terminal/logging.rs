use std::io::Write;

use console::style;
use log::LevelFilter;

use crate::cli::LogLevel;

impl From<LogLevel> for LevelFilter {
    fn from(value: LogLevel) -> Self {
        match value {
            LogLevel::Off => Self::Off,
            LogLevel::Error => Self::Error,
            LogLevel::Warn => Self::Warn,
            LogLevel::Info => Self::Info,
            LogLevel::Debug => Self::Debug,
            LogLevel::Trace => Self::Trace,
        }
    }
}

pub fn setup_logger(log_level: LogLevel) {
    env_logger::Builder::from_env("KDLFMT_LOG")
        .filter(None, LevelFilter::Off)
        .filter_module("kdlfmt::terminal", log_level.into())
        .format_timestamp(None)
        .format_module_path(false)
        .format_target(false)
        .format_level(false)
        .format(move |buf, record| match record.level() {
            log::Level::Error => {
                writeln!(buf, "{}", style(format!("{}", record.args())).red().bold())
            }
            log::Level::Warn => writeln!(
                buf,
                "{}",
                style(format!("{}", record.args())).yellow().bold()
            ),
            log::Level::Info => writeln!(buf, "{}", record.args()),
            log::Level::Debug | log::Level::Trace => {
                writeln!(buf, "{}", style(format!("{}", record.args())).dim())
            }
        })
        .init();
}
