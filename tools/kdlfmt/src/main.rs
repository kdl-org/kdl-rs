use clap::Parser;
use cli::Cli;
use commands::execute_command;
use terminal::{logging::setup_logger, print_error};

mod cli;
mod commands;
mod config;
mod error;
mod fs;
mod kdl;
mod terminal;

fn main() {
    let cli = Cli::parse();

    setup_logger(cli.log_level.unwrap_or(cli::LogLevel::Debug));

    if let Err(error) = execute_command(cli.command) {
        print_error(&error);

        std::process::exit(1);
    }
}
