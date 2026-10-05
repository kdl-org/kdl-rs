# kdlfmt

A formatter for [kdl](https://kdl.dev) documents.

kdlfmt is a thin cli wrapper built on-top of the official `kdl` parser for Rust ([`kdl-rs`](https://github.com/kdl-org/kdl-rs)), so any formatting/parsing issues should be reported there.

```
kdlfmt
A code formatter for kdl documents.

Usage: kdlfmt [OPTIONS] <COMMAND>

Commands:
  format       Format kdl files
  check        Validate files are formatted
  init         Initialize formatter config
  completions  Generate shell completions
  help         Print this message or the help of the given subcommand(s)

Options:
      --log-level <LOG_LEVEL>  [possible values: trace, debug, info, warn, error, off]
  -h, --help                   Print help
  -V, --version                Print version
```

## Installation

### Using Cargo

kdlfmt can be installed using Cargo, the package manager for Rust ([crates.io](https://crates.io/crates/kdlfmt)).

```shell
cargo install kdlfmt
```

### Using npm/npx

You can install `kdlfmt` using [npm](https://www.npmjs.com/package/kdlfmt):

```shell
npm install -g kdlfmt

kdlfmt format .
```

Or run it directly using npx:

```shell
npx kdlfmt format .
```

### Precompiled Binaries

Precompiled binaries can be found on the [release](https://github.com/kdl-org/kdl-rs/releases) page.

## Other package managers

A non-complete list of other package managers with support for installing kdlfmt can be found at [Repology](https://repology.org/project/kdlfmt).

[![Packaging status](https://repology.org/badge/vertical-allrepos/kdlfmt.svg?columns=3)](https://repology.org/project/kdlfmt/versions)

## Usage

Once installed the formatted can be invoked by running `kdlfmt format`.

```shell
kdlfmt format PATH
```

Or reading from stdin and printing the formatted output to stdout.

```shell
cat somefile.kdl | kdlfmt format -
```

```
Format kdl files

Usage: kdlfmt format [OPTIONS] [INPUT]...

Arguments:
  [INPUT]...
          Path to file OR directory.

          Use "-" to read from stdin and print to stdout.

Options:
      --kdl-version <KDL_VERSION>
          kdl specification to use.

          By default all versions are tried

          [possible values: v1, v2]

      --stdin
          Read from stdin and print to stdout

      --config <CONFIG>
          Path to config file

      --log-level <LOG_LEVEL>
          [possible values: trace, debug, info, warn, error, off]

  -h, --help
          Print help (see a summary with '-h')

  -V, --version
          Print version
```

### Specifying KDL version

This tool supports both KDL version 1 and version 2.

KDL version 2 is NOT 100% backward compatible with version 1, so knowing which version you wish to format using is important.

By default this tool tries to format using version 2 first, and then version 1.

To specify which version you wish to use supply the `--kdl-version` argument to the command.

### Validating files are formatted

kdlfmt also support validating if files are formatted using the `kdlfmt check` command.

```shell
kdlfmt check PATH
```

Or reading from stdin.

```shell
cat somefile.kdl | kdlfmt check -
```

```
Validate files are formatted

Usage: kdlfmt check [OPTIONS] [INPUT]...

Arguments:
  [INPUT]...
          Path to file OR directory.

          Use "-" to read from stdin and print to stdout.

Options:
      --kdl-version <KDL_VERSION>
          kdl specification to use.

          By default all versions are tried

          [possible values: v1, v2]

      --stdin
          Read from stdin and print to stdout

      --config <CONFIG>
          Path to config file

      --log-level <LOG_LEVEL>
          [possible values: trace, debug, info, warn, error, off]

  -h, --help
          Print help (see a summary with '-h')

  -V, --version
          Print version
```

### Ignoring files

`.kdlfmtignore` files are used to ignore files/directories.

It uses the same syntax as `.gitignore` files.

### Configuration

A sample configuration file can be created by running `kdlfmt init`.

```kdl
// Amount of spaces to use for each indentation level
indent_size 4
// Whether to use tabs or spaces for indentation
use_tabs #false
```

### GitHub Action

There are a lot of different ways to run `kdlfmt` using GitHub actions.

The easiest way, in my opinion, is to use the official GitHub action to install `kdlfmt`.

After that you can run the binary like you would in your terminal.

```yaml
name: kdlfmt
on:
    - push
jobs:
    format:
        runs-on: ubuntu-latest
        steps:
            - name: Checkout repository
              uses: actions/checkout@v4

            - name: Install kdlfmt
              uses: hougesen/kdlfmt@main

            - name: Run kdlfmt format
              run: kdlfmt format .

            - name: Commit changes
              uses: EndBug/add-and-commit@v9
              with:
                  message: "style: formatted kdl"
```

### pre-commit

See [pre-commit](https://github.com/pre-commit/pre-commit) for instructions

Sample `.pre-commit-config.yaml`:

```yaml
repos:
    - repo: https://github.com/hougesen/kdlfmt
      rev: main
      hooks:
          - id: kdlfmt-format
```

### treefmt

Add the following to your `treefmt.toml` to run kdlfmt using [treefmt](https://github.com/numtide/treefmt).

```toml
# treefmt.toml

[formatter.kdlfmt]
command = "kdlfmt"
options = ["format"]
includes = ["*.kdl"]
```

### mdsf

[mdsf](https://github.com/hougesen/mdsf) has built in support for kdlfmt, and can easily be used by adding the following to your `mdsf.json` config file.

```json
{
    "kdl": "kdlfmt"
}
```

### Shell completion

Shell completion can be generated using the `kdlfmt completions` command.

```
Generate shell completions

Usage: kdlfmt completions [OPTIONS] <SHELL>

Arguments:
  <SHELL>  [possible values: bash, elvish, fish, nushell, powershell, zsh]

Options:
      --log-level <LOG_LEVEL>  [possible values: trace, debug, info, warn, error, off]
  -h, --help                   Print help
  -V, --version                Print version
```

#### Bash

Add the following to your `.bashrc`.

```bash
eval "$(kdlfmt completions bash)"
```

#### Zsh

Add the following to your `.zshrc`:

```zsh
eval "$(kdlfmt completions zsh)"
```

#### Fish

Add the following to `~/.config/fish/config.fish`.

```fish
kdlfmt completions fish | source
```

#### PowerShell

Add the following to your PowerShell configuration (Can be found by running `$PROFILE`).

```powershell
Invoke-Expression (&kdlfmt completions powershell)
```

#### Elvish

Add the following to `~/.elvish/rc.elv`.

```elvish
eval (kdlfmt completions elvish)
```

#### Nushell

Generate completions for [nushell](https://github.com/nushell/nushell).

```nushell
kdlfmt completions nushell
```
