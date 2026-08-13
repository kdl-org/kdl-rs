use predicates::prelude::PredicateBooleanExt;

fn completions_command() -> assert_cmd::Command {
    let mut cmd = assert_cmd::cargo_bin_cmd!("kdlfmt");

    cmd.arg("completions");

    cmd
}

#[test]
fn help_arg_outputs_message() {
    completions_command()
        .arg("--help")
        .assert()
        .success()
        .stdout(predicates::str::is_empty().not());
}

#[test]
fn outputs_shell_completions() {
    let shells = ["bash", "elvish", "fish", "nushell", "powershell", "zsh"];

    for shell in shells {
        completions_command()
            .arg(shell)
            .assert()
            .success()
            .stdout(predicates::str::is_empty().not());
    }
}
