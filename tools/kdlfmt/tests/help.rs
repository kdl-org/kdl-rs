use predicates::prelude::PredicateBooleanExt;

#[test]
fn base_help_arg_outputs_message() {
    assert_cmd::cargo_bin_cmd!("kdlfmt")
        .arg("--help")
        .assert()
        .success()
        .stdout(predicates::str::is_empty().not());
}

#[test]
fn base_version_arg_outputs_message() {
    assert_cmd::cargo_bin_cmd!("kdlfmt")
        .arg("--version")
        .assert()
        .success()
        .stdout(predicates::str::is_empty().not());
}

#[test]
fn help_command_outputs_help_message() {
    assert_cmd::cargo_bin_cmd!("kdlfmt")
        .arg("help")
        .assert()
        .success()
        .stdout(predicates::str::is_empty().not());
}
