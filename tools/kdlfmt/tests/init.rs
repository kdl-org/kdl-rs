use predicates::prelude::PredicateBooleanExt;

fn init_command(path: Option<&std::path::Path>) -> assert_cmd::Command {
    let mut cmd = assert_cmd::cargo_bin_cmd!("kdlfmt");

    if let Some(path) = path {
        cmd.current_dir(path);
    }

    cmd.arg("init");

    cmd
}

#[test]
fn help_arg_outputs_message() -> std::io::Result<()> {
    let dir = tempfile::tempdir()?;

    init_command(Some(dir.path()))
        .arg("--help")
        .assert()
        .success()
        .stdout(predicates::str::is_empty().not());

    Ok(())
}

#[test]
fn creates_a_config_file() -> std::io::Result<()> {
    let dir = tempfile::tempdir()?;

    init_command(Some(dir.path())).assert().success();

    let config_file_created = dir.path().join("kdlfmt.kdl").try_exists()?;

    assert!(config_file_created);

    Ok(())
}

#[test]
fn fails_if_config_exists() -> std::io::Result<()> {
    let dir = tempfile::tempdir()?;

    init_command(Some(dir.path())).assert().success();

    init_command(Some(dir.path())).assert().failure();

    Ok(())
}

#[test]
fn force_config_arg() -> std::io::Result<()> {
    let dir = tempfile::tempdir()?;

    init_command(Some(dir.path())).assert().success();

    init_command(Some(dir.path())).assert().failure();

    init_command(Some(dir.path()))
        .arg("--force")
        .assert()
        .success();

    Ok(())
}
