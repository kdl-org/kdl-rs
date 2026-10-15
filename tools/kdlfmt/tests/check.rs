use std::io::Write as _;

use predicates::prelude::PredicateBooleanExt;

const BROKEN_V1_CODE: &str = r#"world          {
      child  "1"
 child                "2"
   }
"#;

const FORMATTED_V1_CODE: &str = r#"world {
    child "1"
    child "2"
}
"#;

const BROKEN_V2_CODE: &str = r#"world          {
      child  "3"
 child                "4"
   }
"#;

const FORMATTED_V2_CODE: &str = r#"world {
    child "3"
    child "4"
}
"#;

const INVALID_V1_CODE: &str = r#""""""""#;

const INVALID_V2_CODE: &str = r#""""""""#;

fn kdlfmt_command(path: Option<&std::path::Path>) -> assert_cmd::Command {
    let mut cmd = assert_cmd::cargo_bin_cmd!("kdlfmt");

    if let Some(path) = path {
        cmd.current_dir(path);
    }

    cmd
}

fn setup_test_input(dir: &std::path::Path, code: &str) -> std::io::Result<tempfile::NamedTempFile> {
    let mut b = tempfile::Builder::new();

    b.rand_bytes(12).suffix(".kdl");

    let mut f = b.tempfile_in(dir)?;

    f.write_all(code.as_bytes())?;
    f.flush()?;

    Ok(f)
}

fn check_command(path: Option<&std::path::Path>) -> assert_cmd::Command {
    let mut cmd = kdlfmt_command(path);

    cmd.arg("check");

    cmd
}

#[test]
fn help_arg_outputs_message() {
    check_command(None)
        .arg("--help")
        .assert()
        .success()
        .stdout(predicates::str::is_empty().not());
}

#[cfg(test)]
mod auto {
    use crate::{
        BROKEN_V1_CODE, BROKEN_V2_CODE, FORMATTED_V1_CODE, FORMATTED_V2_CODE, INVALID_V1_CODE,
        INVALID_V2_CODE, check_command, setup_test_input,
    };

    #[test]
    fn success_with_formatted_input() -> std::io::Result<()> {
        let dir = tempfile::tempdir()?;

        {
            let file = setup_test_input(dir.path(), FORMATTED_V1_CODE)?;

            check_command(Some(dir.path()))
                .arg(file.path())
                .assert()
                .success();
        };

        {
            let file = setup_test_input(dir.path(), FORMATTED_V2_CODE)?;

            check_command(Some(dir.path()))
                .arg(file.path())
                .assert()
                .success();
        };

        Ok(())
    }

    #[test]
    fn success_with_formatted_input_stdin() {
        check_command(None)
            .arg("--stdin")
            .write_stdin(FORMATTED_V1_CODE)
            .assert()
            .success();

        check_command(None)
            .arg("--stdin")
            .write_stdin(FORMATTED_V2_CODE)
            .assert()
            .success();
    }

    #[test]
    fn fail_with_unformatted_input() -> std::io::Result<()> {
        let dir = tempfile::tempdir()?;

        {
            let file = setup_test_input(dir.path(), BROKEN_V1_CODE)?;

            check_command(Some(dir.path()))
                .arg(file.path())
                .assert()
                .failure();

            let content_after = std::fs::read_to_string(file.path())?;
            assert_eq!(content_after, BROKEN_V1_CODE);
        };

        {
            let file = setup_test_input(dir.path(), BROKEN_V2_CODE)?;

            check_command(Some(dir.path()))
                .arg(file.path())
                .assert()
                .failure();

            let content_after = std::fs::read_to_string(file.path())?;
            assert_eq!(content_after, BROKEN_V2_CODE);
        };

        Ok(())
    }

    #[test]
    fn fail_with_unformatted_input_stdin() {
        check_command(None)
            .arg("--stdin")
            .write_stdin(BROKEN_V1_CODE)
            .assert()
            .failure();

        check_command(None)
            .arg("--stdin")
            .write_stdin(BROKEN_V2_CODE)
            .assert()
            .failure();
    }

    #[test]
    fn do_nothing_without_input() {
        check_command(None).assert().success();
    }

    #[test]
    fn accepts_multiple_paths() -> std::io::Result<()> {
        let dir = tempfile::tempdir()?;

        {
            let file1 = setup_test_input(dir.path(), FORMATTED_V1_CODE)?;
            let file2 = setup_test_input(dir.path(), FORMATTED_V1_CODE)?;

            check_command(Some(dir.path()))
                .arg(file1.path())
                .arg(file2.path())
                .assert()
                .success();
        };

        {
            let file1 = setup_test_input(dir.path(), FORMATTED_V2_CODE)?;
            let file2 = setup_test_input(dir.path(), FORMATTED_V2_CODE)?;

            check_command(Some(dir.path()))
                .arg(file1.path())
                .arg(file2.path())
                .assert()
                .success();
        };

        Ok(())
    }

    #[test]
    fn it_should_fail_if_kdl_is_invalid() -> std::io::Result<()> {
        let dir = tempfile::tempdir()?;

        {
            let file = setup_test_input(dir.path(), INVALID_V1_CODE)?;

            check_command(Some(dir.path()))
                .arg(file.path())
                .assert()
                .failure()
                .stderr(predicates::str::contains("Error parsing file "));

            let output = std::fs::read_to_string(file.path())?;

            assert_eq!(output, INVALID_V1_CODE);
        };

        {
            let file = setup_test_input(dir.path(), INVALID_V2_CODE)?;

            check_command(Some(dir.path()))
                .arg(file.path())
                .assert()
                .failure()
                .stderr(predicates::str::contains("Error parsing file "));

            let output = std::fs::read_to_string(file.path())?;

            assert_eq!(output, INVALID_V2_CODE);
        };

        Ok(())
    }

    #[test]
    fn it_should_fail_if_kdl_is_invalid_stdin() {
        check_command(None)
            .arg("--stdin")
            .write_stdin(INVALID_V1_CODE)
            .assert()
            .failure()
            .stderr(predicates::str::contains("Error parsing content"));

        check_command(None)
            .arg("--stdin")
            .write_stdin(INVALID_V2_CODE)
            .assert()
            .failure()
            .stderr(predicates::str::contains("Error parsing content"));
    }
}

#[cfg(test)]
mod v1 {
    use crate::{
        BROKEN_V1_CODE, FORMATTED_V1_CODE, INVALID_V1_CODE, check_command, setup_test_input,
    };

    #[test]
    fn success_with_formatted_input() -> std::io::Result<()> {
        let dir = tempfile::tempdir()?;

        let file = setup_test_input(dir.path(), FORMATTED_V1_CODE)?;

        check_command(Some(dir.path()))
            .arg("--kdl-version")
            .arg("v1")
            .arg(file.path())
            .assert()
            .success();

        Ok(())
    }

    #[test]
    fn success_with_formatted_input_stdin() {
        check_command(None)
            .arg("--kdl-version")
            .arg("v1")
            .arg("--stdin")
            .write_stdin(FORMATTED_V1_CODE)
            .assert()
            .success();
    }

    #[test]
    fn fail_with_unformatted_input() -> std::io::Result<()> {
        let dir = tempfile::tempdir()?;

        let file = setup_test_input(dir.path(), BROKEN_V1_CODE)?;

        check_command(Some(dir.path()))
            .arg("--kdl-version")
            .arg("v1")
            .arg(file.path())
            .assert()
            .failure();

        let content_after = std::fs::read_to_string(file.path())?;
        assert_eq!(content_after, BROKEN_V1_CODE);

        Ok(())
    }

    #[test]
    fn fail_with_unformatted_input_stdin() {
        check_command(None)
            .arg("--kdl-version")
            .arg("v1")
            .arg("--stdin")
            .write_stdin(BROKEN_V1_CODE)
            .assert()
            .failure();
    }

    #[test]
    fn do_nothing_without_input() {
        check_command(None)
            .arg("--kdl-version")
            .arg("v1")
            .assert()
            .success();
    }

    #[test]
    fn accepts_multiple_paths() -> std::io::Result<()> {
        let dir = tempfile::tempdir()?;

        let file1 = setup_test_input(dir.path(), FORMATTED_V1_CODE)?;
        let file2 = setup_test_input(dir.path(), FORMATTED_V1_CODE)?;

        check_command(Some(dir.path()))
            .arg("--kdl-version")
            .arg("v1")
            .arg(file1.path())
            .arg(file2.path())
            .assert()
            .success();

        Ok(())
    }

    #[test]
    fn it_should_fail_if_kdl_is_invalid() -> std::io::Result<()> {
        let dir = tempfile::tempdir()?;

        let file = setup_test_input(dir.path(), INVALID_V1_CODE)?;

        check_command(Some(dir.path()))
            .arg("--kdl-version")
            .arg("v1")
            .arg(file.path())
            .assert()
            .failure()
            .stderr(predicates::str::contains("Error parsing file "));

        let output = std::fs::read_to_string(file.path())?;

        assert_eq!(output, INVALID_V1_CODE);

        Ok(())
    }

    #[test]
    fn it_should_fail_if_kdl_is_invalid_stdin() {
        check_command(None)
            .arg("--kdl-version")
            .arg("v1")
            .arg("--stdin")
            .write_stdin(INVALID_V1_CODE)
            .assert()
            .failure()
            .stderr(predicates::str::contains("Error parsing content"));
    }
}

#[cfg(test)]
mod v2 {
    use crate::{
        BROKEN_V2_CODE, FORMATTED_V2_CODE, INVALID_V2_CODE, check_command, setup_test_input,
    };

    #[test]
    fn success_with_formatted_input() -> std::io::Result<()> {
        let dir = tempfile::tempdir()?;

        let file = setup_test_input(dir.path(), FORMATTED_V2_CODE)?;

        check_command(Some(dir.path()))
            .arg("--kdl-version")
            .arg("v2")
            .arg(file.path())
            .assert()
            .success();

        Ok(())
    }

    #[test]
    fn success_with_formatted_input_stdin() {
        check_command(None)
            .arg("--kdl-version")
            .arg("v2")
            .arg("--stdin")
            .write_stdin(FORMATTED_V2_CODE)
            .assert()
            .success();
    }

    #[test]
    fn fail_with_unformatted_input() -> std::io::Result<()> {
        let dir = tempfile::tempdir()?;

        let file = setup_test_input(dir.path(), BROKEN_V2_CODE)?;

        check_command(Some(dir.path()))
            .arg("--kdl-version")
            .arg("v2")
            .arg(file.path())
            .assert()
            .failure();

        let content_after = std::fs::read_to_string(file.path())?;
        assert_eq!(content_after, BROKEN_V2_CODE);

        Ok(())
    }

    #[test]
    fn fail_with_unformatted_input_stdin() {
        check_command(None)
            .arg("--kdl-version")
            .arg("v2")
            .arg("--stdin")
            .write_stdin(BROKEN_V2_CODE)
            .assert()
            .failure();
    }

    #[test]
    fn do_nothing_without_input() -> std::io::Result<()> {
        let dir = tempfile::tempdir()?;

        check_command(Some(dir.path()))
            .arg("--kdl-version")
            .arg("v2")
            .assert()
            .success();

        Ok(())
    }

    #[test]
    fn accepts_multiple_paths() -> std::io::Result<()> {
        let dir = tempfile::tempdir()?;

        let file1 = setup_test_input(dir.path(), FORMATTED_V2_CODE)?;
        let file2 = setup_test_input(dir.path(), FORMATTED_V2_CODE)?;

        check_command(Some(dir.path()))
            .arg("--kdl-version")
            .arg("v2")
            .arg(file1.path())
            .arg(file2.path())
            .assert()
            .success();

        Ok(())
    }

    #[test]
    fn it_should_fail_if_kdl_is_invalid() -> std::io::Result<()> {
        let dir = tempfile::tempdir()?;

        let file = setup_test_input(dir.path(), INVALID_V2_CODE)?;

        check_command(Some(dir.path()))
            .arg("--kdl-version")
            .arg("v2")
            .arg(file.path())
            .assert()
            .failure()
            .stderr(predicates::str::contains("Error parsing file "));

        let output = std::fs::read_to_string(file.path())?;

        assert_eq!(output, INVALID_V2_CODE);

        Ok(())
    }

    #[test]
    fn it_should_fail_if_kdl_is_invalid_stdin() {
        check_command(None)
            .arg("--kdl-version")
            .arg("v2")
            .arg("--stdin")
            .write_stdin(INVALID_V2_CODE)
            .assert()
            .failure()
            .stderr(predicates::str::contains("Error parsing content"));
    }
}
