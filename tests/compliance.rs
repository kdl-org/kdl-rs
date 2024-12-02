use std::{
    collections::HashMap,
    fs,
    path::{Path, PathBuf},
};

use kdl::{KdlDocument, KdlIdentifier, KdlParseFailure, KdlValue};
use miette::{Diagnostic, IntoDiagnostic};
use thiserror::Error;

#[derive(Debug, Error, Diagnostic)]
#[error("Compliance test suite failed {} of {total_checks}.", diagnostics.len())]
struct ComplianceSuiteFailure {
    total_checks: usize,
    #[related]
    diagnostics: Vec<ComplianceDiagnostic>,
}

#[derive(Debug, Error, Diagnostic)]
enum ComplianceDiagnostic {
    #[error("{}", PathBuf::from(.0.file_name().unwrap()).display())]
    #[diagnostic(code(kdl::compliance::parse_failure))]
    KdlParseFailure(
        PathBuf,
        #[source]
        #[diagnostic_source]
        KdlParseFailure,
    ),

    #[error("{}:\nExpected:\n{expected}\nActual:\n{actual}", PathBuf::from(file.file_name().unwrap()).display())]
    #[diagnostic(code(kdl::compliance::expectation_mismatch))]
    ExpectationMismatch {
        file: PathBuf,
        original: String,
        expected: String,
        actual: String,
    },

    #[error(transparent)]
    #[diagnostic(code(kdl::compliance::io_error))]
    IoError(#[from] std::io::Error),
}

#[test]
fn spec_compliance() -> miette::Result<()> {
    let input = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("tests")
        .join("test_cases")
        .join("input");
    let mut failures = Vec::new();
    let mut count = 0usize;
    for test_name in fs::read_dir(input).into_diagnostic()? {
        let test_path = test_name.into_diagnostic()?.path();
        let src = normalize_line_endings(fs::read_to_string(&test_path).into_diagnostic()?);
        let res = src.parse();
        if let Err(e) = validate_res(res, &test_path, &src) {
            failures.push(e);
        }
        count += 1;
    }
    if failures.is_empty() {
        Ok(())
    } else {
        let mut output = String::new();
        for failure in &failures {
            output.push_str(format!("\n{failure}").as_str());
        }
        Err(ComplianceSuiteFailure {
            total_checks: count,
            diagnostics: failures,
        }
        .into())
    }
}

fn validate_res(
    res: Result<KdlDocument, KdlParseFailure>,
    path: &Path,
    src: &str,
) -> Result<(), ComplianceDiagnostic> {
    let file_name = path.file_name().unwrap();
    let expected_dir = path
        .parent()
        .unwrap()
        .parent()
        .unwrap()
        .join("expected_kdl");
    let expected_path = expected_dir.join(file_name);
    let underscored = expected_dir.join(format!("_{}", PathBuf::from(file_name).display()));
    if expected_path.exists() {
        let doc = res.map_err(|e| ComplianceDiagnostic::KdlParseFailure(path.into(), e))?;
        let expected = normalize_line_endings(fs::read_to_string(&expected_path)?);
        let actual = stringify_to_expected(doc);
        if actual != expected {
            return Err(ComplianceDiagnostic::ExpectationMismatch {
                file: path.into(),
                original: src.into(),
                expected: expected.replace('\n', "\\n").replace(" ", "."),
                actual: actual.replace('\n', "\\n").replace(" ", "."),
            });
        }
    } else if underscored.exists() {
        eprintln!(
            "skipped reserialization for {}",
            PathBuf::from(file_name).display()
        );
        // } else {
        //     res.map_err(|e| ComplianceDiagnostic::KdlParseFailure(path.into(), e))?;
    }
    Ok(())
}

fn normalize_line_endings(src: String) -> String {
    src.replace("\r\n", "\n")
}

fn stringify_to_expected(mut doc: KdlDocument) -> String {
    doc.autoformat_no_comments();
    normalize_strings(&mut doc);
    normalize_identifiers(&mut doc);
    dedupe_props(&mut doc);
    remove_empty_children(&mut doc);
    doc.to_string()
}

fn normalize_strings(doc: &mut KdlDocument) {
    for node in doc.nodes_mut() {
        for entry in node.entries_mut() {
            if let Some(value) = entry.value().as_string() {
                *entry.value_mut() = KdlValue::String(value.to_string());
            }
        }
        if let Some(children) = node.children_mut() {
            normalize_strings(children);
        }
    }
}

fn normalize_identifiers(doc: &mut KdlDocument) {
    for node in doc.nodes_mut() {
        node.name_mut().clear_format();
        for entry in node.entries_mut() {
            if entry.name().is_some() {
                if let Some(x) = entry.name_mut() {
                    x.clear_format()
                }
            }
        }
        if let Some(children) = node.children_mut() {
            normalize_identifiers(children);
        }
    }
}

fn dedupe_props(doc: &mut KdlDocument) {
    for node in doc.nodes_mut() {
        let mut props = HashMap::<KdlIdentifier, Vec<usize>>::new();
        for (idx, entry) in node.entries_mut().iter_mut().enumerate() {
            if let Some(name) = entry.name() {
                if !props.contains_key(name) {
                    props.insert(name.clone(), Vec::new());
                }
                if let Some(indices) = props.get_mut(name) {
                    indices.push(idx);
                }
            }
        }
        let new_entries = node
            .entries()
            .iter()
            .enumerate()
            .filter_map(|(idx, entry)| {
                if let Some(name) = entry.name() {
                    if let Some(indices) = props.get(name) {
                        if &idx == indices.last().unwrap() {
                            return Some(entry.clone());
                        } else {
                            return None;
                        }
                    }
                }
                Some(entry.clone())
            });
        *node.entries_mut() = new_entries.collect();
        if let Some(children) = node.children_mut() {
            dedupe_props(children);
        }
    }
}

fn remove_empty_children(doc: &mut KdlDocument) {
    for node in doc.nodes_mut() {
        let maybe_children = node.children_mut();
        if maybe_children.is_some() && maybe_children.as_ref().unwrap().nodes().is_empty() {
            *maybe_children = None;
        }
        if let Some(children) = maybe_children {
            remove_empty_children(children);
        }
    }
}
