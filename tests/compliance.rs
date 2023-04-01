use std::{
    collections::HashMap,
    fs,
    path::{Path, PathBuf},
};

use kdl::{KdlDocument, KdlError, KdlIdentifier, KdlValue};
use miette::IntoDiagnostic;

#[test]
fn spec_compliance() -> miette::Result<()> {
    let input = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("tests")
        .join("test_cases")
        .join("input");
    for test_name in fs::read_dir(&input).into_diagnostic()? {
        let test_path = test_name.into_diagnostic()?.path();
        println!(
            "parsing {}:",
            PathBuf::from(test_path.file_name().unwrap()).display()
        );
        let src = normalize_line_endings(fs::read_to_string(&test_path).into_diagnostic()?);
        println!("src: {}", src);
        let res: Result<KdlDocument, KdlError> = src.parse();
        validate_res(res, &test_path)?;
    }
    Ok(())
}

fn validate_res(res: Result<KdlDocument, KdlError>, path: &Path) -> miette::Result<()> {
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
        let doc = res?;
        let expected =
            normalize_line_endings(fs::read_to_string(&expected_path).into_diagnostic()?);
        println!("expected: {}", expected);
        let stringified = stringify_to_expected(doc);
        println!("stringified: {}", stringified);
        assert_eq!(stringified, expected);
    } else if underscored.exists() {
        println!(
            "skipped reserialization for {}",
            PathBuf::from(file_name).display()
        );
    } else {
        assert!(res.is_err(), "parse should not have succeeded");
    }
    Ok(())
}

fn normalize_line_endings(src: String) -> String {
    src.replace("\r\n", "\n")
}

fn stringify_to_expected(mut doc: KdlDocument) -> String {
    doc.fmt_no_comments();
    normalize_numbers(&mut doc);
    normalize_strings(&mut doc);
    dedupe_props(&mut doc);
    remove_empty_children(&mut doc);
    doc.to_string()
}

fn normalize_numbers(doc: &mut KdlDocument) {
    for node in doc.nodes_mut() {
        for entry in node.entries_mut() {
            if let Some(value) = entry.value().as_i64() {
                *entry.value_mut() = KdlValue::Base10(value);
            }
        }
        if let Some(children) = node.children_mut() {
            normalize_numbers(children);
        }
    }
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
