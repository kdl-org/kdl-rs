use crate::cli::KdlVersion;

pub fn parse_kdl(
    input: &str,
    version: Option<KdlVersion>,
) -> miette::Result<(kdl::KdlDocument, KdlVersion)> {
    match version {
        Some(KdlVersion::V1) => Ok((kdl::KdlDocument::parse_v1(input)?, KdlVersion::V1)),
        Some(KdlVersion::V2) => Ok((kdl::KdlDocument::parse_v2(input)?, KdlVersion::V2)),
        None => {
            if let Ok(v2) = kdl::KdlDocument::parse_v2(input) {
                Ok((v2, KdlVersion::V2))
            } else {
                Ok((kdl::KdlDocument::parse_v1(input)?, KdlVersion::V1))
            }
        }
    }
}

pub fn format_kdl(
    mut input: kdl::KdlDocument,
    format_config: &kdl::FormatConfig,
    version: KdlVersion,
) -> String {
    input.autoformat_config(format_config);

    if KdlVersion::V1 == version {
        input.ensure_v1();
    } else {
        input.ensure_v2();
    }

    input.to_string()
}

#[cfg(test)]
mod test {
    use super::parse_kdl;
    use crate::{cli::KdlVersion, kdl::format_kdl};

    #[test]
    fn it_should_be_reversible() {
        let input = r#"world {
    child "1"
    child "2"
}
"#;

        let (doc, version) = parse_kdl(input, Some(KdlVersion::V1)).expect("it to parse valid kdl");

        let formatted = format_kdl(doc, &kdl::FormatConfig::default(), version);

        assert_eq!(input, formatted);
    }
}
