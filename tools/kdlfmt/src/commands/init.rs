use crate::{cli::InitCommandArguments, config::KdlFmtConfig, error::KdlFmtError, kdl::format_kdl};

pub fn run(args: &InitCommandArguments) -> Result<(), KdlFmtError> {
    let config_path = std::path::Path::new(KdlFmtConfig::filename());

    if !args.force && config_path.try_exists().map_err(KdlFmtError::Io)? {
        return Err(KdlFmtError::ConfigAlreadyExist);
    }

    let config = KdlFmtConfig::default();

    let mut doc = kdl::KdlDocument::new();

    let mut indent_size_node = kdl::KdlNode::new(KdlFmtConfig::indent_size_key());
    indent_size_node.push(kdl::KdlEntry::from(kdl::KdlValue::Integer(
        config.indent.len() as i128,
    )));
    doc.nodes_mut().push(indent_size_node);

    let mut use_tab_node = kdl::KdlNode::new(KdlFmtConfig::use_tabs_key());
    use_tab_node.push(kdl::KdlEntry::from(kdl::KdlValue::Bool(config.use_tabs)));
    doc.nodes_mut().push(use_tab_node);

    let format_config = kdl::FormatConfig::builder().indent(&config.indent).build();

    let doc = format_kdl(doc, &format_config, args.kdl_version.unwrap_or_default());

    std::fs::write(config_path, &doc).map_err(KdlFmtError::Io)
}
