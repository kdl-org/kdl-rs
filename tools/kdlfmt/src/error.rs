#[derive(Debug)]
pub enum KdlFmtError {
    Io(std::io::Error),
    ParseKdl(Option<std::path::PathBuf>, miette::Error),
    ReadStdin(std::io::Error),
    CheckModeChanges,
    ConfigAlreadyExist,
}

impl std::fmt::Display for KdlFmtError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Io(error) => error.fmt(f),
            Self::ReadStdin(error) => write!(f, "Error reading input from stdin - {error}"),
            Self::ParseKdl(maybe_path, error) => {
                if let Some(path) = maybe_path {
                    write!(f, "Error parsing file '{}' - {error:?}", path.display())
                } else {
                    write!(f, "Error parsing content - {error:?}")
                }
            }
            Self::CheckModeChanges => write!(f, "Found changes while running in check mode"),
            Self::ConfigAlreadyExist => write!(f, "A config already exists in this directory"),
        }
    }
}

impl std::error::Error for KdlFmtError {}
