use std::{collections::HashMap, sync::LazyLock};

use miette::SourceSpan;

use crate::KdlDocument;

// Someday, this will be replaced with a proper serde-style implementation that
// we can have nicer code around. But for now, this is how we live.
static KDL_SCHEMA_SCHEMA: LazyLock<KdlSchema> = LazyLock::new(|| {
  KdlSchema::new_(include_str!("./kdl-schema.kdl").parse().expect("Failed to parse KDL Schema Schema?"))  
});
    
/// Represents a KDL Schema.
#[derive(Debug, Default, Clone, Eq, PartialEq)]
pub struct KdlSchema {
    schema_doc: KdlDocument,
    id: String,
    title: String,
    description: String,
    nodes: HashMap<String, KdlNodeSpec>,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct KdlNodeSpec {
    /// KPath to location of node definition in schema
    pub schema_path: String,
    pub id: String,
    pub name: String,
    pub about: String,
    pub required: bool,
    pub min: usize,
    pub max: usize,
    pub references: Vec<KdlNodeRef>,
    pub deprecated: Option<KdlNodeDeprecationInfo>,
    pub annotations: Vec<KdlNodeAnnotationInfo>,
    pub props: HashMap<String, KdlNodePropValidation>,
    pub other_props: KdlNodeOtherPropValidations,
    pub args: Vec<KdlNodeArgValidation>,
    pub other_args: KdlNodeOtherArgValidations,
    pub children: Vec<
}

// Public API
impl KdlSchema {
    /// Creates a new KdlSchema.
    /// 
    /// Returns a [`KdlSchemaError`] if the input is not a valid KDL Schema
    /// itself.
    pub fn new(doc: KdlDocument) -> Result<Self, KdlSchemaError> {
        KDL_SCHEMA_SCHEMA.validate(&doc)?;
        Ok(Self::new_(doc))
    }
    
    fn new_(doc: KdlDocument) -> Self {
        Self {
            schema_doc: doc,
        }
    }

    /// Gets the schema ID.
    pub fn id(&self) -> &str {
        &self.id
    }

    /// Gets the schema title.
    pub fn title(&self) -> &str {
        &self.title
    }

    /// Gets the schema description.
    pub fn description(&self) -> &str {
        &self.description
    }
    
    /// Validates a document against this schema.
    pub fn validate(&self, doc: &KdlDocument) -> Result<(), KdlSchemaError> {
        let mut errs = Vec::new();
        self.validate_metadata(doc).map_err(|e| errs.extend(e.validations.into_iter()));
        self.validate_definitions(doc).map_err(|e| errs.extend(e.validations.into_iter()));
        self.validate_document(doc).map_err(|e| errs.extend(e.validations.into_iter()));
        self.validate_examples(doc).map_err(|e| errs.extend(e.validations.into_iter()));
        if errs.is_empty() {
            Ok(())
        } else {
            errs.sort_by(|a, b| a.span.offset.cmp(b.span.offset));
            Err(KdlSchemaError {
                validations: errs,
            })
        }
    }
}

impl TryFrom<KdlDocument> for KdlSchema {
    type Error = KdlSchemaError;
    
    fn try_from(value: KdlDocument) -> Result<Self, Self::Error> {
        Self::new(value)
    }
}

impl From<KdlSchema> for KdlDocument {
    fn from(value: KdlSchema) -> Self {
        value.0
    }
}

// Private stuff
impl KdlSchema {
    // Panics if key is not in the metadata, or if metadata is missing
    fn get_meta_str(&self, key: &str) -> &str {
        self.0
            .get("metadata")
            .expect("we should have validated that doc has metadata.")
            .get(key)
            .expect("we should have validated that metadata has this field.")
            .as_string()
            .expect("we should have already validated that id is a string.")
    }
}

/// Groups all related schema validation failures for a document together.
#[derive(Debug, thiserror::Error, miette::Diagnostic)]
#[error("Failed to validate the document against the given schema.")]
pub struct KdlSchemaError {
    /// Validation failures for the document this error is associated with.
    #[related]
    pub validations: Vec<KdlSchemaValidation>,
}

/// Individual validation failure. Has some utility [`miette::Diagnostic`]
/// fields for easy integration with `miette` error reporting, as well as a
/// `path` that may be used for navigating the document tree to the failure
/// location.
#[derive(Debug, thiserror::Error, miette::Diagnostic)]
#[error("{}", message.clone().unwrap_or_else(|| "Failed validation".into()))]
pub struct KdlSchemaValidation {
    
    /// Message for the error itself.
    pub message: Option<String>,
    
    /// Path to bad component.
    pub path: Vec<String>,
    
    /// Offset in chars of the error.
    #[label("{}", label.clone().unwrap_or_else(|| "here".into()))]
    pub span: SourceSpan,

    /// Label text for this span. Defaults to `"here"`.
    pub label: Option<String>,
    
    /// Suggestion for fixing the validation error.
    #[help]
    pub help: Option<String>,
    
    /// Severity level for the Diagnostic.
    #[diagnostic(severity)]
    pub severity: miette::Severity,
}
