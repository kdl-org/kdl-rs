use dashmap::DashMap;
use kdl::{KdlDocument, KdlError};
use miette::Diagnostic as _;
use ropey::Rope;
use tower_lsp::jsonrpc::Result;
use tower_lsp::lsp_types::*;
use tower_lsp::{Client, LanguageServer, LspService, Server};
use tracing_subscriber::prelude::*;
use tracing_subscriber::EnvFilter;

#[derive(Debug)]
struct Backend {
    client: Client,
    document_map: DashMap<String, Rope>,
}

impl Backend {
    async fn on_change(&self, uri: Url, text: &str) {
        let rope = ropey::Rope::from_str(text);
        self.document_map.insert(uri.to_string(), rope.clone());
    }
}

#[tower_lsp::async_trait]
impl LanguageServer for Backend {
    async fn initialize(&self, _: InitializeParams) -> Result<InitializeResult> {
        Ok(InitializeResult {
            capabilities: ServerCapabilities {
                text_document_sync: Some(TextDocumentSyncCapability::Options(
                    TextDocumentSyncOptions {
                        open_close: Some(true),
                        change: Some(TextDocumentSyncKind::FULL),
                        save: Some(TextDocumentSyncSaveOptions::SaveOptions(SaveOptions {
                            include_text: Some(true),
                        })),
                        ..Default::default()
                    },
                )),
                workspace: Some(WorkspaceServerCapabilities {
                    workspace_folders: Some(WorkspaceFoldersServerCapabilities {
                        supported: Some(true),
                        change_notifications: Some(OneOf::Left(true)),
                    }),
                    file_operations: None,
                }),
                diagnostic_provider: Some(DiagnosticServerCapabilities::RegistrationOptions(
                    DiagnosticRegistrationOptions {
                        text_document_registration_options: TextDocumentRegistrationOptions {
                            document_selector: Some(vec![DocumentFilter {
                                language: Some("kdl".into()),
                                scheme: Some("file".into()),
                                pattern: None,
                            }]),
                        },
                        ..Default::default()
                    },
                )),
                // hover_provider: Some(HoverProviderCapability::Simple(true)),
                // completion_provider: Some(Default::default()),
                ..Default::default()
            },
            ..Default::default()
        })
    }

    async fn initialized(&self, _: InitializedParams) {
        self.client
            .log_message(MessageType::INFO, "server initialized!")
            .await;
    }

    async fn shutdown(&self) -> Result<()> {
        self.client
            .log_message(MessageType::INFO, "server shutting down")
            .await;
        Ok(())
    }

    async fn did_open(&self, params: DidOpenTextDocumentParams) {
        self.on_change(params.text_document.uri, &params.text_document.text)
            .await;
    }

    async fn did_change(&self, params: DidChangeTextDocumentParams) {
        self.on_change(params.text_document.uri, &params.content_changes[0].text)
            .await;
    }

    async fn did_save(&self, params: DidSaveTextDocumentParams) {
        if let Some(text) = params.text.as_ref() {
            self.on_change(params.text_document.uri, text).await;
        }
    }

    async fn did_close(&self, params: DidCloseTextDocumentParams) {
        self.document_map
            .remove(&params.text_document.uri.to_string());
    }

    async fn diagnostic(
        &self,
        params: DocumentDiagnosticParams,
    ) -> Result<DocumentDiagnosticReportResult> {
        tracing::debug!("diagnostic req");
        if let Some(doc) = self.document_map.get(&params.text_document.uri.to_string()) {
            let res: std::result::Result<KdlDocument, KdlError> = doc.to_string().parse();
            if let Err(kdl_err) = res {
                let diags = kdl_err
                    .diagnostics
                    .into_iter()
                    .map(|diag| {
                        Diagnostic::new(
                            Range::new(
                                char_to_position(diag.span.offset(), &doc),
                                char_to_position(diag.span.offset() + diag.span.len(), &doc),
                            ),
                            diag.severity().map(to_lsp_sev),
                            diag.code().map(|c| NumberOrString::String(c.to_string())),
                            None,
                            diag.to_string(),
                            None,
                            None,
                        )
                    })
                    .collect();
                return Ok(DocumentDiagnosticReportResult::Report(
                    DocumentDiagnosticReport::Full(RelatedFullDocumentDiagnosticReport {
                        related_documents: None,
                        full_document_diagnostic_report: FullDocumentDiagnosticReport {
                            result_id: None,
                            items: diags,
                        },
                    }),
                ));
            }
        }
        Ok(DocumentDiagnosticReportResult::Report(
            DocumentDiagnosticReport::Full(RelatedFullDocumentDiagnosticReport::default()),
        ))
    }

    // TODO(@zkat): autocomplete #-keywords
    // TODO(@zkat): autocomplete schema stuff
    // async fn completion(&self, _: CompletionParams) -> Result<Option<CompletionResponse>> {
    //     tracing::debug!("Completion request");
    //     Ok(Some(CompletionResponse::Array(vec![
    //         CompletionItem::new_simple("Hello".to_string(), "Some detail".to_string()),
    //         CompletionItem::new_simple("Bye".to_string(), "More detail".to_string()),
    //     ])))
    // }

    // TODO(@zkat): We'll use this when we actually do schema stuff.
    // async fn hover(&self, _: HoverParams) -> Result<Option<Hover>> {
    //     tracing::debug!("Hover request");
    //     Ok(Some(Hover {
    //         contents: HoverContents::Scalar(MarkedString::String("You're hovering!".to_string())),
    //         range: None,
    //     }))
    // }
}

fn char_to_position(char_idx: usize, rope: &Rope) -> Position {
    let line_idx = rope.char_to_line(char_idx);
    let line_char_idx = rope.line_to_char(line_idx);
    let column_idx = char_idx - line_char_idx;
    Position::new(line_idx as u32, column_idx as u32)
}

fn to_lsp_sev(sev: miette::Severity) -> DiagnosticSeverity {
    match sev {
        miette::Severity::Advice => DiagnosticSeverity::HINT,
        miette::Severity::Warning => DiagnosticSeverity::WARNING,
        miette::Severity::Error => DiagnosticSeverity::ERROR,
    }
}

#[tokio::main]
async fn main() {
    tracing_subscriber::registry()
        .with(
            tracing_subscriber::fmt::layer()
                .map_writer(move |_| std::io::stderr)
                .with_ansi(false),
        )
        .with(EnvFilter::from_default_env())
        .init();

    let stdin = tokio::io::stdin();
    let stdout = tokio::io::stdout();

    let (service, socket) = LspService::new(|client| Backend {
        client,
        document_map: DashMap::new(),
    });
    Server::new(stdin, stdout, socket).serve(service).await;
}
