/// Show how to build your own diagnostics, without having to use the
/// `fancy` feature or having `main()` return `miette::Result`
use kdl::KdlDocument;
use miette::Diagnostic;
use miette::SourceSpan;

#[derive(Debug)]
pub struct MyError {
    pub message: String,
}

fn parse(input: &str) -> Result<KdlDocument, MyError> {
    let doc = input.parse::<KdlDocument>();
    doc.map_err(|error| {
        let source = error
            .source_code()
            .expect("parse errors should have source code");
        let help = error.help.unwrap_or_default();
        let span: SourceSpan = error.span;
        let contents = source
            .read_span(&span, 0, 0)
            .expect("source should have span contents");
        // miette uses 0 based indexes, but humans prefer 1-based
        let line = contents.line() + 1;
        let column = contents.column() + 1;
        let message = format!(
            "line {}, column {}: {}\n  help: {}",
            line, column, error, help
        );
        MyError { message }
    })
}

fn main() {
    let input = r#"
    foo {
      bar {
         baz 1.
       }
    }
    "#;
    let err = parse(input).unwrap_err();
    eprintln!("{}", err.message);
    // Output:
    //  line 4, column 14: Expected valid value.
    //    help: Floating point numbers must be base 10, and have numbers after the decimal point.
}
