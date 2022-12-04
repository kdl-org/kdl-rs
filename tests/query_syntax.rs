use kdl::KdlDocument;
use miette::Result;

#[test]
fn syntax_errors() -> Result<()> {
    macro_rules! assert_syntax_errors {
            ($(($input:expr, $msg:expr, ($offset:expr, $len:expr))),*) => {
                $(
                    let err = "node".parse::<KdlDocument>()
                        .unwrap()
                        .query_all($input)
                        .expect_err("query parse should've failed.");
                    assert_eq!(err.to_string(), $msg, "unexpected error message");
                    assert_eq!(err.span.offset(), $offset, "unexpected span offset");
                    assert_eq!(err.span.len(), $len, "unexpected span length");
                )*
            }
        }

    assert_syntax_errors! {
        ("", "Expected a valid node matcher.", (0, 0)),
        (" scope(", "Expected a valid scope accessor.", (1, 6)),
        ("(", "Expected closing ')' for type annotation.", (0, 1)),
        (")", "Expected a valid node matcher.", (0, 0)),
        ("[", "Expected a closing ']' for this attribute matcher.", (0, 1)),
        ("]", "Expected a valid node matcher.", (0, 0)),
        ("a b", "Expected a valid KQL query.", (2, 0)),
        ("a\nb", "Expected a valid KQL query.", (2, 0)),
        (",", "Expected a valid node matcher.", (0, 0)),
        ("[] > scope( )", "Expected scope() to be the first item in this selector.", (5, 8)),
        ("()(type)", "Expected only one type annotation per selector.", (2, 6)),
        ("(type)()", "Expected only one type annotation per selector.", (6, 2)),
        ("name(type)", "Expected type annotation to not be used after a node name.", (4, 6)),
        ("[]name", "Expected node name to come before attribute matcher(s).", (2, 4)),
        ("[]()", "Expected type annotation to come before attribute matcher(s).", (2, 2)),
        ("[type(blah)]", "Expected a closing ')' for this 'type()' accessor.", (1, 5)),
        ("[scope()]", "Expected 'scope()' to be the first item only at the top level of the query selector.", (1, 7)),
        ("[scope ( )]", "Expected 'scope()' to be the first item only at the top level of the query selector.", (1, 9)),
        ("[other()]", "Expected a valid attribute accessor.", (1, 7)),
        ("[arg()1]", "Expected a closing ']' for this attribute matcher.", (0, 6)),
        ("[arg() 1]", "Expected a closing ']' for this attribute matcher.", (0, 7)),
        ("[arg()=identifier]", "Expected a valid operator argument.", (7, 0)),
        // // Only string values are allowed here.
        ("[arg()*=1]", "Expected a string as an operator value.", (8, 1)),
        ("[arg()^=1]", "Expected a string as an operator value.", (8, 1)),
        ("[arg()$=1]", "Expected a string as an operator value.", (8, 1)),
        ("[arg()*=null]", "Expected a string as an operator value.", (8, 4)),
        ("[arg()^=null]", "Expected a string as an operator value.", (8, 4)),
        ("[arg()$=null]", "Expected a string as an operator value.", (8, 4)),
        ("[arg()*=true]", "Expected a string as an operator value.", (8, 4)),
        ("[arg()^=true]", "Expected a string as an operator value.", (8, 4)),
        ("[arg()$=true]", "Expected a string as an operator value.", (8, 4))
    }

    Ok(())
}
