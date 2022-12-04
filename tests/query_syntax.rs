use kdl::KdlDocument;
use miette::Result;

#[test]
fn syntax_errors() -> Result<()> {
    macro_rules! assert_syntax_errors {
            ($($input:expr),*) => {
                $(
                    assert!("node".parse::<KdlDocument>().unwrap().query_all($input).is_err(), $input);
                )*
            }
        }

    assert_syntax_errors! {
        "",
        "scope(",
        "(",
        ")",
        "[",
        "]",
        "()(type)",
        "(type)()",
        "name(type)",
        "[]name",
        "[]()",
        "[scope()]",
        "[other()]",
        "[val()1]",
        "[val() 1]",
        "[val()=identifier]",
        // Only string values are allowed here.
        "[val()*=1]",
        "[val()^=1]",
        "[val()$=1]",
        "[val()*=null]",
        "[val()^=null]",
        "[val()$=null]",
        "[val()*=true]",
        "[val()^=true]",
        "[val()$=true]",
        "a b",
        "a\nb",
        ","
    }

    Ok(())
}
