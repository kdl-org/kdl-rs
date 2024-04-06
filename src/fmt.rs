use std::fmt::Write as _;

pub(crate) fn autoformat_leading(leading: &mut String, indent: usize, _no_comments: bool) {
    if leading.is_empty() {
        return;
    }
    // TODO
    let mut result = String::new();
    // if !no_comments {
    //     let input = leading.trim();
    //     let kdl_parser = crate::v1_parser::KdlParser { full_input: input };
    //     let comments = kdl_parser
    //         .parse(crate::v1_parser::leading_comments(&kdl_parser))
    //         .expect("invalid leading text");
    //     for line in comments {
    //         let trimmed = line.trim();
    //         if !trimmed.is_empty() {
    //             writeln!(result, "{:indent$}{}", "", trimmed, indent = indent).unwrap();
    //         }
    //     }
    // }
    write!(result, "{:indent$}", "", indent = indent).unwrap();
    *leading = result;
}

pub(crate) fn autoformat_trailing(decor: &mut String, _no_comments: bool) {
    if decor.is_empty() {
        return;
    }
    *decor = decor.trim().to_string();
    let result = String::new();
    // TODO
    // if !no_comments {
    //     let input = &*decor;
    //     let kdl_parser = crate::v1_parser::KdlParser { full_input: input };
    //     let comments = kdl_parser
    //         .parse(crate::v1_parser::trailing_comments(&kdl_parser))
    //         .expect("invalid trailing text");
    //     for comment in comments {
    //         result.push_str(comment);
    //     }
    // }
    *decor = result;
}
