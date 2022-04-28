pub(crate) fn fmt_leading(leading: &mut String, indent: usize, no_comments: bool) {
    if leading.is_empty() {
        return;
    }
    let mut result = String::new();
    if !no_comments {
        let comments = crate::parser::parse(leading.trim(), crate::parser::leading_comments)
            .expect("invalid leading text");
        for line in comments {
            let trimmed = line.trim();
            if !trimmed.is_empty() {
                result.push_str(&format!("{:indent$}{}\n", "", trimmed, indent = indent));
            }
        }
    }
    result.push_str(&format!("{:indent$}", "", indent = indent));
    *leading = result;
}

pub(crate) fn fmt_trailing(decor: &mut String, no_comments: bool) {
    if decor.is_empty() {
        return;
    }
    *decor = decor.trim().to_string();
    let mut result = String::new();
    if !no_comments {
        let comments = crate::parser::parse(decor, crate::parser::trailing_comments)
            .expect("invalid trailing text");
        for comment in comments {
            result.push_str(comment);
        }
    }
    *decor = result;
}
