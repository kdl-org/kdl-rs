use std::fmt::Write as _;

pub(crate) fn autoformat_leading(leading: &mut String, indent: usize, no_comments: bool) {
    let mut result = String::new();
    if !no_comments {
        let input = leading.trim();
        if !input.is_empty() {
            for line in input.lines() {
                let trimmed = line.trim();
                if !trimmed.is_empty() {
                    writeln!(result, "{:indent$}{}", "", trimmed, indent = indent).unwrap();
                }
            }
        }
    }
    write!(result, "{:indent$}", "", indent = indent).unwrap();
    *leading = result;
}

pub(crate) fn autoformat_trailing(decor: &mut String, no_comments: bool) {
    if decor.is_empty() {
        return;
    }
    *decor = decor.trim().to_string();
    let mut result = String::new();
    if !no_comments {
        for comment in decor.lines() {
            writeln!(result, "{comment}").unwrap();
        }
    }
    *decor = result;
}
