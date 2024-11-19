use crate::parser_combinator::Parser;

pub fn all_character(s: &str) -> Option<(char, &str)> {
    crate::regex!(r#"^[^"\\[:cntrl:]]"#, |s| s.chars().next())
        .or(crate::regex!(r#"^\\u[0-9a-fA-F]{4}"#, hex_code))
        .or(crate::regex!(r#"^\\."#, escape))(s)
}

pub fn markdown_character(s: &str) -> Option<(char, &str)> {
    crate::regex!(r#"^[^"\\#\\*\\_\\[:cntrl:]]"#, |s| s.chars().next())
        .or(crate::regex!(r#"^\\u[0-9a-fA-F]{4}"#, hex_code))
        .or(crate::regex!(r#"^\\."#, escape))(s)
}

pub fn hex_code(code: &str) -> Option<char> {
    code.strip_prefix("\\u")
        .and_then(|code| u32::from_str_radix(code, 16).ok())
        .and_then(std::char::from_u32)
}

pub fn escape(s: &str) -> Option<char> {
    match s {
        "\\\"" => Some('"'),
        "\\\\" => Some('\\'),
        "\\#" => Some('#'),
        "\\*" => Some('*'),
        "\\_" => Some('_'),
        "\\/" => Some('/'),
        "\\b" => Some('\x08'),
        "\\f" => Some('\x0C'),
        "\\n" => Some('\n'),
        "\\r" => Some('\r'),
        "\\t" => Some('\t'),
        _ => None,
    }
}

#[cfg(test)]
mod test {
    use crate::markdown::characters::markdown_character;

    #[test]
    fn test_markdown_character() {
        assert_eq!(markdown_character("a"), Some(('a', "")));
        assert_eq!(markdown_character("あ"), Some(('あ', "")));
        assert_eq!(markdown_character("\n"), None);
        assert_eq!(markdown_character("\\n"), Some(('\n', "")));
        assert_eq!(markdown_character("\\u3042"), Some(('あ', "")));
        assert_eq!(markdown_character("\\u304"), None);
        assert_eq!(markdown_character("\\u"), None);
    }
}
