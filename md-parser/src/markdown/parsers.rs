use crate::markdown::characters::*;
use crate::markdown::types::*;
use crate::parser_combinator::*;

pub fn parse(s: &str) -> Token {
    fn tokenize(mut s: &str) -> Vec<Token> {
        let parser = header
            .or(blockquote)
            .or(emphasis)
            .or(strong)
            .or(paragraph)
            .or(line_break);
        let mut tokens = vec![];

        while let Some((md_type, rest)) = parser(s) {
            if md_type == MDContent::LineBreak {
                s = rest;
                continue;
            }

            let inner_text = get_inner_text(&md_type);
            if inner_text.map(|t| t == s).unwrap_or(false) {
                break;
            }

            let token = Token {
                children: inner_text.map(tokenize).unwrap_or(vec![]),
                md_type,
            };
            tokens.push(token);

            s = rest;
        }

        tokens
    }

    Token {
        children: tokenize(s),
        md_type: MDContent::Root,
    }
}

pub fn get_inner_text(md_type: &MDContent) -> Option<&str> {
    match md_type {
        MDContent::Header1(text) => Some(text.as_str()),
        MDContent::Header2(text) => Some(text.as_str()),
        MDContent::Header3(text) => Some(text.as_str()),
        MDContent::Header4(text) => Some(text.as_str()),
        MDContent::Header5(text) => Some(text.as_str()),
        MDContent::Header6(text) => Some(text.as_str()),
        MDContent::Paragraph(text) => Some(text.as_str()),
        MDContent::Blockquote(text) => Some(text.as_str()),
        MDContent::CodeBlock(text) => Some(text.as_str()),
        MDContent::Emphasis(text) => Some(text.as_str()),
        MDContent::Strong(text) => Some(text.as_str()),
        MDContent::Strikethrough(text) => Some(text.as_str()),
        _ => None,
    }
}

pub fn get_inner_list(token: &Token) -> Option<Vec<(u8, &str)>> {
    match &token.md_type {
        MDContent::UnorderedList(items) => {
            Some(items.iter().map(|item| (item.0, item.1.as_str())).collect())
        }
        MDContent::OrderedList(items) => {
            Some(items.iter().map(|item| (item.0, item.1.as_str())).collect())
        }
        _ => None,
    }
}

fn line_break(s: &str) -> Option<(MDContent, &str)> {
    string("\n")(s).map(|(_, rest)| (MDContent::LineBreak, rest))
}

fn header(s: &str) -> Option<(MDContent, &str)> {
    let header1 = string("# ")
        .and(lexeme(many1(all_character)))
        .map(|(_, text)| MDContent::Header1(text.into_iter().collect()));
    let header2 = string("## ")
        .and(lexeme(many1(all_character)))
        .map(|(_, text)| MDContent::Header2(text.into_iter().collect()));
    let header3 = string("### ")
        .and(lexeme(many1(all_character)))
        .map(|(_, text)| MDContent::Header3(text.into_iter().collect()));
    let header4 = string("#### ")
        .and(lexeme(many1(all_character)))
        .map(|(_, text)| MDContent::Header4(text.into_iter().collect()));
    let header5 = string("##### ")
        .and(lexeme(many1(all_character)))
        .map(|(_, text)| MDContent::Header5(text.into_iter().collect()));
    let header6 = string("###### ")
        .and(lexeme(many1(all_character)))
        .map(|(_, text)| MDContent::Header6(text.into_iter().collect()));

    header1
        .or(header2)
        .or(header3)
        .or(header4)
        .or(header5)
        .or(header6)(s)
}

fn paragraph(s: &str) -> Option<(MDContent, &str)> {
    many1(markdown_character)(s)
        .map(|(text, rest)| (MDContent::Paragraph(text.into_iter().collect()), rest))
}

fn blockquote(s: &str) -> Option<(MDContent, &str)> {
    string("> ").and(many1(markdown_character))(s)
        .map(|((_, text), rest)| (MDContent::Blockquote(text.into_iter().collect()), rest))
}

fn emphasis(s: &str) -> Option<(MDContent, &str)> {
    let asterisk = character('*')
        .and(many(markdown_character))
        .and(character('*'));
    let underscore = character('_')
        .and(many(markdown_character))
        .and(character('_'));

    asterisk.or(underscore)(s)
        .map(|(((_, text), _), rest)| (MDContent::Emphasis(text.into_iter().collect()), rest))
}

fn strong(s: &str) -> Option<(MDContent, &str)> {
    let asterisk = string("**").and(many(markdown_character)).and(string("**"));
    let underscore = string("__").and(many(markdown_character)).and(string("__"));

    asterisk.or(underscore)(s)
        .map(|(((_, text), _), rest)| (MDContent::Strong(text.into_iter().collect()), rest))
}

#[cfg(test)]
mod test {
    use crate::markdown::{
        parsers::{blockquote, emphasis, header, paragraph, parse, strong},
        types::{MDContent, Token},
    };

    #[test]
    fn test_header() {
        assert_eq!(
            header("# hello, world!"),
            Some((MDContent::Header1("hello, world!".to_string()), ""))
        );
        assert_eq!(
            header("## hello, world!"),
            Some((MDContent::Header2("hello, world!".to_string()), ""))
        );
        assert_eq!(
            header("### hello, world!"),
            Some((MDContent::Header3("hello, world!".to_string()), ""))
        );
        assert_eq!(
            header("#### hello, world!"),
            Some((MDContent::Header4("hello, world!".to_string()), ""))
        );
        assert_eq!(
            header("##### hello, world!"),
            Some((MDContent::Header5("hello, world!".to_string()), ""))
        );
        assert_eq!(
            header("###### hello, world!"),
            Some((MDContent::Header6("hello, world!".to_string()), ""))
        );
    }

    #[test]
    fn test_paragraph() {
        assert_eq!(
            paragraph("hello, world!"),
            Some((MDContent::Paragraph("hello, world!".to_string()), ""))
        );
    }

    #[test]
    fn test_blockquote() {
        assert_eq!(
            blockquote("> hello, world!"),
            Some((MDContent::Blockquote("hello, world!".to_string()), ""))
        );
    }

    #[test]
    fn test_emphasis() {
        assert_eq!(
            emphasis("*hello, world!*"),
            Some((MDContent::Emphasis("hello, world!".to_string()), ""))
        );
        assert_eq!(
            emphasis("_hello, world_"),
            Some((MDContent::Emphasis("hello, world".to_string()), ""))
        );
    }

    #[test]
    fn test_strong() {
        assert_eq!(
            strong("**hello, world!**"),
            Some((MDContent::Strong("hello, world!".to_string()), ""))
        );
        assert_eq!(
            strong("__hello, world__"),
            Some((MDContent::Strong("hello, world".to_string()), ""))
        );
    }

    #[test]
    fn test_line_break() {
        assert_eq!(
            header("# hello, world!\n## hello, world!"),
            Some((
                MDContent::Header1("hello, world!".to_string()),
                "\n## hello, world!"
            ))
        );
    }

    #[test]
    fn test_tokenize() {
        let expected = Token {
            children: vec![Token {
                children: vec![],
                md_type: MDContent::Header1("hello, world!".to_string()),
            }],
            md_type: MDContent::Root,
        };

        assert_eq!(parse("# hello, world!"), expected);
    }

    #[test]
    fn test_tokenize_multiple() {
        let expected = Token {
            children: vec![
                Token {
                    children: vec![],
                    md_type: MDContent::Header1("hello, world!".to_string()),
                },
                Token {
                    children: vec![],
                    md_type: MDContent::Header2("hello, world!".to_string()),
                },
            ],
            md_type: MDContent::Root,
        };

        assert_eq!(parse("# hello, world!\n## hello, world!"), expected);
    }

    #[test]
    fn test_tokenize_nest() {
        let expected = Token {
            children: vec![Token {
                children: vec![Token {
                    children: vec![],
                    md_type: MDContent::Emphasis("hello, world!".to_string()),
                }],
                md_type: MDContent::Header1("*hello, world!*".to_string()),
            }],
            md_type: MDContent::Root,
        };

        assert_eq!(parse("# *hello, world!*"), expected);
    }
}
