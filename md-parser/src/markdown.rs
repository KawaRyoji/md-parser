use crate::parser_combinator::*;
use std::vec;

#[derive(Debug, PartialEq, Clone)]
pub enum MDContent {
    Root,
    LineBreak,
    HorizontalRule,
    Header1(String),
    Header2(String),
    Header3(String),
    Header4(String),
    Header5(String),
    Header6(String),
    Paragraph(String),
    Blockquote(String),
    CodeBlock(String),
    Emphasis(String),
    Strong(String),
    Strikethrough(String),
    UnorderedList(Vec<(u8, String)>),
    OrderedList(Vec<(u8, String)>),
    Link(String, String),
    Image(String, String),
}

#[derive(Debug, PartialEq, Clone)]
pub struct Token {
    md_type: MDContent,
    children: Vec<Token>,
}

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

pub fn render(token: &Token) -> String {
    if token.children.is_empty() {
        return render_content(&token.md_type);
    }

    let render_children = token
        .children
        .iter()
        .map(|child| render(child))
        .collect::<Vec<String>>()
        .join("");

    render_string_content(&token.md_type, render_children.as_str())
        .unwrap_or(render_content(&token.md_type))
}

pub fn render_string_content(markdown: &MDContent, content: &str) -> Option<String> {
    match markdown {
        MDContent::Root => Some(format!("<div>{}</div>", content)),
        MDContent::LineBreak => Some("".to_string()),
        MDContent::HorizontalRule => Some("<hr>".to_string()),
        MDContent::Header1(_) => Some(format!("<h1>{}</h1>", content)),
        MDContent::Header2(_) => Some(format!("<h2>{}</h2>", content)),
        MDContent::Header3(_) => Some(format!("<h3>{}</h3>", content)),
        MDContent::Header4(_) => Some(format!("<h4>{}</h4>", content)),
        MDContent::Header5(_) => Some(format!("<h5>{}</h5>", content)),
        MDContent::Header6(_) => Some(format!("<h6>{}</h6>", content)),
        MDContent::Paragraph(_) => Some(format!("<p>{}</p>", content)),
        MDContent::Blockquote(_) => Some(format!("<blockquote>{}</blockquote>", content)),
        MDContent::CodeBlock(_) => Some(format!("<pre><code>{}</code></pre>", content)),
        MDContent::Emphasis(_) => Some(format!("<em>{}</em>", content)),
        MDContent::Strong(_) => Some(format!("<strong>{}</strong>", content)),
        MDContent::Strikethrough(_) => Some(format!("<del>{}</del>", content)),
        _ => None,
    }
}

pub fn render_list_content(markdown: &MDContent, contents: Vec<String>) -> Option<String> {
    match markdown {
        MDContent::UnorderedList(_) => {
            let items = contents
                .into_iter()
                .map(|item| format!("<li>{}</li>", item))
                .collect::<Vec<String>>()
                .join("");
            Some(format!("<ul>{}</ul>", items))
        }
        MDContent::OrderedList(_) => {
            let items = contents
                .into_iter()
                .map(|item| format!("<li>{}</li>", item))
                .collect::<Vec<String>>()
                .join("");
            Some(format!("<ol>{}</ol>", items))
        }
        _ => None,
    }
}

pub fn render_content(markdown: &MDContent) -> String {
    match markdown {
        MDContent::Root => "".to_string(),
        MDContent::LineBreak => "".to_string(),
        MDContent::Header1(text) => format!("<h1>{}</h1>", text),
        MDContent::Header2(text) => format!("<h2>{}</h2>", text),
        MDContent::Header3(text) => format!("<h3>{}</h3>", text),
        MDContent::Header4(text) => format!("<h4>{}</h4>", text),
        MDContent::Header5(text) => format!("<h5>{}</h5>", text),
        MDContent::Header6(text) => format!("<h6>{}</h6>", text),
        MDContent::Paragraph(text) => format!("<p>{}</p>", text),
        MDContent::Blockquote(text) => format!("<blockquote>{}</blockquote>", text),
        MDContent::CodeBlock(text) => format!("<pre><code>{}</code></pre>", text),
        MDContent::UnorderedList(items) => {
            let items = items
                .into_iter()
                .map(|item| format!("<li>{}</li>", item.1))
                .collect::<Vec<String>>()
                .join("");
            format!("<ul>{}</ul>", items)
        }
        MDContent::OrderedList(items) => {
            let items = items
                .into_iter()
                .map(|item| format!("<li>{}</li>", item.1))
                .collect::<Vec<String>>()
                .join("");
            format!("<ol>{}</ol>", items)
        }
        MDContent::HorizontalRule => "<hr>".to_string(),
        MDContent::Link(text, url) => format!("<a href=\"{}\">{}</a>", url, text),
        MDContent::Image(text, url) => format!("<img src=\"{}\" alt=\"{}\">", url, text),
        MDContent::Emphasis(text) => format!("<em>{}</em>", text),
        MDContent::Strong(text) => format!("<strong>{}</strong>", text),
        MDContent::Strikethrough(text) => format!("<del>{}</del>", text),
    }
}

fn all_character(s: &str) -> Option<(char, &str)> {
    crate::regex!(r#"^[^"\\[:cntrl:]]"#, |s| s.chars().next())
        .or(crate::regex!(r#"^\\u[0-9a-fA-F]{4}"#, hex_code))
        .or(crate::regex!(r#"^\\."#, escape))(s)
}

fn markdown_character(s: &str) -> Option<(char, &str)> {
    crate::regex!(r#"^[^"\\#\\*\\_\\[:cntrl:]]"#, |s| s.chars().next())
        .or(crate::regex!(r#"^\\u[0-9a-fA-F]{4}"#, hex_code))
        .or(crate::regex!(r#"^\\."#, escape))(s)
}

fn hex_code(code: &str) -> Option<char> {
    code.strip_prefix("\\u")
        .and_then(|code| u32::from_str_radix(code, 16).ok())
        .and_then(std::char::from_u32)
}

fn escape(s: &str) -> Option<char> {
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
    use super::*;

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

    #[test]
    fn test_render() {
        let root = Token {
            md_type: MDContent::Root,
            children: vec![
                Token {
                    md_type: MDContent::Header1("*Hello, world!*".to_string()),
                    children: vec![Token {
                        md_type: MDContent::Emphasis("Hello, world!".to_string()),
                        children: vec![],
                    }],
                },
                Token {
                    md_type: MDContent::Header2("Hello, world!".to_string()),
                    children: vec![],
                },
            ],
        };
        let expected = "<div><h1><em>Hello, world!</em></h1><h2>Hello, world!</h2></div>";

        assert_eq!(render(&root), expected);
    }

    #[test]
    fn test_render_header1() {
        let markdown = MDContent::Header1("Hello, world!".to_string());
        let expected = "<h1>Hello, world!</h1>";

        assert_eq!(render_content(&markdown), expected);
    }

    #[test]
    fn test_render_header2() {
        let markdown = MDContent::Header2("Hello, world!".to_string());
        let expected = "<h2>Hello, world!</h2>";

        assert_eq!(render_content(&markdown), expected);
    }

    #[test]
    fn test_render_header3() {
        let markdown = MDContent::Header3("Hello, world!".to_string());
        let expected = "<h3>Hello, world!</h3>";

        assert_eq!(render_content(&markdown), expected);
    }

    #[test]
    fn test_render_header4() {
        let markdown = MDContent::Header4("Hello, world!".to_string());
        let expected = "<h4>Hello, world!</h4>";

        assert_eq!(render_content(&markdown), expected);
    }

    #[test]
    fn test_render_header5() {
        let markdown = MDContent::Header5("Hello, world!".to_string());
        let expected = "<h5>Hello, world!</h5>";

        assert_eq!(render_content(&markdown), expected);
    }

    #[test]
    fn test_render_header6() {
        let markdown = MDContent::Header6("Hello, world!".to_string());
        let expected = "<h6>Hello, world!</h6>";

        assert_eq!(render_content(&markdown), expected);
    }

    #[test]
    fn test_render_paragraph() {
        let markdown = MDContent::Paragraph("Hello, world!".to_string());
        let expected = "<p>Hello, world!</p>";

        assert_eq!(render_content(&markdown), expected);
    }

    #[test]
    fn test_render_blockquote() {
        let markdown = MDContent::Blockquote("Hello, world!".to_string());
        let expected = "<blockquote>Hello, world!</blockquote>";

        assert_eq!(render_content(&markdown), expected);
    }

    #[test]
    fn test_render_code_block() {
        let markdown = MDContent::CodeBlock("Hello, world!".to_string());
        let expected = "<pre><code>Hello, world!</code></pre>";

        assert_eq!(render_content(&markdown), expected);
    }

    #[test]
    fn test_render_unordered_list() {
        let markdown =
            MDContent::UnorderedList(vec![(0, "Hello".to_string()), (0, "World".to_string())]);
        let expected = "<ul><li>Hello</li><li>World</li></ul>";

        assert_eq!(render_content(&markdown), expected);
    }

    #[test]
    fn test_render_ordered_list() {
        let markdown =
            MDContent::OrderedList(vec![(0, "Hello".to_string()), (0, "World".to_string())]);
        let expected = "<ol><li>Hello</li><li>World</li></ol>";

        assert_eq!(render_content(&markdown), expected);
    }

    #[test]
    fn test_render_horizontal_rule() {
        let markdown = MDContent::HorizontalRule;
        let expected = "<hr>";

        assert_eq!(render_content(&markdown), expected);
    }

    #[test]
    fn test_render_link() {
        let markdown = MDContent::Link("Hello".to_string(), "https://example.com".to_string());
        let expected = "<a href=\"https://example.com\">Hello</a>";

        assert_eq!(render_content(&markdown), expected);
    }

    #[test]
    fn test_render_image() {
        let markdown = MDContent::Image(
            "Hello".to_string(),
            "https://example.com/image.png".to_string(),
        );
        let expected = "<img src=\"https://example.com/image.png\" alt=\"Hello\">";

        assert_eq!(render_content(&markdown), expected);
    }

    #[test]
    fn test_render_emphasis() {
        let markdown = MDContent::Emphasis("Hello".to_string());
        let expected = "<em>Hello</em>";

        assert_eq!(render_content(&markdown), expected);
    }

    #[test]
    fn test_render_strong() {
        let markdown = MDContent::Strong("Hello".to_string());
        let expected = "<strong>Hello</strong>";

        assert_eq!(render_content(&markdown), expected);
    }

    #[test]
    fn test_render_strikethrough() {
        let markdown = MDContent::Strikethrough("Hello".to_string());
        let expected = "<del>Hello</del>";

        assert_eq!(render_content(&markdown), expected);
    }
}
