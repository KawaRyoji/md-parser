use super::types::{MDContent, Token};

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

#[cfg(test)]
mod test {
    use crate::markdown::{
        renderer::{render, render_content},
        types::{MDContent, Token},
    };

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
