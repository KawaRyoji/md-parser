fn main() {
    let input = stdin();
    let objects = parse_markdown(&input);
    let output = render_markdown(objects);
    stdout(&output);
}

fn stdin() -> String {
    let mut buffer = String::new();
    std::io::stdin().read_line(&mut buffer).unwrap();
    buffer.trim().to_string()
}

fn stdout(s: &str) {
    println!("{}", s);
}

fn stderr(s: &str) {
    eprintln!("{}", s);
}

#[derive(Debug, PartialEq)]
enum MarkdownType {
    Header1,
    Header2,
    Header3,
    Header4,
    Header5,
    Header6,
    Paragraph,
    List,
    ListItem,
}

#[derive(Debug, PartialEq)]
struct MarkdownObject {
    _type: MarkdownType,
    content: String,
}

fn parse_line(line: &str) -> MarkdownObject {
    if line.starts_with("# ") {
        MarkdownObject {
            _type: MarkdownType::Header1,
            content: line[2..].to_string(),
        }
    } else if line.starts_with("## ") {
        MarkdownObject {
            _type: MarkdownType::Header2,
            content: line[3..].to_string(),
        }
    } else if line.starts_with("### ") {
        MarkdownObject {
            _type: MarkdownType::Header3,
            content: line[4..].to_string(),
        }
    } else if line.starts_with("#### ") {
        MarkdownObject {
            _type: MarkdownType::Header4,
            content: line[5..].to_string(),
        }
    } else if line.starts_with("##### ") {
        MarkdownObject {
            _type: MarkdownType::Header5,
            content: line[6..].to_string(),
        }
    } else if line.starts_with("###### ") {
        MarkdownObject {
            _type: MarkdownType::Header6,
            content: line[7..].to_string(),
        }
    } else {
        MarkdownObject {
            _type: MarkdownType::Paragraph,
            content: line.to_string(),
        }
    }
}

fn parse_markdown(input: &str) -> Vec<MarkdownObject> {
    input
        .lines()
        .map(parse_line)
        .collect::<Vec<MarkdownObject>>()
}

fn render_markdown(objects: Vec<MarkdownObject>) -> String {
    objects
        .iter()
        .map(|obj| match obj._type {
            MarkdownType::Header1 => format!("<h1>{}</h1>", obj.content),
            MarkdownType::Header2 => format!("<h2>{}</h2>", obj.content),
            MarkdownType::Header3 => format!("<h3>{}</h3>", obj.content),
            MarkdownType::Header4 => format!("<h4>{}</h4>", obj.content),
            MarkdownType::Header5 => format!("<h5>{}</h5>", obj.content),
            MarkdownType::Header6 => format!("<h6>{}</h6>", obj.content),
            MarkdownType::Paragraph => format!("<p>{}</p>", obj.content),
            _ => "".to_string(),
        })
        .collect::<Vec<String>>()
        .join("\n")
}

#[cfg(test)]
mod tests {
    use crate::*;

    #[test]
    // 見出し(#, ##, ###, ...)が正しいオブジェクトに変換される
    fn test_heading() {
        let input = "# Header1";
        let output = MarkdownObject {
            _type: MarkdownType::Header1,
            content: "Header1".to_string(),
        };
        assert_eq!(parse_line(input), output);

        let input = "## Header2";
        let output = MarkdownObject {
            _type: MarkdownType::Header2,
            content: "Header2".to_string(),
        };
        assert_eq!(parse_line(input), output);

        let input = "### Header3";
        let output = MarkdownObject {
            _type: MarkdownType::Header3,
            content: "Header3".to_string(),
        };
        assert_eq!(parse_line(input), output);

        let input = "#### Header4";
        let output = MarkdownObject {
            _type: MarkdownType::Header4,
            content: "Header4".to_string(),
        };
        assert_eq!(parse_line(input), output);

        let input = "##### Header5";
        let output = MarkdownObject {
            _type: MarkdownType::Header5,
            content: "Header5".to_string(),
        };
        assert_eq!(parse_line(input), output);

        let input = "###### Header6";
        let output = MarkdownObject {
            _type: MarkdownType::Header6,
            content: "Header6".to_string(),
        };
        assert_eq!(parse_line(input), output);
    }

    #[test]
    // パラグラフが正しいオブジェクトに変換される
    fn test_paragraph() {
        let input = "Paragraph";
        let output = MarkdownObject {
            _type: MarkdownType::Paragraph,
            content: "Paragraph".to_string(),
        };
        assert_eq!(parse_line(input), output);
    }

    #[test]
    // 見出しとパラグラフを含むオブジェクトが正しくレンダリングされる
    fn test_render_markdown() {
        let input = vec![
            MarkdownObject {
                _type: MarkdownType::Header1,
                content: "Header1".to_string(),
            },
            MarkdownObject {
                _type: MarkdownType::Header2,
                content: "Header2".to_string(),
            },
            MarkdownObject {
                _type: MarkdownType::Header3,
                content: "Header3".to_string(),
            },
            MarkdownObject {
                _type: MarkdownType::Header4,
                content: "Header4".to_string(),
            },
            MarkdownObject {
                _type: MarkdownType::Header5,
                content: "Header5".to_string(),
            },
            MarkdownObject {
                _type: MarkdownType::Header6,
                content: "Header6".to_string(),
            },
            MarkdownObject {
                _type: MarkdownType::Paragraph,
                content: "Paragraph".to_string(),
            },
        ];
        let output = "<h1>Header1</h1>\n<h2>Header2</h2>\n<h3>Header3</h3>\n<h4>Header4</h4>\n<h5>Header5</h5>\n<h6>Header6</h6>\n<p>Paragraph</p>";
        assert_eq!(render_markdown(input), output);
    }
}
