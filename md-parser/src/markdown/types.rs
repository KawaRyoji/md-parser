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
    pub md_type: MDContent,
    pub children: Vec<Token>,
}
