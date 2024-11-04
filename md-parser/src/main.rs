fn main() {
    let input = stdin();
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

trait Parser<T>: Fn(&str) -> Option<(T, &str)> {}
impl<T, F> Parser<T> for F where F: Fn(&str) -> Option<(T, &str)> {}

fn character(c: char) -> impl Parser<()> {
    move |input: &str| {
        if input.starts_with(c) {
            Some(((), &input[c.len_utf8()..]))
        } else {
            None
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_character() {
        let parser = character('a');

        assert_eq!(parser("abc"), Some(((), "bc")));
        assert_eq!(parser("def"), None);
        assert_eq!(parser(""), None);
    }
}
