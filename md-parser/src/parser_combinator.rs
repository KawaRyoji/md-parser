use regex::Regex;

pub trait Parser<T>: Fn(&str) -> Option<(T, &str)> {
    fn map<U>(self, f: impl Fn(T) -> U) -> impl Parser<U>;
    fn flat_map<U>(self, f: impl Fn(T) -> Option<U>) -> impl Parser<U>;
    fn or(self, other: impl Parser<T>) -> impl Parser<T>;
    fn and<U>(self, other: impl Parser<U>) -> impl Parser<(T, U)>;
}

impl<T, F> Parser<T> for F
where
    F: Fn(&str) -> Option<(T, &str)>,
{
    fn map<U>(self, f: impl Fn(T) -> U) -> impl Parser<U> {
        move |input: &str| self(input).map(|(t, rest)| (f(t), rest))
    }

    fn flat_map<U>(self, f: impl Fn(T) -> Option<U>) -> impl Parser<U> {
        move |input: &str| match self(input) {
            Some((t, rest)) => f(t).map(|u| (u, rest)),
            None => None,
        }
    }

    fn or(self, other: impl Parser<T>) -> impl Parser<T> {
        move |input: &str| match self(input) {
            Some(result) => Some(result),
            None => other(input),
        }
    }

    fn and<U>(self, other: impl Parser<U>) -> impl Parser<(T, U)> {
        move |input: &str| match self(input) {
            Some((t, rest)) => other(rest).map(|(u, rest)| ((t, u), rest)),
            None => None,
        }
    }
}

pub fn character(c: char) -> impl Parser<()> {
    move |input: &str| {
        if input.starts_with(c) {
            Some(((), &input[c.len_utf8()..]))
        } else {
            None
        }
    }
}

pub fn string<'a>(s: &'a str) -> impl Parser<()> + 'a {
    move |input: &str| {
        if input.starts_with(s) {
            Some(((), &input[s.len()..]))
        } else {
            None
        }
    }
}

pub fn digits() -> impl Parser<i32> {
    move |s: &str| {
        let end = s.find(|c: char| !c.is_digit(10)).unwrap_or(s.len());
        match s[..end].parse() {
            Ok(n) => Some((n, &s[end..])),
            Err(_) => None,
        }
    }
}

pub fn lexeme<T>(parser: impl Parser<T>) -> impl Parser<T> {
    move |input: &str| parser(input.trim_start())
}

pub fn many<T>(parser: impl Parser<T>) -> impl Parser<Vec<T>> {
    move |mut input: &str| {
        let mut result = Vec::new();
        while let Some((t, rest)) = parser(input) {
            result.push(t);
            input = rest;
        }
        Some((result, input))
    }
}

pub fn many1<T>(parser: impl Parser<T>) -> impl Parser<Vec<T>> {
    move |input: &str| {
        let (t, rest) = parser(input)?;
        let mut result = vec![t];
        let mut input = rest;
        while let Some((t, rest)) = parser(input) {
            result.push(t);
            input = rest;
        }
        Some((result, input))
    }
}

pub fn separated<T>(parser: impl Parser<T>, separator: impl Parser<()>) -> impl Parser<Vec<T>> {
    move |mut input: &str| {
        let mut result = Vec::new();
        if let Some((t, rest)) = parser(input) {
            result.push(t);
            input = rest;
        } else {
            return None;
        }

        while let Some((_, rest)) = separator(input) {
            if let Some((t, rest)) = parser(rest) {
                result.push(t);
                input = rest;
            } else {
                return None;
            }
        }

        Some((result, input))
    }
}

pub fn regex<'a, T>(pattern: &'a Regex, f: impl Fn(&str) -> Option<T> + 'a) -> impl Parser<T> + 'a {
    move |input| {
        pattern
            .find(input)
            .and_then(|m| f(m.as_str()).map(|t| (t, &input[m.end()..])))
    }
}
#[macro_export]
macro_rules! regex {
    ($pattern:expr, $f:expr) => {{
        use once_cell::sync::Lazy;
        use regex::Regex;
        static RE: Lazy<Regex> = Lazy::new(|| Regex::new($pattern).unwrap());
        regex(&RE, $f)
    }};
}

#[cfg(test)]
mod tests {
    use regex::Regex;

    use super::*;

    #[test]
    fn test_character() {
        let parser = character('a');

        assert_eq!(parser("abc"), Some(((), "bc")));
        assert_eq!(parser("def"), None);
        assert_eq!(parser(""), None);
    }

    #[test]
    fn test_string() {
        let parser = string("abc");

        assert_eq!(parser("abc"), Some(((), "")));
        assert_eq!(parser("abcdef"), Some(((), "def")));
        assert_eq!(parser("def"), None);
        assert_eq!(parser(""), None);
    }

    #[test]
    fn test_map() {
        let parser = character('a').map(|_| 1);

        assert_eq!(parser("abc"), Some((1, "bc")));
        assert_eq!(parser("def"), None);
        assert_eq!(parser(""), None);
    }

    #[test]
    fn test_or() {
        let parser = character('a').or(character('b'));

        assert_eq!(parser("abc"), Some(((), "bc")));
        assert_eq!(parser("bcd"), Some(((), "cd")));
        assert_eq!(parser("def"), None);
        assert_eq!(parser(""), None);
    }

    #[test]
    fn test_and() {
        let parser = lexeme(digits()).and(lexeme(digits())).and(lexeme(digits()));

        assert_eq!(parser("1 2 3"), Some((((1, 2), 3), "")));
        // 本当はこうしたい
        // assert_eq!(parser("1 2 3"), Some(((1, 2, 3), "")));
        assert_eq!(parser("1 2 3abc"), Some((((1, 2), 3), "abc")));
        assert_eq!(parser("abc"), None);
        assert_eq!(parser(""), None);
    }

    #[test]
    fn test_digit() {
        let parser = digits();

        assert_eq!(parser("123"), Some((123, "")));
        assert_eq!(parser("12abc"), Some((12, "abc")));
        assert_eq!(parser("abc"), None);
        assert_eq!(parser(""), None);
    }

    #[test]
    fn test_many() {
        let parser = many(lexeme(digits()));

        assert_eq!(parser("1 2 3"), Some((vec![1, 2, 3], "")));
        assert_eq!(parser("1 2 3abc"), Some((vec![1, 2, 3], "abc")));
        assert_eq!(parser("abc"), Some((vec![], "abc")));
        assert_eq!(parser(""), Some((vec![], "")));
    }

    #[test]
    fn test_many1() {
        let parser = many1(lexeme(digits()));

        assert_eq!(parser("1 2 3"), Some((vec![1, 2, 3], "")));
        assert_eq!(parser("1 2 3abc"), Some((vec![1, 2, 3], "abc")));
        assert_eq!(parser("abc"), None);
        assert_eq!(parser(""), None);
    }

    #[test]
    fn test_separated() {
        let parser = separated(digits(), character(','));

        assert_eq!(parser("1,2,3"), Some((vec![1, 2, 3], "")));
        assert_eq!(parser("1,2,3abc"), Some((vec![1, 2, 3], "abc")));
        assert_eq!(parser("1a"), Some((vec![1], "a")));
        assert_eq!(parser("abc"), None);
        assert_eq!(parser(""), None);
    }

    #[test]
    fn test_regex() {
        let pattern = Regex::new(r"\d+").unwrap();
        let parser = regex(&pattern, |s| Some(s.to_owned()));

        assert_eq!(parser("123abc"), Some(("123".to_owned(), "abc")));
        assert_eq!(parser("abc"), None);
        assert_eq!(parser(""), None);
    }
}
