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
