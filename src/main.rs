#![feature(if_let_guard)]
// TODO: Remove this when the project is more mature
#![allow(dead_code)]

mod defs;
mod lexer;

fn main() {
    let code = std::fs::read_to_string("test.arst").expect("Failed to read code file");

    let tokens = lexer::parse_tokens(&code);
    dbg!(&tokens);
}
