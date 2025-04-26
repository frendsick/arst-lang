use std::fmt;

pub const DELIMITERS: [char; 7] = [',', ':', ';', '(', ')', '{', '}'];

#[derive(Debug, PartialEq)]
pub enum TokenType {
    // Literals
    Integer(i32),

    // Other
    Operator(Operator),
    Symbol(Symbol),
    Keyword(Keyword),
    Identifier,
}

#[derive(Debug, PartialEq)]
pub enum Keyword {
    Let,
}

#[derive(Debug, PartialEq)]
pub enum Operator {
    // Arithmetic
    Addition,
    Subtraction,
    Multiplication,
    Division,

    // Assignment
    Assignment,
}

#[derive(Debug, PartialEq)]
pub enum Symbol {
    Arrow,
    CloseCurly,
    CloseParen,
    Colon,
    Comma,
    OpenCurly,
    OpenParen,
    Semicolon,
}

impl fmt::Display for Keyword {
    fn fmt(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        write!(formatter, "{}", format!("{:?}", self).to_lowercase())
    }
}

#[derive(Debug)]
pub struct Token {
    pub value: String,
    pub typ: TokenType,
}

impl Token {
    pub fn new(value: &str, typ: TokenType) -> Self {
        Self {
            value: value.to_string(),
            typ,
        }
    }
}
