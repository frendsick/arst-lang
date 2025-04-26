use crate::defs::{DELIMITERS, Keyword, Literal, Operator, Symbol, Token, TokenType};

const NEWLINE: char = 0xA as char;

pub fn parse_tokens(code: &str) -> Vec<Token> {
    let mut tokens = Vec::new();
    let mut unparsed_code = code;
    while let Some(token) = get_next_token(unparsed_code) {
        unparsed_code = unparsed_code[token.value.len()..].trim_start();
        tokens.push(token);
    }

    // Make sure the whole code was parsed
    assert!(unparsed_code.is_empty());

    tokens
}

/// The first character must not be a whitespace
fn get_next_token(code: &str) -> Option<Token> {
    let mut chars = code.chars().peekable();

    // Single character tokens
    let first = chars.next()?;
    assert!(!first.is_whitespace());

    #[cfg_attr(any(), rustfmt::skip)]
    match first {
        '=' => return Some(Token::new("=", TokenType::Operator(Operator::Assignment))),
        '+' => return Some(Token::new("+", TokenType::Operator(Operator::Addition))),
        '-' => {
            let next = chars.peek();
            if next == Some(&'>') {
                return Some(Token::new("->", TokenType::Symbol(Symbol::Arrow)));
            }
            return Some(Token::new("-", TokenType::Operator(Operator::Subtraction)))
        },
        '*' => return Some(Token::new("*", TokenType::Operator(Operator::Multiplication))),
        '/' => {
            let next = chars.peek();
            if next == Some(&'/') {
                return parse_single_line_comment_token(code);
            }
            return Some(Token::new("/", TokenType::Operator(Operator::Division)));
        }
        '}' => return Some(Token::new("}", TokenType::Symbol(Symbol::CloseCurly))),
        ')' => return Some(Token::new(")", TokenType::Symbol(Symbol::CloseParen))),
        ':' => return Some(Token::new(":", TokenType::Symbol(Symbol::Colon))),
        ',' => return Some(Token::new(",", TokenType::Symbol(Symbol::Comma))),
        '{' => return Some(Token::new("{", TokenType::Symbol(Symbol::OpenCurly))),
        '(' => return Some(Token::new("(", TokenType::Symbol(Symbol::OpenParen))),
        ';' => return Some(Token::new(";", TokenType::Symbol(Symbol::Semicolon))),
        '"' => return parse_string_literal_token(code),
        _ => {}
    }

    // Other tokens
    let value = parse_until_whitespace_or_delimiter(code);
    match value {
        "" => None,
        "let" => Some(Token::new("let", TokenType::Keyword(Keyword::Let))),
        v if let Some(token) = parse_integer_literal_token(v) => Some(token),
        _ => Some(Token::new(value, TokenType::Identifier)),
    }
}

fn parse_single_line_comment_token(code: &str) -> Option<Token> {
    let mut chars = code.char_indices();
    let (_, first) = chars.next()?;
    let (mut end_index, second) = chars.next()?;

    if first != '/' || second != '/' {
        return None;
    }

    for (index, char) in chars {
        if char == NEWLINE {
            break;
        }
        end_index = index;
    }

    let slice = &code[..=end_index];
    Some(Token::new(slice, TokenType::NoOperation))
}

fn parse_integer_literal_token(code: &str) -> Option<Token> {
    let mut chars = code.char_indices();
    let (mut end_index, first) = chars.next()?;

    if first != '-' && !first.is_ascii_digit() {
        return None;
    }

    for (index, char) in chars {
        if !char.is_ascii_digit() {
            break;
        }
        end_index = index;
    }

    let slice = &code[..=end_index];
    let integer = slice.parse::<i32>().ok()?;

    Some(Token::new(
        slice,
        TokenType::Literal(Literal::Integer(integer)),
    ))
}

fn parse_string_literal_token(code: &str) -> Option<Token> {
    let mut chars = code.char_indices();
    let (mut end_index, first) = chars.next()?;

    if first != '"' {
        return None;
    }

    for (index, char) in chars {
        // String should be on single line
        if char == NEWLINE {
            return None;
        }
        if char == '"' {
            break;
        }
        end_index = index;
    }

    let slice = &code[1..=end_index];
    Some(Token::new(
        &format!("\"{}\"", slice),
        TokenType::Literal(Literal::String(slice.to_string())),
    ))
}

fn parse_until_whitespace_or_delimiter(code: &str) -> &str {
    let end_index = code
        .char_indices()
        .find(|&(_, c)| c.is_whitespace() || DELIMITERS.contains(&c))
        .map(|(idx, _)| idx)
        .unwrap_or(code.len());

    &code[..end_index]
}
