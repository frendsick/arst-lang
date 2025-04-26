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

    let next = chars.peek();
    #[cfg_attr(any(), rustfmt::skip)]
    match (first, next) {
        ('=', _) => return Some(Token::new("=", TokenType::Operator(Operator::Assignment))),
        ('+', _) => return Some(Token::new("+", TokenType::Operator(Operator::Addition))),
        ('-', Some('>')) => return Some(Token::new("->", TokenType::Symbol(Symbol::Arrow))),
        ('-', _) => return Some(Token::new("-", TokenType::Operator(Operator::Subtraction))),
        ('*', _) => return Some(Token::new("*", TokenType::Operator(Operator::Multiplication))),
        ('/', Some('/')) => return parse_single_line_comment_token(code),
        ('/', Some('*')) => return parse_multi_line_comment_token(code),
        ('/', _) => return Some(Token::new("/", TokenType::Operator(Operator::Division))),
        ('}', _) => return Some(Token::new("}", TokenType::Symbol(Symbol::CloseCurly))),
        (')', _) => return Some(Token::new(")", TokenType::Symbol(Symbol::CloseParen))),
        (':', _) => return Some(Token::new(":", TokenType::Symbol(Symbol::Colon))),
        (',', _) => return Some(Token::new(",", TokenType::Symbol(Symbol::Comma))),
        ('{', _) => return Some(Token::new("{", TokenType::Symbol(Symbol::OpenCurly))),
        ('(', _) => return Some(Token::new("(", TokenType::Symbol(Symbol::OpenParen))),
        (';', _) => return Some(Token::new(";", TokenType::Symbol(Symbol::Semicolon))),
        ('"', _) => return parse_string_literal_token(code),
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
    Some(Token::new(slice, TokenType::Comment))
}

fn parse_multi_line_comment_token(code: &str) -> Option<Token> {
    let mut chars = code.char_indices().peekable();
    let (_, first) = chars.next()?;
    let (_, second) = chars.next()?;

    if first != '/' || second != '*' {
        return None;
    }

    #[allow(unused_assignments)]
    let mut end_index = 0;
    loop {
        let (_, char) = chars.next()?;
        let (next_index, next) = chars.peek()?;
        if char == '*' && *next == '/' {
            end_index = *next_index;
            break;
        }
    }

    let slice = &code[..=end_index];
    Some(Token::new(slice, TokenType::Comment))
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
