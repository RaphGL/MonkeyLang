use std::collections::HashMap;
use std::{iter::Peekable, str::Chars};

#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    Illegal,
    Eof,

    // identifiers + literals
    Ident(String),
    Int(String),

    // operators
    Assign,   // =
    Plus,     // +
    Minus,    // -
    Bang,     // !
    Asterisk, // *
    Slash,    // /

    LT,    // <
    GT,    // >
    Eq,    // ==
    NotEq, // !=

    // delimiters
    Comma,
    Semicolon,

    LParen,
    RParen,
    LBrace,
    RBrace,

    // keywords
    Function,
    Let,
    True,
    False,
    If,
    Else,
    Return,
}

pub struct Lexer<'a> {
    chars: Peekable<Chars<'a>>,
    keywords: HashMap<&'a str, Token>,
}

impl<'a> Lexer<'a> {
    pub fn new(text: &'a str) -> Self {
        Self {
            chars: text.chars().peekable(),
            keywords: HashMap::from([
                ("fn", Token::Function),
                ("let", Token::Let),
                ("true", Token::True),
                ("false", Token::False),
                ("if", Token::If),
                ("else", Token::Else),
                ("return", Token::Return),
            ]),
        }
    }

    pub fn next_token(&mut self) -> Token {
        self.skip_whitespace();

        let Some(ch) = self.chars.next() else {
            return Token::Eof;
        };

        match ch {
            '=' => match self.chars.peek() {
                Some(next_ch) => {
                    if *next_ch == '=' {
                        self.chars.next().unwrap();
                        Token::Eq
                    } else {
                        Token::Assign
                    }
                }

                None => Token::Assign,
            },

            ';' => Token::Semicolon,
            '(' => Token::LParen,
            ')' => Token::RParen,
            ',' => Token::Comma,
            '+' => Token::Plus,
            '{' => Token::LBrace,
            '}' => Token::RBrace,
            '-' => Token::Minus,

            '!' => match self.chars.peek() {
                Some(next_ch) => {
                    if *next_ch == '=' {
                        self.chars.next().unwrap();
                        Token::NotEq
                    } else {
                        Token::Bang
                    }
                }

                None => Token::Bang,
            },

            '*' => Token::Asterisk,
            '/' => Token::Slash,
            '<' => Token::LT,
            '>' => Token::GT,

            _ => {
                if ch.is_alphabetic() {
                    let mut ident = self.read_identifier();
                    ident.insert(0, ch);
                    self.lookup_ident(ident.as_str())
                } else if ch.is_numeric() {
                    let mut num = self.read_number();
                    num.insert(0, ch);
                    Token::Int(num)
                } else {
                    Token::Illegal
                }
            }
        }
    }

    fn skip_whitespace(&mut self) {
        while let Some(ch) = self.chars.peek() {
            if !ch.is_whitespace() {
                break;
            }
            self.chars.next();
        }
    }

    fn read_identifier(&mut self) -> String {
        let mut ident = String::new();

        while let Some(ch) = self.chars.peek() {
            if ch.is_alphabetic() || *ch == '_' {
                ident.push(*ch);
                self.chars.next();
            } else {
                break;
            }
        }

        ident
    }

    // looks up if ident if a keyword, if not it creates an identifier token
    fn lookup_ident(&self, ident: &str) -> Token {
        match self.keywords.get(ident) {
            Some(keyword) => keyword.clone(),
            None => Token::Ident(ident.into()),
        }
    }

    fn read_number(&mut self) -> String {
        let mut num = String::new();

        while let Some(ch) = self.chars.peek() {
            if ch.is_numeric() {
                num.push(*ch);
                self.chars.next();
            } else {
                break;
            }
        }

        num
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn next_token() {
        let input = r#"
            let five = 5;
            let ten = 10;

            let add = fn(x, y) {
                x + y;
            };

            let result = add(five, ten);

            !-/*5;
            5 < 10 > 5;

            if (5 < 10) {
                return true;
            } else {
                return false;
            }

            10 == 10;
            10 != 9;
        "#;

        let expected_tokens = [
            Token::Let,
            Token::Ident("five".into()),
            Token::Assign,
            Token::Int("5".into()),
            Token::Semicolon,
            Token::Let,
            Token::Ident("ten".into()),
            Token::Assign,
            Token::Int("10".into()),
            Token::Semicolon,
            Token::Let,
            Token::Ident("add".into()),
            Token::Assign,
            Token::Function,
            Token::LParen,
            Token::Ident("x".into()),
            Token::Comma,
            Token::Ident("y".into()),
            Token::RParen,
            Token::LBrace,
            Token::Ident("x".into()),
            Token::Plus,
            Token::Ident("y".into()),
            Token::Semicolon,
            Token::RBrace,
            Token::Semicolon,
            Token::Let,
            Token::Ident("result".into()),
            Token::Assign,
            Token::Ident("add".into()),
            Token::LParen,
            Token::Ident("five".into()),
            Token::Comma,
            Token::Ident("ten".into()),
            Token::RParen,
            Token::Semicolon,
            Token::Bang,
            Token::Minus,
            Token::Slash,
            Token::Asterisk,
            Token::Int("5".into()),
            Token::Semicolon,
            Token::Int("5".into()),
            Token::LT,
            Token::Int("10".into()),
            Token::GT,
            Token::Int("5".into()),
            Token::Semicolon,
            Token::If,
            Token::LParen,
            Token::Int("5".into()),
            Token::LT,
            Token::Int("10".into()),
            Token::RParen,
            Token::LBrace,
            Token::Return,
            Token::True,
            Token::Semicolon,
            Token::RBrace,
            Token::Else,
            Token::LBrace,
            Token::Return,
            Token::False,
            Token::Semicolon,
            Token::RBrace,
            // 10 == 10;
            // 10 != 9;
            Token::Int("10".into()),
            Token::Eq,
            Token::Int("10".into()),
            Token::Semicolon,
            Token::Int("10".into()),
            Token::NotEq,
            Token::Int("9".into()),
            Token::Semicolon,
            Token::Eof,
        ];

        let mut lexer = Lexer::new(input);
        for token in expected_tokens {
            let next_token = lexer.next_token();
            assert_eq!(token, next_token);
        }
    }
}
