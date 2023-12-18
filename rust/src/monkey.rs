use lazy_static::lazy_static;
use std::collections::HashMap;

pub trait Node {
    //prints the node name
    fn name(&self) -> String;
    fn to_string(&self) -> String;
}

pub trait Statement: Node {
    fn statement_type(&self) -> String;
}

pub trait Expression: Node {
    fn expression_type(&self) -> String;
}

struct Identifier {}

//In Monkey identifier can be an expression
impl Expression for Identifier {
    fn expression_type(&self) -> String {
        "expression".to_string()
    }
}

impl Node for Identifier {
    fn name(&self) -> String {
        "ident".to_string()
    }

    fn to_string(&self) -> String {
        todo!()
    }
}

struct LetStatement {
    let_token: Token,
    identifier: Identifier,
    value: Box<dyn Expression>,
}

impl Statement for LetStatement {
    fn statement_type(&self) -> String {
        "statement_type".to_string()
    }
}

impl Node for LetStatement {
    fn name(&self) -> String {
        self.let_token.literal.clone()
    }

    fn to_string(&self) -> String {
        format!("let {} = {}", self.identifier.name(), self.value.name())
    }
}

struct MonkeyExpression {}

impl Node for MonkeyExpression {
    fn name(&self) -> String {
        "expression".to_string()
    }

    fn to_string(&self) -> String {
        todo!()
    }
}

impl Expression for MonkeyExpression {
    fn expression_type(&self) -> String {
        "monkey_expression".to_string()
    }
}

pub struct Program {
    pub statements: Vec<Box<dyn Statement>>,
}

pub struct Parser {
    lexer: Lexer,
    current_token: Token,
    peek_token: Token,
}

impl Parser {
    pub fn new(l: Lexer) -> Parser {
        Self {
            lexer: l,
            current_token: Token {
                token_type: TokenType::Illegal,
                literal: "Illegal".to_string(),
            },
            peek_token: Token {
                token_type: TokenType::Illegal,
                literal: "Illegal".to_string(),
            },
        }
    }

    /// Returns the parse of this [`Parser`].
    pub fn parse(&mut self) -> Program {
        let program = Program {
            statements: Vec::new(),
        };
        self.advance_tokens();

        while self.is_valid_token() {
            if self.current_token.token_type == TokenType::Let {
                //handle let statement
            }
            self.advance_tokens();
        }

        return program;
    }

    fn advance_tokens(&mut self) {
        self.current_token = self.lexer.next_token();
        self.peek_token = self.lexer.next_token();
    }

    fn is_valid_token(&mut self) -> bool {
        return self.current_token.token_type != TokenType::EndOfFile;
    }
}

//Lexer tokenizes the code input
pub struct Lexer {
    input: String,
    current_index: usize,
    next_index: usize,
    current_char: char,
}

impl Lexer {
    //new returns the Lexer instance
    pub fn new(input: String) -> Lexer {
        Self {
            input,
            current_char: char::REPLACEMENT_CHARACTER,
            current_index: 0,
            next_index: 0,
        }
    }

    //returns the next token from the input
    pub fn next_token(&mut self) -> Token {
        self.read_char();
        self.skip_whitespace();
        match self.current_char {
            char::REPLACEMENT_CHARACTER => Token::new(TokenType::EndOfFile, "eof".to_string()),
            '+' => Token::new(TokenType::Plus, self.token_value()),
            '-' => Token::new(TokenType::Minus, self.token_value()),
            '=' => {
                if self.peek_char() == '=' {
                    Token::new(TokenType::Equal, self.two_chars_token_value())
                } else {
                    Token::new(TokenType::Assign, self.token_value())
                }
            }
            ';' => Token::new(TokenType::Semicolon, self.token_value()),
            ',' => Token::new(TokenType::Comma, self.token_value()),
            '(' => Token::new(TokenType::LParen, self.token_value()),
            ')' => Token::new(TokenType::RParen, self.token_value()),
            '{' => Token::new(TokenType::LBrace, self.token_value()),
            '}' => Token::new(TokenType::RBrace, self.token_value()),
            '!' => {
                if self.peek_char() == '=' {
                    Token::new(TokenType::NotEqual, self.two_chars_token_value())
                } else {
                    Token::new(TokenType::Bang, self.token_value())
                }
            }
            '*' => Token::new(TokenType::Asterisk, self.token_value()),
            '/' => Token::new(TokenType::Slash, self.token_value()),
            '>' => Token::new(TokenType::GreaterThan, self.token_value()),
            '<' => Token::new(TokenType::LessThan, self.token_value()),
            _ => {
                if is_letter(self.current_char) {
                    let identifier = self.read_identifier();

                    let token_type_option = KEYWORDS.get(identifier.as_str());
                    match token_type_option {
                        Some(token_type) => Token::new(*token_type, identifier),
                        None => Token::new(TokenType::Identifier, identifier),
                    }
                } else if is_digit(self.current_char) {
                    let number = self.read_number();
                    Token::new(TokenType::Int, number)
                } else {
                    Token::new(TokenType::Illegal, "illegal".to_string())
                }
            }
        }
    }

    fn read_number(&mut self) -> String {
        let start_index = self.current_index;

        let mut next_ch = self.peek_char();
        while is_digit(next_ch) {
            self.read_char();
            next_ch = self.peek_char();
        }

        return self.input[start_index..self.next_index].to_string();
    }

    fn read_identifier(&mut self) -> String {
        let start_index = self.current_index;

        let mut next_ch = self.peek_char();
        while is_letter(next_ch) {
            self.read_char();
            next_ch = self.peek_char();
        }

        return self.input[start_index..self.next_index].to_string();
    }

    fn token_value(&mut self) -> String {
        self.current_char.to_string()
    }

    fn two_chars_token_value(&mut self) -> String {
        let ch = self.current_char;
        self.read_char();

        format!("{}{}", ch, self.current_char)
    }

    fn peek_char(&mut self) -> char {
        let ch: Option<char> = self.input.chars().nth(self.next_index);
        match ch {
            Some(c) => c,
            None => char::REPLACEMENT_CHARACTER,
        }
    }

    fn read_char(&mut self) {
        let ch: Option<char> = self.input.chars().nth(self.next_index);
        match ch {
            Some(c) => {
                self.current_char = c;
            }
            None => {
                self.current_char = char::REPLACEMENT_CHARACTER;
            }
        }
        self.current_index = self.next_index;
        self.next_index = self.next_index + 1;
    }

    fn skip_whitespace(&mut self) {
        while self.current_char.is_whitespace()
            || self.current_char == '\n'
            || self.current_char == '\t'
        {
            self.read_char();
        }
    }
}

fn is_letter(ch: char) -> bool {
    return ch.is_ascii_alphabetic() || ch == '_';
}

fn is_digit(ch: char) -> bool {
    return ch.is_ascii_digit();
}

pub struct Token {
    pub token_type: TokenType,
    pub literal: String,
}

impl Token {
    pub fn new(token_type: TokenType, literal: String) -> Self {
        Self {
            token_type,
            literal,
        }
    }
}

lazy_static! {
    static ref KEYWORDS: HashMap<&'static str, TokenType> = {
        let mut map = HashMap::new();
        map.insert("let", TokenType::Let);
        map.insert("fn", TokenType::Function);
        map.insert("true", TokenType::True);
        map.insert("false", TokenType::False);
        map.insert("return", TokenType::Return);
        map.insert("if", TokenType::If);
        map.insert("else", TokenType::Else);

        map
    };
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TokenType {
    Illegal,
    EndOfFile,
    Identifier,
    Int,
    Assign,
    Plus,
    Minus,
    Comma,
    Semicolon,
    LParen,
    RParen,
    LBrace,
    RBrace,
    Bang,
    Asterisk,
    Slash,
    GreaterThan,
    LessThan,
    Function,
    Let,
    If,
    Else,
    Return,
    True,
    False,
    Equal,
    NotEqual,
}

#[cfg(test)]
fn mod_name() -> String {
    "monkey".to_string()
}

#[cfg(test)]
mod tests {
    use super::*;

    struct TestToken {
        token_type: TokenType,
        token_val: String,
    }

    #[test]
    fn test_works() {
        assert_eq!("monkey", mod_name());
    }

    #[test]
    fn parser_let_statement_test() {
        let input = r#"
        let x = 5;
        let y = 10;
        let foobar = 838383;
        "#;

        let lexer = Lexer::new(input.to_string());
        let mut parser = Parser::new(lexer);

        let program = parser.parse();
        for stmt in program.statements.iter() {
            println!("statement name:{}", stmt.name());
            println!("statement value:{}", stmt.to_string());
        }
    }

    #[test]
    fn lexer_tests() {
        let input = String::from("+{}();,");
        println!("First test input {}", input);
        lexer_test(
            input,
            vec![
                test_token(TokenType::Plus, "+"),
                test_token(TokenType::LBrace, "{"),
                test_token(TokenType::RBrace, "}"),
                test_token(TokenType::LParen, "("),
                test_token(TokenType::RParen, ")"),
                test_token(TokenType::Semicolon, ";"),
                test_token(TokenType::Comma, ","),
            ],
        );

        println!("First test finished");
        println!("Second test begins...");

        let input2 = r#"
        let five = 5;
        let ten = 10;
        let add = fn (x, y) {
            x + y;
        };
        let result = add(five + ten);
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
        lexer_test(
            input2.to_string(),
            vec![
                test_token(TokenType::Let, "let"),
                test_token(TokenType::Identifier, "five"),
                test_token(TokenType::Assign, "="),
                test_token(TokenType::Int, "5"),
                test_token(TokenType::Semicolon, ";"),
                test_token(TokenType::Let, "let"),
                test_token(TokenType::Identifier, "ten"),
                test_token(TokenType::Assign, "="),
                test_token(TokenType::Int, "10"),
                test_token(TokenType::Semicolon, ";"),
                test_token(TokenType::Let, "let"),
                test_token(TokenType::Identifier, "add"),
                test_token(TokenType::Assign, "="),
                test_token(TokenType::Function, "fn"),
                test_token(TokenType::LParen, "("),
                test_token(TokenType::Identifier, "x"),
                test_token(TokenType::Comma, ","),
                test_token(TokenType::Identifier, "y"),
                test_token(TokenType::RParen, ")"),
                test_token(TokenType::LBrace, "{"),
                test_token(TokenType::Identifier, "x"),
                test_token(TokenType::Plus, "+"),
                test_token(TokenType::Identifier, "y"),
                test_token(TokenType::Semicolon, ";"),
                test_token(TokenType::RBrace, "}"),
                test_token(TokenType::Semicolon, ";"),
                test_token(TokenType::Let, "let"),
                test_token(TokenType::Identifier, "result"),
                test_token(TokenType::Assign, "="),
                test_token(TokenType::Identifier, "add"),
                test_token(TokenType::LParen, "("),
                test_token(TokenType::Identifier, "five"),
                test_token(TokenType::Plus, "+"),
                test_token(TokenType::Identifier, "ten"),
                test_token(TokenType::RParen, ")"),
                test_token(TokenType::Semicolon, ";"),
                test_token(TokenType::Bang, "!"),
                test_token(TokenType::Minus, "-"),
                test_token(TokenType::Slash, "/"),
                test_token(TokenType::Asterisk, "*"),
                test_token(TokenType::Int, "5"),
                test_token(TokenType::Semicolon, ";"),
                test_token(TokenType::Int, "5"),
                test_token(TokenType::LessThan, "<"),
                test_token(TokenType::Int, "10"),
                test_token(TokenType::GreaterThan, ">"),
                test_token(TokenType::Int, "5"),
                test_token(TokenType::Semicolon, ";"),
                test_token(TokenType::If, "if"),
                test_token(TokenType::LParen, "("),
                test_token(TokenType::Int, "5"),
                test_token(TokenType::LessThan, "<"),
                test_token(TokenType::Int, "10"),
                test_token(TokenType::RParen, ")"),
                test_token(TokenType::LBrace, "{"),
                test_token(TokenType::Return, "return"),
                test_token(TokenType::True, "true"),
                test_token(TokenType::Semicolon, ";"),
                test_token(TokenType::RBrace, "}"),
                test_token(TokenType::Else, "else"),
                test_token(TokenType::LBrace, "{"),
                test_token(TokenType::Return, "return"),
                test_token(TokenType::False, "false"),
                test_token(TokenType::Semicolon, ";"),
                test_token(TokenType::RBrace, "}"),
                test_token(TokenType::Int, "10"),
                test_token(TokenType::Equal, "=="),
                test_token(TokenType::Int, "10"),
                test_token(TokenType::Semicolon, ";"),
                test_token(TokenType::Int, "10"),
                test_token(TokenType::NotEqual, "!="),
                test_token(TokenType::Int, "9"),
                test_token(TokenType::Semicolon, ";"),
            ],
        )
    }

    fn test_token(token_type: TokenType, token_val: &str) -> TestToken {
        TestToken {
            token_type,
            token_val: token_val.to_string(),
        }
    }

    fn lexer_test(input: String, expected_tokens: Vec<TestToken>) {
        let mut lexer: Lexer = Lexer::new(input.clone());
        for expected_token in expected_tokens.iter() {
            let actual_token = lexer.next_token();
            assert_eq!(expected_token.token_type, actual_token.token_type);
            assert_eq!(expected_token.token_val, actual_token.literal);
            println!(
                "Actual token: {:?}, Value: {}",
                actual_token.token_type, actual_token.literal
            );
        }

        println!("===================== CHECK EOF ===================");
        let eof_token = lexer.next_token();
        assert_eq!(eof_token.token_type, TokenType::EndOfFile);
        println!("================ LEXER TEST FINISHED ==============")
    }
}
