use derive_more::Display;
use lazy_static::lazy_static;
use std::collections::HashMap;

pub trait Node {
    //prints the node name
    fn name(&self) -> String;
    fn to_string(&self) -> String;
}

pub enum Statement {
    Let {
        token: Token,
        identifier: Box<dyn Expression>,
        value: Box<dyn Expression>,
    },
    Return {
        token: Token,
        value: Box<dyn Expression>,
    },
    Expr {
        expr_type: ExprType,
    },
}

pub enum ExprType {
    Identifier { value: Box<Identifier> },
    IntegerLiteral { value: Box<IntegerLiteral> },
    PrefixExpression { value: Box<PrefixExpression> },
    MonkeyExpression { value: Box<dyn Expression> },
}

impl Node for ExprType {
    fn name(&self) -> String {
        "expr_type".to_string()
    }

    fn to_string(&self) -> String {
        match self {
            ExprType::Identifier { value } => value.to_string(),
            ExprType::IntegerLiteral { value } => value.to_string(),
            ExprType::PrefixExpression { value } => value.to_string(),
            ExprType::MonkeyExpression { value } => value.to_string(),
        }
    }
}

pub trait Expression: Node {
    fn expression_type(&self) -> String;
}

pub struct Identifier {
    value: String,
}

pub struct IntegerLiteral {
    token: Token,
    value: usize,
}

pub struct PrefixExpression {
    prefix_token: Token,
    operator: String,
    right: Box<dyn Expression>,
}

//In Monkey identifier can be an expression
impl Expression for Identifier {
    fn expression_type(&self) -> String {
        "identifier".to_string()
    }
}

impl Node for Identifier {
    fn name(&self) -> String {
        "identifier".to_string()
    }

    fn to_string(&self) -> String {
        self.value.clone()
    }
}

impl Expression for IntegerLiteral {
    fn expression_type(&self) -> String {
        return "integer".to_string();
    }
}

impl Node for IntegerLiteral {
    fn name(&self) -> String {
        return self.token.literal.clone();
    }

    fn to_string(&self) -> String {
        format!("{}", self.value)
    }
}

impl Node for PrefixExpression {
    fn name(&self) -> String {
        self.prefix_token.literal.to_string()
    }

    fn to_string(&self) -> String {
        format!("({}{})", self.operator, self.right.to_string())
    }
}

impl Expression for PrefixExpression {
    fn expression_type(&self) -> String {
        "prefix".to_string()
    }
}

impl Node for Statement {
    fn name(&self) -> String {
        match self {
            Statement::Let {
                token: _,
                identifier: _,
                value: _,
            } => "let".to_string(),
            Statement::Return { token: _, value: _ } => "return".to_string(),
            Statement::Expr { expr_type: _ } => "expr".to_string(),
        }
    }

    fn to_string(&self) -> String {
        match self {
            Statement::Let {
                token,
                identifier,
                value,
            } => {
                format!(
                    "{} {} = {};",
                    token.literal,
                    identifier.to_string(),
                    value.to_string()
                )
            }

            Statement::Return { token, value } => {
                format!("{} {};", token.literal, value.to_string())
            }

            Statement::Expr { expr_type } => {
                format!("{}", expr_type.to_string())
            }
        }
    }
}

struct MonkeyExpression {
    value: String,
}

impl Node for MonkeyExpression {
    fn name(&self) -> String {
        "expression".to_string()
    }

    fn to_string(&self) -> String {
        self.value.clone()
    }
}

impl Expression for MonkeyExpression {
    fn expression_type(&self) -> String {
        "monkey_expression".to_string()
    }
}

pub struct Program {
    pub statements: Vec<Statement>,
}

#[derive(Debug, Display)]
pub enum ParserError {
    SyntaxError(String),
}

pub struct Parser {
    lexer: Lexer,
    current_token: Token,
    peek_token: Token,
}

impl Parser {
    pub fn new(l: Lexer) -> Parser {
        let mut p = Self {
            lexer: l,
            current_token: Token {
                token_type: TokenType::Illegal,
                literal: "Illegal".to_string(),
            },
            peek_token: Token {
                token_type: TokenType::Illegal,
                literal: "Illegal".to_string(),
            },
        };

        p.current_token = p.lexer.next_token();
        p.peek_token = p.lexer.next_token();

        return p;
    }

    /// Returns the parse of this [`Parser`].
    pub fn parse(&mut self) -> Result<Program, ParserError> {
        let mut program = Program {
            statements: Vec::new(),
        };

        while self.is_valid_token() {
            if self.matches_current_token(TokenType::Let) {
                let let_stmt = self.parse_let_statement()?; //handle let statement
                program.statements.push(let_stmt);
            } else if self.matches_current_token(TokenType::Return) {
                let ret_stmt = self.parse_return_statement()?;
                program.statements.push(ret_stmt);
            } else {
                let expr_stmt = self.parse_expression_statement()?;
                program.statements.push(expr_stmt);

                //semicolon is optional in expression statement
                if self.matches_peek_token(TokenType::Semicolon) {
                    self.advance_tokens();
                }
            }

            self.advance_tokens();
        }
        return Ok(program);
    }

    fn parse_let_statement(&mut self) -> Result<Statement, ParserError> {
        let let_token = Token::new(TokenType::Let, self.current_token.literal.clone());

        if !self.matches_peek_token(TokenType::Identifier) {
            return Err(ParserError::SyntaxError(
                "invalid let statement, missing identifier after let".to_owned(),
            ));
        }
        self.advance_tokens();

        let identifier = self.parse_identifier()?;
        if !self.matches_peek_token(TokenType::Assign) {
            return Err(ParserError::SyntaxError(
                "invalid let statement, missing assign ('=') after identifier".to_string(),
            ));
        }
        //read assign token
        self.advance_tokens();
        //skip assign token
        self.advance_tokens();

        let value = self.parse_expression()?;
        if !self.matches_peek_token(TokenType::Semicolon) {
            return Err(ParserError::SyntaxError("missing semicolon".to_string()));
        }

        //skip semicolon
        self.advance_tokens();

        Ok(Statement::Let {
            token: let_token,
            identifier,
            value,
        })
    }

    fn parse_return_statement(&mut self) -> Result<Statement, ParserError> {
        let ret_token = Token::new(TokenType::Return, self.current_token.literal.clone());
        self.advance_tokens();

        let value = self.parse_expression()?;
        if !self.matches_peek_token(TokenType::Semicolon) {
            return Err(ParserError::SyntaxError(
                "missing semicolon on return statement".to_string(),
            ));
        }

        //skip Semicolon
        self.advance_tokens();

        Ok(Statement::Return {
            token: ret_token,
            value,
        })
    }

    fn parse_expression_statement(&mut self) -> Result<Statement, ParserError> {
        if self.matches_current_token(TokenType::Identifier) {
            let indent_val = self.parse_identifier()?;
            return Ok(Statement::Expr {
                expr_type: ExprType::Identifier { value: indent_val },
            });
        }

        if self.matches_current_token(TokenType::Int) {
            let int_literal_val = self.parse_integer_literal()?;
            return Ok(Statement::Expr {
                expr_type: ExprType::IntegerLiteral {
                    value: int_literal_val,
                },
            });
        }

        if self.matches_current_token(TokenType::Bang)
            || self.matches_current_token(TokenType::Minus)
        {
            let prefix_expr = self.parse_prefix_expression()?;
            return Ok(Statement::Expr {
                expr_type: ExprType::PrefixExpression { value: prefix_expr },
            });
        }

        let expr_val = self.parse_expression()?;
        return Ok(Statement::Expr {
            expr_type: ExprType::MonkeyExpression { value: expr_val },
        });
    }

    fn parse_identifier(&mut self) -> Result<Box<Identifier>, ParserError> {
        return Ok(Box::new(Identifier {
            value: self.current_token.literal.clone(),
        }));
    }

    fn parse_integer_literal(&mut self) -> Result<Box<IntegerLiteral>, ParserError> {
        let token = Token::new(TokenType::Int, self.current_token.literal.clone());
        let usize_value = self.current_token.literal.parse::<usize>().unwrap();
        return Ok(Box::new(IntegerLiteral {
            token,
            value: usize_value,
        }));
    }

    fn parse_prefix_expression(&mut self) -> Result<Box<PrefixExpression>, ParserError> {
        let prefix_token = Token::new(
            self.current_token.token_type,
            self.current_token.literal.clone(),
        );
        let operator = prefix_token.literal.clone();

        self.advance_tokens();

        let right = self.parse_expression()?;
        return Ok(Box::new(PrefixExpression {
            prefix_token,
            operator,
            right,
        }));
    }

    fn parse_expression(&mut self) -> Result<Box<dyn Expression>, ParserError> {
        Ok(Box::new(MonkeyExpression {
            value: self.current_token.literal.clone(),
        }))
    }

    fn matches_peek_token(&self, token_type: TokenType) -> bool {
        return self.peek_token.token_type == token_type;
    }

    fn matches_current_token(&self, token_type: TokenType) -> bool {
        return self.current_token.token_type == token_type;
    }

    fn advance_tokens(&mut self) {
        //copy value in peek_token as current token
        self.current_token =
            Token::new(self.peek_token.token_type, self.peek_token.literal.clone());
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
    use core::panic;

    use super::*;

    struct TestToken {
        token_type: TokenType,
        token_val: String,
    }

    #[test]
    fn test_works() {
        assert_eq!("monkey", mod_name());
    }

    fn parser_for_input(input: String) -> Parser {
        let lexer = Lexer::new(input);
        Parser::new(lexer)
    }

    #[test]
    fn test_prefix_expressions() {
        let input = r#"
        !5;
        -10;
        "#;

        let mut parser = parser_for_input(input.to_string());
        match parser.parse() {
            Ok(program) => {
                for stmt in program.statements.iter() {
                    match stmt {
                        Statement::Expr { expr_type } => match expr_type {
                            ExprType::PrefixExpression { value } => {
                                assert_eq!(value.expression_type(), "prefix");
                                if value.operator != "-" && value.operator != "!" {
                                    println!("actual operator {}", value.operator);
                                    panic!("invalid operator, expected bang or minus");
                                }
                                println!("expression value:{}", value.right.to_string());
                            }
                            _ => panic!("invalid expression type, expected prefix expression"),
                        },
                        _ => panic!("invalid statement, expected expression"),
                    }
                }
            }
            Err(err) => panic!("test failed {:?}", err),
        }
    }

    #[test]
    fn test_parser_int_literal() {
        let input = "10";
        let lexer = Lexer::new(input.to_string());
        let mut parser = Parser::new(lexer);

        match parser.parse() {
            Ok(program) => {
                for stmt in program.statements.iter() {
                    match stmt {
                        Statement::Expr { expr_type } => match expr_type {
                            ExprType::IntegerLiteral { value } => {
                                assert_eq!(value.expression_type(), "integer");
                                assert_eq!(value.value, 10);
                            }
                            _ => panic!("invalid expression type, expected integer literal"),
                        },
                        _ => panic!("invalid token type on expr"),
                    };
                }
            }
            Err(err) => panic!("parsing let statement failed {:?}", err),
        }
    }

    #[test]
    fn test_parser_identifier_expr() {
        let input = "foobar";
        let lexer = Lexer::new(input.to_string());
        let mut parser = Parser::new(lexer);

        match parser.parse() {
            Ok(program) => {
                for stmt in program.statements.iter() {
                    match stmt {
                        Statement::Expr { expr_type } => match expr_type {
                            ExprType::Identifier { value } => {
                                assert_eq!(value.expression_type(), "identifier");
                                assert_eq!(value.to_string(), input);
                            }
                            _ => panic!("invalid expression type, expected identifier"),
                        },
                        _ => panic!("invalid token type on expr. expected identifier type"),
                    };
                }
            }
            Err(err) => panic!("parsing identifier expression failed {:?}", err),
        }
    }

    #[test]
    fn test_parser() {
        let input = r#"
        let x = 5;
        let y = 10;
        let foobar = 838383;
        return 30;
        return y;
        foobar;
        10;
        "#;

        let lexer = Lexer::new(input.to_string());
        let mut parser = Parser::new(lexer);

        match parser.parse() {
            Ok(program) => {
                for stmt in program.statements.iter() {
                    match stmt {
                        Statement::Let {
                            token,
                            identifier,
                            value,
                        } => {
                            println!(
                                "Let statement parsed = {:?}, {:?}, {:?}!",
                                token.literal,
                                identifier.to_string(),
                                value.to_string()
                            );
                        }

                        Statement::Return { token, value } => {
                            println!(
                                "Return statement parsed = {:?}, {:?}",
                                token.literal,
                                value.to_string()
                            );
                        }

                        Statement::Expr { expr_type } => {
                            println!("Expr statement = {:}", expr_type.to_string());
                        }
                    }
                }
            }

            Err(err) => panic!("parsing let statement failed {:?}", err),
        }
    }

    #[test]
    fn test_lexer() {
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
