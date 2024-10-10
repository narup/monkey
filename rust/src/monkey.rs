use derive_more::Display;
use lazy_static::lazy_static;
use std::collections::HashMap;

const PRECEDENCE_LOWEST: i32 = 0; // lowest
const PRECEDENCE_EQUALS: i32 = 1; // ==
const PRECEDENCE_LESS_GREATER: i32 = 2; // > or <
const PRECEDENCE_SUM: i32 = 3; // +
const PRECEDENCE_PRODUCT: i32 = 4; // *
const PRECEDENCE_PREFIX: i32 = 5; // -X or !X
const PRECEDENCE_CALL: i32 = 6; // my_function(X)

pub trait Node {
    //prints the node name
    fn name(&self) -> String;
    fn to_string(&self) -> String;
}

pub struct Program {
    pub statements: Vec<Statement>,
}

pub enum Statement {
    Let {
        token: Token,
        identifier: Box<Expression>,
        value: Box<Expression>,
    },
    Return {
        token: Token,
        value: Box<Expression>,
    },
    Expr {
        value: Box<Expression>,
    },
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Expression {
    Identifier {
        token: Token,
        name: String,
    },
    IntegerLiteral {
        token: Token,
        value: usize,
    },
    Boolean {
        token: Token,
        value: bool,
    },
    Prefix {
        prefix_token: Token,
        right: Box<Expression>,
    },
    Infix {
        operator_token: Token,
        left: Box<Expression>,
        right: Box<Expression>,
    },
}

#[derive(Debug, Display)]
pub enum ParserError {
    SyntaxError(String),
}

pub struct Parser {
    lexer: Lexer,
    current_token: Token,
    peek_token: Token,
    debug_mode: bool,
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
            Statement::Expr { value } => value.to_string(),
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

            Statement::Expr { value } => value.to_string().clone(),
        }
    }
}

impl Node for Expression {
    fn name(&self) -> String {
        match self {
            Expression::Identifier { token: _, name: _ } => "identifier".to_string(),
            Expression::IntegerLiteral { token: _, value: _ } => "integer_literal".to_string(),
            Expression::Boolean { token: _, value: _ } => "boolean".to_string(),
            Expression::Prefix {
                prefix_token: _,
                right: _,
            } => "prefix".to_string(),
            Expression::Infix {
                operator_token: _,
                left: _,
                right: _,
            } => "infix".to_string(),
        }
    }

    fn to_string(&self) -> String {
        match self {
            Expression::Identifier { token: _, name } => name.clone(),
            Expression::IntegerLiteral { token: _, value } => format!("{}", value),
            Expression::Boolean { token: _, value } => format!("{}", value),
            Expression::Prefix {
                prefix_token,
                right,
            } => format!("({}{})", prefix_token.literal.clone(), right.to_string()),
            Expression::Infix {
                operator_token,
                left,
                right,
            } => format!(
                "({} {} {})",
                left.to_string(),
                operator_token.literal.clone(),
                right.to_string()
            ),
        }
    }
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
            debug_mode: true,
        };

        p.current_token = p.lexer.next_token();
        p.peek_token = p.lexer.next_token();

        if p.debug_mode {
            println!("Token:{:?}", p.current_token.token_type);
        }

        p //return parser
    }

    /// Returns the parse of this [`Parser`].
    pub fn parse(&mut self) -> Result<Program, ParserError> {
        //program with statements
        let mut program = Program {
            statements: Vec::new(),
        };

        while self.is_valid_token() {
            match self.current_token.token_type {
                TokenType::Let => program.statements.push(self.parse_let_statement()?),
                TokenType::Return => program.statements.push(self.parse_return_statement()?),
                _ => {
                    let expr_stmt = self.parse_expression_statement()?;
                    program.statements.push(expr_stmt);

                    //semicolon is optional in expression statement
                    if self.matches_peek_token(TokenType::Semicolon) {
                        self.advance_tokens();
                    }
                }
            }
            self.advance_tokens();
        }

        Ok(program)
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

        let expression_result = self.parse_expression(PRECEDENCE_LOWEST);
        match expression_result {
            Ok(expression) => {
                if !self.matches_peek_token(TokenType::Semicolon) {
                    return Err(ParserError::SyntaxError(
                        "missing semicolon on let statement".to_string(),
                    ));
                }

                //skip semicolon
                self.advance_tokens();

                Ok(Statement::Let {
                    token: let_token,
                    identifier,
                    value: expression,
                })
            }
            Err(err) => Err(err),
        }
    }

    fn parse_return_statement(&mut self) -> Result<Statement, ParserError> {
        let ret_token = Token::new(TokenType::Return, self.current_token.literal.clone());
        self.advance_tokens();

        let expression_result = self.parse_expression(PRECEDENCE_LOWEST);
        match expression_result {
            Ok(expression) => {
                if !self.matches_peek_token(TokenType::Semicolon) {
                    return Err(ParserError::SyntaxError(
                        "missing semicolon on return statement".to_string(),
                    ));
                }

                //skip semicolon
                self.advance_tokens();
                Ok(Statement::Return {
                    token: ret_token,
                    value: expression,
                })
            }
            Err(err) => Err(err),
        }
    }

    //in monkey expressions is a statement as well
    fn parse_expression_statement(&mut self) -> Result<Statement, ParserError> {
        let expression = self.parse_expression(PRECEDENCE_LOWEST)?;
        Ok(Statement::Expr { value: expression })
    }

    fn parse_expression(&mut self, precedence: i32) -> Result<Box<Expression>, ParserError> {
        let left_expression_result: Option<Box<Expression>> = match self.current_token.token_type {
            TokenType::Identifier => Some(self.parse_identifier()?),
            TokenType::Int => Some(self.parse_integer_literal()?),
            TokenType::True | TokenType::False => Some(self.parse_boolean()?),
            TokenType::Bang | TokenType::Minus => Some(self.parse_prefix_expression()?),
            _ => None,
        };

        let mut final_expression: Box<Expression>;
        match left_expression_result {
            Some(left_expression) => {
                final_expression = left_expression;

                while !self.matches_peek_token(TokenType::Semicolon)
                    && precedence < self.peek_precedence()
                {
                    //if we find a higher precedence expression process it
                    match self.peek_token.token_type {
                        TokenType::Plus
                        | TokenType::Minus
                        | TokenType::Slash
                        | TokenType::Asterisk
                        | TokenType::Equal
                        | TokenType::NotEqual
                        | TokenType::LessThan
                        | TokenType::GreaterThan => {
                            self.advance_tokens();

                            let expression = self.parse_infix_expression(final_expression)?;
                            final_expression = expression;
                        }
                        _ => return Ok(final_expression), //no expression left
                    }
                }
                Ok(final_expression)
            }
            None => Err(ParserError::SyntaxError(format!(
                "invalid expression, unable to parse `{}` token",
                self.current_token.literal.clone()
            ))),
        }
    }

    fn parse_infix_expression(
        &mut self,
        left: Box<Expression>,
    ) -> Result<Box<Expression>, ParserError> {
        let operator_token = Token::new(
            self.current_token.token_type,
            self.current_token.literal.clone(),
        );

        let precedence = self.current_precedence();
        self.advance_tokens();

        let right = self.parse_expression(precedence)?;

        Ok(Box::new(Expression::Infix {
            operator_token,
            left,
            right,
        }))
    }

    fn parse_identifier(&mut self) -> Result<Box<Expression>, ParserError> {
        let ident_val = Box::new(Expression::Identifier {
            token: Token {
                token_type: TokenType::Identifier,
                literal: self.current_token.literal.clone(),
            },
            name: self.current_token.literal.clone(),
        });

        Ok(ident_val)
    }

    fn parse_boolean(&mut self) -> Result<Box<Expression>, ParserError> {
        let boolean_val = Box::new(Expression::Boolean {
            token: Token {
                token_type: self.current_token.token_type,
                literal: self.current_token.literal.clone(),
            },
            value: (self.current_token.token_type == TokenType::True),
        });
        Ok(boolean_val)
    }

    fn parse_integer_literal(&mut self) -> Result<Box<Expression>, ParserError> {
        let token = Token::new(TokenType::Int, self.current_token.literal.clone());
        let usize_value = self.current_token.literal.parse::<usize>().unwrap();
        Ok(Box::new(Expression::IntegerLiteral {
            token,
            value: usize_value,
        }))
    }

    fn parse_prefix_expression(&mut self) -> Result<Box<Expression>, ParserError> {
        let prefix_token = Token::new(
            self.current_token.token_type,
            self.current_token.literal.clone(),
        );

        self.advance_tokens();

        let expression_result = self.parse_expression(PRECEDENCE_PREFIX);
        match expression_result {
            Ok(expression) => {
                let prefix_expr = Box::new(Expression::Prefix {
                    prefix_token,
                    right: expression,
                });
                Ok(prefix_expr)
            }
            Err(err) => Err(err),
        }
    }

    fn parse_grouped_expression(&mut self) -> Result<Box<Expression>, ParserError> {
        todo!()
    }

    fn matches_peek_token(&self, token_type: TokenType) -> bool {
        self.peek_token.token_type == token_type
    }

    fn advance_tokens(&mut self) {
        //copy value in peek_token as current token
        self.current_token =
            Token::new(self.peek_token.token_type, self.peek_token.literal.clone());
        self.peek_token = self.lexer.next_token();

        if self.debug_mode {
            println!("Token:{:?}", self.current_token.token_type);
        }
    }

    fn is_valid_token(&mut self) -> bool {
        self.current_token.token_type != TokenType::EndOfFile
    }

    fn peek_precedence(&mut self) -> i32 {
        match PRECEDENCES.get(&self.peek_token.token_type) {
            Some(p) => *p,
            _ => PRECEDENCE_LOWEST,
        }
    }

    fn current_precedence(&mut self) -> i32 {
        match PRECEDENCES.get(&self.current_token.token_type) {
            Some(p) => *p,
            _ => PRECEDENCE_LOWEST,
        }
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

        self.input[start_index..self.next_index].to_string()
    }

    fn read_identifier(&mut self) -> String {
        let start_index = self.current_index;

        let mut next_ch = self.peek_char();
        while is_letter(next_ch) {
            self.read_char();
            next_ch = self.peek_char();
        }

        self.input[start_index..self.next_index].to_string()
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
        self.next_index += 1
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
    ch.is_ascii_alphabetic() || ch == '_'
}

fn is_digit(ch: char) -> bool {
    ch.is_ascii_digit()
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
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
    static ref PRECEDENCES: HashMap<TokenType, i32> = {
        let mut map = HashMap::new();
        map.insert(TokenType::Equal, PRECEDENCE_EQUALS);
        map.insert(TokenType::NotEqual, PRECEDENCE_EQUALS);
        map.insert(TokenType::LessThan, PRECEDENCE_LESS_GREATER);
        map.insert(TokenType::GreaterThan, PRECEDENCE_LESS_GREATER);
        map.insert(TokenType::Plus, PRECEDENCE_SUM);
        map.insert(TokenType::Minus, PRECEDENCE_SUM);
        map.insert(TokenType::Slash, PRECEDENCE_PRODUCT);
        map.insert(TokenType::Asterisk, PRECEDENCE_PRODUCT);

        map
    };
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
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
    use std::i32;

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
    fn test_parse_identifier_expr() {
        let input = r#"
           foobar;
            a;
            my_var;
            "#;

        let output: Vec<&str> = vec!["foobar", "a", "my_var"];

        let lexer = Lexer::new(input.to_string());
        let mut parser = Parser::new(lexer);

        let program = parser.parse().expect("parsing failed");
        for (index, stmt) in program.statements.iter().enumerate() {
            if let Statement::Expr { value } = stmt {
                assert_identifier(*value.to_owned(), output.get(index).unwrap());
            } else {
                panic!("invalid token type on expr. expected identifier type");
            }
        }
        println!("****identifier assertion passed****");
    }

    #[test]
    fn test_parse_boolean() {
        let input = r#"
            true;
            false;
            "#;

        let output: Vec<bool> = vec![true, false];
        let lexer = Lexer::new(input.to_string());
        let mut parser = Parser::new(lexer);

        let program = parser.parse().expect("parsing failed");
        for (index, stmt) in program.statements.iter().enumerate() {
            if let Statement::Expr { value } = stmt {
                assert_boolean(*value.to_owned(), *output.get(index).unwrap())
            } else {
                panic!("invalid expression type");
            }
        }

        println!("****boolean assertion passed****");
    }

    #[test]
    fn test_parse_integer_literal() {
        let input = r#"
            10;
            4;
            100;
            "#;

        let output: Vec<i32> = vec![10, 4, 100];

        let lexer = Lexer::new(input.to_string());
        let mut parser = Parser::new(lexer);

        let program = parser.parse().expect("parsing failed");
        for (index, stmt) in program.statements.iter().enumerate() {
            if let Statement::Expr { value } = stmt {
                assert_integer_literal(*value.to_owned(), *output.get(index).unwrap());
            } else {
                panic!("invalid expression type");
            }
        }
        println!("****integer literal assertion passed****");
    }

    #[test]
    fn test_parse_prefix_expressions() {
        let input = r#"
            !5;
            -10;
            -bar;
            "#;

        let output = [("!", "5"), ("-", "10"), ("-", "bar")];

        let mut parser = parser_for_input(input.to_string());
        let program = parser.parse().expect("parsing failed");
        for (index, stmt) in program.statements.iter().enumerate() {
            if let Statement::Expr { value } = stmt {
                let (prefix, val) = &output[index];
                assert_prefix_expression(*value.to_owned(), prefix, val);
            } else {
                panic!("parsing failed, incorrect expression type found");
            }
        }
        println!("****prefix expression assertion passed****");
    }

    #[test]
    fn test_parse_infix_expressions() {
        let input = r#"
                5 + 5;
                5 - 5;
                5 * 5;
                5 / 5;
                5 > 5;
                5 < 5;
                5 == 5;
                5 != 5;
            "#;

        let output = vec![
            ("5", "+", "5"),
            ("5", "-", "5"),
            ("5", "*", "5"),
            ("5", "/", "5"),
            ("5", ">", "5"),
            ("5", "<", "5"),
            ("5", "==", "5"),
            ("5", "!=", "5"),
        ];

        let mut parser = parser_for_input(input.to_string());
        let program = parser
            .parse()
            .expect("parsing infix expressions program failed");
        for (index, stmt) in program.statements.iter().enumerate() {
            if let Statement::Expr { value } = stmt {
                let (left, operator, right) = &output[index];
                assert_infix_expression(*value.to_owned(), operator, left, right)
            }
        }

        println!("****infix expression assertion passed****");
    }

    #[test]
    fn test_parse_precedence_expressions() {
        //test cases - vector of tuples with input and expected output
        let test_cases = vec![
            ("true", "true"),
            ("false", "false"),
            ("3 > 5 == false", "((3 > 5) == false)"),
            ("3 < 5 == true", "((3 < 5) == true)"),
            ("-a + b", "((-a) + b)"),
            ("-a * b", "((-a) * b)"),
            ("a + b + c", "((a + b) + c)"),
            ("a + b - c", "((a + b) - c)"),
            ("a * b * c", "((a * b) * c)"),
            ("a * b / c", "((a * b) / c)"),
            ("a + b * c + d / e - f", "(((a + (b * c)) + (d / e)) - f)"),
            ("3 + 4 * -5 * 5", "(3 + ((4 * (-5)) * 5))"),
            ("5 > 4 == 3 < 4", "((5 > 4) == (3 < 4))"),
            ("5 < 4 != 3 > 4", "((5 < 4) != (3 > 4))"),
            (
                "3 + 4 * 5 == 3 * 1 + 4 * 5",
                "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))",
            ),
        ];

        for (input, output) in test_cases {
            let lexer = Lexer::new(input.to_string());
            let mut parser = Parser::new(lexer);
            let program = parser
                .parse()
                .expect("parsing program with various statements failed");

            for stmt in program.statements.iter() {
                if let Statement::Expr { value } = stmt {
                    println!(
                        "Output of precedence expression input: {} is: {}",
                        input,
                        value.to_string()
                    );

                    assert!(value.to_string() == output);
                }
            }
        }

        println!("**** precedence parser tests passed****");
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
        5 + 2;
        4 + y;
        let x = a + b * c;
        return x + y;
        "#;

        let lexer = Lexer::new(input.to_string());
        let mut parser = Parser::new(lexer);
        let program = parser
            .parse()
            .expect("parsing program with various statements failed");

        for stmt in program.statements.iter() {
            match stmt {
                Statement::Let {
                    token,
                    identifier,
                    value,
                } => {
                    println!(
                        "{} {} = {}",
                        token.literal,
                        identifier.to_string(),
                        value.to_string()
                    );
                }
                Statement::Return { token, value } => {
                    println!("{} {}", token.literal, value.to_string());
                }
                Statement::Expr { value } => {
                    println!("{}", value.to_string());
                }
            }
        }
    }

    fn assert_infix_expression(
        expr: Expression,
        operator_token_val: &str,
        left_exp_val: &str,
        right_exp_val: &str,
    ) {
        if let Expression::Infix {
            operator_token,
            left,
            right,
        } = expr.to_owned()
        {
            assert!(operator_token.literal == operator_token_val);
            assert!(right.to_string() == right_exp_val);
            assert!(left.to_string() == left_exp_val);
            assert!(expr.to_owned().name() == "infix");
        } else {
            panic!("Invalid expression type. Expected infix expression type");
        }
    }

    fn assert_prefix_expression(expr: Expression, prefix_token_val: &str, right_exp_val: &str) {
        if let Expression::Prefix {
            prefix_token,
            right,
        } = expr.to_owned()
        {
            assert!(prefix_token.literal == prefix_token_val);
            assert!(right.to_string() == right_exp_val);
            assert!(expr.to_owned().name() == "prefix");
        } else {
            panic!("Invalid expression type. Expected prefix expression type");
        }
    }

    fn assert_integer_literal(expr: Expression, expected_val: i32) {
        if let Expression::IntegerLiteral { token, value } = expr.to_owned() {
            assert!(token.token_type == TokenType::Int);
            assert!(value as i32 == expected_val);
            assert!(token.literal == format!("{}", expected_val));
            assert!(expr.to_owned().name() == "integer_literal");
        } else {
            panic!("Invalid expression type. Expected Identifier");
        }
    }

    fn assert_boolean(expr: Expression, expected_val: bool) {
        if let Expression::Boolean { token, value } = expr.to_owned() {
            assert!(token.token_type == TokenType::True || token.token_type == TokenType::False);
            assert!(value == expected_val);
            assert!(token.literal == format!("{}", expected_val).as_str());
            assert!(expr.to_owned().name() == "boolean");
        } else {
            panic!("Invalid expression type. Expected Identifier");
        }
    }

    fn assert_identifier(expr: Expression, expected_val: &str) {
        if let Expression::Identifier { token, name } = expr.to_owned() {
            assert!(token.token_type == TokenType::Identifier);
            assert!(token.literal == expected_val);
            assert!(name == expected_val);
            assert!(expr.to_owned().name() == "identifier");
        } else {
            panic!("Invalid expression type. Expected Identifier");
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
