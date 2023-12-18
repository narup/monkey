use std::io::{self, Write};

use crate::monkey::TokenType;

mod monkey;

fn main() {
    println!("Welcome to Monkey Language!");
    println!("Type Ctrl+C or :quit to exit the shell");

    loop {
        print!("mky> ");

        io::stdout().flush().unwrap();

        let mut input = String::new();
        io::stdin()
            .read_line(&mut input)
            .expect("Error reading the input");

        if input.len() == 0 || input.len() == 1 {
            continue;
        }
        if input.trim() == ":quit" {
            println!("Exiting monkey shell! Bye!");
            break;
        }

        let mut lexer: monkey::Lexer = monkey::Lexer::new(input.clone());
        loop {
            let token: monkey::Token = lexer.next_token();
            if token.token_type == TokenType::EndOfFile {
                println!("EndOfFile");
                break;
            }

            println!(
                "Token(type: {:?}, value: {})",
                token.token_type, token.literal
            );
        }

        let mut parser = monkey::Parser::new(lexer);
        let program: monkey::Program = parser.parse();

        for stmt in program.statements.iter() {
            println!("Statement literal:{}", stmt.to_string());
        }
    }
}
