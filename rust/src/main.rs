use std::io::{self, Write};

use crate::monkey::{Statement, TokenType};

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
                                "{} {} = {}",
                                token.literal,
                                identifier.to_value(),
                                value.to_value()
                            )
                        }

                        Statement::Return { token, value } => {
                            println!("{} {}", token.literal, value.to_value())
                        }
                    }
                }
            }
            Err(err) => println!("Parser error:{}", err),
        }
    }
}
