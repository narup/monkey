use std::io::{self, Write};

use crate::monkey::Node;

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

        let lexer = monkey::Lexer::new(input.clone());
        let mut parser = monkey::Parser::new(lexer);

        match parser.parse() {
            Ok(program) => {
                for stmt in program.statements.iter() {
                    println!("Statement: {}", stmt.to_value());
                }
            }
            Err(err) => println!("Parser error:{}", err),
        }
    }
}
