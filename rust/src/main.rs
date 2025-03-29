mod monkey;

use crate::monkey::Node;
use std::io::{self, Write};

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

        if input.is_empty() || input.len() == 1 {
            continue;
        }
        if input.trim() == ":quit" {
            println!("Exiting monkey shell! Bye!");
            break;
        }

        let lexer = monkey::Lexer::new(input.clone());
        let mut parser = monkey::Parser::new(lexer);

        match parser.parse() {
            Ok(program) =>{
                for stmt in &program.statements {
                    println!("{}", stmt.to_string());
                }

                println!("Evaluating....");
                let eval_result = program.eval();
                match eval_result {
                    Ok(result) => println!("{}", result.to_string()),
                    Err(err) => println!("Evaluation error:{}", err),
                }
            }
            Err(err) => println!("Parser error:{}", err),
        }
    }
}
    