use std::io::{self, Write};

mod monkey;

fn main() {
    println!("Welcome to Monkey Language!");
    println!("Type Ctrl+C or :quit to exit the shell");

    loop {
        print!("mky>");

        io::stdout().flush().unwrap();

        let mut input = String::new();
        io::stdin()
            .read_line(&mut input)
            .expect("Error reading the input");

        if input.len() == 0 || input.len() == 1 {
            continue;
        }

        let mut lexer: monkey::Lexer = monkey::Lexer::new(input.clone());
        let token: monkey::Token = lexer.next_token();

        println!("Token type:{:?}", token.token_type);
        println!("Token val:{}", token.literal);

        println!("Output: {:?}", input.as_str());
        println!("Input: {}", lexer.remaining_input());

        if input.trim() == ":quit" {
            println!("Exiting monkey shell! Bye!");
            break;
        }
    }
}
