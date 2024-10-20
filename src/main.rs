mod codegen;
mod parse;
mod tokenize;

use codegen::codegen;
use parse::{Node, NodeKind, Parser};
use std::env;
use tokenize::tokenize;

fn main() {
    let s = env::args().nth(1).unwrap();
    let mut lines = s.lines();
    let s = s.chars().collect::<Vec<char>>();
    let tokens = tokenize(&s);
    //dbg!(&tokens);
    let program = Parser::new(tokens.into_iter().peekable()).program();
    match program {
        Ok(program) => {
            dbg!(&program);
            codegen(program);
        }
        Err(error) => {
            eprintln!("{}:{}", error.pos.1, lines.nth(error.pos.1).unwrap());
            eprintln!("{}^", " ".repeat(error.pos.0 + 1));
            eprintln!("{}: {}", error.ty, error.msg);
        }
    }
}
