mod codegen;
mod parse;
mod tokenize;

use codegen::codegen;
use parse::{Node, NodeKind, Parser};
use std::env;
use tokenize::tokenize;

fn main() {
    let s = env::args().nth(1).unwrap().chars().collect::<Vec<char>>();
    let tokens = tokenize(&s);
    dbg!(&tokens);
    let program = Parser::new(tokens.into_iter().peekable()).program();
    dbg!(&program);
    codegen(program);
}
