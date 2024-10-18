use std::io::{self, BufRead};
use std::iter::Peekable;

#[derive(Debug, PartialEq, Eq)]
enum Token {
    Reserved(char),
    Num(u64),
}

type Tokens = Vec<Token>;

fn tokenize(s: &[char]) -> Tokens {
    let mut ret = Vec::new();
    let mut idx = 0;
    while idx < s.len() {
        if s[idx].is_whitespace() {
            idx += 1;
            continue;
        }

        if s[idx] == '+'
            || s[idx] == '-'
            || s[idx] == '*'
            || s[idx] == '/'
            || s[idx] == '('
            || s[idx] == ')'
        {
            ret.push(Token::Reserved(s[idx]));
            idx += 1;
        } else if s[idx].is_ascii_digit() {
            let mut num = 0;
            while idx < s.len() && s[idx].is_ascii_digit() {
                num *= 10;
                num += s[idx].to_digit(10).unwrap();
                idx += 1;
            }
            ret.push(Token::Num(num as u64));
        }
    }
    ret
}

#[derive(Debug)]
enum NodeKind {
    Add,
    Sub,
    Mul,
    Div,
}

#[derive(Debug)]
enum Node {
    Op {
        kind: NodeKind,
        lhs: Box<Node>,
        rhs: Box<Node>,
    },
    Num {
        val: u64,
    },
}

fn expr<T: Iterator<Item = Token>>(iter: &mut Peekable<T>) -> Node {
    let mut node = mul(iter);
    loop {
        if iter.next_if_eq(&Token::Reserved('+')).is_some() {
            node = Node::Op {
                kind: NodeKind::Add,
                lhs: Box::new(node),
                rhs: Box::new(mul(iter)),
            }
        } else if iter.next_if_eq(&Token::Reserved('-')).is_some() {
            node = Node::Op {
                kind: NodeKind::Sub,
                lhs: Box::new(node),
                rhs: Box::new(mul(iter)),
            }
        } else {
            break;
        }
    }
    node
}

fn mul<T: Iterator<Item = Token>>(iter: &mut Peekable<T>) -> Node {
    let mut node = primary(iter);
    loop {
        if iter.next_if_eq(&Token::Reserved('*')).is_some() {
            node = Node::Op {
                kind: NodeKind::Mul,
                lhs: Box::new(node),
                rhs: Box::new(primary(iter)),
            }
        } else if iter.next_if_eq(&Token::Reserved('/')).is_some() {
            node = Node::Op {
                kind: NodeKind::Div,
                lhs: Box::new(node),
                rhs: Box::new(primary(iter)),
            }
        } else {
            break;
        }
    }
    node
}

fn primary<T: Iterator<Item = Token>>(iter: &mut Peekable<T>) -> Node {
    match iter.next().unwrap() {
        Token::Reserved(ty) => {
            if ty == '(' {
                let node = expr(iter);
                if iter.next().unwrap() != Token::Reserved(')') {
                    panic!("");
                }
                node
            } else {
                panic!("")
            }
        }
        Token::Num(num) => Node::Num { val: num },
    }
}

fn calc(node: &Node) -> u64 {
    match node {
        Node::Num { val } => *val,
        Node::Op { kind, lhs, rhs } => {
            let lhs = calc(lhs);
            let rhs = calc(rhs);
            match kind {
                NodeKind::Add => lhs + rhs,
                NodeKind::Sub => lhs - rhs,
                NodeKind::Mul => lhs * rhs,
                NodeKind::Div => lhs / rhs,
            }
        }
    }
}

fn main() {
    let stdin = io::stdin();
    let s: Vec<char> = stdin
        .lock()
        .lines()
        .next()
        .unwrap()
        .unwrap()
        .chars()
        .collect();
    let tokens = tokenize(&s);
    let node = expr(&mut tokens.into_iter().peekable());
    println!("{}", calc(&node));
}
