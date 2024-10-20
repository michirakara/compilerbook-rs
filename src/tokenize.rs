use std::cmp::min;

trait IsAlnum {
    fn is_ascii_alphanumeric_under(&self) -> bool;
    fn is_ascii_alphabetic_under(&self) -> bool;
}
impl IsAlnum for char {
    fn is_ascii_alphanumeric_under(&self) -> bool {
        self.is_ascii_alphanumeric() || self == &'_'
    }
    fn is_ascii_alphabetic_under(&self) -> bool {
        self.is_ascii_alphabetic() || self == &'_'
    }
}

struct Raw<'a> {
    s: &'a [char],
    idx: usize,
}

impl<'a> Iterator for Raw<'a> {
    type Item = char;

    fn next(&mut self) -> Option<Self::Item> {
        if self.idx < self.s.len() {
            self.idx += 1;
            Some(self.s[self.idx - 1])
        } else {
            None
        }
    }
}
impl<'a> Raw<'a> {
    fn peek(&self) -> Option<char> {
        if self.idx < self.s.len() {
            Some(self.s[self.idx])
        } else {
            None
        }
    }
    fn peek_take(&self, n: usize) -> String {
        self.s[self.idx..min(self.s.len(), self.idx + n)]
            .iter()
            .collect()
    }
    fn take_if(&mut self, n: usize, func: impl FnOnce(&String) -> bool) -> Option<String> {
        let tmp = self.peek_take(n);
        if func(&tmp) {
            self.idx += n;
            Some(tmp)
        } else {
            None
        }
    }
    fn next_if(&mut self, func: impl FnOnce(&char) -> bool) -> Option<char> {
        if let Some(elem) = self.peek() {
            if func(&elem) {
                self.idx += 1;
                Some(self.s[self.idx - 1])
            } else {
                None
            }
        } else {
            None
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum BaseType {
    Int,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Token {
    If,
    Else,
    While,
    For,
    Return,
    BaseType(BaseType),
    Reserved(&'static str),
    Ident(String),
    Num(u64),
}

const RESERVED: [&str; 18] = [
    "<=", ">=", "==", "!=", "<", ">", "+", "-", "*", "/", "(", ")", "=", ";", "{", "}", ",", "&",
];

type Tokens = Vec<(Token, (usize, usize))>;

pub fn tokenize(s: &[char]) -> Tokens {
    let mut x = 0;
    let mut y = 0;
    let mut ret = Vec::new();
    let mut iter = Raw { s, idx: 0 };
    'i: while iter.peek().is_some() {
        if iter.next_if(|&c| c == '\n').is_some() {
            x = 0;
            y += 1;
        }
        if iter.next_if(|&c| c.is_whitespace()).is_some() {
            x += 1;
            continue;
        }
        // Reserved operators
        for op in RESERVED {
            if iter.take_if(op.len(), |s| s.starts_with(op)).is_some() {
                ret.push((Token::Reserved(op), (x, y)));
                x += op.len();
                continue 'i;
            }
        }
        // Reserved words and other identifiers
        if iter.peek().unwrap_or('あ').is_ascii_alphabetic_under() {
            let mut s = String::new();
            while let Some(c) = iter.next_if(|&c| c.is_ascii_alphanumeric_under()) {
                s.push(c);
            }
            let (tx, ty) = (x, y);
            ret.push((
                match s.as_str() {
                    "if" => {
                        x += 2;
                        Token::If
                    }
                    "else" => {
                        x += 4;
                        Token::Else
                    }
                    "while" => {
                        x += 5;
                        Token::While
                    }
                    "for" => {
                        x += 3;
                        Token::For
                    }
                    "return" => {
                        x += 6;
                        Token::Return
                    }
                    "int" => {
                        x += 3;
                        Token::BaseType(BaseType::Int)
                    }
                    _ => {
                        x += s.len();
                        Token::Ident(s)
                    }
                },
                (tx, ty),
            ));
            continue;
        }
        // Numeric literal
        let mut num = 0u64;
        let (tx, ty) = (x, y);
        while let Some(c) = iter.next_if(|&c| c.is_ascii_digit()) {
            x += 1;
            num *= 10;
            num += c.to_digit(10).unwrap() as u64;
        }
        ret.push((Token::Num(num), (tx, ty)));
    }
    ret
}
