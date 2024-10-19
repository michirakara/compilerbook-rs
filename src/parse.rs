use crate::tokenize::Token;
use std::iter::Peekable;

#[derive(Debug,Clone)]
pub enum NodeKind {
    Add,
    Sub,
    Mul,
    Div,
    Eq,
    Ne,
    Lt,
    Le,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
    Ptr { to: Box<Type> },
    Int,
}

#[derive(Debug, Clone)]
pub struct LVar {
    pub name: String,
    pub ty: Type,
    pub offset: usize,
}

#[derive(Debug, Clone)]
pub struct Locals {
    locals: Vec<LVar>,
    pub stack_size: usize,
}

#[derive(Debug, Clone)]
pub enum NodeType {
    Block {
        stmts: Vec<Node>,
    },
    If {
        cond: Box<Node>,
        if_do: Box<Node>,
        else_do: Option<Box<Node>>,
    },
    While {
        cond: Box<Node>,
        while_do: Box<Node>,
    },
    For {
        init: Option<Box<Node>>,
        cond: Option<Box<Node>>,
        expr: Option<Box<Node>>,
        stmt: Box<Node>,
    },
    Return {
        stmt: Box<Node>,
    },
    Assign {
        lhs: Box<Node>,
        rhs: Box<Node>,
    },
    Addr {
        val: Box<Node>,
    },
    Deref {
        val: Box<Node>,
    },
    Op {
        kind: NodeKind,
        lhs: Box<Node>,
        rhs: Box<Node>,
    },
    FunCall {
        name: String,
        args: Vec<Node>,
    },
    LVar {
        val: LVar,
    },
    Num {
        val: u64,
    },
    None,
}

#[derive(Debug, Clone)]
pub struct Node {
    pub ty: Option<Type>,
    pub node: NodeType,
}

#[derive(Debug, Clone)]
pub struct Function {
    pub name: String,
    pub args: Vec<LVar>,
    pub ret: Type,
    pub nodes: Vec<Node>,
    pub stack_size: usize,
}

pub struct Parser<T: Iterator<Item = Token> + Clone> {
    pub iter: Peekable<T>,
    locals: Locals,
    funcs: Vec<Function>,
}
impl<T: Iterator<Item = Token> + Clone> Parser<T> {
    pub fn new(iter: Peekable<T>) -> Parser<T> {
        Self {
            iter,
            locals: Locals {
                locals: Vec::new(),
                stack_size: 0,
            },
            funcs: Vec::new(),
        }
    }
}

impl<T: Iterator<Item = Token> + Clone> Parser<T> {
    fn new_lvar(&mut self, s: &str, ty: &Type) -> LVar {
        LVar {
            name: s.to_string(),
            ty: ty.clone(),
            offset: {
                self.locals.stack_size += 8;
                self.locals.locals.push(LVar {
                    name: s.to_string(),
                    ty: ty.clone(),
                    offset: self.locals.stack_size,
                });
                self.locals.locals.last().unwrap().offset
            },
        }
    }
    fn get_lvar(&mut self, s: &str) -> LVar {
        LVar {
            name: s.to_string(),
            ty: self.locals.locals[self
                .locals
                .locals
                .iter()
                .position(|elem| elem.name == s)
                .expect("TODO")]
            .ty
            .clone(),
            offset: self.locals.locals[self
                .locals
                .locals
                .iter()
                .position(|elem| elem.name == s)
                .expect("TODO")]
            .offset,
        }
    }
    fn expect(&mut self, s: &'static str) {
        if Some(Token::Reserved(s)) != self.iter.next() {
            dbg!(s);
            panic!("ParseError");
        }
    }
    pub fn program(&mut self) -> Vec<Function> {
        let mut iter_tmp = self.iter.clone();
        while iter_tmp.peek().is_some() {
            if let Some(Token::BaseType(_)) = iter_tmp.next() {
                let mut ty = Type::Int;
                while iter_tmp.next_if_eq(&Token::Reserved("*")).is_some() {
                    ty = Type::Ptr { to: Box::new(ty) };
                }
                if let Some(Token::Ident(s)) = iter_tmp.next() {
                    let mut func = Function {
                        name: s.to_string(),
                        args: Vec::new(),
                        ret: ty,
                        nodes: Vec::new(),
                        stack_size: 0,
                    };
                    if Some(Token::Reserved("(")) != iter_tmp.next() {
                        panic!("ParseError");
                    }
                    if iter_tmp.next_if_eq(&Token::Reserved(")")).is_none() {
                        if let Some(Token::BaseType(_)) = iter_tmp.next() {
                            let mut ty = Type::Int;
                            while iter_tmp.next_if_eq(&Token::Reserved("*")).is_some() {
                                ty = Type::Ptr { to: Box::new(ty) };
                            }
                            if let Some(Token::Ident(s)) = iter_tmp.next() {
                                func.args.push(self.new_lvar(&s, &ty));
                            } else {
                                panic!("ParseError");
                            }
                        } else {
                            panic!("ParseError");
                        }
                        while iter_tmp.next_if_eq(&Token::Reserved(")")).is_none() {
                            if Some(Token::Reserved(",")) != iter_tmp.next() {
                                panic!("ParseError");
                            }
                            if let Some(Token::BaseType(_)) = iter_tmp.next() {
                                let mut ty = Type::Int;
                                while iter_tmp.next_if_eq(&Token::Reserved("*")).is_some() {
                                    ty = Type::Ptr { to: Box::new(ty) };
                                }
                                if let Some(Token::Ident(s)) = iter_tmp.next() {
                                    func.args.push(self.new_lvar(&s, &ty));
                                } else {
                                    panic!("ParseError");
                                }
                            } else {
                                panic!("ParseError");
                            }
                        }
                    }
                    if Some(Token::Reserved("{")) != iter_tmp.next() {
                        panic!("ParseError");
                    }
                    let mut cnt = 1;
                    while let Some(s) = iter_tmp.next() {
                        if s == Token::Reserved("{") {
                            cnt += 1;
                        }
                        if s == Token::Reserved("}") {
                            cnt -= 1;
                        }
                        if cnt == 0 {
                            break;
                        }
                    }
                    self.funcs.push(func);
                } else {
                    panic!("ParseError");
                }
                self.locals.locals=Vec::new();
                self.locals.stack_size=0;
            } else {
                panic!("ParseError");
            }
        }

        let mut idx = 0;
        while self.iter.peek().is_some() {
            if let Some(Token::BaseType(_)) = self.iter.next() {
                let mut ty = Type::Int;
                while self.iter.next_if_eq(&Token::Reserved("*")).is_some() {
                    ty = Type::Ptr { to: Box::new(ty) };
                }
                if let Some(Token::Ident(s)) = self.iter.next() {
                    let mut func = Function {
                        name: s.to_string(),
                        args: Vec::new(),
                        ret: ty,
                        nodes: Vec::new(),
                        stack_size: 0,
                    };
                    self.expect("(");
                    if self.iter.next_if_eq(&Token::Reserved(")")).is_none() {
                        if let Some(Token::BaseType(_)) = self.iter.next() {
                            let mut ty = Type::Int;
                            while self.iter.next_if_eq(&Token::Reserved("*")).is_some() {
                                ty = Type::Ptr { to: Box::new(ty) };
                            }
                            if let Some(Token::Ident(s)) = self.iter.next() {
                                func.args.push(self.new_lvar(&s, &ty));
                            }
                        }
                        while self.iter.next_if_eq(&Token::Reserved(")")).is_none() {
                            self.expect(",");
                            if let Some(Token::BaseType(_)) = self.iter.next() {
                                let mut ty = Type::Int;
                                while self.iter.next_if_eq(&Token::Reserved("*")).is_some() {
                                    ty = Type::Ptr { to: Box::new(ty) };
                                }
                                if let Some(Token::Ident(s)) = self.iter.next() {
                                    func.args.push(self.new_lvar(&s, &ty));
                                }
                            }
                        }
                    }
                    self.expect("{");
                    while self.iter.next_if_eq(&Token::Reserved("}")).is_none() {
                        func.nodes.push(self.stmt());
                    }
                    func.stack_size = self.locals.stack_size;
                    self.funcs[idx] = func;
                    idx += 1;
                } else {
                    panic!("ParseError");
                }
                self.locals.locals=Vec::new();
                self.locals.stack_size=0;
            } else {
                panic!("ParseError");
            }
        }
        self.funcs.clone()
    }
    fn stmt(&mut self) -> Node {
        if self.iter.next_if_eq(&Token::Reserved("{")).is_some() {
            let mut ret = Vec::new();
            while self.iter.peek() != Some(&Token::Reserved("}")) {
                ret.push(self.stmt());
            }
            self.iter.next();
            Node {
                ty: None,
                node: NodeType::Block { stmts: ret },
            }
        } else if self.iter.next_if_eq(&Token::If).is_some() {
            self.expect("(");
            let node = self.expr();
            self.expect(")");
            Node {
                ty: None,
                node: NodeType::If {
                    cond: Box::new(node),
                    if_do: Box::new(self.stmt()),
                    else_do: if self.iter.next_if_eq(&Token::Else).is_some() {
                        Some(Box::new(self.stmt()))
                    } else {
                        None
                    },
                },
            }
        } else if self.iter.next_if_eq(&Token::While).is_some() {
            self.expect("(");
            let node = self.expr();
            self.expect(")");
            Node {
                ty: None,
                node: NodeType::While {
                    cond: Box::new(node),
                    while_do: Box::new(self.stmt()),
                },
            }
        } else if self.iter.next_if_eq(&Token::For).is_some() {
            self.expect("(");
            Node {
                ty: None,
                node: NodeType::For {
                    init: if self.iter.next_if_eq(&Token::Reserved(";")).is_some() {
                        None
                    } else {
                        let tmp = Some(Box::new(self.expr()));
                        self.expect(";");
                        tmp
                    },
                    cond: if self.iter.next_if_eq(&Token::Reserved(";")).is_some() {
                        None
                    } else {
                        let tmp = Some(Box::new(self.expr()));
                        self.expect(";");
                        tmp
                    },
                    expr: if self.iter.next_if_eq(&Token::Reserved(")")).is_some() {
                        None
                    } else {
                        let tmp = Some(Box::new(self.expr()));
                        self.expect(")");
                        tmp
                    },
                    stmt: Box::new(self.stmt()),
                },
            }
        } else if self.iter.next_if_eq(&Token::Return).is_some() {
            let child_node = self.expr();
            self.expect(";");
            Node {
                ty: child_node.ty.clone(),
                node: NodeType::Return {
                    stmt: Box::new(child_node),
                },
            }
        } else if let Some(&Token::BaseType(_)) = self.iter.peek() {
            self.iter.next();
            let mut ty = Type::Int;
            while self.iter.next_if_eq(&Token::Reserved("*")).is_some() {
                ty = Type::Ptr { to: Box::new(ty) };
            }
            if let Some(Token::Ident(s)) = self.iter.next() {
                let lvar = self.new_lvar(&s, &ty);
                if self.iter.next_if_eq(&Token::Reserved("=")).is_some() {
                    let rhs = self.expr();
                    if rhs.ty.is_some() && rhs.ty != Some(ty.clone()) {
                        panic!("TypeError");
                    }
                    let ret = NodeType::Assign {
                        lhs: Box::new(Node {
                            ty: Some(ty),
                            node: NodeType::LVar { val: lvar },
                        }),
                        rhs: Box::new(rhs),
                    };
                    self.expect(";");
                    Node {
                        ty: None,
                        node: ret,
                    }
                } else {
                    self.expect(";");
                    Node {
                        ty: None,
                        node: NodeType::None,
                    }
                }
            } else {
                panic!("ParseError");
            }
        } else {
            let node = self.expr();
            self.expect(";");
            node
        }
    }
    fn expr(&mut self) -> Node {
        self.assign()
    }
    fn assign(&mut self) -> Node {
        let mut node = self.equality();
        if self.iter.next_if_eq(&Token::Reserved("=")).is_some() {
            let node2 = self.assign();
            if node.ty.is_some() && node2.ty.is_some() && node.ty != node2.ty {
                panic!("TypeError");
            }
            node = Node {
                ty: node2.ty.clone(),
                node: NodeType::Assign {
                    lhs: Box::new(node),
                    rhs: Box::new(node2),
                },
            };
        }
        node
    }

    fn equality(&mut self) -> Node {
        let mut node = self.relational();
        loop {
            if self.iter.next_if_eq(&Token::Reserved("==")).is_some() {
                let node2 = self.relational();
                if node.ty != node2.ty {
                    panic!("TypeError")
                }
                node = Node {
                    ty: Some(Type::Int),
                    node: NodeType::Op {
                        kind: NodeKind::Eq,
                        lhs: Box::new(node),
                        rhs: Box::new(node2),
                    },
                };
            } else if self.iter.next_if_eq(&Token::Reserved("!=")).is_some() {
                let node2 = self.relational();
                if node.ty != node2.ty {
                    panic!("TypeError")
                }
                node = Node {
                    ty: Some(Type::Int),
                    node: NodeType::Op {
                        kind: NodeKind::Ne,
                        lhs: Box::new(node),
                        rhs: Box::new(node2),
                    },
                };
            } else {
                break;
            }
        }
        node
    }
    fn relational(&mut self) -> Node {
        let mut node = self.add();
        loop {
            if self.iter.next_if_eq(&Token::Reserved("<")).is_some() {
                let node2 = self.add();
                if node.ty != node2.ty {
                    panic!("TypeError")
                }
                node = Node {
                    ty: Some(Type::Int),
                    node: NodeType::Op {
                        kind: NodeKind::Lt,
                        lhs: Box::new(node),
                        rhs: Box::new(node2),
                    },
                };
            } else if self.iter.next_if_eq(&Token::Reserved("<=")).is_some() {
                let node2 = self.add();
                if node.ty != node2.ty {
                    panic!("TypeError")
                }
                node = Node {
                    ty: Some(Type::Int),
                    node: NodeType::Op {
                        kind: NodeKind::Le,
                        lhs: Box::new(node),
                        rhs: Box::new(node2),
                    },
                };
            } else if self.iter.next_if_eq(&Token::Reserved(">")).is_some() {
                let node2 = self.add();
                if node.ty != node2.ty {
                    panic!("TypeError")
                }
                node = Node {
                    ty: Some(Type::Int),
                    node: NodeType::Op {
                        kind: NodeKind::Lt,
                        lhs: Box::new(node2),
                        rhs: Box::new(node),
                    },
                };
            } else if self.iter.next_if_eq(&Token::Reserved(">=")).is_some() {
                let node2 = self.add();
                if node.ty != node2.ty {
                    panic!("TypeError")
                }
                node = Node {
                    ty: Some(Type::Int),
                    node: NodeType::Op {
                        kind: NodeKind::Le,
                        lhs: Box::new(node2),
                        rhs: Box::new(node),
                    },
                };
            } else {
                break;
            }
        }
        node
    }
    fn add(&mut self) -> Node {
        let mut node = self.mul();
        loop {
            if self.iter.next_if_eq(&Token::Reserved("+")).is_some() {
                let mut node_r = self.mul();
                if let Some(Type::Ptr { to }) = node_r.ty.clone() {
                    if let Some(Type::Ptr { to: _ }) = node.ty.clone() {
                        panic!("TypeError");
                    }
                    node.ty = Some(Type::Ptr { to: to.clone() });
                    std::mem::swap(&mut node, &mut node_r);
                }
                node = Node {
                    ty: node.ty.clone(),
                    node: NodeType::Op {
                        kind: NodeKind::Add,
                        lhs: Box::new(node),
                        rhs: Box::new(node_r),
                    },
                };
            } else if self.iter.next_if_eq(&Token::Reserved("-")).is_some() {
                let node_r = self.mul();
                if let Some(Type::Ptr { to }) = node_r.ty.clone() {
                    if let Some(Type::Ptr { to: _ }) = node.ty.clone() {
                        panic!("TypeError");
                    }
                    node.ty = Some(Type::Ptr { to });
                }
                node = Node {
                    ty: node.ty.clone(),
                    node: NodeType::Op {
                        kind: NodeKind::Sub,
                        lhs: Box::new(node),
                        rhs: Box::new(node_r),
                    },
                };
            } else {
                break;
            }
        }
        node
    }

    fn mul(&mut self) -> Node {
        let mut node = self.unary();
        loop {
            if self.iter.next_if_eq(&Token::Reserved("*")).is_some() {
                let node_r = self.unary();
                if node.ty != Some(Type::Int) || node_r.ty != Some(Type::Int) {
                    panic!("TypeError");
                }
                node = Node {
                    ty: node.ty.clone(),
                    node: NodeType::Op {
                        kind: NodeKind::Mul,
                        lhs: Box::new(node),
                        rhs: Box::new(node_r),
                    },
                }
            } else if self.iter.next_if_eq(&Token::Reserved("/")).is_some() {
                let node_r = self.unary();
                if node.ty != Some(Type::Int) || node_r.ty != Some(Type::Int) {
                    panic!("TypeError");
                }
                node = Node {
                    ty: node.ty.clone(),
                    node: NodeType::Op {
                        kind: NodeKind::Div,
                        lhs: Box::new(node),
                        rhs: Box::new(node_r),
                    },
                }
            } else {
                break;
            }
        }
        node
    }

    fn unary(&mut self) -> Node {
        if self.iter.next_if_eq(&Token::Reserved("+")).is_some() {
            self.unary()
        } else if self.iter.next_if_eq(&Token::Reserved("-")).is_some() {
            let rhs = self.unary();
            if rhs.ty != Some(Type::Int) {
                panic!("TypeError");
            }
            Node {
                ty: Some(Type::Int),
                node: NodeType::Op {
                    kind: NodeKind::Sub,
                    lhs: Box::new(Node {
                        ty: Some(Type::Int),
                        node: NodeType::Num { val: 0 },
                    }),
                    rhs: Box::new(rhs),
                },
            }
        } else if self.iter.next_if_eq(&Token::Reserved("&")).is_some() {
            let Node { ty, node } = self.unary();
            let ty = ty.expect("TypeError");
            Node {
                ty: Some(Type::Ptr {
                    to: Box::new(ty.clone()),
                }),
                node: NodeType::Addr {
                    val: Box::new(Node { ty: Some(ty), node }),
                },
            }
        } else if self.iter.next_if_eq(&Token::Reserved("*")).is_some() {
            let node = self.unary();
            let ret;
            if let Some(ty) = node.ty.clone() {
                if let Type::Ptr { to } = ty {
                    ret = Some(*to);
                } else {
                    panic!("TypeError");
                }
            } else {
                ret = None;
            }
            Node {
                ty: ret,
                node: NodeType::Deref {
                    val: Box::new(node),
                },
            }
        } else {
            self.primary()
        }
    }

    fn primary(&mut self) -> Node {
        match self.iter.next().unwrap() {
            Token::Reserved(ty) => {
                if ty == "(" {
                    let node = self.expr();
                    self.expect(")");
                    node
                } else {
                    panic!("ParseError")
                }
            }
            Token::Num(num) => Node {
                ty: Some(Type::Int),
                node: NodeType::Num { val: num },
            },
            Token::Ident(s) => {
                if self.iter.next_if_eq(&Token::Reserved("(")).is_some() {
                    let ret_type;
                    let arg_types;
                    if let Some(idx) = self.funcs.iter().position(|i| i.name == s) {
                        let func = &self.funcs[idx];
                        ret_type = Some(func.ret.clone());
                        arg_types = Some(func.args.clone());
                    } else {
                        ret_type = None;
                        arg_types = None;
                    }
                    if self.iter.next_if_eq(&Token::Reserved(")")).is_some() {
                        if arg_types.is_some() && arg_types.unwrap().len() != 0 {
                            panic!("TypeError");
                        }
                        Node {
                            ty: ret_type,
                            node: NodeType::FunCall {
                                name: s,
                                args: Vec::new(),
                            },
                        }
                    } else {
                        let mut args = Vec::new();
                        args.push(self.assign());
                        while self.iter.peek() != Some(&Token::Reserved(")")) {
                            self.expect(",");
                            args.push(self.assign());
                        }
                        if arg_types.is_some()
                            && args
                                .iter()
                                .map(|i| i.ty.clone().unwrap())
                                .collect::<Vec<_>>()
                                != arg_types
                                    .unwrap()
                                    .iter()
                                    .map(|i| i.ty.clone())
                                    .collect::<Vec<_>>()
                        {
                            panic!("TypeError");
                        }
                        self.iter.next();
                        Node {
                            ty: ret_type,
                            node: NodeType::FunCall { name: s, args },
                        }
                    }
                } else {
                    Node {
                        ty: Some(self.get_lvar(&s).ty),
                        node: NodeType::LVar {
                            val: self.get_lvar(&s),
                        },
                    }
                }
            }
            val => {
                dbg!(val);
                panic!("ParseError");
            }
        }
    }
}
