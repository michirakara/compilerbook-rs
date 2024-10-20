use crate::tokenize::{BaseType, Token};
use std::iter::Peekable;

#[derive(Debug, Clone)]
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
    Array { of: Box<Type>, size: usize },
    Int,
}

impl Type {
    pub fn sizeof(&self) -> usize {
        match self {
            Self::Int => 8,
            Self::Ptr { to: _ } => 8,
            Self::Array { of, size } => of.sizeof() * size,
        }
    }
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

#[derive(Debug, Clone)]
pub enum ErrorType {
    Parse,
    Type,
    Name,
}

impl std::fmt::Display for ErrorType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Parse => write!(f, "ParseError"),
            Self::Type => write!(f, "TypeError"),
            Self::Name => write!(f, "NameError"),
        }
    }
}

#[derive(Debug, Clone)]
#[allow(dead_code)]
pub struct Error {
    pub ty: ErrorType,
    pub pos: (usize, usize),
    pub msg: String,
}

pub struct Parser<T: Iterator<Item = (Token, (usize, usize))> + Clone> {
    pub iter: Peekable<T>,
    locals: Locals,
    funcs: Vec<Function>,
    pos: (usize, usize),
}
impl<T: Iterator<Item = (Token, (usize, usize))> + Clone> Parser<T> {
    pub fn new(iter: Peekable<T>) -> Parser<T> {
        Self {
            iter,
            locals: Locals {
                locals: Vec::new(),
                stack_size: 0,
            },
            funcs: Vec::new(),
            pos: (0, 1),
        }
    }
}

impl<T: Iterator<Item = (Token, (usize, usize))> + Clone> Parser<T> {
    fn next(&mut self) -> Option<(Token, (usize, usize))> {
        let tmp = self.iter.next();
        if let Some((_, pos)) = tmp {
            self.pos = pos;
        }
        tmp
    }
    fn next_if(
        &mut self,
        f: impl FnOnce(&(Token, (usize, usize))) -> bool,
    ) -> Option<(Token, (usize, usize))> {
        let tmp = self.iter.next_if(f);
        if let Some((_, pos)) = tmp {
            self.pos = pos;
        }
        tmp
    }
    fn new_lvar(&mut self, s: &str, ty: &Type) -> Result<LVar, Error> {
        Ok(LVar {
            name: s.to_string(),
            ty: ty.clone(),
            offset: {
                self.locals.stack_size += ty.sizeof();
                self.locals.locals.push(LVar {
                    name: s.to_string(),
                    ty: ty.clone(),
                    offset: self.locals.stack_size,
                });
                self.locals.locals.last().unwrap().offset
            },
        })
    }
    fn get_lvar(&mut self, s: &str) -> Result<LVar, Error> {
        let idx = self
            .locals
            .locals
            .iter()
            .position(|elem| elem.name == s)
            .ok_or(Error {
                ty: ErrorType::Name,
                pos: self.pos,
                msg: format!("name {s} is not defined"),
            })?;
        Ok(LVar {
            name: s.to_string(),
            ty: self.locals.locals[idx].ty.clone(),
            offset: self.locals.locals[idx].offset,
        })
    }
    fn expect(&mut self, s: &'static str) -> Result<(), Error> {
        let (tok, pos) = self.next().ok_or(Error {
            ty: ErrorType::Parse,
            pos: self.pos,
            msg: format!("expected \"{s}\""),
        })?;
        self.pos = pos;
        if Token::Reserved(s) != tok {
            Err(Error {
                ty: ErrorType::Parse,
                pos: self.pos,
                msg: format!("expected \"{s}\""),
            })
        } else {
            Ok(())
        }
    }
    fn is_expected_then_next(&mut self, s: &'static str) -> bool {
        let tmp = self.next_if(|i| i.0 == Token::Reserved(s));
        if let Some((_, pos)) = tmp {
            self.pos = pos;
            true
        } else {
            false
        }
    }
    fn expect_ident(&mut self) -> Result<String, Error> {
        if let Some((Token::Ident(s), _)) = self.next() {
            Ok(s)
        } else {
            Err(Error {
                ty: ErrorType::Parse,
                pos: self.pos,
                msg: "expected an identifier".to_string(),
            })
        }
    }
    fn expect_number(&mut self) -> Result<u64, Error> {
        if let Some((Token::Num(n), _)) = self.next() {
            Ok(n)
        } else {
            Err(Error {
                ty: ErrorType::Parse,
                pos: self.pos,
                msg: "expected a number".to_string(),
            })
        }
    }
    fn basetype(&mut self) -> Result<Type, Error> {
        if self
            .next_if(|i| i.0 == Token::BaseType(BaseType::Int))
            .is_some()
        {
            let mut ty = Type::Int;
            while self.is_expected_then_next("*") {
                ty = Type::Ptr { to: Box::new(ty) };
            }
            Ok(ty)
        } else {
            Err(Error {
                ty: ErrorType::Parse,
                pos: self.pos,
                msg: "expected int".to_string(),
            })
        }
    }
    fn type_suffix(&mut self, ty: Type) -> Result<Type, Error> {
        if self.is_expected_then_next("[") {
            let size = self.expect_number()? as usize;
            self.expect("]")?;
            let ty = self.type_suffix(ty)?;
            Ok(Type::Array {
                of: Box::new(ty),
                size,
            })
        } else {
            Ok(ty)
        }
    }
    pub fn program(&mut self) -> Result<Vec<Function>, Error> {
        let tmp = self.iter.clone();
        while self.iter.peek().is_some() {
            let ty = self.basetype()?;
            let s = self.expect_ident()?;
            let ty = self.type_suffix(ty)?;
            let mut func = Function {
                name: s.to_string(),
                args: Vec::new(),
                ret: ty,
                nodes: Vec::new(),
                stack_size: 0,
            };
            self.expect("(")?;
            if !self.is_expected_then_next(")") {
                let ty = self.basetype()?;
                let s = self.expect_ident()?;
                let ty = self.type_suffix(ty)?;
                func.args.push(self.new_lvar(&s, &ty)?);
                while !self.is_expected_then_next(")") {
                    self.expect(",")?;
                    let ty = self.basetype()?;
                    let s = self.expect_ident()?;
                    let ty = self.type_suffix(ty)?;
                    func.args.push(self.new_lvar(&s, &ty)?);
                }
            }
            self.expect("{")?;
            let mut cnt = 1;
            while let Some(s) = self.next() {
                if s.0 == Token::Reserved("{") {
                    cnt += 1;
                }
                if s.0 == Token::Reserved("}") {
                    cnt -= 1;
                }
                if cnt == 0 {
                    break;
                }
            }
            self.funcs.push(func);
            self.locals.locals = Vec::new();
            self.locals.stack_size = 0;
        }
        let mut idx = 0;
        self.iter = tmp;
        self.pos = (0, 1);
        while self.iter.peek().is_some() {
            let ty = self.basetype()?;
            let s = self.expect_ident()?;
            let ty = self.type_suffix(ty)?;
            let mut func = Function {
                name: s.to_string(),
                args: Vec::new(),
                ret: ty,
                nodes: Vec::new(),
                stack_size: 0,
            };
            self.expect("(")?;
            if !self.is_expected_then_next(")") {
                let ty = self.basetype()?;
                let s = self.expect_ident()?;
                let ty = self.type_suffix(ty)?;
                func.args.push(self.new_lvar(&s, &ty)?);
                while !self.is_expected_then_next(")") {
                    self.expect(",")?;
                    let ty = self.basetype()?;
                    let s = self.expect_ident()?;
                    let ty = self.type_suffix(ty)?;
                    func.args.push(self.new_lvar(&s, &ty)?);
                }
            }
            self.expect("{")?;
            while !self.is_expected_then_next("}") {
                func.nodes.push(self.stmt()?);
            }
            func.stack_size = self.locals.stack_size;
            self.funcs[idx] = func;
            idx += 1;
            self.locals.locals = Vec::new();
            self.locals.stack_size = 0;
        }
        Ok(self.funcs.clone())
    }
    fn stmt(&mut self) -> Result<Node, Error> {
        if self.is_expected_then_next("{") {
            let mut ret = Vec::new();
            while !self.is_expected_then_next("}") {
                ret.push(self.stmt()?);
            }
            Ok(Node {
                ty: None,
                node: NodeType::Block { stmts: ret },
            })
        } else if self.next_if(|i| i.0 == Token::If).is_some() {
            self.expect("(")?;
            let node = self.expr()?;
            self.expect(")")?;
            Ok(Node {
                ty: None,
                node: NodeType::If {
                    cond: Box::new(node),
                    if_do: Box::new(self.stmt()?),
                    else_do: if self.next_if(|i| i.0 == Token::Else).is_some() {
                        Some(Box::new(self.stmt()?))
                    } else {
                        None
                    },
                },
            })
        } else if self.next_if(|i| i.0 == Token::While).is_some() {
            self.expect("(")?;
            let node = self.expr()?;
            self.expect(")")?;
            Ok(Node {
                ty: None,
                node: NodeType::While {
                    cond: Box::new(node),
                    while_do: Box::new(self.stmt()?),
                },
            })
        } else if self.next_if(|i| i.0 == Token::For).is_some() {
            self.expect("(")?;
            Ok(Node {
                ty: None,
                node: NodeType::For {
                    init: if self.is_expected_then_next(";") {
                        None
                    } else {
                        let tmp = Some(Box::new(self.expr()?));
                        self.expect(";")?;
                        tmp
                    },
                    cond: if self.is_expected_then_next(";") {
                        None
                    } else {
                        let tmp = Some(Box::new(self.expr()?));
                        self.expect(";")?;
                        tmp
                    },
                    expr: if self.is_expected_then_next(")") {
                        None
                    } else {
                        let tmp = Some(Box::new(self.expr()?));
                        self.expect(")")?;
                        tmp
                    },
                    stmt: Box::new(self.stmt()?),
                },
            })
        } else if self.next_if(|i| i.0 == Token::Return).is_some() {
            let child_node = self.expr()?;
            self.expect(";")?;
            Ok(Node {
                ty: child_node.ty.clone(),
                node: NodeType::Return {
                    stmt: Box::new(child_node),
                },
            })
        } else if let Some((Token::BaseType(_), (_, _))) = self.iter.peek() {
            self.declaration()
        } else {
            let node = self.expr()?;
            self.expect(";")?;
            Ok(node)
        }
    }
    fn declaration(&mut self) -> Result<Node, Error> {
        let ty = self.basetype()?;
        let s = self.expect_ident()?;
        let ty = self.type_suffix(ty)?;
        if self.is_expected_then_next("=") {
            let lvar = self.new_lvar(&s, &ty)?;
            let rhs = self.expr()?;
            // TODO: type check
            let ret = NodeType::Assign {
                lhs: Box::new(Node {
                    ty: Some(ty),
                    node: NodeType::LVar { val: lvar },
                }),
                rhs: Box::new(rhs),
            };
            self.expect(";")?;
            Ok(Node {
                ty: None,
                node: ret,
            })
        } else {
            self.new_lvar(&s, &ty)?;
            self.expect(";")?;
            Ok(Node {
                ty: None,
                node: NodeType::None,
            })
        }
    }
    fn expr(&mut self) -> Result<Node, Error> {
        self.assign()
    }
    fn assign(&mut self) -> Result<Node, Error> {
        let mut node = self.equality()?;
        if self.is_expected_then_next("=") {
            let node2 = self.assign()?;
            // TODO: type check
            node = Node {
                ty: node2.ty.clone(),
                node: NodeType::Assign {
                    lhs: Box::new(node),
                    rhs: Box::new(node2),
                },
            };
        }
        Ok(node)
    }

    fn equality(&mut self) -> Result<Node, Error> {
        let mut node = self.relational()?;
        loop {
            if self.is_expected_then_next("==") {
                let node2 = self.relational()?;
                if node.ty != node2.ty {
                    return Err(Error {
                        ty: ErrorType::Type,
                        pos: self.pos,
                        msg: format!(
                            "cannot compare {} and {}",
                            if let Type::Ptr { to: _ } = node.ty.unwrap().clone() {
                                "pointer"
                            } else {
                                "int"
                            },
                            if let Type::Ptr { to: _ } = node2.ty.unwrap().clone() {
                                "pointer"
                            } else {
                                "int"
                            },
                        ),
                    });
                }
                node = Node {
                    ty: Some(Type::Int),
                    node: NodeType::Op {
                        kind: NodeKind::Eq,
                        lhs: Box::new(node),
                        rhs: Box::new(node2),
                    },
                };
            } else if self.is_expected_then_next("!=") {
                let node2 = self.relational()?;
                if node.ty != node2.ty {
                    return Err(Error {
                        ty: ErrorType::Type,
                        pos: self.pos,
                        msg: format!(
                            "cannot compare {} and {}",
                            if let Type::Ptr { to: _ } = node.ty.unwrap().clone() {
                                "pointer"
                            } else {
                                "int"
                            },
                            if let Type::Ptr { to: _ } = node2.ty.unwrap().clone() {
                                "pointer"
                            } else {
                                "int"
                            },
                        ),
                    });
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
        Ok(node)
    }
    fn relational(&mut self) -> Result<Node, Error> {
        let mut node = self.add()?;
        loop {
            if self.is_expected_then_next("<") {
                let node2 = self.add()?;
                if node.ty != node2.ty {
                    return Err(Error {
                        ty: ErrorType::Type,
                        pos: self.pos,
                        msg: format!(
                            "cannot compare {} and {}",
                            if let Type::Ptr { to: _ } = node.ty.unwrap().clone() {
                                "pointer"
                            } else {
                                "int"
                            },
                            if let Type::Ptr { to: _ } = node2.ty.unwrap().clone() {
                                "pointer"
                            } else {
                                "int"
                            },
                        ),
                    });
                }
                node = Node {
                    ty: Some(Type::Int),
                    node: NodeType::Op {
                        kind: NodeKind::Lt,
                        lhs: Box::new(node),
                        rhs: Box::new(node2),
                    },
                };
            } else if self.is_expected_then_next("<=") {
                let node2 = self.add()?;
                if node.ty != node2.ty {
                    return Err(Error {
                        ty: ErrorType::Type,
                        pos: self.pos,
                        msg: format!(
                            "cannot compare {} and {}",
                            if let Type::Ptr { to: _ } = node.ty.unwrap().clone() {
                                "pointer"
                            } else {
                                "int"
                            },
                            if let Type::Ptr { to: _ } = node2.ty.unwrap().clone() {
                                "pointer"
                            } else {
                                "int"
                            },
                        ),
                    });
                }
                node = Node {
                    ty: Some(Type::Int),
                    node: NodeType::Op {
                        kind: NodeKind::Le,
                        lhs: Box::new(node),
                        rhs: Box::new(node2),
                    },
                };
            } else if self.is_expected_then_next(">") {
                let node2 = self.add()?;
                if node.ty != node2.ty {
                    return Err(Error {
                        ty: ErrorType::Type,
                        pos: self.pos,
                        msg: format!(
                            "cannot compare {} and {}",
                            if let Type::Ptr { to: _ } = node.ty.unwrap().clone() {
                                "pointer"
                            } else {
                                "int"
                            },
                            if let Type::Ptr { to: _ } = node2.ty.unwrap().clone() {
                                "pointer"
                            } else {
                                "int"
                            },
                        ),
                    });
                }
                node = Node {
                    ty: Some(Type::Int),
                    node: NodeType::Op {
                        kind: NodeKind::Lt,
                        lhs: Box::new(node2),
                        rhs: Box::new(node),
                    },
                };
            } else if self.is_expected_then_next(">=") {
                let node2 = self.add()?;
                if node.ty != node2.ty {
                    return Err(Error {
                        ty: ErrorType::Type,
                        pos: self.pos,
                        msg: format!(
                            "cannot compare {} and {}",
                            if let Type::Ptr { to: _ } = node.ty.unwrap().clone() {
                                "pointer"
                            } else {
                                "int"
                            },
                            if let Type::Ptr { to: _ } = node2.ty.unwrap().clone() {
                                "pointer"
                            } else {
                                "int"
                            },
                        ),
                    });
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
        Ok(node)
    }
    fn add(&mut self) -> Result<Node, Error> {
        let mut node = self.mul()?;
        loop {
            if self.is_expected_then_next("+") {
                let mut node_r = self.mul()?;
                match &node_r.ty {
                    Some(Type::Ptr { to }) | Some(Type::Array { of: to, size: _ }) => {
                        // TODO: type check
                        node.ty = Some(Type::Ptr { to: to.clone() });
                        std::mem::swap(&mut node, &mut node_r);
                    }
                    _ => {}
                }
                node = Node {
                    ty: node.ty.clone(),
                    node: NodeType::Op {
                        kind: NodeKind::Add,
                        lhs: Box::new(node),
                        rhs: Box::new(node_r),
                    },
                };
            } else if self.is_expected_then_next("-") {
                let node_r = self.mul()?;
                if let Some(Type::Ptr { to }) = node_r.ty.clone() {
                    // TODO: type check
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
        Ok(node)
    }

    fn mul(&mut self) -> Result<Node, Error> {
        let mut node = self.unary()?;
        loop {
            if self.is_expected_then_next("*") {
                let node_r = self.unary()?;
                if node.ty != Some(Type::Int) || node_r.ty != Some(Type::Int) {
                    return Err(Error {
                        ty: ErrorType::Type,
                        pos: self.pos,
                        msg: format!(
                            "cannot multiply {} and {}",
                            if let Type::Ptr { to: _ } = node.ty.unwrap().clone() {
                                "pointer"
                            } else {
                                "int"
                            },
                            if let Type::Ptr { to: _ } = node_r.ty.unwrap().clone() {
                                "pointer"
                            } else {
                                "int"
                            },
                        ),
                    });
                }
                node = Node {
                    ty: node.ty.clone(),
                    node: NodeType::Op {
                        kind: NodeKind::Mul,
                        lhs: Box::new(node),
                        rhs: Box::new(node_r),
                    },
                }
            } else if self.is_expected_then_next("/") {
                let node_r = self.unary()?;
                if node.ty != Some(Type::Int) || node_r.ty != Some(Type::Int) {
                    return Err(Error {
                        ty: ErrorType::Type,
                        pos: self.pos,
                        msg: format!(
                            "cannot divide {} and {}",
                            if let Type::Ptr { to: _ } = node.ty.unwrap().clone() {
                                "pointer"
                            } else {
                                "int"
                            },
                            if let Type::Ptr { to: _ } = node_r.ty.unwrap().clone() {
                                "pointer"
                            } else {
                                "int"
                            },
                        ),
                    });
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
        Ok(node)
    }

    fn unary(&mut self) -> Result<Node, Error> {
        if self.is_expected_then_next("sizeof") {
            Ok(Node {
                ty: Some(Type::Int),
                node: NodeType::Num {
                    val: self.unary()?.ty.unwrap().sizeof() as u64,
                },
            })
        } else if self.is_expected_then_next("+") {
            self.primary()
        } else if self.is_expected_then_next("-") {
            let rhs = self.unary()?;
            // TODO: type check
            Ok(Node {
                ty: Some(Type::Int),
                node: NodeType::Op {
                    kind: NodeKind::Sub,
                    lhs: Box::new(Node {
                        ty: Some(Type::Int),
                        node: NodeType::Num { val: 0 },
                    }),
                    rhs: Box::new(rhs),
                },
            })
        } else if self.is_expected_then_next("&") {
            let Node { ty, node } = self.unary()?;
            let ty = ty.ok_or(Error {
                ty: ErrorType::Type,
                pos: self.pos,
                msg: "???".to_string(),
            })?;
            Ok(Node {
                ty: Some(Type::Ptr {
                    to: Box::new(ty.clone()),
                }),
                node: NodeType::Addr {
                    val: Box::new(Node { ty: Some(ty), node }),
                },
            })
        } else if self.is_expected_then_next("*") {
            let node = self.unary()?;
            let ret;
            if let Some(ty) = node.ty.clone() {
                match ty {
                    Type::Ptr { to } | Type::Array { of: to, size: _ } => {
                        ret = Some(*to);
                    }
                    _ => {
                        return Err(Error {
                            ty: ErrorType::Type,
                            pos: self.pos,
                            msg: "cannot dereference int".to_string(),
                        });
                    }
                }
            } else {
                ret = None;
            }
            Ok(Node {
                ty: ret,
                node: NodeType::Deref {
                    val: Box::new(node),
                },
            })
        } else {
            self.postfix()
        }
    }

    fn postfix(&mut self) -> Result<Node, Error> {
        let mut node = self.primary()?;
        while self.is_expected_then_next("[") {
            let mut node_r = self.expr()?;
            match &node_r.ty {
                Some(Type::Ptr { to }) | Some(Type::Array { of: to, size: _ }) => {
                    // TODO: type check
                    node.ty = Some(Type::Ptr { to: to.clone() });
                    std::mem::swap(&mut node, &mut node_r);
                }
                _ => {}
            }
            let ret;
            if let Some(ty) = node.ty.clone() {
                match ty {
                    Type::Ptr { to } | Type::Array { of: to, size: _ } => {
                        ret = Some(*to);
                    }
                    _ => {
                        todo!()
                    }
                }
            } else {
                ret = None;
            }
            node = Node {
                ty: ret,
                node: NodeType::Deref {
                    val: Box::new(Node {
                        ty: node.ty.clone(),
                        node: NodeType::Op {
                            kind: NodeKind::Add,
                            lhs: Box::new(node),
                            rhs: Box::new(node_r),
                        },
                    }),
                },
            };
            self.expect("]")?;
        }
        Ok(node)
    }

    fn primary(&mut self) -> Result<Node, Error> {
        match self.next().unwrap().0 {
            Token::Reserved(ty) => {
                if ty == "(" {
                    let node = self.expr();
                    self.expect(")")?;
                    node
                } else {
                    panic!("Parse")
                }
            }
            Token::Num(num) => Ok(Node {
                ty: Some(Type::Int),
                node: NodeType::Num { val: num },
            }),
            Token::Ident(s) => {
                if self.is_expected_then_next("(") {
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
                    if self.is_expected_then_next(")") {
                        if arg_types.is_some() && !arg_types.unwrap().is_empty() {
                            return Err(Error {
                                ty: ErrorType::Type,
                                pos: self.pos,
                                msg: "argument to this function is wrong".to_string(),
                            });
                        }
                        Ok(Node {
                            ty: ret_type,
                            node: NodeType::FunCall {
                                name: s,
                                args: Vec::new(),
                            },
                        })
                    } else {
                        let mut args = Vec::new();
                        args.push(self.assign()?);
                        while !self.is_expected_then_next(")") {
                            self.expect(",")?;
                            args.push(self.assign()?);
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
                            return Err(Error {
                                ty: ErrorType::Type,
                                pos: self.pos,
                                msg: "argument to this function is wrong".to_string(),
                            });
                        }
                        Ok(Node {
                            ty: ret_type,
                            node: NodeType::FunCall { name: s, args },
                        })
                    }
                } else {
                    Ok(Node {
                        ty: Some(self.get_lvar(&s)?.ty),
                        node: NodeType::LVar {
                            val: self.get_lvar(&s)?,
                        },
                    })
                }
            }
            _ => Err(Error {
                ty: ErrorType::Parse,
                pos: self.pos,
                msg: "expected something".to_string(),
            }),
        }
    }
}
