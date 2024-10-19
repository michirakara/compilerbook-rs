use crate::{parse::Function, parse::NodeType, parse::Type, Node, NodeKind};
use std::iter::zip;

const ARGREG: [&str; 6] = ["rdi", "rsi", "rdx", "rcx", "r8", "r9"];

pub struct Data {
    label_idx: usize,
    funcname: String,
    stack_size: usize,
}

pub fn codegen(program: Vec<Function>) {
    println!(".intel_syntax noprefix");
    let mut label_idx = 0;
    for f in program {
        let mut data = Data {
            label_idx,
            funcname: f.name.clone(),
            stack_size: f.stack_size,
        };
        println!(".global {}", f.name);
        println!("{}:", f.name);

        println!("    push rbp");
        println!("    mov rbp, rsp");
        println!("    sub rsp, {}", f.stack_size);

        for (v, r) in zip(f.args, ARGREG) {
            println!("    mov [rbp-{}], {}", f.stack_size + 8 - v.offset, r);
        }

        for i in f.nodes {
            gen(i, &mut data);
        }
        println!(".Lreturn.{}:", f.name);
        println!("    mov rsp, rbp");
        println!("    pop rbp");
        println!("    ret");
        label_idx = data.label_idx;
    }
}

fn gen_addr(node: Node, data: &mut Data) {
    if let NodeType::LVar { val } = node.node {
        println!("    lea rax, [rbp-{}]", data.stack_size + 8 - val.offset);
        println!("    push rax");
    } else if let NodeType::Deref { val } = node.node {
        gen(*val, data);
    } else {
        dbg!(node);
        panic!();
    }
}

fn store() {
    println!("    pop rdi");
    println!("    pop rax");
    println!("    mov [rax], rdi");
    println!("    push rdi");
}

fn load() {
    println!("    pop rax");
    println!("    mov rax, [rax]");
    println!("    push rax");
}

pub fn gen(node: Node, data: &mut Data) {
    match node.node {
        NodeType::Block { stmts } => {
            for i in stmts {
                gen(i, data);
            }
        }
        NodeType::If {
            cond,
            if_do,
            else_do,
        } => {
            let idx = data.label_idx;
            data.label_idx += 1;
            gen(*cond, data);
            println!("    pop rax");
            println!("    cmp rax, 0");
            println!("    je .Lelse{}", idx);
            gen(*if_do, data);
            println!("    jmp .Lend{}", idx);
            println!(".Lelse{}:", idx);
            if let Some(else_do) = else_do {
                gen(*else_do, data);
            }
            println!(".Lend{}:", idx);
        }
        NodeType::For {
            init,
            cond,
            expr,
            stmt,
        } => {
            let idx = data.label_idx;
            data.label_idx += 1;
            if let Some(init) = init {
                gen(*init, data);
            }
            println!(".Lbegin{}:", idx);
            if let Some(cond) = cond {
                gen(*cond, data);
            }
            println!("    pop rax");
            println!("    cmp rax, 0");
            println!("    je .Lend{}", idx);
            gen(*stmt, data);
            if let Some(expr) = expr {
                gen(*expr, data);
            }
            println!("    jmp .Lbegin{}", idx);
            println!(".Lend{}:", idx);
        }
        NodeType::While { cond, while_do } => {
            let idx = data.label_idx;
            data.label_idx += 1;
            println!(".Lbegin{}:", idx);
            gen(*cond, data);
            println!("    pop rax");
            println!("    cmp rax, 0");
            println!("    je .Lend{}", idx);
            gen(*while_do, data);
            println!("    jmp .Lbegin{}", idx);
            println!(".Lend{}:", idx);
        }
        NodeType::Return { stmt } => {
            gen(*stmt, data);
            println!("    pop rax");
            println!("    jmp .Lreturn.{}", data.funcname);
        }
        NodeType::Assign { lhs, rhs } => {
            gen_addr(*lhs, data);
            gen(*rhs, data);
            store();
        }
        NodeType::Addr { val } => {
            gen_addr(*val, data);
        }
        NodeType::Deref { val } => {
            gen(*val, data);
            load();
        }
        NodeType::Num { val } => {
            println!("    push {}", val);
        }
        NodeType::FunCall { name, args } => {
            let tmp = args.len();
            for i in args {
                gen(i, data);
            }
            for (_, reg) in zip(0..tmp, ARGREG).rev() {
                println!("    pop {}", reg);
            }
            let idx = data.label_idx;
            data.label_idx += 1;
            println!("    mov rax, rsp");
            println!("    and rax, 15");
            println!("    jnz .Lcall{}", idx);
            println!("    mov rax, 0");
            println!("    call {}", name);
            println!("    jmp .Lend{}", idx);
            println!(".Lcall{}:", idx);
            println!("    sub rsp, 8");
            println!("    mov rax, 0");
            println!("    call {}", name);
            println!("    add rsp, 8");
            println!(".Lend{}:", idx);
            println!("    push rax");
        }
        NodeType::LVar { val: _ } => {
            gen_addr(node, data);
            load();
        }
        NodeType::Op { kind, lhs, rhs } => {
            gen(*lhs, data);
            gen(*rhs, data);
            println!("    pop rdi");
            println!("    pop rax");
            match kind {
                NodeKind::Add => {
                    if let Some(Type::Ptr { to: _ }) = node.ty {
                        println!("    imul rdi, 8");
                    }
                    println!("    add rax, rdi");
                }
                NodeKind::Sub => {
                    if let Some(Type::Ptr { to: _ }) = node.ty {
                        println!("    imul rdi, 8");
                    }
                    println!("    sub rax, rdi");
                }
                NodeKind::Mul => println!("    imul rax, rdi"),
                NodeKind::Div => {
                    println!("    cqo");
                    println!("    idiv rdi");
                }
                NodeKind::Eq => {
                    println!("    cmp rax, rdi");
                    println!("    sete al");
                    println!("    movzb rax, al");
                }
                NodeKind::Ne => {
                    println!("    cmp rax, rdi");
                    println!("    setne al");
                    println!("    movzb rax, al");
                }
                NodeKind::Lt => {
                    println!("    cmp rax, rdi");
                    println!("    setl al");
                    println!("    movzb rax, al");
                }
                NodeKind::Le => {
                    println!("    cmp rax, rdi");
                    println!("    setle al");
                    println!("    movzb rax, al");
                }
            }
            println!("    push rax");
        }
        NodeType::None => {}
    }
}
