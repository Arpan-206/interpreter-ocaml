// ast.rs — mirrors the OCaml Expr.t / Stmt.t AST
// Tags must match the serialisation order in serialise.ml exactly.

use serde::Deserialize;

#[derive(Debug, Deserialize)]
pub enum Literal {
    Bool(bool),  // tag 0
    Nil,         // tag 1
    Num(f64),    // tag 2
    Str(String), // tag 3
}

#[derive(Debug, Deserialize)]
pub enum UnaryOp {
    Negate, // tag 0
    Not,    // tag 1
}

#[derive(Debug, Deserialize)]
pub enum BinaryOp {
    Add,          // tag 0
    Subtract,     // tag 1
    Multiply,     // tag 2
    Divide,       // tag 3
    Greater,      // tag 4
    GreaterEqual, // tag 5
    Less,         // tag 6
    LessEqual,    // tag 7
    Equal,        // tag 8
    NotEqual,     // tag 9
}

#[derive(Debug, Deserialize)]
pub enum Expr {
    Literal(Literal),                       // tag 0
    Grouping(Box<Expr>),                    // tag 1
    Unary(UnaryOp, Box<Expr>),              // tag 2
    Binary(Box<Expr>, BinaryOp, Box<Expr>), // tag 3
    Variable {
        name: String,
        line: u32,
        uid: u32,
    }, // tag 4
    Assign {
        name: String,
        value: Box<Expr>,
        line: u32,
        uid: u32,
    }, // tag 5
    Or(Box<Expr>, Box<Expr>),               // tag 6
    And(Box<Expr>, Box<Expr>),              // tag 7
    Call {
        callee: Box<Expr>,
        args: Vec<Expr>,
        line: u32,
    }, // tag 8
    Get {
        object: Box<Expr>,
        name: String,
        line: u32,
    }, // tag 9
    Set {
        object: Box<Expr>,
        name: String,
        value: Box<Expr>,
        line: u32,
    }, // tag 10
    This {
        line: u32,
        uid: u32,
    }, // tag 11
    Super {
        line: u32,
        method: String,
        uid: u32,
    }, // tag 12
}

#[derive(Debug, Deserialize)]
pub enum Stmt {
    Print(Expr),      // tag 0
    Expression(Expr), // tag 1
    VarDecl {
        name: String,
        init: Option<Expr>,
        line: u32,
    }, // tag 2
    Block(Vec<Stmt>), // tag 3
    If {
        cond: Expr,
        then_: Box<Stmt>,
        else_: Option<Box<Stmt>>,
    }, // tag 4
    While {
        cond: Expr,
        body: Box<Stmt>,
    }, // tag 5
    FunDecl {
        name: String,
        params: Vec<String>,
        body: Vec<Stmt>,
        line: u32,
    }, // tag 6
    Return {
        value: Option<Expr>,
        line: u32,
    }, // tag 7
    ClassDecl {
        // tag 8
        name: String,
        superclass: Option<Expr>,
        methods: Vec<Stmt>,
        line: u32,
    },
}
