use crate::scanner::Token;

#[derive(Debug, PartialEq, Clone)]
pub enum Expr {
    Binary {
        left: Box<Expr>,
        operator: Token,
        right: Box<Expr>,
    },
    Unary {
        operator: Token,
        right: Box<Expr>,
    },
    Literal(Literal),
    Grouping {
        expression: Box<Expr>,
    },
    Variable {
        name: String,
    },
    VariableAssignment {
        name: String,
        value: Box<Expr>,
    },
    Logical {
        left: Box<Expr>,
        operator: Token,
        right: Box<Expr>,
    },
    Call {
        arguments: Vec<Expr>,
    },
}

#[derive(Debug, PartialEq, Clone)]
pub enum Literal {
    Number(f64),
    Bool(bool),
    Nil,
    String(String),
}
