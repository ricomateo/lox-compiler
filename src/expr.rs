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
}

#[derive(Debug, PartialEq, Clone)]
pub enum Literal {
    Number(f64),
}
