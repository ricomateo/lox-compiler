use crate::expr::Expr;

#[derive(Debug)]
pub enum Declaration {
    VariableDeclaration,
    Statement(Statement),
}

#[derive(Debug)]
pub enum Statement {
    ExprStatement(Expr),
    PrintStatement(Expr),
}
