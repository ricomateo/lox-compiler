use crate::expr::Expr;

#[derive(Debug)]
pub enum Declaration {
    VariableDeclaration {
        name: String,
        initializer: Option<Expr>,
    },
    Statement(Statement),
}

#[derive(Debug)]
pub enum Statement {
    ExprStatement(Expr),
    PrintStatement(Expr),
}
