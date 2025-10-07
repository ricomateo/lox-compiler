use crate::expr::Expr;

#[derive(Debug, Clone)]
pub struct Declaration {
    pub inner: DeclarationKind,
    pub line: usize,
}

impl Declaration {
    pub fn variable_declaration(name: String, initializer: Option<Expr>, line: usize) -> Self {
        Self {
            inner: DeclarationKind::VariableDeclaration { name, initializer },
            line,
        }
    }

    pub fn statement(statement: Statement, line: usize) -> Self {
        Self {
            inner: DeclarationKind::Statement(statement),
            line,
        }
    }
}

#[derive(Debug, Clone)]
pub enum DeclarationKind {
    VariableDeclaration {
        name: String,
        initializer: Option<Expr>,
    },
    Statement(Statement),
}

#[derive(Debug, Clone)]
pub enum Statement {
    ExprStatement(Expr),
    PrintStatement(Expr),
}
