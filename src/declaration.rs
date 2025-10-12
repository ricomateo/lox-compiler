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

    pub fn block(declarations: Vec<Declaration>, line: usize) -> Self {
        Self {
            inner: DeclarationKind::Block(declarations),
            line,
        }
    }

    pub fn if_statement(
        condition: Expr,
        then_branch: Box<Declaration>,
        else_branch: Option<Box<Declaration>>,
        line: usize,
    ) -> Self {
        Self {
            inner: DeclarationKind::Statement(Statement::IfStatement {
                condition,
                then_branch,
                else_branch,
            }),
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
    Block(Vec<Declaration>),
}

#[derive(Debug, Clone)]
pub enum Statement {
    ExprStatement(Expr),
    PrintStatement(Expr),
    IfStatement {
        condition: Expr,
        then_branch: Box<Declaration>,
        else_branch: Option<Box<Declaration>>,
    },
}
