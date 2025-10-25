use crate::expr::Expr;

#[derive(Debug, Clone, PartialEq)]
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

    pub fn while_statement(condition: Expr, body: Box<Declaration>, line: usize) -> Self {
        Self {
            inner: DeclarationKind::Statement(Statement::WhileStatement { condition, body }),
            line,
        }
    }

    pub fn for_statement(
        initializer_clause: Option<Declaration>,
        condition_clause: Option<Expr>,
        increment_clause: Option<Expr>,
        body: Declaration,
        line: usize,
    ) -> Self {
        Self {
            inner: DeclarationKind::Statement(Statement::ForStatement {
                initializer_clause: Box::new(initializer_clause),
                condition_clause,
                increment_clause,
                body: Box::new(body),
            }),
            line,
        }
    }

    pub fn function_declaration(
        name: String,
        parameters: Vec<String>,
        body: Declaration,
        line: usize,
    ) -> Self {
        Self {
            inner: DeclarationKind::Statement(Statement::FunctionDeclaration {
                name,
                parameters,
                body: Box::new(body),
            }),
            line,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum DeclarationKind {
    VariableDeclaration {
        name: String,
        initializer: Option<Expr>,
    },
    Statement(Statement),
    Block(Vec<Declaration>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Statement {
    ExprStatement(Expr),
    PrintStatement(Expr),
    IfStatement {
        condition: Expr,
        then_branch: Box<Declaration>,
        else_branch: Option<Box<Declaration>>,
    },
    WhileStatement {
        condition: Expr,
        body: Box<Declaration>,
    },
    ForStatement {
        initializer_clause: Box<Option<Declaration>>,
        condition_clause: Option<Expr>,
        increment_clause: Option<Expr>,
        body: Box<Declaration>,
    },
    FunctionDeclaration {
        name: String,
        parameters: Vec<String>,
        body: Box<Declaration>,
    },
}
