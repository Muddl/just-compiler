use crate::define::Operator;

/// ASTs of `rust-lexer`
pub enum Ast {
    /// Function definition
    Function {
        name: String,
        args: Vec<String>,
        body: AstBox,
    },

    /// Statement block
    Block { stmts: Vec<AstBox> },

    /// Assign statement
    Assign { name: String, expr: AstBox },

    /// If-else statement
    If {
        cond: AstBox,
        then: AstBox,
        else_then: Option<AstBox>,
    },

    /// Repeat statement
    Repeat {
        do_this: AstBox,
        until: AstBox,
    },

    /// While statement
    While {
        do_this: AstBox,
        while_case: AstBox,
    },

    /// Binary boolean expression
    Binary {
        op: Operator,
        lhs: AstBox,
        rhs: AstBox,
    },

    // Function call
    FunCall {
        name: String,
        args: Vec<AstBox>,
    }
}

pub type AstBox = Box<Ast>;