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

    /// Unary expression
    Unary {
        op: Operator,
        opr: AstBox
    },

    // Function call
    FunCall {
        name: String,
        args: Vec<AstBox>,
    },

    Int {
        val: i32,
    },

    Id {
        id: String,
    },

    End {
        val: String,
    }
}

pub type AstBox = Box<Ast>;

pub trait AstVisitor {
    type Result;

    // Visits an AST
    fn visit(&mut self, ast: &AstBox) -> Self::Result {
        use Ast::*;
        match ast.as_ref() {
            Function { name, args, body } => self.visit_function(name, args, body),
            Block { stmts } => self.visit_block(stmts),
            Assign { name, expr } => self.visit_assign(name, expr),
            If { cond, then, else_then } => self.visit_if(cond, then, else_then),
            Repeat { do_this, until } => self.visit_repeat(do_this, until),
            While { do_this, while_case } => self.visit_while(do_this, while_case),
            Binary { op, lhs, rhs } => self.visit_binary(op, lhs, rhs),
            Unary { op, opr } => self.visit_unary(op, opr),
            FunCall { name, args } => self.visit_func_call(name, args),
            Int { val } => self.visit_int(val),
            Id { id } => self.visit_id(id),
            End { val } => self.visit_end(val)
        }
    }

    fn visit_function(&mut self, name: &String, args: &[String], body: &AstBox) -> Self::Result;
    fn visit_block(&mut self, stmts: &[AstBox]) -> Self::Result;
    fn visit_assign(&mut self, name: &String, expr: &AstBox) -> Self::Result;
    fn visit_if(&mut self, cond: &AstBox, then: &AstBox, else_then: &Option<AstBox>) -> Self::Result;
    fn visit_repeat(&mut self, do_this: &AstBox, until: &AstBox) -> Self::Result;
    fn visit_while(&mut self, do_this: &AstBox, while_case: &AstBox) -> Self::Result;
    fn visit_unary(&mut self, op: &Operator, opr: &AstBox) -> Self::Result;
    fn visit_binary(&mut self, op: &Operator, lhs: &AstBox, rhs: &AstBox) -> Self::Result;
    fn visit_func_call(&mut self, name: &String, args: &[AstBox]) -> Self::Result;
    fn visit_int(&mut self, val: &i32) -> Self::Result;
    fn visit_id(&mut self, val: &String) -> Self::Result;
    fn visit_end(&mut self, val: &String) -> Self::Result;
}

