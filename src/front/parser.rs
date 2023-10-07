use super::lexer::Lexer;
use crate::define;
use define::{Ast, AstBox, Keyword, Operator, Token};
use std::io::Read;

/// Parser for `Julia` language.
pub struct Parser<T: Read> {
    lexer: Lexer<T>,
    cur_token: super::lexer::Result,
}

/// Error information of `Parser`
#[derive(Debug)]
pub enum Error {
    /// End of parsing process
    End,
    /// Parser error
    Error(String),
}

/// `Result` for parser functions of `Parser`
pub type Result = std::result::Result<AstBox, Error>;

impl<T:Read> Parser<T> {
    /// Creates a new `Parser` object from the specific `Lexer`
    pub fn new(lexer: Lexer<T>) -> Self {
        let mut parser = Self {
            lexer,
            cur_token: Ok(Token::End),
        };
        parser.next_token();
        parser
    }

    /// Parses the next AST.
    pub fn parse_next(&mut self) -> Result {
        match &self.cur_token {
            Ok(Token::End) => Err(Error::End),
            Ok(_) => self.parse_function(),
            Err(err) => Err(Error::Error(err.clone())),
        }
    }

    /// Gets the next token and returns it
    fn next_token(&mut self) {
        self.cur_token = self.lexer.next_token();
    }

    /// Parses function definitions
    fn parse_function(&mut self) -> Result {
        self.next_token();
        // Get function name
        let name = self.expect_id()?;
        // Check & eat '()'
        self.expect_delim()?;
        // // Get formal arguments
        let mut args = Vec::new();
        // if !self.is_token_char(')') {
        //     loop {
        //         // Get name of the current argument
        //         args.push(self.expect_id()?);
        //         // eat ','
        //         if !self.is_token_char(',') {
        //             break;
        //         }
        //         self.next_token();
        //     }
        // }
        // // Check & eat ')'
        // println!("Making it to this point");
        // self.expect_delim()?;
        // Get function body
        self.parse_block().map(|body| {
            Box::new(Ast::Function {
                name,
                args,
                body,
            })
        })
    }

    /// Parses block
    fn parse_block(&mut self) -> Result {
        // Get statments
        let mut stmts = Vec::new();
        while !self.is_token_key(Keyword::End) {
            stmts.push(self.parse_statement()?);
        }
        Ok(Box::new(Ast::Block { stmts }))
    }

    /// Parse statements
    fn parse_statement(&mut self) -> Result {
        println!("{:?}", &self.cur_token);
        match &self.cur_token {
            Ok(Token::Id(id)) => {
                let id = id.to_string();
                self.parse_assign(id)
            }
            Ok(Token::Key(Keyword::If)) => self.parse_if_else(),
            Ok(Token::Key(Keyword::While)) => self.parse_while(),
            Ok(Token::Key(Keyword::Print)) => self.parse_print(),
            Ok(Token::Key(Keyword::Repeat)) => self.parse_repeat(),
            Ok(Token::Key(Keyword::End)) => self.parse_end(),
            _ => Self::get_error("Invalid statement"),
        }
    }

    /// Parses assign statements
    fn parse_assign(&mut self, id: String) -> Result {
        // Eat id
        self.next_token();
        // Check if is a function call
        if self.is_token_char('(') {
            return self.parse_func_call(id);
        }
        // Check if is assign
        if !self.is_token_op(Operator::Assign) {
            return Self::get_error("Expected '='");
        }
        self.next_token();
        // Get expression
        self.parse_expr().map(|expr| {
            Box::new(Ast::Assign {
                name: id,
                expr,
            })
        })
    }

    /// Parses if-else statements
    fn parse_if_else(&mut self) -> Result {
        // Eat 'if'
        self.next_token();
        // Get condition
        let cond = self.parse_expr()?;
        // Get 'then' body
        let then = self.parse_block()?;
        // Check & get 'else-then' body
        Ok(Box::new(Ast::If {
            cond,
            then,
            else_then: if self.is_token_key(Keyword::Else) {
                // Eat 'else'
                self.next_token();
                // Parse 'if' or block of 'else'
                Some(if self.is_token_key(Keyword::If) {
                    self.parse_if_else()
                } else {
                    self.parse_block()
                }?)
            } else {
                None
            },
        }))
    }

    /// Parses while statement
    fn parse_while(&mut self) -> Result {
        // Eat 'while'
        self.next_token();
        // Get condition
        let cond = self.parse_expr()?;
        // Get 'do_this' body
        let do_this = self.parse_block()?;
        // Check loop condition and follow through if so
        Ok(Box::new(Ast::While {
            do_this,
            while_case: cond,
        }))
    }
    
    /// Parses repeat statement
    fn parse_repeat(&mut self) -> Result {
        // Eat 'repeat'
        self.next_token();
        // Get 'do_this' body
        let do_this = self.parse_block()?;
        // Get condition
        let cond = self.parse_expr()?;
        // Check loop condition and follow through if so
        Ok(Box::new(Ast::Repeat {
            do_this,
            until: cond,
        }))
    }

    /// Parses print statement
    fn parse_print(&mut self) -> Result {
        // Eat 'while'
        self.next_token();
        // Eat first delim
        self.expect_delim()?;
        // Grab argument
        let mut arg = Vec::new();
        arg.push(Box::new(Ast::Id { id: self.expect_id()? }));
        // Eat end delim
        self.expect_delim()?;
        Ok(Box::new(Ast::FunCall {
            name: "print".to_string(),
            args: arg
        }))
    }

    /// Parses end statement
    fn parse_end(&mut self) -> Result {
        // Eat 'end'
        self.next_token();
        Ok(AstBox::new(Ast::End { val: "".to_string() }))
    }

    /// Parses expression
    fn parse_expr(&mut self) -> Result {
        let f = |p: &mut Parser<T>| p.parse_land_expr();
        self.parse_binary(f, &[Operator::LOr])
    }

    /// Parses logical AND expressions
    fn parse_land_expr(&mut self) -> Result {
        let f = |p: &mut Parser<T>| p.parse_eq_expr();
        self.parse_binary(f, &[Operator::LAnd])
    }

    /// Parses EQ expressions.
    fn parse_eq_expr(&mut self) -> Result {
        let f = |p: &mut Parser<T>| p.parse_rel_expr();
        self.parse_binary(f, &[Operator::Eq, Operator::NotEq])
    }

    /// Parses relation expressions.
    fn parse_rel_expr(&mut self) -> Result {
        let f = |p: &mut Parser<T>| p.parse_add_expr();
        self.parse_binary(f, &[Operator::Less, Operator::LessEq, Operator::Great, Operator::GreatEq])
    }

    /// Parses add/sub expressions.
    fn parse_add_expr(&mut self) -> Result {
        let f = |p: &mut Parser<T>| p.parse_mul_expr();
        self.parse_binary(f, &[Operator::Add, Operator::Sub])
    }

    /// Parses mul/div expressions.
    fn parse_mul_expr(&mut self) -> Result {
        let f = |p: &mut Parser<T>| p.parse_unary();
        self.parse_binary(f, &[Operator::Mul, Operator::Div])
    }
    
    /// Parses unary expressions.
    fn parse_unary(&mut self) -> Result {
        // Check if is unary expression
        if let Ok(Token::Op(op)) = &self.cur_token {
            let op = op.clone();
            self.next_token();
            // Check if is a valid unary operator
            match op {
                Operator::Sub | Operator::LNot => (),
                _ => return Self::get_error("Invalid unary operator"),
            }
            // Get operand
            self
                .parse_expr()
                .map(|expr| Box::new(Ast::Unary { op: op, opr: expr }))
        } else {
            self.parse_value()
        }
    }

    /// Parses values.
    fn parse_value(&mut self) -> Result {
        match &self.cur_token {
            Ok(Token::Int(int)) => {
                // Get integer value
                let val = *int;
                self.next_token();
                // Integer literal
                Ok(Box::new(Ast::Int { val: val }))
            }
            Ok(Token::Id(id)) => {
                // Eat id
                let id = id.to_string();
                self.next_token();
                // Check if is a function call
                if self.is_token_char('(') {
                    self.parse_func_call(id)
                } else {
                    Ok(Box::new(Ast::Id { id: id }))
                }
            }
            Ok(Token::Other(c)) if *c == '(' => {
                // Eat '('
                self.next_token();
                // Get expression
                let expr = self.parse_expr()?;
                // Check & eat ')'
                self.expect_char(')')?;
                Ok(expr)
            }
            _ => Self::get_error("Invalid value"),
        }
    }

    /// Parses function calls.
    fn parse_func_call(&mut self, id: String) -> Result {
        // Eat '('
        self.next_token();
        // Get arguments
        let mut args = Vec::new();
        if !self.is_token_char(')') {
            loop {
                // Get the current argument
                args.push(self.parse_expr()?);
                // Eat ','
                if !self.is_token_char(',') {
                    break;
                }
                self.next_token();
            }
        }
        // Check & eat ')'
        self.expect_char(')')?;
        Ok(Box::new(Ast::FunCall {
            name: id.to_string(),
            args,
        }))
    }

    /// Parses binary expression.
    fn parse_binary<F>(&mut self, parser: F, ops: &[Operator]) -> Result
    where
        F: Fn(&mut Parser<T>) -> Result,
    {
        // Get left-hand side expression
        let mut lhs = parser(self)?;
        // Get the rest things
        loop {
            // Stop if error
            let op = match self.is_token_ops(ops) {
                Some(op) => op,
                None => break,
            };
            self.next_token();
            // Get right-hand side expression
            let rhs = parser(self)?;
            // Update lhs
            lhs = Box::new(Ast::Binary {
                op: op,
                lhs: lhs,
                rhs: rhs,
            })
        }
        Ok(lhs)
    }

    /// Returns a parser error.
    fn get_error(message: &str) -> Result {
        Err(Error::Error(message.to_string()))
    }

    /// Expects an identifier from lexer.
    fn expect_id(&mut self) -> std::result::Result<String, Error> {
        println!("{:?}", &self.cur_token);
        if let Ok(Token::Id(id)) = &self.cur_token {
            let id = id.to_string();
            self.next_token();
            Ok(id)
        } else {
            Err(Error::Error("Expected identifier".to_string()))
        }
    }

    /// Expects the specific character from lexer.
    fn expect_char(&mut self, c: char) -> std::result::Result<(), Error> {
        println!("{:?}", &self.cur_token);
        if !self.is_token_char(c) {
            Err(Error::Error(format!("Expected '{}'", c)))
        } else {
            self.next_token();
            Ok(())
        }
    }

    /// Expects the specific delimiter from lexer.
    fn expect_delim(&mut self) -> std::result::Result<String, Error> {
        println!("{:?}", &self.cur_token);
        if let Ok(Token::Delim(delim)) = &self.cur_token {
            let delim = delim.to_string();
            self.next_token();
            Ok(delim)
        } else {
            Err(Error::Error("Expected delimiter".to_string()))
        }
    }

    /// Checks if the current token is the specific character.
    fn is_token_char(&self, c: char) -> bool {
        self
            .cur_token
            .as_ref()
            .map_or(false, |t| *t == Token::Other(c))
    }

    /// Checks if the current token is the specific operator.
    fn is_token_op(&self, op: Operator) -> bool {
        self
            .cur_token
            .as_ref()
            .map_or(false, |t| *t == Token::Op(op))
    }

    /// Checks if the current token is one of the specific operators.
    /// Returns the operator if matched.
    fn is_token_ops(&self, ops: &[Operator]) -> Option<Operator> {
        match &self.cur_token {
            Ok(Token::Op(op)) if ops.iter().find(|&x| *op == *x).is_some() => Some(op.clone()),
            _ => None,
        }
    }

    /// Checks if the current token is the specific keyword.
    fn is_token_key(&self, key: Keyword) -> bool {
        self
            .cur_token
            .as_ref()
            .map_or(false, |t| *t == Token::Key(key))
    }
}