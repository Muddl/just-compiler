use crate::define::{Ast, AstBox, AstVisitor};
use crate::define::{NestedMap, Operator};
use crate::unwrap_struct;
use lazy_static::lazy_static;
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

/// Interpreter for `Julia` language
pub struct Interpreter {
    /// Implementation of the evaluator.
    intp: InterpreterImpl,
}

/// `Result` for `Interpreter`
pub type Result = std::result::Result<i32, &'static str>;

impl Interpreter {
    /// Creates a new interpreter
    pub fn new() -> Self {
        Self {
            intp: InterpreterImpl {
                funcs: Rc::new(RefCell::new(HashMap::new())),
                envs: NestedMap::new(),
            },
        }
    }

    // Adds the specific function definition to the interpreter
    pub fn add_func_def(&mut self, func: AstBox) -> std::result::Result<(), &str> {
        match func.as_ref() {
            // Get function name
            Ast::Function { name, .. } => {
                // Check if already defined
                if !self.intp.funcs.borrow().contains_key(name) {
                    // Add function definition
                    self.intp.funcs.borrow_mut().insert(name.clone(), func);
                    Ok(())
                } else {
                    Err("Function has already been defined")
                }
            }

            _ => panic!("Not a function"),
        }
    }

    /// Evaluates the current program
    pub fn eval(&mut self) -> Result {
        // Find and evaluate the `main` function
        match self.intp.funcs.clone().borrow().get("a") {
            Some(a) => self.intp.visit(a),
            _ => Err("Entry function 'a' not found"),
        }
    }
}

/// Implementation of the interpreter
struct InterpreterImpl {
    /// All function definitions
    funcs: Rc<RefCell<HashMap<String, AstBox>>>,
    /// Environments
    envs: NestedMap<String, i32>,
}

lazy_static! {
    /// Name of return value when evaluating
    static ref RET_VAL: String = "$ret".to_string();
}

impl InterpreterImpl {
    /// Performs library function call
    fn call_lib_func(
        &mut self,
        name: &str,
        args: &[AstBox],
    ) -> std::result::Result<Option<i32>, &'static str> {
        match name {
            "input" => {
                // Check arguments
                if !args.is_empty() {
                    Err("Argument count mismatch")
                } else {
                    // Read integer from stdin
                    let mut line = String::new();
                    std::io::stdin()
                        .read_line(&mut line)
                        .expect("Failed to read from stdin");
                    match line.trim().parse::<i32>() {
                        Ok(ret) => Ok(Some(ret)),
                        _ => Err("Invalid input, expected integer"),
                    }
                }
            }
            "print" => {
                // Check arguments
                if args.len() != 1 {
                    Err("Argument count mismatch")
                } else {
                    // Evaluate argument
                    let arg = self.visit(args.first().unwrap())?;
                    // Print to stdout
                    println!("{}", arg);
                    Ok(Some(0))
                }
            }
            // Not a library function call
            _ => Ok(None),
        }
    }
}

impl AstVisitor for InterpreterImpl {
    type Result = Result;

    fn visit_function(&mut self, _name: &String, _args: &[String], body: &AstBox) -> Self::Result {
        // Set up the default return value
        let ret = self.envs.add(RET_VAL.clone(), 0);
        debug_assert!(ret, "Environment corrupted");
        // Evaluate function body
        self.visit(body)?;
        // Get return value
        Ok(*self.envs.get(&RET_VAL, false).unwrap())
    }

    fn visit_block(&mut self, stmts: &[AstBox]) -> Self::Result {
        // Enter a new environment
        self.envs.push();
        // Evaluate all statements
        for stmt in stmts {
            self.visit(stmt)?;
        }
        // Exit the current environment
        self.envs.pop();
        Ok(0)
    }

    fn visit_assign(&mut self, name: &String, expr: &AstBox) -> Self::Result {
        // Evaluate the expression
        let expr = self.visit(expr)?;
        // Update value of the symbol
        self
            .envs
            .update_until(name, expr, |map| map.contains_key::<String>(&RET_VAL))
            .then(|| 0)
            .ok_or("Symbol has not been defined")
    }

    fn visit_if(&mut self, cond: &AstBox, then: &AstBox, else_then: &Option<AstBox>) -> Self::Result {
        // Evaluate the condition
        let cond = self.visit(cond)?;
        // Evaluate true/false branch
        if cond != 0 {
            self.visit(then)
        } else {
            else_then.as_ref().map_or(Ok(0), |ast| self.visit(&ast))
        }
    }

    fn visit_repeat(&mut self, do_this: &AstBox, until: &AstBox) -> Self::Result {
        // Evaluate the condition
        let cond = self.visit(until)?;
        // Evaluate true/false branch and loop as necessary
        while (cond != 0) {
            self.visit(do_this)?;
            self.visit_repeat(do_this, until)?;
        } {}
        Ok(0)
    }

    fn visit_while(&mut self, do_this: &AstBox, while_case: &AstBox) -> Self::Result {
        // Evaluate the condition
        let cond = self.visit(while_case)?;
        // Evaluate true/false branch and loop as necessary
        while (cond != 0) {
            self.visit(do_this)?;
            self.visit_while(do_this, while_case)?;
        }
        Ok(0)
    }

    fn visit_unary(&mut self, op: &Operator, opr: &AstBox) -> Self::Result {
        // Evaluate the operand
        let opr = self.visit(opr)?;
        // Perform unary operation
        Ok(match *op {
            Operator::Sub => -opr,
            Operator::LNot => !opr,
            _ => panic!("invalid unary operator"),
        })
    }

    fn visit_binary(&mut self, op: &Operator, lhs: &AstBox, rhs: &AstBox) -> Self::Result {
        match *op {
            _ => {
                // Evaluate the lhs & rhs
                let lhs = self.visit(lhs)?;
                let rhs = self.visit(rhs)?;
                // Perform binary operation
                Ok(match *op {
                    Operator::Add => lhs + rhs,
                    Operator::Sub => lhs - rhs,
                    Operator::Mul => lhs * rhs,
                    Operator::Div => lhs / rhs,
                    Operator::Great => (lhs > rhs) as i32,
                    Operator::GreatEq => (lhs >= rhs) as i32,
                    Operator::Less => (lhs < rhs) as i32,
                    Operator::LessEq => (lhs <= rhs) as i32,
                    Operator::Eq => (lhs == rhs) as i32,
                    Operator::NotEq => (lhs != rhs) as i32,
                    _ => panic!("Unknown binary operator"),
                })
            }
        }
    }

    fn visit_funcall(&mut self, name: &String, args: &[AstBox]) -> Self::Result {
        // Handle library function call
        if let Some(ret) = self.call_lib_func(name, args)? {
            return Ok(ret);
        }
        // Find the specific function
        match self.funcs.clone().borrow().get(name) {
            Some(func) => {
                // Make a new environment for arguments
                self.envs.push();
                // Evaluate arguments
                let (_, arg_names, _) = unwrap_struct!(func.as_ref(), Ast::Function, name, args, body);
                if arg_names.len() != args.len() {
                    return Err("Argument count mismatch");
                }
                for (arg, name) in args.iter().zip(arg_names.iter()) {
                    // Evaluate the current arguments
                    let arg = self.visit(arg)?;
                    // Add to the current environment
                    if !self.envs.add(name.clone(), arg) {
                        return Err("Redefinition of argument");
                    }
                }
                // Call the specific function
                let ret = self.visit(func);
                // Exit the current environment
                self.envs.pop();
                ret
            }
            None => Err("Function not found"),
        }
    }

    fn visit_int(&mut self, val: &i32) -> Self::Result {
        Ok(*val)
    }
    
    fn visit_id(&mut self, val: &String) -> Self::Result {
        // Find in environment
        self
            .envs
            .get_rec(val)
            .map_or(Err("Symbol has not been defined"), |v| Ok(*v))
    }
}