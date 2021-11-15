use crate::collection;
use crate::define::{AstBox, AstVisitor, NestedMap, Operator};
use crate::define::{FunDefRc, FunctionDef, Inst, ValRc, Value}; // IR define file TODO
use std::cell::{RefCell, RefMut};
use std::collections::HashMap;
use std::io;
use std::rc::Rc;

/// Compiler
pub struct Compiler {
    gen: Generator,
}

/// `Result` for `Compiler`
pub type Result = std::result::Result<Option<ValRc>, &'static str>;

impl Compiler {
    /// Creates a new `Compiler`
    pub fn new() -> Self {
        Self {
            gen: Generator {
                func: None,
                funcs: collection! {
                    "input".to_string() => Rc::new(RefCell::new(FunctionDef::new_lib("input".to_string(), 0))),
                    "print".to_string() => Rc::new(RefCell::new(FunctionDef::new_lib("print".to_string(), 1))),
                },
                vars: NestedMap::new(),
                label_id: 0,
            },
        }
    }

    /// Compiles the specific AST
    pub fn compile(&mut self, ast: AstBox) -> Result {
        self.gen.visit(&ast)
    }

    /// Dumps RISC-V assembly of all compiled ASTs
    pub fn dump(&self, writer: &mut impl io::Write) -> io::Result<()> {
        for func in self.gen.funcs.values().filter(|f| !f.borrow().is_lib()) {
            func.borrow().dump(writer)?;
        }
        Ok(())
    }
}

/// IR Generator
struct Generator {
    /// Current function
    func: Option<FunDefRc>,

    /// All defined functions
    funcs: HashMap<String, FunDefRc>,

    /// All defined variables
    vars: NestedMap<String, ValRc>,

    /// Current label id
    label_id: usize,
}

impl Generator {
    fn new_label(&mut self) -> ValRc {
        let label = Rc::new(Value::Label { id: self.label_id });
        self.label_id += 1;
        label
    }

    fn func(&self) -> RefMut<'_, FunctionDef> {
        self.func.as_ref().unwrap().borrow_mut()
    }
}

impl AstVisitor for Generator {
    type Result = Result;

    fn visit_function(&mut self, name: &String, args: &[String], body: &AstBox) -> Self::Result {
        // Check argument count
        (args.len() <= 8)
            .then(|| ())
            .ok_or("Argument count must be less than or equal to 8")?;
        // Create function definition IR
        let func = Rc::new(RefCell::new(FunctionDef::new(name.clone(), args.len())));
        self.func = Some(func.clone());
        // Add to function map
        self
            .funcs
            .insert(name.clone(), func)
            .map_or(Ok(()), |_| Err("Function has already been defined"))?;
        // Enter argument environment
        self.vars.push();
        // Add definitions of arguments
        for (i, arg) in args.iter().enumerate() {
            self.vars.add(arg.clone(), Rc::new(Value::ArgRef { id: i }));
        }
        // Generate body
        self.visit(body)?;
        // Exit argument environment
        self.vars.pop();
        Ok(None)
    }

    fn visit_block(&mut self, stmts: &[AstBox]) -> Self::Result {
        // Enter a new environment
        self.vars.push();
        // Generate on all statements
        for stmt in stmts {
            self.visit(stmt)?;
        }
        // Exit the current environment
        self.vars.pop();
        Ok(None)
    }

    fn visit_assign(&mut self, name: &String, expr: &AstBox) -> Self::Result {
        // Generate expression
        let expr = self.visit(expr)?.unwrap();
        // Get stack slot of the symbol
        let slot = self
            .vars
            .get_rec(name)
            .ok_or("Symbol has not been defined")?;
        // Generate assign instruction
        self.func().push_inst(Box::new(Inst::Assign {
            dest: slot.clone(),
            val: expr,
        }));
        Ok(None)
    }

    fn visit_if(&mut self, cond: &AstBox, then: &AstBox, else_then: &Option<AstBox>) -> Self::Result {
        // Generate condition
        let cond = self.visit(cond)?.unwrap();
        // Create labels
        let false_branch = self.new_label();
        let end_if = else_then.is_some().then(|| self.new_label());
        // Generate conditional branch
        let branch = Box::new(Inst::BranchEqz {
            cond: cond,
            label: false_branch.clone(),
        });
        self.func().push_inst(branch);
        // Generate the true branch
        self.visit(then)?;
        else_then.is_some().then(|| {
            self.func().push_inst(Box::new(Inst::Jump {
                label: end_if.clone().unwrap(),
            }))
        });
        // Generate the false branch
        self.func().push_inst(Box::new(Inst::Label {
            label: false_branch,
        }));
        else_then.as_ref().map_or(Ok(None), |ast| {
            self.visit(ast)?;
            self.func().push_inst(Box::new(Inst::Label {
                label: end_if.unwrap(),
            }));
            Ok(None)
        })
    }

    fn visit_repeat(&mut self, do_this: &AstBox, until: &AstBox) -> Self::Result {
        // Figure this boy out yourself TODO
        Ok(None)
    }

    fn visit_while(&mut self, do_this: &AstBox, while_case: &AstBox) -> Self::Result {
        Ok(None)
    }

    fn visit_binary(&mut self, op: &Operator, lhs: &AstBox, rhs: &AstBox) -> Self::Result {
        // Check if is logical operator
        if *op == Operator::LAnd || *op == Operator::LOr {
            // Logical AND operation, generate labels
            let end_logic = self.new_label();
            // Generate lhs first
            let lhs = self.visit(lhs)?.unwrap();
            // Generate conditional branch
            self.func().push_inst(Box::new(if *op == Operator::LAnd {
                Inst::BranchEqz {
                    cond: lhs.clone(),
                    label: end_logic.clone(),
                }
            } else {
                Inst::BranchNez {
                    cond: lhs.clone(),
                    label: end_logic.clone(),
                }
            }));
            // Generate rhs
            let rhs = self.visit(rhs)?.unwrap();
            self.func().push_inst(Box::new(Inst::Assign {
                dest: lhs.clone(),
                val: rhs,
            }));
            // Generate label definition
            self
                .func()
                .push_inst(Box::new(Inst::Label { label: end_logic }));
            Ok(Some(lhs))
            } else {
                // Generate lhs & rhs
                let lhs = self.visit(lhs)?.unwrap();
                let rhs = self.visit(rhs)?.unwrap();
                // Generate binary operation
                let dest = self.func().add_slot();
                self.func().push_inst(Box::new(Inst::Binary {
                    dest: dest.clone(),
                    op: op.clone(),
                    lhs: lhs,
                    rhs: rhs,
            }));
            Ok(Some(dest))
        }
    }

    fn visit_unary(&mut self, op: &Operator, opr: &AstBox) -> Self::Result {
        // Generate operand
        let opr = self.visit(opr)?.unwrap();
        // Generate unary operation
        let dest = self.func().add_slot();
        self.func().push_inst(Box::new(Inst::Unary {
            dest: dest.clone(),
            op: op.clone(),
            opr: opr,
        }));
        Ok(Some(dest))
    }

    fn visit_funcall(&mut self, name: &String, args: &[AstBox]) -> Self::Result {
        // Get the function definition
        let func = self.funcs.get(name).ok_or("function not found")?.clone();
        // Check argument count
        (args.len() == func.borrow().arg_num())
            .then(|| ())
            .ok_or("argument count mismatch")?;
        // Generate arguments
        let args: std::result::Result<Vec<_>, _> = args
            .iter()
            .map(|ast| self.visit(ast).map(|v| v.unwrap()))
            .collect();
        // Generate function call
        let dest = self.func().add_slot();
        self.func().push_inst(Box::new(Inst::Call {
            dest: dest.clone(),
            func: Rc::downgrade(&func),
            args: args?,
        }));
        Ok(Some(dest))
    }

    fn visit_int(&mut self, val: &i32) -> Self::Result {
        Ok(Some(Rc::new(Value::Integer { val: *val })))
    }
    
    fn visit_id(&mut self, val: &String) -> Self::Result {
        // Get stack slot of the symbol
        Ok(Some(
            self
                .vars
                .get_rec(val)
                .ok_or("Symbol has not been defined")?
                .clone(),
        ))
    }
}