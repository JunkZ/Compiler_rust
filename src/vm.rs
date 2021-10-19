
use crate::ast::{Block, Expr, FnDeclaration, Literal, Op, Prog, Statement, UnOp};
use crate::common::Eval;
use crate::env::{Env, Ref};
use crate::error::Error;

#[derive(Debug, Clone, PartialEq)]
pub enum Val {
    Lit(Literal),
    Ref(Ref),
    UnInit,
    Mut(Box<Val>),
}

use std::collections::HashMap;
use std::collections::VecDeque;

#[derive(Debug)]
pub enum VmErr {
    Err(String),
}

pub type VarEnv = VecDeque<HashMap<String, Literal>>;

// Helpers for Val
// Alternatively implement the TryFrom trait
impl Val {
    pub fn get_bool(&self) -> Result<bool, Error> {
        match self {
            Val::Lit(Literal::Bool(b)) => Ok(*b),
            _ => Err(format!("cannot get Bool from {:?}", self)),
        }
    }

    pub fn get_int(&self) -> Result<i32, Error> {
        match self {
            Val::Lit(Literal::Int(i)) => Ok(*i),
            _ => Err(format!("cannot get integer from {:?}", self)),
        }
    }
}

// Helper for Op
impl Op {
    // Evaluate operator to literal
    pub fn eval(&self, left: Val, right: Val) -> Result<Val, Error> {
        use Literal::{Bool, Int};
        match self {
            Op::Add => Ok(Val::Lit(Int(left.get_int()? + right.get_int()?))),
            Op::Sub => Ok(Val::Lit(Int(left.get_int()? - right.get_int()?))),
            Op::Mul => Ok(Val::Lit(Int(left.get_int()? * right.get_int()?))),
            Op::Div => Ok(Val::Lit(Int(left.get_int()? / right.get_int()?))),
            Op::And => Ok(Val::Lit(Bool(left.get_bool()? && right.get_bool()?))),
            Op::Or => Ok(Val::Lit(Bool(left.get_bool()? || right.get_bool()?))),
            Op::Eq => Ok(Val::Lit(Bool(left == right))), // overloading
            Op::Lt => Ok(Val::Lit(Bool(left.get_int()? < right.get_int()?))),
            Op::Gt => Ok(Val::Lit(Bool(left.get_int()? > right.get_int()?))),
        }
    }
}

impl Eval<Val> for Block {
    fn eval(&self, env: &mut Env<Val>) -> Result<(Val, Option<Ref>), Error> {
        let mut return_val = (Val::Lit(Literal::Unit),None);
        env.v.push_scope();
        for be in &self.statements {
            match be {
                Statement::Let(_,id, _, e) => {
                    match e {
                        Some(e) => {
                            let l = e.eval(env)?;
                            let r =env.v.alloc(id, l.clone().0);
                            return_val=(l.0,Some(r));
                        },
                        None => {
                            let r = env.v.alloc(id, Val::UnInit);
                            return_val = (Val::Lit(Literal::Unit),Some(r))
                        },
                    }
                }
                Statement::Assign(id, e) => {
                    let expr_e = e.eval(env)?;
                    match id.eval(env).unwrap() {
                        (Val::Lit(_), None) => Err("Mismatch assignment")?,
                        (Val::Lit(_), Some(r)) => env.v.set_ref(r, expr_e.0),
                        (Val::Ref(_), None) => Err("Mismatch assignment")?,
                        (Val::Ref(_), Some(r)) => env.v.set_ref(r, expr_e.0),
                        (Val::UnInit, None) => Err("Mismatch assignment")?,
                        (Val::UnInit, Some(r)) => env.v.set_ref(r, expr_e.0),
                        (Val::Mut(_), None) => Err("Mismatch assignment")?,
                        (Val::Mut(_), Some(r)) => env.v.set_ref(r, expr_e.0),
                    }
                }
                Statement::Expr(e) => {
                    return_val = e.eval(env).unwrap();
                },
                Statement::While(c, block) => {
                    while c.eval(env).unwrap().0.get_bool().unwrap() {
                        block.eval(env).unwrap();
                    }
                },
                Statement::Fn(f) => {return_val = f.eval(env)?},
            }
        }
        env.v.pop_scope();
        match self.semi {
            true => Ok((Val::Lit(Literal::Unit),None)),
            false => Ok(return_val),
        }

    }
}
impl Eval<Val> for FnDeclaration {
    fn eval(&self, env: &mut Env<Val>) -> Result<(Val, Option<Ref>), Error> {
        let l_env = env.clone();
        let mut params = vec![];
        for p in self.parameters.0.iter() {
            let par = p.clone();
            if params.contains(&par) {
                return Err("Parameter already exists".to_string());
            } else {
                params.push(par)
            }
        }
        if params.len() != self.parameters.0.len() {
            return Err("Number of parameters mismatch".to_string());
        }
        return Ok(self.body.eval(env)?);
    }
}
impl Eval<Val> for Expr {
    fn eval(&self, env: &mut Env<Val>) -> Result<(Val, Option<Ref>), Error> {
        match self {       
            Expr::Ident(id) => match env.v.get(id).clone() {
                Some(t) => {
                    Ok((t, env.v.get_ref(id)))
                }
                None => { 
                    Err("variable not found".to_string())
                }
            },
            Expr::Lit(literal) => Ok((Val::Lit(literal.clone()),None)),
            Expr::BinOp(op, left, right) => Ok((op.eval(left.eval(env).unwrap().0, right.eval(env).unwrap().0)?,None)),
            Expr::Par(e) => e.eval(env),
            Expr::IfThenElse(c, t, e) => match c.eval(env)?.0.get_bool()? {
                true => (*t).eval(env),
                false => match e {
                    Some(e) => e.eval(env),
                    None => Ok((Val::UnInit,None)),
                },
            },
            Expr::Call(id, args) => {
                 match env.clone().f.0.get(id) {
                    Some(f) => {
                        //println!("f is {:?}",f);
                        if f.0.id == "print" {
                            return Ok((Val::Lit(Literal::Unit),None))
                        }
                        if args.0.len() != f.0.parameters.0.clone().len() {
                            return Err("Mismatch number of args and parameters".to_string());
                        } else {
                            let i = 0;
                            for par in &f.0.parameters.0 {
                                //type vs val ugly fix
                                let x = args.0.get(i).unwrap().eval(env)?.0.clone();
                                let h= x.get_int()?.to_string();
                                let y =par.ty.to_string();
                                if !y.contains(&h) {
                                    return Err("Parameter mismatch arg type!".to_string())
                                }

                            }
                            return Ok((Val::UnInit,None))
                        }
                    },
                    None => return Err("No function!".to_string()),
                }

            },
            Expr::Block(b) => b.eval(env),
            Expr::UnOp(op, b) => match op {
                UnOp::Ref => {
                    let e_eval = b.eval(env);
                    match e_eval.clone()?.1 {
                        Some(asg) => {
                            Ok((Val::Ref(asg),Some(asg)))
                        },
                        None => {
                            Ok((Val::Ref(env.v.stack_val(e_eval?.0)),None))
                        },
                    }
                },

                UnOp::DeRef => {
                    match b.eval(env)?.0 {
                        //Val::Lit(_) => todo!(),
                        //Val::UnInit => todo!(),
                        //Val::Mut(_) => todo!(),
                        Val::Ref(r) => {
                            Ok((env.v.de_ref(r), Some(r)))
                    },
                    Val::Lit(_) => Err("Derefrencing a non reference".to_string()),
                    Val::UnInit => Err("Derefrencing a non reference".to_string()),
                    Val::Mut(_) => Err("Derefrencing a non reference".to_string()),
                        
                }
                },
                UnOp::Mut => {
                    Ok((Val::Mut(Box::new(b.eval(env)?.0)),None))
                },
                UnOp::Bang => {
                    Ok((Val::Lit(Literal::Bool(!b.eval(env)?.0.get_bool()?)),None))
            }, 
        }
    }
}
}

impl Eval<Val> for Prog {
    fn eval(&self, env: &mut Env<Val>) -> Result<(Val, Option<Ref>), Error> {
        let _ = env.f.add_functions_unique(self.0.clone());
        let mut decl_ok = (Val::Lit(Literal::Unit),None);
        for func in &self.0 {
            if func.eval(env).is_ok(){
                decl_ok = func.eval(env)?;
            } else {
                return Err("Invalid function declaration".to_string())
            }
        }
        return  Ok(decl_ok);
    }
}
    
#[cfg(test)]
mod tests {
    use super::Val;
    use crate::ast::{Block, Prog, Literal};
    use crate::common::parse_test;

    #[test]
    fn test_block_let() {
        let v = parse_test::<Block, Val>(
            "
    {
        let a: i32 = 1;
        let b: i32 = 2;

        a + b
    }",
        );
        assert_eq!(v.unwrap().get_int().unwrap(), 3);
    }

    #[test]
    fn test_block_let_shadow() {
        let v = parse_test::<Block, Val>(
            "
    {
        let a: i32 = 1;
        let b: i32 = 2;
        let a: i32 = 3;
        let b: i32 = 4;

        a + b
    }",
        );
        assert_eq!(v.unwrap().get_int().unwrap(), 7);
    }

    #[test]
    fn test_block_assign() {
        let v = parse_test::<Block, Val>(
            "
    {
        let mut a: i32 = 1;
        a = a + 2;
        a
    }",
        );
        assert_eq!(v.unwrap().get_int().unwrap(), 3);
    }

    #[test]
    fn test_expr_if_then_else() {
        let v = parse_test::<Block, Val>(
            "
    {
        let mut a: i32 = 1;
        a = if a > 0 { a + 1 } else { a - 2 };
        a
    }",
        );

        assert_eq!(v.unwrap().get_int().unwrap(), 2);
    }

    #[test]
    fn test_expr_if_then_else2() {
        let v = parse_test::<Block, Val>(
            "
    {
        let mut a: i32 = 1;
        a = if a < 0 { a + 1 } else { a - 2 };
        a
    }",
        );

        assert_eq!(v.unwrap().get_int().unwrap(), -1);
    }

    #[test]
    fn test_ref_deref() {
        let v = parse_test::<Block, Val>(
            "
    {
        let a = 1;
        let b = &a;
        *b
    }
    ",
        );

        assert_eq!(v.unwrap().get_int().unwrap(), 1);
    }

    #[test]
    fn test_ref_deref_indirect() {
        let v = parse_test::<Block, Val>(
            "
    {
        let a = 1;
        let b = &a;
        let c = b;
        *c
    }
    ",
        );

        assert_eq!(v.unwrap().get_int().unwrap(), 1);
    }

    #[test]
    fn test_deref_assign() {
        let v = parse_test::<Block, Val>(
            "
    {
        let a = 1;
        let b = &a;
        *b = 7;
        a
    }
    ",
        );

        assert_eq!(v.unwrap().get_int().unwrap(), 7);
    }

    #[test]
    fn test_while() {
        let v = parse_test::<Block, Val>(
            "
    {
        let a = 2;
        let b = 0;
        while a > 0 {
            a = a - 1;
            b = b + 1;
        }
        b
    }
    ",
        );

        assert_eq!(v.unwrap().get_int().unwrap(), 2);
    }

    #[test]
    fn test_while_ref() {
        let v = parse_test::<Block, Val>(
            "
    {
        let a = 2;
        let b = 0;
        let c = &b;
        while a > 0 {
            a = a - 1;
            *c = (*c) + 1;
        }
        *c
    }
    ",
        );

        assert_eq!(v.unwrap().get_int().unwrap(), 2);
    }

    #[test]
    fn test_while_ref2() {
        let v = parse_test::<Block, Val>(
            "
    {
        let a = 2;
        let b = 0;
        let c = &b;
        let d = &a;
        
        while (*d) > 0 {
            *d = (*d) - 1;
            *c = (*c) + 1;
        }
        *c
    }
    ",
        );

        assert_eq!(v.unwrap().get_int().unwrap(), 2);
    }

    #[test]
    fn test_bool() {
        let v = parse_test::<Block, Val>(
            "
    {
        let a = true && false;
        a
    }
    ",
        );

        assert_eq!(v.unwrap().get_bool().unwrap(), false);
    }

    #[test]
    fn test_bool_bang() {
        let v = parse_test::<Block, Val>(
            "
    {
        let a = true && !false;
        a
    }
    ",
        );

        assert_eq!(v.unwrap().get_bool().unwrap(), true);
    }

    #[test]
    fn test_bool_bang2() {
        let v = parse_test::<Block, Val>(
            "
    {
        let a = (!true) && false;
        a
    }
    ",
        );

        assert_eq!(v.unwrap().get_bool().unwrap(), false);
    }

    #[test]
    fn test_local_block() {
        let v = parse_test::<Block, Val>(
            "
    {
        let a = 1;
        { 
            let b = &a;
            *b = 2;
        };
        a
    }
    ",
        );

        assert_eq!(v.unwrap().get_int().unwrap(), 2);
    }

    #[test]
    fn test_local_block_assign() {
        let v = parse_test::<Block, Val>(
            "
    {
        let a = 6;
        let b = { 
            let b = &a;
            *b = (*b) + 1;
            *b
        };
        b
    }
    ",
        );

        assert_eq!(v.unwrap().get_int().unwrap(), 7);
    }

    #[test]
    fn test_prog() {
        let v = parse_test::<Prog, Val>(
            "
    fn main() {
        let a = 1;
        a
    }
    ",
        );

        assert_eq!(v.unwrap().get_int().unwrap(), 1);
    }

    #[test]
     //I add the func to fnenv, and print parse tesat works so idk
    fn test_local_fn() {
        let v = parse_test::<Prog, Val>(
            "
    fn main() {
        fn f(i: i32, j: i32) -> i32 {
            i + j
        }
        let a = f(1, 2);
        println!(\"a = {} and another a = {}\", a, a);
    }
    ",
        );

        assert_eq!(v.unwrap(), Val::Lit(Literal::Unit));
    }  
    
    #[test]
    fn test_check_if_then_else_shadowing() {
        let v = parse_test::<Block, Val>(
            "
        {
            let a: i32 = 1 + 2; // a == 3
            let a: i32 = 2 + a; // a == 5
            if true {
                a = a - 1;      // outer a == 4
                let a: i32 = 0; // inner a == 0
                a = a + 1       // inner a == 1
            } else {
                a = a - 1
            };
            a   // a == 4
        }
        ",
        );

        assert_eq!(v.unwrap().get_int().unwrap(), 4);
    }
    #[test]
    fn test_ref() {
        let v = parse_test::<Block, Val>(
            "
        {
            let a = &1;
            *a
        }
        ",
        );
        assert_eq!(v.unwrap().get_int().unwrap(), 1);
    }
}
