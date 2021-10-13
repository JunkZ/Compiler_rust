
use crate::ast::{Block, Expr, FnDeclaration, Literal, Op, Prog, Statement};
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
                            let l = e.eval(env)?.0;
                            env.v.alloc(id, l);
                        },
                        None => {
                            env.v.alloc(id, Val::UnInit);
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
                    let condition = c.eval(env).unwrap().0.get_bool().unwrap();
                    while condition {
                        block.eval(env).unwrap();
                    }
                },
                Statement::Fn(_) => todo!(),
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
        //env.v.alloc(&self.id, self);
        println!("env in fndecl is {:?}",env);
        todo!()
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
            Expr::Call(_, _) => todo!(),
            Expr::Block(b) => b.eval(env),
            Expr::UnOp(_, _) => todo!(), 
        }
    }
}
impl Eval<Val> for Prog {
    fn eval(&self, env: &mut Env<Val>) -> Result<(Val, Option<Ref>), Error> {
        todo!("not implemented {:?}", self)
    }
}
/* impl Eval<Val> for Statement {
    fn eval(&self, env: &mut Env<Val>) -> Result<(Val, Option<Ref>), Error> {
        match self {
            Statement::Let(m,id, t, e) => {
                // let a: i32 = 5 + 2
                // for now just accept an ident
                //let f = self.clone();
                //if f.to_string().contains("a") & !f.to_string().contains("0") & !f.to_string().contains("false"){
                //    return Ok((Val::UnInit,None))
                //} 
                /* if id.contains("a") & !id.contains("0") & !id.contains("false"){
                    return Ok((Val::UnInit,None))
                } */
/*                 if t.unwrap() == Type::I32 {
                    return Ok((Val::Lit(e.unwrap()),None))
                } */
                //return check_expr(e, env);
                //return self.eval(env);

                return Ok((Val::UnInit,None))

            },
            Statement::Expr(e) => {
                //return check_expr(e, env);
                return  e.eval(env);
                // the type of an Expr is returned
            },
            Statement::Assign(id, e) => {
                // a = 5
                //let b = check_expr(e, env);
                let b = self.eval(env);
                let a = self.eval(env);
    
                //let a = check_expr(id, env);
                if a.is_err() {
                    return Ok((Val::UnInit,None))
                } else if b.is_err() {
                    return Ok((Val::UnInit,None))
                } else if a == b {
                    return Ok((Val::UnInit,None));
                } else {
                    return Err("types mismatch for assign".to_string())
                }

            },
            Statement::While(e, b) => { 
                /* let checkblock=b.eval(env);
                let check= e.eval(env);
                if unify(Ty::Lit(check.clone().unwrap().0),Ty::Lit(Type::Bool),None.unwrap()).is_err()  {
                    return check
                }
                else {
                    return checkblock;
                } */
                return Ok((Val::UnInit,None));
            },
            Statement::Fn(f) => { 
                return Ok((Val::UnInit,None))
            }
        }
    }
} */
    
#[cfg(test)]
mod tests {
    use super::Val;
    use crate::ast::Literal;
    use crate::ast::{Block, Prog};
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

    /* #[test]
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
    }*/

    #[test]
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
/* #[test]
fn test_check_block1() {
    let ts: proc_macro2::TokenStream = "
    {
        let a: i32 = 1 + 2; 
        a = a + 1; 
        a
    }
    "
    .parse()
    .unwrap();
    let bl: Block = syn::parse2(ts).unwrap();
    println!("bl {:?}", bl);
    let l = bl.eval(&mut VarEnv::new()).unwrap();
    println!("l {:?}", l);
    assert_eq!(l.get_int().unwrap(), 4);
}

#[test]
fn test_check_if_then_else() {
    let ts: proc_macro2::TokenStream = "
    {
        let a: i32 = 1 + 2; 
        if false { 
            a = a + 1 
        } else {
            a = a - 1
        };
        if true { 
            a = a + 3 
        };
        a
    }
    "
    .parse()
    .unwrap();
    let bl: Block = syn::parse2(ts).unwrap();
    println!("bl {:?}", bl);
    let l = bl.eval(&mut VarEnv::new()).unwrap();
    println!("l {:?}", l);
    assert_eq!(l.get_int().unwrap(), 5);
}

#[test]
fn test_check_if_then_else_shadowing() {
    let ts: proc_macro2::TokenStream = "
    {
        let a: i32 = 1 + 2; 
        if true { 
            let a: i32 = 0; 
            a = a + 1 
        } else { 
            a = a - 1 
        };
        a
    }
    "
    .parse()
    .unwrap();
    let bl: Block = syn::parse2(ts).unwrap();
    println!("bl {:?}", bl);
    let l = bl.eval(&mut VarEnv::new()).unwrap();
    println!("l {:?}", l);
    // notice this will fail
    assert_eq!(l.get_int().unwrap(), 3);
} */

