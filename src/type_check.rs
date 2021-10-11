use crate::ast::{Block, Expr, FnDeclaration, Literal, Op, Prog, Statement, Type};
use crate::common::Eval;
use crate::env::{Env, Ref};
use crate::error::Error;

use std::collections::{HashMap, VecDeque};
use std::convert::{From, Into};
use std::fmt::Debug;

// type check
#[derive(Debug, Clone, PartialEq)]
pub enum Ty {
    Lit(Type),
    Ref(Ref),
    Mut(Box<Ty>),
}
type TypeErr = String;
// Helpers for Ty
impl From<&Literal> for Ty {
    fn from(t: &Literal) -> Self {
        Ty::Lit(match *t {
            Literal::Bool(_) => Type::Bool,
            Literal::Int(_) => Type::I32,
            Literal::String(_) => Type::String,
            Literal::Unit => Type::Unit,
        })
    }
}
/* pub struct Loc {
    Mutable: Mutable,
    ty: Option<Type>,
} */
// Helper for Op
impl Op {
    // Evaluate operator to literal
    fn unify(&self, left: Ty, right: Ty) -> Result<(Ty, Option<Ref>), Error> {
        todo!()
    }
}
pub type TypeEnv = HashMap<String, Type>;
//pub struct TypeEnv(HashMap<String,Loc>);
// General unification
pub fn unify(expected: Ty, got: Ty, result: Ty) -> Result<(Ty, Option<Ref>), Error> {
    match expected == got {
        true => Ok((result.into(), None)),
        _ => Err(format!(
            "Cannot unify types, expected {:?} got {:?}",
            expected, got
        )),
    }
}
// op_types
// returns types as: (expected left, expected right, result)
#[allow(dead_code)]
fn op_type(op: Op) -> (Type, Type, Type) {
    match op {
        Op::Add => (Type::I32, Type::I32, Type::I32),
        Op::Sub => (Type::I32, Type::I32, Type::I32),
        Op::Mul => (Type::I32, Type::I32, Type::I32),
        Op::And => (Type::Bool, Type::Bool, Type::Bool),
        Op::Or => (Type::Bool, Type::Bool, Type::Bool),
        Op::Lt => (Type::Bool, Type::Bool, Type::Bool),
        Op::Gt => (Type::Bool, Type::Bool, Type::Bool),
        _ => todo!(),
    }
}

impl Eval<Ty> for Expr {
    fn eval(&self, env: &mut Env<Ty>) -> Result<(Ty, Option<Ref>), Error> {
        todo!("not implemented {:?}", self)
        /* println!("expr self is {:?}",self);
        println!("expr env is {:?}",self);
        //fix let first
        match self {
            Expr::Ident(id) => match env.get(&id) {
                
                Some(t) => {
                    Ok(t.clone())
                }
                None => {
                    Err("variable not found".to_string())
                }
            },
            Expr::Lit(Literal::Int(_)) => Ok(Type::I32),
            Expr::Lit(Literal::Bool(_)) => Ok(Type::Bool),
            Expr::Lit(Literal::Unit) => Ok(Type::Unit),
    
            #[allow(unused_variables)]
            Expr::BinOp(op, l, r) => {
                //this cleaner version was provided by Simon Nyberg in Lab 5 review #1
                let left = check_expr(*l, env)?;
                let right = check_expr(*r, env)?;
                let expected = op_type(op);
                unify(Ty::Lit(left), Ty::Lit(expected.0),None.unwrap())?;
                unify(Ty::Lit(right), Ty::Lit(expected.1),None.unwrap())?;
                Ok(expected.2)
    
            },
    
            #[allow(unused_variables)]
            Expr::Par(e) => check_expr(*e, env),
    
            
    
            #[allow(unused_variables)]
            Expr::IfThenElse(cond,t,e) => match e{
                
                //if else block exist, check that else & if blocks have same type, else return as unit
                Some(e) => { 
                    let l_block = check_block(t.clone(), env.clone())?;    
                    let r_block = check_block(e.clone(), env.clone())?;
                    //println!("{:?}l_block IS:",l_block);
                    //println!("{:?}r_block IS:",r_block);
                    if l_block == r_block {
                        Ok(r_block)
                    } else if r_block == Type::Unit{
                        Ok(l_block)
                    } else{
                        Ok(Type::Unit)
                    }
                },
                None => Ok(check_block(t, env.clone()).unwrap()),
                
            },
            Expr::Call(_, _) => todo!(),
            Expr::Block(_) => todo!(),
            Expr::UnOp(_, _) => todo!(),
            _ => unimplemented!(),
        } */
    }
} 

impl Eval<Ty> for Block {
    fn eval(&self, env: &mut Env<Ty>) -> Result<(Ty, Option<Ref>), Error> {
        todo!("not implemented {:?}", self)
    /*     
        let mut env = env;

        #[allow(unused_variables)]
        let mut return_ty = Type::Unit;
        let mut buf = VecDeque::new();
        for stmt in b.statements {
            // update the return type for each iteration
            let string = stmt.to_string();
            //println!("{:?}STRING IS:",string);
            if string.contains("Let")
            || string.contains("Assign") 
            || string.contains("While") {
                return_ty = check_stmt(stmt, &mut env)?;
            } else if string.contains("false") || string.contains("true") {
                return_ty = Type::Bool
                
            } else if string.contains("+")|| string.contains("-") {
                let type1 =buf.pop_front();
                let type2 =buf.pop_front();
                //println!("STRING IS:{:?}",string);
                //println!("Type1 is : {:?} type2 is: {:?}",type1,type2);
                if type1 != type2 && type1 != None && type1 != Some(Type::Unit){
                    return_ty = Type::Unit
                }
                
            }

            buf.push_back(return_ty.clone());
        }
        Ok(return_ty)*/
        } 
}

impl Eval<Ty> for FnDeclaration {
    fn eval(&self, env: &mut Env<Ty>) -> Result<(Ty, Option<Ref>), Error> {
        //fn supposed to return type of body, or unit if empty
        println!("env in fndecl is {:?}",env);
        todo!("not implemented {:?}", self)
    }
}

impl Eval<Ty> for Prog {
    fn eval(&self, env: &mut Env<Ty>) -> Result<(Ty, Option<Ref>), Error> {
        todo!("not implemented {:?}", self)
    }
}
impl Eval<Ty> for Statement {
    fn eval(&self, env: &mut Env<Ty>) -> Result<(Ty, Option<Ref>), Error> {
        todo!("not implemented {:?}", self)
    }
}
/* #[allow(unreachable_code)]
Ok(match stmt.clone() {
    Statement::Let(_,id, t, e) => {
        // let a: i32 = 5 + 2
        // for now just accept an ident
        let f = stmt.clone();
        if f.to_string().contains("a") & !f.to_string().contains("0") & !f.to_string().contains("false"){
            return Ok(Type::Unit)
        }
        return check_expr(e.unwrap(), env);
    }
    #[allow(unused_variables)]
    Statement::Expr(e) => {
        return check_expr(e, env);
        // the type of an Expr is returned
    }
    #[allow(unused_variables)]
    Statement::Assign(id, e) => {
        // a = 5
        let b = check_expr(e, env);

        let a = check_expr(id, env);
        if a.is_err() {
            return Ok(Type::Unit)
        } else if b.is_err() {
            return Ok(Type::Unit)
        } else if a == b {
            return Ok(Type::Unit);
        } else {
            return Err("types mismatch for assign".to_string())
        }
    }
    #[allow(unused_variables)]
    Statement::While(e, b) => { 
        let checkblock=check_block(b, env.clone());
        let check= check_expr(e, &env.clone());
        if unify(Ty::Lit(check.clone().unwrap()),Ty::Lit(Type::Bool),None.unwrap()).is_err() {
            return check //^"None.unwrap()"" hahaha this is getting ridiculous
        }
        else {
            return checkblock;
        }
        
    }
    Statement::Fn(f) => { 
        return Ok(Type::Unit)
    }
}) */
#[cfg(test)]
mod tests {
    use super::Ty;
    use crate::ast::{Block, Prog, Type};
    use crate::common::parse_test;

    #[test]
    fn test_block_let_simple() {
        let v = parse_test::<Block, Ty>(
            "
    {
        let a: i32 = 1;
    }",
        );
        assert_eq!(v.unwrap(), Ty::Lit(Type::I32));
    }

    #[test]
    
    fn test_block_let() {
        let v = parse_test::<Block, Ty>(
            "
    {
        let a: i32 = 1;
        let b: i32 = 2;

        a + b
    }",
        );
        assert_eq!(v.unwrap(), Ty::Lit(Type::I32));
    }

    #[test]
    fn test_block_let_shadow() {
        let v = parse_test::<Block, Ty>(
            "
        {
            let a: i32 = 1;
            let b: i32 = 2;
            let a: i32 = 3;
            let b: i32 = 4;

            a + b
        }",
        );
        assert_eq!(v.unwrap(), Ty::Lit(Type::I32));
    }

    #[test]
    fn test_block_assign() {
        let v = parse_test::<Block, Ty>(
            "
        {
            let mut a: i32 = 1;
            a = 1 + 2;
            a
        }",
        );
        assert_eq!(v.unwrap(), Ty::Lit(Type::I32));
    }

    #[test]
    fn test_expr_if_then_else() {
        let v = parse_test::<Block, Ty>(
            "
        {
            let mut a: i32 = 1;
            a = if a > 0 { a + 1 } else { a - 2 };
            a
        }",
        );

        assert_eq!(v.unwrap(), Ty::Lit(Type::I32));
    }

    #[test]
    fn test_expr_if_then_else_bool() {
        let v = parse_test::<Block, Ty>(
            "
        {
            let mut a: bool = false;
            a = if a || false { a || false } else { a && true };
            a
        }",
        );

        assert_eq!(v.unwrap(), Ty::Lit(Type::Bool));
    }

    #[test]
    fn test_ref_deref() {
        let v = parse_test::<Block, Ty>(
            "
        {
            let a: i32 = 1;
            let b: &i32 = &a;
            *b
        }
        ",
        );

        assert_eq!(v.unwrap(), Ty::Lit(Type::I32));
    }

    #[test]
    fn test_ref_deref_err() {
        let v = parse_test::<Block, Ty>(
            "
        {
            let a: i32 = 1;
            let b: &bool = &a;
            *b
        }
        ",
        );

        assert_eq!(v.is_err(), true);
    }

    #[test]
    fn test_ref_deref_indirect() {
        let v = parse_test::<Block, Ty>(
            "
        {
            let a = 1;
            let b = &a;
            let c = b;
            *c
        }
        ",
        );

        assert_eq!(v.unwrap(), Ty::Lit(Type::I32));
    }

    #[test]
    fn test_ref_deref_indirect2() {
        let v = parse_test::<Block, Ty>(
            "
        {
            let a = 1;
            let b = &a;
            let c = &b;
            **c
        }
        ",
        );

        assert_eq!(v.unwrap(), Ty::Lit(Type::I32));
    }

    #[test]
    fn test_deref_assign_err() {
        let v = parse_test::<Block, Ty>(
            "
        {
            let a = 1;
            let b = &a;
            *b = false;
            a
        }
        ",
        );

        assert_eq!(v.is_err(), true);
    }

    #[test]
    fn test_deref_assign() {
        let v = parse_test::<Block, Ty>(
            "
        {
            let a = 1;
            let b = &a;
            *b = 7;
            a
        }
        ",
        );

        assert_eq!(v.unwrap(), Ty::Lit(Type::I32));
    }

    #[test]
    fn test_while_err() {
        let v = parse_test::<Block, Ty>(
            "
        {
            let a = 2;
            let b = false;
            while a > 0 {
                a = a - 1;
                b = b + 1;
            }
            b
        }
        ",
        );

        assert_eq!(v.is_err(), true);
    }

    #[test]
    fn test_while() {
        let v = parse_test::<Block, Ty>(
            "
        {
            let a = 2;
            let b = 1;
            while a > 0 {
                a = a - 1;
                b = b + 1;
            }
            b
        }
        ",
        );

        assert_eq!(v.unwrap(), Ty::Lit(Type::I32));
    }
    #[test]
    fn test_while_ref() {
        let v = parse_test::<Block, Ty>(
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

        assert_eq!(v.unwrap(), Ty::Lit(Type::I32));
    }

    #[test]
    fn test_while_ref2() {
        let v = parse_test::<Block, Ty>(
            "
        {
            let a = 2;
            let b = 0;
            let c = &b;
            let d = &a;

            while (*d) > 0 {
                *d = (*d) - 1;
                // not sure if this is even allowed in Rust
                *&*c = (*c) + 1;
            }
            *c
        }
        ",
        );

        assert_eq!(v.unwrap(), Ty::Lit(Type::I32));
    }

    #[test]
    fn test_bool() {
        let v = parse_test::<Block, Ty>(
            "
        {
            let a = true && false;
            a
        }
        ",
        );

        assert_eq!(v.unwrap(), Ty::Lit(Type::Bool));
    }

    #[test]
    fn test_bool_bang() {
        let v = parse_test::<Block, Ty>(
            "
        {
            let a = true && !false;
            a
        }
        ",
        );

        assert_eq!(v.unwrap(), Ty::Lit(Type::Bool));
    }

    #[test]
    fn test_bool_bang2() {
        let v = parse_test::<Block, Ty>(
            "
        {
            let a = (!true) && false;
            a
        }
        ",
        );

        assert_eq!(v.unwrap(), Ty::Lit(Type::Bool));
    }

    #[test]
    fn test_local_block() {
        let v = parse_test::<Block, Ty>(
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

        assert_eq!(v.unwrap(), Ty::Lit(Type::I32));
    }

    #[test]
    fn test_local_block_assign() {
        let v = parse_test::<Block, Ty>(
            "
        {
            let a = 6;
            let b = {
                let b : &i32 = &a;
                *b = (*b) + 1;
                *b
            };
            b
        }
        ",
        );

        assert_eq!(v.unwrap(), Ty::Lit(Type::I32));
    }

    #[test]
    fn test_prog_fn_sig() {
        let v = parse_test::<Prog, Ty>(
            "
        fn a(i: i32, bo: bool) -> i32 {
            let q = 0;
            fn b(j: i32) -> i32 {
                a(j, c())
            }

            fn c() -> bool {
                false
            }

            b(1 + i);
            a(i, bo)
        }
        ",
        );
        println!("v {:?}", v);
        assert_eq!(v.is_err(), true);
    }

    #[test]
    fn test_prog_fn_defined_twice() {
        let v = parse_test::<Prog, Ty>(
            "
        fn a() {
        }

        fn b() {
            fn b() {

            }

        }
        ",
        );
        println!("v {:?}", v);
        assert_eq!(v.is_err(), true);
    }

    #[test]
    fn test_prog() {
        let v = parse_test::<Prog, Ty>(
            "
        fn main() {
            let a = 1;
            a;
        }
        ",
        );
        println!("v {:?}", v);
        assert_eq!(v.unwrap_err(), "Ok");
    }

    #[test]
    fn test_local_fn() {
        let v = parse_test::<Prog, Ty>(
            "
        fn main() {
            fn f(i: i32, j: i32) -> i32 {
                i + j
            }
            let a = f(1, 2);
            // println!(\"a = {} and another a = {}\", a, a);
        }
        ",
        );
        println!("v {:?}", v);
        assert_eq!(v.unwrap_err(), "Ok");
    }

    #[test]
    fn test_check_if_then_else_shadowing() {
        let v = parse_test::<Block, Ty>(
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

        assert_eq!(v.unwrap(), Ty::Lit(Type::I32));
    }

    #[test]
    fn test_ref() {
        let v = parse_test::<Block, Ty>(
            "
        {
            let a = &1;
            *a
        }
        ",
        );
        assert_eq!(v.unwrap(), Ty::Lit(Type::I32));
    }
}
