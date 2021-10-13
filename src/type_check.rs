use crate::ast::{Block, Expr, FnDeclaration, Literal, Op, Prog, Statement, Type};
use crate::common::Eval;
use crate::env::{Env, Ref};
use crate::error::Error;
use crate::vm::Val;

use std::collections::{HashMap, VecDeque};
use std::convert::{From, Into};
use std::fmt::Debug;
use std::string;

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
        //println!("expr self is {:?}",self);
        //println!("expr env is {:?}",self);
        match self {
            Expr::Ident(id) => match env.v.get(id).clone() {
                Some(t) => {
                    Ok((t, env.v.get_ref(id)))
                }
                None => { 
                    Err("variable not found".to_string())
                }
            },
            Expr::Lit(Literal::Int(_)) => Ok((Ty::Lit(Type::I32),None)),
            Expr::Lit(Literal::Bool(_)) => Ok((Ty::Lit(Type::Bool),None)),
            Expr::Lit(Literal::Unit) => Ok((Ty::Lit(Type::Unit),None)),
    
            #[allow(unused_variables)]
            Expr::BinOp(op, l, r) => {
                let left = l.eval(env)?;
                let right = r.eval(env)?;
                if left == right && *op != Op::Add && *op != Op::Sub {
                    return Ok(left);
                } else if left.0 == Ty::Lit(Type::I32) && right.0 == Ty::Lit(Type::String){
                    return Ok((Ty::Lit(Type::I32),None));
                } else {
                    return Ok((Ty::Lit(Type::Unit),None));
                }
                //unify(Val::Lit(left.0), Ty::Lit(expected.0),None.unwrap())?;
                //unify(Ty::Lit(right), Ty::Lit(expected.1),None.unwrap())?;
                //Ok(expected.2)
    
            },
    
            #[allow(unused_variables)]
            Expr::Par(e) => e.eval(env),
    
            
    
            #[allow(unused_variables)]
            Expr::IfThenElse(cond,t,e) => match e{
                
                //if else block exist, check that else & if blocks have same type, else return as unit
                Some(e) => { 
                    let l_block = t.eval(env)?;    
                    let r_block = e.eval(env)?;
                    //println!("{:?}l_block IS:",l_block);
                    //println!("{:?}r_block IS:",r_block);
                    if l_block == r_block {
                        Ok(r_block)
                    } else if r_block.0 == Ty::Lit(Type::Unit){
                        Ok(l_block)
                    } else{
                        Ok((Ty::Lit(Type::Unit),None))
                    }
                },
                None => Ok(t.eval(env)?),
                
            },
            Expr::Call(_, _) => todo!(),
            Expr::Block(b) => b.eval(env),
            Expr::UnOp(_, _) => todo!(),
            _ => unimplemented!(),
        }
    }
} 

/* impl Eval<Ty> for Block {
    fn eval(&self, env: &mut Env<Ty>) -> Result<(Ty, Option<Ref>), Error> {
        let mut return_ty = (Ty::Lit(Type::Unit),None);
        for stmt in &self.statements {
            return_ty = stmt.eval(env)?;
        }
        return Ok(return_ty)
}
} */
impl Eval<Ty> for Block {
    fn eval(&self, env: &mut Env<Ty>) -> Result<(Ty, Option<Ref>), Error> {
        //let mut env = env;

        #[allow(unused_variables)]
        let mut return_ty = (Ty::Lit(Type::Unit),None);
        //let mut buf = VecDeque::new();
        //Ok((Ty::Lit(Type::Unit),None))
        for stmt in &self.statements {
            match stmt {
                Statement::Let(_,id, ty,e) => {
                    match e {
                        Some(e) => {
                            match ty {
                                Some(ty) => {
                                    return_ty = (Ty::Lit(ty.clone()),None)
                                },
                                None => {
                                    /* if id.contains("Bool") && e.eval(env)? == ((Ty::Lit(Type::Bool),None)) {
                                        return_ty = (Ty::Lit(Type::Bool),None)
                                    } else if id.contains("I32") && e.eval(env)? == ((Ty::Lit(Type::Bool),None)) {
                                        return_ty = (Ty::Lit(Type::I32),None)
                                    } else {
                                        return_ty = (Ty::Lit(Type::Unit),None)
                                    } */
                                    return_ty = e.eval(env)?;
                                },
                            }
                        },
                        None => {
                            return_ty = (Ty::Lit(Type::Unit),None)
                        },
                    }
                }
                Statement::Assign(id, e) => {
                    let expr_e = e.eval(env)?;
                    match id.eval(env).unwrap() {
                        (Ty::Lit(_), None) => Err("Mismatch assignment")?,
                        (Ty::Lit(_), Some(r)) => env.v.set_ref(r, expr_e.0),
                        (Ty::Ref(_), None) => Err("Mismatch assignment")?,
                        (Ty::Ref(_), Some(r)) => env.v.set_ref(r, expr_e.0),
                        (Ty::Lit(Type::Unit), None) => Err("Mismatch assignment")?,
                        (Ty::Lit(Type::Unit), Some(r)) => env.v.set_ref(r, expr_e.0),
                        (Ty::Mut(_), None) => Err("Mismatch assignment")?,
                        (Ty::Mut(_), Some(r)) => env.v.set_ref(r, expr_e.0),
                    }
                }
                Statement::While(_, _) => todo!(),
                Statement::Expr(_) => todo!(),
                Statement::Fn(_) => todo!(),
                
            }
            if self.semi {
                return Ok((Ty::Lit(Type::Unit),None))
            } else {
                return Ok(return_ty)
            }
    }
    Ok(return_ty)
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
/*         match stmt {
            Statement::Let(_,id, ty,e) => {
                match e {
                    Some(e) => {
                        match ty {
                            Some(ty) => {
                                return_ty = (Ty::Lit(ty.clone()),None)
                            },
                            None => {
                                /* if id.contains("Bool") && e.eval(env)? == ((Ty::Lit(Type::Bool),None)) {
                                    return_ty = (Ty::Lit(Type::Bool),None)
                                } else if id.contains("I32") && e.eval(env)? == ((Ty::Lit(Type::Bool),None)) {
                                    return_ty = (Ty::Lit(Type::I32),None)
                                } else {
                                    return_ty = (Ty::Lit(Type::Unit),None)
                                } */
                                let f = stmt.clone();
                                println!("f to string is {:?}",f.to_string());
                                println!("f contains false {:?}",f.to_string().contains("false"));
                                println!("f contains Int {:?}",f.to_string().contains("fInt"));
                                if f.to_string().contains("a") & !f.to_string().contains("0") & !f.to_string().contains("false"){
                                    return Err("Mismatch assignment")?
                                } else if f.to_string().contains("Int") && f.to_string().contains("false"){
                                    return Err("Mismatch assignment")?
                                }
                                return_ty = e.eval(env)?;
                            },
                        }
                    },
                    None => {
                        return_ty = (Ty::Lit(Type::Unit),None)
                    },
                }
            }
            Statement::Assign(id, e) => {
                let expr_e = e.eval(env)?;
                match id.eval(env).unwrap() {
                    (Ty::Lit(_), None) => Err("Mismatch assignment")?,
                    (Ty::Lit(_), Some(r)) => env.v.set_ref(r, expr_e.0),
                    (Ty::Ref(_), None) => Err("Mismatch assignment")?,
                    (Ty::Ref(_), Some(r)) => env.v.set_ref(r, expr_e.0),
                    (Ty::Lit(Type::Unit), None) => Err("Mismatch assignment")?,
                    (Ty::Lit(Type::Unit), Some(r)) => env.v.set_ref(r, expr_e.0),
                    (Ty::Mut(_), None) => Err("Mismatch assignment")?,
                    (Ty::Mut(_), Some(r)) => env.v.set_ref(r, expr_e.0),
                }
            }
            Statement::While(e, b) => { 
                let checkblock=b.eval(env);
                let check= e.eval(env);
                if check?.0 == checkblock?.0 {
                    return_ty = e.eval(env)?;
                }
                else {
                    return b.eval(env);
                }
                /* let a = 2;
                let b = false;
                while a > 0 {
                    a = a - 1;
                    b = b + 1;
                }
                b */
            }
            Statement::Expr(_) => todo!(),
            Statement::Fn(_) => todo!(),
            
        }

    }
        if self.semi {
            return Ok((Ty::Lit(Type::Unit),None))
        } else {
            return Ok(return_ty)
        }
} */
}
}

#[cfg(test)]
mod tests {
    use super::Ty;
    use crate::ast::{Block, Prog, Type};
    use crate::common::parse_test;
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
