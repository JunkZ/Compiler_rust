use crate::ast::{Block, Expr, FnDeclaration, Literal, Op, Prog, Statement, Type, UnOp};
use crate::common::Eval;
use crate::env::{Env, Ref};
use crate::error::Error;

use std::collections::HashMap;
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
    pub fn unify(expected: Ty, got: Ty, result: Ty) -> Result<(Ty, Option<Ref>), Error> {
        match expected == got {
            true => Ok((result.into(), None)),
            _ => Err(format!(
                "Cannot unify types, expected {:?} got {:?}",
                expected, got
            )),
        }
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
fn unify_simple(got: Type, expected: Type) -> Result<Type, TypeErr> {
    match got == expected {
        true => Ok(expected),
        false => Err(format!("expected type {:?}, got type {:?}", expected, got)),
    }
}

// op_types
// returns types as: (expected left, expected right, result)
fn op_type(op: Op) -> (Type, Type, Type) {
    match op {
        Op::Add => (Type::I32, Type::I32, Type::I32),
        Op::Sub => (Type::I32, Type::I32, Type::I32),
        Op::Mul => (Type::I32, Type::I32, Type::I32),
        Op::And => (Type::Bool, Type::Bool, Type::Bool),
        Op::Or => (Type::Bool, Type::Bool, Type::Bool),
        Op::Lt => (Type::I32, Type::I32, Type::Bool),
        Op::Gt => (Type::I32, Type::I32, Type::Bool),
        _ => todo!(),
    }
}

impl Eval<Ty> for Expr {
    fn eval(&self, env: &mut Env<Ty>) -> Result<(Ty, Option<Ref>), Error> {
        //println!("expr self is {:?}",self);
        //println!("expr env is {:?}",self);
        match self {
            Expr::Ident(id) => match env.v.get(&id) {
                Some(t) => {
                    Ok((t, None))
                }
                None => { 
                    Err("variable not found".to_string())
                }
            },
            Expr::Lit(Literal::Int(_)) => Ok((Ty::Lit(Type::I32),None)),
            Expr::Lit(Literal::Bool(_)) => Ok((Ty::Lit(Type::Bool),None)),
            Expr::Lit(Literal::Unit) => Ok((Ty::Lit(Type::Unit),None)),
            Expr::BinOp(op, l, r) => {
                println!("l is {:?}",l);
                println!("r is {:?}",r);
                println!("op is {:?}",op);
                let lhs = l.eval(env)?;
                let rhs = r.eval(env)?;
                let check = op_type(*op);

                if lhs.0 == Ty::Lit(check.0.clone()) && rhs.0 == Ty::Lit(check.1.clone()) {
                    if *op == Op::Lt || *op == Op::Gt || *op == Op::Eq {
                        return Ok((Ty::Lit(Type::Bool),None))
                    }
                    return Ok(lhs);
                } else {
                    Err("Binop mismatched types".to_string())
                }
    
            },
            Expr::Par(e) => e.eval(env),
    
            Expr::IfThenElse(cond,t,e) => match e{
                
                //if else block exist, check that else & if blocks have same type, else return as unit
                Some(e) => { 
                    let l_block = t.eval(env)?;    
                    let r_block = e.eval(env)?;
                    if cond.eval(env)?.0!= Ty::Lit(Type::Bool) {
                        Err("Condition not bool".to_string())
                    } else if l_block == r_block {
                        Ok(r_block)
                    } else if r_block.0 == Ty::Lit(Type::Unit){
                        Ok(l_block)
                    } else{
                        Err("Block type mismatch".to_string())
                    }
                },
                None => Ok(t.eval(env)?),
                
            },
            Expr::Call(id, args) => {
                match env.clone().f.0.get(id) {
                   Some(f) => {
                       if args.0.len() != f.0.parameters.0.clone().len() {
                           return Err("Mismatch number of args and parameters".to_string());
                       } else {
                           let mut i = 0;
                           
                           for par in &f.0.parameters.0 {
                               let x =args.0.get(i).unwrap().eval(env)?.0.clone();
                               if  x != Ty::Lit(par.ty.clone()) {
                                   return Err("Parameter mismatch arg type!".to_string())
                               }
                               i = i+1;

                           }
                           return Ok((Ty::Lit(Type::Unit),None))
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
                            Ok((Ty::Ref(asg),Some(asg)))
                        },
                        None => {
                            Ok((Ty::Ref(env.v.stack_val(e_eval?.0)),None))
                        },
                    }
                },
                UnOp::DeRef => {
                    match b.eval(env)?.0 {

                        Ty::Ref(r) => {
                            Ok((env.v.de_ref(r), Some(r)))
                    },
                    Ty::Lit(_) => Err("Derefrencing a non reference".to_string()),
                    Ty::Mut(_) => Err("Derefrencing a non reference".to_string()),
                        
                }
                },
                UnOp::Mut => {return Ok((Ty::Mut(Box::new(b.eval(env)?.0)),None))},
                UnOp::Bang => {
                    return Ok((Ty::Lit(Type::Bool),None))
                },
            }, 
            _ => unimplemented!(),
        }
    }
} 

impl Eval<Ty> for Block {
    fn eval(&self, env: &mut Env<Ty>) -> Result<(Ty, Option<Ref>), Error> {
        let mut return_val = (Ty::Lit(Type::Unit),None);
        for stmt in &self.statements {
            return_val = stmt.eval(env)?;
        }
        return Ok(return_val)
    }
}



impl Eval<Ty> for FnDeclaration {
    fn eval(&self, env: &mut Env<Ty>) -> Result<(Ty, Option<Ref>), Error> {
        //fn supposed to return type of body, or unit if empty
        /*        
            "
        fn main() {
            let a = 1;
            a;
        }
        ",
        );
        assert_eq!(v.unwrap_err(), "Ok"); */
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
        //println!("env in fndecl is {:?}",env);
        //todo!("not implemented {:?}", self)
    }
}

impl Eval<Ty> for Prog {
    fn eval(&self, env: &mut Env<Ty>) -> Result<(Ty, Option<Ref>), Error> {
        for func in &self.0 {
            if func.eval(env).is_err(){
                return Err("Invalid function declaration".to_string())
            }
        }
        return Err("Ok")?;
    }
}
impl Eval<Ty> for Statement {
    fn eval(&self, env: &mut Env<Ty>) -> Result<(Ty, Option<Ref>), Error> {
        let mut return_val = (Ty::Lit(Type::Unit),None);
            env.v.push_scope();
            match self {
                Statement::Let(_,id, _, e) => {
                    match e {
                        Some(e) => {
                            let l = e.eval(env)?;
                            let r =env.v.alloc(id, l.clone().0);
                            return_val=(l.0,Some(r));
                        },
                        None => {
                            let r = env.v.alloc(id, Ty::Lit(Type::Unit));
                            return_val = (Ty::Lit(Type::Unit),Some(r));
                        },
                    }
                }
                Statement::Assign(id, e) => {
                    let b = e.eval(env);

                    let a = id.eval(env);
                    //println!("a is {:?}",a);
                    //println!("b is {:?}",b);
                    if a.is_err() {
                        return Err("Assignment ID is error".to_string())
                    } else if b.is_err() {
                        return Err("Assignment Expression is error".to_string())
                    }
                    else if a.clone()?.0 == b.clone()?.0{
                        return Ok(a?);
                    } else if b?.0 == Ty::Lit(Type::Unit){
                        return Ok(a?);
                    } else {
                        return Err("types mismatch for assign".to_string())
                    } 
                    //return Ok((Ty::Lit(Type::Unit),None))
        
                }
                Statement::Expr(e) => {
                    return_val = e.eval(env).unwrap();
                },
                Statement::While(c, block) => {
                    let check= c.eval(env);
                    if check?.0 != Ty::Lit(Type::Bool) {
                        return Err("Condition not of type bool".to_string())
                    }else{
                        let x = block.eval(env)?;
                        return Ok(x);
                    }
                },
                Statement::Fn(f) => {return_val = f.eval(env)?},
            }
        Ok(return_val)
}
}
 



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

    
}
