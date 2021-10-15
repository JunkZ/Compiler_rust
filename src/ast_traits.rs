// Extra traits implemented for AST

use std::fmt;

use crate::ast::*;

// Back-port utility functions/traits for your AST here.

impl Expr {
    pub fn bin_op(o: Op, left: Expr, right: Expr) -> Self {
        Expr::BinOp(o, Box::new(left), Box::new(right))
    }
}

impl From<Literal> for Expr {
    fn from(lit: Literal) -> Self {
        Expr::Lit(lit)
    }
}

impl From<i32> for Expr {
    fn from(i: i32) -> Self {
        Expr::Lit(Literal::Int(i))
    }
}

impl From<i32> for Literal {
    fn from(i: i32) -> Self {
        Literal::Int(i)
    }
}

impl From<Expr> for Literal {
    fn from(e: Expr) -> Self {
        match e {
            Expr::Lit(l) => l,
            _ => unreachable!(),
        }
    }
}

impl fmt::Display for Op {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let s = match self {
            Op::Add => "+",
            Op::Sub => "-",
            Op::Mul => "*",
            Op::Div => "/",
            Op::And => "&&",
            Op::Or => "||",
            Op::Eq => "==",
            Op::Lt => "<",
            Op::Gt => ">",
        };
        write!(f, "{}", s)
    }
}

// Back-port your ast Display traits here
// You may want to re-factor tests into module.
// See e.g., vm.rs

impl fmt::Display for Literal {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let s = match self {
            Literal::Bool(b) => b.to_string(),
            Literal::Int(i) => i.to_string(),
            Literal::Unit => "()".to_string(),
            Literal::String(s) => s.to_string(),
        };
        write!(f, "{}", s)
    }
}
impl fmt::Display for Statement { //pretty printing for statements
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let s = match self {
            Statement::Assign(a,b)=> format!("{} = {}",a, b),
            Statement::Expr(c)=> format!("{} ",c), //should not have any text
            Statement::Let(_,d,Some(e),f )=> format!("Let {}: {} = {}",d, e, f.clone().unwrap()),
            Statement::Let(_,d,None,f )=> format!("{}",f.clone().unwrap()),
            Statement::While(g,h)=> format!("while {} {}",g, h),
            //Statement::Fn(f)=> format!("{}",f),
            
            Statement::Fn(x) => format!("fn {}",x),
            //_ => unimplemented!(),
        };
        write!(f, "{}", s)
    }
}
impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let s = match self {
            Type::I32 => "i32",
            Type::Bool => "bool",
            Type::Unit => "()",
            Type::String => "string",
            Type::Ref(x) => {
                let mut ref_print = String::from("&{");
                ref_print.push_str(&x.to_string());
                ref_print.push_str("}");
                return write!(f, "{}",ref_print);
            }
        };
        write!(f, "{}", s)
    }
}
impl fmt::Display for Block { //pretty printing for blocks
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut hello = String::from("{\n");
        for iter in self.statements.iter() {
            //println!("CURRENT LINE IS: {:?}", iter.to_string());
            hello.push_str("  ");
            if iter.to_string() != ";" {
                hello.push_str(&iter.to_string());
                hello.push_str("\n");
            }
            
        }
        hello.push_str("}");
        write!(f, "{}", hello)
    }
}


impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        println!("self is{:?}",self);
        let s = match self {
            Expr::Ident(a) => a.to_owned(),
            Expr::Lit(l) => format!("{}", l),
            Expr::BinOp(op, l, r) => format!("{} {} {}", l, op, r),
            Expr::Par(e) => format!("({})", e),
            Expr::IfThenElse(bo, bl, o) => {
                if let None = o { //used help from: https://stackoverflow.com/questions/53177980/how-do-i-conditionally-execute-code-only-when-an-option-is-none
                    format!("if {} {}", bo, bl)
                } else {
                    format!("if {} {} else {}", bo, bl, o.clone().unwrap()) //no display for option
                }
            }
            Expr::Call(s, arg) => format!("{}({})", s,arg),
            Expr::Block(b) => format!("{}",b),
            Expr::UnOp(op, e) => format!("{}{}", op,e),
            Expr::Print() => format!("println"),
        };
        write!(f, "{}", s)
    }
}

#[test]
fn display_literal() {
    println!("{}", Literal::Int(3));
    println!("{}", Literal::Bool(false));
    println!("{}", Literal::Unit);
    assert_eq!(format!("{}", Literal::Int(3)), "3");
    assert_eq!(format!("{}", Literal::Bool(false)), "false");
    assert_eq!(format!("{}", Literal::Unit), "()");
}
#[test]
fn display_type() {
    assert_eq!(format!("{}", Type::I32), "i32");
    assert_eq!(format!("{}", Type::Bool), "bool");
    assert_eq!(format!("{}", Type::Unit), "()");
    assert_eq!(format!("{}", Type::String), "string");
}

impl fmt::Display for UnOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        //let unop =self;
        let unop =match self{
            UnOp::Ref => "&",
            UnOp::DeRef => "*",
            UnOp::Mut => "mut",
            UnOp::Bang => "!",
        };
        write!(f,"{}",unop)
    }
}

impl fmt::Display for Mutable {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        todo!() //is this needed
    }
}

impl fmt::Display for Parameter {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f,"{}", format!("{} {}", self.id, self.ty))
    }
}

impl fmt::Display for Parameters {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut params_print = String::from("");
        for x in self.0.iter() {
            params_print.push_str(&x.to_string()); //XIA
        };
        write!(f, "{}",params_print)
    }
}

impl fmt::Display for Arguments {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut hello = String::from("");
        for arg in self.0.iter() {
            hello.push_str(&arg.to_string());
            hello.push_str(", ");
        }
        write!(f, "{}", hello)
    }
}

impl fmt::Display for FnDeclaration {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut hello = String::from("fn ");
        match &self.ty {
            Some(t) => {hello.push_str(&t.to_string()); hello.push_str(" ");},
            None => (),
        }
        hello.push_str(&self.id.to_string());
        hello.push_str("(");
        hello.push_str(&self.parameters.to_string());
        hello.push_str(") {");
        hello.push_str(&self.body.to_string());
        hello.push_str("}");
        /*  for iter in self.statements.iter() {
            //println!("CURRENT LINE IS: {:?}", iter.to_string());
            hello.push_str("  ");
            if iter.to_string() != ";" {
                hello.push_str(&iter.to_string());
                hello.push_str("\n");
            }
            
        } */
        write!(f, "{}", hello)
    }
}

impl fmt::Display for Prog {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut hello = String::from("");
        //hello.push_str(self.0.to_string());
        for item in self.0.iter() {
            hello.push_str(&item.to_string());
            //hello.push_str(%item);

        }
        write!(f, "{}", hello)
    }
}


#[test]
fn display_if_then_else() {
    let ts: proc_macro2::TokenStream = "
    if a {
        let a : i32 = false;
        0
    } else {
        if a == 5 { b = 8 };
        while b {
            e;
        };
        b
    }
    "
    .parse()
    .unwrap();
    let e: Expr = syn::parse2(ts).unwrap();
    println!("ast:\n{:?}", e);

    println!("pretty:\n{}", e);
}

#[test]
fn display_while() {
    let ts: proc_macro2::TokenStream = "
    while a == 9 {
        let b : i32 = 7;
    }
    "
    .parse()
    .unwrap();
    let e: Statement = syn::parse2(ts).unwrap();
    println!("ast:\n{:?}", e);

    println!("pretty:\n{}", e);
}

#[test]
fn display_expr() {
    println!("{}", Expr::Ident("a".to_string()));
    println!("{}", Expr::Lit(Literal::Int(7)));
    println!("{}", Expr::Lit(Literal::Bool(false)));
    let e = Expr::BinOp(
        Op::Add,
        Box::new(Expr::Ident("a".to_string())),
        Box::new(Expr::Lit(Literal::Int(7))),
    );
    println!("{}", e);
    assert_eq!(format!("{}", e), "a + 7");
}

// As you see it becomes cumbersome to write tests
// if you have to construct the Expr by hand.
//
// Instead we might use our parser

#[test]
fn parse_display_expr() {
    let ts: proc_macro2::TokenStream = "a + 7".parse().unwrap();
    let e: Expr = syn::parse2(ts).unwrap();
    println!("e {}", e);
}

// This one will fail (Display for `if` is not yet implemented).
// Implement it as an optional assignment
//
// Hint: You need to implement Display for Statement and Block

#[test]
fn parse_display_if() {
    let ts: proc_macro2::TokenStream = "if a > 5 {5}".parse().unwrap();
    let e: Expr = syn::parse2(ts).unwrap();
    println!("e {}", e);
}
