# Type Rules
# Literals

integer = "1" | "2" | ... | "2147483647";

i32 = integer in bits;

bool = "true" | "false";


# Literal helpers

type = bool | i32 | "()";

# Expressions

```math
fn \: BinOp(<a_1:expr>,<a_2:env>) \rightarrow type \\
fn \: Ident(<a_1:string>) \rightarrow type \\
fn \: IfThenElse(<a_1:bool>,<a_2:block>,<a_3:block>) \rightarrow type \\
```

# Operations

```math
fn \: And(<a_1:bool>,<a_2:bool>) \rightarrow bool \\
fn \: Or(<a_1:bool>,<a_2:bool>) \rightarrow bool \\
fn \: Lt(<a_1:i32>,<a_2:i32>) \rightarrow bool \\
fn \: Gt(<a_1:i32>,<a_2:i32>) \rightarrow bool \\
```
```math
fn \: Add(<a_1:i32>,<a_2:i32>) \rightarrow i32 \\
fn \: Sub(<a_1:i32>,<a_2:i32>) \rightarrow i32 \\
fn \: Mul(<a_1:i32>,<a_2:i32>) \rightarrow i32 \\
fn \: Div(<a_1:i32>,<a_2:i32>) \rightarrow i32 \\
```

# Statements

```math
fn \: Let(<a_1:expr>,<a_2:type>,<a_3:expr>) \rightarrow type \\
fn \: Expr(<a_1:expr>) \rightarrow type \\
fn \: Assign(<a_1:expr>,<a_2:expr>) \rightarrow type \\
fn \: While(<a_1:expr>,<a_2:block>) \rightarrow type \\
```

# Unary ops
```math
fn : Ref(<a_1:expr>) \rightarrow ref \\ 
fn : DeRef(<a_1:ref>) \rightarrow value \\
```

# Function
```math
fn : Fn(<a_1:block>) \rightarrow type \\ 
```

# Prog
```math
fn : Prog(<a_1:prog>,<a_2:ref>) \rightarrow type \\ 
```