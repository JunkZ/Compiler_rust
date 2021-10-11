# Structural Operational Semantics

integer = "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9";


bool = "true" | "false";

c = statement | skip;

b = (evaluated expr) bool;

n = integer;

m = integer;

```math
\sigma = store \\
Var = \frac{}{<a,\sigma> \rightarrow  <n,\sigma>} \\
```
# Operations
bin = "And" | "Or" | "Lt" | "Gt" | "Leq" | "Geq"
```math
Add = \frac{<n,\sigma> \rightarrow <m,\sigma> }{<n+m,\sigma> \rightarrow <n+m,\sigma> } \\
Sub = \frac{<n,\sigma> \rightarrow <m,\sigma> }{<n-m,\sigma> \rightarrow <n-m,\sigma> } \\
Mul = \frac{<n,\sigma> \rightarrow <m,\sigma> }{<n*m,\sigma> \rightarrow <n*m,\sigma> } \\

bin\_op = \frac{<n,\sigma> \rightarrow <m,\sigma> }{<n \ bin \ m,\sigma> \rightarrow <n \ bin \  m,\sigma> } \\
```
ari_op = Add | Sub | Mul

# Expression

expr = bin_op | ari_op; 

a = expr;

```math
IfThenElse \ True = \frac{<b,\sigma> \Downarrow true <c_1,\sigma> \Downarrow \sigma'' }{<if\ b \ then \ c_1 \ else \ c_2,\sigma> \Downarrow \sigma'} \\
IfThenElse \ False = \frac{<b,\sigma> \Downarrow false <c_2,\sigma> \Downarrow \sigma'' }{<if\ b \ then \ c_1 \ else \ c_2,\sigma> \Downarrow \sigma'} \\


Var = \frac{}{<a,\sigma> \rightarrow  <n,\sigma>} \\
```
# Statements

```math

Assign = \frac{<a,a> \Downarrow n }{<x:=a,a>\Downarrow a[x \mapsto n]} \\
Let = \frac{<a,a> \Downarrow n }{<x=a,a>\Downarrow a[x \mapsto n]} \\
Block = \frac{<c_0,\sigma> \Downarrow \sigma' <c_1,\sigma> \Downarrow \sigma'' }{<c_0;c_1,\sigma>\Downarrow \sigma''} \\

```