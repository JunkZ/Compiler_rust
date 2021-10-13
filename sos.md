# Structural Operational Semantics

integer = "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9";


bool = "true" | "false";

c = statement | skip;

b = (evaluated expr) bool;

n = integer;

σ = store;

```math
Var = \frac{}{<a,\sigma> \rightarrow  <n,\sigma>} \\
```
# Operations
bin = "And" | "Or" | "Lt" | "Gt" | "Leq" | "Geq"
```math
Add = \frac{<n,\sigma> \rightarrow <m,\sigma> }{<n+n',\sigma> \rightarrow <n+n',\sigma> } \\ \\
Sub = \frac{<n,\sigma> \rightarrow <m,\sigma> }{<n-n',\sigma> \rightarrow <n-n',\sigma> } \\ \\
Mul = \frac{<n,\sigma> \rightarrow <m,\sigma> }{<n*n',\sigma> \rightarrow <n*n',\sigma> } \\ \\

bin\_op = \frac{<n,\sigma> \rightarrow <n',\sigma> }{<n \ bin \ n',\sigma> \rightarrow <n \ bin \  n',\sigma> } \\
```
ari_op = Add | Sub | Mul

# Expression

expr = bin_op | ari_op; 

a = expr;

```math
IfThenElse \ True = \frac{<b,\sigma> \Downarrow true <c_1,\sigma> \Downarrow \sigma'' }{<if\ b \ then \ c_1 \ else \ c_2,\sigma> \Downarrow \sigma'} \\ \\
IfThenElse \ False = \frac{<b,\sigma> \Downarrow false <c_2,\sigma> \Downarrow \sigma'' }{<if\ b \ then \ c_1 \ else \ c_2,\sigma> \Downarrow \sigma'} \\ 
```
# Statements

```math

Assign = \frac{<a,a> \Downarrow n }{<x:=a,a>\Downarrow a[x \mapsto n]} \\ \\
Let = \frac{<a,a> \Downarrow n }{<x=a,a>\Downarrow a[x \mapsto n]} \\ \\
Block = \frac{<c_0,\sigma> \Downarrow \sigma' <c_1,\sigma> \Downarrow \sigma'' }{<c_0;c_1,\sigma>\Downarrow \sigma''} \\ \\
While \ true = \frac{<a_0,\sigma> \Downarrow true <while a_0 do S,s>\Downarrow \sigma'}{<while a_0 do S,s>\Downarrow \sigma'} \\ \\
While \ false = \frac{<a_0,\sigma> \Downarrow false}{<while a_0 do S,s>\Downarrow \sigma}

```
