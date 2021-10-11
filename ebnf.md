# Literals

character = "A" | "B" | ... | "Z" | "a" | "b" | ... | "z" ;

integer = "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9";

number = "0" | ( [ "-" ] , { integer } );

bool = "true" | "false";

# Literal helpers

type = "bool" | "i32" | "()";

literal = number | bool;

ident = { character };

# Operations

bool_op = bool, ("&&" | "||" | "<" | ">" ), bool;

int_op = integer, ("+" | "-" | "*" | "/"), integer;

bin_op = int_op | bool_op; 

ifthenelse = "if", expr, block, [ "else", block ];

# Expressions

expr = ident | ifthenelse | literal | bin_op;

expr_combined = expr, "(", expr, ")";

# Statements

while = "while", bool_op, block;

assign = expr, "=", expr;

let = "let", expr, ":", type, "=", expr;

statement = let | assign | while | expr;

block = { statement }, ";";