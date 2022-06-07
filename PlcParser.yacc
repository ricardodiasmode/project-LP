%%

%name PlcParser

%pos int

%term VAR | INT | BOOL | NIL
    | TRUE | FALSE
    | EQUAL | PLUS | MINUS | DIV | MULTI
    | NOT | AND
    | SEMICOLON
    | Name of string
    | Number of int
    | EOF
    | Prim1
    | Prim2
    | ELSE
    | OPENPARENT
    | CLOSEPARENT

%nonterm Prog of expr
    | Declar of expr
    | Expr of expr
    | Const


%left Prim1 Prim2
%left AND EQUAL MULTI DIV PLUS MINUS
%right SEMICOLON OPENPARENT CLOSEPARENT
%nonassoc NOT Name

%eop EOF

%noshift EOF

%start Prog

%%

Prog: Expr (Expr)
    | Declar (Declar)

Declar: VAR Name EQUAL Expr SEMICOLON Prog (Let(Name, Expr, Prog))

Expr: NOT Expr (Prim1("!", Expr1))
    | MINUS Expr (Prim1("-", Expr1))
    | Expr AND Expr (Prim2("&&", Expr1, Expr2))
    | Expr PLUS Expr (Prim2("+", Expr1, Expr2))
    | Expr MINUS Expr (Prim2("-", Expr1, Expr2))
    | Expr MULTI Expr (Prim2("*", Expr1, Expr2))
    | Expr DIV Expr (Prim2("/", Expr1, Expr2))
    | Expr EQUAL Expr (Prim2("=", Expr1, Expr2))
    | Expr SEMICOLON Expr (Prim2(";", Expr1, Expr2))
    | TRUE (ConB(true))
    | FALSE (ConB(false))
    | Number (ConI(Number))
    | OPENPARENT CLOSEPARENT ()
    | OPENPARENT Expr CLOSEPARENT (Expr1)
