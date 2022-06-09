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
    | COMMA
    | OPENSQRBRACKET
    | CLOSESQRBRACKET

%nonterm Prog of expr
    | Declar of expr
    | Expr of expr
    | Const
    | PlcT of plcType
    | ExprList of expr


%left Prim1 Prim2
%left AND EQUAL MULTI DIV PLUS MINUS OPENSQRBRACKET CLOSESQRBRACKET
%right SEMICOLON OPENPARENT CLOSEPARENT VAR COMMA
%nonassoc NOT Name

%eop EOF

%noshift EOF

%start Prog

%%

Prog: Expr (Expr)
    | Declar (Declar)
    | ExprList (ExprList)

Declar: VAR Name EQUAL Expr SEMICOLON Expr Prog (Let(Name1, Expr1, Expr2))

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
    | ExprList OPENSQRBRACKET Number CLOSESQRBRACKET (Item(Number, ExprList))
    | OPENPARENT PlcT CLOSEPARENT (ESeq(PlcT1))

ExprList: OPENPARENT Expr CLOSEPARENT (List([Expr1]))
    | OPENPARENT Expr COMMA Expr CLOSEPARENT (List([Expr1, Expr2]))
    | OPENPARENT CLOSEPARENT (List([]))

PlcT: OPENSQRBRACKET PlcT CLOSESQRBRACKET (SeqT (PlcT1))
    | BOOL (BoolT)