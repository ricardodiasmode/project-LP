%%

%name PlcParser

%pos int

%term VAR | FUN | REC
    | IF | THEN | ELSE
    | MATCH | WITH
    | NOT | MINUS | HD | TL | ISEQUAL | PRINT
    | AND | PLUS | MULTI | DIV | EQUAL 
    | DIFF | LSTHAN | LSEQTHAN | ADDEOP
    | SEMICOLON
    | COMMA | COLON
    | RSBRAC | LSBRAC
    | RBRA | LBRA
    | RPAR | LPAR
    | FN | END
    | TRUE | FALSE
    | PIPE | ARROW | ARROWFUNCTION | UNDERSCORE
    | NIL | BOOL | INT
    | Name of string | CINT of int | Number of int
    | EOF

%nonterm Prog of expr
    | Decl of expr
    | Expr of expr
    | AtomicExpr of expr 
    | AppExpr of expr
    | Comps of expr list
    | MatchExpr of (expr option * expr) list
    | CondExpr of expr option
    | Args of (plcType * string) list
    | Params of (plcType * string) list
    | TypedVar of plcType * string
    | Type of plcType
    | UType of plcType
    | Types of plcType list

%right SEMICOLON ARROW
%nonassoc IF NOT HD TL ISEQUAL PRINT Name
%left ELSE
%left PLUS MINUS MULTI DIV
%left EQUAL DIFF LSTHAN LSEQTHAN LSBRAC AND
%right ADDEOP

%eop EOF

%noshift EOF

%start Prog

%%

Prog:Expr (Expr)
    | Decl (Decl)

Decl:VAR Name EQUAL Expr SEMICOLON Prog (Let(Name, Expr, Prog))
    | FUN Name Args EQUAL Expr SEMICOLON Prog (Let(Name, makeAnon(Args, Expr), Prog))
    | FUN REC Name Args COLON Type EQUAL Expr SEMICOLON Prog (makeFun(Name, Args, Type, Expr, Prog))

Expr:AtomicExpr (AtomicExpr)
    | AppExpr (AppExpr)
    | IF Expr THEN Expr ELSE Expr (If(Expr1, Expr2, Expr3))
    | MATCH Expr WITH MatchExpr (Match(Expr, MatchExpr))
    | NOT Expr (Prim1("!", Expr1))
    | MINUS Expr (Prim1("-", Expr1))
    | HD Expr (Prim1("hd", Expr1))
    | TL Expr (Prim1("tl", Expr1))
    | ISEQUAL Expr (Prim1("ise", Expr1))
    | PRINT Expr (Prim1("print", Expr1))
    | Expr AND Expr (Prim2("&&", Expr1, Expr2))
    | Expr PLUS Expr (Prim2("+", Expr1, Expr2))
    | Expr MINUS Expr (Prim2("-", Expr1, Expr2))
    | Expr MULTI Expr (Prim2("*", Expr1, Expr2))
    | Expr DIV Expr (Prim2("/", Expr1, Expr2))
    | Expr EQUAL Expr (Prim2("=", Expr1, Expr2))
    | Expr DIFF Expr (Prim2("!=", Expr1, Expr2))
    | Expr LSTHAN Expr (Prim2("<", Expr1, Expr2))
    | Expr LSEQTHAN Expr (Prim2("<=", Expr1, Expr2))
    | Expr ADDEOP Expr (Prim2("::", Expr1, Expr2))
    | Expr SEMICOLON Expr (Prim2(";", Expr1, Expr2))
    | Expr LSBRAC Number RSBRAC (Item(Number, Expr1))
    | TRUE (ConB(true))
    | FALSE (ConB(false))
    | Number (ConI(Number))
    | LPAR RPAR (List [])
    | LPAR Type LSBRAC RSBRAC RPAR (ESeq(Type))

AtomicExpr:Name (Var(Name))
    | LBRA Prog RBRA (Prog)
    | LPAR Expr RPAR (Expr)
    | LPAR Comps RPAR (List Comps)
    | FN Args ARROWFUNCTION Expr END (makeAnon(Args, Expr))

AppExpr:AtomicExpr AtomicExpr (Call(AtomicExpr1, AtomicExpr2))
    | AppExpr AtomicExpr (Call(AppExpr, AtomicExpr))

Comps:Expr COMMA Expr (Expr1::Expr2::[])
  | Expr COMMA Comps (Expr::Comps)

MatchExpr:END ([])
  | PIPE CondExpr ARROW Expr MatchExpr ((CondExpr, Expr)::MatchExpr)

CondExpr:Expr (SOME(Expr))
    | UNDERSCORE (NONE)

Args:LPAR RPAR ([])
    | LPAR Params RPAR (Params)

Params:TypedVar (TypedVar::[])
  | TypedVar COMMA Params (TypedVar::Params)

TypedVar: Type Name (Type, Name)

Type:UType (UType)
    | LPAR Types RPAR (ListT (Types))
    | LSBRAC Type RSBRAC (SeqT(Type))
    | Type ARROW Type (FunT(Type1, Type2))

UType: NIL (ListT [])
    | BOOL (BoolT)
    | INT (IntT)
    | LPAR Type RPAR (Type)

Types: Type COMMA Type (Type1::Type2::[])
  | Type COMMA Types (Type::Types)