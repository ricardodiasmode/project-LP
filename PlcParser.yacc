%%

%name PlcParser

%pos int

%term VAR | INT | BOOL | NIL | FUN | REC
    | TRUE | FALSE
    | EQUAL | PLUS | MINUS | DIV | MULTI
    | NOT | AND
    | SEMICOLON | COMMA | COLON
    | Name of string
    | Number of int
    | CINT of int
    | EOF 
    | IF | THEN | ELSE
    | MATCH | WITH
    | DIFF | LSTHAN | LSEQTHAN | SCOPEOP
    | RSBRAC | LSBRAC
    | RBRA | LBRA
    | RPAR | LPAR
    | FN | END
    | PIPE | ARROW | ARROWFUNCTION | UNDERSCORE
    | PRINT
    | HD | TL | ISEQUAL

%nonterm Prog of expr
    | Declar of expr
    | Expr of expr
    | AtomicExpr of expr 
    | AppExpr of expr 
    | Const of expr
    | Comps of expr list
    | MatchExpr of (expr option * expr) list
    | CondExpr of expr option
    | Args of (plcType * string) list
    | Params of (plcType * string) list
    | TypedVar of plcType * string
    | Type of plcType
    | AtomicType of plcType
    | Types of plcType list


%left AND EQUAL MULTI DIV PLUS MINUS
%right SEMICOLON
%nonassoc NOT HD TL ISEQUAL PRINT Name
%right ARROW
%nonassoc IF
%left ELSE
%left DIFF
%left LSTHAN LSEQTHAN
%right SCOPEOP 
%left LSBRAC

%eop EOF

%noshift EOF

%start Prog

%%

Prog: Expr (Expr)
    | Declar (Declar)

Declar: VAR Name EQUAL Expr SEMICOLON Prog (Let(Name, Expr, Prog))
      | FUN Name Args EQUAL Expr SEMICOLON Prog (Let(Name, makeAnon(Args, Expr), Prog))
      | FUN REC Name Args COLON Type EQUAL Expr SEMICOLON Prog (makeFun(Name, Args, Type, Expr, Prog))

Expr: NOT Expr (Prim1("!", Expr1))
    | MINUS Expr (Prim1("-", Expr1))
    | Expr AND Expr (Prim2("&&", Expr1, Expr2))
    | Expr PLUS Expr (Prim2("+", Expr1, Expr2))
    | Expr MINUS Expr (Prim2("-", Expr1, Expr2))
    | Expr MULTI Expr (Prim2("*", Expr1, Expr2))
    | Expr DIV Expr (Prim2("/", Expr1, Expr2))
    | Expr EQUAL Expr (Prim2("=", Expr1, Expr2))
    | Expr SEMICOLON Expr (Prim2(";", Expr1, Expr2))
    | Expr DIFF Expr (Prim2("!=", Expr1, Expr2))
    | Expr LSTHAN Expr (Prim2("<", Expr1, Expr2))
    | Expr LSEQTHAN Expr (Prim2("<=", Expr1, Expr2))
    | Expr SCOPEOP Expr (Prim2("::", Expr1, Expr2))
    | Expr LSBRAC Number RSBRAC (Item(Number, Expr1))
    | AppExpr (AppExpr)
    | IF Expr THEN Expr ELSE Expr (If(Expr1, Expr2, Expr3))
    | MATCH Expr WITH MatchExpr (Match(Expr, MatchExpr))
    | HD Expr (Prim1("hd", Expr1))
    | TL Expr (Prim1("tl", Expr1))
    | ISEQUAL Expr (Prim1("ise", Expr1))
    | PRINT Expr (Prim1("print", Expr1))
    | AtomicExpr (AtomicExpr)


AtomicExpr:Const (Const)
    | Name (Var(Name))
    | LBRA Prog RBRA (Prog)
    | LPAR Expr RPAR (Expr)
    | LPAR Comps RPAR (List Comps)
    | FN Args ARROWFUNCTION Expr END (makeAnon(Args, Expr))

AppExpr:AtomicExpr AtomicExpr (Call(AtomicExpr1, AtomicExpr2))
    | AppExpr AtomicExpr (Call(AppExpr, AtomicExpr))

Const:LPAR RPAR (List [])
    | LPAR Type LSBRAC RSBRAC RPAR (ESeq(Type))
    | TRUE (ConB(true))
    | FALSE (ConB(false))
    | Number (ConI(Number))
    
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

Type: AtomicType (AtomicType)
    | LPAR Types RPAR (ListT (Types))
    | LSBRAC Type RSBRAC (SeqT(Type))
    | Type ARROW Type (FunT(Type1, Type2))

AtomicType: NIL (ListT [])
    | BOOL (BoolT)
    | INT (IntT)
    | LPAR Type RPAR (Type)

Types: Type COMMA Type (Type1::Type2::[])
  | Type COMMA Types (Type::Types)