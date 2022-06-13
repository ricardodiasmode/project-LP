%%

%name PlcParser

%pos int

%term VAR | FUN | REC | IF | THEN | ELSE | MATCH | WITH | NOT | MINUS | HD | TL | ISEQUAL | PRINT | OCOMEN | CCOMEN
    | AND | PLUS | MULTI | DIV | EQUAL | DIFF | LSTHAN | LSEQTHAN | ADDTL | SEMICOLON | COMMA | COLON
    | RSBRAC | LSBRAC| RBRA | LBRA | RPAR | LPAR | FN | END | TRUE | FALSE | PIPE | ARROW | ARROWFUNCTION 
    | UNDERSCORE | NIL | BOOL | INT | ID of string | CINT of int | Number of int | EOF

%nonterm Prog of expr
    | Decl of expr
    | AnonFun of expr
    | Expr of expr
    | Cond of expr
    | UExpr of expr 
    | DeclFun of expr
    | Const of expr
    | Text of expr
    | LogicalExpr of expr
    | AlgebricExpr of expr
    | ComparativExpr of expr
    | AppExpr of expr
    | Comen of expr
    | NotExpr of expr
    | Comps of expr list
    | MatchExpr of (expr option * expr) list
    | KeyExpr of expr option
    | Args of (plcType * string) list
    | Params of (plcType * string) list
    | TypedVar of plcType * string
    | Type of plcType
    | Types of plcType list

%right SEMICOLON ARROW
%nonassoc IF NOT HD TL ISEQUAL PRINT ID
%left ELSE
%left PLUS MINUS MULTI DIV
%left EQUAL DIFF LSTHAN LSEQTHAN LSBRAC AND
%right ADDTL

%eop EOF

%start Prog

%%

Prog: Expr (Expr)
    | Decl (Decl)
    | Comen (Comen)

Comen: LPAR MULTI Text MULTI RPAR Expr (Expr)

Text: ID (Var(ID))
    | Text COLON (Text)   
    | Text ID (Text)
    | Text LPAR (Text)
    | Text RPAR (Text)

Decl: VAR ID EQUAL Expr SEMICOLON Prog (Let(ID, Expr, Prog))
    | DeclFun (DeclFun)

DeclFun: FUN ID Args EQUAL Expr SEMICOLON Prog (Let(ID, makeAnon(Args, Expr), Prog))
       | FUN REC ID Args COLON Type EQUAL Expr SEMICOLON Prog (makeFun(ID, Args, Type, Expr, Prog))
       | AnonFun (AnonFun)

AnonFun: FN Args ARROWFUNCTION Expr END (makeAnon(Args, Expr))

Expr: UExpr (UExpr)
    | Cond (Cond)
    | AppExpr (AppExpr)
    | LogicalExpr (LogicalExpr)
    | AlgebricExpr (AlgebricExpr)
    | ComparativExpr (ComparativExpr)
    | NotExpr (NotExpr)
    | Const (Const)
    | MATCH Expr WITH MatchExpr (Match(Expr, MatchExpr))
    | HD Expr (Prim1("hd", Expr1))
    | TL Expr (Prim1("tl", Expr1))
    | PRINT Expr (Prim1("print", Expr1))
    | LPAR RPAR (List [])
    | LPAR Type LSBRAC RSBRAC RPAR (ESeq(Type))

Cond: IF Expr THEN Expr ELSE Expr (If(Expr1, Expr2, Expr3))

Const: Number(ConI(Number))
    | TRUE (ConB(true))
    | FALSE (ConB(false))

LogicalExpr: Expr AND Expr (Prim2("&&", Expr1, Expr2))


AlgebricExpr: Expr PLUS Expr (Prim2("+", Expr1, Expr2))
            | Expr MINUS Expr (Prim2("-", Expr1, Expr2))
            | Expr MULTI Expr (Prim2("*", Expr1, Expr2))
            | Expr DIV Expr (Prim2("/", Expr1, Expr2))

ComparativExpr: Expr EQUAL Expr (Prim2("=", Expr1, Expr2))
              | Expr DIFF Expr (Prim2("!=", Expr1, Expr2))
              | Expr LSTHAN Expr (Prim2("<", Expr1, Expr2))
              | Expr LSEQTHAN Expr (Prim2("<=", Expr1, Expr2))
              | Expr ADDTL Expr (Prim2("::", Expr1, Expr2))
              | Expr SEMICOLON Expr (Prim2(";", Expr1, Expr2))
              | Expr LSBRAC Number RSBRAC (Item(Number, Expr1))
              | ISEQUAL Expr (Prim1("ise", Expr1))

NotExpr: NOT Expr (Prim1("!", Expr1))
       | MINUS Expr (Prim1("-", Expr1))

MatchExpr:END ([])
  | PIPE KeyExpr ARROW Expr MatchExpr ((KeyExpr, Expr)::MatchExpr)

UExpr: ID (Var(ID))
     | LBRA Prog RBRA (Prog)
     | LPAR Expr RPAR (Expr)
     | LPAR Comps RPAR (List Comps)


AppExpr: UExpr UExpr (Call(UExpr1, UExpr2))
       | AppExpr UExpr (Call(AppExpr, UExpr))

Comps: Expr COMMA Expr (Expr1::Expr2::[])
     | Expr COMMA Comps (Expr::Comps)

KeyExpr: Expr (SOME(Expr))
        | UNDERSCORE (NONE)

Args: LPAR RPAR ([])
    | LPAR Params RPAR (Params)

Params: TypedVar (TypedVar::[])
      | TypedVar COMMA Params (TypedVar::Params)

TypedVar: Type ID (Type, ID)

Type: NIL (ListT [])
    | BOOL (BoolT)
    | INT (IntT)
    | LPAR Type RPAR (Type)
    | LPAR Types RPAR (ListT (Types))
    | LSBRAC Type RSBRAC (SeqT(Type))
    | Type ARROW Type (FunT(Type1, Type2))


Types: Type COMMA Type (Type1::Type2::[])
     | Type COMMA Types (Type::Types)
