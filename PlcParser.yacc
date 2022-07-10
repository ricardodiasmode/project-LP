(*
    Implementa a gramatica da linguagem
    Os %% separam os blocos. Sao tres blocos no total
*)
(*  Neste bloco:
    Declaraçoes do usuario, como funçoes, valores
*)

%%

(*  Neste bloco:
    gramaticas terminais e nao terminais, regras de precedencia,
    simbolo inicial e final para o fluxo do token final para gramatica,
    regras de associatividade, etc
*)

%name PlcParser

%pos int

%term VAR | FUN | REC
    | IF | THEN | ELSE
    | MATCH | WITH
    | NOT | MINUS | HD | TL | ISEQUAL | PRINT
    | AND | PLUS | MULTI | DIV | EQUAL 
    | DIFF | LSTHAN | LSEQTHAN | ADDTL
    | SEMICOLON
    | COMMA | COLON
    | RSBRAC | LSBRAC
    | RBRA | LBRA
    | RPAR | LPAR
    | FN | END
    | TRUE | FALSE
    | PIPE | ARROW | ARROWFUNCTION | UNDERSCORE
    | NIL | BOOL | INT
    | ID of string | CINT of int
    | Number of int
    | EOF

%nonterm Prog of expr
    | Decl of expr
    | Expr of expr
    | UExpr of expr 
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

%right SEMICOLON ARROW
%nonassoc IF
%left ELSE
%left AND
%left EQUAL DIFF
%left LSTHAN LSEQTHAN
%right ADDTL
%left PLUS MINUS 
%left MULTI DIV
%nonassoc NOT HD TL ISEQUAL PRINT ID
%left LSBRAC

%eop EOF

%noshift EOF

%start Prog

%%
(*  Neste bloco:
    Regras
    Cria as regras que vao descrever as regras de produçao e criar as expressoes em sintaxe abstrata,
    baseada no fluxo do que esta lendo.
    "Do token sera gerado a sintaxe abstrata"
*)

Prog:Expr (Expr)
    | Decl (Decl)

Decl:VAR ID EQUAL Expr SEMICOLON Prog (Let(ID, Expr, Prog))
    | FUN ID Args EQUAL Expr SEMICOLON Prog (Let(ID, makeAnon(Args, Expr), Prog))
    | FUN REC ID Args COLON Type EQUAL Expr SEMICOLON Prog (makeFun(ID, Args, Type, Expr, Prog))

Expr:UExpr (UExpr)
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
    | Expr ADDTL Expr (Prim2("::", Expr1, Expr2))
    | Expr SEMICOLON Expr (Prim2(";", Expr1, Expr2))
    | Expr LSBRAC Number RSBRAC (Item(Number, Expr1))

UExpr:Const (Const)
    | ID (Var(ID))
    | LBRA Prog RBRA (Prog)
    | LPAR Expr RPAR (Expr)
    | LPAR Comps RPAR (List Comps)
    | FN Args ARROWFUNCTION Expr END (makeAnon(Args, Expr))

AppExpr:UExpr UExpr (Call(UExpr1, UExpr2))
    | AppExpr UExpr (Call(AppExpr, UExpr))

Const:TRUE (ConB(true))
    | FALSE (ConB(false))
    | Number (ConI(Number))
    | LPAR RPAR (List [])
    | LPAR Type LSBRAC RSBRAC RPAR (ESeq(Type))

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

TypedVar: Type ID (Type, ID)

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