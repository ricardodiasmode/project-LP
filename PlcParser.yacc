%%

%name PlcParser
(*
%term ConI of int
  | ConB of bool
  | ESeq of plcType
  | Var of string
  | Let of string * expr * expr
  | Letrec of string * plcType * string * plcType * expr * expr
  | Prim1 of string * expr
  | Prim2 of string * expr * expr
  | If of expr * expr * expr
  | Match of expr * (expr option * expr) list
  | Call of expr * expr
  | List of expr list
  | Item of int * expr
  | Anon of plcType * string * expr
  | BoolV of bool
  | IntV of int
  | ListV of plcVal list
  | SeqV of plcVal list
  | Clos of string * string * expr * plcVal env
  | IntT
  | BoolT
  | FunT of plcType * plcType
  | ListT of plcType list
  | SeqT of plcType
  | EOF


%nonterm plcType | expr | plcVal

*)

%term VAR | INT | BOOL | NIL
    | TRUE | FALSE
    | EQUAL | PLUS | MINUS | DIV | MULTI
    | NOT | AND
    | SEMICOLON
    | Name of string
    | Number of Int
    | EOF

%nonterm Prog of expr
    | Declar of expr
    | Expr of expr


%left Prim1 Prim2
%left AND EQUAL MULTI DIV PLUS MINUS
%right SEMICOLON
%nonassoc NOT, Name

%pos int

%eop EOF

%noshift EOF

%start Prog

%%
(*
plcType : IntT (IntT)
        | BoolT (BoolT)
        | env (makeType(env))

expr : ConI (ConI)
     | ConB (ConB)
     | ESeq (ESeq)
     | Var (Var)
     | env expr (makeAnon(env expr))
     | Var env plcType expr expr (makeFun(Var env plcType expr expr))
     | int env expr (makeFunAux(int env expr))

plcVal : BoolV (BoolV)
       | IntV (IntV)
*)

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
    | Const (Const)

Const: TRUE (ConB(true))
     | FALSE (ConB(false))
     | Number (ConI(Number))