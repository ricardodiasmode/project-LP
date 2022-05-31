%%

%name PlcParser

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

%nonterm plcType | expr | plcVal

%left Prim1 Prim2

%pos int



%eop EOF

%noshift EOF

%start Prog

%%

PlcType : IntT
        | BoolT
        | 
