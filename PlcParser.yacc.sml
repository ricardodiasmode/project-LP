functor PlcParserLrValsFun(structure Token : TOKEN)
 : sig structure ParserData : PARSER_DATA
       structure Tokens : PlcParser_TOKENS
   end
 = 
struct
structure ParserData=
struct
structure Header = 
struct

end
structure LrTable = Token.LrTable
structure Token = Token
local open LrTable in 
val table=let val actionRows =
"\
\\001\000\001\000\048\000\005\000\048\000\006\000\048\000\007\000\045\000\
\\008\000\045\000\009\000\045\000\010\000\045\000\011\000\045\000\
\\012\000\048\000\013\000\045\000\014\000\012\000\016\000\048\000\
\\017\000\045\000\021\000\048\000\000\000\
\\001\000\001\000\011\000\005\000\010\000\006\000\009\000\007\000\018\000\
\\008\000\017\000\009\000\038\000\010\000\015\000\011\000\014\000\
\\012\000\007\000\013\000\013\000\014\000\012\000\016\000\006\000\
\\021\000\005\000\000\000\
\\001\000\001\000\011\000\005\000\010\000\006\000\009\000\009\000\008\000\
\\012\000\007\000\016\000\006\000\021\000\005\000\000\000\
\\001\000\005\000\010\000\006\000\009\000\009\000\008\000\012\000\007\000\
\\015\000\034\000\016\000\006\000\021\000\005\000\000\000\
\\001\000\005\000\010\000\006\000\009\000\009\000\008\000\012\000\007\000\
\\016\000\006\000\021\000\005\000\000\000\
\\001\000\007\000\018\000\008\000\017\000\009\000\016\000\010\000\015\000\
\\011\000\014\000\013\000\013\000\014\000\012\000\022\000\030\000\000\000\
\\001\000\007\000\018\000\008\000\017\000\009\000\016\000\010\000\015\000\
\\011\000\014\000\013\000\013\000\014\000\033\000\000\000\
\\001\000\007\000\031\000\000\000\
\\001\000\008\000\035\000\000\000\
\\001\000\015\000\022\000\000\000\
\\001\000\017\000\000\000\000\000\
\\041\000\007\000\018\000\008\000\017\000\009\000\016\000\010\000\015\000\
\\011\000\014\000\013\000\013\000\014\000\012\000\000\000\
\\042\000\000\000\
\\043\000\000\000\
\\044\000\000\000\
\\045\000\014\000\012\000\000\000\
\\046\000\014\000\012\000\000\000\
\\047\000\014\000\012\000\000\000\
\\048\000\014\000\012\000\000\000\
\\049\000\014\000\012\000\000\000\
\\050\000\014\000\012\000\000\000\
\\051\000\014\000\012\000\000\000\
\\052\000\014\000\012\000\000\000\
\\053\000\000\000\
\\054\000\000\000\
\\055\000\000\000\
\\056\000\000\000\
\"
val actionRowNumbers =
"\002\000\011\000\012\000\004\000\
\\025\000\004\000\004\000\024\000\
\\023\000\009\000\004\000\004\000\
\\004\000\004\000\004\000\004\000\
\\004\000\005\000\014\000\015\000\
\\007\000\022\000\016\000\019\000\
\\020\000\018\000\017\000\021\000\
\\026\000\004\000\006\000\003\000\
\\008\000\004\000\001\000\013\000\
\\004\000\000\000\010\000"
val gotoT =
"\
\\001\000\038\000\002\000\002\000\003\000\001\000\000\000\
\\000\000\
\\000\000\
\\003\000\017\000\000\000\
\\000\000\
\\003\000\018\000\000\000\
\\003\000\019\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\003\000\021\000\000\000\
\\003\000\022\000\000\000\
\\003\000\023\000\000\000\
\\003\000\024\000\000\000\
\\003\000\025\000\000\000\
\\003\000\026\000\000\000\
\\003\000\027\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\003\000\030\000\000\000\
\\000\000\
\\003\000\021\000\000\000\
\\000\000\
\\003\000\034\000\000\000\
\\001\000\035\000\002\000\002\000\003\000\001\000\000\000\
\\000\000\
\\003\000\037\000\000\000\
\\000\000\
\\000\000\
\"
val numstates = 39
val numrules = 16
val s = ref "" and index = ref 0
val string_to_int = fn () => 
let val i = !index
in index := i+2; Char.ord(String.sub(!s,i)) + Char.ord(String.sub(!s,i+1)) * 256
end
val string_to_list = fn s' =>
    let val len = String.size s'
        fun f () =
           if !index < len then string_to_int() :: f()
           else nil
   in index := 0; s := s'; f ()
   end
val string_to_pairlist = fn (conv_key,conv_entry) =>
     let fun f () =
         case string_to_int()
         of 0 => EMPTY
          | n => PAIR(conv_key (n-1),conv_entry (string_to_int()),f())
     in f
     end
val string_to_pairlist_default = fn (conv_key,conv_entry) =>
    let val conv_row = string_to_pairlist(conv_key,conv_entry)
    in fn () =>
       let val default = conv_entry(string_to_int())
           val row = conv_row()
       in (row,default)
       end
   end
val string_to_table = fn (convert_row,s') =>
    let val len = String.size s'
        fun f ()=
           if !index < len then convert_row() :: f()
           else nil
     in (s := s'; index := 0; f ())
     end
local
  val memo = Array.array(numstates+numrules,ERROR)
  val _ =let fun g i=(Array.update(memo,i,REDUCE(i-numstates)); g(i+1))
       fun f i =
            if i=numstates then g i
            else (Array.update(memo,i,SHIFT (STATE i)); f (i+1))
          in f 0 handle General.Subscript => ()
          end
in
val entry_to_action = fn 0 => ACCEPT | 1 => ERROR | j => Array.sub(memo,(j-2))
end
val gotoT=Array.fromList(string_to_table(string_to_pairlist(NT,STATE),gotoT))
val actionRows=string_to_table(string_to_pairlist_default(T,entry_to_action),actionRows)
val actionRowNumbers = string_to_list actionRowNumbers
val actionT = let val actionRowLookUp=
let val a=Array.fromList(actionRows) in fn i=>Array.sub(a,i) end
in Array.fromList(List.map actionRowLookUp actionRowNumbers)
end
in LrTable.mkLrTable {actions=actionT,gotos=gotoT,numRules=numrules,
numStates=numstates,initialState=STATE 0}
end
end
local open Header in
type pos = int
type arg = unit
structure MlyValue = 
struct
datatype svalue = VOID | ntVOID of unit ->  unit
 | Number of unit ->  (int) | Name of unit ->  (string)
 | Expr of unit ->  (expr) | Declar of unit ->  (expr)
 | Prog of unit ->  (expr)
end
type svalue = MlyValue.svalue
type result = expr
end
structure EC=
struct
open LrTable
infix 5 $$
fun x $$ y = y::x
val is_keyword =
fn _ => false
val preferred_change : (term list * term list) list = 
nil
val noShift = 
fn (T 16) => true | _ => false
val showTerminal =
fn (T 0) => "VAR"
  | (T 1) => "INT"
  | (T 2) => "BOOL"
  | (T 3) => "NIL"
  | (T 4) => "TRUE"
  | (T 5) => "FALSE"
  | (T 6) => "EQUAL"
  | (T 7) => "PLUS"
  | (T 8) => "MINUS"
  | (T 9) => "DIV"
  | (T 10) => "MULTI"
  | (T 11) => "NOT"
  | (T 12) => "AND"
  | (T 13) => "SEMICOLON"
  | (T 14) => "Name"
  | (T 15) => "Number"
  | (T 16) => "EOF"
  | (T 17) => "Prim1"
  | (T 18) => "Prim2"
  | (T 19) => "ELSE"
  | (T 20) => "OPENPARENT"
  | (T 21) => "CLOSEPARENT"
  | _ => "bogus-term"
local open Header in
val errtermvalue=
fn _ => MlyValue.VOID
end
val terms : term list = nil
 $$ (T 21) $$ (T 20) $$ (T 19) $$ (T 18) $$ (T 17) $$ (T 16) $$ (T 13)
 $$ (T 12) $$ (T 11) $$ (T 10) $$ (T 9) $$ (T 8) $$ (T 7) $$ (T 6) $$ 
(T 5) $$ (T 4) $$ (T 3) $$ (T 2) $$ (T 1) $$ (T 0)end
structure Actions =
struct 
exception mlyAction of int
local open Header in
val actions = 
fn (i392,defaultPos,stack,
    (()):arg) =>
case (i392,stack)
of  ( 0, ( ( _, ( MlyValue.Expr Expr1, Expr1left, Expr1right)) :: 
rest671)) => let val  result = MlyValue.Prog (fn _ => let val  (Expr
 as Expr1) = Expr1 ()
 in (Expr)
end)
 in ( LrTable.NT 0, ( result, Expr1left, Expr1right), rest671)
end
|  ( 1, ( ( _, ( MlyValue.Declar Declar1, Declar1left, Declar1right))
 :: rest671)) => let val  result = MlyValue.Prog (fn _ => let val  (
Declar as Declar1) = Declar1 ()
 in (Declar)
end)
 in ( LrTable.NT 0, ( result, Declar1left, Declar1right), rest671)
end
|  ( 2, ( ( _, ( MlyValue.Prog Prog1, _, Prog1right)) :: ( _, ( 
MlyValue.Expr Expr2, _, _)) :: _ :: ( _, ( MlyValue.Name Name2, _, _))
 :: _ :: ( _, ( MlyValue.Expr Expr1, _, _)) :: _ :: ( _, ( 
MlyValue.Name Name1, _, _)) :: ( _, ( _, VAR1left, _)) :: rest671)) =>
 let val  result = MlyValue.Declar (fn _ => let val  Name1 = Name1 ()
 val  Expr1 = Expr1 ()
 val  Name2 = Name2 ()
 val  Expr2 = Expr2 ()
 val  Prog1 = Prog1 ()
 in (Let(Name1, Expr1, Prim2("+", Var(Name2), Expr2)))
end)
 in ( LrTable.NT 1, ( result, VAR1left, Prog1right), rest671)
end
|  ( 3, ( ( _, ( MlyValue.Expr Expr1, _, Expr1right)) :: ( _, ( _, 
NOT1left, _)) :: rest671)) => let val  result = MlyValue.Expr (fn _ =>
 let val  Expr1 = Expr1 ()
 in (Prim1("!", Expr1))
end)
 in ( LrTable.NT 2, ( result, NOT1left, Expr1right), rest671)
end
|  ( 4, ( ( _, ( MlyValue.Expr Expr1, _, Expr1right)) :: ( _, ( _, 
MINUS1left, _)) :: rest671)) => let val  result = MlyValue.Expr (fn _
 => let val  Expr1 = Expr1 ()
 in (Prim1("-", Expr1))
end)
 in ( LrTable.NT 2, ( result, MINUS1left, Expr1right), rest671)
end
|  ( 5, ( ( _, ( MlyValue.Expr Expr2, _, Expr2right)) :: _ :: ( _, ( 
MlyValue.Expr Expr1, Expr1left, _)) :: rest671)) => let val  result = 
MlyValue.Expr (fn _ => let val  Expr1 = Expr1 ()
 val  Expr2 = Expr2 ()
 in (Prim2("&&", Expr1, Expr2))
end)
 in ( LrTable.NT 2, ( result, Expr1left, Expr2right), rest671)
end
|  ( 6, ( ( _, ( MlyValue.Expr Expr2, _, Expr2right)) :: _ :: ( _, ( 
MlyValue.Expr Expr1, Expr1left, _)) :: rest671)) => let val  result = 
MlyValue.Expr (fn _ => let val  Expr1 = Expr1 ()
 val  Expr2 = Expr2 ()
 in (Prim2("+", Expr1, Expr2))
end)
 in ( LrTable.NT 2, ( result, Expr1left, Expr2right), rest671)
end
|  ( 7, ( ( _, ( MlyValue.Expr Expr2, _, Expr2right)) :: _ :: ( _, ( 
MlyValue.Expr Expr1, Expr1left, _)) :: rest671)) => let val  result = 
MlyValue.Expr (fn _ => let val  Expr1 = Expr1 ()
 val  Expr2 = Expr2 ()
 in (Prim2("-", Expr1, Expr2))
end)
 in ( LrTable.NT 2, ( result, Expr1left, Expr2right), rest671)
end
|  ( 8, ( ( _, ( MlyValue.Expr Expr2, _, Expr2right)) :: _ :: ( _, ( 
MlyValue.Expr Expr1, Expr1left, _)) :: rest671)) => let val  result = 
MlyValue.Expr (fn _ => let val  Expr1 = Expr1 ()
 val  Expr2 = Expr2 ()
 in (Prim2("*", Expr1, Expr2))
end)
 in ( LrTable.NT 2, ( result, Expr1left, Expr2right), rest671)
end
|  ( 9, ( ( _, ( MlyValue.Expr Expr2, _, Expr2right)) :: _ :: ( _, ( 
MlyValue.Expr Expr1, Expr1left, _)) :: rest671)) => let val  result = 
MlyValue.Expr (fn _ => let val  Expr1 = Expr1 ()
 val  Expr2 = Expr2 ()
 in (Prim2("/", Expr1, Expr2))
end)
 in ( LrTable.NT 2, ( result, Expr1left, Expr2right), rest671)
end
|  ( 10, ( ( _, ( MlyValue.Expr Expr2, _, Expr2right)) :: _ :: ( _, ( 
MlyValue.Expr Expr1, Expr1left, _)) :: rest671)) => let val  result = 
MlyValue.Expr (fn _ => let val  Expr1 = Expr1 ()
 val  Expr2 = Expr2 ()
 in (Prim2("=", Expr1, Expr2))
end)
 in ( LrTable.NT 2, ( result, Expr1left, Expr2right), rest671)
end
|  ( 11, ( ( _, ( MlyValue.Expr Expr2, _, Expr2right)) :: _ :: ( _, ( 
MlyValue.Expr Expr1, Expr1left, _)) :: rest671)) => let val  result = 
MlyValue.Expr (fn _ => let val  Expr1 = Expr1 ()
 val  Expr2 = Expr2 ()
 in (Prim2(";", Expr1, Expr2))
end)
 in ( LrTable.NT 2, ( result, Expr1left, Expr2right), rest671)
end
|  ( 12, ( ( _, ( _, TRUE1left, TRUE1right)) :: rest671)) => let val  
result = MlyValue.Expr (fn _ => (ConB(true)))
 in ( LrTable.NT 2, ( result, TRUE1left, TRUE1right), rest671)
end
|  ( 13, ( ( _, ( _, FALSE1left, FALSE1right)) :: rest671)) => let
 val  result = MlyValue.Expr (fn _ => (ConB(false)))
 in ( LrTable.NT 2, ( result, FALSE1left, FALSE1right), rest671)
end
|  ( 14, ( ( _, ( MlyValue.Number Number1, Number1left, Number1right))
 :: rest671)) => let val  result = MlyValue.Expr (fn _ => let val  (
Number as Number1) = Number1 ()
 in (ConI(Number))
end)
 in ( LrTable.NT 2, ( result, Number1left, Number1right), rest671)
end
|  ( 15, ( ( _, ( _, _, CLOSEPARENT1right)) :: ( _, ( MlyValue.Expr 
Expr1, _, _)) :: ( _, ( _, OPENPARENT1left, _)) :: rest671)) => let
 val  result = MlyValue.Expr (fn _ => let val  Expr1 = Expr1 ()
 in (Expr1)
end)
 in ( LrTable.NT 2, ( result, OPENPARENT1left, CLOSEPARENT1right), 
rest671)
end
| _ => raise (mlyAction i392)
end
val void = MlyValue.VOID
val extract = fn a => (fn MlyValue.Prog x => x
| _ => let exception ParseInternal
	in raise ParseInternal end) a ()
end
end
structure Tokens : PlcParser_TOKENS =
struct
type svalue = ParserData.svalue
type ('a,'b) token = ('a,'b) Token.token
fun VAR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 0,(
ParserData.MlyValue.VOID,p1,p2))
fun INT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 1,(
ParserData.MlyValue.VOID,p1,p2))
fun BOOL (p1,p2) = Token.TOKEN (ParserData.LrTable.T 2,(
ParserData.MlyValue.VOID,p1,p2))
fun NIL (p1,p2) = Token.TOKEN (ParserData.LrTable.T 3,(
ParserData.MlyValue.VOID,p1,p2))
fun TRUE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 4,(
ParserData.MlyValue.VOID,p1,p2))
fun FALSE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 5,(
ParserData.MlyValue.VOID,p1,p2))
fun EQUAL (p1,p2) = Token.TOKEN (ParserData.LrTable.T 6,(
ParserData.MlyValue.VOID,p1,p2))
fun PLUS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 7,(
ParserData.MlyValue.VOID,p1,p2))
fun MINUS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 8,(
ParserData.MlyValue.VOID,p1,p2))
fun DIV (p1,p2) = Token.TOKEN (ParserData.LrTable.T 9,(
ParserData.MlyValue.VOID,p1,p2))
fun MULTI (p1,p2) = Token.TOKEN (ParserData.LrTable.T 10,(
ParserData.MlyValue.VOID,p1,p2))
fun NOT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 11,(
ParserData.MlyValue.VOID,p1,p2))
fun AND (p1,p2) = Token.TOKEN (ParserData.LrTable.T 12,(
ParserData.MlyValue.VOID,p1,p2))
fun SEMICOLON (p1,p2) = Token.TOKEN (ParserData.LrTable.T 13,(
ParserData.MlyValue.VOID,p1,p2))
fun Name (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 14,(
ParserData.MlyValue.Name (fn () => i),p1,p2))
fun Number (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 15,(
ParserData.MlyValue.Number (fn () => i),p1,p2))
fun EOF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 16,(
ParserData.MlyValue.VOID,p1,p2))
fun Prim1 (p1,p2) = Token.TOKEN (ParserData.LrTable.T 17,(
ParserData.MlyValue.VOID,p1,p2))
fun Prim2 (p1,p2) = Token.TOKEN (ParserData.LrTable.T 18,(
ParserData.MlyValue.VOID,p1,p2))
fun ELSE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 19,(
ParserData.MlyValue.VOID,p1,p2))
fun OPENPARENT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 20,(
ParserData.MlyValue.VOID,p1,p2))
fun CLOSEPARENT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 21,(
ParserData.MlyValue.VOID,p1,p2))
end
end
