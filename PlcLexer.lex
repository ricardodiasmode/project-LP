(* Plc Lexer *)

(* User declarations *)

open Tokens
type pos = int
type slvalue = Tokens.svalue
type ('a,'b) token = ('a,'b) Tokens.token
type lexresult = (slvalue, pos)token

val pos = ref 0

fun keyWords (s, lpos, rpos) =
    case s of
          "Bool" => BOOL(lpos, rpos)
        | "Int" => INT(lpos, rpos)
        | "if" => IF(lpos, rpos)
        | "then" => THEN(lpos, rpos)
        | "else" => ELSE(lpos, rpos)
        | "false" => FALSE(lpos, rpos)
        | "true" => TRUE(lpos, rpos)
        | "var" => VAR(lpos, rpos)
        | "end" => END(lpos, rpos)
        | "fn" => FN(lpos, rpos)
        | "fun" => FUN(lpos, rpos)
        | "hd" => HD(lpos, rpos)
        | "ise" => ISEQUAL(lpos, rpos)
        | "match" => MATCH(lpos, rpos)
        | "Nil" => NIL(lpos, rpos)
        | "print" => PRINT(lpos, rpos)
        | "rec" => REC(lpos, rpos)
        | "tl" => TL(lpos, rpos)
        | "with" => WITH(lpos, rpos)
        | "_" => UNDERSCORE(lpos, rpos)
        | _ => Name(s, lpos, rpos)


(* Convert a str to an int *)
fun strToInt s =
    case Int.fromString s of
         SOME i => i
       | NONE => raise Fail ("Could not convert string '" ^ s ^ "' to int!")

(* A function to print a message error on the screen. *)
val error = fn x => TextIO.output(TextIO.stdOut, x ^ "\n")
val lineNumber = ref 0

(* Get the current line being read. *)
fun getLineAsString() =
    let
        val lineNum = !lineNumber
    in
        Int.toString lineNum
    end

(* Define what to do when the end of the file is reached. *)
fun eof () = Tokens.EOF(0,0)

(* Initialize the lexer. *)
fun init() = ()
%%
%header (functor PlcLexerFun(structure Tokens: PlcParser_TOKENS));

number = [0-9];
whitespace = [\ \t];
name = [a-zA-Z_][a-zA-Z_0-9]*;

%%
\n => (lineNumber := !lineNumber + 1; lex());
{whitespace}+ => (lex());
{number}+ => (Number(strToInt(yytext), !pos, !pos));
{name} => (keyWords(yytext, !pos, !pos));
"!" => (NOT(!pos, !pos)); 
"&&" => (AND(!pos, !pos));
"=" => (EQUAL(!pos, !pos));
"!=" => (DIFF(!pos, !pos));
"+" => (PLUS(!pos, !pos));
"-" => (MINUS(!pos, !pos));
"*" => (MULTI(!pos, !pos));
"/" => (DIV(!pos, !pos));
"," => (COMMA(!pos, !pos));
";" => (SEMICOLON(!pos, !pos));
":" => (COLON(!pos, !pos));
"::" => (ADDEOP(!pos, !pos));
"<" => (LSTHAN(!pos, !pos));
"<=" => (LSEQTHAN(!pos, !pos));
"[" => (LSBRAC(!pos, !pos));
"]" => (RSBRAC(!pos, !pos));
"{" => (LBRA(!pos, !pos));
"}" => (RBRA(!pos, !pos));
"(" => (LPAR(!pos, !pos));
")" => (RPAR(!pos, !pos));
"|" => (PIPE(!pos, !pos));
"->" => (ARROW(!pos, !pos));
"=>" => (ARROWFUNCTION(!pos, !pos));
"_" => (UNDERSCORE(!pos, !pos));
. => (error("\n*** Lexer error: character invalid ***\n");
      raise Fail("Lexer error: character invalid " ^ yytext));