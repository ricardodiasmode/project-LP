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
        | "else" => ELSE(lpos, rpos)
        | "false" => FALSE(lpos, rpos)
        | "true" => TRUE(lpos, rpos)
        | "var" => VAR(lpos, rpos)
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
letter = [A-Za-z];
name = [a-zA-Z_][a-zA-Z_0-9]*;
%%
\n => (lineNumber := !lineNumber + 1; EOF(!pos, !pos));
{whitespace}+ => (lex());
{number}+ => (Number(strToInt(yytext), !pos, !pos));
{name} => (keyWords(yytext, !pos, !pos));
"!" => (NOT(!pos, !pos));
"&&" => (AND(!pos, !pos));
"=" => (EQUAL(!pos, !pos));
"+" => (PLUS(!pos, !pos));
"-" => (MINUS(!pos, !pos));
"*" => (MULTI(!pos, !pos));
"/" => (DIV(!pos, !pos));
";" => (SEMICOLON(!pos, !pos));
"(" => (OPENPARENT(!pos, !pos));
")" => (CLOSEPARENT(!pos, !pos));
. => (error("\n*** Lexer error: character invalid ***\n");
      raise Fail("Lexer error: character invalid " ^ yytext));