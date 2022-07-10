(* Plc interpreter main file *)

CM.make("$/basis.cm");
CM.make("$/ml-yacc-lib.cm");

use "Environ.sml";
use "Absyn.sml";
use "PlcParserAux.sml";
use "PlcParser.yacc.sig";
use "PlcParser.yacc.sml";
use "PlcLexer.lex.sml";

use "Parse.sml";
use "PlcInterp.sml";
use "PlcChecker.sml";

Control.Print.printLength := 1000;
Control.Print.printDepth  := 1000;
Control.Print.stringDepth := 1000;

open PlcFrontEnd;

(* Passa uma expressão e vai ser retornado uma string *)
fun run (e:expr) = 
    let
      val checkType = teval e []
      val checkValue = eval e []
    in
       (val2string checkValue) ^ " : " ^ (type2string checkType)
    end
    handle
        EmptySeq => "Erro: A sequencia de entrada nao contem nenhum elemento."
        | UnknownType => "Erro: Tipo desconhecido."
        | NotEqTypes => "Erro: Se os tipos usados na comparacao sao diferentes."
        | WrongRetType => "Erro: O tipo de retorno da função não condiz com o corpo da mesma."
        | DiffBrTypes => "Erro: Os tipos da expressao dos possiveis caminhos de um If divergem."
        | IfCondNotBool => "Erro: A condição do if nao e booleana."
        | NoMatchResults => "Erro: Nao ha resultados para a expressao match."
        | MatchResTypeDiff => "Erro: O tipo de algum dos casos em match difere dos demais."
        | MatchCondTypesDiff => "Erro: O tipo das opcoes de match difere do tipo da expressao passada para Match."
        | CallTypeMisM => "Erro: Voce esta passando para uma chamada de função um tipo diferente do qual ela suporta."
        | NotAFunc => "Erro: Voce esta tentando chamar algo que nao e uma funcao."
        | ListOutOfRange => "Erro: Tentativa de acessar um elemento fora dos limites da lista."
        | OpNonList => "Erro: Tentativa de acessar um elemento em uma expressao que nao e uma lista."
        | Impossible => "Erro: Avaliacao Impossivel."
        | HDEmptySeq => "Erro: Cabeça da Lista vazia."
        | TLEmptySeq => "Erro: Cauda da lista vazia."
        | ValueNotFoundInMatch => "Erro: Valor nao encontrado no Match."
        | SymbolNotFound => "Erro: Simbolo nao encontrado."
