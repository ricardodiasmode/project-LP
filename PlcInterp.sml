(* PlcInterp *)

exception Impossible
exception HDEmptySeq
exception TLEmptySeq
exception ValueNotFoundInMatch
exception NotAFunc
exception Impossible
exception HDEmptySeq
exception TLEmptySeq
exception ValueNotFoundInMatch
exception NotAFunc

(* Passa uma expressão para um valor *)
fun eval(e:expr) (env:plcVal env) : plcVal =
    case e of
        Var a => lookup env a
        | ConI a => IntV a
        | ConB a => BoolV a
        | List [] => ListV []
        | List l =>
		    let 
		    	fun evalList (h::[]) = eval h env :: []
		    	| evalList (h::t) = eval h env :: evalList t
		    	| evalList (_) = raise Impossible;
		    in
		    	ListV (evalList l)
		    end
        | ESeq a => SeqV []
        | Let(a, t1, t2) => eval t2 ((a, eval t1 env) :: env)
        | Letrec(f, argType, a, fType, e1, e2) =>  eval e2 ((f, Clos(f, a, e1, env)) :: env)
        | Anon(t, a, exp) => Clos ("", a, exp, env)
        | Call(e1, e2) =>
            let
                fun aux (List((h::[]))) = [eval h env]
                    | aux (List(h::tail)) = [eval h env] @ aux(List tail)
                    | aux (exp) = [eval exp env]
            in
                case eval e1 env of
                    Clos(name, a, exp, cEnv) =>
                        eval exp ((a, (eval e2 ([("$list", ListV (aux e2))] @ env)))::(name, eval e1 env)::cEnv)
                    | _ => raise NotAFunc
            end
        | If(e1, e2, e3) =>
            let in
                case eval e1 env of
                    BoolV true => eval e2 env
                    | BoolV false => eval e3 env
                    | _ => raise Impossible
            end
        | Match (e1, matchList) =>
            let
                fun checkCases (a, h::[]) env =
                    let in
                        case h of
                            (SOME e2, e3) =>
                                if a = eval e2 env then
                                    e3
                                else
                                    raise ValueNotFoundInMatch
                            | (NONE, e3) =>
                                e3
                    end
                    | checkCases (a, h::tail) env =
                        let in
                            case h of
                                (SOME e2, e3) =>
                                    if a = eval e2 env then
                                        e3
                                    else
                                        checkCases (a, tail) env
                            | (NONE, e3) =>
                                raise Impossible
                        end
                    | checkCases (a, _ ) env =
                        raise Impossible
            in
                eval (checkCases ((eval e1 env), matchList) env) env
            end
        | Prim1(oper, exp) =>
            let
                val t1 = eval exp env
            in
                case t1 of
                    IntV x =>
                        let in
                            case oper of
                                "-" => IntV(~x)
                                | "print" =>
                                    let
                                        val printVariable = print(val2string((IntV x)) ^ "\n")
                                    in
                                        ListV []
                                    end
                                | _ => raise Impossible
                        end
                    | BoolV x =>
                        let in
                            case oper of
                                "!" => BoolV(not x)
                                | "print" =>
                                    let
                                        val printVariable = print(val2string((BoolV x)) ^ "\n")
                                    in
                                        ListV []
                                    end
                                | _ => raise Impossible
                        end
                    | SeqV x =>
                        let in
                            case oper of
                                "hd" =>
                                    let in
                                        if x <> [] then
                                            hd x
                                        else
                                            raise HDEmptySeq
                                    end
                                | "tl" =>
                                    let in
                                        if x <> [] then
                                            SeqV (tl x)
                                        else
                                            raise TLEmptySeq
                                    end
                                | "ise" =>
                                    let in
                                        case x of
                                            [] => BoolV true
                                            | _ => BoolV false
                                    end
                                | "print" =>
                                    let
                                        val printVariable = print(list2string(val2string, x) ^ "\n")
                                    in
                                        ListV []
                                    end
                                | _ => raise Impossible
                        end
                    | ListV x =>
                        let in
                            case oper of
                                "print" =>
                                    let
                                        val printVariable = print(list2string(val2string, x) ^ "\n")
                                    in
                                        ListV []
                                    end
                                | _ => raise Impossible
                        end
                    | _ => raise Impossible
            end
       | Prim2(";", e1, e2) =>
            let
                val aux = eval e1 env
            in
                eval e2 env
            end
        | Prim2("::", e1, e2) =>
            let
                val t1 = eval e1 env
                val t2 = eval e2 env
            in
                case ((t1), (t2)) of
                    (IntV i, SeqV s) => SeqV (IntV i::s)
                    | (BoolV b, SeqV s) =>  SeqV (BoolV b::s)
                    | (ListV l, SeqV s) => SeqV (ListV l::s)
                    | _ => raise Impossible
            end
        | Prim2(oper, e1, e2) =>
            let
                val value1 = eval e1 env
                val value2 = eval e2 env
            in
                case (value1, value2) of
                    (IntV v1, IntV v2) =>
                        let in
                            case oper of
                                "+" => IntV(v1 + v2)
                                | "-" => IntV(v1 - v2)
                                | "*" => IntV(v1 * v2)
                                | "/" => IntV(v1 div v2)
                                | "<" => BoolV(v1 < v2)
                                | "<=" => BoolV(v1 <= v2)
                                | "=" => BoolV(v1 = v2)
                                | "!=" => BoolV(v1 <> v2)
                                | _ => raise Impossible
                            end
                    | (BoolV v1, BoolV v2) =>
                        let in
                            case oper of
                                "&&" => BoolV(v1 andalso v2)
                                | "=" => BoolV(v1 = v2)
                                | "!=" => BoolV(v1 <> v2)
                                | _ => raise Impossible
                        end
                    | (SeqV v1, SeqV v2) =>
                        let in
                            case oper of
                                "=" => BoolV(v1 = v2)
                                | "!=" => BoolV(v1 <> v2)
                                | _ => raise Impossible
                        end
                    | (ListV v1, ListV v2) =>
                        let in
                            case oper of
                                "=" => BoolV(v1 = v2)
                                | "!=" => BoolV(v1 <> v2)
                                | _ => raise Impossible
                        end
                    | _ => raise Impossible
            end
        | Item(i, exp) =>
            let
                fun getIndexElement(i, []) = raise Impossible
                    | getIndexElement(i, (h::[])) =
                        if i = 1 then
                            h
                        else
                            raise Impossible
                    | getIndexElement(i, (h::tail)) =
                        if i = 1 then
                            h
                        else
                            getIndexElement(i - 1, tail)
            in
                case eval exp env of
                    ListV x => getIndexElement(i, x)
                    | _ => raise Impossible
            end
fun eval (e:expr) (env:plcVal env) : plcVal =
	case e of
(**)		  ConI i => IntV i
(**)		| ConB i => BoolV i
(**)		| ESeq i => SeqV[]
(**)		| Var x => lookup env x
(**)		| Let(x, e1, e2) =>
			let
				val v = eval e1 env
				val env2 = (x,v)::env
			in
				eval e2 env2
			end
(**)		| Letrec(f, argType, a, fType, e1, e2) =>  eval e2 ((f, Clos(f, a, e1, env)) :: env)	
(**)		| Prim1(oper, exp) =>
            let
                val t1 = eval exp env
            in
                case t1 of
                    IntV x =>
                        let in
                            case oper of
                                "-" => IntV(~x)
                                | "print" =>
                                    let
                                        val printVariable = print(val2string((IntV x)) ^ "\n")
                                    in
                                        ListV []
                                    end
                                | _ => raise Impossible
                        end
                    | BoolV x =>
                        let in
                            case oper of
                                "!" => BoolV(not x)
                                | "print" =>
                                    let
                                        val printVariable = print(val2string((BoolV x)) ^ "\n")
                                    in
                                        ListV []
                                    end
                                | _ => raise Impossible
                        end
                    | SeqV x =>
                        let in
                            case oper of
                                "hd" =>
                                    let in
                                        if x <> [] then
                                            hd x
                                        else
                                            raise HDEmptySeq
                                    end
                                | "tl" =>
                                    let in
                                        if x <> [] then
                                            SeqV (tl x)
                                        else
                                            raise TLEmptySeq
                                    end
                                | "ise" =>
                                    let in
                                        case x of
                                            [] => BoolV true
                                            | _ => BoolV false
                                    end
                                | "print" =>
                                    let
                                        val printVariable = print(list2string(val2string, x) ^ "\n")
                                    in
                                        ListV []
                                    end
                                | _ => raise Impossible
                        end
                    | ListV x =>
                        let in
                            case oper of
                                "print" =>
                                    let
                                        val printVariable = print(list2string(val2string, x) ^ "\n")
                                    in
                                        ListV []
                                    end
                                | _ => raise Impossible
                        end
                    | _ => raise Impossible
            end	
			| Prim2(";", e1, e2) => 
				let
					val aux = eval e1 env
				in
					eval e2 env
				end
			| Prim2("::", e1, e2) =>
				let
					val t1 = eval e1 env
					val t2 = eval e2 env
				in
					case ((t1), (t2)) of
						(IntV i, SeqV s) => SeqV (IntV i::s)
						| (BoolV b, SeqV s) =>  SeqV (BoolV b::s)
						| (ListV l, SeqV s) => SeqV (ListV l::s)
						| _ => raise Impossible
				end	
			| Prim2(opr, e1, e2) =>
				let
					val v1 = eval e1 env
					val v2 = eval e2 env
				in
					case (opr, v1, v2) of
						 ("*" , IntV i1, IntV i2) => IntV (i1 * i2)
						| ("/" , IntV i1, IntV i2) => IntV (i1 div i2)
						| ("+" , IntV i1, IntV i2) => IntV (i1 + i2)
						| ("-" , IntV i1, IntV i2) => IntV (i1 - i2)
						| ("<" , IntV i1, IntV i2) => BoolV (i1 < i2)
						| ("<=" , IntV i1, IntV i2) => BoolV (i1 <= i2)
						| ("=" , IntV i1, IntV i2) => BoolV (i1 = i2)
						| ("!=" , IntV i1, IntV i2) => BoolV (i1 <> i2)
						| ("&&" , BoolV i1, BoolV i2) => BoolV (i1 andalso i2)
						| ("=" , BoolV i1, BoolV i2) => BoolV (i1 = i2)
						| ("!=" , BoolV i1, BoolV i2) => BoolV (i1 <> i2)
						| ("=" , SeqV i1, SeqV i2) => BoolV (i1 = i2)
						| ("!=" , SeqV i1, SeqV i2) => BoolV (i1 <> i2)
						| ("=" , ListV i1, ListV i2) => BoolV (i1 = i2)
						| ("!=" , ListV i1, ListV i2) => BoolV (i1 <> i2)
						| _ => raise Impossible
				end
(**)		| If (cond, e1, e2) =>
			let in
				case eval cond env of
					BoolV true => eval e1 env
				|	BoolV false => eval e2 env
				| _ => raise Impossible
			end
(**)		| List [] => ListV []
(**)		| List e =>
			let 
				fun makeList (h::[]) = eval h env :: []
				| makeList (h::t) = eval h env :: makeList t
				| makeList (_) = raise Impossible;
			in
				ListV (makeList e)
			end
(**)		| Item(i, exp) =>
            let
                fun getIndexElement(i, []) = raise Impossible
                    | getIndexElement(i, (h::[])) =
                        if i = 1 then
                            h
                        else
                            raise Impossible
                    | getIndexElement(i, (h::tail)) =
                        if i = 1 then
                            h
                        else
                            getIndexElement(i - 1, tail)
            in
                case eval exp env of
                    ListV x => getIndexElement(i, x)
                    | _ => raise Impossible
            end
(**)		| Anon (ft, aenv, e2) => Clos("", aenv, e2, env)
(**)		| Call(e1, e2) =>
			let
				fun aux (List((h::[]))) = [eval h env]
					| aux (List(h::tail)) = [eval h env] @ aux(List tail)
					| aux (exp) = [eval exp env]
			in
				case eval e1 env of
					Clos(name, a, exp, cEnv) =>
						eval exp ((a, (eval e2 ([("$list", ListV (aux e2))] @ env)))::(name, eval e1 env)::cEnv)
					| _ => raise NotAFunc
			end
(**)		| Match (e1, matchList) =>
            let
                fun checkCases (a, h::[]) env =
                    let in
                        case h of
                            (SOME e2, e3) =>
                                if a = eval e2 env then
                                    e3
                                else
                                    raise ValueNotFoundInMatch
                            | (NONE, e3) =>
                                e3
                    end
                    | checkCases (a, h::tail) env =
                        let in
                            case h of
                                (SOME e2, e3) =>
                                    if a = eval e2 env then
                                        e3
                                    else
                                        checkCases (a, tail) env
                            | (NONE, e3) =>
                                raise Impossible
                        end
                    | checkCases (a, _ ) env =
                        raise Impossible
            in
                eval (checkCases ((eval e1 env), matchList) env) env
            end