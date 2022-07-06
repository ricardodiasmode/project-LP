(* PlcInterp *)

exception Impossible
exception HDEmptySeq
exception TLEmptySeq
exception ValueNotFoundInMatch
exception NotAFunc

fun makeList (l) = 
	case tl l of 
	 [] => hd l
	| _ => [hd l; makeList (tl l)]

fun eval (e:expr) (env:plcVal env) : plcVal =
	case e of
		  ConI i => IntV i
		| ConB i => BoolV i
		| ESeq i => 
			let
				val s = val2string SeqV ([hd i, eval (tl i) env])
			in
				print(s^"\n"); SeqV []
			end
		| Var x => lookup env x
		| Let(x, e1, e2) =>
			let
				val v = eval e1 env
				val env2 = (x,v)::env
			in
				eval e2 env2
			end
		| Letrec (f, tf, v, tv, e1, e2) =>
			let
				val v1 = eval e1 env
				val env' = (v,v1)::env
			in
				eval e2 env'
			end
		| Prim1(opr, e1) =>
				let
					val v1 = eval e1 env
				in
					case (opr, v1) of
							("-", IntV i) => IntV (~i)
						| ("print", _) =>
										let
											val s = val2string v1
										in
											print(s^"\n"); ListV []
										end
						| _   => raise Impossible
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
						| (";" , _ , _) => v2
						| _ => raise Impossible
				end
		| If (cond, e1, e2) =>
			if eval cond env 
				then eval e1 env
			else eval e2 env
		| List (e) =>
			case e of
				[] => ListV []
			  | _ => ListV makeList(e)
		| Item (idx, e1) =>
			if idx = 1
				then eval (hd e1) env
			else eval Item(idx-1, tl e1) env
		| Anon (ft, aenv, e2) =>
			Clos("", aenv, e2, env)
		| _ => raise Impossible

