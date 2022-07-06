(* PlcChecker *)

exception EmptySeq
exception UnknownType
exception NotEqTypes
exception WrongRetType
exception DiffBrTypes
exception IfCondNotBool
exception NoMatchResults
exception MatchResTypeDiff
exception MatchCondTypesDiff
exception CallTypeMisM
exception NotFunc
exception ListOutOfRange
exception OpNonList
exception CouldNotGetFunTypes
exception ListTypesDiff

fun getFunTypes (f:FunT) (env: plcType env) : (plcType, plcType) =
	case f of
		  FunT(t1, t2) => (teval t1 env, teval t2 env)
		| _ => raise CouldNotGetFunTypes

fun teval (e:expr) (env: plcType env) : plcType =
	case e of
		  ConI => IntT
		| ConB => BoolT
		| ESeq(seq) => if teval seq env = SeqT then teval (hd seq) env else EmptySeq
		| Var x => lookup env x
		| Prim1(opr, e1) =>
				let
					val t1 = teval e1 env
				in
					case (opr, t1) of
						 ("print", _) => ListT []
						| _ => raise UnknownType
				end
		| Prim2(opr, e1, e2) =>
				let
					val t1 = teval e1 env
					val t2 = teval e2 env
				in
					case (opr, t1, t2) of
					 ("*" , IntT, IntT) => IntT
					| ("/" , IntT, IntT) => IntT
					| ("+" , IntT, IntT) => IntT
					| ("-" , IntT, IntT) => IntT
					| (";" , _ , _)    => t2
					| _   =>  raise UnknownType
				end
		| Let(x, e1, e2) =>
			let
				val t = teval e1 env
				val env' = (x,t)::env
			in
				teval e2 env'
			end
		| Letrec(f, tf, v, tv, e1, e2) =>
			let
				val t1 = teval e1 env
				val tv1 = teval tv env
				val env' = (v,tv1)::env
				val t2 = teval e2 env'
			in
				if t1 != tf then raise WrongRetType
				else if t1 != tv then raise CallTypeMisM
					 else t2
			end
        | If(cond, e1, e2) =>
            let
                val condType = teval cond env
                val e1Type = teval e1 env
                val e2type = teval e2 env
            in
                if condType = BoolT then 
                                        if((teval e1Type env) = (teval e2Type env)) then teval e1Type
                                        else raise DiffBrTypes
                else raise IfCondNotBool
			end
		| Match (e1, e2) =>
			if e2 = [] then raise NoMatchResults
			else
				let
					val t1 = teval e1 env
					val t2 = teval e1 env
				in
					if tl e2 = [] then 
									if t1 = t2 then t1
									else raise MatchCondTypesDiff
					else 
						let 
							val t3 = teval (hd (tl e2)) env
						in
							if t3 = teval Match(hd e2, tl e2) env then t3
							else raise MatchResTypeDiff
						end
				end
		| Call(e1, e2) =>
			if teval e1 env != FunT then raise NotFunc
			else
				let
					val (ft, pt) = getFunTypes e1
				in
					if teval e2 env != teval pt env then raise CallTypeMisM
					else
						let
							val rt = teval e2 env
						in
							if rt = ft then FunT (teval e2 env, ft)
							else raise WrongRetType
						end
				end
		| List(lhd::ltl) =>
			if lhd = [] then ListOutOfRange
			else
				let
					val thd = teval lhd env
					val ttl = if tl ltl = [] then teval (hd ltl) env
							else teval (List(ltl)) env
				in
					if thd = ttl then ListT(thd)
					else raise ListTypesDiff
				end
		| Item(idx, l) =>
			if idx < 0 then ListOutOfRange
			else
				if teval l env != ListT then OpNonList
				else 
					if idx = 0 then teval (hd l) env
					else 
						if tl l = [] then ListOutOfRange
						else teval Item(idx-1, tl l) env
		| Anon(ft, aenv, e2) =>
				let
					val rt = teval e2 aenv
				in
					if rt != e1 then raise WrongRetType
					else FunT(rt, ft)
				end
		| _   =>  raise UnknownType
