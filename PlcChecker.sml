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
		| Match(e1, e2) =>
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
							val rt = teval (eval e2) env
						in
							if rt = ft then FunT (teval e2 env, ft)
							else raise WrongRetType
						end
				end

		| _   =>  raise UnknownType
