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
(*)
fun getFunTypes (f:FunT) (env: plcType env) : (plcType, plcType) =
	case f of
		  FunT(t1, t2) => (teval t1 env, teval t2 env)
		| _ => raise CouldNotGetFunTypes
*)
fun teval (e:expr) (env: plcType env) : plcType =
	case e of
		  ConI _ => IntT
		| ConB _ => BoolT
		| ESeq(seq) => let in
							case seq of
								SeqT elem => SeqT elem
								| _ => raise EmptySeq
						end
		| Var x => lookup env x
		| Prim2(opr, e1, e2) => 
				let
					val t1 = teval e1 env;
					val t2 = teval e2 env;
				in case (opr) of
					("&&") => if ((t1 = t2) andalso (t1 = BoolT)) then BoolT else raise NotEqTypes
					| ("+") => if ((t1 = t2) andalso (t1 = IntT)) then IntT else raise NotEqTypes
					| ("-") => if ((t1 = t2) andalso (t1 = IntT)) then IntT else raise NotEqTypes
					| ("*") => if ((t1 = t2) andalso (t1 = IntT)) then IntT else raise NotEqTypes
					| ("/") => if ((t1 = t2) andalso (t1 = IntT)) then IntT else raise NotEqTypes
					| ("=") =>
							let in
								case (t1) of
									IntT =>
										if (t2) = IntT andalso (t1) = (t2) then
											BoolT
										else
											raise NotEqTypes
									| BoolT =>
										if (t2) = BoolT andalso (t1) = (t2) then
											BoolT
										else
											raise NotEqTypes
									| SeqT t =>
										let in
											case t of
												BoolT => BoolT
												| IntT => BoolT
												| ListT([]) => BoolT
												| _ => raise NotEqTypes
										end
									| ListT([]) =>
										if (t2) = ListT([]) andalso (t1) = (t2) then
											BoolT
										else
											raise NotEqTypes
									| ListT(types) =>
										let
											val aux = map(
												fn(t) =>
													case t of
														BoolT => BoolT
														| IntT => IntT
														| ListT([]) => ListT([])
														| _ => raise NotEqTypes
											) types
										in
											BoolT
										end
									| _ => raise NotEqTypes
							end
					| ("!=") =>
							let in
								case (t1) of
									IntT =>
										if (t2) = IntT andalso (t1) = (t2) then
											BoolT
										else
											raise NotEqTypes
									| BoolT =>
										if (t2) = BoolT andalso (t1) = (t2) then
											BoolT
										else
											raise NotEqTypes
									| SeqT t =>
										let in
											case t of
												BoolT => BoolT
												| IntT => BoolT
												| ListT([]) => BoolT
												| _ => raise NotEqTypes
										end
									| ListT([]) =>
										if (t2) = ListT([]) andalso (t1) = (t2) then
											BoolT
										else
											raise NotEqTypes
									| ListT(types) =>
										let
											val aux = map(
												fn(t) =>
													case t of
														BoolT => BoolT
														| IntT => IntT
														| ListT([]) => ListT([])
														| _ => raise NotEqTypes
											) types
										in
											BoolT
										end
									| _ => raise NotEqTypes
							end
					| ("<") => if ((t1 = t2) andalso (t1 = IntT)) then BoolT else raise NotEqTypes
					| ("<=") => if ((t1 = t2) andalso (t1 = IntT)) then BoolT else raise NotEqTypes
					| ("::") =>
							let in
								case (t1, t2) of
									(IntT, ListT []) =>
										SeqT IntT
									| (IntT, SeqT t) =>
										if t = IntT then
											SeqT t
										else
											raise NotEqTypes
									| (BoolT, ListT []) =>
										SeqT BoolT
									| (BoolT, SeqT t) =>
										if t = BoolT then
											SeqT BoolT
										else
											raise NotEqTypes
									| (ListT t, ListT []) => SeqT(ListT t)
									| (ListT t, SeqT s) =>
										if s = (ListT t) then
											SeqT s
										else
											raise NotEqTypes
									| _ => raise UnknownType
								end
					| (";") => if (t1 = t2) then t1 else raise NotEqTypes
					| _ => raise UnknownType
				end
		| Let(x, e1, e2) =>
			let
				val t = teval e1 env
				val env' = (x,t)::env
			in
				teval e2 env'
			end
		| Letrec(f, at, arg, ft, e1, e2) =>
            let
                val t1 = teval e1 ((f, FunT(at, ft))::(arg, at)::env)
                val t2 = teval e2 ((f, FunT(at, ft))::env)
            in
                if t1 = ft then
                    t2
                else
                    raise WrongRetType
            end
        | If(cond, e1, e2) =>
            let
                val condType = teval cond env
                val t1 = teval e1 env
                val t2 = teval e2 env
            in
                if condType = BoolT then 
                                        if((t1) = (t2)) then t1
                                        else raise DiffBrTypes
                else raise IfCondNotBool
			end	
		| Match(exp, cases) =>
            if cases = [] then
                raise NoMatchResults
            else
                let
                    val expType = teval exp env
                    val firstResult = (#2 (hd cases))
                    val firstResultType = teval firstResult env
                    fun readList(h::[]) =
                        let in
                            case h of
                                (SOME e, res) =>
                                    if teval e env <> expType then
                                        raise MatchCondTypesDiff
                                    else
                                        if (teval res env) <> firstResultType then
                                            raise MatchResTypeDiff
                                        else
                                            firstResultType
                                | (NONE, res) =>
                                    if (teval res env) <> firstResultType then
                                        raise MatchResTypeDiff
                                    else
                                        firstResultType
                        end
                    | readList(h::tail) =
                        let in
                            case h of
                                (SOME e, res) =>
                                    if (teval e env) <> expType then
                                        raise MatchCondTypesDiff
                                    else
                                        if (teval res env) <> firstResultType then
                                            raise MatchResTypeDiff
                                        else
                                            readList(tail)
                                | _ => raise UnknownType
                        end
                in
                    readList(cases)
                end
		(*
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
		*)
		| Call(e1, e2) =>
            let in
                case (teval e1 env) of
                    FunT(at, rt) =>
                        if (teval e2 env) = at then
                            rt
                        else if (teval e2 env) = rt then 
							raise WrongRetType
						else
                            raise CallTypeMisM
                    | _ => raise NotFunc
            end
		| Item(i, exp) =>
            let
                fun getIndexElement(i, []) = raise ListOutOfRange
                    | getIndexElement(i, (h::[])) =
                        if i = 1 then
                            h
                        else
                            raise ListOutOfRange
                    | getIndexElement(i, (h::tail)) =
                        if i = 1 then
                            h else
                        getIndexElement(i - 1, tail)
            in
                case teval exp env of
                    ListT l => getIndexElement(i, l)
                    | _ => raise OpNonList
            end
		(*
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
					val rt = teval e2 env
				in
					if rt != e1 then raise WrongRetType
					else FunT(rt, ft)
				end
		| _   =>  raise UnknownType
		*)
		|Prim1(opr, e1) => let in
				case (opr) of
						("!") => if teval e1 env = BoolT then BoolT else raise UnknownType
						| ("-") => if teval e1 env = IntT then IntT else raise UnknownType 
						| ("hd") => let in
											case (teval e1 env) of
												SeqT s => s
												| _ => raise UnknownType
										end
						| ("tl") => let in
											case (teval e1 env) of
												SeqT s => SeqT s
												| _ => raise UnknownType
										end
						| ("ise") => let in
											case (teval e1 env) of
												SeqT s => BoolT
												| _ => raise UnknownType
										end	
						| ("print") => 		let
												val t = teval e1 env
											in
												ListT []
											end
				end

        | _ => raise UnknownType