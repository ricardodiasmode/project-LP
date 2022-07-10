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
		| Prim1("!", e) =>
            if teval e env = BoolT then BoolT else raise UnknownType
	    | Prim1("-", e) =>
            if teval e env = IntT then IntT else raise UnknownType
	    | Prim1("hd", e) =>
		    let in
		    	case (teval e env) of
		    		SeqT s => s
		    		| _ => raise UnknownType
		    end
	    | Prim1("tl", e) =>
	    	let in
	    		case (teval e env) of
	    			SeqT s => SeqT s
	    			| _ => raise UnknownType
	    	end
	    | Prim1("ise", e) =>
	    	let in
	    		case (teval e env) of
	    			SeqT s => BoolT
	    			| _ => raise UnknownType
	    	end	
	    | Prim1("print", e) =>
            let
                val t = teval e env
            in
                ListT []
            end
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
					| readList(_) = raise UnknownType
                in
                    readList(cases)
                end
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
		| List [] => ListT []
        | List l =>
		    let 
		    	fun tevalList (h::[]) = (teval h env)::[]
		    	| tevalList (h::t) = (teval h env)::(tevalList t)
		    	| tevalList (_) = []
		    in
		    	ListT (tevalList l)
		    end
		| Anon(t, a, e) => FunT(t, (teval e ((a,t)::env)))
        | _ => raise UnknownType
		