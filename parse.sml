structure Parse : sig

  val parse : Token.token list -> SULC.program

end = struct

  structure T = Token
  structure S = SULC

  fun err info = raise Fail ("parse error " ^ info)
		    
  fun toksStr toks = "[" ^ String.concatWith "," (List.map T.tos toks) ^ "]"
		    
  fun nextTerm toks =
    let
      fun lp (T.Var x :: ts)  = SOME (S.Var x, ts) 
	| lp (T.Abbr a :: ts) = SOME (S.Var a, ts)
	| lp (T.ID a :: ts)   = SOME (S.ID a,  ts)
	| lp (T.Nat n :: ts)  = SOME (S.Nat n, ts)
	| lp (T.LBrack :: ts) =
	    (case lp ts of
	       SOME (t1, ts1) =>
	         (case lp ts1 of
		    SOME (t2, T.RBrack :: ts2) => SOME (S.Pair (t1, t2), ts2)
		  | _ => err "at end of pair")
	     | NONE => err "within pair")
        | lp (T.LBrace :: T.Caret :: ts) =
	    (case ts of
	       T.Var x :: T.Dot :: ts1 =>
	         (case lp ts1 of
		    SOME (t1, T.RBrace :: ts2) => SOME (S.Abs (x, t1), ts2)
		  | _ => err "at end of abstraction")
             | _ => err "within abstraction")
	| lp (T.LBrace :: T.Fst :: ts) =
	    (case lp ts of
		 SOME (t1, T.RBrace :: ts1) => SOME (S.Select1 t1, ts1)
	       | _ => err "in first")
	| lp (T.LBrace :: T.Snd :: ts) =
	    (case lp ts of
		 SOME (t1, T.RBrace :: ts1) => SOME (S.Select2 t1, ts1)
	       | _ => err "in second")
	| lp (T.LBrace :: ts) =
	  (case lp ts of
	       SOME (t1, ts1) =>
	       (case nextTerm ts1 of
		    SOME (t2, T.RBrace :: ts2) => SOME (S.App (t1, t2), ts2)
		  | _ => err "at end of application")
	     | _ => err "within application")	    
	| lp ts = err ("in term at " ^ toksStr ts)
  in
    lp toks
  end

  and nextAbbr (T.Abbr a :: T.LeftArrow :: ts) =
        (case nextTerm ts of
	    SOME (t, ts') => S.Abbr (a, t, nextAbbr ts')
	  | NONE => err ("at abbreviation " ^ a))
    | nextAbbr ts =
        (case nextTerm ts of
	    SOME (t, []) => S.Term t
	  | SOME (t, ts) => err (": too many tokens " ^ toksStr ts)
	  | NONE => err "no main term?")

  fun parse tokens = nextAbbr tokens
	
end
