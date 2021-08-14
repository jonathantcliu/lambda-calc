structure Scan : sig

  val scan : string -> Token.token list

end = struct

  structure T = Token

  fun loop (chars, acc, f) =
    case chars
      of [] => (List.rev acc, [])
      | first::rest =>
        if f first then
          loop (rest, first::acc, f)
        else
          (List.rev acc, first::rest)

  fun nextToken [] = NONE
    | nextToken (#" " :: cs) = nextToken cs
    | nextToken (#"\t":: cs) = nextToken cs
    | nextToken (#"\n":: cs) = nextToken cs
    | nextToken (#"[" :: cs) = SOME (T.LBrack, cs)
    | nextToken (#"]" :: cs) = SOME (T.RBrack, cs)
    | nextToken (#"{" :: cs) = SOME (T.LBrace, cs)
    | nextToken (#"}" :: cs) = SOME (T.RBrace, cs)
    | nextToken (#"^" :: cs) = SOME (T.Caret, cs)
    | nextToken (#"." :: cs) = SOME (T.Dot, cs)
    | nextToken (#"<" :: #"-" :: cs) = SOME (T.LeftArrow, cs)
    | nextToken (#"f" :: #"s" :: #"t" :: cs) = SOME (T.Fst, cs)
    | nextToken (#"s" :: #"n" :: #"d" :: cs) = SOME (T.Snd, cs)
    | nextToken (c :: cs) =
      if Char.isDigit c then
        let
          val (numlist, tail) = loop (c::cs, [], Char.isDigit)
          val numoption = Int.fromString (implode numlist)
        in
          (case numoption
            of NONE => raise Fail "Scan.nextToken: malformed Nat"
            | SOME n => SOME (T.Nat n, tail))
        end
      else if Char.isLower c then
        let
          val (charlist, tail) = loop (c::cs, [], Char.isLower)
          val string = implode charlist
        in
          SOME (T.Var string, tail)
        end
      else if c = #"_" then
        let
          val (charlist, tail) = loop (cs, [], Char.isLower) (* c is _ *)
          val string = "_" ^ (implode charlist)
        in
          SOME (T.Abbr string, tail)
        end
      else if Char.isUpper c then
        let
          val (charlist, tail) = loop (c::cs, [], Char.isUpper)
          val string = implode charlist
        in
          SOME (T.ID string, tail)
        end
      else raise Fail ("Scan.nextToken: unable to scan at " ^ Char.toString c)

     (* complete the scanner so it can recognize tokens Nat, Var, Abbr, and ID *)

  fun scan code =
    let
      fun lp [] = []
	| lp cs = (case nextToken cs of
		       SOME (tok, cs') => tok :: lp cs'
		     | NONE => [])
    in
      lp (explode code)
    end

end
