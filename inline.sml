structure Inline : sig

  val inline : SULC.program -> SULC.term

end = struct


  fun member (x, l) =
    case l
      of [] => false
      | first::rest => if x = first then true else member (x, rest)

  fun inline p =
    case p
      of SULC.Abbr (string, term, program) =>
        let
          fun replace (prog, str, trm, strings) =
            (case prog
              of SULC.Term t =>
                (case t
                  of SULC.Var s => if s = str then trm else t
                  | SULC.Abs (s, t1) =>
                    SULC.Abs (s, replace (SULC.Term t1, str, trm, strings))
                  | SULC.App (t1, t2) =>
                    SULC.App (replace (SULC.Term t1, str, trm, strings),
                              replace (SULC.Term t2, str, trm, strings))
                  | SULC.Pair (t1, t2) =>
                    SULC.Pair (replace (SULC.Term t1, str, trm, strings),
                               replace (SULC.Term t2, str, trm, strings))
                  | SULC.Select1 t1 =>
                    SULC.Select1 (replace (SULC.Term t1, str, trm, strings))
                  | SULC.Select2 t1 =>
                    SULC.Select2 (replace (SULC.Term t1, str, trm, strings))
                  | _ => t)
              | SULC.Abbr (abbrs, abbrt, prest) =>
                if str = abbrs orelse member (abbrs, strings) then
                  raise Fail "Inline.inline: double abbreviation"
                else
                  let
                    val strings' = abbrs::strings
                    val replaced_abbrt =
                      replace (SULC.Term abbrt, str, trm, strings')
                    val inlined = replace (prest, str, trm, strings')
                  in
                    replace
                      (SULC.Term inlined, abbrs, replaced_abbrt, strings')
                  end)
        in
          replace (program, string, term, [])
        end
      | SULC.Term t => t

end
