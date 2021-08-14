structure Desugar : sig

  val desugar : SULC.term -> ULC.term

end = struct

  fun natToAbs n =
    let
      fun helper number =
        if number = 0 then ULC.Var "z"
        else ULC.App (ULC.Var "s", helper (number - 1))
    in
      ULC.Abs ("s", ULC.Abs ("z", helper n))
    end

  fun desugar t =
    case t
      of SULC.ID s => ULC.Abs (s, ULC.Var s)
      | SULC.Var s => ULC.Var s
      | SULC.Abs (s, t1) => ULC.Abs (s, desugar t1)
      | SULC.App (t1, t2) => ULC.App (desugar t1, desugar t2)
      | SULC.Nat n => natToAbs n
      | SULC.Pair (t1, t2) =>
        let
          val pairhelper =
            ULC.Abs ("f",
            ULC.Abs ("s",
            ULC.Abs ("b",
              ULC.App
                (ULC.App (ULC.Var "b", ULC.Var "f"),
                ULC.Var "s"))))
        in
          ULC.App (ULC.App (pairhelper, desugar t1), desugar t2)
        end
      | SULC.Select1 t1 =>
        let
          val tru = ULC.Abs ("t", ULC.Abs ("f", ULC.Var "t"))
        in
          ULC.App
            (ULC.Abs ("p",
              ULC.App (ULC.Var "p", tru)), desugar t1)
        end
      | SULC.Select2 t1 =>
        let
          val fls = ULC.Abs ("t", ULC.Abs ("f", ULC.Var "f"))
        in
          ULC.App
            (ULC.Abs ("p",
              ULC.App (ULC.Var "p", fls)), desugar t1)
        end

end
