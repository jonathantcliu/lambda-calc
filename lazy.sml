structure Lazy : SMALL_STEP = struct

  fun step t =
    case t
      of ULC.Var s => NONE
      | ULC.Abs (s, t1) => NONE
      | ULC.App (t1, t2) =>
        (case step t1
          of SOME t1' =>
            SOME (ULC.App (t1', t2))
          | NONE =>
            (case t1
              of ULC.Abs (x, t12) => SOME (Subst.subst x t2 t12)
              | _ => NONE))

end
