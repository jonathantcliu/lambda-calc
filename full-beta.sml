structure FullBeta : SMALL_STEP = struct

  fun step t =
    case t
      of ULC.Var s => NONE
      | ULC.Abs (s, t1) =>
        (case step t1
          of SOME t1' => SOME (ULC.Abs (s, t1'))
          | NONE => NONE)
      | ULC.App (t1, t2) =>
        (case step t1
          of SOME t1' =>
            SOME (ULC.App (t1', t2))
          | NONE =>
            (case step t2
              of SOME t2' => SOME (ULC.App (t1, t2'))
              | NONE =>
                (case t1
                  of ULC.Abs (x, tone) => SOME (Subst.subst x t2 tone)
                  | _ => NONE)))

end
