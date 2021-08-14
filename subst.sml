structure Subst : sig

  val fv    : ULC.term -> VarSet.set

  val subst : string -> ULC.term -> ULC.term -> ULC.term
  (* NOTE: subst x s2 t1 means "rewrite x to s2 in t1" *)
  (* i.e. "subst x s2 t1" corresponds to "[x |-> s2] t1" in TaPL *)

end = struct

  structure U = ULC

  fun fv t =
    case t
      of ULC.Var x => VarSet.singleton x
      | ULC.Abs (x, t1) => VarSet.remove (x, fv t1)
      | ULC.App (t1, t2) => VarSet.union (fv t1, fv t2)

  fun subst x v2 t1 =
    (case t1
      of ULC.Var s => if x = s then v2 else t1
      | ULC.Abs (s, term) =>
        if x = s then t1
        else
          if VarSet.member (s, fv v2) then
            let
              val fresh = Fresh.name ()
              val term' = subst s (ULC.Var fresh) term
            in
              subst x v2 (ULC.Abs (fresh, term'))
            end
          else ULC.Abs (s, subst x v2 term)
      | ULC.App (term1, term2) =>
        ULC.App (subst x v2 term1, subst x v2 term2))

end
