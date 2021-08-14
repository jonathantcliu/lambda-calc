structure VarSet :> sig

  type set

  val empty     : set
  val singleton : string -> set

  val sizeof : set -> int

  val member : string * set -> bool
  val insert : string * set -> set
  val remove : string * set -> set
  val union  : set * set -> set

end = struct

  type set = string list (* <== change this to something else! *)

  val empty = []

  fun singleton x = [x]

  fun sizeof s = length s

  fun member (x, s) =
    case s
      of [] => false
      | first :: rest => if x = first then true else member (x, rest)

  fun insert (x, s) = if member (x, s) then s else x :: s

  fun remove (x, s) =
    case s
      of [] => []
      | first :: rest =>
        if x = first then remove (x, rest) else (first :: remove (x, rest))

  fun union (s1, s2) =
    let
      fun helper l acc =
        (case l
          of [] => List.rev acc
          | x :: xs => helper (remove (x, xs)) (x :: acc))
    in
      helper (s1 @ s2) [] (* s1 @ s2 *)
    end

end
