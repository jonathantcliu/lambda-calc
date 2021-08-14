structure Token = struct

  datatype token
    = Var of string
    | Abbr of string
    | ID of string
    | Nat of int
    | LBrace
    | RBrace
    | LBrack
    | RBrack
    | Caret
    | LeftArrow
    | Dot
    | Fst
    | Snd
	
  fun tos (Var x) = "Var(" ^ x ^ ")"
    | tos (Abbr a) = "Abbr(" ^ a ^ ")"
    | tos (ID a) = "ID(" ^ a ^ ")"
    | tos (Nat n) = "Nat(" ^ Int.toString n ^ ")"
    | tos LBrace = "LBrace"
    | tos RBrace = "RBrace"
    | tos LBrack = "LBrack"
    | tos RBrack = "RBrack"
    | tos Caret = "Caret"
    | tos LeftArrow = "LeftArrow"
    | tos Dot = "Dot"
    | tos Fst = "Fst"
    | tos Snd = "Snd"
		  
end
