# lambda-calc
Lambda calculus interpreter

p ::= _x <- t p // abbreviations  
    | t         // terms  

t ::= x       // variables  
    | {^x.t}  // abstractions (functions)  
    | {t t}   // applications  
    | n       // natural numbers  
    | A       // identity functions  
    | [t t]   // pairs  
    | {fst t} // pair first  
    | {snd t} // pair second  

Note: you can define abbreviations like so:  
_tru  <- {^t.{^f.t}}  
_test <- {^l.{^m.{^n.{{l m} n}}}}  
{{{_test _tru} A} B}  
