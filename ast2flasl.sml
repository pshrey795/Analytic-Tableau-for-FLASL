structure ast2flasl =
struct 

open AST

fun convert(prop) = 
case prop of
ATOM(atom)          => "\""^atom^"\""
| NOT(p)            => "NOT "^convert(p)
| AND(p1,p2)        => convert(p1)^" AND "^convert(p2)
| OR(p1,p2)         => convert(p1)^" OR "^convert(p2)
| COND(p1,p2)       => "IF "^convert(p1)^" THEN "^convert(p2)
| BIC(p1,p2)        => convert(p1)^" IFF "^convert(p2)
| ITE(p1,p2,p3)     => "IF "^convert(p1)^" THEN "^convert(p2)^" ELSE "^convert(p3)          

fun convertList([],s) = s
| convertList(p::proplist,s) = convertList(proplist,s^convert(p)^". ")

fun astToString(argument) = 
case argument of HENCE(proplist,prop) => convertList(proplist,"")^"THEREFORE "^convert(prop)^"."

end