structure flasl2ast =
struct

open AST

fun printProp(prop) = 
case prop of
ATOM(atom)          => "AST.ATOM(\""^atom^"\")"
| NOT(p)            => "AST.NOT("^printProp(p)^")"
| AND(p1,p2)        => "AST.AND("^printProp(p1)^","^printProp(p2)^")"
| OR(p1,p2)         => "AST.OR("^printProp(p1)^","^printProp(p2)^")"
| COND(p1,p2)       => "AST.COND("^printProp(p1)^","^printProp(p2)^")"
| BIC(p1,p2)        => "AST.BIC("^printProp(p1)^","^printProp(p2)^")"
| ITE(p1,p2,p3)     => "AST.ITE("^printProp(p1)^","^printProp(p2)^","^printProp(p3)^")"

fun printPropList([]) = "]"
| printPropList(p::[]) = printProp(p)^"]"
| printPropList(p::proplist) = printProp(p)^","^printPropList(proplist)

fun printArgument arg = 
case arg of
HENCE(proplist,prop) => "AST.HENCE(["^printPropList(proplist)^","^printProp(prop)^")"
    
end