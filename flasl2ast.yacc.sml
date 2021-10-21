functor flasl2astLrValsFun(structure Token : TOKEN)
 : sig structure ParserData : PARSER_DATA
       structure Tokens : flasl2ast_TOKENS
   end
 = 
struct
structure ParserData=
struct
structure Header = 
struct
(* 

EBNF for Formal Language for Arguments in Sentential Logic. This EBNF covers the associativity and precedence rules 

Argument            ::= Hypothesis THEREFORE Proposition TERM .
Hypothesis          ::= epsilon | Proposition TERM Hypothesis .
Proposition         ::= Prop_ITE IFF Proposition | Prop_ITE .
Prop_ITE            ::= IF Prop_ITE THEN Prop_ITE ELSE Prop_ITE | Prop_IF . 
Prop_IF             ::= Prop_IF IF Prop_OR | IF Prop_IF THEN Prop_IF | Prop_OR .
Prop_OR             ::= Prop_AND OR Prop_OR | Prop_AND .
Prop_AND            ::= Prop_NOT AND Prop_AND | Prop_NOT . 
Prop_NOT            ::= NOT Prop_NOT | LPAREN Proposition RPAREN | Atom
THEREFORE           ::= "THEREFORE" .
TERM                ::= "." .
IFF                 ::= "IFF" .
IF                  ::= "IF" .
THEN                ::= "THEN" .
ELSE                ::= "ELSE" .
OR                  ::= "NOT" .
AND                 ::= "AND" .
NOT                 ::= "NOT" .
LPAREN              ::= "(" .
RPAREN              ::= ")" .
QUOTE               ::= """ .
Atom                ::= QUOTE {Character} QUOTE . 
LETTER              ::= "-" | "!" | [#-'] | [*-,] | [0-~] | Whitespace | Newline
Whitespace          ::= " " | "\t" .
Newline             ::= "\n" | "\r" | "\n\r" .

*)



open AST


end
structure LrTable = Token.LrTable
structure Token = Token
local open LrTable in 
val table=let val actionRows =
"\
\\001\000\001\000\009\000\002\000\008\000\005\000\007\000\012\000\006\000\000\000\
\\001\000\003\000\014\000\004\000\013\000\005\000\012\000\006\000\026\000\
\\008\000\011\000\000\000\
\\001\000\003\000\014\000\004\000\013\000\005\000\012\000\008\000\011\000\
\\010\000\010\000\000\000\
\\001\000\003\000\014\000\004\000\013\000\005\000\012\000\008\000\011\000\
\\010\000\027\000\000\000\
\\001\000\003\000\014\000\004\000\013\000\005\000\012\000\008\000\011\000\
\\011\000\025\000\000\000\
\\001\000\009\000\015\000\000\000\
\\001\000\013\000\000\000\000\000\
\\032\000\000\000\
\\033\000\000\000\
\\034\000\000\000\
\\035\000\001\000\009\000\002\000\008\000\005\000\007\000\012\000\006\000\000\000\
\\036\000\000\000\
\\037\000\000\000\
\\038\000\000\000\
\\039\000\003\000\014\000\000\000\
\\040\000\003\000\014\000\004\000\013\000\005\000\012\000\007\000\029\000\000\000\
\\041\000\003\000\014\000\004\000\013\000\005\000\012\000\000\000\
\\042\000\003\000\014\000\004\000\013\000\000\000\
\\043\000\003\000\014\000\004\000\013\000\005\000\012\000\008\000\011\000\000\000\
\\044\000\000\000\
\"
val actionRowNumbers =
"\010\000\002\000\005\000\007\000\
\\000\000\000\000\000\000\011\000\
\\010\000\000\000\000\000\000\000\
\\000\000\000\000\004\000\001\000\
\\012\000\009\000\018\000\017\000\
\\014\000\013\000\003\000\019\000\
\\000\000\008\000\015\000\000\000\
\\016\000\006\000"
val gotoT =
"\
\\001\000\029\000\002\000\003\000\003\000\002\000\004\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\004\000\014\000\000\000\
\\004\000\015\000\000\000\
\\004\000\016\000\000\000\
\\000\000\
\\003\000\017\000\004\000\001\000\000\000\
\\004\000\018\000\000\000\
\\004\000\019\000\000\000\
\\004\000\020\000\000\000\
\\004\000\021\000\000\000\
\\004\000\022\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\004\000\026\000\000\000\
\\000\000\
\\000\000\
\\004\000\028\000\000\000\
\\000\000\
\\000\000\
\"
val numstates = 30
val numrules = 13
val s = ref "" and index = ref 0
val string_to_int = fn () => 
let val i = !index
in index := i+2; Char.ord(String.sub(!s,i)) + Char.ord(String.sub(!s,i+1)) * 256
end
val string_to_list = fn s' =>
    let val len = String.size s'
        fun f () =
           if !index < len then string_to_int() :: f()
           else nil
   in index := 0; s := s'; f ()
   end
val string_to_pairlist = fn (conv_key,conv_entry) =>
     let fun f () =
         case string_to_int()
         of 0 => EMPTY
          | n => PAIR(conv_key (n-1),conv_entry (string_to_int()),f())
     in f
     end
val string_to_pairlist_default = fn (conv_key,conv_entry) =>
    let val conv_row = string_to_pairlist(conv_key,conv_entry)
    in fn () =>
       let val default = conv_entry(string_to_int())
           val row = conv_row()
       in (row,default)
       end
   end
val string_to_table = fn (convert_row,s') =>
    let val len = String.size s'
        fun f ()=
           if !index < len then convert_row() :: f()
           else nil
     in (s := s'; index := 0; f ())
     end
local
  val memo = Array.array(numstates+numrules,ERROR)
  val _ =let fun g i=(Array.update(memo,i,REDUCE(i-numstates)); g(i+1))
       fun f i =
            if i=numstates then g i
            else (Array.update(memo,i,SHIFT (STATE i)); f (i+1))
          in f 0 handle General.Subscript => ()
          end
in
val entry_to_action = fn 0 => ACCEPT | 1 => ERROR | j => Array.sub(memo,(j-2))
end
val gotoT=Array.fromList(string_to_table(string_to_pairlist(NT,STATE),gotoT))
val actionRows=string_to_table(string_to_pairlist_default(T,entry_to_action),actionRows)
val actionRowNumbers = string_to_list actionRowNumbers
val actionT = let val actionRowLookUp=
let val a=Array.fromList(actionRows) in fn i=>Array.sub(a,i) end
in Array.fromList(List.map actionRowLookUp actionRowNumbers)
end
in LrTable.mkLrTable {actions=actionT,gotos=gotoT,numRules=numrules,
numStates=numstates,initialState=STATE 0}
end
end
local open Header in
type pos = int
type arg = unit
structure MlyValue = 
struct
datatype svalue = VOID | ntVOID of unit ->  unit
 | Atom of unit ->  (string) | PROP of unit ->  (Prop)
 | HYP of unit ->  (Prop list) | ARGUMENT of unit ->  (Argument)
 | FILE of unit ->  (Argument)
end
type svalue = MlyValue.svalue
type result = Argument
end
structure EC=
struct
open LrTable
infix 5 $$
fun x $$ y = y::x
val is_keyword =
fn _ => false
val preferred_change : (term list * term list) list = 
nil
val noShift = 
fn (T 12) => true | _ => false
val showTerminal =
fn (T 0) => "Atom"
  | (T 1) => "NOT"
  | (T 2) => "AND"
  | (T 3) => "OR"
  | (T 4) => "IF"
  | (T 5) => "THEN"
  | (T 6) => "ELSE"
  | (T 7) => "IFF"
  | (T 8) => "THEREFORE"
  | (T 9) => "TERM"
  | (T 10) => "RPAREN"
  | (T 11) => "LPAREN"
  | (T 12) => "EOF"
  | _ => "bogus-term"
local open Header in
val errtermvalue=
fn _ => MlyValue.VOID
end
val terms : term list = nil
 $$ (T 12) $$ (T 11) $$ (T 10) $$ (T 9) $$ (T 8) $$ (T 7) $$ (T 6) $$ 
(T 5) $$ (T 4) $$ (T 3) $$ (T 2) $$ (T 1)end
structure Actions =
struct 
exception mlyAction of int
local open Header in
val actions = 
fn (i392,defaultPos,stack,
    (()):arg) =>
case (i392,stack)
of  ( 0, ( ( _, ( MlyValue.ARGUMENT ARGUMENT1, ARGUMENT1left, 
ARGUMENT1right)) :: rest671)) => let val  result = MlyValue.FILE (fn _
 => let val  (ARGUMENT as ARGUMENT1) = ARGUMENT1 ()
 in (ARGUMENT)
end)
 in ( LrTable.NT 0, ( result, ARGUMENT1left, ARGUMENT1right), rest671)

end
|  ( 1, ( ( _, ( _, _, TERM1right)) :: ( _, ( MlyValue.PROP PROP1, _,
 _)) :: _ :: ( _, ( MlyValue.HYP HYP1, HYP1left, _)) :: rest671)) =>
 let val  result = MlyValue.ARGUMENT (fn _ => let val  (HYP as HYP1) =
 HYP1 ()
 val  (PROP as PROP1) = PROP1 ()
 in (HENCE(HYP,PROP))
end)
 in ( LrTable.NT 1, ( result, HYP1left, TERM1right), rest671)
end
|  ( 2, ( ( _, ( MlyValue.HYP HYP1, _, HYP1right)) :: _ :: ( _, ( 
MlyValue.PROP PROP1, PROP1left, _)) :: rest671)) => let val  result = 
MlyValue.HYP (fn _ => let val  (PROP as PROP1) = PROP1 ()
 val  (HYP as HYP1) = HYP1 ()
 in (PROP::HYP)
end)
 in ( LrTable.NT 2, ( result, PROP1left, HYP1right), rest671)
end
|  ( 3, ( rest671)) => let val  result = MlyValue.HYP (fn _ => ([]))
 in ( LrTable.NT 2, ( result, defaultPos, defaultPos), rest671)
end
|  ( 4, ( ( _, ( MlyValue.Atom Atom1, Atom1left, Atom1right)) :: 
rest671)) => let val  result = MlyValue.PROP (fn _ => let val  (Atom
 as Atom1) = Atom1 ()
 in (ATOM(Atom))
end)
 in ( LrTable.NT 3, ( result, Atom1left, Atom1right), rest671)
end
|  ( 5, ( ( _, ( MlyValue.PROP PROP1, _, PROP1right)) :: ( _, ( _, 
NOT1left, _)) :: rest671)) => let val  result = MlyValue.PROP (fn _ =>
 let val  (PROP as PROP1) = PROP1 ()
 in (NOT(PROP))
end)
 in ( LrTable.NT 3, ( result, NOT1left, PROP1right), rest671)
end
|  ( 6, ( ( _, ( MlyValue.PROP PROP2, _, PROP2right)) :: _ :: ( _, ( 
MlyValue.PROP PROP1, PROP1left, _)) :: rest671)) => let val  result = 
MlyValue.PROP (fn _ => let val  PROP1 = PROP1 ()
 val  PROP2 = PROP2 ()
 in (AND(PROP1,PROP2))
end)
 in ( LrTable.NT 3, ( result, PROP1left, PROP2right), rest671)
end
|  ( 7, ( ( _, ( MlyValue.PROP PROP2, _, PROP2right)) :: _ :: ( _, ( 
MlyValue.PROP PROP1, PROP1left, _)) :: rest671)) => let val  result = 
MlyValue.PROP (fn _ => let val  PROP1 = PROP1 ()
 val  PROP2 = PROP2 ()
 in (OR(PROP1,PROP2))
end)
 in ( LrTable.NT 3, ( result, PROP1left, PROP2right), rest671)
end
|  ( 8, ( ( _, ( MlyValue.PROP PROP2, _, PROP2right)) :: _ :: ( _, ( 
MlyValue.PROP PROP1, _, _)) :: ( _, ( _, IF1left, _)) :: rest671)) =>
 let val  result = MlyValue.PROP (fn _ => let val  PROP1 = PROP1 ()
 val  PROP2 = PROP2 ()
 in (COND(PROP1,PROP2))
end)
 in ( LrTable.NT 3, ( result, IF1left, PROP2right), rest671)
end
|  ( 9, ( ( _, ( MlyValue.PROP PROP3, _, PROP3right)) :: _ :: ( _, ( 
MlyValue.PROP PROP2, _, _)) :: _ :: ( _, ( MlyValue.PROP PROP1, _, _))
 :: ( _, ( _, IF1left, _)) :: rest671)) => let val  result = 
MlyValue.PROP (fn _ => let val  PROP1 = PROP1 ()
 val  PROP2 = PROP2 ()
 val  PROP3 = PROP3 ()
 in (ITE(PROP1,PROP2,PROP3))
end)
 in ( LrTable.NT 3, ( result, IF1left, PROP3right), rest671)
end
|  ( 10, ( ( _, ( MlyValue.PROP PROP2, _, PROP2right)) :: _ :: ( _, ( 
MlyValue.PROP PROP1, PROP1left, _)) :: rest671)) => let val  result = 
MlyValue.PROP (fn _ => let val  PROP1 = PROP1 ()
 val  PROP2 = PROP2 ()
 in (COND(PROP2,PROP1))
end)
 in ( LrTable.NT 3, ( result, PROP1left, PROP2right), rest671)
end
|  ( 11, ( ( _, ( MlyValue.PROP PROP2, _, PROP2right)) :: _ :: ( _, ( 
MlyValue.PROP PROP1, PROP1left, _)) :: rest671)) => let val  result = 
MlyValue.PROP (fn _ => let val  PROP1 = PROP1 ()
 val  PROP2 = PROP2 ()
 in (BIC(PROP1,PROP2))
end)
 in ( LrTable.NT 3, ( result, PROP1left, PROP2right), rest671)
end
|  ( 12, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( MlyValue.PROP PROP1,
 _, _)) :: ( _, ( _, LPAREN1left, _)) :: rest671)) => let val  result
 = MlyValue.PROP (fn _ => let val  (PROP as PROP1) = PROP1 ()
 in (PROP)
end)
 in ( LrTable.NT 3, ( result, LPAREN1left, RPAREN1right), rest671)
end
| _ => raise (mlyAction i392)
end
val void = MlyValue.VOID
val extract = fn a => (fn MlyValue.FILE x => x
| _ => let exception ParseInternal
	in raise ParseInternal end) a ()
end
end
structure Tokens : flasl2ast_TOKENS =
struct
type svalue = ParserData.svalue
type ('a,'b) token = ('a,'b) Token.token
fun Atom (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 0,(
ParserData.MlyValue.Atom (fn () => i),p1,p2))
fun NOT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 1,(
ParserData.MlyValue.VOID,p1,p2))
fun AND (p1,p2) = Token.TOKEN (ParserData.LrTable.T 2,(
ParserData.MlyValue.VOID,p1,p2))
fun OR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 3,(
ParserData.MlyValue.VOID,p1,p2))
fun IF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 4,(
ParserData.MlyValue.VOID,p1,p2))
fun THEN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 5,(
ParserData.MlyValue.VOID,p1,p2))
fun ELSE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 6,(
ParserData.MlyValue.VOID,p1,p2))
fun IFF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 7,(
ParserData.MlyValue.VOID,p1,p2))
fun THEREFORE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 8,(
ParserData.MlyValue.VOID,p1,p2))
fun TERM (p1,p2) = Token.TOKEN (ParserData.LrTable.T 9,(
ParserData.MlyValue.VOID,p1,p2))
fun RPAREN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 10,(
ParserData.MlyValue.VOID,p1,p2))
fun LPAREN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 11,(
ParserData.MlyValue.VOID,p1,p2))
fun EOF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 12,(
ParserData.MlyValue.VOID,p1,p2))
end
end
