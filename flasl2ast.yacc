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

%%

%name flasl2ast

%term
     Atom of string | NOT | AND | OR | IF | THEN | ELSE | IFF | THEREFORE | TERM | RPAREN | LPAREN | EOF  
%nonterm 
     FILE of Argument | ARGUMENT of Argument | HYP of Prop list | PROP of Prop 

%pos int
%eop EOF
%noshift EOF
%start FILE


%right IFF
%right THEN ELSE
%left IF
%left OR
%left AND
%right NOT
%nonassoc LPAREN

%verbose

%%

FILE: ARGUMENT(ARGUMENT)

ARGUMENT: HYP THEREFORE PROP TERM(HENCE(HYP,PROP))   

HYP:  PROP TERM HYP(PROP::HYP)
     | ([])

PROP: Atom(ATOM(Atom))
     | NOT PROP(NOT(PROP))
     | PROP AND PROP(AND(PROP1,PROP2))
     | PROP OR PROP(OR(PROP1,PROP2))
     | IF PROP THEN PROP(COND(PROP1,PROP2))
     | IF PROP THEN PROP ELSE PROP(ITE(PROP1,PROP2,PROP3))
     | PROP IF PROP(COND(PROP2,PROP1))
     | PROP IFF PROP(BIC(PROP1,PROP2))
     | LPAREN PROP RPAREN(PROP)		 			