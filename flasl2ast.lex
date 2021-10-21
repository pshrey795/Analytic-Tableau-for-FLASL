structure Token = Tokens
  
    type pos = int
    type svalue = Tokens.svalue
    type ('a,'b) token = ('a,'b) Tokens.token  
    type lexresult = (svalue, pos) token

    exception ScanError

    val rowNum = ref 1;
    val colNum = ref 1;
    val endOfLine = ref 0;

    val eof = fn () => Tokens.EOF(!rowNum, !colNum) 

    val error = fn (e, row:int, col:int) => TextIO.output(TextIO.stdOut,"ScanError:" ^ (Int.toString row) ^ ":" ^ (Int.toString col) ^ ":" ^ e ^ "\n")

    fun substr(s,ss) =
    let
        fun substr_itr(s,[],ssp) = true
        | substr_itr([],ss,ssp) = false
        | substr_itr(x::s,y::ss,ssp) = if x=y then (substr_itr(s,ss,ssp) orelse substr_itr(s,ssp,ssp))
        else substr_itr(s,ssp,ssp)
    in
        substr_itr(s,ss,ss)
    end

    fun check(s) =
    substr(explode s,explode "IF") orelse substr(explode s,explode "THEN") orelse substr(explode s,explode "ELSE") orelse
    substr(explode s,explode "IFF") orelse substr(explode s,explode "AND") orelse substr(explode s,explode "OR") orelse
    substr(explode s,explode "NOT") orelse substr(explode s,explode "THEREFORE")

    fun atomString([],s,t) = s
    | atomString(x::[],s,t) = s
    | atomString(head::head2::atomList,s,t) = 
    if head = #"\"" then atomString(head2::atomList,s,true)
    else if (head = #" " orelse head = #"\t" orelse head = #"\n" orelse head = #"\r") 
    then (if (head2 = #" " orelse head2 = #"\t" orelse head2 = #"\n" orelse head2 = #"\r" orelse head2 = #"\"") then atomString(head2::atomList,s,t)
    else if t then atomString(head2::atomList,s,true) else atomString(head2::atomList,s^" ",false))
    else atomString(head2::atomList,s^Char.toString(head),false)
  
%%

%header (functor flasl2astLexFun(structure Tokens:flasl2ast_TOKENS));
 
ws = [\  \t];
newline = [\r \n \r\n];
atom = [- | ! | #-' | *-, | 0-~ | \ | \t | \r | \n | \r\n ];
word = [- | ! | #-' | *-, | 0-~ ];
quote = "\"";

%%
{quote}{atom}*{quote}   => (colNum := yypos - !(endOfLine);(if check(yytext) then (error("Keyword in atom",!rowNum,!colNum);raise AST.Atom_exception) else Token.Atom(atomString(explode yytext,"",false),!rowNum,!colNum)));
"NOT"           	=> (colNum := yypos - !(endOfLine);Token.NOT(!rowNum,!colNum));
"AND"           	=> (colNum := yypos - !(endOfLine);Token.AND(!rowNum,!colNum));
"OR"            	=> (colNum := yypos - !(endOfLine);Token.OR(!rowNum,!colNum));
"IF"            	=> (colNum := yypos - !(endOfLine);Token.IF(!rowNum,!colNum));
"THEN"          	=> (colNum := yypos - !(endOfLine);Token.THEN(!rowNum,!colNum));
"ELSE"          	=> (colNum := yypos - !(endOfLine);Token.ELSE(!rowNum,!colNum));
"IFF"               => (colNum := yypos - !(endOfLine);Token.IFF(!rowNum,!colNum));
"THEREFORE"         => (colNum := yypos - !(endOfLine);Token.THEREFORE(!rowNum,!colNum));
"("             	=> (colNum := yypos - !(endOfLine);Token.LPAREN(!rowNum,!colNum));
")"             	=> (colNum := yypos - !(endOfLine);Token.RPAREN(!rowNum,!colNum));
"."                 => (colNum := yypos - !(endOfLine);Token.TERM(!rowNum,!colNum));
{newline}       	=> (rowNum := !rowNum + 1;endOfLine := yypos;lex());
{ws}+           	=> (lex());
.               	=> (colNum := yypos - !(endOfLine)-size yytext;error(yytext,!rowNum,!colNum);raise ScanError);