structure flasl2astLrVals = flasl2astLrValsFun(structure Token = LrParser.Token)
structure flasl2astLex = flasl2astLexFun(structure Tokens = flasl2astLrVals.Tokens);
structure flasl2astParser =	Join(structure LrParser = LrParser
                    structure ParserData = flasl2astLrVals.ParserData
     	       		structure Lex = flasl2astLex)

open AST
open flasl2ast
open ast2flasl
open tableau

fun fileToLexer fileName =
let
    val instream = TextIO.openIn fileName
    val inputString = TextIO.input instream
    val _ = TextIO.closeIn instream
    val done = ref false
    val lexer = flasl2astParser.makeLexer(fn _ => if !done then "" else (done := true;inputString))
in
    lexer
end

fun invokeLexer lexer = 
let
    fun print_error(s,rowNum,colNum) = TextIO.output(TextIO.stdOut, "ParseError:"^(Int.toString rowNum)^":"^(Int.toString colNum)^"\n")
in
    flasl2astParser.parse(0,lexer,print_error,())
end

fun invokeParser(lexer) =
let
    val dummyEOF = flasl2astLrVals.Tokens.EOF(0,0)
    val (result,lexer) = invokeLexer lexer
    val (nextToken,lexer) = flasl2astParser.Stream.get lexer
in
    if flasl2astParser.sameToken(nextToken,dummyEOF) then result
    else ((TextIO.output(TextIO.stdOut, "Warning: Unconsumed input \n"));result)
end

val runParser = invokeParser o fileToLexer

fun convertflasl2ast inputFile = runParser inputFile

fun convertast2flasl inputFile outputFile =
let 
    val arg = runParser inputFile
    val outstream = TextIO.openOut outputFile
    val outputString = astToString(arg)
in
    (TextIO.output(outstream,outputString);TextIO.closeOut outstream)
end

fun falsify([]) = ""
| falsify(atom::atoms) = 
case atom of 
ATOM(a)         => a^" : True"^"\n"^falsify(atoms)
| NOT(ATOM(a))  => a^" : False"^"\n"^falsify(atoms)

fun check inputFile outputFile =
let
    val arg = runParser inputFile
    val outstream = TextIO.openOut outputFile
    val (truthValue,atoms) = checkValidity(arg)
    val outputString = if truthValue then "Valid!" else "Invalid! \nA falsifying assignment is as below: \n\n"^falsify(atoms)
in
    (TextIO.output(outstream,outputString);TextIO.closeOut outstream)
end