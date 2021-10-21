all:
	ml-lex flasl2ast.lex 
	ml-yacc flasl2ast.yacc
	rlwrap sml loader.sml

clean:
	rm flasl2ast.yacc.desc
	rm flasl2ast.yacc.sig
	rm flasl2ast.yacc.sml
	rm flasl2ast.lex.sml