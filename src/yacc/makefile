target: compile

compile:
	flex gpp_lexer.l 
	yacc -d gpp_interpreter.y
	cc lex.yy.c y.tab.c -o gpp_interpreter
run:
	./gpp_interpreter