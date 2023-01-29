
%{
	#include <stdio.h>
	#include <stdlib.h>
	#include "y.tab.h"
%}

VALUEF   	([1-9]+[0-9]*|[0])[f]([1-9]+[0-9]*)
IDENTIFIER  [_a-zA-Z][_a-zA-Z0-9]*
COMMENT 	[;]{2}.*
SKIP		[ \n\r\t]
INVALIDIDT	[0-9]+[_a-zA-Z]+
INVALIDFVAL ([1-9]+[0-9]*|[0])[f]([0]+[1-9]*)

%%

and 		{ return OP_AND; }
or			{ return OP_OR; }
not			{ return OP_NOT; }
eq 		    { return OP_EQ; }
gt			{ return OP_GT; }
set 		{ return OP_SET; }
defvar		{ return KW_DEFVAR; }
deffun		{ return KW_DEFFUN; }
while		{ return KW_WHILE; }	
if 			{ return KW_IF; }
exit		{ return KW_EXIT; }
true		{ return KW_TRUE; }
false		{ return KW_FALSE; }
progn		{ return KW_PROGN; }

"+"		{ return OP_PLUS; }
"-"		{ return OP_MINUS; }
"*"		{ return OP_MULT; }
"("		{ return OP_OP; }
")"		{ return OP_CP; }
	
{COMMENT} { ;}
{VALUEF} { yylval.fval = malloc(sizeof(char)*strlen(yytext) + 1); strcpy(yylval.fval, yytext); return VALUEF; }

{IDENTIFIER} { yylval.fval = malloc(sizeof(char)*strlen(yytext) + 1); strcpy(yylval.fval, yytext); return IDENTIFIER; }

{INVALIDIDT} { printf("LEXICAL ERROR:  %s is not valid identifier.\n", yytext); }
{INVALIDFVAL} { printf("LEXICAL ERROR:  %s is not valid fractional value.\n", yytext); }
{SKIP} {;}

.	{ printf("LEXICAL ERROR:  %s cannot be tokenized\n", yytext); }
%%

int yywrap(void) { return 1; }