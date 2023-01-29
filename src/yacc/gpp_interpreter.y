%{
    #include <stdio.h>
    #include <stdlib.h>
    #include "gpp_interpreter.h"
    int yylex();
    void yyerror(char *s);
    extern FILE *yyin;
    symbol_table *table = NULL;
    function_table *func_table = NULL;
%}

%token OP_AND
%token OP_OR
%token OP_NOT
%token OP_EQ
%token OP_GT
%token OP_SET
%token COMMENT
%token KW_DEFVAR
%token KW_DEFFUN
%token KW_WHILE
%token KW_IF
%token KW_EXIT
%token KW_TRUE
%token KW_FALSE
%token OP_PLUS
%token OP_MINUS
%token OP_MULT
%token OP_OP
%token OP_CP
%token KW_PROGN

%union{ char *fval; char *str; int val; }

%token <fval> VALUEF
%token <str> IDENTIFIER

%type <fval> EXP
%type <fval> EXPLIST
%type <fval> EXPLISTTEMP
%type <val> EXPB
%type <fval> INPUT
%type <fval> ASG
%type <fval> FUNCTION
%type <fval> FCALL

%start START
%%
START       : INPUT { printf("Syntax OK.\nResult: %s\n", $1); }
            | START INPUT { printf("Syntax OK.\nResult: %s\n", $2); }
            | OP_OP KW_EXIT OP_CP { printf("Syntax OK.\nResult: EXIT\nTerminating\n"); exit(0); }
            ;

INPUT       : FUNCTION  { $$ = $1; } 
            | EXP       { $$ = $1; }
            | EXPLIST   { $$ = $1; }
            ;

EXP         : OP_OP OP_PLUS EXP EXP OP_CP   { $$ = add_frac($3, $4); } 
            | OP_OP OP_MINUS EXP EXP OP_CP  { $$ = sub_frac($3, $4); }
            | OP_OP OP_MULT EXP EXP OP_CP   { $$ = mul_frac($3, $4); }
            | OP_OP KW_IF EXPB EXPLIST EXPLIST OP_CP { $$ = $3 ? $4:$5; }
            | OP_OP KW_IF EXPB EXP EXP OP_CP { $$ = $3 ? $4:$5;}
            | OP_OP KW_IF EXPB EXPLIST EXP OP_CP { $$ = $3 ? $4:$5;}
            | OP_OP KW_IF EXPB EXP EXPLIST OP_CP { $$ = $3 ? $4:$5;}
            | OP_OP KW_WHILE EXPB EXPLIST OP_CP { $$ = $4; }
            | IDENTIFIER { $$ = get_id(&table, $1); }
            | VALUEF     { $$ = $1; }
            | FCALL      { $$ = $1; }
            | ASG        { $$ = $1; }
            ;

EXPB        : OP_OP OP_EQ EXP EXP OP_CP { $$ = frac_eq($3, $4); }
            | OP_OP OP_GT EXP EXP OP_CP { $$ = frac_gt($3, $4); }
            | KW_TRUE { $$ = 1;  }
            | KW_FALSE { $$ = 0; }
            | OP_OP OP_AND EXPB EXPB OP_CP { $$ = ($3 && $4); }
            | OP_OP OP_OR EXPB EXPB OP_CP  { $$ = ($3 && $4); }
            | OP_OP OP_NOT EXPB OP_CP      { $$ = !($3); }
            ;

EXPLIST     : OP_OP KW_PROGN EXPLISTTEMP OP_CP { $$ = $3; }
            ;

EXPLISTTEMP : EXP {$$ = $1;}
            | EXPLISTTEMP EXP {$$ = $2;} 
            ;

ASG         : OP_OP OP_SET IDENTIFIER EXP OP_CP { $$ = set_id(&table, $3, $4); }
            | OP_OP KW_DEFVAR IDENTIFIER EXP OP_CP { $$ = set_id(&table, $3, $4); }
            ;

FUNCTION    : OP_OP KW_DEFFUN IDENTIFIER OP_OP OP_CP EXPLIST OP_CP { $$ = set_func(&func_table, $3, "0f1"); }
            | OP_OP KW_DEFFUN IDENTIFIER OP_OP IDENTIFIER OP_CP EXPLIST OP_CP { $$ = set_func(&func_table, $3, "0f1"); }
            | OP_OP KW_DEFFUN IDENTIFIER OP_OP IDENTIFIER IDENTIFIER OP_CP EXPLIST OP_CP { $$ = set_func(&func_table, $3, "0f1"); }
            | OP_OP KW_DEFFUN IDENTIFIER OP_OP IDENTIFIER IDENTIFIER IDENTIFIER OP_CP EXPLIST OP_CP { $$ = set_func(&func_table, $3, "0f1"); }
            ;

FCALL       : OP_OP IDENTIFIER OP_CP { $$ = get_func(&func_table, $2); }
            | OP_OP IDENTIFIER EXP OP_CP { $$ = get_func(&func_table, $2); }
            | OP_OP IDENTIFIER EXP EXP OP_CP { $$ = get_func(&func_table, $2); }
            | OP_OP IDENTIFIER EXP EXP EXP OP_CP { $$ = get_func(&func_table, $2); }
            ;
%%


void yyerror(char *s) {
    printf("syntax error occured: %s\n",yylval.fval);
}
int main(int argc, char **argv) {
	if (argc > 1) {
		yyin = fopen(argv[1], "r");
		if (yyin == NULL) {
			printf("File does not exist.\n");
			return 0;
		}
	}
	return yyparse();

	return 0;
}
