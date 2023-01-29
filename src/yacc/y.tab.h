/* A Bison parser, made by GNU Bison 3.5.1.  */

/* Bison interface for Yacc-like parsers in C

   Copyright (C) 1984, 1989-1990, 2000-2015, 2018-2020 Free Software Foundation,
   Inc.

   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <http://www.gnu.org/licenses/>.  */

/* As a special exception, you may create a larger work that contains
   part or all of the Bison parser skeleton and distribute that work
   under terms of your choice, so long as that work isn't itself a
   parser generator using the skeleton or a modified version thereof
   as a parser skeleton.  Alternatively, if you modify or redistribute
   the parser skeleton itself, you may (at your option) remove this
   special exception, which will cause the skeleton and the resulting
   Bison output files to be licensed under the GNU General Public
   License without this special exception.

   This special exception was added by the Free Software Foundation in
   version 2.2 of Bison.  */

/* Undocumented macros, especially those whose name start with YY_,
   are private implementation details.  Do not rely on them.  */

#ifndef YY_YY_Y_TAB_H_INCLUDED
# define YY_YY_Y_TAB_H_INCLUDED
/* Debug traces.  */
#ifndef YYDEBUG
# define YYDEBUG 0
#endif
#if YYDEBUG
extern int yydebug;
#endif

/* Token type.  */
#ifndef YYTOKENTYPE
# define YYTOKENTYPE
  enum yytokentype
  {
    OP_AND = 258,
    OP_OR = 259,
    OP_NOT = 260,
    OP_EQ = 261,
    OP_GT = 262,
    OP_SET = 263,
    COMMENT = 264,
    KW_DEFVAR = 265,
    KW_DEFFUN = 266,
    KW_WHILE = 267,
    KW_IF = 268,
    KW_EXIT = 269,
    KW_TRUE = 270,
    KW_FALSE = 271,
    OP_PLUS = 272,
    OP_MINUS = 273,
    OP_MULT = 274,
    OP_OP = 275,
    OP_CP = 276,
    KW_PROGN = 277,
    VALUEF = 278,
    IDENTIFIER = 279
  };
#endif
/* Tokens.  */
#define OP_AND 258
#define OP_OR 259
#define OP_NOT 260
#define OP_EQ 261
#define OP_GT 262
#define OP_SET 263
#define COMMENT 264
#define KW_DEFVAR 265
#define KW_DEFFUN 266
#define KW_WHILE 267
#define KW_IF 268
#define KW_EXIT 269
#define KW_TRUE 270
#define KW_FALSE 271
#define OP_PLUS 272
#define OP_MINUS 273
#define OP_MULT 274
#define OP_OP 275
#define OP_CP 276
#define KW_PROGN 277
#define VALUEF 278
#define IDENTIFIER 279

/* Value type.  */
#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED
union YYSTYPE
{
#line 33 "gpp_interpreter.y"
 char *fval; char *str; int val; 

#line 108 "y.tab.h"

};
typedef union YYSTYPE YYSTYPE;
# define YYSTYPE_IS_TRIVIAL 1
# define YYSTYPE_IS_DECLARED 1
#endif


extern YYSTYPE yylval;

int yyparse (void);

#endif /* !YY_YY_Y_TAB_H_INCLUDED  */
