/* A Bison parser, made by GNU Bison 1.875c.  */

/* Skeleton parser for Yacc-like parsing with Bison,
   Copyright (C) 1984, 1989, 1990, 2000, 2001, 2002, 2003 Free Software Foundation, Inc.

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 59 Temple Place - Suite 330,
   Boston, MA 02111-1307, USA.  */

/* As a special exception, when this file is copied by Bison into a
   Bison output file, you may use that output file without restriction.
   This special exception was added by the Free Software Foundation
   in version 1.24 of Bison.  */

/* Tokens.  */
#ifndef YYTOKENTYPE
# define YYTOKENTYPE
   /* Put the tokens into the symbol table, so that GDB and other debuggers
      know about them.  */
   enum yytokentype {
     T_EOF = 258,
     T_minusminus = 259,
     T_plusplus = 260,
     T_ddot = 261,
     T_dddot = 262,
     T_assign = 263,
     T_eqeq = 264,
     T_eqneq = 265,
     T_flassign = 266,
     T_atat = 267,
     T_projection = 268,
     T_angle = 269,
     T_cycle = 270,
     T_and = 271,
     T_or = 272,
     T_xor = 273,
     T_not = 274,
     T_mapsto = 275,
     T_bindto = 276,
     T_emptybrackets = 277,
     T_compose = 278,
     T_surrounding = 279,
     T_lesseq = 280,
     T_greatereq = 281,
     T_llthan = 282,
     T_declaretype = 283,
     T_let = 284,
     T_letstar = 285,
     T_letrec = 286,
     T_unit = 287,
     T_defaultunit = 288,
     T_tex = 289,
     T_dynamic = 290,
     T_continuation = 291,
     T_continue = 292,
     T_class = 293,
     T_members = 294,
     T_prepare = 295,
     T_abstract = 296,
     T_overrides = 297,
     T_gr__ = 298,
     T_int = 299,
     T_float = 300,
     T_length = 301,
     T_speciallength = 302,
     T_bool = 303,
     T_string = 304,
     T_identifier = 305,
     T_at_identifier = 306,
     T_at_llthan = 307,
     T_dynamiccolon = 308
   };
#endif
#define T_EOF 258
#define T_minusminus 259
#define T_plusplus 260
#define T_ddot 261
#define T_dddot 262
#define T_assign 263
#define T_eqeq 264
#define T_eqneq 265
#define T_flassign 266
#define T_atat 267
#define T_projection 268
#define T_angle 269
#define T_cycle 270
#define T_and 271
#define T_or 272
#define T_xor 273
#define T_not 274
#define T_mapsto 275
#define T_bindto 276
#define T_emptybrackets 277
#define T_compose 278
#define T_surrounding 279
#define T_lesseq 280
#define T_greatereq 281
#define T_llthan 282
#define T_declaretype 283
#define T_let 284
#define T_letstar 285
#define T_letrec 286
#define T_unit 287
#define T_defaultunit 288
#define T_tex 289
#define T_dynamic 290
#define T_continuation 291
#define T_continue 292
#define T_class 293
#define T_members 294
#define T_prepare 295
#define T_abstract 296
#define T_overrides 297
#define T_gr__ 298
#define T_int 299
#define T_float 300
#define T_length 301
#define T_speciallength 302
#define T_bool 303
#define T_string 304
#define T_identifier 305
#define T_at_identifier 306
#define T_at_llthan 307
#define T_dynamiccolon 308




#if ! defined (YYSTYPE) && ! defined (YYSTYPE_IS_DECLARED)
#line 92 "metapdfparser.y"
typedef union YYSTYPE {
  Ast::Expression * expr;
  Ast::Node * node;
  Ast::MethodIdExpr * methodId;
  std::list< Ast::Node * > * nodeList;
  std::list< Ast::Expression * > * exprList;
  std::list< RefCountPtr< const char > > * strList;
  std::map< const char *, Ast::Expression *, charPtrLess > * namedExprMap;
  Kernel::Formals * formals;
  std::list< Kernel::Formals * > * formalsList;
  Ast::ArgListExprs * argList;
  int intVal;
  double floatVal;
  bool boolVal;
  char * str;
  int tokenID;
  std::list< const Ast::CallExpr * > * callExprList;
  std::list< Ast::ClassSection * > * classSectionList;
  Ast::ClassSection * classSection;
  Ast::MemberSection * memberSection;
  Ast::MemberDeclaration * memberDeclaration;
  Ast::MemberMode memberMode;
  Ast::ClassMode classMode;
  Ast::FunctionMode functionMode;
} YYSTYPE;
/* Line 1275 of yacc.c.  */
#line 169 "metapdfparser.tab.h"
# define yystype YYSTYPE /* obsolescent; will be withdrawn */
# define YYSTYPE_IS_DECLARED 1
# define YYSTYPE_IS_TRIVIAL 1
#endif

extern YYSTYPE metapdflval;

#if ! defined (YYLTYPE) && ! defined (YYLTYPE_IS_DECLARED)
typedef struct YYLTYPE
{
  int first_line;
  int first_column;
  int last_line;
  int last_column;
} YYLTYPE;
# define yyltype YYLTYPE /* obsolescent; will be withdrawn */
# define YYLTYPE_IS_DECLARED 1
# define YYLTYPE_IS_TRIVIAL 1
#endif

extern YYLTYPE metapdflloc;


