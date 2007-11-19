/*
 * File:	pageyylex.l
 * ----------------
 * Lex inupt file to generate the yylex method for the scanning of length data stored in a page's content stream.
 */

%{

/* The text within this first region delimited by %{ and %} is assumed to
 * be C/C++ code and will be copied verbatim to the lex.pdf.c file ahead
 * of the definitions of the pdflex() function. Add other header file inclusions
 * or C++ variable declarations/prototypes that are needed by your code here.
 */

#include "pagescanner.h"
#include <string.h>
#include <iostream>
#include <iomanip>

%}

 /*
	* The section before the first %% is the Definitions section of the lex
	* input file. Here is where you set options for the scanner, define lex
	* states, and can set up definitions to give names to regular expressions
	* as a simple substitution mechanism that allows for more readable
	* entries in the Rules section later. 
	*/

WhiteSpace [ \t\n\r]

Float [+-]?[0-9]*[.]?[0-9]*

Name [/]{Regular}*


IndirectRef {PlainInteger}{WhiteSpace}+{PlainInteger}{WhiteSpace}*"R"
IndirectDef {PlainInteger}{WhiteSpace}+{PlainInteger}{WhiteSpace}*"obj"

%option c++
%option noyywrap

%option yyclass="PageScanner"

%x Copy

%%

<Copy>(.|[\n]) { (*yyout) << *yytext ; }

({Float}{WhiteSpace}+){5}{Float}{WhiteSpace}*"cm" {
	char * end;
	double dummy;
	dummy = strtod( yytext, & end );
	if( dummy != 1 )
		{
			throw( "Expected pure translation in the initial coordinate transform matrix" );
		}
	dummy = strtod( end, & end );
	if( dummy != 0 )
		{
			throw( "Expected pure translation in the initial coordinate transform matrix" );
		}
	dummy = strtod( end, & end );
	if( dummy != 0 )
		{
			throw( "Expected pure translation in the initial coordinate transform matrix" );
		}
	dummy = strtod( end, & end );
	if( dummy != 1 )
		{
			throw( "Expected pure translation in the initial coordinate transform matrix" );
		}
	x0 = strtod( end, & end );
	y0 = strtod( end, & end );
	(*yyout) << yytext << endl ;
}

"q" ;
"[]0 d" ;
"0 J" ;
{Float}{WhiteSpace}*"w" ;
"S" ;

"Q" {
	if( state == 3 )
		{
			BEGIN( Copy );
		}
}

{Float}{WhiteSpace}*{Float}{WhiteSpace}*"m" {
	char * end;
	double dummy;
	dummy = strtod( yytext, & end );
	ytmp = strtod( end, & end );
}

{Float}{WhiteSpace}*{Float}{WhiteSpace}*"l" {
	char * end;
	strtod( yytext, & end );
	double tmp = strtod( end, & end ) - 3;
	switch( state )
		{
		case 0:
			height = tmp;
			break;
		case 1:
			depth = tmp;
			break;
		case 2:
			width = tmp;
			break;
		default:
			throw( "Surprising state in pagescanner" );
		}
	++state;
}

{WhiteSpace}+ ;

. { throw( string( "Page scanner found unrecognized token: " ) + yytext ); }

%%
/* The closing %% above marks the end of the Rules section and the beginning
 * of the User Subroutines section. All text from here to the end of the
 * file is copied verbatim to the end of the generated lex.pdf.c file.
 * This section is where you put definitions of helper functions.
 */


