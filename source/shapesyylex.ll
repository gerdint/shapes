/* This file is part of Shapes.
 *
 * Shapes is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * any later version.
 *
 * Shapes is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with Shapes.  If not, see <http://www.gnu.org/licenses/>.
 *
 * Copyright 2008 Henrik Tidefelt
 */

%{

#include <cmath>

#include "shapesscanner.h"
#include "shapesvalue.h"
#include "shapestypes.h"
#include "astflow.h"
#include "astvar.h"
#include "astclass.h"
#include "shapesexceptions.h"
#include "texlabelmanager.h"
#include "globals.h"
#include "exitcodes.h"

using namespace Shapes;
#include "yyltype.h"
#include "shapesparser.h"

#include <string>
#include <cstdlib>
#include <iostream>
#include <fstream>
#include <iomanip>

#define YY_USER_ACTION doBeforeEachAction( );
#define YY_EXIT_FAILURE Shapes::Interaction::EXIT_INTERNAL_ERROR

double shapes_strtod( char * str, char ** end );

%}

WhiteSpace [ \t]

Float [~]?[0-9]+([.][0-9]*)?("*^"[~]?[0-9]+)?
Greek "α"|"β"|"γ"|"Γ"|"δ"|"Δ"|"ε"|"ζ"|"η"|"Θ"|"ι"|"κ"|"λ"|"Λ"|"μ"|"ν"|"χ"|"Ξ"|"π"|"Π"|"ρ"|"σ"|"Σ"|"τ"|"ϕ"|"ω"|"Ω"
LowerCaseLetter [a-z_?]
UpperCaseLetter [A-Z]
Letter {LowerCaseLetter}|{UpperCaseLetter}|{Greek}
Identifier {Letter}({Letter}|[0-9])*
DynamicMark "@"
StateMark "#"|"•"
TypeMark "//"|"§"

/*
	At the moment, escape characters must occypy exactly 2 bytes.
*/
Escape "¢"|"¤"


%option c++
%option noyywrap
%option yylineno

%option prefix="shapes"
%option yyclass="ShapesScanner"

%x DiscardRestOfLineAndBEGIN_INITIAL
%x Incl
%x Needs
%x InclFrom
%x InclPath
%x String
%x PoorMansString
%x Comment
%x LaTeXOption
%x LaTeXClass
%x LaTeXPreamble
%x LaTeXDocumentTop
%x RandSeed
%x NewUnitName
%x NewUnitEqual
%x NewUnitValue
%x Echo
%x Author


%%

^"##classoption"[ \t]+ { BEGIN( LaTeXOption ); }
^"##documentclass"[ \t]+ { BEGIN( LaTeXClass ); }
^"##preamble"[ \t] { BEGIN( LaTeXPreamble ); }
^"##documenttop"[ \t] { BEGIN( LaTeXDocumentTop ); }
^"##no-lmodernT1"[\n] { Kernel::theTeXLabelManager.setlmodernT1( shapeslloc, false ); }
^"##no-utf8"[\n] { Kernel::theTeXLabelManager.setutf8( shapeslloc, false ); }
^"##seed"[ \t]+ { BEGIN( RandSeed ); }
^"##unit"[ \t]+ { BEGIN( NewUnitName ); }
^"##include"[ \t]+ { BEGIN( Incl ); }
^"##needs"[ \t]+ { BEGIN( Needs ); }
^"##echo"[ \t] { BEGIN( Echo ); }
^"##author"[ \t] { BEGIN( Author ); }
^"##" {
	Ast::theAnalysisErrorsList.push_back( new Exceptions::ScannerError( shapeslloc, strrefdup( "All lines beginning with ## must be scanner specials.	Please use a leading horizontal whitespace if this is not what is intended." ) ) );
}

<DiscardRestOfLineAndBEGIN_INITIAL>.* {
	BEGIN( INITIAL );
}

<LaTeXOption>[^,\n]+ { Kernel::theTeXLabelManager.addDocumentOption( shapeslloc, yytext	); }
<LaTeXOption>[\n] {
	shapeslloc.firstLine = shapeslloc.lastLine + 1;
	shapeslloc.lastLine = shapeslloc.firstLine;
	shapeslloc.lastColumn = 0;
	BEGIN( INITIAL );
}

<LaTeXClass>.+ { Kernel::theTeXLabelManager.setDocumentClass( shapeslloc, yytext ); }
<LaTeXClass>[\n] {
	shapeslloc.firstLine = shapeslloc.lastLine + 1;
	shapeslloc.lastLine = shapeslloc.firstLine;
	shapeslloc.lastColumn = 0;
	BEGIN( INITIAL );
}

<LaTeXPreamble>.* { Kernel::theTeXLabelManager.addPreambleLine( shapeslloc, yytext ); }
<LaTeXPreamble>[\n] {
	shapeslloc.firstLine = shapeslloc.lastLine + 1;
	shapeslloc.lastLine = shapeslloc.firstLine;
	shapeslloc.lastColumn = 0;
	BEGIN( INITIAL );
}

<LaTeXDocumentTop>.* { Kernel::theTeXLabelManager.addDocumentTopLine( shapeslloc, yytext );	}
<LaTeXDocumentTop>[\n] {
	shapeslloc.firstLine = shapeslloc.lastLine + 1;
	shapeslloc.lastLine = shapeslloc.firstLine;
	shapeslloc.lastColumn = 0;
	BEGIN( INITIAL );
}

<RandSeed>.* {
	char * end;
	long int s = strtol( yytext, & end, 10 );
	if( *end != '\0' )
		{
			Ast::theAnalysisErrorsList.push_back( new Exceptions::ScannerError( shapeslloc, strrefdup( "Failed to scan integer." ) ) );
		}
	else
		{
			if( s < 0 )
				{
					Ast::theAnalysisErrorsList.push_back( new Exceptions::ScannerError( shapeslloc, strrefdup( "Random nuber generator seed must not be negative." ) ) );
				}
			else
				{
					srand( s );
				}
		}
}
<RandSeed>[\n] {
	shapeslloc.firstLine = shapeslloc.lastLine + 1;
	shapeslloc.lastLine = shapeslloc.firstLine;
	shapeslloc.lastColumn = 0;
	BEGIN( INITIAL );
}

<NewUnitName>{Identifier} {
	newUnitName = strdup( yytext );
	BEGIN( NewUnitEqual );
}
<NewUnitName>. {
	Ast::theAnalysisErrorsList.push_back( new Exceptions::ScannerError( shapeslloc, strrefdup( "Expected identifier." ) ) );
	BEGIN( DiscardRestOfLineAndBEGIN_INITIAL );
}
<NewUnitEqual>[ \t]*"="[ \t]* { BEGIN( NewUnitValue ); }
<NewUnitEqual>. {
	Ast::theAnalysisErrorsList.push_back( new Exceptions::ScannerError( shapeslloc, strrefdup( "Expected '='." ) ) );
	BEGIN( DiscardRestOfLineAndBEGIN_INITIAL );
 }
<NewUnitValue>{Float}"bp" {
	char * end;
	double newUnitValue = shapes_strtod( yytext, & end );
	typedef typeof unitTable MapType;
	MapType::const_iterator i = unitTable.find( newUnitName );
	if( i != unitTable.end( ) )
		{
			if( i->second != newUnitValue )
				{
					Ast::theAnalysisErrorsList.push_back( new Exceptions::IntroducingExistingUnit( shapeslloc, strrefdup( newUnitName ) ) );
				}
		}
	else
		{
			unitTable[ newUnitName ] = newUnitValue;
		}
	BEGIN( INITIAL );
}
<NewUnitValue>{Float}"mm" {
	char * end;
	double newUnitValue = shapes_strtod( yytext, & end ) * ( 0.1 * 72 / 2.54 );
	typedef typeof unitTable MapType;
	MapType::const_iterator i = unitTable.find( newUnitName );
	if( i != unitTable.end( ) )
		{
			if( i->second != newUnitValue )
				{
					Ast::theAnalysisErrorsList.push_back( new Exceptions::IntroducingExistingUnit( shapeslloc, strrefdup( newUnitName ) ) );
				}
		}
	else
		{
			unitTable[ newUnitName ] = newUnitValue;
		}
	BEGIN( INITIAL );
}
<NewUnitValue>{Float}"cm" {
	char * end;
	double newUnitValue = shapes_strtod( yytext, & end ) * ( 72 / 2.54 );
	typedef typeof unitTable MapType;
	MapType::const_iterator i = unitTable.find( newUnitName );
	if( i != unitTable.end( ) )
		{
			if( i->second != newUnitValue )
				{
					Ast::theAnalysisErrorsList.push_back( new Exceptions::IntroducingExistingUnit( shapeslloc, strrefdup( newUnitName ) ) );
				}
		}
	else
		{
			unitTable[ newUnitName ] = newUnitValue;
		}
	BEGIN( INITIAL );
}
<NewUnitValue>{Float}"m" {
	char * end;
	double newUnitValue = shapes_strtod( yytext, & end ) * ( 100 * 72 / 2.54 );
	typedef typeof unitTable MapType;
	MapType::const_iterator i = unitTable.find( newUnitName );
	if( i != unitTable.end( ) )
		{
			if( i->second != newUnitValue )
				{
					Ast::theAnalysisErrorsList.push_back( new Exceptions::IntroducingExistingUnit( shapeslloc, strrefdup( newUnitName ) ) );
				}
		}
	else
		{
			unitTable[ newUnitName ] = newUnitValue;
		}
	BEGIN( INITIAL );
}
<NewUnitValue>{Float}"in" {
	char * end;
	double newUnitValue = shapes_strtod( yytext, & end ) * 72;
	typedef typeof unitTable MapType;
	MapType::const_iterator i = unitTable.find( newUnitName );
	if( i != unitTable.end( ) )
		{
			if( i->second != newUnitValue )
				{
					Ast::theAnalysisErrorsList.push_back( new Exceptions::IntroducingExistingUnit( shapeslloc, strrefdup( newUnitName ) ) );
				}
		}
	else
		{
			unitTable[ newUnitName ] = newUnitValue;
		}
	BEGIN( INITIAL );
}
<NewUnitValue>{Float}{Identifier} {
	char * end;
	double val = shapes_strtod( yytext, & end );

	typedef typeof unitTable MapType;
	MapType::const_iterator i = unitTable.find( end );
	if( i == unitTable.end( ) )
		{
			Ast::theAnalysisErrorsList.push_back( new Exceptions::LookupUnknownUnit( shapeslloc, strrefdup( end ) ) );
		}
	else
		{
			double newUnitValue = val * i->second;

			i = unitTable.find( newUnitName );
			if( i != unitTable.end( ) )
				{
					if( i->second != newUnitValue )
						{
							Ast::theAnalysisErrorsList.push_back( new Exceptions::IntroducingExistingUnit( shapeslloc, strrefdup( newUnitName ) ) );
						}
				}
			else
				{
					unitTable[ newUnitName ] = newUnitValue;
				}
		}
	BEGIN( INITIAL );
}
<NewUnitValue>. {
	Ast::theAnalysisErrorsList.push_back( new Exceptions::ScannerError( shapeslloc, strrefdup( "Expected a length." ) ) );
	BEGIN( DiscardRestOfLineAndBEGIN_INITIAL );
 }



{Float}"°" {
	char * end;
	shapeslval.floatVal = M_PI / 180 * shapes_strtod( yytext, & end );
	return T_float;
}

{Float}{Escape}"degree" {
	char * end;
	shapeslval.floatVal = M_PI / 180 * shapes_strtod( yytext, & end );
	return T_float;
}

{Float}"bp" {
	char * end;
	shapeslval.floatVal = shapes_strtod( yytext, & end );
	return T_length;
}

{Float}"mm" {
	char * end;
	shapeslval.floatVal = shapes_strtod( yytext, & end ) * ( 0.1 * 72 / 2.54 );
	return T_length;
}

{Float}"cm" {
	char * end;
	shapeslval.floatVal = shapes_strtod( yytext, & end ) * ( 72 / 2.54 );
	return T_length;
}

{Float}"m" {
	char * end;
	shapeslval.floatVal = shapes_strtod( yytext, & end ) * ( 100 * 72 / 2.54 );
	return T_length;
}

{Float}"in" {
	char * end;
	shapeslval.floatVal = shapes_strtod( yytext, & end ) * 72;
	return T_length;
}

{Float}{Identifier} {
	char * end;
	double val = shapes_strtod( yytext, & end );

	typedef typeof unitTable MapType;
	MapType::const_iterator i = unitTable.find( end );
	if( i == unitTable.end( ) )
		{
			Ast::theAnalysisErrorsList.push_back( new Exceptions::LookupUnknownUnit( shapeslloc, strrefdup( end ) ) );
		}
	else
		{
			shapeslval.floatVal = val * i->second;
		}
	return T_length;
}

{Float}[\%][D0] {
	char * end;
	double val = shapes_strtod( yytext, & end );
	shapeslval.expr = new Ast::SpecialLength( shapeslloc, val, Computation::SPECIALU_DIST );
	return T_speciallength;
}

{Float}[\%][C1] {
	char * end;
	double val = shapes_strtod( yytext, & end );
	shapeslval.expr = new Ast::SpecialLength( shapeslloc, val, Computation::SPECIALU_DIST | Computation::SPECIALU_CIRC );
	return T_speciallength;
}

{Float}[\%][M2] {
	char * end;
	double val = shapes_strtod( yytext, & end );
	shapeslval.expr = new Ast::SpecialLength( shapeslloc, val, Computation::SPECIALU_DIST | Computation::SPECIALU_CORR );
	return T_speciallength;
}

{Float}[\%][F3] {
	char * end;
	double val = shapes_strtod( yytext, & end );
	shapeslval.expr = new Ast::SpecialLength( shapeslloc, val, Computation::SPECIALU_DIST | Computation::SPECIALU_CIRC | Computation::SPECIALU_CORR );
	return T_speciallength;
}

{Float}[\%][d4] {
	char * end;
	double val = shapes_strtod( yytext, & end );
	shapeslval.expr = new Ast::SpecialLength( shapeslloc, val, Computation::SPECIALU_DIST | Computation::SPECIALU_NOINFLEX );
	return T_speciallength;
}

{Float}[\%][c5] {
	char * end;
	double val = shapes_strtod( yytext, & end );
	shapeslval.expr = new Ast::SpecialLength( shapeslloc, val, Computation::SPECIALU_DIST | Computation::SPECIALU_CIRC | Computation::SPECIALU_NOINFLEX	);
	return T_speciallength;
}

{Float}[\%][m6] {
	char * end;
	double val = shapes_strtod( yytext, & end );
	shapeslval.expr = new Ast::SpecialLength( shapeslloc, val, Computation::SPECIALU_DIST | Computation::SPECIALU_CORR | Computation::SPECIALU_NOINFLEX	);
	return T_speciallength;
}

{Float}[\%][f7] {
	char * end;
	double val = shapes_strtod( yytext, & end );
	shapeslval.expr = new Ast::SpecialLength( shapeslloc, val, Computation::SPECIALU_DIST | Computation::SPECIALU_CIRC | Computation::SPECIALU_CORR | Computation::SPECIALU_NOINFLEX	);
	return T_speciallength;
}

{Float}[\%][i9] {
	char * end;
	double val = shapes_strtod( yytext, & end );
	shapeslval.expr = new Ast::SpecialLength( shapeslloc, val, Computation::SPECIALU_NOINFLEX );
	return T_speciallength;
}


{Float} {
	char * end;
	shapeslval.floatVal = shapes_strtod( yytext, & end );
	return T_float;
}

"\'"[~]?[0-9]+ {
	const char * src = yytext + 1;
	bool negative = false;
	if( *src == '~' )
		{
			negative = true;
			++src;
		}
	char * end;
	int absval = strtol( src, & end, 10 );
	if( negative )
		{
			shapeslval.intVal = - absval;
		}
	else
		{
			shapeslval.intVal = absval;
		}
	return T_int;
}

"\'0x"[0-9A-F]+ {
	char * end;
	shapeslval.intVal = strtol( yytext + 3, & end, 16 );
	return T_int;
}

"\'0b"[0-1]+ {
	char * end;
	shapeslval.intVal = strtol( yytext + 3, & end, 2 );
	return T_int;
}

"∞" {
	shapeslval.floatVal = HUGE_VAL;
	return T_float;
}

"true" {
	shapeslval.boolVal = true;
	return T_bool;
}

"false" {
	shapeslval.boolVal = false;
	return T_bool;
}

"--" { return T_minusminus; }
"++" { return T_plusplus; }
".." { return T_ddot; }
"..." { return T_dddot; }

"::" { return T_declaretype; }
":=" { return T_assign; }
"{:=}" { return T_flassign; }
"==" { return T_eqeq; }
"=/="|"≠" { return T_eqneq; }

".>" { return T_mapsto; }
"../" { return T_surrounding; }
"->" { return T_bindto; }
"[]" { return T_emptybrackets; }
"[...]" { return T_dddotbrackets; }
"[!]" { return T_bangbrackets; }
"[!...]" { return T_bangdddotbrackets; }
"()" { return T_compose; }

"(>" { return T_unionLeft; }
"<)" { return T_unionRight; }
"<>" { return T_split; }
"(<" { return T_splitLeft; }
">)" { return T_splitRight; }

[\{\}\(\)\[\]\<\>] { return yytext[0]; }
[\.\,\;\:\_\@\!\#\%\&\|\^\-\+\'\"\\] { return yytext[0]; }
[*/~=] { return yytext[0]; }

"*/" { return T_projection; }
"/_" { return T_angle; }
"&|" { return T_ampersandMore; }

"<=" { return T_lesseq; }
">=" { return T_greatereq; }

"<<" { return T_llthan; }
">>" { return T_ggthan; }

"!!" { return	T_bangbang; }

"@@" { return T_atat; }

"and" { return T_and; }
"or" { return T_or; }
"xor" { return T_xor; }
"not" { return T_not; }
"let" { return T_let; }
"let*" { return T_letstar; }
"letrec" { return T_letrec; }

"dynamic" { return T_dynamic; }

"cycle" { return T_cycle; }

"TeX" { return T_tex; }

"continuation" { return T_continuation; }
"continue" { return T_continue; }
"escape_continuation" { return T_esc_continuation; }
"escape_continue" { return T_esc_continue; }

"class" { return T_class; }
"__members__" { return T_members; }
"__prepare__" { return T_prepare; }
"__abstract__" { return T_abstract; }
"__overrides<" { return T_overrides; }
">__" { return T_gr__; }

{WhiteSpace}+ ;

[\n] {
	shapeslloc.firstLine = shapeslloc.lastLine + 1;
	shapeslloc.lastLine = shapeslloc.firstLine;
	shapeslloc.lastColumn = 0;
}

<INITIAL>"|**".*[\n] { ++shapeslloc.lastLine; shapeslloc.lastColumn = 0; }
<INITIAL>"/**" { quoteDepth = 1; BEGIN( Comment ); }
<INITIAL>"**/" { throw Exceptions::ScannerError( shapeslloc, strrefdup( "Found closing comment delimiter outside comment" ) ); }
<Comment>"/**" { ++quoteDepth; more( ); }
<Comment>"**/" {
	--quoteDepth;
	if( quoteDepth == 0 )
		{
			BEGIN( INITIAL );
		}
	else
		{
			more( );
		}
}
<Comment>. { }
<Comment>[\n] { ++shapeslloc.lastLine; shapeslloc.lastColumn = 0; }
<Comment><<EOF>> {
	/* It seems like YY_USER_ACTION is not invoked at EOF, so we do this manually,
	 * however ignornig yyleng (which has the value 1).
	 */
	shapeslloc.firstColumn = shapeslloc.lastColumn;
	throw Exceptions::ScannerError( shapeslloc, strrefdup( "Found EOF while scanning comment" ) );
 }

<INITIAL>[`][\n]? {
	if( yyleng > 1 )
		{
			++shapeslloc.lastLine;
			shapeslloc.lastColumn = 0;
		}
	quoteDepth = 1;
	BEGIN( String );
}
<INITIAL>"´" { throw Exceptions::ScannerError( shapeslloc, strrefdup( "Found closing quote outside string" ) ); }
<String>"`" { ++quoteDepth; more( ); }
<String>"´" {
	--quoteDepth;
	if( quoteDepth == 0 )
		{
			yytext[ yyleng - 2 ] = '\0';
			rinseString( );
			--shapeslloc.firstColumn;
			BEGIN( INITIAL );
			return T_string;
		}
	else
		{
			more( );
		}
}
<String,PoorMansString>[\n] { ++shapeslloc.lastLine; shapeslloc.lastColumn = 0; more( ); }
<String,PoorMansString>{Escape}[`nt\n\"] {
	// The escaped characters each occupy 1 byte
	char * dst = yytext + yyleng - 3; // 3 = 2 + 1
	switch( yytext[ yyleng - 1 ] )
		{
		case 'n':
			*dst = '\n';
			break;
		case 't':
			*dst = '\t';
			break;
		case '\n':
			++shapeslloc.lastLine;
			shapeslloc.lastColumn = 0;
			*dst = '\0';
			break;
		default:
			*dst = yytext[ yyleng - 1 ];
		}
	++dst;
	*dst = '\0';
	++dst;
	*dst = '\0';
	yymore( ); // The purpose of this line is only to let flex know that we use yy_more_flag
	more( );
}
<String,PoorMansString>{Escape}({Escape}|"´") {
	char * dst = yytext;
	char * src = yytext + 2;
	for( ; *src != '\0'; ++dst, ++src )
		{
			*dst = *src;
		}
	*dst = '\0';
	++dst;
	*dst = '\0';
	more( );
}
<String,PoorMansString>{Escape} {
	Ast::theAnalysisErrorsList.push_back( new Exceptions::ScannerError( shapeslloc, strrefdup( "The only characters possible to protect using [¢¤] are [¢¤`´\"nt\n]." ) ) );
	more( );
}
<String,PoorMansString>. { more( ); }
<String,PoorMansString><<EOF>> {
	/* It seems like YY_USER_ACTION is not invoked at EOF, so we do this manually,
	 * however ignornig yyleng (which has the value 1).
	 */
	shapeslloc.firstColumn = shapeslloc.lastColumn;
	throw Exceptions::ScannerError( shapeslloc, strrefdup( "Found EOF while scanning string" ) );
 }

<INITIAL>"(\""[\n]? {
	if( yyleng > 2 )
		{
			++shapeslloc.lastLine;
			shapeslloc.lastColumn = 0;
		}
	quoteDepth = 1;
	BEGIN( PoorMansString );
}
<INITIAL>"\")" { throw Exceptions::ScannerError( shapeslloc, strrefdup( "Found closing poor man's quote outside string" ) ); }
<PoorMansString>"\")" {
	yytext[ yyleng - 2 ] = '\0';
	rinseString( );
	--shapeslloc.firstColumn;
	BEGIN( INITIAL );
	return T_string;
}

<Incl>[^ \t\n]+ {
	currentNeedFile = yytext;
	currentNeedPushCount = 0;
	currentNeedIsNeed = false;
	BEGIN( InclFrom );
}

<Needs>[^ \t\n]+ {
	currentNeedFile = yytext;
	currentNeedPushCount = 0;
	currentNeedIsNeed = true;
	BEGIN( InclFrom );
}

<InclFrom>[ \t]+ { }
<InclFrom>[\n] { doInclusion( ); }
<InclFrom>":"[ \t]+ { BEGIN( InclPath ); }
<InclFrom>":". { throw Exceptions::ScannerError( shapeslloc, strrefdup( "The \":\" must be followed by whitespace." ) ); }
<InclFrom>. { throw Exceptions::ScannerError( shapeslloc, strrefdup( "Expected \":\"." ) ); }

<InclPath>[ \t]+ { }
<InclPath>[^ \t\n]+ {
	push_frontNeedPath( yytext );
	++currentNeedPushCount;
}
<InclPath>[\n] {
	if( currentNeedPushCount == 0 )
		{
			throw Exceptions::ScannerError( shapeslloc, strrefdup( "Missing paths after \"from\"." ) );
		}
	doInclusion( );
}

<Echo>.* {
	std::cerr << yytext << std::endl ;
	BEGIN( INITIAL );
}

<Author>.* {
	if( ! stateStack.empty( ) )
		{
			Kernel::theDocInfo.addExtensionAuthorString( shapeslloc.filename + std::string( " by " ) + yytext );
		}
	else
		{
			if( ! Kernel::theDocInfo.addInfo( "Author", SimplePDF::newString( yytext ) ) )
				{
					Ast::theAnalysisErrorsList.push_back( new Exceptions::ScannerError( shapeslloc, strrefdup( "Multiply specified #author." ) ) );
				}
		}
	BEGIN( INITIAL );
}


<<EOF>> {
	/* It seems like YY_USER_ACTION is not invoked at EOF, so we do this manually,
	 * however ignornig yyleng (which has the value 1).
	 */
	shapeslloc.firstColumn = shapeslloc.lastColumn;

	if( stateStack.empty( ) )
	{
		if( appendStream_ != 0 )
			{
				yy_switch_to_buffer( yy_create_buffer( appendStream_, YY_BUF_SIZE ) );
				appendStream_ = 0;
			}
		else
			{
				return T_EOF;
			}
	}
	else
	{
		yy_delete_buffer( YY_CURRENT_BUFFER );
		yy_switch_to_buffer( stateStack.top( ) );
		stateStack.pop( );
		shapeslloc = locStack.top( );
		locStack.pop( );
		for( size_t tmp = pathCountStack.top( ); tmp > 0; --tmp )
			{
				pop_frontNeedPath( );
			}
		pathCountStack.pop( );
	}
}

{Identifier} {
	shapeslval.str = strdup( yytext );
	return T_identifier;
}
{TypeMark}{Identifier} {
	const char * id = yytext + 2; // The type mark is allways 2 bytes.
	shapeslval.str = strdup( id );
	return T_typename;
}

{DynamicMark}{Identifier} {
	shapeslval.str = strdup( yytext + 1 );
	return T_dynamic_identifier;
}
{StateMark}{Identifier} {
	const char * id = yytext;
	// Depending on which state mark is used, we must skip different number of bytes.
	if( *id == '#' )
		{
			id += 1;
		}
	else // The state mark is '•'
		{
			id += 3;
		}
	shapeslval.str = strdup( id );
	return T_state_identifier;
}
{DynamicMark}{StateMark}{Identifier} {
	const char * id = yytext + 1; // The dynamic mark is allways 1 byte.
	// Depending on which state mark is used, we must skip different number of bytes.
	if( *id == '#' )
		{
			id += 1;
		}
	else // The state mark is '•'
		{
			id += 3;
		}
	shapeslval.str = strdup( id );
	return T_dynamic_state_identifier;
}

. {
	Ast::theAnalysisErrorsList.push_back( new Exceptions::ScannerError( shapeslloc, strrefdup( ( std::string( "Scanner found unrecognized token: " ) + yytext ).c_str( ) ) ) );
	BEGIN( DiscardRestOfLineAndBEGIN_INITIAL );
}

%%
/* The closing %% above marks the end of the Rules section and the beginning
 * of the User Subroutines section. All text from here to the end of the
 * file is copied verbatim to the end of the generated lex.pdf.c file.
 * This section is where you put definitions of helper functions.
 */

double shapes_strtod( char * str, char ** end )
{
	char termTmp;
	char * term = str;
	for( ; *term != '\0'; ++term )
		{
			switch( *term )
				{
				case '0':
				case '1':
				case '2':
				case '3':
				case '4':
				case '5':
				case '6':
				case '7':
				case '8':
				case '9':
				case '.':
					continue;
				case '~':
					*term = '-';
					continue;
				case '*':
					// We replace the "*^" by something in the notation of strtod
					*term = 'e';
					if( *(term+2) == '~' )
						{
							*(term+1) = '-';
							*(term+2) = '0';
							term += 3;
						}
					else
						{
							*(term+1) = '0';
							term += 2;
						}
				}
			break;
		}
	termTmp = *term;
	*term = '\0';
	double val = strtod( str, end );
	*term = termTmp;
	return val;
}

void
ShapesScanner::doInclusion( )
{
	std::string path;
	if( currentNeedIsNeed )
		{
			path = searchFile( currentNeedFile + ".shext" );
		}
	else
		{
			path = searchFile( currentNeedFile + ".shape" );
		}

	if( currentNeedIsNeed )
		{
			struct stat theStat;
			if( stat( path.c_str( ), & theStat ) != 0 )
				{
					throw Exceptions::FileReadOpenError( shapeslloc, strrefdup( path.c_str( ) ), 0, 0, Exceptions::FileReadOpenError::STAT );
				}
			FileID fileID( theStat );
			if( neededFiles.find( fileID ) != neededFiles.end( ) )
				{
					shapeslloc.firstLine = shapeslloc.lastLine + 1;
					shapeslloc.lastLine = shapeslloc.firstLine;
					shapeslloc.lastColumn = 0;
					BEGIN( INITIAL );
					return;
				}
			neededFiles.insert( fileID );
		}

	std::ifstream * iFile = new std::ifstream( path.c_str( ) );
	if( ! iFile->good( ) )
		{
			throw Exceptions::FileReadOpenError( shapeslloc, strrefdup( path.c_str( ) ), 0, 0 );
		}

	shapeslloc.firstLine = shapeslloc.lastLine + 1;
	shapeslloc.lastLine = shapeslloc.firstLine;
	shapeslloc.lastColumn = 0;

	locStack.push( shapeslloc );
	shapeslloc = Ast::SourceLocation( strdup( path.c_str( ) ) );
	stateStack.push( YY_CURRENT_BUFFER );
	yy_switch_to_buffer( yy_create_buffer( iFile, YY_BUF_SIZE ) );
	pathCountStack.push( currentNeedPushCount );

	BEGIN( INITIAL );
}

void
ShapesScanner::prependStream( std::istream * is )
{
	appendStream_ = yyin;
	yy_switch_to_buffer( yy_create_buffer( is, YY_BUF_SIZE ) );
}

