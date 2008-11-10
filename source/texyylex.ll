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

#include "texscanner.h"
#include "strrefdup.h"
#include "exitcodes.h"

#include <string.h>
#include <iostream>

#define YY_EXIT_FAILURE Shapes::Interaction::EXIT_INTERNAL_ERROR

%}

EnvironmentName [a-zA-Z0-9_]+

%option c++
%option noyywrap

%option prefix="tex"
%option yyclass="TeXScanner"

%%

"{" {
	delimStack.push_back( BRACE );
}
"[" {
	delimStack.push_back( BRACKET );
}
"\\begin{"{EnvironmentName}"}" {
	delimStack.push_back( BEGIN_END );
}
"}" {
	if( delimStack.empty( ) )
		{
			std::ostringstream msg;
			msg << "There is no opening delimiter to be matched by '" << yytext << "'." ;
			throw strrefdup( msg );
		}
	if( delimStack.back( ) != BRACE )
		{
			std::ostringstream msg;
			msg << "Mismatched closing delimiter '" << yytext << "' (expecting '" << closing( delimStack.back( ) ) << "')." ;
			throw strrefdup( msg );
		}
	delimStack.pop_back( );
}
"]" {
	if( delimStack.empty( ) )
		{
			std::ostringstream msg;
			msg << "There is no opening delimiter to be matched by '" << yytext << "'." ;
			throw strrefdup( msg );
		}
	if( delimStack.back( ) != BRACKET )
		{
			std::ostringstream msg;
			msg << "Mismatched closing delimiter '" << yytext << "' (expecting '" << closing( delimStack.back( ) ) << "')." ;
			throw strrefdup( msg );
		}
	delimStack.pop_back( );
}
"\\end{"{EnvironmentName}"}" {
	if( delimStack.empty( ) )
		{
			std::ostringstream msg;
			msg << "There is no opening delimiter to be matched by '" << yytext << "'." ;
			throw strrefdup( msg );
		}
	if( delimStack.back( ) != BEGIN_END )
		{
			std::ostringstream msg;
			msg << "Mismatched closing delimiter '" << yytext << "' (expecting '" << closing( delimStack.back( ) ) << "')." ;
			throw strrefdup( msg );
		}
	delimStack.pop_back( );
}

"\\{"|"\\}"|"\\["|"\\]"|.|\n {
  // Do nothing
}

<<EOF>> {
  return 0;
}

%%
