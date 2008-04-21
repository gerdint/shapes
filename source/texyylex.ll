%{

#include "texscanner.h"
#include "strrefdup.h"

#include <string.h>
#include <iostream>

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
