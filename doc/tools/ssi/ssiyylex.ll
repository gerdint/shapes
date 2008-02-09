%{

/* The text within this first region delimited by %{ and %} is assumed to
 * be C/C++ code and will be copied verbatim to the lex.pdf.c file ahead
 * of the definitions of the pdflex() function. Add other header file inclusions
 * or C++ variable declarations/prototypes that are needed by your code here.
 */

#include <cmath>

#include "ssiscanner.h"

#include <cstdlib>
#include <iostream>
#include <fstream>
#include <sstream>

 /*
	* The section before the first %% is the Definitions section of the lex
	* input file. Here is where you set options for the scanner, define lex
	* states, and can set up definitions to give names to regular expressions
	* as a simple substitution mechanism that allows for more readable
	* entries in the Rules section later.
	*/

/*
	At the moment, escape characters must occypy exactly 2 bytes.
*/

%}

%option c++
%option noyywrap
%option yylineno

%option prefix="ssi"
%option yyclass="SSIScanner"

%x INCLUDE
%x INCLUDE_FILENAME
%x INCLUDE_DEPTH
%x EXPAND


%%

<INITIAL>"<!--#include"[ \t]+ {
	 currentDepthLimit_ = depthLimitStack_.top( ) - 1;
	 BEGIN( INCLUDE );
}

<INITIAL>"<?xml"[^?]*"?>" {
	if( onlyDependencies_ )
		{
			// Do nothing
		}
	else
		{
			if( stateStack_.empty( ) )
				{
					ECHO;
				}
			else
				{
					*yyout << "<!--SSI comment: " ;
					ECHO;
					*yyout << "-->" ;
				}
		}
}

<INCLUDE>[ \t]+ { }
<INCLUDE>"virtual"[ \t]*"="[ \t]* { BEGIN( INCLUDE_FILENAME ); }
<INCLUDE>"depth"[ \t]*"="[ \t]* { BEGIN( INCLUDE_DEPTH ); }
<INCLUDE>"-->" {
	doInclusion( );
	BEGIN( INITIAL );
}

<INITIAL>"<!--#expand-next-string"[ \t]*"-->"[ \t]* {
	if( ! onlyDependencies_ )
		{
			BEGIN( EXPAND );
		}
 }

<INCLUDE_FILENAME>[\"][^\"]*[\"] {
	yytext[ strlen( yytext ) - 1 ] = '\0';
	includeFilename_ = expandDefines( yytext + 1 );
	BEGIN( INCLUDE );
}
<INCLUDE_FILENAME>[\'][^\']*[\'] {
	yytext[ strlen( yytext ) - 1 ] = '\0';
	includeFilename_ = expandDefines( yytext + 1 );
	BEGIN( INCLUDE );
}

<INCLUDE_DEPTH>[\"][^\"]*[\"] {
	yytext[ strlen( yytext ) - 1 ] = '\0';
	char * endp;
	int depthSigned = strtol( yytext + 1, &endp, 10 );
	if( *endp != '\0' )
		{
			std::cerr << "Invalid depth string in SSI file inclusion: " << ( yytext + 1 ) << std::endl ;
		}
	size_t depthTmp = 0;
	if( depthSigned > 0 )
		{
			depthTmp = depthSigned;
		}
	if( depthTmp < currentDepthLimit_ )
		{
			currentDepthLimit_ = depthTmp;
		}
	BEGIN( INCLUDE );
}
<INCLUDE_DEPTH>[\'][^\']*[\'] {
	yytext[ strlen( yytext ) - 1 ] = '\0';
	char * endp;
	int depthSigned = strtol( yytext + 1, &endp, 10 );
	if( *endp != '\0' )
		{
			std::cerr << "Invalid depth string in SSI file inclusion: " << ( yytext + 1 ) << std::endl ;
		}
	size_t depthTmp = 0;
	if( depthSigned > 0 )
		{
			depthTmp = depthSigned;
		}
	if( depthTmp < currentDepthLimit_ )
		{
			currentDepthLimit_ = depthTmp;
		}
	BEGIN( INCLUDE );
}

<EXPAND>[\"][^\"]*[\"] {
	char stringDelim = yytext[ 0 ];
	yytext[ strlen( yytext ) - 1 ] = '\0';
	const char * expanded = expandDefines( yytext + 1 );
	*yyout << stringDelim << expanded << stringDelim ;
	delete( expanded );
	BEGIN( INITIAL );
}
<EXPAND>[\'][^\']*[\'] {
	char stringDelim = yytext[ 0 ];
	yytext[ strlen( yytext ) - 1 ] = '\0';
	const char * expanded = expandDefines( yytext + 1 );
	*yyout << stringDelim << expanded << stringDelim ;
	delete( expanded );
	BEGIN( INITIAL );
}
<<EOF>> {

	/* It seems like YY_USER_ACTION is not invoked at EOF, so we do this manually,
	 * however ignornig yyleng (which has the value 1).
	 */
	if( stateStack_.empty( ) )
	{
		return 0;
	}
	else
	{
		yy_delete_buffer( YY_CURRENT_BUFFER );
		yy_switch_to_buffer( stateStack_.top( ) );
		stateStack_.pop( );
		depthLimitStack_.pop( );
	}
}


<INITIAL>.|\n {
	if( onlyDependencies_ )
		{
			ECHO;
		}
	else
		{
			*yyout << yytext ;
		}
}

%%
/* The closing %% above marks the end of the Rules section and the beginning
 * of the User Subroutines section. All text from here to the end of the
 * file is copied verbatim to the end of the generated lex.pdf.c file.
 * This section is where you put definitions of helper functions.
 */

char *
SSIScanner::expandDefines( const char * str )
{
	std::ostringstream os;
	for( const char * src = str; *src != '\0'; ++src )
		{
			if( *src != '$' )
				{
					os << *src ;
					continue;
				}
			++src;
			if( *src == '$' )
				{
					os << *src ;
					continue;
				}
			if( *src != '(' )
				{
					std::cerr << "Expected $ or ( after $ in string to be expanded: " << str << std::endl ;
					exit( 1 );
				}
			++src;
			char * close = strchr( src, ')' );
			if( close == 0 )
				{
					std::cerr << "Missing closing ) after $( in string to be expanded: " << str << std::endl ;
					exit( 1 );
				}
			*close = '\0';
			const char * expansion = getenv( src );
			if( expansion == 0 )
				{
					std::cerr << "The variable " << src ;
					*close = ')';
					std::cerr << " was not defined in string to be expanded: " << str << std::endl ;
					exit( 1 );
				}
			os << expansion ;
			src = close; // It will be incremented once more by the for loop.
		}
	return strdup( os.str( ).c_str( ) );
}

void
SSIScanner::doInclusion( )
{
	if( depthLimitStack_.top( ) == 0 )
		{
			return;
		}
	depthLimitStack_.push( currentDepthLimit_ );

	if( onlyDependencies_ )
		{
			*yyout << " " << includeFilename_ ;
		}

	std::ifstream * iFile = new std::ifstream( includeFilename_ );
	if( ! iFile->good( ) )
		{
			std::cerr << "Failed to open included file: " << includeFilename_ << std::endl ;
			exit( 1 );
		}

	stateStack_.push( YY_CURRENT_BUFFER );
	yy_switch_to_buffer( yy_create_buffer( iFile, YY_BUF_SIZE ) );
}

