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

%}

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

%option c++
%option noyywrap
%option yylineno

%option prefix="ssi"
%option yyclass="SSIScanner"

%x INCLUDE_FILENAME
%x INCLUDE_CLOSING


%%

<INITIAL>"<!--#include"[ \t]+"virtual"[ \t]*"="[ \t]* { BEGIN( INCLUDE_FILENAME ); }
<INCLUDE_FILENAME>[\"][^\"]*[\"] {
	yytext[ strlen( yytext ) - 1 ] = '\0';
	includeFilename_ = strdup( yytext + 1 );
	BEGIN( INCLUDE_CLOSING );
}
<INCLUDE_FILENAME>[\'][^\']*[\'] {
	yytext[ strlen( yytext ) - 1 ] = '\0';
	includeFilename_ = strdup( yytext + 1 );
	BEGIN( INCLUDE_CLOSING );
}
<INCLUDE_CLOSING>[ \t]*"-->" {
	doInclusion( );
	BEGIN( INITIAL );
}
<<EOF>> {

	if( onlyDependencies_ && ! depStack_.top( ).empty( ) )
		{
			*yyout << filenameStack_.top( ) << ":" ;
			for( std::list< const char * >::const_iterator i = depStack_.top( ).begin( ); i != depStack_.top( ).end( ); ++i )
				{
					*yyout << " " << *i ;
				}
			*yyout << std::endl ;
		}
	depStack_.pop( );
	filenameStack_.pop( );

	/* It seems like YY_USER_ACTION is not invoked at EOF, so we do this manually,
	 * however ignornig yyleng (which has the value 1).
	 */
	if( stateStack_.size( ) == 0 )
	{
		return 0;
	}
	else
	{
		yy_delete_buffer( YY_CURRENT_BUFFER );
		yy_switch_to_buffer( stateStack_.top( ) );
		stateStack_.pop( );
	}
}


<INITIAL>.|\n {
	if( onlyDependencies_ )
		{
			// Do nothing
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

void
SSIScanner::doInclusion( )
{
	depStack_.top( ).push_back( includeFilename_ );

	std::ifstream * iFile = new std::ifstream( includeFilename_ );
	if( ! iFile->good( ) )
		{
			std::cerr << "Failed to open included file: " << includeFilename_ << std::endl ;
			exit( 1 );
		}

	stateStack_.push( YY_CURRENT_BUFFER );
	yy_switch_to_buffer( yy_create_buffer( iFile, YY_BUF_SIZE ) );

	filenameStack_.push( includeFilename_ );
	depStack_.push( std::list< const char * >( ) );

	BEGIN( INITIAL );
}

