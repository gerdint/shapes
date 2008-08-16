%{

#include <cmath>

#include "ssiscanner.h"

#include <cstdlib>
#include <iostream>
#include <fstream>
#include <sstream>

bool strtobool( const char * str, const char * attribute );

%}

%option c++
%option noyywrap
%option yylineno

%option prefix="ssi"
%option yyclass="SSIScanner"

%x INCLUDE
%x INCLUDE_FILENAME
%x INCLUDE_DEPTH
%x INCLUDE_META
%x EXPAND


%%

<INITIAL>"<!--#include"[ \t]+ {
	 currentDepthLimit_ = depthLimitStack_.top( ) - 1;
	 currentMeta_ = false;
	 BEGIN( INCLUDE );
}

<INITIAL>"<?xml"[^?]*"?>" |
<INITIAL>"<!DOCTYPE"[ \t\n]+[^ \t\n]+[ \t\n]*"["[^\]]*"]>" {
	if( onlyDependencies_ )
		{
			// Do nothing
		}
	else
		{
			if( metaInclusionStack_.top( ) )
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
<INCLUDE>"meta"[ \t]*"="[ \t]* { BEGIN( INCLUDE_META ); }
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

<INCLUDE_FILENAME>[\"][^\"]*[\"] |
<INCLUDE_FILENAME>[\'][^\']*[\'] {
	size_t len = strlen( yytext ) ;
	yytext[ len - 1 ] = '\0';
	char * buf = new char[ strlen( includebase_ ) + len - 2 + 1];
	strcpy( buf, includebase_ );
	strcat( buf, yytext + 1 );
	includeFilename_ = expandDefines( buf );
	delete [] buf;
	BEGIN( INCLUDE );
}

<INCLUDE_DEPTH>[\"][^\"]*[\"] |
<INCLUDE_DEPTH>[\'][^\']*[\'] {
	yytext[ strlen( yytext ) - 1 ] = '\0';
	char * endp;
	int depthSigned = strtol( yytext + 1, &endp, 10 );
	if( *endp != '\0' )
		{
			std::cerr << "Invalid depth string in SSI file inclusion: " << ( yytext + 1 ) << std::endl ;
			exit( 1 );
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

<INCLUDE_META>[\"][^\"]*[\"] |
<INCLUDE_META>[\'][^\']*[\'] {
	yytext[ strlen( yytext ) - 1 ] = '\0';
	currentMeta_ = strtobool( yytext + 1, "meta" );
	BEGIN( INCLUDE );
}

<EXPAND>[\"][^\"]*[\"] |
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
		metaInclusionStack_.pop( );
	}
}


<INITIAL>.|\n {
	if( onlyDependencies_ )
		{
			// Do nothing
		}
	else
		{
			ECHO;
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
	metaInclusionStack_.push( currentMeta_ );

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

bool
strtobool( const char * str, const char * attribute )
{
	if( strcmp( str, "yes" ) == 0 ||
			strcmp( str, "true" ) == 0 )
		{
			return true;
		}
	if( strcmp( str, "no" ) == 0 ||
			strcmp( str, "false" ) == 0 )
		{
			return false;
		}
	std::cerr << "Invalid boolean value for the SSI inclusion attribute " << attribute << ": " << str << std::endl ;
	exit( 1 );
}
