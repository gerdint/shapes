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
	const char * filearg = yytext + 1;  /* Skip leading delimiter. */
	yytext[ strlen( yytext ) - 1 ] = '\0'; /* Remove trailing delimiter. */
	if( filearg[0] == '\0' )
		{
			std::cerr << "Found empty file name in #include directive." << std::endl ;
			exit( 1 );
		}
	includeFilename_ = expandDefines( filearg );
	if( includeFilename_.compare( 0, 2, "^/" ) == 0 )
		{
			includeFilename_ = includeFilename_.substr( 2 );
		}
	else if( includeFilename_[0] != '/' )
		{
			includeFilename_ = dirStack_.top( ) + includeFilename_;
		}
	/* Here, we could compact away outward directory references, but we're too lazy at the moment. */
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
	if( ! onlyDependencies_ )
		{
			*yyout << stringDelim << expanded << stringDelim ;
		}
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
		dirStack_.pop( );
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

	if( onlyDependencies_ )
		{
			*yyout << " " << includeFilename_ ;
		}

	std::ifstream * iFile = new std::ifstream;
	typedef std::vector< std::string >::const_iterator cItr;
	for( cItr i = includePath_.begin( ); ! iFile->is_open( ) && i != includePath_.end( ); ++i )
		{
			iFile->open( ( *i + includeFilename_ ).c_str( ) );
		}
	if( ! iFile->is_open( ) || iFile->bad( ) )
		{
			if( onlyDependencies_ )
				{
					if( currentDepthLimit_ == 0 )
						{
							return;
						}
					std::cerr << "Missing #include file is included with depth greater than zero: " << includeFilename_ << std::endl ;
					exit( 1 );
				}
			std::cerr << "Failed to open included file: " << includeFilename_ << std::endl ;
			exit( 1 );
		}

	depthLimitStack_.push( currentDepthLimit_ );
	metaInclusionStack_.push( currentMeta_ );
	/* Here, we know that the filename is an absolute path, so we just discard what's after the
	 * last slash, and we get the directory.
	 */
	dirStack_.push( includeFilename_.substr( 0, includeFilename_.rfind( '/' ) + 1 ) );
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
