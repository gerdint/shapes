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

#include "shapesscanner.h"
#include "yyltype.h"

#include "shapestypes.h"
#include "ast.h"
#include "astflow.h"
#include "astclass.h"

using namespace Shapes;
#include "shapesparser.h"

#include <iostream>

using namespace std;


ShapesScanner::ShapesScanner( istream * yyin, ostream * yyout )
	: yyFlexLexer( yyin, yyout ), moreState( false ), lastleng( 0 ),
		showFiles( false ), randSeedSet( false ), // loc( "<?>" ),
		appendStream_( 0 )
{
	shapeslloc.filename = "<?>";
	unitTable[ "bp" ] = 1;
	unitTable[ "mm" ] = 0.1 * 72 / 2.54;
	unitTable[ "cm" ] = 72 / 2.54;
	unitTable[ "m" ] = 100 * 72 / 2.54;
	unitTable[ "in" ] = 72;
}

ShapesScanner::~ShapesScanner( )
{ }

void
ShapesScanner::setNameOf_yyin( const char * yyinName )
{
	shapeslloc.filename = yyinName;
}


// The following method is placed in shapesyylex to access YY_BUF_SIZE
// ShapesScanner::prependStream( std::istream * is )

void
ShapesScanner::setSourceDir( const std::string & sourceDir )
{
	sourceDir_ = sourceDir;
}

void
ShapesScanner::push_backNeedPath( const std::string & path )
{
	if( path.empty( ) ||
			path[ path.size( ) - 1 ] == '/' )
		{
			needSearchPath.push_back( path );
		}
	else
		{
			needSearchPath.push_back( path + "/" );
		}
}

void
ShapesScanner::push_frontNeedPath( const std::string & path )
{
	if( path.empty( ) ||
			path[ path.size( ) - 1 ] == '/' )
		{
			needSearchPath.push_front( path );
		}
	else
		{
			needSearchPath.push_front( path + "/" );
		}
}

void
ShapesScanner::pop_frontNeedPath( )
{
	needSearchPath.pop_front( );
}

void
ShapesScanner::setShowFiles( bool _showFiles )
{
	showFiles = _showFiles;
}


void
ShapesScanner::more( )
{
	moreState = true; // This one is for ourselves to use in DoBeforeEachAction
	yy_more_flag = 1; // This one is for flex, and will be reset before we reach doBeforeEachAction
	lastleng = yyleng;
}

void
ShapesScanner::doBeforeEachAction( )
{
	if( moreState )
		{
			shapeslloc.lastColumn += yyleng - lastleng;
		}
	else
		{
			shapeslloc.firstLine = shapeslloc.lastLine;
			shapeslloc.firstColumn = shapeslloc.lastColumn;
			shapeslloc.lastColumn = shapeslloc.firstColumn + yyleng;
		}
	moreState = false;
}

double
ShapesScanner::lookupUnitFactor( const char * name ) const
{
	typedef typeof unitTable MapType;
	MapType::const_iterator i = unitTable.find( name );
	if( i == unitTable.end( ) )
		{
			return -1;
		}
	return 1 / i->second;
}

Concrete::Length
ShapesScanner::strtoLength( const char * str ) const
{
	char * endp;
	double scalar = strtod( str, &endp );
	typedef typeof unitTable MapType;
	MapType::const_iterator i = unitTable.find( endp );
	if( i == unitTable.end( ) )
		{
			throw "Malformed length.";
		}
	return scalar * i->second;
}

std::string
ShapesScanner::searchFile( const std::string & suffix ) const
{
	std::string res;

	if( suffix.empty( ) )
		{
			throw Exceptions::InternalError( strrefdup( "ShapesScanner::searchFile called with empty argument." ) );
		}

	if( suffix[ 0 ] == '/' )
		{
			res = suffix;
			struct stat theStatDummy;
			if( stat( res.c_str( ), & theStatDummy ) == 0 )
				{
					goto foundFile;
				}
			throw Exceptions::FileReadOpenError( shapeslloc, strrefdup( suffix ), 0, 0 );
		}

	if( needSearchPath.empty( ) )
		{
			throw Exceptions::ScannerError( shapeslloc, strrefdup( "Relative file inclusion impossible since search path is empty." ) );
		}

	typedef typeof needSearchPath ListType;
	for( ListType::const_iterator i = needSearchPath.begin( ); i != needSearchPath.end( ); ++i )
		{
			if( (*i)[0] == '/' )
				{
					res = *i + suffix;
				}
			else if( *i == "./" )
				{
					res = sourceDir_ + suffix;
				}
			else
				{
					res = sourceDir_ + *i + suffix;
				}

			struct stat theStatDummy;
			if( stat( res.c_str( ), & theStatDummy ) == 0 )
				{
					goto foundFile;
				}
		}
	throw Exceptions::FileReadOpenError( shapeslloc, strrefdup( suffix ), & sourceDir_, & needSearchPath );

 foundFile:
	if( showFiles )
		{
			for( size_t i = 0; i < locStack.size( ); ++i )
				{
					std::cerr << "	" ;
				}
			std::cerr << "	" << res << std::endl ;
		}
	return res;
}

void
ShapesScanner::rinseString( )
{
	/* Both types of strings are terminated by a two byte sequence, ") or ´. */
	size_t bytecount = yyleng - ( ( yyleng >= 3 && yytext[ yyleng - 3 ] == '\n' ) ? 3 : 2 );
	char * res = new char[ bytecount + 1 ];
	memcpy( res, yytext, bytecount );
	res[ bytecount ] = '\0';
	shapeslval.Lang_String = new Lang::String( RefCountPtr< const char >( res ), bytecount );
}

void
ShapesScanner::concatenateDataString( )
{
	/* Remember: strcpy, strdup and friends may fail here, since the string may contain zeros. */

	char * res = new char[ dataStringTotalLength_ + 1 ];
	char * dst = res;
	while( ! dataStringChunks_.empty( ) )
		{
			char * ptr = dataStringChunks_.front( ).first;
			memcpy( dst, ptr, dataStringChunks_.front( ).second );
			dst += dataStringChunks_.front( ).second;
			delete ptr;
			dataStringChunks_.pop_front( );
		}
	*dst = '\0';
	shapeslval.Lang_String = new Lang::String( RefCountPtr< const char >( res ), dataStringTotalLength_ );
}
