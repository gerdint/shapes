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
			throw Exceptions::FileOpenError( shapeslloc, strrefdup( suffix ), 0, 0 );
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
			else
				{
					res = sourceDir_ + "/" + *i + suffix;
				}

			struct stat theStatDummy;
			if( stat( res.c_str( ), & theStatDummy ) == 0 )
				{
					goto foundFile;
				}
		}
	throw Exceptions::FileOpenError( shapeslloc, strrefdup( suffix ), & sourceDir_, & needSearchPath );

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
	shapeslval.str = new char[ yyleng - 1 ];
	char * dst = shapeslval.str;
	char * src = yytext;
	char * end = yytext + yyleng - 2;		// the "- 1" comes from empirical studies...
	for( ; src != end; ++src )
		{
			if( *src != 0 )
				{
					*dst = *src;
					++dst;
				}
		}
	/* The following condition catches the optional trailing newline at the end of
	 * the literal.	That this condition is non-trivial indicates that this feature
	 * must be used with care.	For example, when the string creation is automated, either
	 * should all newlines be written `¢n´, or should the optional newlines always be there.
	 * The last of the three conditions ensures that the trailing newline was not generated
	 * as an escape sequence, in which case it shall be kept.
	 */
	if( dst > shapeslval.str && *( dst - 1 ) == '\n' && *( src - 1 ) != '\0' )
		{
			--dst;
		}
	*dst = '\0';
}
