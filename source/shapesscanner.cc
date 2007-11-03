#include "metapdfscanner.h"
#include "yyltype.h"

#include "metapdftypes.h"
#include "ast.h"
#include "astflow.h"
#include "astclass.h"

using namespace MetaPDF;
#include "metapdfparser.tab.h"

#include <iostream>

using namespace std;


MetaPDFScanner::MetaPDFScanner( istream * yyin, ostream * yyout )
  : yyFlexLexer( yyin, yyout ), moreState( false ), lastleng( 0 ), 
    showFiles( false ), randSeedSet( false ), // loc( "<?>" ),
    appendStream_( 0 )
{
  metapdflloc.filename = "<?>";
  unitTable[ "bp" ] = 1;
  unitTable[ "mm" ] = 0.1 * 72 / 2.54;
  unitTable[ "cm" ] = 72 / 2.54;
  unitTable[ "m" ] = 100 * 72 / 2.54;
  unitTable[ "in" ] = 72;
}

MetaPDFScanner::~MetaPDFScanner( )
{ }

void
MetaPDFScanner::setNameOf_yyin( const char * yyinName )
{
  metapdflloc.filename = yyinName;
}


// The following method is placed in metapdfyylex to access YY_BUF_SIZE
// MetaPDFScanner::prependStream( std::istream * is )

void
MetaPDFScanner::push_backNeedPath( const char * path )
{
  if( path[ strlen( path ) - 1 ] == '/' )
    {
      throw Exceptions::ScannerError( metapdflloc, strrefdup( "The entries in the search path must not include a trailing slash." ) );
    }
  needSearchPath.push_back( std::string( path ) );
}

void
MetaPDFScanner::push_frontNeedPath( const char * path )
{
  if( path[ strlen( path ) - 1 ] == '/' )
    {
      throw Exceptions::ScannerError( metapdflloc, strrefdup( "The entries in the search path must not include a trailing slash." ) );
    }
  needSearchPath.push_front( std::string( path ) );
}

void
MetaPDFScanner::pop_frontNeedPath( )
{
  needSearchPath.pop_front( );
}

void
MetaPDFScanner::setShowFiles( bool _showFiles )
{
  showFiles = _showFiles;
}


void
MetaPDFScanner::more( )
{
  moreState = true; // This one is for ourselves to use in DoBeforeEachAction
  yy_more_flag = 1; // This one is for flex, and will be reset before we reach doBeforeEachAction
  lastleng = yyleng;
}

void
MetaPDFScanner::doBeforeEachAction( )
{
  if( moreState )
    {
      metapdflloc.lastColumn += yyleng - lastleng;
    }
  else
    {
      metapdflloc.firstLine = metapdflloc.lastLine;
      metapdflloc.firstColumn = metapdflloc.lastColumn;
      metapdflloc.lastColumn = metapdflloc.firstColumn + yyleng;
    }
  moreState = false;
}

double
MetaPDFScanner::lookupUnitFactor( const char * name ) const
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
MetaPDFScanner::searchFile( const std::string & suffix ) const
{
  std::string res;

  if( suffix.size( ) == 0 )
    {
      throw Exceptions::InternalError( strrefdup( "MetaPDFScanner::searchFile called with empty argument." ) );
    }

  if( suffix[ 0 ] == '/' )
    {
      res = suffix;
      struct stat theStatDummy;
      if( stat( res.c_str( ), & theStatDummy ) == 0 )
	{
	  goto foundFile;
	}
      throw Exceptions::FileOpenError( metapdflloc, strrefdup( suffix ) );
    }
  
  if( needSearchPath.size( ) == 0 )
    {
      throw Exceptions::ScannerError( metapdflloc, strrefdup( "Relative file inclusion impossible since search path is empty." ) );
    }

  typedef typeof needSearchPath ListType;
  for( ListType::const_iterator i = needSearchPath.begin( ); i != needSearchPath.end( ); ++i )
    {
      res = *i + "/" + suffix;

      struct stat theStatDummy;
      if( stat( res.c_str( ), & theStatDummy ) == 0 )
	{
	  goto foundFile;
	}
    }
  throw Exceptions::FileOpenError( metapdflloc, strrefdup( suffix ) );

 foundFile:
  if( showFiles )
    {
      for( size_t i = 0; i < locStack.size( ); ++i )
	{
	  std::cerr << "  " ;
	}
      std::cerr << "  " << res << std::endl ;
    }
  return res;
}
