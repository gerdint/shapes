#include "sourcelocation.h"
#include "charconverters.h"
#include "globals.h"

#include <fstream>

using namespace MetaPDF;


Ast::SourceLocation::SourceLocation( )
  : filename( "*uninitialized*" ), firstLine( 1 ), firstColumn( 0 ), lastLine( 1 ), lastColumn( 0 )
{ }

Ast::SourceLocation::SourceLocation( const char * _filename )
  : filename( _filename ), firstLine( 1 ), firstColumn( 0 ), lastLine( 1 ), lastColumn( 0 )
{ }

Ast::SourceLocation::SourceLocation( const Ast::SourceLocation & orig )
  : filename( orig.filename ), firstLine( orig.firstLine ), firstColumn( orig.firstColumn ), lastLine( orig.lastLine ), lastColumn( orig.lastColumn )
{ }

Ast::SourceLocation::SourceLocation( const Ast::SourceLocation & firstLoc, const Ast::SourceLocation & lastLoc )
  : filename( firstLoc.filename ), firstLine( firstLoc.firstLine ), firstColumn( firstLoc.firstColumn ), lastLine( lastLoc.lastLine ), lastColumn( lastLoc.lastColumn )
{ }

bool
Ast::SourceLocation::contains( const Ast::SourceLocation & loc2 ) const
{
  return
    strcmp( filename, loc2.filename ) == 0 &&
    firstLine <= loc2.firstLine &&
    lastLine >= loc2.lastLine &&
    firstColumn <= loc2.firstColumn &&
    lastColumn >= loc2.lastColumn;
}

std::ostream &
Ast::operator << ( std::ostream & os, const Ast::SourceLocation & self )
{
  if( Interaction::characterColumnInBytes )
    {
      if( self.firstLine == self.lastLine )
	{
	  os << self.filename << ":" << self.firstLine << "(" << self.firstColumn << "-" << self.lastColumn << ")" ;
	}
      else
	{
	  os << self.filename << ":" << self.firstLine << "(" << self.firstColumn << ")-" << self.lastLine << "(" << self.lastColumn << ")" ;
	}
    }
  else
    {
      if( self.firstLine == self.lastLine )
	{
	  os << self.filename << ":" << self.firstLine << "("
	     << Ast::SourceLocation::byteColumnToUTF8Column( self.filename, self.firstLine, self.firstColumn )
	     << "-"
	     << Ast::SourceLocation::byteColumnToUTF8Column( self.filename, self.lastLine, self.lastColumn )
	     << ")" ;
	}
      else
	{
	  os << self.filename << ":" << self.firstLine << "("
	     << Ast::SourceLocation::byteColumnToUTF8Column( self.filename, self.firstLine, self.firstColumn )
	     << ")-"
	     << self.lastLine << "("
	     << Ast::SourceLocation::byteColumnToUTF8Column( self.filename, self.lastLine, self.lastColumn )
	     << ")" ;
	}
    }

  return os;
}

size_t
Ast::SourceLocation::byteColumnToUTF8Column( const char * filename, size_t line, size_t byteCol )
{
  static const char * filenameInCache = "";
  size_t lineInCache;
  static std::string cachedLine;

  if( strcmp( filename, filenameInCache ) != 0 ||
      line != lineInCache )
    {
      filenameInCache = filename;
      std::ifstream iFile( filename );
      if( ! iFile.is_open( ) )
	{
	  std::cerr << "Error in error message: Failed to open file pointed to by source location: " << filename << std::endl ;
	  exit( 1 );
	}
      for( lineInCache = 0; lineInCache < line; ++lineInCache )
	{
	  getline( iFile, cachedLine );
	  if( iFile.eof( ) )
	    {
	      std::cerr << "Error in error message: Source location's line (" << line << ") is beyond end of file: " << filename << std::endl ;
	      exit( 1 );	  
	    }
	}
    }

  return byteColumnToUTF8Column( cachedLine, byteCol );  
}

size_t
Ast::SourceLocation::byteColumnToUTF8Column( const std::string & line, size_t byteCol )
{
  iconv_t converter = Helpers::requireUTF8ToUCS4Converter( );
  
  static size_t bufSize = 0;
  static char * buf = 0;
  {
    size_t neededSize = ( byteCol + 1 ) * 4;
    if( bufSize < neededSize )
      {
	if( buf != 0 )
	  {
	    delete buf;
	  }
	buf = new char[ neededSize ];
	bufSize = neededSize;
      }
  }
  

  const char * inbuf = line.c_str( );
  size_t inbytesleft = byteCol;
  char * outbuf = buf;
  size_t outbytesleft = bufSize;
  // For some reason, my iconv header seems unaware of the const modifier...
  size_t count = iconv( converter,
			& inbuf, & inbytesleft,
			& outbuf, & outbytesleft );
  if( count == (size_t)(-1) )
    {
      if( errno == EINVAL )
	{
	  std::cerr << "Error in error message, when converting byte column to utf-8: (EINVAL) Found invalid utf-8 byte sequence." << std::endl ;
	  exit( 1 );	  
	}
      else if( errno == EILSEQ )
	{
	  std::cerr << "Error in error message, when converting byte column to utf-8: (EILSEQ) Found invalid utf-8 byte." << std::endl ;
	  exit( 1 );
	}
      else if( errno == E2BIG )
	{
	  std::cerr << "Error in error message, when converting byte column to utf-8: (E2BIG) Insufficient memory allocated." << std::endl ;
	  exit( 1 );
	}
      else
	{
	  std::cerr << "Error in error message, when converting byte column to utf-8: iconv failed with un unrecognized error code: " << errno << "." << std::endl ;
	  exit( 1 );
	}
    }
  
  return ( bufSize - outbytesleft ) / 4;
}
