#include "sourcelocation.h"

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
  if( self.firstLine == self.lastLine )
    {
      os << self.filename << ":" << self.firstLine << "(" << self.firstColumn << "-" << self.lastColumn << ")" ;
    }
  else
    {
      os << self.filename << ":" << self.firstLine << "(" << self.firstColumn << ")-" << self.lastLine << "(" << self.lastColumn << ")" ;
    }
  return os;
}
