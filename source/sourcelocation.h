#ifndef sourcelocation_h
#define sourcelocation_h

#include <stddef.h>
#include <iostream>

namespace MetaPDF
{
  namespace Ast
  {
    class SourceLocation
    {
    public:
      const char * filename;
      size_t firstLine;
      size_t firstColumn;
      size_t lastLine;
      size_t lastColumn;
      
      SourceLocation( );
      SourceLocation( const char * _filename );
      SourceLocation( const SourceLocation & orig );
      SourceLocation( const SourceLocation & firstLoc, const Ast::SourceLocation & lastLoc );
      bool contains( const SourceLocation & loc2 ) const;
      friend std::ostream & operator << ( std::ostream & os, const SourceLocation & self );
    };
    
    extern SourceLocation THE_UNKNOWN_LOCATION;
  }
}

#endif
