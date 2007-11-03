#ifndef sourcelocation_h
#define sourcelocation_h

#include <stddef.h>
#include <iostream>

namespace Shapes
{
  namespace Ast
  {
    class SourceLocation
    {
    public:
      const char * filename;
      size_t firstLine;
      size_t firstColumn; // In bytes, shall be converted to utf-8 position when displaying.
      size_t lastLine;
      size_t lastColumn; // In bytes, shall be converted to utf-8 position when displaying.
      
      SourceLocation( );
      SourceLocation( const char * _filename );
      SourceLocation( const SourceLocation & orig );
      SourceLocation( const SourceLocation & firstLoc, const Ast::SourceLocation & lastLoc );
      bool contains( const SourceLocation & loc2 ) const;
      friend std::ostream & operator << ( std::ostream & os, const SourceLocation & self );

      static size_t byteColumnToUTF8Column( const char * filename, size_t line, size_t byteCol );
      static size_t byteColumnToUTF8Column( const std::string & line, size_t byteCol );
    };
    
    extern SourceLocation THE_UNKNOWN_LOCATION;
  }
}

#endif
