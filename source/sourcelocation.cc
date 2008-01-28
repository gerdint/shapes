#include "sourcelocation.h"
#include "charconverters.h"
#include "shapesexceptions.h"
#include "globals.h"
#include "utf8tools.h"
#include "config.h"

#include <cerrno>
#include <fstream>
#include <sstream>
#include <limits>
#include <iomanip>

using namespace Shapes;


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
	if( *(self.filename) == '\0' )
		{
			os << "< unknown location >" ;
		}
	else if( Interaction::characterColumnInBytes )
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
					os << self.filename << ":" << self.firstLine << "(" ;
					{
						size_t col = Ast::SourceLocation::byteColumnToUTF8Column( self.filename, self.firstLine, self.firstColumn );
						if( col != std::numeric_limits< size_t >::max( ) )
							{
								os << col ;
							}
						else
							{
								os << "?" ;
							}
					}
					os << "-" ;
					{
						size_t col = Ast::SourceLocation::byteColumnToUTF8Column( self.filename, self.lastLine, self.lastColumn );
						if( col != std::numeric_limits< size_t >::max( ) )
							{
								os << col ;
							}
						else
							{
								os << "?" ;
							}
					}
					os << ")" ;
				}
			else
				{
					os << self.filename << ":" << self.firstLine << "(" ;
					{
						size_t col = Ast::SourceLocation::byteColumnToUTF8Column( self.filename, self.firstLine, self.firstColumn );
						if( col != std::numeric_limits< size_t >::max( ) )
							{
								os << col ;
							}
						else
							{
								os << "?" ;
							}
					}
					os << ")-"
						 << self.lastLine << "(" ;
					{
						size_t col = Ast::SourceLocation::byteColumnToUTF8Column( self.filename, self.lastLine, self.lastColumn );
						if( col != std::numeric_limits< size_t >::max( ) )
							{
								os << col ;
							}
						else
							{
								os << "?" ;
							}
					}
					os << ")" ;
				}
		}

	return os;
}

size_t
Ast::SourceLocation::byteColumnToUTF8Column( const char * filename, size_t line, size_t byteCol )
{
	static const char * filenameInCache = "";
	static size_t lineInCache = 0;
	static std::string cachedLine;

	if( byteCol == 0 )
		{
			return 0;
		}

	if( strcmp( filename, filenameInCache ) != 0 ||
			line != lineInCache )
		{
			filenameInCache = filename;
			std::ifstream iFile( filename );
			if( ! iFile.is_open( ) )
				{
					std::ostringstream msg;
					msg << "Error in error message: Failed to open file pointed to by source location: " << filename ;
					Kernel::thePostCheckErrorsList.push_back( new Exceptions::InternalError( msg ) );
					return 0;
				}
			for( lineInCache = 1; lineInCache < line; ++lineInCache )
				{
					iFile.ignore( std::numeric_limits< std::streamsize >::max( ), '\n' );
				}
			if( iFile.eof( ) )
				{
					std::ostringstream msg;
					msg << "Error in error message: Source location's line (" << line << ") is way beyond end of file: " << filename ;
					Kernel::thePostCheckErrorsList.push_back( new Exceptions::InternalError( msg ) );
					return 0;
				}
			getline( iFile, cachedLine );
			/* iFile.eod( ) is acceptable here, since it "just" means that there was no newline
			 * terminating the last line in the file.
			 */
//			 if( iFile.eof( ) )
//				 {
//					std::ostringstream msg;
//					msg << "Error in error message: Source location's line (" << line << ") is one beyond end of file: " << filename ;
//					Kernel::thePostCheckErrorsList.push_back( new Exceptions::InternalError( msg );
//					 return 0;
//				 }
		}

	return byteColumnToUTF8Column( cachedLine, byteCol );
}

size_t
Ast::SourceLocation::byteColumnToUTF8Column( const std::string & line, size_t byteCol )
{
	size_t count = 0;
	const char * end = line.c_str( ) + byteCol;
	for( const char * src = line.c_str( ); src != end && *src != '\0'; ++src )
		{
			if( Helpers::utf8leadByte( *src ) )
				{
					++count;
				}
		}
	return count;
}
