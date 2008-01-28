#include "glyphlist.h"
#include "charconverters.h"
#include "autoonoff.h"
#include "shapesexceptions.h"
#include "charconverters.h"
#include "utf8tools.h"
#include "config.h"

#include <cstring>
#include <sstream>
#include <errno.h>	// How come iconv is not enough?
#include <iomanip>

using namespace FontMetrics;

// This function is rather special, since it assumes that numbers are written in groups
// of four characters, separated by exactly one space.
// Use it with care!
// If this fails since we're not given a singleton character, this is indicated by setting
// *end = src and returning 0.
FontMetrics::GlyphList::UnicodeType
glyphlist_strtol( char * src, char ** end, iconv_t converterUTF16BEToUCS4 )
{
	const size_t BUF_SIZE = 16;
	static char bufUTF16BE[ BUF_SIZE ];

	char * start = src;

//	 std::cerr << "src: " ;
//	 for( const char * s = src; *s != '\0' && *s != '\r'; ++s )
//		 {
//			 std::cerr << *s ;
//		 }

	size_t inbytesleft = 0;
	{
		char buf[ 4 ];
		buf[ 2 ] = '\0';
		unsigned char * dst = reinterpret_cast< unsigned char * >( bufUTF16BE );
		while( true )
			{
				// Read most significant byte
				buf[ 0 ] = *src;
				++src;
				buf[ 1 ] = *src;
				++src;
				*dst = strtol( buf, end, 16 );
				++dst;

				// Read least significant byte
				buf[ 0 ] = *src;
				++src;
				buf[ 1 ] = *src;
				++src;
				*dst = strtol( buf, end, 16 );
				++dst;

				inbytesleft += 2;

				if( *src != ' ' )
					{
						// end already points at the end.
						break;
					}
				++src;
			}
	}

//	 std::cerr << " (" << inbytesleft << " bytes: " ;
//	 for( size_t i = 0; i < inbytesleft; i += 2 )
//		 {
//			 std::cerr << std::setiosflags(std::ios::hex) << *reinterpret_cast< uint16_t * >( bufUTF16BE + i )
//								 << std::resetiosflags(std::ios::hex) ;
//		 }
//	 std::cerr << ")" ;

	const char * inbuf = bufUTF16BE;

	FontMetrics::GlyphList::UnicodeType res;
	char * outbuf = reinterpret_cast< char * >( & res );;
	size_t outbytesleft = sizeof( FontMetrics::GlyphList::UnicodeType );

	// The ICONV_CAST macro is defined in config.h.
	size_t count = iconv( converterUTF16BEToUCS4,
												ICONV_CAST( & inbuf ), & inbytesleft,
												& outbuf, & outbytesleft );
	if( count == (size_t)(-1) )
		{
			if( errno == EILSEQ )
				{
					throw "Conversion from UTF-16BE to UCS-4 failed do to illegal value.";
				}
			else if( errno == EINVAL )
				{
					throw "Conversion from UTF-16BE to UCS-4 failed do to incomplete value.";
				}
			else if( errno == E2BIG )
				{
					*end = start;
					return 0;
				}
			else
				{
					throw "Conversion from UTF-16BE to UCS-4 failed with an unrecognized error code.";
				}
		}
	if( outbytesleft != 0 )
		{
			throw "Conversion from UTF-16BE to UCS-4 produced to output.";
		}
//	 std::cerr << " --> " 
//						 << std::setiosflags(std::ios::hex) << res
//						 << std::resetiosflags(std::ios::hex)
//						 << std::endl ;
	return res;
}

GlyphList::GlyphList( std::istream & iFile )
{
	namePtrs_.resize( TABLE_SIZE, 0 );
	readfile( iFile );
}

GlyphList::~GlyphList( )
{ }

bool
GlyphList::UCS4_to_name( UnicodeType code, const char ** dst ) const
{
	if( code < TABLE_SIZE )
		{
			*dst = namePtrs_[ code ];
			return *dst != 0;
		}

	typedef typeof namePtrsWide_ MapType;
	MapType::const_iterator i = namePtrsWide_.find( code );
	if( i == namePtrsWide_.end( ) )
		{
			return false;
		}

	*dst = i->second;
	return true;
}

bool
GlyphList::UTF8_to_name( const char * code, const char ** dst ) const
{
	// I've read that a UTF-8 value may occupy 6 bytes.	Hence assume we get no more than 6 bytes of input.
	// Then forget that this should be just one character.	Then this could be 6 single byte code points.
	// Since it seems like a UTF-16BE value my occupy 8 bytes (at least there are this big numbers in glyphlist.txt),
	// 6 * 8 = 48 bytes should be enough.
	const size_t BUF_SIZE = 64;
	static char buf[ BUF_SIZE ];

	if( *code == '\0' )
		{
			throw Shapes::Exceptions::MiscellaneousRequirement( "When converting a single UTF-8 value to UTF-16BE:	The source value is empty." );
		}

	size_t inbytesMax = strlen( code );
	if( 8 * inbytesMax > BUF_SIZE )	// 8 for the size of a UTF-16BE code point.
		{
			throw Shapes::Exceptions::MiscellaneousRequirement( "When converting a single UTF-8 value to UTF-16BE:	This many bytes definitely represents more than one UTF-8 character." );
		}

	{
		// Make sure there is just one character in the string.
		size_t count = 0;
		for( const char * src = code; *src != '\0'; ++src )
			{
				if( Shapes::Helpers::utf8leadByte( *src ) )
					{
						++count;
					}
			}
		if( count != 1 )
			{
				throw Shapes::Exceptions::MiscellaneousRequirement( "When converting a single UTF-8 value to UTF-16BE:	There was not exactly one character." );
			}
	}


	iconv_t converter = Shapes::Helpers::requireUTF8ToUCS4Converter( );

	const char * inbuf = code;
	size_t inbytesleft = inbytesMax;
	char * outbuf = buf;
	size_t outbytesleft = BUF_SIZE;
	// The ICONV_CAST macro is defined in config.h.
	size_t count = iconv( converter,
												ICONV_CAST( & inbuf ), & inbytesleft,
												& outbuf, & outbytesleft );
	if( count == (size_t)(-1) )
		{
			if( errno == EINVAL )
				{
					throw Shapes::Exceptions::ExternalError( "The single UTF-8 character to be converted to UTF-16BE was incomplete." );
				}
			else if( errno == EILSEQ )
				{
					throw Shapes::Exceptions::ExternalError( "An invalid UTF-8 byte was encountered." );
				}
			else if( errno == E2BIG )
				{
					throw Shapes::Exceptions::InternalError( "The buffer allocated for UTF-8 to UTF-16BE conversion was too small." );
				}
			else
				{
					std::ostringstream msg;
					msg << "iconv failed with an unrecognized error code: " << errno ;
					throw Shapes::Exceptions::InternalError( strrefdup( msg ) );
				}
		}
	size_t bytesUsed = outbuf - buf;
	if( bytesUsed > 8 )
		{
			throw Shapes::Exceptions::ExternalError( "Conversion of one UTF-8 character to UTF-16BE resulted in more than 8 bytes." );
		}

	// Next we proceed in two steps.	I can't see what the probelm here is, but it could be some alignment stuff...
	// 1) Place in most significant bytes of a UnicodeType, with crap to the left.
	UnicodeType codeUCS4 = *reinterpret_cast< const UnicodeType * >( buf ) >> ( 8 * ( sizeof( UnicodeType ) - bytesUsed ) );
	return UCS4_to_name( codeUCS4, dst );
}

bool
GlyphList::name_to_UCS4( const char * name, UnicodeType * dst ) const
{
	typedef typeof nameMap_ MapType;
	MapType::const_iterator i = nameMap_.find( name );
	if( i == nameMap_.end( ) )
		{
			return false;
		}
	*dst = i->second;
	return true;
}

void
GlyphList::readfile( std::istream & iFile )
{
	size_t BUF_SIZE = 255;
	char buf[ BUF_SIZE ];

	iconv_t converterUTF16BEToUCS4 = Shapes::Helpers::requireUTF16BEToUCS4Converter( );

	char c;
	iFile.get( c );
	while( c == '#' )
		{
			for( iFile.get( c ); c != '\n'; iFile.get( c ) )
				;
			iFile.get( c );
		}

	buf[ 0 ] = c;
	iFile.getline( buf + 1, BUF_SIZE - 1 );
	while( buf[ 0 ] != '#' )
		{
			char * delim = strchr( buf, ';' );
			*delim = '\0';
			char * name = strdup( buf );
			char * end; // Not const, since strtol want's it that way.

			UnicodeType code;
			try
				{
					code = glyphlist_strtol( delim + 1, & end, converterUTF16BEToUCS4 );
					if( code == 0 )
						{
							// This is a glyph name that is mapped to a sequence of characters.
							// It is not what we want.
							iFile.getline( buf, BUF_SIZE );
							continue;
						}
				}
			catch( const char * ball )
				{
					std::cerr << "When dealing with \"" << name << "\", the following error (ignored) occurred: "
										<< ball << std::endl ;
					iFile.getline( buf, BUF_SIZE );
					continue;
				}
			catch( const Shapes::Exceptions::Exception & ball )
				{
					throw;
				}
			catch( ... )
				{
					std::cerr << "Failed to catch ball. name: " << name << std::endl ;
					exit( 1 );
				}

			if( code != 0 && *end != '\0' && *end != '\r' )
				{
					std::ostringstream oss;
					oss << "An error in the glyphlist file was found near the character \"" << name << "\"." ;
					throw oss.str( );
				}

//			 if( code != 0 && code < 50 )
//				 {
//					 std::cerr << "Found small character: " << code << " --> " << name << std::endl ;
//				 }

			nameMem_.push_back( name );
			if( code < TABLE_SIZE )
				{
					if( namePtrs_[ code ] != 0 )
						{
							if( strncmp( namePtrs_[ code ], "afii", 4 ) == 0 ||
									strlen( namePtrs_[ code ] ) < strlen( name ) )
								{
									// Names starting with "afii", or that are shorter than their alternatives are overridden.
									namePtrs_[ code ] = name;
								}
//							 else
//								 {
//									 std::cerr << "Discarding name with code " << code
//														 << ", kept name: " << namePtrs_[ code ]
//														 << ", new name (discarded): " << name << std::endl ;
//								 }
						}
					else
						{
							namePtrs_[ code ] = name;
						}
				}
			else
				{
					typedef typeof namePtrsWide_ MapType;
					MapType::iterator i = namePtrsWide_.find( code );
					if( i != namePtrsWide_.end( ) )
						{
							if( strncmp( i->second, "afii", 4 ) == 0 ||
									strlen( i->second ) < strlen( name ) )
								{
									// Names starting with "afii", or that are shorter than their alternatives are overridden.
									namePtrsWide_.insert( i, MapType::value_type( code, name ) );
								}
//							 else
//								 {
//									 std::cerr << "Discarding name with code " << code
//														 << ", kept name: " << i->second
//														 << ", new name (discarded): " << name << std::endl ;
//								 }
						}
					else
						{
							namePtrsWide_[ code ] = name;
						}
				}
			nameMap_[ name ] = code;
			iFile.getline( buf, BUF_SIZE );
		}
}

size_t
GlyphList::size( ) const
{
	return nameMem_.size( );
}
