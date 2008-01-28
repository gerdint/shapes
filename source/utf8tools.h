#ifndef utf8tools_h
#define utf8tools_h


namespace Shapes
{
	namespace Helpers
	{
		inline bool utf8leadByte( char c )
		{
			return ! ( 0x80 <= *reinterpret_cast< unsigned char * >( & c ) && *reinterpret_cast< unsigned char * >( & c ) <= 0xBF );
		}
		inline bool utf8leadByte( unsigned char c )
		{
			return ! ( 0x80 <= c && c <= 0xBF );
		}
	}
}


#endif
