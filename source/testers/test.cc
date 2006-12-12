#include <iostream>
//#include <cstdlib>
#include <stdlib.h>
#include "iconvselect.h"
#include <string>

int
main( int argc, char ** argv )
{
  {
    std::cout << "Enter input encoding: " ;
    std::string inputenc;
    std::cin >> inputenc ;

    std::cout << "Enter output encoding: " ;
    std::string outputenc;
    std::cin >> outputenc ;

    iconv_t cd = libiconv_open( const_cast< char * >( outputenc.c_str( ) ),
			     const_cast< char * >( inputenc.c_str( ) ) );
  if( cd == (iconv_t)( -1 ) )
    {
      std::cout << "Couldn't create converter." << std::endl ;
    }
  else
    {
      std::cout << "A converter is available." << std::endl ;
    }
  }

  char OUTENCODING[] = "MacRoman";
  iconv_t cd = iconv_open( OUTENCODING, "UTF-8" );
  if( cd == (iconv_t)( -1 ) )
    {
      std::cerr << "Failed to create converter." << std::endl ;
      exit( 1 );
    }

  ///  char data [] = "ABCÅÄÖåäö";
  char data [] = "ABC";
  
  char * buf = new char[ strlen( data ) ];

  const char * inbuf = & data[0];
  size_t inbytesleft = strlen( data );
  char * outbuf = buf;
  size_t outbytesleft = strlen( data );
  size_t count = iconv( cd,
			& inbuf, & inbytesleft,
			& outbuf, & outbytesleft );
  if( count == (size_t)( -1 ) )
    {
      std::cerr << "An error occured." << std::endl ;
      exit( 1 );
    }

  std::cout << "Output character codes (" << OUTENCODING << "):" << std::endl << "  " ;
  for( const char * src = buf; src != outbuf; ++src )
    {
      std::cout << static_cast< int >( *reinterpret_cast< const unsigned char * >( src ) ) << "  " ;
    }
  std::cout << std::endl ;
}
