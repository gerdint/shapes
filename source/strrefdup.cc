#include "strrefdup.h"

RefCountPtr< const char >
strrefdup( const char * str )
{
  return RefCountPtr< const char >( strdup( str ) );
}

RefCountPtr< const char >
strrefdup( const std::string & str )
{
  return RefCountPtr< const char >( strdup( str.c_str( ) ) );
}

RefCountPtr< const char >
strrefdup( const std::ostringstream & str )
{
  return RefCountPtr< const char >( strdup( str.str( ).c_str( ) ) );
}

