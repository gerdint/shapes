#ifndef strrefdup_h
#define strrefdup_h


#include "refcount.h"

#include <string>
#include <sstream>


RefCountPtr< const char > strrefdup( const char * str );
RefCountPtr< const char > strrefdup( const std::string & str );
RefCountPtr< const char > strrefdup( const std::ostringstream & str );


#endif
