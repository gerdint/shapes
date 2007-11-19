#ifndef charptrless_h
#define charptrless_h

#include "refcount.h"

#include <string.h>

class charPtrLess
{
 public:
	bool operator () ( const char * s1, const char * s2 ) const
	{
		return strcmp( s1, s2 ) < 0;
	}
};

class charRefPtrLess
{
 public:
	bool operator () ( const RefCountPtr< const char > & s1, const RefCountPtr< const char > & s2 ) const
	{
		return strcmp( s1.getPtr( ), s2.getPtr( ) ) < 0;
	}
};



#endif
