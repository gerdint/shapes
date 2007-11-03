#ifndef methodid_h
#define methodid_h

#include "Shapes_Lang_decls.h"

#include "refcount.h"

namespace Shapes
{
  namespace Kernel
  {

    class MethodId
    {
      RefCountPtr< const Lang::Class > myClass;
      const char * msg;
    public:
      MethodId( const RefCountPtr< const Lang::Class > & _myClass, const char * _msg );
      ~MethodId( );
      RefCountPtr< const char > prettyName( ) const;
      const RefCountPtr< const Lang::Class > & getClass( ) const { return myClass; }
      const char * getIdentifier( ) const;
      friend bool operator < ( const MethodId & mid1, const MethodId & mid2 );
    };

  }
}

#endif
