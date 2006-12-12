#include "classtreemacros.h"
#include "metapdfvalue.h"
#include "metapdftypes.h"

using namespace MetaPDF;

#define GETTYPEID_MAKER( T ) \
Kernel::QuickTypeID \
Lang::T::getTypeID( ) const \
{\
  return Kernel::TYPEID_ ## T; \
}

SINGLELOOP1( CLASSTREE1_ROOT, GETTYPEID_MAKER )

