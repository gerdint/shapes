#include "classtreemacros.h"
#include "shapesvalue.h"
#include "shapestypes.h"

using namespace Shapes;

#define GETTYPEID_MAKER( T ) \
Kernel::QuickTypeID \
Lang::T::getTypeID( ) const \
{\
	return Kernel::TYPEID_ ## T; \
}

SINGLELOOP1( CLASSTREE1_ROOT, GETTYPEID_MAKER )

