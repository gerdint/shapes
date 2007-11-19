#ifndef Shapes_Helpers_decls
#define Shapes_Helpers_decls

#include "refcount.h"

#include "Shapes_Lang_decls.h"
#include "Shapes_Kernel_decls.h"
#include "Shapes_Ast_decls.h"
#include "Shapes_Concrete_decls.h"

#include <list>

namespace Shapes
{
	namespace Helpers
	{
		RefCountPtr< const Lang::ElementaryPath2D >
		elementaryPathCast2D( const char * title, Kernel::Arguments & args, size_t argNo, const Ast::SourceLocation & callLoc );
		RefCountPtr< const Lang::ElementaryPath2D >
		elementaryPathCast2D( const RefCountPtr< const Lang::Value > & arg, const Kernel::Continuation * loc );
		RefCountPtr< const Lang::ElementaryPath2D >
		elementaryPathTry2D( const RefCountPtr< const Lang::Value > & arg );
		Concrete::SplineTime
		pathTimeCast( const char * title, const RefCountPtr< const Lang::ElementaryPath2D > & pRef, Kernel::Arguments & args, size_t argNo, const Ast::SourceLocation & callLoc );
		Concrete::SplineTime
		pathTimeCast( const Lang::ElementaryPath2D * p, const Lang::Value * tPtr, const Kernel::Continuation * loc );

		RefCountPtr< const Lang::ElementaryPath3D >
		elementaryPathCast3D( const char * title, Kernel::Arguments & args, size_t argNo, const Ast::SourceLocation & callLoc );
		RefCountPtr< const Lang::ElementaryPath3D >
		elementaryPathCast3D( const RefCountPtr< const Lang::Value > & arg, const Kernel::Continuation * loc );
		RefCountPtr< const Lang::ElementaryPath3D >
		elementaryPathTry3D( const RefCountPtr< const Lang::Value > & arg );
		Concrete::SplineTime
		pathTimeCast( const char * title, const RefCountPtr< const Lang::ElementaryPath3D > & pRef, Kernel::Arguments & args, size_t argNo, const Ast::SourceLocation & callLoc );
		Concrete::SplineTime
		pathTimeCast( const Lang::ElementaryPath3D * p, const Lang::Value * tPtr, const Kernel::Continuation * loc );

		RefCountPtr< const char > typeSetString( RefCountPtr< const char > type1, RefCountPtr< const char > type2 );
		RefCountPtr< const char > typeSetString( RefCountPtr< const char > type1, RefCountPtr< const char > type2, RefCountPtr< const char > type3 );
		RefCountPtr< const char > typeSetString( RefCountPtr< const char > type1, RefCountPtr< const char > type2, RefCountPtr< const char > type3, RefCountPtr< const char > type4 );
		RefCountPtr< const char > typeSetString( RefCountPtr< const char > type1, RefCountPtr< const char > type2, RefCountPtr< const char > type3, RefCountPtr< const char > type4, RefCountPtr< const char > type5 );
		RefCountPtr< const char > typeSetString( const std::list< RefCountPtr< const char > > types );
	}


}


#endif
