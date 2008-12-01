/* This file is part of Shapes.
 *
 * Shapes is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * any later version.
 *
 * Shapes is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with Shapes.  If not, see <http://www.gnu.org/licenses/>.
 *
 * Copyright 2008 Henrik Tidefelt
 */

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
