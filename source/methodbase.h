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

#ifndef methodbase_h
#define methodbase_h


#include "functiontypes.h"

namespace Shapes
{
	namespace Kernel
	{
		class MethodBaseBase : public Lang::Function
		{
		private:
			RefCountPtr< const char > titleMem_;
		protected:
			const char * getTitlePtr( ) const { return titleMem_.getPtr( ); }
		public:
			MethodBaseBase( const RefCountPtr< const char > & title, bool forceAll )
				: Lang::Function( new Kernel::EvaluatedFormals( title.getPtr( ), forceAll ) ),
					titleMem_( title )
			{	}
			MethodBaseBase( const RefCountPtr< const char > & title )
				: Lang::Function( new Kernel::EvaluatedFormals( title.getPtr( ) ) ),
					titleMem_( title )
			{	}
			virtual ~MethodBaseBase( )
			{ }
		};
	}

	namespace Lang
	{

		template< class T >
		class MethodBase : public Kernel::MethodBaseBase
		{
		public:
			typedef T class_type;
		protected:
			RefCountPtr< const T > self_;
			const char * title_;
			bool transforming_;
		public:
			MethodBase( RefCountPtr< const T > self, const char * field, bool transforming );
			MethodBase( RefCountPtr< const T > self, const char * field, bool transforming, bool forceAll );
			virtual ~MethodBase( );
			virtual void gcMark( Kernel::GCMarkedSet & marked );
			virtual bool isTransforming( ) const;
		};

	}
}

#include "methodbase_impl.h"


#endif
