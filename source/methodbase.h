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
		protected:
			const char * title_;
		public:
			MethodBaseBase( const char * title, bool forceAll )
				: Lang::Function( new Kernel::EvaluatedFormals( title, forceAll ) ),
					title_( title )
			{	}
			MethodBaseBase( const char * title )
				: Lang::Function( new Kernel::EvaluatedFormals( title ) ),
					title_( title )
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
			bool transforming_;
		public:
			MethodBase( RefCountPtr< const T > self, const char * title, bool transforming );
			MethodBase( RefCountPtr< const T > self, const char * title, bool transforming, bool forceAll );
			virtual ~MethodBase( );
			virtual void gcMark( Kernel::GCMarkedSet & marked );
			virtual bool isTransforming( ) const;
		};

	}

	namespace Kernel
	{

		class MethodFactoryBase
		{
		protected:
			const char * field_;
		public:
			MethodFactoryBase( const char * field )
				: field_( field )
			{ }
			virtual ~MethodFactoryBase( )
			{ }
			const char * field( ) const { return field_; }
			virtual Kernel::VariableHandle build( const RefCountPtr< const Lang::Value > self ) const = 0;
		};

		template< class ValueType, class Method >
		class MethodFactory : public MethodFactoryBase
		{
			RefCountPtr< const char > fullMethodID_;
		public:
			MethodFactory( )
				: MethodFactoryBase( Method::staticFieldID( ) ),
					fullMethodID_( Kernel::MethodId( ValueType::TypeID, Method::staticFieldID( ) ).prettyName( ) )
			{ }
			virtual ~MethodFactory( )
			{ }
			virtual Kernel::VariableHandle build( const RefCountPtr< const Lang::Value > self ) const
			{
				return Helpers::newValHandle( new Method( Helpers::down_cast_internal< const ValueType >( self ), fullMethodID_.getPtr( ) ) );
			}
		};

	}
}

#include "methodbase_impl.h"


#endif
