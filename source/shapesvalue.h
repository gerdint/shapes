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

#ifndef shapesvalue_h
#define shapesvalue_h

#include "Shapes_Lang_decls.h"
#include "Shapes_Kernel_decls.h"

#include "refcount.h"
#include "classtreemacros.h"
#include "shapesexceptions.h"

#include <iostream>
#include <set>
#include <limits.h>

namespace Shapes
{

#define MAKE_TYPEID( T ) TYPEID_ ## T ,
	namespace Kernel
	{
		enum QuickTypeID
		{ SINGLELOOP1( CLASSTREE1_ROOT, MAKE_TYPEID )
			NUMBER_OF_TYPEID };
	}

#define TYPEINFODECL																												\
	static RefCountPtr< const ::Shapes::Lang::Class > TypeID;								\
	virtual const RefCountPtr< const ::Shapes::Lang::Class > & getClass( ) const; \
	static RefCountPtr< const char > staticTypeName( );


#define TYPEINFOIMPL( T )												\
	const RefCountPtr< const ::Shapes::Lang::Class > &				\
		Lang::T::getClass( ) const												\
	{																								\
		return TypeID;																\
	}																								\
	RefCountPtr< const char >												\
		Lang::T::staticTypeName( )								\
		{																								\
			return TypeID->getPrettyName( );								\
		}

	namespace Lang
	{

		class Value
		{
			mutable const Ast::Node * node_;
		public:
			Value( );
			virtual ~Value( );
			virtual const RefCountPtr< const ::Shapes::Lang::Class > & getClass( ) const = 0;
			virtual Kernel::QuickTypeID getTypeID( ) const = 0;

			virtual void gcMark( Kernel::GCMarkedSet & marked ) = 0;

			virtual Kernel::VariableHandle getField( const char * fieldID, const RefCountPtr< const Lang::Value > & selfRef ) const;

			virtual void show( std::ostream & os ) const;

			RefCountPtr< const char > getTypeName( ) const;

			inline void set_node( const Ast::Node * node ) const { node_ = node; }
			inline const Ast::Node * node( ) const { return node_; }

			DISPATCHBASEDECL;
		};

	class NoOperatorOverloadValue : public Lang::Value
			{
			public:
				NoOperatorOverloadValue( ){ }
				virtual ~NoOperatorOverloadValue( ){ }
				DISPATCHDECL;
				//		TYPEINFODECL;
			};

	class Geometric2D : public Lang::Value
			{
			public:
				Geometric2D( ){ }
				virtual ~Geometric2D( ){ }
				virtual RefCountPtr< const Lang::Geometric2D > transformed( const Lang::Transform2D & transform, const RefCountPtr< const Lang::Geometric2D > & self ) const = 0;
				virtual RefCountPtr< const Lang::Geometric3D > to3D( const RefCountPtr< const Lang::Geometric2D > & self ) const = 0;
				TYPEINFODECL;
			};

	class NoOperatorOverloadGeometric2D : public Lang::Geometric2D
			{
			public:
				NoOperatorOverloadGeometric2D( ){ }
				virtual ~NoOperatorOverloadGeometric2D( ){ }
				DISPATCHDECL;
				//		TYPEINFODECL;
			};

	class Geometric3D : public Lang::Value
			{
			public:
				Geometric3D( ){ }
				virtual ~Geometric3D( ){ }
				virtual RefCountPtr< const Lang::Geometric3D > transformed( const Lang::Transform3D & transform, const RefCountPtr< const Lang::Geometric3D > & self ) const = 0;
				virtual RefCountPtr< const Lang::Geometric2D > to2D( const Kernel::PassedDyn & dyn, const RefCountPtr< const Lang::Geometric3D > & self ) const = 0;
				TYPEINFODECL;
			};

	}

	namespace Helpers
	{

		template< class T >
		RefCountPtr< T >
		down_cast( const RefCountPtr< const Lang::Value > & val, const Ast::SourceLocation & loc )
		{
			RefCountPtr< T > res = val.down_cast< T >( );
			if( res == NullPtr< T >( ) )
				{
					throw Exceptions::TypeMismatch( loc, val->getTypeName( ), T::staticTypeName( ) );
				}
			return res;
		}

		template< class T >
		RefCountPtr< T >
		down_cast_internal( const RefCountPtr< const Lang::Value > & val )
			{
				RefCountPtr< T > res = val.down_cast< T >( );
				if( res == NullPtr< T >( ) )
					{
						throw Exceptions::InternalError( "down_cast_internal: type mismatch." );
					}
				return res;
			}

		Kernel::VariableHandle newValHandle( const Lang::Value * val );

	}
}

#endif
