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

#include "shapesvalue.h"
#include "shapesexceptions.h"
#include "consts.h"
#include "classtreemacros.h"

#if DISPATCHSTYLE == DISPATCHSTYLE_VOID
#include "globals.h"
#elif DISPATCHSTYLE == DISPATCHSTYLE_CASE
#include "astexpr.h"
#endif

using namespace Shapes;


Lang::Value::Value( )
	: node_( 0 )
{ }

Lang::Value::~Value( )
{ }

DISPATCHBASEIMPL

Kernel::VariableHandle
Lang::Value::getField( const char * fieldID, const RefCountPtr< const Lang::Value > & selfRef ) const
{
	throw Exceptions::ElementaryWithout( Exceptions::ElementaryWithout::VALUE, Exceptions::ElementaryWithout::FIELD, getTypeName( ) );
}

void
Lang::Value::show( std::ostream & os ) const
{
	os << "(unable to display)" ;
}

/*
RefCountPtr< const Lang::Value >
Lang::Value::transformed( const Shapes::Transform & transform, Kernel::ValueRef self ) const
{
	throw Exceptions::CoreTypeMismatch( strrefdup( "<transform>" ), 1, this->getTypeName( ), Shapes::SEVERAL_TYPES );
}

RefCountPtr< const Lang::Value >
Lang::Value::transformed( const Lang::Transform3D & transform, Kernel::ValueRef self ) const
{
	throw Exceptions::CoreTypeMismatch( strrefdup( "<transform3D>" ), 1, this->getTypeName( ), Shapes::SEVERAL_TYPES );
}
*/

RefCountPtr< const char >
Lang::Value::getTypeName( ) const
{
	return this->getClass( )->getPrettyName( );
}


DISPATCHIMPL( NoOperatorOverloadValue );

DISPATCHIMPL( NoOperatorOverloadGeometric2D );


//RefCountPtr< const Lang::Class > Lang::NoOperatorOverloadValue::TypeID( new Lang::SystemFinalClass( strrefdup( "NonDispatching" ) ) );
//TYPEINFOIMPL( NoOperatorOverloadValue );

RefCountPtr< const Lang::Class > Lang::Geometric2D::TypeID = NullPtr< const Lang::Class >( );	/* The value is set in main */
TYPEINFOIMPL( Geometric2D );

RefCountPtr< const Lang::Class > Lang::Geometric3D::TypeID = NullPtr< const Lang::Class >( );	/* The value is set in main */
TYPEINFOIMPL( Geometric3D );


Kernel::VariableHandle
Helpers::newValHandle( const Lang::Value * val )
{
	return Kernel::VariableHandle( new Kernel::Variable( RefCountPtr< const Lang::Value >( val ) ) );
}

