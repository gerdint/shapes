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

#include "ast.h"
#include "shapesexceptions.h"

template< class T >
Kernel::Thunk< T >::Thunk< T >( Ast::Expression * _expr, const Kernel::GraphicsState * _metaState, Kernel::PassedEnv _env )
	: expr( _expr ), metaState( *_metaState ), env( _env ), forced( false ), val( NullPtr< const T >( ) )
{ }

template< class T >
Kernel::Thunk< T >::Thunk< T >( const RefCountPtr< const T > & _val )
	: forced( true ), val( _val )
{ }

template< class T >
Kernel::Thunk< T >::~Thunk< T >( )
{ }

template< class T >
RefCountPtr< const T >
Kernel::Thunk< T >::getVal( ) const
{
	if( ! forced )
		{
			RefCountPtr< const Lang::Value > untypedVal = expr->value( & metaState, env );
			val = untypedVal.down_cast< const T >( );
			if( val == NullPtr< const T >( ) )
				{
					throw Exceptions::TypeMismatch( expr, untypedVal->getTypeName( ), T::staticTypeName( ) );
				}
			forced = true;
		}
	return val;
}
