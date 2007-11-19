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
