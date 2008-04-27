#include <cmath>

#include "astexprs.h"
#include "shapesexceptions.h"
#include "lighttypes.h"
#include "globals.h"

using namespace Shapes;
using namespace std;


RefCountPtr< const Lang::Value >
Ast::PlusPlusExpr::impl( DUMMYANDREF( const Lang::Float ) arg1, DUMMYANDREF( const Lang::Float ) arg2, const Kernel::PassedDyn & dyn ) const
{
	if( arg1->val_ < 0 )
		{
			throw Exceptions::OutOfRange( expr1_->loc( ), strrefdup( "Arguments to Pythagorean addition must be positive." ) );
		}
	if( arg2->val_ < 0 )
		{
			throw Exceptions::OutOfRange( expr2_->loc( ), strrefdup( "Arguments to Pythagorean addition must be positive." ) );
		}
	return RefCountPtr< const Lang::Value >( new Lang::Float( sqrt( (arg1->val_)*(arg1->val_) + (arg2->val_)*(arg2->val_) ) ) );
}

RefCountPtr< const Lang::Value >
Ast::PlusPlusExpr::impl( DUMMYANDREF( const Lang::Length ) arg1, DUMMYANDREF( const Lang::Length ) arg2, const Kernel::PassedDyn & dyn ) const
{
	Concrete::Length tmp1 = arg1->get( );
	Concrete::Length tmp2 = arg2->get( );
	if( tmp1 < 0 )
		{
			throw Exceptions::OutOfRange( expr1_->loc( ), strrefdup( "Arguments to Pythagorean addition must be positive." ) );
		}
	if( tmp2 < 0 )
		{
			throw Exceptions::OutOfRange( expr2_->loc( ), strrefdup( "Arguments to Pythagorean addition must be positive." ) );
		}
	return RefCountPtr< const Lang::Value >( new Lang::Length( hypotPhysical( tmp1, tmp2 ) ) );
}


RefCountPtr< const Lang::Value >
Ast::MinusMinusExpr::impl( DUMMYANDREF( const Lang::Float ) arg1, DUMMYANDREF( const Lang::Float ) arg2, const Kernel::PassedDyn & dyn ) const
{
	if( arg1->val_ < 0 )
		{
			throw Exceptions::OutOfRange( expr1_->loc( ), strrefdup( "Arguments to Pythagorean subtraction must be positive." ) );
		}
	if( arg2->val_ < 0 )
		{
			throw Exceptions::OutOfRange( expr2_->loc( ), strrefdup( "Arguments to Pythagorean subtraction must be positive." ) );
		}
	if( arg1->val_ < arg2->val_ )
		{
			throw Exceptions::OutOfRange( this->loc( ), strrefdup( "The first argument bust be greater than the second one in Pythagorean subtraction." ) );
		}
	return RefCountPtr< const Lang::Value >( new Lang::Float( sqrt( (arg1->val_)*(arg1->val_) - (arg2->val_)*(arg2->val_) ) ) );
}

RefCountPtr< const Lang::Value >
Ast::MinusMinusExpr::impl( DUMMYANDREF( const Lang::Length ) arg1, DUMMYANDREF( const Lang::Length ) arg2, const Kernel::PassedDyn & dyn ) const
{
	Concrete::Length tmp1 = arg1->get( );
	Concrete::Length tmp2 = arg2->get( );
	if( tmp1 < 0 )
		{
			throw Exceptions::OutOfRange( expr1_->loc( ), strrefdup( "Arguments to Pythagorean subtraction must be positive." ) );
		}
	if( tmp2 < 0 )
		{
			throw Exceptions::OutOfRange( expr2_->loc( ), strrefdup( "Arguments to Pythagorean subtraction must be positive." ) );
		}
	if( tmp1 < tmp2 )
		{
			throw Exceptions::OutOfRange( this->loc( ), strrefdup( "The first argument bust be greater than the second one in Pythagorean subtraction." ) );
		}
	return RefCountPtr< const Lang::Value >( new Lang::Length( sqrtPhysical( tmp1*tmp1 - tmp2*tmp2 ) ) );
}

RefCountPtr< const Lang::Value >
Ast::MinusMinusExpr::impl( DUMMYANDREF( const Lang::Coords2D ) arg1, DUMMYANDREF( const Lang::Coords2D ) arg2, const Kernel::PassedDyn & dyn ) const
{
	return Kernel::ValueRef( new Lang::Connection2D( arg1, arg2 ) );
}

RefCountPtr< const Lang::Value >
Ast::MinusMinusExpr::impl( DUMMYANDREF( const Lang::Coords2D ) arg1, DUMMYANDREF( const Lang::PathPoint2D ) arg2, const Kernel::PassedDyn & dyn ) const
{
	return Kernel::ValueRef( new Lang::Connection2D( arg1, arg2 ) );
}

RefCountPtr< const Lang::Value >
Ast::MinusMinusExpr::impl( DUMMYANDREF( const Lang::PathPoint2D ) arg1, DUMMYANDREF( const Lang::Coords2D ) arg2, const Kernel::PassedDyn & dyn ) const
{
	return Kernel::ValueRef( new Lang::Connection2D( arg1, arg2 ) );
}

RefCountPtr< const Lang::Value >
Ast::MinusMinusExpr::impl( DUMMYANDREF( const Lang::Coords2D ) arg1, DUMMYANDREF( const Lang::Path2D ) arg2, const Kernel::PassedDyn & dyn ) const
{
	if( arg2->isClosed( ) )
		{
			throw Exceptions::OutOfRange( this->loc( ), strrefdup( "Closed subpaths cannot be connected with." ) );
		}
	return Kernel::ValueRef( new Lang::Connection2D( arg1, arg2 ) );
}

RefCountPtr< const Lang::Value >
Ast::MinusMinusExpr::impl( DUMMYANDREF( const Lang::Path2D ) arg1, DUMMYANDREF( const Lang::Coords2D ) arg2, const Kernel::PassedDyn & dyn ) const
{
	if( arg1->isClosed( ) )
		{
			throw Exceptions::OutOfRange( this->loc( ), strrefdup( "Closed subpaths cannot be connected with." ) );
		}
	return Kernel::ValueRef( new Lang::Connection2D( arg1, arg2 ) );
}

RefCountPtr< const Lang::Value >
Ast::MinusMinusExpr::impl( DUMMYANDREF( const Lang::PathPoint2D ) arg1, DUMMYANDREF( const Lang::PathPoint2D ) arg2, const Kernel::PassedDyn & dyn ) const
{
	return Kernel::ValueRef( new Lang::Connection2D( arg1, arg2 ) );
}

RefCountPtr< const Lang::Value >
Ast::MinusMinusExpr::impl( DUMMYANDREF( const Lang::PathPoint2D ) arg1, DUMMYANDREF( const Lang::Path2D ) arg2, const Kernel::PassedDyn & dyn ) const
{
	if( arg2->isClosed( ) )
		{
			throw Exceptions::OutOfRange( this->loc( ), strrefdup( "Closed subpaths cannot be connected with." ) );
		}
	return Kernel::ValueRef( new Lang::Connection2D( arg1, arg2 ) );
}

RefCountPtr< const Lang::Value >
Ast::MinusMinusExpr::impl( DUMMYANDREF( const Lang::Path2D ) arg1, DUMMYANDREF( const Lang::PathPoint2D ) arg2, const Kernel::PassedDyn & dyn ) const
{
	if( arg1->isClosed( ) )
		{
			throw Exceptions::OutOfRange( this->loc( ), strrefdup( "Closed subpaths cannot be connected with." ) );
		}
	return Kernel::ValueRef( new Lang::Connection2D( arg1, arg2 ) );
}

RefCountPtr< const Lang::Value >
Ast::MinusMinusExpr::impl( DUMMYANDREF( const Lang::Path2D ) arg1, DUMMYANDREF( const Lang::Path2D ) arg2, const Kernel::PassedDyn & dyn ) const
{
	if( arg1->isClosed( ) )
		{
			throw Exceptions::OutOfRange( this->loc( ), strrefdup( "Closed subpaths cannot be connected with." ) );
		}
	if( arg2->isClosed( ) )
		{
			throw Exceptions::OutOfRange( this->loc( ), strrefdup( "Closed subpaths cannot be connected with." ) );
		}
	return Kernel::ValueRef( new Lang::Connection2D( arg1, arg2 ) );
}

RefCountPtr< const Lang::Value >
Ast::MinusMinusExpr::impl( DUMMYANDREF( const Lang::Coords3D ) arg1, DUMMYANDREF( const Lang::Coords3D ) arg2, const Kernel::PassedDyn & dyn ) const
{
	return Kernel::ValueRef( new Lang::Connection3D( RefCountPtr< Lang::Path3D >( new Lang::SinglePointPath3D( arg1 ) ),
																											 RefCountPtr< Lang::Path3D >( new Lang::SinglePointPath3D( arg2 ) ) ) );
}

RefCountPtr< const Lang::Value >
Ast::MinusMinusExpr::impl( DUMMYANDREF( const Lang::Coords3D ) arg1, DUMMYANDREF( const Lang::PathPoint3D ) arg2, const Kernel::PassedDyn & dyn ) const
{
	return Kernel::ValueRef( new Lang::Connection3D( RefCountPtr< Lang::Path3D >( new Lang::SinglePointPath3D( arg1 ) ),
																											 RefCountPtr< Lang::Path3D >( new Lang::SinglePointPath3D( arg2 ) ) ) );
}

RefCountPtr< const Lang::Value >
Ast::MinusMinusExpr::impl( DUMMYANDREF( const Lang::PathPoint3D ) arg1, DUMMYANDREF( const Lang::Coords3D ) arg2, const Kernel::PassedDyn & dyn ) const
{
	return Kernel::ValueRef( new Lang::Connection3D( RefCountPtr< Lang::Path3D >( new Lang::SinglePointPath3D( arg1 ) ),
																											 RefCountPtr< Lang::Path3D >( new Lang::SinglePointPath3D( arg2 ) ) ) );
}

RefCountPtr< const Lang::Value >
Ast::MinusMinusExpr::impl( DUMMYANDREF( const Lang::Coords3D ) arg1, DUMMYANDREF( const Lang::Path3D ) arg2, const Kernel::PassedDyn & dyn ) const
{
	if( arg2->isClosed( ) )
		{
			throw Exceptions::OutOfRange( this->loc( ), strrefdup( "Closed subpaths cannot be connected with." ) );
		}
	return Kernel::ValueRef( new Lang::Connection3D( RefCountPtr< Lang::Path3D >( new Lang::SinglePointPath3D( arg1 ) ), arg2 ) );
}

RefCountPtr< const Lang::Value >
Ast::MinusMinusExpr::impl( DUMMYANDREF( const Lang::Path3D ) arg1, DUMMYANDREF( const Lang::Coords3D ) arg2, const Kernel::PassedDyn & dyn ) const
{
	if( arg1->isClosed( ) )
		{
			throw Exceptions::OutOfRange( this->loc( ), strrefdup( "Closed subpaths cannot be connected with." ) );
		}
	return Kernel::ValueRef( new Lang::Connection3D( arg1, RefCountPtr< Lang::Path3D >( new Lang::SinglePointPath3D( arg2 ) ) ) );
}

RefCountPtr< const Lang::Value >
Ast::MinusMinusExpr::impl( DUMMYANDREF( const Lang::PathPoint3D ) arg1, DUMMYANDREF( const Lang::PathPoint3D ) arg2, const Kernel::PassedDyn & dyn ) const
{
	return Kernel::ValueRef( new Lang::Connection3D( RefCountPtr< Lang::Path3D >( new Lang::SinglePointPath3D( arg1 ) ),
																											 RefCountPtr< Lang::Path3D >( new Lang::SinglePointPath3D( arg2 ) ) ) );
}

RefCountPtr< const Lang::Value >
Ast::MinusMinusExpr::impl( DUMMYANDREF( const Lang::PathPoint3D ) arg1, DUMMYANDREF( const Lang::Path3D ) arg2, const Kernel::PassedDyn & dyn ) const
{
	if( arg2->isClosed( ) )
		{
			throw Exceptions::OutOfRange( this->loc( ), strrefdup( "Closed subpaths cannot be connected with." ) );
		}
	return Kernel::ValueRef( new Lang::Connection3D( RefCountPtr< Lang::Path3D >( new Lang::SinglePointPath3D( arg1 ) ), arg2 ) );
}

RefCountPtr< const Lang::Value >
Ast::MinusMinusExpr::impl( DUMMYANDREF( const Lang::Path3D ) arg1, DUMMYANDREF( const Lang::PathPoint3D ) arg2, const Kernel::PassedDyn & dyn ) const
{
	if( arg1->isClosed( ) )
		{
			throw Exceptions::OutOfRange( this->loc( ), strrefdup( "Closed subpaths cannot be connected with." ) );
		}
	return Kernel::ValueRef( new Lang::Connection3D( arg1, RefCountPtr< Lang::Path3D >( new Lang::SinglePointPath3D( arg2 ) ) ) );
}

RefCountPtr< const Lang::Value >
Ast::MinusMinusExpr::impl( DUMMYANDREF( const Lang::Path3D ) arg1, DUMMYANDREF( const Lang::Path3D ) arg2, const Kernel::PassedDyn & dyn ) const
{
	if( arg1->isClosed( ) )
		{
			throw Exceptions::OutOfRange( this->loc( ), strrefdup( "Closed subpaths cannot be connected with." ) );
		}
	if( arg2->isClosed( ) )
		{
			throw Exceptions::OutOfRange( this->loc( ), strrefdup( "Closed subpaths cannot be connected with." ) );
		}
	return Kernel::ValueRef( new Lang::Connection3D( arg1, arg2 ) );
}

RefCountPtr< const Lang::Value >
Ast::MinusMinusExpr::impl( DUMMYANDREF( const Lang::PathSlider2D ) arg1, DUMMYANDREF( const Lang::PathSlider2D ) arg2, const Kernel::PassedDyn & dyn ) const
{
	RefCountPtr< const Lang::ElementaryPath2D > path = arg1->getPath( );
	if( path != arg2->getPath( ) )
		{
			throw Exceptions::OutOfRange( this->loc( ), strrefdup( "Cannot connect sliders that belong to different paths." ) );
		}

	if( arg1->getFront( ) != NullPtr< const Lang::Value >( ) )
		{
			throw Exceptions::OutOfRange( expr1_->loc( ), strrefdup( "When connecting sliders, the rear slider cannot have a front handle." ) );
		}
	if( arg2->getRear( ) != NullPtr< const Lang::Value >( ) )
		{
			throw Exceptions::OutOfRange( expr2_->loc( ), strrefdup( "When connecting sliders, the front slider cannot have a rear handle." ) );
		}

	if( arg1->getRear( ) == NullPtr< const Lang::Value >( ) &&
			arg2->getFront( ) == NullPtr< const Lang::Value >( ) )
		{
			return path->subpath( arg1->getTime( ), arg2->getTime( ) );
		}

	return Kernel::ValueRef( new Lang::HeadedPath2D( arg1->getRear( ), path->subpath( arg1->getTime( ), arg2->getTime( ) ), arg2->getFront( ) ) );
}

RefCountPtr< const Lang::Value >
Ast::MinusMinusExpr::impl( DUMMYANDREF( const Lang::PathSlider3D ) arg1, DUMMYANDREF( const Lang::PathSlider3D ) arg2, const Kernel::PassedDyn & dyn ) const
{
	RefCountPtr< const Lang::ElementaryPath3D > path = arg1->getPath( );
	if( path != arg2->getPath( ) )
		{
			throw Exceptions::OutOfRange( this->loc( ), strrefdup( "Cannot connect sliders that belong to different paths." ) );
		}

	if( arg1->getFront( ) != NullPtr< const Lang::Value >( ) )
		{
			throw Exceptions::OutOfRange( expr1_->loc( ), strrefdup( "When connecting sliders, the rear slider cannot have a front handle." ) );
		}
	if( arg2->getRear( ) != NullPtr< const Lang::Value >( ) )
		{
			throw Exceptions::OutOfRange( expr2_->loc( ), strrefdup( "When connecting sliders, the front slider cannot have a rear handle." ) );
		}

	if( arg1->getRear( ) == NullPtr< const Lang::Value >( ) &&
			arg2->getFront( ) == NullPtr< const Lang::Value >( ) )
		{
			return path->subpath( arg1->getTime( ), arg2->getTime( ) );
		}

	return Kernel::ValueRef( new Lang::HeadedPath3D( arg1->getRear( ), path->subpath( arg1->getTime( ), arg2->getTime( ) ), arg2->getFront( ) ) );
}



RefCountPtr< const Lang::Value >
Ast::AmpersandExpr::impl( DUMMYANDREF( const Lang::Path2D ) arg1, DUMMYANDREF( const Lang::Path2D ) arg2, const Kernel::PassedDyn & dyn ) const
{
	Lang::MultiPath2D * res = new Lang::MultiPath2D;
	res->push_back( arg1 );
	res->push_back( arg2 );
	return RefCountPtr< Lang::Value >( res );
}

RefCountPtr< const Lang::Value >
Ast::AmpersandExpr::impl( DUMMYANDREF( const Lang::MultiPath2D ) arg1, DUMMYANDREF( const Lang::Path2D ) arg2, const Kernel::PassedDyn & dyn ) const
{
	Lang::MultiPath2D * res = arg1->clone( );
	res->push_back( arg2 );
	return RefCountPtr< Lang::Value >( res );
}

RefCountPtr< const Lang::Value >
Ast::AmpersandExpr::impl( DUMMYANDREF( const Lang::Path2D ) arg1, DUMMYANDREF( const Lang::MultiPath2D ) arg2, const Kernel::PassedDyn & dyn ) const
{
	Lang::MultiPath2D * res = arg2->clone( );
	res->push_front( arg1 );
	return RefCountPtr< Lang::Value >( res );
}

RefCountPtr< const Lang::Value >
Ast::AmpersandExpr::impl( DUMMYANDREF( const Lang::MultiPath2D ) arg1, DUMMYANDREF( const Lang::MultiPath2D ) arg2, const Kernel::PassedDyn & dyn ) const
{
	Lang::MultiPath2D * res = arg1->clone( );
	typedef typeof( *arg2 ) ListType;
	for( ListType::const_iterator i = arg2->begin( ); i != arg2->end( ); ++i )
		{
			res->push_back( *i );
		}
	return RefCountPtr< Lang::Value >( res );
}

RefCountPtr< const Lang::Value >
Ast::AmpersandExpr::impl( DUMMYANDREF( const Lang::Path3D ) arg1, DUMMYANDREF( const Lang::Path3D ) arg2, const Kernel::PassedDyn & dyn ) const
{
	Lang::MultiPath3D * res = new Lang::MultiPath3D;
	res->push_back( arg1 );
	res->push_back( arg2 );
	return RefCountPtr< Lang::Value >( res );
}

RefCountPtr< const Lang::Value >
Ast::AmpersandExpr::impl( DUMMYANDREF( const Lang::MultiPath3D ) arg1, DUMMYANDREF( const Lang::Path3D ) arg2, const Kernel::PassedDyn & dyn ) const
{
	Lang::MultiPath3D * res = arg1->clone( );
	res->push_back( arg2 );
	return RefCountPtr< Lang::Value >( res );
}

RefCountPtr< const Lang::Value >
Ast::AmpersandExpr::impl( DUMMYANDREF( const Lang::Path3D ) arg1, DUMMYANDREF( const Lang::MultiPath3D ) arg2, const Kernel::PassedDyn & dyn ) const
{
	Lang::MultiPath3D * res = arg2->clone( );
	res->push_front( arg1 );
	return RefCountPtr< Lang::Value >( res );
}

RefCountPtr< const Lang::Value >
Ast::AmpersandExpr::impl( DUMMYANDREF( const Lang::MultiPath3D ) arg1, DUMMYANDREF( const Lang::MultiPath3D ) arg2, const Kernel::PassedDyn & dyn ) const
{
	Lang::MultiPath3D * res = arg1->clone( );
	typedef typeof( *arg2 ) ListType;
	for( ListType::const_iterator i = arg2->begin( ); i != arg2->end( ); ++i )
		{
			res->push_back( *i );
		}
	return RefCountPtr< Lang::Value >( res );
}

RefCountPtr< const Lang::Value >
Ast::AmpersandExpr::impl( DUMMYANDREF( const Lang::DynamicBindings ) arg1, DUMMYANDREF( const Lang::DynamicBindings ) arg2, const Kernel::PassedDyn & dyn ) const
{
	if( arg1 == Lang::THE_NULL_DYNAMIC_BINDINGS )
		{
			return arg2;
		}
	if( arg2 == Lang::THE_NULL_DYNAMIC_BINDINGS )
		{
			return arg1;
		}
	return RefCountPtr< Lang::DynamicBindings >( new Lang::DynamicBindingsPair( arg1, arg2 ) );
}

RefCountPtr< const Lang::Value >
Ast::AmpersandExpr::impl( DUMMYANDREF( const Lang::Drawable2D ) arg1, DUMMYANDREF( const Lang::Drawable2D ) arg2, const Kernel::PassedDyn & dyn ) const
{
	RefCountPtr< const Kernel::GraphicsState > metaState = dyn->getGraphicsState( );
	return RefCountPtr< Lang::Group2D >( new Lang::GroupPair2D
																			 ( arg2,
																				 RefCountPtr< Lang::Group2D >( new Lang::GroupPair2D
																																			 ( arg1,
																																				 Lang::THE_NULL2D,
																																				 metaState ) ),
																				 metaState ) );
}

RefCountPtr< const Lang::Value >
Ast::AmpersandExpr::impl( DUMMYANDREF( const Lang::Group2D ) arg1, DUMMYANDREF( const Lang::Drawable2D ) arg2, const Kernel::PassedDyn & dyn ) const
{
	return RefCountPtr< Lang::Group2D >( new Lang::GroupPair2D( arg2, arg1, dyn->getGraphicsState( ) ) );
}

RefCountPtr< const Lang::Value >
Ast::AmpersandExpr::impl( DUMMYANDREF( const Lang::Drawable3D ) arg1, DUMMYANDREF( const Lang::Drawable3D ) arg2, const Kernel::PassedDyn & dyn ) const
{
	RefCountPtr< const Kernel::GraphicsState > metaState = dyn->getGraphicsState( );
	return RefCountPtr< Lang::Group3D >( new Lang::GroupPair3D
																			 ( arg2,
																				 RefCountPtr< Lang::Group3D >( new Lang::GroupPair3D
																																			 ( arg1,
																																				 Lang::THE_NULL3D,
																																				 metaState ) ),
																				 metaState ) );
}

RefCountPtr< const Lang::Value >
Ast::AmpersandExpr::impl( DUMMYANDREF( const Lang::Group3D ) arg1, DUMMYANDREF( const Lang::Drawable3D ) arg2, const Kernel::PassedDyn & dyn ) const
{
	return RefCountPtr< Lang::Group3D >( new Lang::GroupPair3D( arg2, arg1, dyn->getGraphicsState( ) ) );
}


RefCountPtr< const Lang::Value >
Ast::AmpersandMoreExpr::impl( DUMMYANDREF( const Lang::DynamicBindings ) arg1, DUMMYANDREF( const Lang::DynamicBindings ) arg2, const Kernel::PassedDyn & dyn ) const
{
	if( arg1 == Lang::THE_NULL_DYNAMIC_BINDINGS )
		{
			return arg2;
		}
	if( arg2 == Lang::THE_NULL_DYNAMIC_BINDINGS )
		{
			return arg1;
		}
	return RefCountPtr< Lang::DynamicBindings >( new Lang::DynamicBindingsPair( arg1, arg2, true ) ); /* True means "second overrides first" */
}


RefCountPtr< const Lang::Value >
Ast::PlusExpr::impl( DUMMYANDREF( const Lang::Float ) arg1, DUMMYANDREF( const Lang::Float ) arg2, const Kernel::PassedDyn & dyn ) const
{
	return RefCountPtr< const Lang::Value >( new Lang::Float( arg1->val_ + arg2->val_ ) );
}

RefCountPtr< const Lang::Value >
Ast::PlusExpr::impl( DUMMYANDREF( const Lang::Integer ) arg1, DUMMYANDREF( const Lang::Integer ) arg2, const Kernel::PassedDyn & dyn ) const
{
	return RefCountPtr< const Lang::Value >( new Lang::Integer( arg1->val_ + arg2->val_ ) );
}

RefCountPtr< const Lang::Value >
Ast::PlusExpr::impl( DUMMYANDREF( const Lang::Length ) arg1, DUMMYANDREF( const Lang::Length ) arg2, const Kernel::PassedDyn & dyn ) const
{
	return RefCountPtr< const Lang::Value >( new Lang::Length( *arg1 + *arg2 ) );
}

RefCountPtr< const Lang::Value >
Ast::PlusExpr::impl( DUMMYANDREF( const Lang::FloatPair ) arg1, DUMMYANDREF( const Lang::FloatPair ) arg2, const Kernel::PassedDyn & dyn ) const
{
	return RefCountPtr< const Lang::Value >( new Lang::FloatPair( arg1->x_ + arg2->x_, arg1->y_ + arg2->y_ ) );
}

RefCountPtr< const Lang::Value >
Ast::PlusExpr::impl( DUMMYANDREF( const Lang::Coords2D ) arg1, DUMMYANDREF( const Lang::Coords2D ) arg2, const Kernel::PassedDyn & dyn ) const
{
	return RefCountPtr< const Lang::Value >( new Lang::Coords2D( arg1->x_ + arg2->x_, arg1->y_ + arg2->y_ ) );
}

RefCountPtr< const Lang::Value >
Ast::PlusExpr::impl( DUMMYANDREF( const Lang::FloatTriple ) arg1, DUMMYANDREF( const Lang::FloatTriple ) arg2, const Kernel::PassedDyn & dyn ) const
{
	return RefCountPtr< const Lang::Value >( new Lang::FloatTriple( arg1->x_ + arg2->x_, arg1->y_ + arg2->y_, arg1->z_ + arg2->z_ ) );
}

RefCountPtr< const Lang::Value >
Ast::PlusExpr::impl( DUMMYANDREF( const Lang::Coords3D ) arg1, DUMMYANDREF( const Lang::Coords3D ) arg2, const Kernel::PassedDyn & dyn ) const
{
	return RefCountPtr< const Lang::Value >( new Lang::Coords3D( arg1->x_ + arg2->x_, arg1->y_ + arg2->y_, arg1->z_ + arg2->z_ ) );
}

RefCountPtr< const Lang::Value >
Ast::PlusExpr::impl( DUMMYANDREF( const Lang::String ) arg1, DUMMYANDREF( const Lang::String ) arg2, const Kernel::PassedDyn & dyn ) const
{
	const char * src1 = arg1->val_.getPtr( );
	const char * src2 = arg2->val_.getPtr( );
	int l1 = strlen( src1 );
	char * mem = new char[ l1 + strlen( src2 ) + 1 ];
	strcpy( mem, src1 );
	strcpy( mem + l1, src2 );
	return RefCountPtr< const Lang::Value >( new Lang::String( mem, false ) );
}

RefCountPtr< const Lang::Value >
Ast::PlusExpr::impl( DUMMYANDREF( const Lang::Dash ) arg1, DUMMYANDREF( const Lang::Length ) arg2, const Kernel::PassedDyn & dyn ) const
{
	return arg1->shifted( arg2->get( ) );
}

RefCountPtr< const Lang::Value >
Ast::PlusExpr::impl( DUMMYANDREF( const Lang::Length ) arg1, DUMMYANDREF( const Lang::Dash ) arg2, const Kernel::PassedDyn & dyn ) const
{
	return arg2->shifted( arg1->get( ) );
}

RefCountPtr< const Lang::Value >
Ast::PlusExpr::impl( DUMMYANDREF( const Lang::PathSlider2D ) arg1, DUMMYANDREF( const Lang::Float ) arg2, const Kernel::PassedDyn & dyn ) const
{
	return arg1->move_time( Concrete::Time( arg2->val_ ) );
}

RefCountPtr< const Lang::Value >
Ast::PlusExpr::impl( DUMMYANDREF( const Lang::PathSlider2D ) arg1, DUMMYANDREF( const Lang::Length ) arg2, const Kernel::PassedDyn & dyn ) const
{
	return arg1->move_length( arg2->get( ) );
}

RefCountPtr< const Lang::Value >
Ast::PlusExpr::impl( DUMMYANDREF( const Lang::PathSlider3D ) arg1, DUMMYANDREF( const Lang::Float ) arg2, const Kernel::PassedDyn & dyn ) const
{
	return arg1->move_time( Concrete::Time( arg2->val_ ) );
}

RefCountPtr< const Lang::Value >
Ast::PlusExpr::impl( DUMMYANDREF( const Lang::PathSlider3D ) arg1, DUMMYANDREF( const Lang::Length ) arg2, const Kernel::PassedDyn & dyn ) const
{
	return arg1->move_length( arg2->get( ) );
}

RefCountPtr< const Lang::Value >
Ast::PlusExpr::impl( DUMMYANDREF( const Lang::RGB ) arg1, DUMMYANDREF( const Lang::RGB ) arg2, const Kernel::PassedDyn & dyn ) const
{
	return RefCountPtr< const Lang::Value >( new Lang::RGB( arg1->components( ).add( arg2->components( ), loc( ) ) ) );
}

RefCountPtr< const Lang::Value >
Ast::PlusExpr::impl( DUMMYANDREF( const Lang::Gray ) arg1, DUMMYANDREF( const Lang::Gray ) arg2, const Kernel::PassedDyn & dyn ) const
{
	return RefCountPtr< const Lang::Value >( new Lang::Gray( arg1->components( ).add( arg2->components( ), loc( ) ) ) );
}

RefCountPtr< const Lang::Value >
Ast::PlusExpr::impl( DUMMYANDREF( const Lang::SpecularReflection ) arg1, DUMMYANDREF( const Lang::SpecularReflection ) arg2, const Kernel::PassedDyn & dyn ) const
{
	return RefCountPtr< const Lang::Value >( new Lang::SpecularReflectionPair( arg1, arg2 ) );
}


RefCountPtr< const Lang::Value >
Ast::MinusExpr::impl( DUMMYANDREF( const Lang::Float ) arg1, DUMMYANDREF( const Lang::Float ) arg2, const Kernel::PassedDyn & dyn ) const
{
	return RefCountPtr< const Lang::Value >( new Lang::Float( arg1->val_ - arg2->val_ ) );
}

RefCountPtr< const Lang::Value >
Ast::MinusExpr::impl( DUMMYANDREF( const Lang::Integer ) arg1, DUMMYANDREF( const Lang::Integer ) arg2, const Kernel::PassedDyn & dyn ) const
{
	return RefCountPtr< const Lang::Value >( new Lang::Integer( arg1->val_ - arg2->val_ ) );
}

RefCountPtr< const Lang::Value >
Ast::MinusExpr::impl( DUMMYANDREF( const Lang::Length ) arg1, DUMMYANDREF( const Lang::Length ) arg2, const Kernel::PassedDyn & dyn ) const
{
	return RefCountPtr< const Lang::Value >( new Lang::Length( *arg1 - *arg2 ) );
}

RefCountPtr< const Lang::Value >
Ast::MinusExpr::impl( DUMMYANDREF( const Lang::FloatPair ) arg1, DUMMYANDREF( const Lang::FloatPair ) arg2, const Kernel::PassedDyn & dyn ) const
{
	return RefCountPtr< const Lang::Value >( new Lang::FloatPair( arg1->x_ - arg2->x_, arg1->y_ - arg2->y_ ) );
}

RefCountPtr< const Lang::Value >
Ast::MinusExpr::impl( DUMMYANDREF( const Lang::Coords2D ) arg1, DUMMYANDREF( const Lang::Coords2D ) arg2, const Kernel::PassedDyn & dyn ) const
{
	return RefCountPtr< const Lang::Value >( new Lang::Coords2D( arg1->x_ - arg2->x_, arg1->y_ - arg2->y_ ) );
}

RefCountPtr< const Lang::Value >
Ast::MinusExpr::impl( DUMMYANDREF( const Lang::FloatTriple ) arg1, DUMMYANDREF( const Lang::FloatTriple ) arg2, const Kernel::PassedDyn & dyn ) const
{
	return RefCountPtr< const Lang::Value >( new Lang::FloatTriple( arg1->x_ - arg2->x_, arg1->y_ - arg2->y_, arg1->z_ - arg2->z_ ) );
}

RefCountPtr< const Lang::Value >
Ast::MinusExpr::impl( DUMMYANDREF( const Lang::Coords3D ) arg1, DUMMYANDREF( const Lang::Coords3D ) arg2, const Kernel::PassedDyn & dyn ) const
{
	return RefCountPtr< const Lang::Value >( new Lang::Coords3D( arg1->x_ - arg2->x_, arg1->y_ - arg2->y_, arg1->z_ - arg2->z_ ) );
}

RefCountPtr< const Lang::Value >
Ast::MinusExpr::impl( DUMMYANDREF( const Lang::PathSlider2D ) arg1, DUMMYANDREF( const Lang::Float ) arg2, const Kernel::PassedDyn & dyn ) const
{
	return arg1->move_time( Concrete::Time( - arg2->val_ ) );
}

RefCountPtr< const Lang::Value >
Ast::MinusExpr::impl( DUMMYANDREF( const Lang::PathSlider2D ) arg1, DUMMYANDREF( const Lang::Length ) arg2, const Kernel::PassedDyn & dyn ) const
{
	return arg1->move_length( - arg2->get( ) );
}

RefCountPtr< const Lang::Value >
Ast::MinusExpr::impl( DUMMYANDREF( const Lang::PathSlider3D ) arg1, DUMMYANDREF( const Lang::Float ) arg2, const Kernel::PassedDyn & dyn ) const
{
	return arg1->move_time( Concrete::Time( - arg2->val_ ) );
}

RefCountPtr< const Lang::Value >
Ast::MinusExpr::impl( DUMMYANDREF( const Lang::PathSlider3D ) arg1, DUMMYANDREF( const Lang::Length ) arg2, const Kernel::PassedDyn & dyn ) const
{
	return arg1->move_length( - arg2->get( ) );
}


RefCountPtr< const Lang::Value >
Ast::AngleExpr::impl( DUMMYANDREF( const Lang::Float ) arg1, DUMMYANDREF( const Lang::Float ) arg2, const Kernel::PassedDyn & dyn ) const
{
	double tmp = arg1->val_ * arg2->val_;
	if( tmp > 0 )
		{
			return RefCountPtr< const Lang::Value >( new Lang::Float( 0 ) );
		}
	if( tmp < 0 )
		{
			return RefCountPtr< const Lang::Value >( new Lang::Float( M_PI ) );
		}
	return RefCountPtr< const Lang::Value >( new Lang::Float( M_PI_2 ) );
}

RefCountPtr< const Lang::Value >
Ast::AngleExpr::impl( DUMMYANDREF( const Lang::Float ) arg1, DUMMYANDREF( const Lang::Length ) arg2, const Kernel::PassedDyn & dyn ) const
{
	Concrete::Length tmp = arg1->val_ * arg2->get( );
	if( tmp > 0 )
		{
			return RefCountPtr< const Lang::Value >( new Lang::Float( 0 ) );
		}
	if( tmp < 0 )
		{
			return RefCountPtr< const Lang::Value >( new Lang::Float( M_PI ) );
		}
	return RefCountPtr< const Lang::Value >( new Lang::Float( M_PI_2 ) );
}

RefCountPtr< const Lang::Value >
Ast::AngleExpr::impl( DUMMYANDREF( const Lang::Length ) arg1, DUMMYANDREF( const Lang::Float ) arg2, const Kernel::PassedDyn & dyn ) const
{
	Concrete::Length tmp = arg1->get( ) * arg2->val_;
	if( tmp > 0 )
		{
			return RefCountPtr< const Lang::Value >( new Lang::Float( 0 ) );
		}
	if( tmp < 0 )
		{
			return RefCountPtr< const Lang::Value >( new Lang::Float( M_PI ) );
		}
	return RefCountPtr< const Lang::Value >( new Lang::Float( M_PI_2 ) );
}

RefCountPtr< const Lang::Value >
Ast::AngleExpr::impl( DUMMYANDREF( const Lang::Length ) arg1, DUMMYANDREF( const Lang::Length ) arg2, const Kernel::PassedDyn & dyn ) const
{
	Physical< 2, 0 > tmp = arg1->get( ) * arg2->get( );
	if( tmp > 0 )
		{
			return RefCountPtr< const Lang::Value >( new Lang::Float( 0 ) );
		}
	if( tmp < 0 )
		{
			return RefCountPtr< const Lang::Value >( new Lang::Float( M_PI ) );
		}
	return RefCountPtr< const Lang::Value >( new Lang::Float( M_PI_2 ) );
}

RefCountPtr< const Lang::Value >
Ast::AngleExpr::impl( DUMMYANDREF( const Lang::FloatPair ) arg1, DUMMYANDREF( const Lang::FloatPair ) arg2, const Kernel::PassedDyn & dyn ) const
{
	const double x1 = arg1->x_;
	const double y1 = arg1->y_;
	const double x2 = arg2->x_;
	const double y2 = arg2->y_;
	double norm1 = hypot( x1, y1 );
	double norm2 = hypot( x2, y2 );
	if( norm1 == 0 || norm2 == 0 )
		{
			return RefCountPtr< const Lang::Value >( new Lang::Float( M_PI_2 ) );
		}
	return RefCountPtr< const Lang::Value >( new Lang::Float( acos( ( (x1/norm1) * (x2/norm2) + (y1/norm1) * (y2/norm2) ) ) ) );
}

RefCountPtr< const Lang::Value >
Ast::AngleExpr::impl( DUMMYANDREF( const Lang::FloatPair ) arg1, DUMMYANDREF( const Lang::Coords2D ) arg2, const Kernel::PassedDyn & dyn ) const
{
	const double x1 = arg1->x_;
	const double y1 = arg1->y_;
	const Concrete::Length x2 = arg2->x_.get( );
	const Concrete::Length y2 = arg2->y_.get( );
	double norm1 = hypot( x1, y1 );
	Concrete::Length norm2 = hypotPhysical( x2, y2 );
	if( norm1 == 0 || norm2 == 0 )
		{
			return RefCountPtr< const Lang::Value >( new Lang::Float( M_PI_2 ) );
		}
	return RefCountPtr< const Lang::Value >( new Lang::Float( acos( ( (x1/norm1) * (x2/norm2) + (y1/norm1) * (y2/norm2) ) ) ) );
}

RefCountPtr< const Lang::Value >
Ast::AngleExpr::impl( DUMMYANDREF( const Lang::Coords2D ) arg1, DUMMYANDREF( const Lang::FloatPair ) arg2, const Kernel::PassedDyn & dyn ) const
{
	const Concrete::Length x1 = arg1->x_.get( );
	const Concrete::Length y1 = arg1->y_.get( );
	const double x2 = arg2->x_;
	const double y2 = arg2->y_;
	Concrete::Length norm1 = hypotPhysical( x1, y1 );
	double norm2 = hypot( x2, y2 );
	if( norm1 == 0 || norm2 == 0 )
		{
			return RefCountPtr< const Lang::Value >( new Lang::Float( M_PI_2 ) );
		}
	return RefCountPtr< const Lang::Value >( new Lang::Float( acos( ( (x1/norm1) * (x2/norm2) + (y1/norm1) * (y2/norm2) ) ) ) );
}

RefCountPtr< const Lang::Value >
Ast::AngleExpr::impl( DUMMYANDREF( const Lang::Coords2D ) arg1, DUMMYANDREF( const Lang::Coords2D ) arg2, const Kernel::PassedDyn & dyn ) const
{
	const Concrete::Length x1 = arg1->x_.get( );
	const Concrete::Length y1 = arg1->y_.get( );
	const Concrete::Length x2 = arg2->x_.get( );
	const Concrete::Length y2 = arg2->y_.get( );
	Concrete::Length norm1 = hypotPhysical( x1, y1 );
	Concrete::Length norm2 = hypotPhysical( x2, y2 );
	if( norm1 == 0 || norm2 == 0 )
		{
			return RefCountPtr< const Lang::Value >( new Lang::Float( M_PI_2 ) );
		}
	return RefCountPtr< const Lang::Value >( new Lang::Float( acos( ( (x1/norm1) * (x2/norm2) + (y1/norm1) * (y2/norm2) ) ) ) );
}

RefCountPtr< const Lang::Value >
Ast::AngleExpr::impl( DUMMYANDREF( const Lang::FloatTriple ) arg1, DUMMYANDREF( const Lang::FloatTriple ) arg2, const Kernel::PassedDyn & dyn ) const
{
	const double x1 = arg1->x_;
	const double y1 = arg1->y_;
	const double z1 = arg1->z_;
	const double x2 = arg2->x_;
	const double y2 = arg2->y_;
	const double z2 = arg2->z_;
	double norm1 = Concrete::Scalar::hypot3( x1, y1, z1 );
	double norm2 = Concrete::Scalar::hypot3( x2, y2, z2 );
	if( norm1 == 0 || norm2 == 0 )
		{
			return RefCountPtr< const Lang::Value >( new Lang::Float( M_PI_2 ) );
		}
	return RefCountPtr< const Lang::Value >( new Lang::Float( acos( ( (x1/norm1) * (x2/norm2) + (y1/norm1) * (y2/norm2) + (z1/norm1) * (z2/norm2) ) ) ) );
}

RefCountPtr< const Lang::Value >
Ast::AngleExpr::impl( DUMMYANDREF( const Lang::FloatTriple ) arg1, DUMMYANDREF( const Lang::Coords3D ) arg2, const Kernel::PassedDyn & dyn ) const
{
	const double x1 = arg1->x_;
	const double y1 = arg1->y_;
	const double z1 = arg1->z_;
	const Concrete::Length x2 = arg2->x_.get( );
	const Concrete::Length y2 = arg2->y_.get( );
	const Concrete::Length z2 = arg2->z_.get( );
	double norm1 = Concrete::Scalar::hypot3( x1, y1, z1 );
	Concrete::Length norm2 = hypotPhysical( x2, y2, z2 );
	if( norm1 == 0 || norm2 == 0 )
		{
			return RefCountPtr< const Lang::Value >( new Lang::Float( M_PI_2 ) );
		}
	return RefCountPtr< const Lang::Value >( new Lang::Float( acos( ( (x1/norm1) * (x2/norm2) + (y1/norm1) * (y2/norm2) + (z1/norm1) * (z2/norm2) ) ) ) );
}

RefCountPtr< const Lang::Value >
Ast::AngleExpr::impl( DUMMYANDREF( const Lang::Coords3D ) arg1, DUMMYANDREF( const Lang::FloatTriple ) arg2, const Kernel::PassedDyn & dyn ) const
{
	const Concrete::Length x1 = arg1->x_.get( );
	const Concrete::Length y1 = arg1->y_.get( );
	const Concrete::Length z1 = arg1->z_.get( );
	const double x2 = arg2->x_;
	const double y2 = arg2->y_;
	const double z2 = arg2->z_;
	Concrete::Length norm1 = hypotPhysical( x1, y1, z1 );
	double norm2 = Concrete::Scalar::hypot3( x2, y2, z2 );
	if( norm1 == 0 || norm2 == 0 )
		{
			return RefCountPtr< const Lang::Value >( new Lang::Float( M_PI_2 ) );
		}
	return RefCountPtr< const Lang::Value >( new Lang::Float( acos( ( (x1/norm1) * (x2/norm2) + (y1/norm1) * (y2/norm2) + (z1/norm1) * (z2/norm2) ) ) ) );
}

RefCountPtr< const Lang::Value >
Ast::AngleExpr::impl( DUMMYANDREF( const Lang::Coords3D ) arg1, DUMMYANDREF( const Lang::Coords3D ) arg2, const Kernel::PassedDyn & dyn ) const
{
	const Concrete::Length x1 = arg1->x_.get( );
	const Concrete::Length y1 = arg1->y_.get( );
	const Concrete::Length z1 = arg1->z_.get( );
	const Concrete::Length x2 = arg2->x_.get( );
	const Concrete::Length y2 = arg2->y_.get( );
	const Concrete::Length z2 = arg2->z_.get( );
	Concrete::Length norm1 = hypotPhysical( x1, y1, z1 );
	Concrete::Length norm2 = hypotPhysical( x2, y2, z2 );
	if( norm1 == 0 || norm2 == 0 )
		{
			return RefCountPtr< const Lang::Value >( new Lang::Float( M_PI_2 ) );
		}
	return RefCountPtr< const Lang::Value >( new Lang::Float( acos( ( (x1/norm1) * (x2/norm2) + (y1/norm1) * (y2/norm2) + (z1/norm1) * (z2/norm2) ) ) ) );
}


RefCountPtr< const Lang::Value >
Ast::StarExpr::impl( DUMMYANDREF( const Lang::Float ) arg1, DUMMYANDREF( const Lang::Float ) arg2, const Kernel::PassedDyn & dyn ) const
{
	return RefCountPtr< const Lang::Value >( new Lang::Float( arg1->val_ * arg2->val_ ) );
}

RefCountPtr< const Lang::Value >
Ast::StarExpr::impl( DUMMYANDREF( const Lang::FloatPair ) arg1, DUMMYANDREF( const Lang::FloatPair ) arg2, const Kernel::PassedDyn & dyn ) const
{
	return RefCountPtr< const Lang::Value >( new Lang::Float( arg1->x_ * arg2->x_ + arg1->y_ * arg2->y_ ) );
}

RefCountPtr< const Lang::Value >
Ast::StarExpr::impl( DUMMYANDREF( const Lang::FloatPair ) arg1, DUMMYANDREF( const Lang::Float ) arg2, const Kernel::PassedDyn & dyn ) const
{
	return RefCountPtr< const Lang::Value >( new Lang::FloatPair( arg1->x_ * arg2->val_, arg1->y_ * arg2->val_ ) );
}

RefCountPtr< const Lang::Value >
Ast::StarExpr::impl( DUMMYANDREF( const Lang::Float ) arg1, DUMMYANDREF( const Lang::FloatPair ) arg2, const Kernel::PassedDyn & dyn ) const
{
	return RefCountPtr< const Lang::Value >( new Lang::FloatPair( arg1->val_ * arg2->x_, arg1->val_ * arg2->y_ ) );
}

RefCountPtr< const Lang::Value >
Ast::StarExpr::impl( DUMMYANDREF( const Lang::FloatTriple ) arg1, DUMMYANDREF( const Lang::FloatTriple ) arg2, const Kernel::PassedDyn & dyn ) const
{
	return RefCountPtr< const Lang::Value >( new Lang::Float( arg1->x_ * arg2->x_ + arg1->y_ * arg2->y_ + arg1->z_ * arg2->z_ ) );
}

RefCountPtr< const Lang::Value >
Ast::StarExpr::impl( DUMMYANDREF( const Lang::FloatTriple ) arg1, DUMMYANDREF( const Lang::Float ) arg2, const Kernel::PassedDyn & dyn ) const
{
	return RefCountPtr< const Lang::Value >( new Lang::FloatTriple( arg1->x_ * arg2->val_, arg1->y_ * arg2->val_, arg1->z_ * arg2->val_ ) );
}

RefCountPtr< const Lang::Value >
Ast::StarExpr::impl( DUMMYANDREF( const Lang::Float ) arg1, DUMMYANDREF( const Lang::FloatTriple ) arg2, const Kernel::PassedDyn & dyn ) const
{
	return RefCountPtr< const Lang::Value >( new Lang::FloatTriple( arg1->val_ * arg2->x_, arg1->val_ * arg2->y_, arg1->val_ * arg2->z_ ) );
}

RefCountPtr< const Lang::Value >
Ast::StarExpr::impl( DUMMYANDREF( const Lang::Length ) arg1, DUMMYANDREF( const Lang::Float ) arg2, const Kernel::PassedDyn & dyn ) const
{
	return RefCountPtr< const Lang::Value >( new Lang::Length( arg1->get( ) * arg2->val_ ) );
}

RefCountPtr< const Lang::Value >
Ast::StarExpr::impl( DUMMYANDREF( const Lang::Float ) arg1, DUMMYANDREF( const Lang::Length ) arg2, const Kernel::PassedDyn & dyn ) const
{
	return RefCountPtr< const Lang::Value >( new Lang::Length( arg1->val_ * arg2->get( ) ) );
}

RefCountPtr< const Lang::Value >
Ast::StarExpr::impl( DUMMYANDREF( const Lang::Coords2D ) arg1, DUMMYANDREF( const Lang::Float ) arg2, const Kernel::PassedDyn & dyn ) const
{
	return RefCountPtr< const Lang::Value >( new Lang::Coords2D( arg1->x_.get( ) * arg2->val_, arg1->y_.get( ) * arg2->val_ ) );
}

RefCountPtr< const Lang::Value >
Ast::StarExpr::impl( DUMMYANDREF( const Lang::Float ) arg1, DUMMYANDREF( const Lang::Coords2D ) arg2, const Kernel::PassedDyn & dyn ) const
{
	return RefCountPtr< const Lang::Value >( new Lang::Coords2D( arg1->val_ * arg2->x_.get( ), arg1->val_ * arg2->y_.get( ) ) );
}

RefCountPtr< const Lang::Value >
Ast::StarExpr::impl( DUMMYANDREF( const Lang::Coords2D ) arg1, DUMMYANDREF( const Lang::FloatPair ) arg2, const Kernel::PassedDyn & dyn ) const
{
	return RefCountPtr< const Lang::Value >( new Lang::Length( arg1->x_.get( ) * arg2->x_ + arg1->y_.get( ) * arg2->y_ ) );
}

RefCountPtr< const Lang::Value >
Ast::StarExpr::impl( DUMMYANDREF( const Lang::FloatPair ) arg1, DUMMYANDREF( const Lang::Coords2D ) arg2, const Kernel::PassedDyn & dyn ) const
{
	return RefCountPtr< const Lang::Value >( new Lang::Length( arg1->x_ * arg2->x_.get( ) + arg1->y_ * arg2->y_.get( ) ) );
}

RefCountPtr< const Lang::Value >
Ast::StarExpr::impl( DUMMYANDREF( const Lang::FloatPair ) arg1, DUMMYANDREF( const Lang::Length ) arg2, const Kernel::PassedDyn & dyn ) const
{
	return RefCountPtr< const Lang::Value >( new Lang::Coords2D( arg1->x_ * arg2->get( ), arg1->y_ * arg2->get( ) ) );
}

RefCountPtr< const Lang::Value >
Ast::StarExpr::impl( DUMMYANDREF( const Lang::Length ) arg1, DUMMYANDREF( const Lang::FloatPair ) arg2, const Kernel::PassedDyn & dyn ) const
{
	return RefCountPtr< const Lang::Value >( new Lang::Coords2D( arg1->get( ) * arg2->x_, arg1->get( ) * arg2->y_ ) );
}

RefCountPtr< const Lang::Value >
Ast::StarExpr::impl( DUMMYANDREF( const Lang::Coords3D ) arg1, DUMMYANDREF( const Lang::Float ) arg2, const Kernel::PassedDyn & dyn ) const
{
	return RefCountPtr< const Lang::Value >( new Lang::Coords3D( arg1->x_.get( ) * arg2->val_, arg1->y_.get( ) * arg2->val_, arg1->z_.get( ) * arg2->val_ ) );
}

RefCountPtr< const Lang::Value >
Ast::StarExpr::impl( DUMMYANDREF( const Lang::Float ) arg1, DUMMYANDREF( const Lang::Coords3D ) arg2, const Kernel::PassedDyn & dyn ) const
{
	return RefCountPtr< const Lang::Value >( new Lang::Coords3D( arg1->val_ * arg2->x_.get( ), arg1->val_ * arg2->y_.get( ), arg1->val_ * arg2->z_.get( ) ) );
}

RefCountPtr< const Lang::Value >
Ast::StarExpr::impl( DUMMYANDREF( const Lang::Coords3D ) arg1, DUMMYANDREF( const Lang::FloatTriple ) arg2, const Kernel::PassedDyn & dyn ) const
{
	return RefCountPtr< const Lang::Value >( new Lang::Length( arg1->x_.get( ) * arg2->x_ + arg1->y_.get( ) * arg2->y_ + arg1->z_.get( ) * arg2->z_ ) );
}

RefCountPtr< const Lang::Value >
Ast::StarExpr::impl( DUMMYANDREF( const Lang::FloatTriple ) arg1, DUMMYANDREF( const Lang::Coords3D ) arg2, const Kernel::PassedDyn & dyn ) const
{
	return RefCountPtr< const Lang::Value >( new Lang::Length( arg1->x_ * arg2->x_.get( ) + arg1->y_ * arg2->y_.get( ) + arg1->z_ * arg2->z_.get( ) ) );
}

RefCountPtr< const Lang::Value >
Ast::StarExpr::impl( DUMMYANDREF( const Lang::FloatTriple ) arg1, DUMMYANDREF( const Lang::Length ) arg2, const Kernel::PassedDyn & dyn ) const
{
	return RefCountPtr< const Lang::Value >( new Lang::Coords3D( arg1->x_ * arg2->get( ), arg1->y_ * arg2->get( ), arg1->z_ * arg2->get( ) ) );
}

RefCountPtr< const Lang::Value >
Ast::StarExpr::impl( DUMMYANDREF( const Lang::Length ) arg1, DUMMYANDREF( const Lang::FloatTriple ) arg2, const Kernel::PassedDyn & dyn ) const
{
	return RefCountPtr< const Lang::Value >( new Lang::Coords3D( arg1->get( ) * arg2->x_, arg1->get( ) * arg2->y_, arg1->get( ) * arg2->z_ ) );
}

RefCountPtr< const Lang::Value >
Ast::StarExpr::impl( DUMMYANDREF( const Lang::Dash ) arg1, DUMMYANDREF( const Lang::Float ) arg2, const Kernel::PassedDyn & dyn ) const
{
	if( arg2->val_ <= 0 )
		{
			throw Exceptions::OutOfRange( this->loc( ), strrefdup( "Dashpatterns can only be scaled by positive values" ) );
		}

	return arg1->scaled( arg2->val_ );
}

RefCountPtr< const Lang::Value >
Ast::StarExpr::impl( DUMMYANDREF( const Lang::Float ) arg1, DUMMYANDREF( const Lang::Dash ) arg2, const Kernel::PassedDyn & dyn ) const
{
	if( arg1->val_ <= 0 )
		{
			throw Exceptions::OutOfRange( this->loc( ), strrefdup( "Dashpatterns can only be scaled by positive values" ) );
		}

	return arg2->scaled( arg1->val_ );
}

RefCountPtr< const Lang::Value >
Ast::StarExpr::impl( DUMMYANDREF( const Lang::Transform2D ) arg1, DUMMYANDREF( const Lang::Transform2D ) arg2, const Kernel::PassedDyn & dyn ) const
{
	return RefCountPtr< const Lang::Value >( new Lang::Transform2D( *arg1, *arg2 ) );
}

RefCountPtr< const Lang::Value >
Ast::StarExpr::impl( DUMMYANDREF( const Lang::Transform3D ) arg1, DUMMYANDREF( const Lang::Transform3D ) arg2, const Kernel::PassedDyn & dyn ) const
{
	return RefCountPtr< const Lang::Value >( new Lang::Transform3D( *arg1, *arg2 ) );
}

RefCountPtr< const Lang::Value >
Ast::StarExpr::impl( DUMMYANDREF( const Lang::Float ) arg1, DUMMYANDREF( const Lang::RGB ) arg2, const Kernel::PassedDyn & dyn ) const
{
	return RefCountPtr< const Lang::Value >( new Lang::RGB( arg2->components( ).mul( arg1->val_, expr1_->loc( ) ) ) );
}

RefCountPtr< const Lang::Value >
Ast::StarExpr::impl( DUMMYANDREF( const Lang::RGB ) arg1, DUMMYANDREF( const Lang::Float ) arg2, const Kernel::PassedDyn & dyn ) const
{
	return RefCountPtr< const Lang::Value >( new Lang::RGB( arg1->components( ).mul( arg2->val_, expr1_->loc( ) ) ) );
}

RefCountPtr< const Lang::Value >
Ast::StarExpr::impl( DUMMYANDREF( const Lang::Float ) arg1, DUMMYANDREF( const Lang::Gray ) arg2, const Kernel::PassedDyn & dyn ) const
{
	return RefCountPtr< const Lang::Value >( new Lang::Gray( arg2->components( ).mul( arg1->val_, expr1_->loc( ) ) ) );
}

RefCountPtr< const Lang::Value >
Ast::StarExpr::impl( DUMMYANDREF( const Lang::Gray ) arg1, DUMMYANDREF( const Lang::Float ) arg2, const Kernel::PassedDyn & dyn ) const
{
	return RefCountPtr< const Lang::Value >( new Lang::Gray( arg1->components( ).mul( arg2->val_, expr1_->loc( ) ) ) );
}

RefCountPtr< const Lang::Value >
Ast::StarExpr::impl( DUMMYANDREF( const Lang::Float ) arg1, DUMMYANDREF( const Lang::SpecularReflection ) arg2, const Kernel::PassedDyn & dyn ) const
{
	double scalar = arg1->val_;
	if( scalar < 0 || scalar > 1 )
		{
			throw Exceptions::OutOfRange( expr1_->loc( ), strrefdup( "The scalar must be in the interval [ 0, 1 ]." ) );
		}
	return arg2->multiply( scalar );
}

RefCountPtr< const Lang::Value >
Ast::StarExpr::impl( DUMMYANDREF( const Lang::SpecularReflection ) arg1, DUMMYANDREF( const Lang::Float ) arg2, const Kernel::PassedDyn & dyn ) const
{
	double scalar = arg2->val_;
	if( scalar < 0 || scalar > 1 )
		{
			throw Exceptions::OutOfRange( expr2_->loc( ), strrefdup( "The scalar must be in the interval [ 0, 1 ]." ) );
		}
	return arg1->multiply( scalar );
}

RefCountPtr< const Lang::Value >
Ast::StarExpr::impl( DUMMYANDREF( const Lang::Integer ) arg1, DUMMYANDREF( const Lang::Integer ) arg2, const Kernel::PassedDyn & dyn ) const
{
	return RefCountPtr< const Lang::Value >( new Lang::Integer( arg1->val_ * arg2->val_ ) );
}

RefCountPtr< const Lang::Value >
Ast::StarExpr::impl( DUMMYANDREF( const Lang::Float ) arg1, DUMMYANDREF( const Lang::Integer ) arg2, const Kernel::PassedDyn & dyn ) const
{
	return RefCountPtr< const Lang::Value >( new Lang::Float( arg1->val_ * arg2->val_ ) );
}

RefCountPtr< const Lang::Value >
Ast::StarExpr::impl( DUMMYANDREF( const Lang::Integer ) arg1, DUMMYANDREF( const Lang::Float ) arg2, const Kernel::PassedDyn & dyn ) const
{
	return RefCountPtr< const Lang::Value >( new Lang::Float( arg1->val_ * arg2->val_ ) );
}

RefCountPtr< const Lang::Value >
Ast::StarExpr::impl( DUMMYANDREF( const Lang::Length ) arg1, DUMMYANDREF( const Lang::Integer ) arg2, const Kernel::PassedDyn & dyn ) const
{
	return RefCountPtr< const Lang::Value >( new Lang::Length( arg1->get( ) * arg2->val_ ) );
}

RefCountPtr< const Lang::Value >
Ast::StarExpr::impl( DUMMYANDREF( const Lang::Integer ) arg1, DUMMYANDREF( const Lang::Length ) arg2, const Kernel::PassedDyn & dyn ) const
{
	return RefCountPtr< const Lang::Value >( new Lang::Length( arg1->val_ * arg2->get( ) ) );
}


RefCountPtr< const Lang::Value >
Ast::ProjectionExpr::impl( DUMMYANDREF( const Lang::FloatPair ) arg1, DUMMYANDREF( const Lang::FloatPair ) arg2, const Kernel::PassedDyn & dyn ) const
{
	const double x1 = arg1->x_;
	const double y1 = arg1->y_;
	const double x2 = arg2->x_;
	const double y2 = arg2->y_;
	double norm2_2 = x2 * x2 + y2 * y2;
	if( norm2_2 == 0 )
		{
			throw Exceptions::OutOfRange( expr2_->loc( ), strrefdup( "Projection on zero length vector." ) );
		}
	double s = ( x1 * x2 + y1 * y2 ) / norm2_2;
	return RefCountPtr< const Lang::Value >( new Lang::FloatPair( s * x2, s * y2 ) );
}

RefCountPtr< const Lang::Value >
Ast::ProjectionExpr::impl( DUMMYANDREF( const Lang::FloatPair ) arg1, DUMMYANDREF( const Lang::Coords2D ) arg2, const Kernel::PassedDyn & dyn ) const
{
	const double x1 = arg1->x_;
	const double y1 = arg1->y_;
	const Concrete::Length x2 = arg2->x_.get( );
	const Concrete::Length y2 = arg2->y_.get( );
	Physical< 2, 0 > norm2_2 = x2 * x2 + y2 * y2;
	if( norm2_2 == 0 )
		{
			throw Exceptions::OutOfRange( expr2_->loc( ), strrefdup( "Projection on zero length vector." ) );
		}
	Physical< -1, 0 > s = ( x1 * x2 + y1 * y2 ) / norm2_2;
	return RefCountPtr< const Lang::Value >( new Lang::FloatPair( s * x2, s * y2 ) );
}

RefCountPtr< const Lang::Value >
Ast::ProjectionExpr::impl( DUMMYANDREF( const Lang::Coords2D ) arg1, DUMMYANDREF( const Lang::FloatPair ) arg2, const Kernel::PassedDyn & dyn ) const
{
	const Concrete::Length x1 = arg1->x_.get( );
	const Concrete::Length y1 = arg1->y_.get( );
	const double x2 = arg2->x_;
	const double y2 = arg2->y_;
	const double norm2_2 = x2 * x2 + y2 * y2;
	if( norm2_2 == 0 )
		{
			throw Exceptions::OutOfRange( expr2_->loc( ), strrefdup( "Projection on zero length vector." ) );
		}
	Concrete::Length s = ( x1 * x2 + y1 * y2 ) / norm2_2;
	return RefCountPtr< const Lang::Value >( new Lang::Coords2D( s * x2, s * y2 ) );
}

RefCountPtr< const Lang::Value >
Ast::ProjectionExpr::impl( DUMMYANDREF( const Lang::Coords2D ) arg1, DUMMYANDREF( const Lang::Coords2D ) arg2, const Kernel::PassedDyn & dyn ) const
{
	const Concrete::Length x1 = arg1->x_.get( );
	const Concrete::Length y1 = arg1->y_.get( );
	const Concrete::Length x2 = arg2->x_.get( );
	const Concrete::Length y2 = arg2->y_.get( );
	Physical< 2, 0 > norm2_2 = x2 * x2 + y2 * y2;
	if( norm2_2 == 0 )
		{
			throw Exceptions::OutOfRange( expr2_->loc( ), strrefdup( "Projection on zero length vector." ) );
		}
	double s = ( x1 * x2 + y1 * y2 ) / norm2_2;
	return RefCountPtr< const Lang::Value >( new Lang::Coords2D( s * x2, s * y2 ) );
}

RefCountPtr< const Lang::Value >
Ast::ProjectionExpr::impl( DUMMYANDREF( const Lang::FloatTriple ) arg1, DUMMYANDREF( const Lang::FloatTriple ) arg2, const Kernel::PassedDyn & dyn ) const
{
	const double x1 = arg1->x_;
	const double y1 = arg1->y_;
	const double z1 = arg1->z_;
	const double x2 = arg2->x_;
	const double y2 = arg2->y_;
	const double z2 = arg2->z_;
	double norm2_2 = x2 * x2 + y2 * y2 + z2 * z2;
	if( norm2_2 == 0 )
		{
			throw Exceptions::OutOfRange( expr2_->loc( ), strrefdup( "Projection on zero length vector." ) );
		}
	double s = ( x1 * x2 + y1 * y2 + z1 * z2 ) / norm2_2;
	return RefCountPtr< const Lang::Value >( new Lang::FloatTriple( s * x2, s * y2, s * z2 ) );
}

RefCountPtr< const Lang::Value >
Ast::ProjectionExpr::impl( DUMMYANDREF( const Lang::FloatTriple ) arg1, DUMMYANDREF( const Lang::Coords3D ) arg2, const Kernel::PassedDyn & dyn ) const
{
	const double x1 = arg1->x_;
	const double y1 = arg1->y_;
	const double z1 = arg1->z_;
	const Concrete::Length x2 = arg2->x_.get( );
	const Concrete::Length y2 = arg2->y_.get( );
	const Concrete::Length z2 = arg2->z_.get( );
	Physical< 2, 0 > norm2_2 = x2 * x2 + y2 * y2 + z2 * z2;
	if( norm2_2 == 0 )
		{
			throw Exceptions::OutOfRange( expr2_->loc( ), strrefdup( "Projection on zero length vector." ) );
		}
	Physical< -1, 0 > s = ( x1 * x2 + y1 * y2 + z1 * z2 ) / norm2_2;
	return RefCountPtr< const Lang::Value >( new Lang::FloatTriple( s * x2, s * y2, s * z2 ) );
}

RefCountPtr< const Lang::Value >
Ast::ProjectionExpr::impl( DUMMYANDREF( const Lang::Coords3D ) arg1, DUMMYANDREF( const Lang::FloatTriple ) arg2, const Kernel::PassedDyn & dyn ) const
{
	const Concrete::Length x1 = arg1->x_.get( );
	const Concrete::Length y1 = arg1->y_.get( );
	const Concrete::Length z1 = arg1->z_.get( );
	const double x2 = arg2->x_;
	const double y2 = arg2->y_;
	const double z2 = arg2->z_;
	const double norm2_2 = x2 * x2 + y2 * y2 + z2 * z2;
	if( norm2_2 == 0 )
		{
			throw Exceptions::OutOfRange( expr2_->loc( ), strrefdup( "Projection on zero length vector." ) );
		}
	Concrete::Length s = ( x1 * x2 + y1 * y2 + z1 * z2 ) / norm2_2;
	return RefCountPtr< const Lang::Value >( new Lang::Coords3D( s * x2, s * y2, s * z2 ) );
}

RefCountPtr< const Lang::Value >
Ast::ProjectionExpr::impl( DUMMYANDREF( const Lang::Coords3D ) arg1, DUMMYANDREF( const Lang::Coords3D ) arg2, const Kernel::PassedDyn & dyn ) const
{
	const Concrete::Length x1 = arg1->x_.get( );
	const Concrete::Length y1 = arg1->y_.get( );
	const Concrete::Length z1 = arg1->z_.get( );
	const Concrete::Length x2 = arg2->x_.get( );
	const Concrete::Length y2 = arg2->y_.get( );
	const Concrete::Length z2 = arg2->z_.get( );
	Physical< 2, 0 > norm2_2 = x2 * x2 + y2 * y2 + z2 * z2;
	if( norm2_2 == 0 )
		{
			throw Exceptions::OutOfRange( expr2_->loc( ), strrefdup( "Projection on zero length vector." ) );
		}
	double s = ( x1 * x2 + y1 * y2 + z1 * z2 ) / norm2_2;
	return RefCountPtr< const Lang::Value >( new Lang::Coords3D( s * x2, s * y2, s * z2 ) );
}


RefCountPtr< const Lang::Value >
Ast::SlashExpr::impl( DUMMYANDREF( const Lang::Float ) arg1, DUMMYANDREF( const Lang::Float ) arg2, const Kernel::PassedDyn & dyn ) const
{
	return RefCountPtr< const Lang::Value >( new Lang::Float( arg1->val_ / arg2->val_ ) );
}

RefCountPtr< const Lang::Value >
Ast::SlashExpr::impl( DUMMYANDREF( const Lang::Length ) arg1, DUMMYANDREF( const Lang::Length ) arg2, const Kernel::PassedDyn & dyn ) const
{
	return RefCountPtr< const Lang::Value >( new Lang::Float( arg1->get( ) / arg2->get( ) ) );
}

RefCountPtr< const Lang::Value >
Ast::SlashExpr::impl( DUMMYANDREF( const Lang::Length ) arg1, DUMMYANDREF( const Lang::Float ) arg2, const Kernel::PassedDyn & dyn ) const
{
	return RefCountPtr< const Lang::Value >( new Lang::Length( arg1->get( ) / arg2->val_ ) );
}

RefCountPtr< const Lang::Value >
Ast::SlashExpr::impl( DUMMYANDREF( const Lang::FloatPair ) arg1, DUMMYANDREF( const Lang::Float ) arg2, const Kernel::PassedDyn & dyn ) const
{
	return RefCountPtr< const Lang::Value >( new Lang::FloatPair( arg1->x_ / arg2->val_, arg1->y_ / arg2->val_ ) );
}

RefCountPtr< const Lang::Value >
Ast::SlashExpr::impl( DUMMYANDREF( const Lang::Coords2D ) arg1, DUMMYANDREF( const Lang::Float ) arg2, const Kernel::PassedDyn & dyn ) const
{
	return RefCountPtr< const Lang::Value >( new Lang::Coords2D( arg1->x_.get( ) / arg2->val_, arg1->y_.get( ) / arg2->val_ ) );
}

RefCountPtr< const Lang::Value >
Ast::SlashExpr::impl( DUMMYANDREF( const Lang::Coords2D ) arg1, DUMMYANDREF( const Lang::Length ) arg2, const Kernel::PassedDyn & dyn ) const
{
	return RefCountPtr< const Lang::Value >( new Lang::FloatPair( arg1->x_.get( ) / arg2->get( ), arg1->y_.get( ) / arg2->get( ) ) );
}

RefCountPtr< const Lang::Value >
Ast::SlashExpr::impl( DUMMYANDREF( const Lang::FloatTriple ) arg1, DUMMYANDREF( const Lang::Float ) arg2, const Kernel::PassedDyn & dyn ) const
{
	return RefCountPtr< const Lang::Value >( new Lang::FloatTriple( arg1->x_ / arg2->val_, arg1->y_ / arg2->val_, arg1->z_ / arg2->val_ ) );
}

RefCountPtr< const Lang::Value >
Ast::SlashExpr::impl( DUMMYANDREF( const Lang::Coords3D ) arg1, DUMMYANDREF( const Lang::Float ) arg2, const Kernel::PassedDyn & dyn ) const
{
	return RefCountPtr< const Lang::Value >( new Lang::Coords3D( arg1->x_.get( ) / arg2->val_, arg1->y_.get( ) / arg2->val_, arg1->z_.get( ) / arg2->val_ ) );
}

RefCountPtr< const Lang::Value >
Ast::SlashExpr::impl( DUMMYANDREF( const Lang::Coords3D ) arg1, DUMMYANDREF( const Lang::Length ) arg2, const Kernel::PassedDyn & dyn ) const
{
	return RefCountPtr< const Lang::Value >( new Lang::FloatTriple( arg1->x_.get( ) / arg2->get( ), arg1->y_.get( ) / arg2->get( ), arg1->z_.get( ) / arg2->get( ) ) );
}

RefCountPtr< const Lang::Value >
Ast::SlashExpr::impl( DUMMYANDREF( const Lang::Integer ) arg1, DUMMYANDREF( const Lang::Integer ) arg2, const Kernel::PassedDyn & dyn ) const
{
	return RefCountPtr< const Lang::Value >( new Lang::Integer( arg1->val_ / arg2->val_ ) );
}

RefCountPtr< const Lang::Value >
Ast::SlashExpr::impl( DUMMYANDREF( const Lang::Float ) arg1, DUMMYANDREF( const Lang::Integer ) arg2, const Kernel::PassedDyn & dyn ) const
{
	return RefCountPtr< const Lang::Value >( new Lang::Float( arg1->val_ / arg2->val_ ) );
}

RefCountPtr< const Lang::Value >
Ast::SlashExpr::impl( DUMMYANDREF( const Lang::Length ) arg1, DUMMYANDREF( const Lang::Integer ) arg2, const Kernel::PassedDyn & dyn ) const
{
	return RefCountPtr< const Lang::Value >( new Lang::Length( arg1->get( ) / static_cast< double >( arg2->val_ ) ) );
}


RefCountPtr< const Lang::Value >
Ast::ComposeExpr::impl( DUMMYANDREF( const Lang::Transform2D ) arg1, DUMMYANDREF( const Lang::Transform2D ) arg2, const Kernel::PassedDyn & dyn ) const
{
	return RefCountPtr< const Lang::Value >( new Lang::Transform2D( *arg1, *arg2 ) );
}

RefCountPtr< const Lang::Value >
Ast::ComposeExpr::impl( DUMMYANDREF( const Lang::Transform3D ) arg1, DUMMYANDREF( const Lang::Transform3D ) arg2, const Kernel::PassedDyn & dyn ) const
{
	return RefCountPtr< const Lang::Value >( new Lang::Transform3D( *arg1, *arg2 ) );
}

RefCountPtr< const Lang::Value >
Ast::ComposeExpr::impl( DUMMYANDREF( const Lang::Function ) arg1, DUMMYANDREF( const Lang::Function ) arg2, const Kernel::PassedDyn & dyn ) const
{
	return RefCountPtr< const Lang::Value >( new Lang::ComposedFunction( arg1, arg2 ) );
}
