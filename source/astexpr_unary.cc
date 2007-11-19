#include <cmath>

#include "astexprs.h"
#include "shapesexceptions.h"
#include "lighttypes.h"

using namespace Shapes;
using namespace std;


RefCountPtr< const Lang::Value >
Ast::NegExpr::impl( DUMMYANDREF( const Lang::Float ) arg, const Kernel::PassedDyn & dyn ) const
{
	return RefCountPtr< const Lang::Value >( new Lang::Float( - arg->val_ ) );
}

RefCountPtr< const Lang::Value >
Ast::NegExpr::impl( DUMMYANDREF( const Lang::Integer ) arg, const Kernel::PassedDyn & dyn ) const
{
	return RefCountPtr< const Lang::Value >( new Lang::Integer( - arg->val_ ) );
}

RefCountPtr< const Lang::Value >
Ast::NegExpr::impl( DUMMYANDREF( const Lang::Length ) arg, const Kernel::PassedDyn & dyn ) const
{
	return RefCountPtr< const Lang::Value >( new Lang::Length( - arg->get( ) ) );
}

RefCountPtr< const Lang::Value >
Ast::NegExpr::impl( DUMMYANDREF( const Lang::FloatPair ) arg, const Kernel::PassedDyn & dyn ) const
{
	return RefCountPtr< const Lang::Value >( new Lang::FloatPair( - arg->x_, - arg->y_ ) );
}

RefCountPtr< const Lang::Value >
Ast::NegExpr::impl( DUMMYANDREF( const Lang::Coords2D ) arg, const Kernel::PassedDyn & dyn ) const
{
	return RefCountPtr< const Lang::Value >( new Lang::Coords2D( - arg->x_.get( ), - arg->y_.get( ) ) );
}

RefCountPtr< const Lang::Value >
Ast::NegExpr::impl( DUMMYANDREF( const Lang::FloatTriple ) arg, const Kernel::PassedDyn & dyn ) const
{
	return RefCountPtr< const Lang::Value >( new Lang::FloatTriple( - arg->x_, - arg->y_, - arg->z_ ) );
}

RefCountPtr< const Lang::Value >
Ast::NegExpr::impl( DUMMYANDREF( const Lang::Coords3D ) arg, const Kernel::PassedDyn & dyn ) const
{
	return RefCountPtr< const Lang::Value >( new Lang::Coords3D( - arg->x_.get( ), - arg->y_.get( ), - arg->z_.get( ) ) );
}


RefCountPtr< const Lang::Value >
Ast::RelativeExpr::impl( DUMMYANDREF( const Lang::Length ) arg, const Kernel::PassedDyn & dyn ) const
{
	return RefCountPtr< const Lang::Value >( new Lang::Length( true, arg->get( ) ) );
}

RefCountPtr< const Lang::Value >
Ast::RelativeExpr::impl( DUMMYANDREF( const Lang::Coords2D ) arg, const Kernel::PassedDyn & dyn ) const
{
	return RefCountPtr< const Lang::Value >( new Lang::Coords2D( Lang::Length( true, arg->x_.get( ) ), Lang::Length( true, arg->y_.get( ) ) ) );
}

RefCountPtr< const Lang::Value >
Ast::RelativeExpr::impl( DUMMYANDREF( const Lang::Coords3D ) arg, const Kernel::PassedDyn & dyn ) const
{
	return RefCountPtr< const Lang::Value >( new Lang::Coords3D( Lang::Length( true, arg->x_.get( ) ), Lang::Length( true, arg->y_.get( ) ), Lang::Length( true, arg->z_.get( ) ) ) );
}


RefCountPtr< const Lang::Value >
Ast::NotExpr::impl( DUMMYANDREF( const Lang::Boolean ) arg, const Kernel::PassedDyn & dyn ) const
{
	return RefCountPtr< const Lang::Value >( new Lang::Boolean( ! arg->val_ ) );
}


RefCountPtr< const Lang::Value >
Ast::CycleExpr::impl( DUMMYANDREF( const Lang::PathPoint2D ) arg, const Kernel::PassedDyn & dyn ) const
{
	const Lang::SinglePointPath2D * tmp = new Lang::SinglePointPath2D( arg );
	return impl( tmp, RefCountPtr< const Lang::SinglePointPath2D >( tmp ), dyn );
}

RefCountPtr< const Lang::Value >
Ast::CycleExpr::impl( DUMMYANDREF( const Lang::Path2D ) arg, const Kernel::PassedDyn & dyn ) const
{
	return RefCountPtr< const Lang::Value >( new Lang::ClosedPath2D( arg ) );
}

RefCountPtr< const Lang::Value >
Ast::CycleExpr::impl( DUMMYANDREF( const Lang::PathPoint3D ) arg, const Kernel::PassedDyn & dyn ) const
{
	const Lang::SinglePointPath3D * tmp = new Lang::SinglePointPath3D( arg );
	return impl( tmp, RefCountPtr< const Lang::SinglePointPath3D >( tmp ), dyn );
}

RefCountPtr< const Lang::Value >
Ast::CycleExpr::impl( DUMMYANDREF( const Lang::Path3D ) arg, const Kernel::PassedDyn & dyn ) const
{
	return RefCountPtr< const Lang::Value >( new Lang::ClosedPath3D( arg ) );
}
