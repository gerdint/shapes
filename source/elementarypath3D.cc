#include <cmath>

#include "metapdftypes.h"
#include "metapdfexceptions.h"
#include "metapdfastexpr.h"
#include "consts.h"
#include "globals.h"
#include "angleselect.h"
#include "bezier.h"
#include "trianglefunctions.h"

#include <complex>
#include <ctype.h>
#include <stack>

using namespace MetaPDF;
using namespace std;


template< int L, int T >
Physical< L + L, T + T >
squareSum( Physical< L, T > d1, Physical< L, T > d2, Physical< L, T > d3 )
{
  return d1 * d1 + d2 * d2 + d3 * d3;
}

Physical< 3, -6 >
tripleInner( Concrete::Speed vx, Concrete::Speed vy, Concrete::Speed vz, 
	     Concrete::Acceleration ax, Concrete::Acceleration ay, Concrete::Acceleration az,
	     Concrete::Jerk jx, Concrete::Jerk jy, Concrete::Jerk jz )
{
  return
      vx * ( ay * jz - az * jy )
    - vy * ( ax * jz - az * jx )
    + vz * ( ax * jy - ay * jx );
}

Lang::ElementaryPath3D::ElementaryPath3D( )
{ }

DISPATCHIMPL( ElementaryPath3D );

Lang::ElementaryPath3D::~ElementaryPath3D( )
{ }

Kernel::HandleType
Lang::ElementaryPath3D::getField( const char * fieldID, const RefCountPtr< const Lang::Value > & selfRef ) const
{
  return getField( fieldID, selfRef.down_cast< const Lang::ElementaryPath3D >( ) );
}

Kernel::HandleType
Lang::ElementaryPath3D::getField( const char * fieldID, const RefCountPtr< const Lang::ElementaryPath3D > & selfRef ) const
{
  if( strcmp( fieldID, "begin" ) == 0 )
    {
      return Helpers::newValHandle( new Lang::PathSlider3D( selfRef, Concrete::ZERO_TIME ) );
    }
  if( strcmp( fieldID, "end" ) == 0 )
    {
      return Helpers::newValHandle( new Lang::PathSlider3D( selfRef, Concrete::Time( duration( ) ) ) );
    }
  throw Exceptions::NonExistentMember( getTypeName( ), fieldID );
}

RefCountPtr< const Lang::ElementaryPath3D >
Lang::ElementaryPath3D::elementaryTransformed( const Lang::Transform3D & tf ) const
{
  Lang::ElementaryPath3D * res = new Lang::ElementaryPath3D;
  if( closed_ )
    {
      res->close( );
    }
  for( const_iterator i = begin( ); i != end( ); ++i )
    {
      res->push_back( (*i)->transformed( tf ) );
    }
  return RefCountPtr< const Lang::ElementaryPath3D >( res );
}

RefCountPtr< const Lang::SubPath3D >
Lang::ElementaryPath3D::typed_transformed( const Lang::Transform3D & tf ) const
{
  return elementaryTransformed( tf );
}

void
Lang::ElementaryPath3D::elementaryJob( std::stack< const Lang::SubPath3D * > * nodeStack, Lang::ElementaryPath3D * pth, Concrete::Coords3D * basePoint ) const
{
  for( const_iterator i = begin( ); i != end( ); ++i )
    {
      pth->push_back( new Concrete::PathPoint3D( **i ) );
    }
}

Concrete::Time
Lang::ElementaryPath3D::timeCheck( Concrete::Time t ) const
{
  if( closed_ )
    {
      if( isinfPhysical( t ) )
	{
	  throw Exceptions::OutOfRange( "The time argument must not be infinite for closed_ paths." );
	}
      Concrete::Time tMax = Concrete::Time( duration( ) );
      t = modPhysical( t, tMax );
      if( t < 0 )
	{
	  t += tMax;
	}
    }
  else
    {
      if( t < 0 )
	{
	  throw Exceptions::OutOfRange( "The time argument must be non-negative for open paths." );
	}
      if( t >= HUGE_VAL )
	{
	  t = Concrete::Time( duration( ) );
	}
      else if( t > Concrete::Time( duration( ) ) )
	{
	  throw Exceptions::OutOfRange( "The time argument must not be greater than the duration of the path." );
	}
    }
  return t;
}

void
Lang::ElementaryPath3D::findSegment( Concrete::Time t, Concrete::Time * tRes, const Concrete::PathPoint3D ** p1, const Concrete::PathPoint3D ** p2 ) const
{
  t = timeCheck( t );
  const_iterator i = begin( );
  for( ; t >= Concrete::UNIT_TIME; t -= Concrete::UNIT_TIME, ++i )
    { }
  *tRes = t;
  if( i == end( ) )
    {
      i = begin( );
    }
  *p1 = *i;

  ++i;
  if( i == end( ) )
    {
      i = begin( );
    }
  *p2 = *i;
}

void
Lang::ElementaryPath3D::findSegmentReverse( Concrete::Time t, Concrete::Time * tRes, const Concrete::PathPoint3D ** p1, const Concrete::PathPoint3D ** p2 ) const
{
  t = timeCheck( t );

  const_iterator i = begin( );
  for( ; t > 0; t -= Concrete::UNIT_TIME, ++i )
    { }
  *tRes = t + 1;
  if( i == end( ) )
    {
      i = begin( );
    }
  *p2 = *i;
  if( i == begin( ) )
    {
      i = end( );
    }
  --i;
  *p1 = *i;
}

RefCountPtr< const Lang::Coords3D >
Lang::ElementaryPath3D::point( Concrete::Time globalTime ) const
{
  if( size( ) == 0 )
    {
      throw Exceptions::OutOfRange( "The empty path has no points at all." );
    }
  Concrete::Time t;
  const Concrete::PathPoint3D * p1;
  const Concrete::PathPoint3D * p2;
  findSegment( globalTime, &t, &p1, &p2 );

  if( t == 0 )
    {
      return RefCountPtr< const Lang::Coords3D >( new Lang::Coords3D( * p1->mid_ ) );
    }
  if( t == 1 )
    {
      return RefCountPtr< const Lang::Coords3D >( new Lang::Coords3D( * p2->mid_ ) );
    }
  
  Concrete::Bezier x0 = p1->mid_->x_.offtype< 0, 3 >( );
  Concrete::Bezier y0 = p1->mid_->y_.offtype< 0, 3 >( );
  Concrete::Bezier z0 = p1->mid_->z_.offtype< 0, 3 >( );
  Concrete::Bezier x1 = p1->front_->x_.offtype< 0, 3 >( );
  Concrete::Bezier y1 = p1->front_->y_.offtype< 0, 3 >( );
  Concrete::Bezier z1 = p1->front_->z_.offtype< 0, 3 >( );
  Concrete::Bezier x2 = p2->rear_->x_.offtype< 0, 3 >( );
  Concrete::Bezier y2 = p2->rear_->y_.offtype< 0, 3 >( );
  Concrete::Bezier z2 = p2->rear_->z_.offtype< 0, 3 >( );
  Concrete::Bezier x3 = p2->mid_->x_.offtype< 0, 3 >( );
  Concrete::Bezier y3 = p2->mid_->y_.offtype< 0, 3 >( );
  Concrete::Bezier z3 = p2->mid_->z_.offtype< 0, 3 >( );

  Concrete::Time tc = Concrete::UNIT_TIME - t; /* complement to t */
  Physical< 0, 3 > k0 =     tc * tc * tc;
  Physical< 0, 3 > k1 = 3 * tc * tc * t;
  Physical< 0, 3 > k2 = 3 * tc * t  * t;
  Physical< 0, 3 > k3 =     t  * t  * t;
  Concrete::Length x = x0 * k0 + x1 * k1 + x2 * k2 + x3 * k3;
  Concrete::Length y = y0 * k0 + y1 * k1 + y2 * k2 + y3 * k3;
  Concrete::Length z = z0 * k0 + z1 * k1 + z2 * k2 + z3 * k3;
  
  return RefCountPtr< const Lang::Coords3D >( new Lang::Coords3D( x, y, z ) );
}

RefCountPtr< const Lang::Length >
Lang::ElementaryPath3D::speed( Concrete::Time globalTime ) const
{
  if( size( ) == 0 )
    {
      throw Exceptions::OutOfRange( "The empty path has no speed at all." );
    }
  Concrete::Time t;
  const Concrete::PathPoint3D * p1;
  const Concrete::PathPoint3D * p2;
  findSegment( globalTime, &t, &p1, &p2 );

  if( t <= Concrete::ZERO_TIME )
    {
      /* here, t = 0, tc == 1, so
       */
      Concrete::Bezier x0 = p1->mid_->x_.offtype< 0, 3 >( );
      Concrete::Bezier y0 = p1->mid_->y_.offtype< 0, 3 >( );
      Concrete::Bezier z0 = p1->mid_->z_.offtype< 0, 3 >( );
      Concrete::Bezier x1 = p1->front_->x_.offtype< 0, 3 >( );
      Concrete::Bezier y1 = p1->front_->y_.offtype< 0, 3 >( );
      Concrete::Bezier z1 = p1->front_->z_.offtype< 0, 3 >( );
      //      Concrete::Speed vx = x0 * (-3) + x1 * 3;
      //      Concrete::Speed vy = y0 * (-3) + y1 * 3;
      //      Concrete::Speed vz = z0 * (-3) + z1 * 3;
      Concrete::Speed vx = ( x1 - x0 ).offtype< 0, -2 >( );
      Concrete::Speed vy = ( y1 - y0 ).offtype< 0, -2 >( );
      Concrete::Speed vz = ( z1 - z0 ).offtype< 0, -2 >( );
      return RefCountPtr< const Lang::Length >( new Lang::Length( hypotPhysical( vx, vy, vz ).offtype< 0, -1 >( ) ) );
    }
  if( t >= Concrete::UNIT_TIME )
    {
      throw Exceptions::InternalError( "t1 == 1 in speed calculation" );
    }
  
  Concrete::Bezier x0 = p1->mid_->x_.offtype< 0, 3 >( );
  Concrete::Bezier y0 = p1->mid_->y_.offtype< 0, 3 >( );
  Concrete::Bezier z0 = p1->mid_->z_.offtype< 0, 3 >( );
  Concrete::Bezier x1 = p1->front_->x_.offtype< 0, 3 >( );
  Concrete::Bezier y1 = p1->front_->y_.offtype< 0, 3 >( );
  Concrete::Bezier z1 = p1->front_->z_.offtype< 0, 3 >( );
  Concrete::Bezier x2 = p2->rear_->x_.offtype< 0, 3 >( );
  Concrete::Bezier y2 = p2->rear_->y_.offtype< 0, 3 >( );
  Concrete::Bezier z2 = p2->rear_->z_.offtype< 0, 3 >( );
  Concrete::Bezier x3 = p2->mid_->x_.offtype< 0, 3 >( );
  Concrete::Bezier y3 = p2->mid_->y_.offtype< 0, 3 >( );
  Concrete::Bezier z3 = p2->mid_->z_.offtype< 0, 3 >( );

  Concrete::Time tc = Concrete::UNIT_TIME - t; /* complement to t */
  //  kv0 = -3 * tc * tc;
  //  kv1 = 3 * tc * tc - 6 * tc * t;
  //  kv2 = 6 * tc * t - 3 * t * t;
  //  kv3 = 3 * t * t;
  Physical< 0, 2 > kv0 = - tc * tc;
  Physical< 0, 2 > kv1 = tc * tc - 2 * tc * t;
  Physical< 0, 2 > kv2 = 2 * tc * t - t * t;
  Physical< 0, 2 > kv3 = t * t;
  Concrete::Speed vx = x0 * kv0 + x1 * kv1 + x2 * kv2 + x3 * kv3;
  Concrete::Speed vy = y0 * kv0 + y1 * kv1 + y2 * kv2 + y3 * kv3;
  Concrete::Speed vz = z0 * kv0 + z1 * kv1 + z2 * kv2 + z3 * kv3;

  return RefCountPtr< const Lang::Length >( new Lang::Length( hypotPhysical( vx, vy, vz ).offtype< 0, -1 >( ) ) );
}

RefCountPtr< const Lang::Length >
Lang::ElementaryPath3D::reverse_speed( Concrete::Time globalTime ) const
{
  if( size( ) == 0 )
    {
      throw Exceptions::OutOfRange( "The empty path has no speed at all." );
    }
  Concrete::Time t;
  const Concrete::PathPoint3D * p1;
  const Concrete::PathPoint3D * p2;
  findSegmentReverse( globalTime, &t, &p1, &p2 );

  if( t <= Concrete::ZERO_TIME )
    {
      throw Exceptions::InternalError( "t1 == 0 in reverse speed calculation" );
    }
  if( t >= Concrete::UNIT_TIME )
    {
      /* here, t = 1, tc == 0, so
       */
      Concrete::Bezier x2 = p2->rear_->x_.offtype< 0, 3 >( );
      Concrete::Bezier y2 = p2->rear_->y_.offtype< 0, 3 >( );
      Concrete::Bezier z2 = p2->rear_->z_.offtype< 0, 3 >( );
      Concrete::Bezier x3 = p2->mid_->x_.offtype< 0, 3 >( );
      Concrete::Bezier y3 = p2->mid_->y_.offtype< 0, 3 >( );
      Concrete::Bezier z3 = p2->mid_->z_.offtype< 0, 3 >( );
      //      Concrete::Speed vx = x2 * (-3) + x3 * 3;
      //      Concrete::Speed vy = y2 * (-3) + y3 * 3;
      //      Concrete::Speed vz = z2 * (-3) + z3 * 3;
      Concrete::Speed vx = ( x3 - x2 ).offtype< 0, -2 >( );
      Concrete::Speed vy = ( y3 - y2 ).offtype< 0, -2 >( );
      Concrete::Speed vz = ( z3 - z2 ).offtype< 0, -2 >( );
      return RefCountPtr< const Lang::Length >( new Lang::Length( hypotPhysical( vx, vy, vz ).offtype< 0, -1 >( ) ) );
    }
  
  Concrete::Bezier x0 = p1->mid_->x_.offtype< 0, 3 >( );
  Concrete::Bezier y0 = p1->mid_->y_.offtype< 0, 3 >( );
  Concrete::Bezier z0 = p1->mid_->z_.offtype< 0, 3 >( );
  Concrete::Bezier x1 = p1->front_->x_.offtype< 0, 3 >( );
  Concrete::Bezier y1 = p1->front_->y_.offtype< 0, 3 >( );
  Concrete::Bezier z1 = p1->front_->z_.offtype< 0, 3 >( );
  Concrete::Bezier x2 = p2->rear_->x_.offtype< 0, 3 >( );
  Concrete::Bezier y2 = p2->rear_->y_.offtype< 0, 3 >( );
  Concrete::Bezier z2 = p2->rear_->z_.offtype< 0, 3 >( );
  Concrete::Bezier x3 = p2->mid_->x_.offtype< 0, 3 >( );
  Concrete::Bezier y3 = p2->mid_->y_.offtype< 0, 3 >( );
  Concrete::Bezier z3 = p2->mid_->z_.offtype< 0, 3 >( );

  Concrete::Time tc = Concrete::UNIT_TIME - t; /* complement to t */
  //  kv0 = -3 * tc * tc;
  //  kv1 = 3 * tc * tc - 6 * tc * t;
  //  kv2 = 6 * tc * t - 3 * t * t;
  //  kv3 = 3 * t * t;
  Physical< 0, 2 > kv0 = - tc * tc;
  Physical< 0, 2 > kv1 = tc * tc - 2 * tc * t;
  Physical< 0, 2 > kv2 = 2 * tc * t - t * t;
  Physical< 0, 2 > kv3 = t * t;
  Concrete::Speed vx = x0 * kv0 + x1 * kv1 + x2 * kv2 + x3 * kv3;
  Concrete::Speed vy = y0 * kv0 + y1 * kv1 + y2 * kv2 + y3 * kv3;
  Concrete::Speed vz = z0 * kv0 + z1 * kv1 + z2 * kv2 + z3 * kv3;

  return RefCountPtr< const Lang::Length >( new Lang::Length( hypotPhysical( vx, vy, vz ).offtype< 0, -1 >( ) ) );
}

RefCountPtr< const Lang::FloatTriple >
Lang::ElementaryPath3D::direction( Concrete::Time globalTime ) const
{
  if( size( ) == 0 )
    {
      throw Exceptions::OutOfRange( "The empty path has no directions at all." );
    }
  Concrete::Time t;
  const Concrete::PathPoint3D * p1;
  const Concrete::PathPoint3D * p2;
  findSegment( globalTime, &t, &p1, &p2 );

  if( t <= Concrete::ZERO_TIME )
    {
      /* here, t = 0, tc == 1, so
       */
      Concrete::Bezier x0 = p1->mid_->x_.offtype< 0, 3 >( );
      Concrete::Bezier y0 = p1->mid_->y_.offtype< 0, 3 >( );
      Concrete::Bezier z0 = p1->mid_->z_.offtype< 0, 3 >( );
      Concrete::Bezier x1 = p1->front_->x_.offtype< 0, 3 >( );
      Concrete::Bezier y1 = p1->front_->y_.offtype< 0, 3 >( );
      Concrete::Bezier z1 = p1->front_->z_.offtype< 0, 3 >( );
      if( x1 != x0 || y1 != y0 || z1 != z0 )
	{
	  Concrete::Speed vx = ( x0 * (-3) + x1 * 3 ).offtype< 0, -2 >( );
	  Concrete::Speed vy = ( y0 * (-3) + y1 * 3 ).offtype< 0, -2 >( );
	  Concrete::Speed vz = ( z0 * (-3) + z1 * 3 ).offtype< 0, -2 >( );
	  Concrete::Speed v = hypotPhysical( vx, vy, vz );
	  if( v == 0 )
	    {
	      return RefCountPtr< const Lang::FloatTriple >( new Lang::FloatTriple( 0, 0, 0 ) );
	    }
	  return RefCountPtr< const Lang::FloatTriple >( new Lang::FloatTriple( vx / v, vy / v, vz / v ) );
	}
      else
	{
	  /* Either there is no control point logically ( p1->front_ == p1->mid_ ), or they just conincide.
	   * Either way, the velocity limit is 0, so to get the direction we must divide by t in the limit
	   * This results in other coefficients:
	   */
	  Concrete::Bezier x2 = p2->rear_->x_.offtype< 0, 3 >( );
	  Concrete::Bezier y2 = p2->rear_->y_.offtype< 0, 3 >( );
	  Concrete::Bezier z2 = p2->rear_->z_.offtype< 0, 3 >( );
	  
	  Concrete::Speed vx = ( x0 * (-6) + x2 * 6 ).offtype< 0, -2 >( );
	  Concrete::Speed vy = ( y0 * (-6) + y2 * 6 ).offtype< 0, -2 >( );
	  Concrete::Speed vz = ( z0 * (-6) + z2 * 6 ).offtype< 0, -2 >( );
	  Concrete::Speed v = hypotPhysical( vx, vy, vz );
	  if( v == 0 )
	    {
	      return RefCountPtr< const Lang::FloatTriple >( new Lang::FloatTriple( 0, 0, 0 ) );
	    }

	  return RefCountPtr< const Lang::FloatTriple >( new Lang::FloatTriple( vx / v, vy / v, vz / v ) );
	}
    }
  if( t >= Concrete::UNIT_TIME )
    {
      throw Exceptions::InternalError( "t1 == 1 in direction calculation" );
    }
  
  Concrete::Bezier x0 = p1->mid_->x_.offtype< 0, 3 >( );
  Concrete::Bezier y0 = p1->mid_->y_.offtype< 0, 3 >( );
  Concrete::Bezier z0 = p1->mid_->z_.offtype< 0, 3 >( );
  Concrete::Bezier x1 = p1->front_->x_.offtype< 0, 3 >( );
  Concrete::Bezier y1 = p1->front_->y_.offtype< 0, 3 >( );
  Concrete::Bezier z1 = p1->front_->z_.offtype< 0, 3 >( );
  Concrete::Bezier x2 = p2->rear_->x_.offtype< 0, 3 >( );
  Concrete::Bezier y2 = p2->rear_->y_.offtype< 0, 3 >( );
  Concrete::Bezier z2 = p2->rear_->z_.offtype< 0, 3 >( );
  Concrete::Bezier x3 = p2->mid_->x_.offtype< 0, 3 >( );
  Concrete::Bezier y3 = p2->mid_->y_.offtype< 0, 3 >( );
  Concrete::Bezier z3 = p2->mid_->z_.offtype< 0, 3 >( );
  
  Concrete::Time tc = Concrete::UNIT_TIME - t; /* complement to t */
  Physical< 0, 2 > kv0 = -3 * tc * tc;
  Physical< 0, 2 > kv1 = 3 * tc * tc - 6 * tc * t;
  Physical< 0, 2 > kv2 = 6 * tc * t - 3 * t * t;
  Physical< 0, 2 > kv3 = 3 * t * t;
  Concrete::Speed vx = x0 * kv0 + x1 * kv1 + x2 * kv2 + x3 * kv3;
  Concrete::Speed vy = y0 * kv0 + y1 * kv1 + y2 * kv2 + y3 * kv3;
  Concrete::Speed vz = z0 * kv0 + z1 * kv1 + z2 * kv2 + z3 * kv3;
  Concrete::Speed v = hypotPhysical( vx, vy, vz );
  if( v == 0 )
    {
      return RefCountPtr< const Lang::FloatTriple >( new Lang::FloatTriple( 0, 0, 0 ) );
    }

  return RefCountPtr< const Lang::FloatTriple >( new Lang::FloatTriple( vx / v, vy / v, vz / v ) );
}

RefCountPtr< const Lang::FloatTriple >
Lang::ElementaryPath3D::reverse_direction( Concrete::Time globalTime ) const
{
  if( size( ) == 0 )
    {
      throw Exceptions::OutOfRange( "The empty path has no directions at all." );
    }
  Concrete::Time t;
  const Concrete::PathPoint3D * p1;
  const Concrete::PathPoint3D * p2;
  findSegmentReverse( globalTime, &t, &p1, &p2 );

  if( t <= Concrete::ZERO_TIME )
    {
      throw Exceptions::InternalError( "t1 == 0 in reverse direction calculation" );
    }
  if( t >= Concrete::UNIT_TIME )
    {
      /* here, t == 1, tc == 0, so
       */
      Concrete::Bezier x2 = p2->rear_->x_.offtype< 0, 3 >( );
      Concrete::Bezier y2 = p2->rear_->y_.offtype< 0, 3 >( );
      Concrete::Bezier z2 = p2->rear_->z_.offtype< 0, 3 >( );
      Concrete::Bezier x3 = p2->mid_->x_.offtype< 0, 3 >( );
      Concrete::Bezier y3 = p2->mid_->y_.offtype< 0, 3 >( );
      Concrete::Bezier z3 = p2->mid_->z_.offtype< 0, 3 >( );
      if( x2 != x3 || y2 != y3 || z2 != z3 )
	{
	  Concrete::Speed vx = ( x2 * (-3) + x3 * 3 ).offtype< 0, -2 >( );
	  Concrete::Speed vy = ( y2 * (-3) + y3 * 3 ).offtype< 0, -2 >( );
	  Concrete::Speed vz = ( z2 * (-3) + z3 * 3 ).offtype< 0, -2 >( );
	  Concrete::Speed v = - hypotPhysical( vx, vy, vz );
	  if( v == 0 )
	    {
	      return RefCountPtr< const Lang::FloatTriple >( new Lang::FloatTriple( 0, 0, 0 ) );
	    }
	  return RefCountPtr< const Lang::FloatTriple >( new Lang::FloatTriple( vx / v, vy / v, vz / v ) );
	}
      else
	{
	  /* Either there is no control point logically ( p2->rear_ == p2->mid_ ), or they just conincide.
	   * Either way, the velocity limit is 0, so to get the direction we must divide by t in the limit
	   * This results in other coefficients:
	   */
	  Concrete::Bezier x1 = p1->front_->x_.offtype< 0, 3 >( );
	  Concrete::Bezier y1 = p1->front_->y_.offtype< 0, 3 >( );
	  Concrete::Bezier z1 = p1->front_->z_.offtype< 0, 3 >( );
	  
	  Concrete::Speed vx = ( x1 * (-6) + x3 * 6 ).offtype< 0, -2 >( );
	  Concrete::Speed vy = ( y1 * (-6) + y3 * 6 ).offtype< 0, -2 >( );
	  Concrete::Speed vz = ( z1 * (-6) + z3 * 6 ).offtype< 0, -2 >( );
	  Concrete::Speed v = - hypotPhysical( vx, vy, vz );
	  if( v == 0 )
	    {
	      return RefCountPtr< const Lang::FloatTriple >( new Lang::FloatTriple( 0, 0, 0 ) );
	    }

	  return RefCountPtr< const Lang::FloatTriple >( new Lang::FloatTriple( vx / v, vy / v, vz / v ) );
	}
    }
  
  Concrete::Bezier x0 = p1->mid_->x_.offtype< 0, 3 >( );
  Concrete::Bezier y0 = p1->mid_->y_.offtype< 0, 3 >( );
  Concrete::Bezier z0 = p1->mid_->z_.offtype< 0, 3 >( );
  Concrete::Bezier x1 = p1->front_->x_.offtype< 0, 3 >( );
  Concrete::Bezier y1 = p1->front_->y_.offtype< 0, 3 >( );
  Concrete::Bezier z1 = p1->front_->z_.offtype< 0, 3 >( );
  Concrete::Bezier x2 = p2->rear_->x_.offtype< 0, 3 >( );
  Concrete::Bezier y2 = p2->rear_->y_.offtype< 0, 3 >( );
  Concrete::Bezier z2 = p2->rear_->z_.offtype< 0, 3 >( );
  Concrete::Bezier x3 = p2->mid_->x_.offtype< 0, 3 >( );
  Concrete::Bezier y3 = p2->mid_->y_.offtype< 0, 3 >( );
  Concrete::Bezier z3 = p2->mid_->z_.offtype< 0, 3 >( );
  
  Concrete::Time tc = Concrete::UNIT_TIME - t; /* complement to t */
  Physical< 0, 2 > kv0 = -3 * tc * tc;
  Physical< 0, 2 > kv1 = 3 * tc * tc - 6 * tc * t;
  Physical< 0, 2 > kv2 = 6 * tc * t - 3 * t * t;
  Physical< 0, 2 > kv3 = 3 * t * t;
  Concrete::Speed vx = x0 * kv0 + x1 * kv1 + x2 * kv2 + x3 * kv3;
  Concrete::Speed vy = y0 * kv0 + y1 * kv1 + y2 * kv2 + y3 * kv3;
  Concrete::Speed vz = z0 * kv0 + z1 * kv1 + z2 * kv2 + z3 * kv3;
  Concrete::Speed v = - hypotPhysical( vx, vy, vz );
  if( v == 0 )
    {
      return RefCountPtr< const Lang::FloatTriple >( new Lang::FloatTriple( 0, 0, 0 ) );
    }

  return RefCountPtr< const Lang::FloatTriple >( new Lang::FloatTriple( vx / v, vy / v, vz / v ) );
}

RefCountPtr< const Lang::FloatTriple >
Lang::ElementaryPath3D::normal( Concrete::Time globalTime ) const
{
  if( size( ) == 0 )
    {
      throw Exceptions::OutOfRange( "The empty path has no normals at all." );
    }
  Concrete::Time t;
  const Concrete::PathPoint3D * p1;
  const Concrete::PathPoint3D * p2;
  findSegment( globalTime, &t, &p1, &p2 );

  Concrete::Bezier x0 = p1->mid_->x_.offtype< 0, 3 >( );
  Concrete::Bezier y0 = p1->mid_->y_.offtype< 0, 3 >( );
  Concrete::Bezier z0 = p1->mid_->z_.offtype< 0, 3 >( );
  Concrete::Bezier x1 = p1->front_->x_.offtype< 0, 3 >( );
  Concrete::Bezier y1 = p1->front_->y_.offtype< 0, 3 >( );
  Concrete::Bezier z1 = p1->front_->z_.offtype< 0, 3 >( );
  Concrete::Bezier x2 = p2->rear_->x_.offtype< 0, 3 >( );
  Concrete::Bezier y2 = p2->rear_->y_.offtype< 0, 3 >( );
  Concrete::Bezier z2 = p2->rear_->z_.offtype< 0, 3 >( );
  Concrete::Bezier x3 = p2->mid_->x_.offtype< 0, 3 >( );
  Concrete::Bezier y3 = p2->mid_->y_.offtype< 0, 3 >( );
  Concrete::Bezier z3 = p2->mid_->z_.offtype< 0, 3 >( );

  if( t <= Concrete::ZERO_TIME )
    {
      /* here, t = 0, tc == 1, so
       */
      Concrete::Speed vx = ( x0 * (-3) + x1 * 3 ).offtype< 0, -2 >( );
      Concrete::Speed vy = ( y0 * (-3) + y1 * 3 ).offtype< 0, -2 >( );
      Concrete::Speed vz = ( z0 * (-3) + z1 * 3 ).offtype< 0, -2 >( );
      Concrete::Acceleration ax = ( x0 * 6 + x1 * (-12) + x2 * 6 ).offtype< 0, -1 >( );
      Concrete::Acceleration ay = ( y0 * 6 + y1 * (-12) + y2 * 6 ).offtype< 0, -1 >( );
      Concrete::Acceleration az = ( z0 * 6 + z1 * (-12) + z2 * 6 ).offtype< 0, -1 >( );

      Concrete::Acceleration tmpx = ax;
      Concrete::Acceleration tmpy = ay;
      Concrete::Acceleration tmpz = az;
      if( tmpx == 0 && tmpy == 0 && tmpz == 0 )
	{
	  // We divide by t and let t approach zero.
	  // To see what the limit becomes, the easiest way is to express the position
	  // as a power series in t, differentiate twice, note what is vanishing, and
	  // then divide by t...
	  Physical< 0, 1 > ka0 = -6;
	  Physical< 0, 1 > ka1 = 18;
	  Physical< 0, 1 > ka2 = -18;
	  Physical< 0, 1 > ka3 = 6;
	  tmpx = x0 * ka0 + x1 * ka1 + x2 * ka2 + x3 * ka3;
	  tmpy = y0 * ka0 + y1 * ka1 + y2 * ka2 + y3 * ka3;
	  tmpz = z0 * ka0 + z1 * ka1 + z2 * ka2 + z3 * ka3;
	}
      if( tmpx == 0 && tmpy == 0 && tmpz == 0 )
	{
	  throw Exceptions::OutOfRange( "The normal is undefined becaue the acceleration vanishes smoothly." );
	}
      Physical< 2, -2 > v2 = squareSum( vx, vy, vz );
      if( v2 > Physical< 2, -2 >( 0 ) )
	{
	  Physical< 0, -1 > f = ( ax * vx + ay * vy + az * vz ) / v2;
	  tmpx -= vx * f;
	  tmpy -= vy * f;
	  tmpz -= vz * f;
	}
      
      Concrete::Acceleration tmpNorm = hypotPhysical( tmpx, tmpy, tmpz );
      if( tmpNorm == 0 )
	{
	  throw Exceptions::OutOfRange( "The normal is undefined where the acceleration is parallel to velocity." );
	}
      return RefCountPtr< const Lang::FloatTriple >( new Lang::FloatTriple( tmpx / tmpNorm, tmpy / tmpNorm, tmpz / tmpNorm ) );
    }
  if( t >= Concrete::UNIT_TIME )
    {
      throw Exceptions::InternalError( "t1 == 1 in normal calculation" );
    }
  
  Concrete::Time tc = Concrete::UNIT_TIME - t; /* complement to t */
  Physical< 0, 2 > kv0 = -3 * tc * tc;
  Physical< 0, 2 > kv1 = 3 * tc * tc - 6 * tc * t;
  Physical< 0, 2 > kv2 = 6 * tc * t - 3 * t * t;
  Physical< 0, 2 > kv3 = 3 * t * t;
  Concrete::Speed vx = x0 * kv0 + x1 * kv1 + x2 * kv2 + x3 * kv3;
  Concrete::Speed vy = y0 * kv0 + y1 * kv1 + y2 * kv2 + y3 * kv3;
  Concrete::Speed vz = z0 * kv0 + z1 * kv1 + z2 * kv2 + z3 * kv3;
  Physical< 0, 1 > ka0 = 6 * tc;
  Physical< 0, 1 > ka1 = -12 * tc + 6 * t;
  Physical< 0, 1 > ka2 = 6 * tc - 12 * t;
  Physical< 0, 1 > ka3 = 6 * t;
  Concrete::Acceleration ax = x0 * ka0 + x1 * ka1 + x2 * ka2 + x3 * ka3;
  Concrete::Acceleration ay = y0 * ka0 + y1 * ka1 + y2 * ka2 + y3 * ka3;
  Concrete::Acceleration az = z0 * ka0 + z1 * ka1 + z2 * ka2 + z3 * ka3;

  Concrete::Acceleration tmpx = ax;
  Concrete::Acceleration tmpy = ay;
  Concrete::Acceleration tmpz = az;
  Physical< 2, -2 > v2 = squareSum( vx, vy, vz );
  if( v2 > Physical< 2, -2 >( 0 ) )
    {
      Physical< 0, -1 > f = ( ax * vx + ay * vy + az * vz ) / v2;
      tmpx -= vx * f;
      tmpy -= vy * f;
      tmpz -= vz * f;
    }

  Physical< 1, -2 > tmpNorm = hypotPhysical( tmpx, tmpy, tmpz );
  if( tmpNorm == Physical< 1, -2 >( 0 ) )
    {
      throw Exceptions::OutOfRange( "The normal is undefined where the acceleration is parallel to velocity." );
    }
  return RefCountPtr< const Lang::FloatTriple >( new Lang::FloatTriple( tmpx / tmpNorm, tmpy / tmpNorm, tmpz / tmpNorm ) );
}

RefCountPtr< const Lang::FloatTriple >
Lang::ElementaryPath3D::reverse_normal( Concrete::Time globalTime ) const
{
  if( size( ) == 0 )
    {
      throw Exceptions::OutOfRange( "The empty path has no normals at all." );
    }
  Concrete::Time t;
  const Concrete::PathPoint3D * p1;
  const Concrete::PathPoint3D * p2;
  findSegmentReverse( globalTime, &t, &p1, &p2 );

  Concrete::Bezier x0 = p1->mid_->x_.offtype< 0, 3 >( );
  Concrete::Bezier y0 = p1->mid_->y_.offtype< 0, 3 >( );
  Concrete::Bezier z0 = p1->mid_->z_.offtype< 0, 3 >( );
  Concrete::Bezier x1 = p1->front_->x_.offtype< 0, 3 >( );
  Concrete::Bezier y1 = p1->front_->y_.offtype< 0, 3 >( );
  Concrete::Bezier z1 = p1->front_->z_.offtype< 0, 3 >( );
  Concrete::Bezier x2 = p2->rear_->x_.offtype< 0, 3 >( );
  Concrete::Bezier y2 = p2->rear_->y_.offtype< 0, 3 >( );
  Concrete::Bezier z2 = p2->rear_->z_.offtype< 0, 3 >( );
  Concrete::Bezier x3 = p2->mid_->x_.offtype< 0, 3 >( );
  Concrete::Bezier y3 = p2->mid_->y_.offtype< 0, 3 >( );
  Concrete::Bezier z3 = p2->mid_->z_.offtype< 0, 3 >( );

  if( t >= Concrete::UNIT_TIME )
    {
      /* here, t = 1, tc == 0, so
       */
      Concrete::Speed vx = ( x2 * (-3) + x3 * 3 ).offtype< 0, -2 >( );
      Concrete::Speed vy = ( y2 * (-3) + y3 * 3 ).offtype< 0, -2 >( );
      Concrete::Speed vz = ( z2 * (-3) + z3 * 3 ).offtype< 0, -2 >( );
      Concrete::Acceleration ax = ( x1 * 6 + x2 * (-12) + x3 * 6 ).offtype< 0, -1 >( );
      Concrete::Acceleration ay = ( y1 * 6 + y2 * (-12) + y3 * 6 ).offtype< 0, -1 >( );
      Concrete::Acceleration az = ( z1 * 6 + z2 * (-12) + z3 * 6 ).offtype< 0, -1 >( );

      Concrete::Acceleration tmpx = ax;
      Concrete::Acceleration tmpy = ay;
      Concrete::Acceleration tmpz = az;
      if( tmpx == 0 && tmpy == 0 && tmpz == 0 )
	{
	  // We divide by (1-t) and let t approach one.
	  // The coefficients are derived via symmetry arguments this time; compare the non-reverse calculation.
	  Physical< 0, 1 > ka0 = 6;
	  Physical< 0, 1 > ka1 = -18;
	  Physical< 0, 1 > ka2 = 18;
	  Physical< 0, 1 > ka3 = -6;
	  tmpx = x0 * ka0 + x1 * ka1 + x2 * ka2 + x3 * ka3;
	  tmpy = y0 * ka0 + y1 * ka1 + y2 * ka2 + y3 * ka3;
	  tmpz = z0 * ka0 + z1 * ka1 + z2 * ka2 + z3 * ka3;
	}
      if( tmpx == 0 && tmpy == 0 && tmpz == 0 )
	{
	  throw Exceptions::OutOfRange( "The normal is undefined becaue the acceleration vanishes smoothly." );
	}
      Physical< 2, -2 > v2 = squareSum( vx, vy, vz );
      if( v2 > Physical< 2, -2 >( 0 ) )
	{
	  Physical< 0, -1 > f = ( ax * vx + ay * vy + az * vz ) / v2;
	  tmpx -= vx * f;
	  tmpy -= vy * f;
	  tmpz -= vz * f;
	}
      
      Concrete::Acceleration tmpNorm = hypotPhysical( tmpx, tmpy, tmpz );
      if( tmpNorm == 0 )
	{
	  throw Exceptions::OutOfRange( "The reverse normal is undefined where the acceleration is parallel to velocity." );
	}
      return RefCountPtr< const Lang::FloatTriple >( new Lang::FloatTriple( tmpx / tmpNorm, tmpy / tmpNorm, tmpz / tmpNorm ) );
    }
  if( t <= Concrete::ZERO_TIME )
    {
      throw Exceptions::InternalError( "t1 == 0 in reverse normal calculation" );
    }
  
  Concrete::Time tc = Concrete::UNIT_TIME - t; /* complement to t */
  Physical< 0, 2 > kv0 = -3 * tc * tc;
  Physical< 0, 2 > kv1 = 3 * tc * tc - 6 * tc * t;
  Physical< 0, 2 > kv2 = 6 * tc * t - 3 * t * t;
  Physical< 0, 2 > kv3 = 3 * t * t;
  Concrete::Speed vx = x0 * kv0 + x1 * kv1 + x2 * kv2 + x3 * kv3;
  Concrete::Speed vy = y0 * kv0 + y1 * kv1 + y2 * kv2 + y3 * kv3;
  Concrete::Speed vz = z0 * kv0 + z1 * kv1 + z2 * kv2 + z3 * kv3;
  Physical< 0, 1 > ka0 = 6 * tc;
  Physical< 0, 1 > ka1 = -12 * tc + 6 * t;
  Physical< 0, 1 > ka2 = 6 * tc - 12 * t;
  Physical< 0, 1 > ka3 = 6 * t;
  Concrete::Acceleration ax = x0 * ka0 + x1 * ka1 + x2 * ka2 + x3 * ka3;
  Concrete::Acceleration ay = y0 * ka0 + y1 * ka1 + y2 * ka2 + y3 * ka3;
  Concrete::Acceleration az = z0 * ka0 + z1 * ka1 + z2 * ka2 + z3 * ka3;

  Concrete::Acceleration tmpx = ax;
  Concrete::Acceleration tmpy = ay;
  Concrete::Acceleration tmpz = az;
  Physical< 2, -2 > v2 = squareSum( vx, vy, vz );
  if( v2 > Physical< 2, -2 >( 0 ) )
    {
      Physical< 0, -1 > f = ( ax * vx + ay * vy + az * vz ) / v2;
      tmpx -= vx * f;
      tmpy -= vy * f;
      tmpz -= vz * f;
    }

  Concrete::Acceleration tmpNorm = hypotPhysical( tmpx, tmpy, tmpz );
  if( tmpNorm == 0 )
    {
      throw Exceptions::OutOfRange( "The reverse normal is undefined where the acceleration is parallel to velocity." );
    }
  return RefCountPtr< const Lang::FloatTriple >( new Lang::FloatTriple( tmpx / tmpNorm, tmpy / tmpNorm, tmpz / tmpNorm ) );
}

RefCountPtr< const Lang::Length >
Lang::ElementaryPath3D::radiusOfCurvature( Concrete::Time globalTime ) const
{
  if( size( ) == 0 )
    {
      throw Exceptions::OutOfRange( "The empty path has no curvature at all." );
    }
  Concrete::Time t;
  const Concrete::PathPoint3D * p1;
  const Concrete::PathPoint3D * p2;
  findSegment( globalTime, &t, &p1, &p2 );

  if( t <= Concrete::ZERO_TIME )
    {
      /* here, t = 0, tc == 1, so
       */
      Concrete::Bezier x0 = p1->mid_->x_.offtype< 0, 3 >( );
      Concrete::Bezier y0 = p1->mid_->y_.offtype< 0, 3 >( );
      Concrete::Bezier z0 = p1->mid_->z_.offtype< 0, 3 >( );
      Concrete::Bezier x1 = p1->front_->x_.offtype< 0, 3 >( );
      Concrete::Bezier y1 = p1->front_->y_.offtype< 0, 3 >( );
      Concrete::Bezier z1 = p1->front_->z_.offtype< 0, 3 >( );
      Concrete::Bezier x2 = p2->rear_->x_.offtype< 0, 3 >( );
      Concrete::Bezier y2 = p2->rear_->y_.offtype< 0, 3 >( );
      Concrete::Bezier z2 = p2->rear_->z_.offtype< 0, 3 >( );
      Concrete::Speed vx = ( x0 * (-3) + x1 * 3 ).offtype< 0, -2 >( );
      Concrete::Speed vy = ( y0 * (-3) + y1 * 3 ).offtype< 0, -2 >( );
      Concrete::Speed vz = ( z0 * (-3) + z1 * 3 ).offtype< 0, -2 >( );
      Concrete::Acceleration ax = ( x0 * 6 + x2 * 6 ).offtype< 0, -1 >( );
      Concrete::Acceleration ay = ( y0 * 6 + y2 * 6 ).offtype< 0, -1 >( );
      Concrete::Acceleration az = ( z0 * 6 + z2 * 6 ).offtype< 0, -1 >( );

      Physical< 2, -3 > denom = hypotPhysical( vy * az - vz * ay, vz * ax - vx * az, vx * ay - vy * ax );
      if( denom == 0 )
	{
	  return RefCountPtr< const Lang::Length >( new Lang::Length( HUGE_VAL ) );
	}
      Physical< 2, -2 > tmp = squareSum( vx, vy, vz );
      return RefCountPtr< const Lang::Length >( new Lang::Length( tmp * sqrtPhysical( tmp ) / denom ) );
    }
  if( t >= Concrete::UNIT_TIME )
    {
      throw Exceptions::InternalError( "t1 == 1 in curvature calculation" );
    }
  
  Concrete::Bezier x0 = p1->mid_->x_.offtype< 0, 3 >( );
  Concrete::Bezier y0 = p1->mid_->y_.offtype< 0, 3 >( );
  Concrete::Bezier z0 = p1->mid_->z_.offtype< 0, 3 >( );
  Concrete::Bezier x1 = p1->front_->x_.offtype< 0, 3 >( );
  Concrete::Bezier y1 = p1->front_->y_.offtype< 0, 3 >( );
  Concrete::Bezier z1 = p1->front_->z_.offtype< 0, 3 >( );
  Concrete::Bezier x2 = p2->rear_->x_.offtype< 0, 3 >( );
  Concrete::Bezier y2 = p2->rear_->y_.offtype< 0, 3 >( );
  Concrete::Bezier z2 = p2->rear_->z_.offtype< 0, 3 >( );
  Concrete::Bezier x3 = p2->mid_->x_.offtype< 0, 3 >( );
  Concrete::Bezier y3 = p2->mid_->y_.offtype< 0, 3 >( );
  Concrete::Bezier z3 = p2->mid_->z_.offtype< 0, 3 >( );
  
  Concrete::Time tc = Concrete::UNIT_TIME - t; /* complement to t */
  Physical< 0, 2 > kv0 = -3 * tc * tc;
  Physical< 0, 2 > kv1 = 3 * tc * tc - 6 * tc * t;
  Physical< 0, 2 > kv2 = 6 * tc * t - 3 * t * t;
  Physical< 0, 2 > kv3 = 3 * t * t;
  Concrete::Speed vx = x0 * kv0 + x1 * kv1 + x2 * kv2 + x3 * kv3;
  Concrete::Speed vy = y0 * kv0 + y1 * kv1 + y2 * kv2 + y3 * kv3;
  Concrete::Speed vz = z0 * kv0 + z1 * kv1 + z2 * kv2 + z3 * kv3;
  Physical< 0, 1 > ka0 = 6 * tc;
  Physical< 0, 1 > ka1 = -12 * tc + 6 * t;
  Physical< 0, 1 > ka2 = 6 * tc - 12 * t;
  Physical< 0, 1 > ka3 = 6 * t;
  Concrete::Acceleration ax = x0 * ka0 + x1 * ka1 + x2 * ka2 + x3 * ka3;
  Concrete::Acceleration ay = y0 * ka0 + y1 * ka1 + y2 * ka2 + y3 * ka3;
  Concrete::Acceleration az = z0 * ka0 + z1 * ka1 + z2 * ka2 + z3 * ka3;

  Physical< 2, -3 > denom = hypotPhysical( vy * az - vz * ay, vz * ax - vx * az, vx * ay - vy * ax );
  if( denom == 0 )
    {
      return RefCountPtr< const Lang::Length >( new Lang::Length( HUGE_VAL ) );
    }
  Physical< 2, -2 > tmp = squareSum( vx, vy, vz );
  return RefCountPtr< const Lang::Length >( new Lang::Length( tmp * sqrtPhysical( tmp ) / denom ) );
}

RefCountPtr< const Lang::Length >
Lang::ElementaryPath3D::reverse_radiusOfCurvature( Concrete::Time globalTime ) const
{
  if( size( ) == 0 )
    {
      throw Exceptions::OutOfRange( "The empty path has no curvature at all." );
    }
  Concrete::Time t;
  const Concrete::PathPoint3D * p1;
  const Concrete::PathPoint3D * p2;
  findSegmentReverse( globalTime, &t, &p1, &p2 );

  if( t <= Concrete::ZERO_TIME )
    {
      throw Exceptions::InternalError( "t1 == 0 in reverse curvature calculation" );
    }
  if( t >= Concrete::UNIT_TIME )
    {
      /* here, t = 1, tc == 0, so
       */
      Concrete::Bezier x1 = p1->front_->x_.offtype< 0, 3 >( );
      Concrete::Bezier y1 = p1->front_->y_.offtype< 0, 3 >( );
      Concrete::Bezier z1 = p1->front_->z_.offtype< 0, 3 >( );
      Concrete::Bezier x2 = p2->rear_->x_.offtype< 0, 3 >( );
      Concrete::Bezier y2 = p2->rear_->y_.offtype< 0, 3 >( );
      Concrete::Bezier z2 = p2->rear_->z_.offtype< 0, 3 >( );
      Concrete::Bezier x3 = p2->mid_->x_.offtype< 0, 3 >( );
      Concrete::Bezier y3 = p2->mid_->y_.offtype< 0, 3 >( );
      Concrete::Bezier z3 = p2->mid_->z_.offtype< 0, 3 >( );
      
      Concrete::Speed vx = ( x2 * (-3) + x3 * 3 ).offtype< 0, -2 >( );
      Concrete::Speed vy = ( y2 * (-3) + y3 * 3 ).offtype< 0, -2 >( );
      Concrete::Speed vz = ( z2 * (-3) + z3 * 3 ).offtype< 0, -2 >( );
      Concrete::Acceleration ax = ( x1 * 6 + x2 * (-12) + x3 * 6 ).offtype< 0, -1 >( );
      Concrete::Acceleration ay = ( y1 * 6 + y2 * (-12) + y3 * 6 ).offtype< 0, -1 >( );
      Concrete::Acceleration az = ( z1 * 6 + z2 * (-12) + z3 * 6 ).offtype< 0, -1 >( );

      Physical< 2, -3 > denom = hypotPhysical( vy * az - vz * ay, vz * ax - vx * az, vx * ay - vy * ax );
      if( denom == 0 )
	{
	  return RefCountPtr< const Lang::Length >( new Lang::Length( HUGE_VAL ) );
	}
      Physical< 2, -2 > tmp = squareSum( vx, vy, vz );
      return RefCountPtr< const Lang::Length >( new Lang::Length( tmp * sqrtPhysical( tmp ) / denom ) );
    }
  
  Concrete::Bezier x0 = p1->mid_->x_.offtype< 0, 3 >( );
  Concrete::Bezier y0 = p1->mid_->y_.offtype< 0, 3 >( );
  Concrete::Bezier z0 = p1->mid_->z_.offtype< 0, 3 >( );
  Concrete::Bezier x1 = p1->front_->x_.offtype< 0, 3 >( );
  Concrete::Bezier y1 = p1->front_->y_.offtype< 0, 3 >( );
  Concrete::Bezier z1 = p1->front_->z_.offtype< 0, 3 >( );
  Concrete::Bezier x2 = p2->rear_->x_.offtype< 0, 3 >( );
  Concrete::Bezier y2 = p2->rear_->y_.offtype< 0, 3 >( );
  Concrete::Bezier z2 = p2->rear_->z_.offtype< 0, 3 >( );
  Concrete::Bezier x3 = p2->mid_->x_.offtype< 0, 3 >( );
  Concrete::Bezier y3 = p2->mid_->y_.offtype< 0, 3 >( );
  Concrete::Bezier z3 = p2->mid_->z_.offtype< 0, 3 >( );
  
  Concrete::Time tc = Concrete::UNIT_TIME - t; /* complement to t */
  Physical< 0, 2 > kv0 = -3 * tc * tc;
  Physical< 0, 2 > kv1 = 3 * tc * tc - 6 * tc * t;
  Physical< 0, 2 > kv2 = 6 * tc * t - 3 * t * t;
  Physical< 0, 2 > kv3 = 3 * t * t;
  Concrete::Speed vx = x0 * kv0 + x1 * kv1 + x2 * kv2 + x3 * kv3;
  Concrete::Speed vy = y0 * kv0 + y1 * kv1 + y2 * kv2 + y3 * kv3;
  Concrete::Speed vz = z0 * kv0 + z1 * kv1 + z2 * kv2 + z3 * kv3;
  Physical< 0, 1 > ka0 = 6 * tc;
  Physical< 0, 1 > ka1 = -12 * tc + 6 * t;
  Physical< 0, 1 > ka2 = 6 * tc - 12 * t;
  Physical< 0, 1 > ka3 = 6 * t;
  Concrete::Acceleration ax = x0 * ka0 + x1 * ka1 + x2 * ka2 + x3 * ka3;
  Concrete::Acceleration ay = y0 * ka0 + y1 * ka1 + y2 * ka2 + y3 * ka3;
  Concrete::Acceleration az = z0 * ka0 + z1 * ka1 + z2 * ka2 + z3 * ka3;

  Physical< 2, -3 > denom = hypotPhysical( vy * az - vz * ay, vz * ax - vx * az, vx * ay - vy * ax );
  if( denom == 0 )
    {
      return RefCountPtr< const Lang::Length >( new Lang::Length( HUGE_VAL ) );
    }
  Physical< 2, -2 > tmp = squareSum( vx, vy, vz );
  return RefCountPtr< const Lang::Length >( new Lang::Length( tmp * sqrtPhysical( tmp ) / denom ) );
}

RefCountPtr< const Lang::Length >
Lang::ElementaryPath3D::radiusOfTorsion( Concrete::Time globalTime ) const
{
  if( size( ) == 0 )
    {
      throw Exceptions::OutOfRange( "The empty path has no torsion at all." );
    }
  Concrete::Time t;
  const Concrete::PathPoint3D * p1;
  const Concrete::PathPoint3D * p2;
  findSegment( globalTime, &t, &p1, &p2 );

  if( t <= Concrete::ZERO_TIME )
    {
      /* here, t = 0, tc == 1, so
       */
      Concrete::Bezier x0 = p1->mid_->x_.offtype< 0, 3 >( );
      Concrete::Bezier y0 = p1->mid_->y_.offtype< 0, 3 >( );
      Concrete::Bezier z0 = p1->mid_->z_.offtype< 0, 3 >( );
      Concrete::Bezier x1 = p1->front_->x_.offtype< 0, 3 >( );
      Concrete::Bezier y1 = p1->front_->y_.offtype< 0, 3 >( );
      Concrete::Bezier z1 = p1->front_->z_.offtype< 0, 3 >( );
      Concrete::Bezier x2 = p2->rear_->x_.offtype< 0, 3 >( );
      Concrete::Bezier y2 = p2->rear_->y_.offtype< 0, 3 >( );
      Concrete::Bezier z2 = p2->rear_->z_.offtype< 0, 3 >( );
      Concrete::Bezier x3 = p2->mid_->x_.offtype< 0, 3 >( );
      Concrete::Bezier y3 = p2->mid_->y_.offtype< 0, 3 >( );
      Concrete::Bezier z3 = p2->mid_->z_.offtype< 0, 3 >( );

      Concrete::Speed vx = ( x0 * (-3) + x1 * 3 ).offtype< 0, -2 >( );
      Concrete::Speed vy = ( y0 * (-3) + y1 * 3 ).offtype< 0, -2 >( );
      Concrete::Speed vz = ( z0 * (-3) + z1 * 3 ).offtype< 0, -2 >( );
      Concrete::Acceleration ax = ( x0 * 6 + x2 * 6 ).offtype< 0, -1 >( );
      Concrete::Acceleration ay = ( y0 * 6 + y2 * 6 ).offtype< 0, -1 >( );
      Concrete::Acceleration az = ( z0 * 6 + z2 * 6 ).offtype< 0, -1 >( );
      Concrete::Jerk jx = x0 * (-6) + x1 * 18 + x2 * (-18) + x3 * 6;
      Concrete::Jerk jy = y0 * (-6) + y1 * 18 + y2 * (-18) + y3 * 6;
      Concrete::Jerk jz = z0 * (-6) + z1 * 18 + z2 * (-18) + z3 * 6;

      Physical< 3, -6 > denom = tripleInner( vx, vy, vz, ax, ay, az, jx, jy, jz );
      if( denom == 0 )
	{
	  return RefCountPtr< const Lang::Length >( new Lang::Length( HUGE_VAL ) );
	}
      Physical< 4, -6 > tmp = squareSum( vy * az - vz * ay, vz * ax - vx * az, vx * ay - vy * ax );
      return RefCountPtr< const Lang::Length >( new Lang::Length( tmp / denom ) );
    }
  if( t >= Concrete::UNIT_TIME )
    {
      throw Exceptions::InternalError( "t1 == 1 in torsion calculation" );
    }
  
  Concrete::Bezier x0 = p1->mid_->x_.offtype< 0, 3 >( );
  Concrete::Bezier y0 = p1->mid_->y_.offtype< 0, 3 >( );
  Concrete::Bezier z0 = p1->mid_->z_.offtype< 0, 3 >( );
  Concrete::Bezier x1 = p1->front_->x_.offtype< 0, 3 >( );
  Concrete::Bezier y1 = p1->front_->y_.offtype< 0, 3 >( );
  Concrete::Bezier z1 = p1->front_->z_.offtype< 0, 3 >( );
  Concrete::Bezier x2 = p2->rear_->x_.offtype< 0, 3 >( );
  Concrete::Bezier y2 = p2->rear_->y_.offtype< 0, 3 >( );
  Concrete::Bezier z2 = p2->rear_->z_.offtype< 0, 3 >( );
  Concrete::Bezier x3 = p2->mid_->x_.offtype< 0, 3 >( );
  Concrete::Bezier y3 = p2->mid_->y_.offtype< 0, 3 >( );
  Concrete::Bezier z3 = p2->mid_->z_.offtype< 0, 3 >( );
  
  Concrete::Time tc = Concrete::UNIT_TIME - t; /* complement to t */
  Physical< 0, 2 > kv0 = -3 * tc * tc;
  Physical< 0, 2 > kv1 = 3 * tc * tc - 6 * tc * t;
  Physical< 0, 2 > kv2 = 6 * tc * t - 3 * t * t;
  Physical< 0, 2 > kv3 = 3 * t * t;
  Concrete::Speed vx = x0 * kv0 + x1 * kv1 + x2 * kv2 + x3 * kv3;
  Concrete::Speed vy = y0 * kv0 + y1 * kv1 + y2 * kv2 + y3 * kv3;
  Concrete::Speed vz = z0 * kv0 + z1 * kv1 + z2 * kv2 + z3 * kv3;
  Physical< 0, 1 > ka0 = 6 * tc;
  Physical< 0, 1 > ka1 = -12 * tc + 6 * t;
  Physical< 0, 1 > ka2 = 6 * tc - 12 * t;
  Physical< 0, 1 > ka3 = 6 * t;
  Concrete::Acceleration ax = x0 * ka0 + x1 * ka1 + x2 * ka2 + x3 * ka3;
  Concrete::Acceleration ay = y0 * ka0 + y1 * ka1 + y2 * ka2 + y3 * ka3;
  Concrete::Acceleration az = z0 * ka0 + z1 * ka1 + z2 * ka2 + z3 * ka3;
  Physical< 0, 0 > kj0 = -6;
  Physical< 0, 0 > kj1 = 18;
  Physical< 0, 0 > kj2 = -18;
  Physical< 0, 0 > kj3 = 6;
  Concrete::Jerk jx = x0 * kj0 + x1 * kj1 + x2 * kj2 + x3 * kj3;
  Concrete::Jerk jy = y0 * kj0 + y1 * kj1 + y2 * kj2 + y3 * kj3;
  Concrete::Jerk jz = z0 * kj0 + z1 * kj1 + z2 * kj2 + z3 * kj3;

  Physical< 3, -6 > denom = tripleInner( vx, vy, vz, ax, ay, az, jx, jy, jz );
  if( denom == 0 )
    {
      return RefCountPtr< const Lang::Length >( new Lang::Length( HUGE_VAL ) );
    }
  Physical< 4, -6 > tmp = squareSum( vy * az - vz * ay, vz * ax - vx * az, vx * ay - vy * ax );
  return RefCountPtr< const Lang::Length >( new Lang::Length( tmp / denom ) );
}

RefCountPtr< const Lang::Length >
Lang::ElementaryPath3D::reverse_radiusOfTorsion( Concrete::Time globalTime ) const
{
  if( size( ) == 0 )
    {
      throw Exceptions::OutOfRange( "The empty path has no torsion at all." );
    }
  Concrete::Time t;
  const Concrete::PathPoint3D * p1;
  const Concrete::PathPoint3D * p2;
  findSegmentReverse( globalTime, &t, &p1, &p2 );

  if( t <= Concrete::ZERO_TIME )
    {
      throw Exceptions::InternalError( "t1 == 0 in reverse torsion calculation" );
    }
  if( t >= Concrete::UNIT_TIME )
    {
      /* here, t = 1, tc == 0, so
       */
      Concrete::Bezier x0 = p1->mid_->x_.offtype< 0, 3 >( );
      Concrete::Bezier y0 = p1->mid_->y_.offtype< 0, 3 >( );
      Concrete::Bezier z0 = p1->mid_->z_.offtype< 0, 3 >( );
      Concrete::Bezier x1 = p1->front_->x_.offtype< 0, 3 >( );
      Concrete::Bezier y1 = p1->front_->y_.offtype< 0, 3 >( );
      Concrete::Bezier z1 = p1->front_->z_.offtype< 0, 3 >( );
      Concrete::Bezier x2 = p2->rear_->x_.offtype< 0, 3 >( );
      Concrete::Bezier y2 = p2->rear_->y_.offtype< 0, 3 >( );
      Concrete::Bezier z2 = p2->rear_->z_.offtype< 0, 3 >( );
      Concrete::Bezier x3 = p2->mid_->x_.offtype< 0, 3 >( );
      Concrete::Bezier y3 = p2->mid_->y_.offtype< 0, 3 >( );
      Concrete::Bezier z3 = p2->mid_->z_.offtype< 0, 3 >( );
      
      Concrete::Speed vx = ( x2 * (-3) + x3 * 3 ).offtype< 0, -2 >( );
      Concrete::Speed vy = ( y2 * (-3) + y3 * 3 ).offtype< 0, -2 >( );
      Concrete::Speed vz = ( z2 * (-3) + z3 * 3 ).offtype< 0, -2 >( );
      Concrete::Acceleration ax = ( x1 * 6 + x2 * (-12) + x3 * 6 ).offtype< 0, -1 >( );
      Concrete::Acceleration ay = ( y1 * 6 + y2 * (-12) + y3 * 6 ).offtype< 0, -1 >( );
      Concrete::Acceleration az = ( z1 * 6 + z2 * (-12) + z3 * 6 ).offtype< 0, -1 >( );
      Concrete::Jerk jx = x0 * (-6) + x1 * 18 + x2 * (-18) + x3 * 6;
      Concrete::Jerk jy = y0 * (-6) + y1 * 18 + y2 * (-18) + y3 * 6;
      Concrete::Jerk jz = z0 * (-6) + z1 * 18 + z2 * (-18) + z3 * 6;

      Physical< 3, -6 > denom = tripleInner( vx, vy, vz, ax, ay, az, jx, jy, jz );
      if( denom == 0 )
	{
	  return RefCountPtr< const Lang::Length >( new Lang::Length( HUGE_VAL ) );
	}
      Physical< 4, -6 > tmp = squareSum( vy * az - vz * ay, vz * ax - vx * az, vx * ay - vy * ax );
      return RefCountPtr< const Lang::Length >( new Lang::Length( tmp / denom ) );
    }
  
  Concrete::Bezier x0 = p1->mid_->x_.offtype< 0, 3 >( );
  Concrete::Bezier y0 = p1->mid_->y_.offtype< 0, 3 >( );
  Concrete::Bezier z0 = p1->mid_->z_.offtype< 0, 3 >( );
  Concrete::Bezier x1 = p1->front_->x_.offtype< 0, 3 >( );
  Concrete::Bezier y1 = p1->front_->y_.offtype< 0, 3 >( );
  Concrete::Bezier z1 = p1->front_->z_.offtype< 0, 3 >( );
  Concrete::Bezier x2 = p2->rear_->x_.offtype< 0, 3 >( );
  Concrete::Bezier y2 = p2->rear_->y_.offtype< 0, 3 >( );
  Concrete::Bezier z2 = p2->rear_->z_.offtype< 0, 3 >( );
  Concrete::Bezier x3 = p2->mid_->x_.offtype< 0, 3 >( );
  Concrete::Bezier y3 = p2->mid_->y_.offtype< 0, 3 >( );
  Concrete::Bezier z3 = p2->mid_->z_.offtype< 0, 3 >( );
  
  Concrete::Time tc = Concrete::UNIT_TIME - t; /* complement to t */
  Physical< 0, 2 > kv0 = -3 * tc * tc;
  Physical< 0, 2 > kv1 = 3 * tc * tc - 6 * tc * t;
  Physical< 0, 2 > kv2 = 6 * tc * t - 3 * t * t;
  Physical< 0, 2 > kv3 = 3 * t * t;
  Concrete::Speed vx = x0 * kv0 + x1 * kv1 + x2 * kv2 + x3 * kv3;
  Concrete::Speed vy = y0 * kv0 + y1 * kv1 + y2 * kv2 + y3 * kv3;
  Concrete::Speed vz = z0 * kv0 + z1 * kv1 + z2 * kv2 + z3 * kv3;
  Physical< 0, 1 > ka0 = 6 * tc;
  Physical< 0, 1 > ka1 = -12 * tc + 6 * t;
  Physical< 0, 1 > ka2 = 6 * tc - 12 * t;
  Physical< 0, 1 > ka3 = 6 * t;
  Concrete::Acceleration ax = x0 * ka0 + x1 * ka1 + x2 * ka2 + x3 * ka3;
  Concrete::Acceleration ay = y0 * ka0 + y1 * ka1 + y2 * ka2 + y3 * ka3;
  Concrete::Acceleration az = z0 * ka0 + z1 * ka1 + z2 * ka2 + z3 * ka3;
  Physical< 0, 0 > kj0 = -6;
  Physical< 0, 0 > kj1 = 18;
  Physical< 0, 0 > kj2 = -18;
  Physical< 0, 0 > kj3 = 6;
  Concrete::Jerk jx = x0 * kj0 + x1 * kj1 + x2 * kj2 + x3 * kj3;
  Concrete::Jerk jy = y0 * kj0 + y1 * kj1 + y2 * kj2 + y3 * kj3;
  Concrete::Jerk jz = z0 * kj0 + z1 * kj1 + z2 * kj2 + z3 * kj3;

  Physical< 3, -6 > denom = tripleInner( vx, vy, vz, ax, ay, az, jx, jy, jz );
  if( denom == 0 )
    {
      return RefCountPtr< const Lang::Length >( new Lang::Length( HUGE_VAL ) );
    }
  Physical< 4, -6 > tmp = squareSum( vy * az - vz * ay, vz * ax - vx * az, vx * ay - vy * ax );
  return RefCountPtr< const Lang::Length >( new Lang::Length( tmp / denom ) );
}

size_t
Lang::ElementaryPath3D::duration( ) const
{
  if( isClosed( ) )
    {
      return size( );
    }
  return size( ) - 1;
}

bool
Lang::ElementaryPath3D::controllingMaximizer( const Lang::FloatTriple & d, Lang::Coords3D * dst ) const
{
  if( size( ) == 0 )
    {
      throw Exceptions::OutOfRange( "The empty path cannot be maximized along." );
    }
  Concrete::Length opt = -HUGE_VAL;
  for( const_iterator i = begin( ); i != end( ); ++i )
    {
      if( (*i)->rear_ != 0 )
	{
	  Concrete::Length y = (*i)->rear_->x_ * d.x_ + (*i)->rear_->y_ * d.y_ + (*i)->rear_->z_ * d.z_;
	  if( y > opt )
	    {
	      opt = y;
	      *dst = *((*i)->rear_);
	    }
	}
      Concrete::Length y = (*i)->mid_->x_ * d.x_ + (*i)->mid_->y_ * d.y_ + (*i)->mid_->z_ * d.z_;
      if( y > opt )
	{
	  opt = y;
	  *dst = *((*i)->mid_);
	}
      if( (*i)->front_ != 0 )
	{
	  Concrete::Length y = (*i)->front_->x_ * d.x_ + (*i)->front_->y_ * d.y_ + (*i)->front_->z_ * d.z_;
	  if( y > opt )
	    {
	      opt = y;
	      *dst = *((*i)->front_);
	    }
	}
    }
  if( opt == -HUGE_VAL )
    {
      return false;
    }
  return true;
}


RefCountPtr< const Lang::Coords3D >
Lang::ElementaryPath3D::discreteMean( ) const
{
  double count = 0;
  Concrete::Length x = 0;
  Concrete::Length y = 0;
  Concrete::Length z = 0;
  if( size( ) == 0 )
    {
      throw Exceptions::OutOfRange( "The empty path cannot be averaged along." );
    }
  for( const_iterator i = begin( ); i != end( ); ++i )
    {
      if( (*i)->rear_ != 0 )
	{
	  ++count;
	  x = x + (*i)->rear_->x_;
	  y = y + (*i)->rear_->y_;
	  z = z + (*i)->rear_->z_;
	}
      ++count;
      x = x + (*i)->mid_->x_;
      y = y + (*i)->mid_->y_;
      z = z + (*i)->mid_->z_;
      if( (*i)->front_ != 0 )
	{
	  ++count;
	  x = x + (*i)->front_->x_;
	  y = y + (*i)->front_->y_;
	  z = z + (*i)->front_->z_;
	}
    }
  return RefCountPtr< const Lang::Coords3D >( new Lang::Coords3D( x / count, y / count, z / count ) );
}

RefCountPtr< const Lang::Coords3D >
Lang::ElementaryPath3D::controllingMaximizer( const Lang::FloatTriple & d ) const
{
  Lang::Coords3D * res = new Lang::Coords3D( Concrete::ZERO_LENGTH, Concrete::ZERO_LENGTH, Concrete::ZERO_LENGTH );
  controllingMaximizer( d, res );
  return RefCountPtr< const Lang::Coords3D >( res );
}

Concrete::SplineTime
Lang::ElementaryPath3D::discreteMaximizer( const Lang::FloatTriple & d ) const
{
  if( size( ) == 0 )
    {
      throw Exceptions::OutOfRange( "The empty path cannot be maximized along." );
    }
  Concrete::Length opt = -HUGE_VAL;
  const double dx = d.x_;
  const double dy = d.y_;
  const double dz = d.z_;
  Concrete::SplineTime res = Concrete::ZERO_TIME;
  Concrete::SplineTime t = Concrete::ZERO_TIME;
  for( const_iterator i = begin( ); i != end( ); ++i, ++t )
    {
      Concrete::Length y = (*i)->mid_->x_ * dx + (*i)->mid_->y_ * dy + (*i)->mid_->z_ * dz;
      if( y > opt )
	{
	  opt = y;
	  res = t;
	}
    }
  return res;
}

Concrete::SplineTime
Lang::ElementaryPath3D::discreteApproximator( const Lang::Coords3D & p ) const
{
  if( size( ) == 0 )
    {
      throw Exceptions::OutOfRange( "The empty path cannot be maximized along." );
    }
  Concrete::Length bestDist = HUGE_VAL;
  const Concrete::Length px = p.x_.get( );
  const Concrete::Length py = p.y_.get( );
  const Concrete::Length pz = p.z_.get( );
  Concrete::SplineTime res = Concrete::ZERO_TIME;
  Concrete::SplineTime t = Concrete::ZERO_TIME;
  for( const_iterator i = begin( ); i != end( ); ++i, ++t )
    {
      Concrete::Length dist = hypotPhysical( (*i)->mid_->x_ - px, (*i)->mid_->y_ - py, (*i)->mid_->z_ - pz );
      if( dist < bestDist )
	{
	  bestDist = dist;
	  res = t;
	}
    }
  return res;
}

RefCountPtr< const Lang::Coords3D >
Lang::ElementaryPath3D::continuousMean( ) const
{
  if( size( ) == 0 )
    {
      throw Exceptions::OutOfRange( "The empty path cannot be averaged along." );
    }
  Physical< 2, 0 > intx = 0;
  Physical< 2, 0 > inty = 0;
  Physical< 2, 0 > intz = 0;
  Physical< 1, 0 > totlen = 0;

  const_iterator i1 = begin( );
  const_iterator i2 = i1;
  ++i2;
  for( ; i1 != end( ); ++i1, ++i2 )
    {
      if( i2 == end( ) )
	{
	  if( closed_ )
	    {
	      i2 = begin( );
	    }
	  else
	    {
	      break;
	    }
	}

      if( (*i1)->front_ == (*i1)->mid_ &&
	  (*i2)->rear_ == (*i2)->mid_ )
	{
	  Concrete::Length x0 = (*i1)->mid_->x_;
	  Concrete::Length y0 = (*i1)->mid_->y_;
	  Concrete::Length z0 = (*i1)->mid_->z_;
	  Concrete::Length x3 = (*i2)->mid_->x_;
	  Concrete::Length y3 = (*i2)->mid_->y_;
	  Concrete::Length z3 = (*i2)->mid_->z_;
	  Concrete::Length len = hypotPhysical( (x3-x0), (y3-y0), (z3-z0) );
	  intx += len * 0.5 * ( x0 + x3 );
	  inty += len * 0.5 * ( y0 + y3 );
	  intz += len * 0.5 * ( z0 + z3 );
	  totlen += len;
	  continue;
	}

      Concrete::Bezier x0 = (*i1)->mid_->x_.offtype< 0, 3 >( );
      Concrete::Bezier y0 = (*i1)->mid_->y_.offtype< 0, 3 >( );
      Concrete::Bezier z0 = (*i1)->mid_->z_.offtype< 0, 3 >( );
      Concrete::Bezier x1 = (*i1)->front_->x_.offtype< 0, 3 >( );
      Concrete::Bezier y1 = (*i1)->front_->y_.offtype< 0, 3 >( );
      Concrete::Bezier z1 = (*i1)->front_->z_.offtype< 0, 3 >( );
      Concrete::Bezier x2 = (*i2)->rear_->x_.offtype< 0, 3 >( );
      Concrete::Bezier y2 = (*i2)->rear_->y_.offtype< 0, 3 >( );
      Concrete::Bezier z2 = (*i2)->rear_->z_.offtype< 0, 3 >( );
      Concrete::Bezier x3 = (*i2)->mid_->x_.offtype< 0, 3 >( );
      Concrete::Bezier y3 = (*i2)->mid_->y_.offtype< 0, 3 >( );
      Concrete::Bezier z3 = (*i2)->mid_->z_.offtype< 0, 3 >( );

      Concrete::Time dt = MetaPDF::computeDt( hypotPhysical( (x1-x0), (y1-y0), (z1-z0) ).offtype< 0, -3 >( ) +
						       hypotPhysical( (x2-x1), (y2-y1), (z2-z1) ).offtype< 0, -3 >( ) +
						       hypotPhysical( (x3-x2), (y3-y2), (z3-z2) ).offtype< 0, -3 >( ) );

      Physical< 2, 0 > tmpSum_x( 0 );
      Physical< 2, 0 > tmpSum_y( 0 );
      Physical< 2, 0 > tmpSum_z( 0 );
      Physical< 1, 0 > tmpSum_l( 0 );
      for( Concrete::Time t = 0; t < 1; t += dt )
	{
	  Concrete::Time tc = Concrete::UNIT_TIME - t; /* complement to t */
	  Physical< 0, 3 > k0 =     tc * tc * tc;
	  Physical< 0, 3 > k1 = 3 * tc * tc * t;
	  Physical< 0, 3 > k2 = 3 * tc * t  * t;
	  Physical< 0, 3 > k3 =     t  * t  * t;
	  Concrete::Length x = x0 * k0 + x1 * k1 + x2 * k2 + x3 * k3;
	  Concrete::Length y = y0 * k0 + y1 * k1 + y2 * k2 + y3 * k3;
	  Concrete::Length z = z0 * k0 + z1 * k1 + z2 * k2 + z3 * k3;
	  Physical< 0, 2 > kv0 = -3 * tc * tc;
	  Physical< 0, 2 > kv1 = 3 * tc * tc - 6 * tc * t;
	  Physical< 0, 2 > kv2 = 6 * tc * t - 3 * t * t;
	  Physical< 0, 2 > kv3 = 3 * t * t;
	  Concrete::Speed vx = x0 * kv0 + x1 * kv1 + x2 * kv2 + x3 * kv3;
	  Concrete::Speed vy = y0 * kv0 + y1 * kv1 + y2 * kv2 + y3 * kv3;
	  Concrete::Speed vz = z0 * kv0 + z1 * kv1 + z2 * kv2 + z3 * kv3;

	  Concrete::Length dl = hypotPhysical( vx, vy, vz ).offtype< 0, -1 >( );
	  tmpSum_x += x * dl;
	  tmpSum_y += y * dl;
	  tmpSum_z += z * dl;
	  tmpSum_l += dl;
	}
      intx += tmpSum_x * dt.offtype< 0, 1 >( );
      inty += tmpSum_y * dt.offtype< 0, 1 >( );
      intz += tmpSum_z * dt.offtype< 0, 1 >( );
      totlen += tmpSum_l * dt.offtype< 0, 1 >( );
    }

  return RefCountPtr< const Lang::Coords3D >( new Lang::Coords3D( intx / totlen, inty / totlen, intz / totlen ) );
}

Concrete::SplineTime
Lang::ElementaryPath3D::continuousMaximizer( const Lang::FloatTriple & d ) const
{
  if( size( ) == 0 )
    {
      throw Exceptions::OutOfRange( "The empty path cannot be maximized along." );
    }
  Concrete::Time res = 0;
  Concrete::Length opt = -Concrete::HUGE_LENGTH;
  Concrete::Time steps = 0;
  const_iterator i1 = begin( );
  const_iterator i2 = i1;
  ++i2;
  for( ; i1 != end( ); ++i1, ++i2, steps += Concrete::UNIT_TIME )
    {
      if( i2 == end( ) )
	{
	  if( closed_ )
	    {
	      i2 = begin( );
	    }
	  else
	    {
	      Concrete::Length v = (*i1)->mid_->x_ * d.x_ + (*i1)->mid_->y_ * d.y_ + (*i1)->mid_->z_ * d.z_;
	      if( v > opt )
		{
		  opt = v;
		  res = steps;
		}
	      break;
	    }
	}
      Concrete::Length v = (*i1)->mid_->x_ * d.x_ + (*i1)->mid_->y_ * d.y_ + (*i1)->mid_->z_ * d.z_;
      if( v > opt )
	{
	  opt = v;
	  res = steps;
	}

      /* if the segment is straight, checking its end points is enough.
       */
      if( (*i1)->front_ == (*i1)->mid_ &&
	  (*i2)->rear_ == (*i2)->mid_ )
	{
	  continue;
	}

      Concrete::Bezier x0 = (*i1)->mid_->x_.offtype< 0, 3 >( );
      Concrete::Bezier y0 = (*i1)->mid_->y_.offtype< 0, 3 >( );
      Concrete::Bezier z0 = (*i1)->mid_->z_.offtype< 0, 3 >( );
      Concrete::Bezier x1 = (*i1)->front_->x_.offtype< 0, 3 >( );
      Concrete::Bezier y1 = (*i1)->front_->y_.offtype< 0, 3 >( );
      Concrete::Bezier z1 = (*i1)->front_->z_.offtype< 0, 3 >( );
      Concrete::Bezier x2 = (*i2)->rear_->x_.offtype< 0, 3 >( );
      Concrete::Bezier y2 = (*i2)->rear_->y_.offtype< 0, 3 >( );
      Concrete::Bezier z2 = (*i2)->rear_->z_.offtype< 0, 3 >( );
      Concrete::Bezier x3 = (*i2)->mid_->x_.offtype< 0, 3 >( );
      Concrete::Bezier y3 = (*i2)->mid_->y_.offtype< 0, 3 >( );
      Concrete::Bezier z3 = (*i2)->mid_->z_.offtype< 0, 3 >( );

      /* First check the control points.  If none of the four control points improves the optimum,
       * searching further is meaningless.  Further, since the mid_ points are allways tested anyway,
       * we can assume that they already are, and conclude that they cannot improve the optimum further.
       * Therefore, there are only the two handles to check.
       */
      {
	if( ( x1 * d.x_ + y1 * d.y_ + z1 * d.z_ ).offtype< 0, -3 >( ) <= opt &&
	    ( x2 * d.x_ + y2 * d.y_ + z2 * d.z_ ).offtype< 0, -3 >( ) <= opt )
	  {
	    continue;
	  }
      }

      /* This is the derivative polynomial.  We use it to see if there is
       * any local optimizers in the interval ( 0, 1 )
       */
      double kd0 = ( 3 * ( d.x_ * ( -x0 + x1 ) +
			   d.y_ * ( -y0 + y1 ) ) ).offtype< 1, -3 >( );
      double kd1 = ( 6 * ( d.x_ * ( x0 - 2 * x1 + x2 ) +
			   d.y_ * ( y0 - 2 * y1 + y2 ) ) ).offtype< 1, -3 >( );
      double kd2 = ( 3 * ( d.x_ * ( -x0 + 3 * x1 - 3 * x2 + x3 ) + 
			   d.y_ * ( -y0 + 3 * y1 - 3 * y2 + y3 ) ) ).offtype< 1, -3 >( );
      Concrete::Time t1( -1 );
      Concrete::Time t2( -1 );
      if( kd2 == 0 )
	{
	  if( kd1 != 0 )
	    {
	      t1 = -kd0 / kd1;
	    }
	}
      else
	{
	  kd0 /= kd2;
	  kd1 /= kd2;
	  double r2 = kd1 * kd1 * 0.25 - kd0;
	  if( r2 >= 0 )
	    {
	      double r = sqrt( r2 );
	      t1 = Concrete::Time( - 0.5 * kd1 - r );
	      t2 = Concrete::Time( - 0.5 * kd1 + r );
	    }
	}
      
      if( Concrete::ZERO_TIME < t1 && t1 < Concrete::UNIT_TIME )
	{
	  Concrete::Time t = t1;
	  Concrete::Time tc = Concrete::UNIT_TIME - t; /* complement to t */
	  Physical< 0, 3 > k0 =     tc * tc * tc;
	  Physical< 0, 3 > k1 = 3 * tc * tc * t;
	  Physical< 0, 3 > k2 = 3 * tc * t  * t;
	  Physical< 0, 3 > k3 =     t  * t  * t;
	  Concrete::Length x = x0 * k0 + x1 * k1 + x2 * k2 + x3 * k3;
	  Concrete::Length y = y0 * k0 + y1 * k1 + y2 * k2 + y3 * k3;
	  Concrete::Length z = z0 * k0 + z1 * k1 + z2 * k2 + z3 * k3;
	  Concrete::Length v = x * d.x_ + y * d.y_ + z * d.z_;
	  if( v > opt )
	    {
	      opt = v;
	      res = steps + t;
	    }
	}
      if( Concrete::ZERO_TIME < t2 && t2 < Concrete::UNIT_TIME )
	{
	  Concrete::Time t = t2;
	  Concrete::Time tc = Concrete::UNIT_TIME - t; /* complement to t */
	  Physical< 0, 3 > k0 =     tc * tc * tc;
	  Physical< 0, 3 > k1 = 3 * tc * tc * t;
	  Physical< 0, 3 > k2 = 3 * tc * t  * t;
	  Physical< 0, 3 > k3 =     t  * t  * t;
	  Concrete::Length x = x0 * k0 + x1 * k1 + x2 * k2 + x3 * k3;
	  Concrete::Length y = y0 * k0 + y1 * k1 + y2 * k2 + y3 * k3;
	  Concrete::Length z = z0 * k0 + z1 * k1 + z2 * k2 + z3 * k3;
	  Concrete::Length v = x * d.x_ + y * d.y_ + z * d.z_;
	  if( v > opt )
	    {
	      opt = v;
	      res = steps + t;
	    }
	}
    }
  /*
    if( opt == -HUGE_VAL )
    {
    }
  */

  return res;
}

namespace MetaPDF
{
  class ApproximationPoly3D
  {
    Concrete::Coords3D p_;
    Bezier::PolyCoeffs< Concrete::Coords3D > polyCoeffs_;
    Physical< 1, 0 > kxD0_0_;
    Physical< 1, -1 > kxD0_1_;
    Physical< 1, -2 > kxD0_2_;
    Physical< 1, -3 > kxD0_3_;
    Physical< 1, -1 > kxD1_0_;
    Physical< 1, -2 > kxD1_1_;
    Physical< 1, -3 > kxD1_2_;
    Physical< 1, -2 > kxD2_0_;
    Physical< 1, -3 > kxD2_1_;

    Physical< 1, 0 > kyD0_0_;
    Physical< 1, -1 > kyD0_1_;
    Physical< 1, -2 > kyD0_2_;
    Physical< 1, -3 > kyD0_3_;
    Physical< 1, -1 > kyD1_0_;
    Physical< 1, -2 > kyD1_1_;
    Physical< 1, -3 > kyD1_2_;
    Physical< 1, -2 > kyD2_0_;
    Physical< 1, -3 > kyD2_1_;

    Physical< 1, 0 > kzD0_0_;
    Physical< 1, -1 > kzD0_1_;
    Physical< 1, -2 > kzD0_2_;
    Physical< 1, -3 > kzD0_3_;
    Physical< 1, -1 > kzD1_0_;
    Physical< 1, -2 > kzD1_1_;
    Physical< 1, -3 > kzD1_2_;
    Physical< 1, -2 > kzD2_0_;
    Physical< 1, -3 > kzD2_1_;

  public:
    ApproximationPoly3D( const Concrete::Coords3D & p0, const Concrete::Coords3D & p1, const Concrete::Coords3D & p2, const Concrete::Coords3D & p3, const Concrete::Coords3D & _p );
    Concrete::Time splitTime( const Concrete::Time t_low, const Concrete::Time t_high, const Concrete::Time t_tol ) const;
    bool isCircular( ) const;
    Concrete::Length distanceAt( Concrete::Time t ) const;
    Physical< 2, 0 > squaredDistanceAt( Concrete::Time t ) const;
    Bezier::ControlPoints< Concrete::Coords3D > getControls( const Concrete::Time t_low, const Concrete::Time t_high ) const;
    const Concrete::Coords3D & getPoint( ) const;
  };
  class ApproximationSegmentSection3D
  {
    Bezier::ControlPoints< Concrete::Coords3D > controls_;
    const ApproximationPoly3D * baseSeg_;
    Concrete::Time steps_;
    Concrete::Time t0_;
    Concrete::Time t1_;
  public:
    
    ApproximationSegmentSection3D( const MetaPDF::ApproximationPoly3D * baseSeg, Concrete::Time steps, Concrete::Time t0, Concrete::Time t1 );
    ApproximationSegmentSection3D * cutAfter( Concrete::Time t ) const;
    ApproximationSegmentSection3D * cutBefore( Concrete::Time t ) const;
    
    Concrete::Length convexHullDistance( ) const;
    Concrete::Time splitTime( const Concrete::Time t_tol ) const;
    Concrete::Time globalTime( Concrete::Time t ) const;
    Concrete::Length distanceAt( Concrete::Time t ) const;
    Concrete::Speed maxSpeed( ) const;
    Concrete::Time duration( ) const;
  private:
    /* Argument points must be ordered so that points[0] is the point on the inside side of the plane containing the triangle.
     */
    static void triangleDistance( std::vector< const Concrete::Coords3D * > & points, const Concrete::Coords3D & p, Concrete::Length * bestDist );
  };
}

Concrete::SplineTime
Lang::ElementaryPath3D::continuousApproximator( const Lang::Coords3D & _p ) const
{
  const Concrete::Length DISTANCE_TOL = 0.001 * Computation::the_arcdelta;

  if( size( ) == 0 )
    {
      throw Exceptions::OutOfRange( "The empty path cannot be approximated along." );
    }

  Concrete::Coords3D p( _p.x_.get( ), _p.y_.get( ), _p.z_.get( ) );
  PtrOwner_back_Access< std::list< const MetaPDF::ApproximationPoly3D * > > memory;
  typedef std::pair< MetaPDF::ApproximationSegmentSection3D *, Concrete::Length > WorkItem;
  std::list< WorkItem > work;

  Concrete::Time res = 0;
  Concrete::Length bestDist = HUGE_VAL;
  Concrete::Time steps = 0;
  const_iterator i1 = begin( );
  const_iterator i2 = i1;
  ++i2;
  for( ; i1 != end( ); ++i1, ++i2, steps += Concrete::UNIT_TIME )
    {
      if( i2 == end( ) )
	{
	  if( closed_ )
	    {
	      i2 = begin( );
	    }
	  else
	    {
	      Concrete::Length dist = hypotPhysical( (*i1)->mid_->x_ - p.x_, (*i1)->mid_->y_ - p.y_, (*i1)->mid_->z_ - p.z_ );
	      if( dist < bestDist )
		{
		  bestDist = dist;
		  res = steps;
		}
	      break;
	    }
	}

      Concrete::Length dist = hypotPhysical( (*i1)->mid_->x_ - p.x_, (*i1)->mid_->y_ - p.y_, (*i1)->mid_->z_ - p.z_ );
      if( dist < bestDist )
	{
	  bestDist = dist;
	  res = steps;
	}

      Concrete::Bezier x0 = (*i1)->mid_->x_.offtype< 0, 3 >( );
      Concrete::Bezier y0 = (*i1)->mid_->y_.offtype< 0, 3 >( );
      Concrete::Bezier z0 = (*i1)->mid_->z_.offtype< 0, 3 >( );
      Concrete::Bezier x3 = (*i2)->mid_->x_.offtype< 0, 3 >( );
      Concrete::Bezier y3 = (*i2)->mid_->y_.offtype< 0, 3 >( );
      Concrete::Bezier z3 = (*i2)->mid_->z_.offtype< 0, 3 >( );

      /* if the segment is straight, finding the closest point is a linear least-squares problem.
       */
      if( (*i1)->front_ == (*i1)->mid_ &&
	  (*i2)->rear_ == (*i2)->mid_ )
	{
	  const Concrete::Length tx = ( x3 - x0 ).offtype< 0, -3 >( );
	  const Concrete::Length ty = ( y3 - y0 ).offtype< 0, -3 >( );
	  const Concrete::Length tz = ( z3 - z0 ).offtype< 0, -3 >( );
	  double s = ( tx * ( p.x_ - x0.offtype< 0, -3 >( ) ) + ty * ( p.y_ - y0.offtype< 0, -3 >( ) ) + tz * ( p.z_ - z0.offtype< 0, -3 >( ) ) ) / ( tx*tx + ty*ty + tz*tz );
	  if( 0 < s && s < 1 )
	    {
	      Concrete::Length dist = hypotPhysical( x0.offtype< 0, -3 >( ) + s * tx - p.x_, y0.offtype< 0, -3 >( ) + s * ty - p.y_, z0.offtype< 0, -3 >( ) + s * tz - p.z_ );
	      if( dist < bestDist )
		{
		  bestDist = dist;
		  res = steps + MetaPDF::straightLineArcTime( s );
		}
	    }
	  continue;
	}

      MetaPDF::ApproximationPoly3D * coeffs =
	new MetaPDF::ApproximationPoly3D( *((*i1)->mid_), *((*i1)->front_), *((*i2)->rear_), *((*i2)->mid_), p );
      MetaPDF::ApproximationSegmentSection3D * seg = new MetaPDF::ApproximationSegmentSection3D( coeffs, steps, 0, 1 );
      Concrete::Length lowerBound = seg->convexHullDistance( ) + DISTANCE_TOL;
      if( lowerBound >= bestDist )
	{
	  delete seg;
	  delete coeffs;
	  continue;
	}

      /* Here, we must detect the ill-contditionned case of center-to-circle.  This is detected
       * by checking the distance at four different points.
       */
      {
	if( coeffs->isCircular( ) )
	  {
	    Concrete::Length dist = coeffs->distanceAt( 0 );
	    if( dist < bestDist )
	      {
		  bestDist = dist;
		  res = steps;
	      }
	    delete seg;
	    delete coeffs;
	    continue;
	  }
      }

      memory.push_back( coeffs );
      work.push_back( WorkItem( seg, lowerBound ) );
    }

  while( work.size( ) > 0 )
    {
      MetaPDF::ApproximationSegmentSection3D * seg = work.front( ).first;
      Concrete::Length lowerBound = work.front( ).second;
      work.pop_front( );
      if( lowerBound < bestDist )
	{
	  const Concrete::Time t_tol = Computation::the_arcdelta / seg->maxSpeed( );
	  Concrete::Time split = seg->splitTime( 0.001 * t_tol );  // increased precicion seems to have no influence on computation time
	  Concrete::Length dist = seg->distanceAt( split );
	  if( dist < bestDist )
	    {
	      bestDist = dist;
	      res = seg->globalTime( split );
	    }
	  if( lowerBound < bestDist &&
	      t_tol < seg->duration( ) )
	    {
	      {
		MetaPDF::ApproximationSegmentSection3D * part = seg->cutAfter( split );
		Concrete::Length lowerBound = part->convexHullDistance( ) + DISTANCE_TOL;
		if( lowerBound >= bestDist )
		  {
		    delete part;
		  }
		else
		  {
		    work.push_back( WorkItem( part, lowerBound ) );
		  }
	      }
	      {
		MetaPDF::ApproximationSegmentSection3D * part = seg->cutBefore( split );
		Concrete::Length lowerBound = part->convexHullDistance( ) + DISTANCE_TOL;
		if( lowerBound >= bestDist )
		  {
		    delete part;
		  }
		else
		  {
		    work.push_back( WorkItem( part, lowerBound ) );
		  }	    
	      }
	    }
	}
      delete seg;
    }

  return res;
}

MetaPDF::ApproximationPoly3D::ApproximationPoly3D( const Concrete::Coords3D & p0, const Concrete::Coords3D & p1, const Concrete::Coords3D & p2, const Concrete::Coords3D & p3, const Concrete::Coords3D & p )
  : p_( p ), polyCoeffs_( Bezier::PolyCoeffs< Concrete::Coords3D >( Bezier::ControlPoints< Concrete::Coords3D >( p0, p1, p2, p3 ) ) )
{
  kxD0_0_ = polyCoeffs_.z0_.x_ - p_.x_;
  kxD0_1_ = polyCoeffs_.z1_.x_.offtype< 0, 1 >( );
  kxD0_2_ = polyCoeffs_.z2_.x_.offtype< 0, 2 >( );
  kxD0_3_ = polyCoeffs_.z3_.x_.offtype< 0, 3 >( );

  kxD1_0_ = kxD0_1_;
  kxD1_1_ = 2 * kxD0_2_;
  kxD1_2_ = 3 * kxD0_3_;

  kxD2_0_ = kxD1_1_;
  kxD2_1_ = 2 * kxD1_2_;

  kyD0_0_ = polyCoeffs_.z0_.y_ - p.y_;
  kyD0_1_ = polyCoeffs_.z1_.y_.offtype< 0, 1 >( );
  kyD0_2_ = polyCoeffs_.z2_.y_.offtype< 0, 2 >( );
  kyD0_3_ = polyCoeffs_.z3_.y_.offtype< 0, 3 >( );

  kyD1_0_ = kyD0_1_;
  kyD1_1_ = 2 * kyD0_2_;
  kyD1_2_ = 3 * kyD0_3_;

  kyD2_0_ = kyD1_1_;
  kyD2_1_ = 2 * kyD1_2_;

  kzD0_0_ = polyCoeffs_.z0_.z_ - p.z_;
  kzD0_1_ = polyCoeffs_.z1_.z_.offtype< 0, 1 >( );
  kzD0_2_ = polyCoeffs_.z2_.z_.offtype< 0, 2 >( );
  kzD0_3_ = polyCoeffs_.z3_.z_.offtype< 0, 3 >( );

  kzD1_0_ = kzD0_1_;
  kzD1_1_ = 2 * kzD0_2_;
  kzD1_2_ = 3 * kzD0_3_;

  kzD2_0_ = kzD1_1_;
  kzD2_1_ = 2 * kzD1_2_;
}

Concrete::Time
MetaPDF::ApproximationPoly3D::splitTime( const Concrete::Time t_low, const Concrete::Time t_high, const Concrete::Time t_tol ) const
{
  Concrete::Time t;
  Physical< 2, 0 > last_f = HUGE_VAL;
  {
    const double GRID = 7;
    const Concrete::Time STEP = ( t_high - t_low ) / GRID;
    for( Concrete::Time tt = t_low + 0.5 * STEP; tt < t_high; tt += STEP )
      {
	Physical< 2, 0 > tmp = squaredDistanceAt( tt );
	if( tmp < last_f )
	  {
	    last_f = tmp;
	    t = tt;
	  }
      }
  }
  Concrete::Time last_t = -1;
  bool lastOffBounds = false;
      
  while( t < last_t - t_tol || t > last_t + t_tol )
    {
      last_t = t;
      Physical< 2, -1 > objectiveD1( 0 );
      Physical< 2, -2 > objectiveD2( 0 );

      {
	/* x-components */
	Physical< 1, 0 > fD0 = kxD0_0_ + t * ( kxD0_1_ + t * ( kxD0_2_ + t * kxD0_3_ ) );
	Physical< 1, -1 > fD1 = kxD1_0_ + t * ( kxD1_1_ + t * kxD1_2_ );
	Physical< 1, -2 > fD2 = kxD2_0_ + t * kxD2_1_;
	objectiveD1 += fD0 * fD1;
	objectiveD2 += fD1 * fD1 + fD0 * fD2;
      }

      {
	/* y-components */
	Physical< 1, 0 > fD0 = kyD0_0_ + t * ( kyD0_1_ + t * ( kyD0_2_ + t * kyD0_3_ ) );
	Physical< 1, -1 > fD1 = kyD1_0_ + t * ( kyD1_1_ + t * kyD1_2_ );
	Physical< 1, -2 > fD2 = kyD2_0_ + t * kyD2_1_;
	objectiveD1 += fD0 * fD1;
	objectiveD2 += fD1 * fD1 + fD0 * fD2;
      }

      {
	/* z-components */
	Physical< 1, 0 > fD0 = kzD0_0_ + t * ( kzD0_1_ + t * ( kzD0_2_ + t * kzD0_3_ ) );
	Physical< 1, -1 > fD1 = kzD1_0_ + t * ( kzD1_1_ + t * kzD1_2_ );
	Physical< 1, -2 > fD2 = kzD2_0_ + t * kzD2_1_;
	objectiveD1 += fD0 * fD1;
	objectiveD2 += fD1 * fD1 + fD0 * fD2;
      }

      Concrete::Time step;
      if( objectiveD2 <= 0 )
	{
	  /* The minimization problem is not convex, locally.
	   * Go to one of the extremes.
	   */
	  if( objectiveD1 < 0 )
	    {
	      step = 1.1 * ( t_high - t );
	    }
	  else
	    {
	      step = 1.1 * ( t_low - t );
	    }
	}
      else
	{
	  step = -objectiveD1 / objectiveD2;
	  if( t + step < t_low )
	    {
	      step = 1.1 * ( t_low - t );   // 1.1 to make sure that the off-bounds test detects minima outside [ t_low t_high ]
	    }
	  else if( t + step > t_high )
	    {
	      step = 1.1 * ( t_high - t );   // 1.1 to make sure that the off-bounds test detects minima outside [ t_low t_high ]
	    }
	}

      Physical< 2, 0 > f = squaredDistanceAt( t + step );
      if( step > 0 )
	{
	  while( f > last_f && step > t_tol )
	    {
	      step = step * 0.5;
	      f = squaredDistanceAt( t + step );
	    }
	}
      else
	{
	  while( f > last_f && step < -t_tol )
	    {
	      step = step * 0.5;
	      f = squaredDistanceAt( t + step );
	    }
	}	
      t += step;
      last_f = f;

      if( t <= t_low )
	{
	  if( lastOffBounds )
	    {
	      return t_low;
	    }
	  else
	    {
	      t = t_low;
	      lastOffBounds = true;
	    }
	}
      else if( t >= t_high )
	{
	  if( lastOffBounds )
	    {
	      return t_high;
	    }
	  else
	    {
	      t = t_high;
	      lastOffBounds = true;
	    }
	}
      else
	{
	  lastOffBounds = false;
	}
    }

  if( lastOffBounds )
    {
      if( t > 0.5 * ( t_low + t_high ) )
	{
	  return t_high;
	}
      return t_low;
    }

  return t;
}

Bezier::ControlPoints< Concrete::Coords3D >
MetaPDF::ApproximationPoly3D::getControls( const Concrete::Time t_low, const Concrete::Time t_high ) const
{
  return Bezier::ControlPoints< Concrete::Coords3D >( polyCoeffs_.subSection( t_low.offtype< 0, 1 >( ), t_high.offtype< 0, 1 >( ) ) );
}

bool
MetaPDF::ApproximationPoly3D::isCircular( ) const
{
  Concrete::Length rmax = -HUGE_VAL;
  Concrete::Length rmin = HUGE_VAL;
  for( Concrete::Time t = 0; t < 1.05; t += 0.33333 )
    {
      Concrete::Length r = distanceAt( t );
      rmax = max( rmax, r );
      rmin = min( rmin, r );
    }
  return rmax <= rmin * 1.0001;
}

const Concrete::Coords3D &
MetaPDF::ApproximationPoly3D::getPoint( ) const
{
  return p_;
}


Concrete::Length
MetaPDF::ApproximationPoly3D::distanceAt( Concrete::Time t ) const
{
  return hypotPhysical( kxD0_0_ + t * ( kxD0_1_ + t * ( kxD0_2_ + t * kxD0_3_ ) ),
			kyD0_0_ + t * ( kyD0_1_ + t * ( kyD0_2_ + t * kyD0_3_ ) ),
			kzD0_0_ + t * ( kzD0_1_ + t * ( kzD0_2_ + t * kzD0_3_ ) ) );
}

Physical< 2, 0 >
MetaPDF::ApproximationPoly3D::squaredDistanceAt( Concrete::Time t ) const
{
  Concrete::Length delta_x = kxD0_0_ + t * ( kxD0_1_ + t * ( kxD0_2_ + t * kxD0_3_ ) );
  Concrete::Length delta_y = kyD0_0_ + t * ( kyD0_1_ + t * ( kyD0_2_ + t * kyD0_3_ ) );
  Concrete::Length delta_z = kzD0_0_ + t * ( kzD0_1_ + t * ( kzD0_2_ + t * kzD0_3_ ) );
  return delta_x * delta_x + delta_y * delta_y + delta_z * delta_z;
}

MetaPDF::ApproximationSegmentSection3D::ApproximationSegmentSection3D( const MetaPDF::ApproximationPoly3D * baseSeg, Concrete::Time steps, Concrete::Time t0, Concrete::Time t1 )
  : controls_( baseSeg->getControls( t0, t1 ) ), baseSeg_( baseSeg ), steps_( steps ), t0_( t0 ), t1_( t1 )
{ }

MetaPDF::ApproximationSegmentSection3D *
MetaPDF::ApproximationSegmentSection3D::cutAfter( Concrete::Time t ) const
{
  return new ApproximationSegmentSection3D( baseSeg_, steps_, t0_, t );
}

MetaPDF::ApproximationSegmentSection3D *
MetaPDF::ApproximationSegmentSection3D::cutBefore( Concrete::Time t ) const
{
  return new ApproximationSegmentSection3D( baseSeg_, steps_, t, t1_ );
}


namespace MetaPDF
{
  class HullSorter3D : public std::binary_function< const Concrete::Coords3D *, const Concrete::Coords3D *, bool >
  {
    const Concrete::Coords3D & p_;
  public:
    HullSorter3D( const Concrete::Coords3D & p ) : p_( p ) { }
    bool operator () ( const Concrete::Coords3D * p1, const Concrete::Coords3D * p2 )
    {
      /* The comparison orders the points according to their counter-clockwise
       * angle to the positive x-axis.
       * Since (by construction) no point can have smaller y-coordinate than p, all angles
       * are in the range [ 0, pi ], and it suffices to reason in terms of the argument
       * to the monotonic function arccotan.
       * The ordinary counter-clockwise-of by means of computing the projection of the 
       * second on the clockwise normal of the other leads to the same equation.
       */
      const Concrete::Length d1x = p1->x_ - p_.x_;
      const Concrete::Length d1y = p1->y_ - p_.y_;
      const Concrete::Length d2x = p2->x_ - p_.x_;
      const Concrete::Length d2y = p2->y_ - p_.y_;
      return d1y * d2x < d2y * d1x;
    }
  };
}

Concrete::Length
MetaPDF::ApproximationSegmentSection3D::convexHullDistance( ) const
{
  Concrete::Coords3D p( baseSeg_->getPoint( ) );
  std::list< const Concrete::Coords3D * > hullPoints;
  
  std::vector< const Concrete::Coords3D * > pointSet( 0 );
  pointSet.reserve( 4 );
  pointSet.push_back( & controls_.p0_ );
  pointSet.push_back( & controls_.p1_ );
  pointSet.push_back( & controls_.p2_ );
  pointSet.push_back( & controls_.p3_ );
  
  Concrete::Length res = HUGE_VAL;

  for( std::vector< const Concrete::Coords3D * >::iterator i = pointSet.begin( );
       i != pointSet.end( );
       ++i )
    {
      if( i != pointSet.begin( ) )
	{
	  const Concrete::Coords3D * tmp = pointSet[0];
	  pointSet[0] = *i;
	  *i = tmp;
	}
      triangleDistance( pointSet, p, & res );
    }

  if( res == HUGE_VAL )
    {
      /* This means that p was on the inside of each line, meaning it
       * is also inside the convex hull.
       */
      return 0;
    }
  return res;
}

void
MetaPDF::ApproximationSegmentSection3D::triangleDistance( std::vector< const Concrete::Coords3D * > & points, const Concrete::Coords3D & p, Concrete::Length * bestDist )
{
  /*
   * Note: We will mess around with points, but it will be returned as passed.
   */

  /* Remember:
   * points[0] is the point which is on the inside side of the triangle.
   * points[1..3] are the corners of the triangle.
   */
  const Concrete::UnitFloatTriple n = Concrete::crossDirection( *points[2] - *points[1], *points[3] - *points[1] );
  const Concrete::Length tn = Concrete::inner( n, p - *points[1] );
  if( ( tn > Concrete::ZERO_LENGTH ) == ( Concrete::inner( n, *points[0] - *points[1] ) > Concrete::ZERO_LENGTH ) )
    {
      /* When p is on the same side of the plane as points[0], we don't update *bestDist.
       * If *bestDist is never updated, it means that p is inside all planes and the minimum distance
       * is 0.  See calling function.
       */
      return;
    }

  Concrete::Length res = HUGE_VAL;

  const Concrete::Coords3D pProj = p - tn * n;
  
  std::vector< const Concrete::Coords3D * >::iterator i0 = points.begin( );
  ++i0;
  std::vector< const Concrete::Coords3D * >::iterator begin = i0;
  for( ; i0 != points.end( ); ++i0 )
    {
      const Concrete::Coords3D * tmp = *begin;
      *begin = *i0;
      *i0 = tmp;

      std::vector< const Concrete::Coords3D * >::iterator j = begin;
      const Concrete::Coords3D & pInside( **j );
      ++j;
      const Concrete::Coords3D & p0( **j );
      ++j;
      const Concrete::Coords3D & p1( **j );

      /* To find an outward normal, we find the projection of pInside, and then take a vector
       * from pInside to the projection.  (We work with coordinates less p0.)
       */
      const Concrete::Coords3D d1 = p1 - p0;
      const Concrete::Coords3D dInside = pInside - p0;
      const Concrete::Coords3D dp = p - p0;
      double d1Norm2 = Concrete::innerScalar( d1, d1 );
      if( d1Norm2 == 0 )
	{
	  res = min( res, dp.norm( ) );
	  goto next;
	}

      /* Introduce new scope to prevent compiler warnings about goto past initializations.
       */
      {
	double sInside = Concrete::innerScalar( dInside, d1 ) / d1Norm2;
	const Concrete::Coords3D nOut = d1 * sInside - dInside;
	const Concrete::Coords3D dpProj = pProj - p0;
	
	if( Concrete::innerScalar( nOut, nOut ) > 0 &&
	    Concrete::innerScalar( nOut, dpProj ) < 0 )
	  {
	    /* pProj is on the same side as pInside
	     */
	    goto next;
	  }
	
	res = min( res, ( dp - d1 * ( Concrete::innerScalar( dpProj, d1 ) / d1Norm2 ) ).norm( ) );
      }
    next:
      /* Swap back so that points remains unchanged
       */
      *i0 = *begin;
      *begin = tmp;
    }

  if( res == HUGE_VAL )
    {
      res = ( p - pProj ).norm( );
    }

  *bestDist = min( *bestDist, res );
}


Concrete::Time
MetaPDF::ApproximationSegmentSection3D::splitTime( const Concrete::Time t_tol ) const
{
  Concrete::Time d = 0.2 * ( t1_ - t0_ );
  return baseSeg_->splitTime( t0_ + d, t1_ - d, t_tol );
}

Concrete::Time
MetaPDF::ApproximationSegmentSection3D::globalTime( Concrete::Time t ) const
{
  return steps_ + t;
}

Concrete::Length
MetaPDF::ApproximationSegmentSection3D::distanceAt( Concrete::Time t ) const
{
  return baseSeg_->distanceAt( t );
}

Concrete::Speed
MetaPDF::ApproximationSegmentSection3D::maxSpeed( ) const
{
  Concrete::Length tmp = max( hypotPhysical( controls_.p1_.x_ - controls_.p0_.x_, controls_.p1_.y_ - controls_.p0_.y_, controls_.p1_.z_ - controls_.p0_.z_ ),
				      hypotPhysical( controls_.p2_.x_ - controls_.p1_.x_, controls_.p2_.y_ - controls_.p1_.y_, controls_.p2_.z_ - controls_.p1_.z_ ) );
  return max( tmp, hypotPhysical( controls_.p3_.x_ - controls_.p2_.x_, controls_.p3_.y_ - controls_.p2_.y_, controls_.p3_.z_ - controls_.p2_.z_ ) ).offtype< 0, 1 >( );
}

Concrete::Time
MetaPDF::ApproximationSegmentSection3D::duration( ) const
{
  return t1_ - t0_;
}


Concrete::Length
Lang::ElementaryPath3D::arcLength( ) const
{
  if( size( ) == 0 )
    {
      throw Exceptions::OutOfRange( "The empty path has no defined arclength." );
    }
  /* Multiplication by dt is extracted to outside the integrals for efficiency.
   */
  Concrete::Length totlen = 0;

  const Concrete::Length arcdelta = Computation::the_arcdelta;

  const_iterator i1 = begin( );
  const_iterator i2 = i1;
  ++i2;
  for( ; i1 != end( ); ++i1, ++i2 )
    {
      if( i2 == end( ) )
	{
	  if( closed_ )
	    {
	      i2 = begin( );
	    }
	  else
	    {
	      break;
	    }
	}
      if( (*i1)->front_ == (*i1)->mid_ &&
	  (*i2)->rear_ == (*i2)->mid_ )
	{
	  const Concrete::Length segLength = hypotPhysical( (*i1)->mid_->x_ - (*i2)->mid_->x_, (*i1)->mid_->y_ - (*i2)->mid_->y_, (*i1)->mid_->z_ - (*i2)->mid_->z_ );
	  totlen += segLength;
	  continue;
	}

      Concrete::Bezier x0 = (*i1)->mid_->x_.offtype< 0, 3 >( );
      Concrete::Bezier y0 = (*i1)->mid_->y_.offtype< 0, 3 >( );
      Concrete::Bezier z0 = (*i1)->mid_->z_.offtype< 0, 3 >( );
      Concrete::Bezier x1 = (*i1)->front_->x_.offtype< 0, 3 >( );
      Concrete::Bezier y1 = (*i1)->front_->y_.offtype< 0, 3 >( );
      Concrete::Bezier z1 = (*i1)->front_->z_.offtype< 0, 3 >( );
      Concrete::Bezier x2 = (*i2)->rear_->x_.offtype< 0, 3 >( );
      Concrete::Bezier y2 = (*i2)->rear_->y_.offtype< 0, 3 >( );
      Concrete::Bezier z2 = (*i2)->rear_->z_.offtype< 0, 3 >( );
      Concrete::Bezier x3 = (*i2)->mid_->x_.offtype< 0, 3 >( );
      Concrete::Bezier y3 = (*i2)->mid_->y_.offtype< 0, 3 >( );
      Concrete::Bezier z3 = (*i2)->mid_->z_.offtype< 0, 3 >( );

      const Concrete::Length segLengthBound = ( hypotPhysical( x1-x0, y1-y0, z1-z0 ) + hypotPhysical( x2-x1, y2-y1, z2-z1 ) + hypotPhysical( x3-x2, y3-y2, z3-z2 ) ).offtype< 0, -3 >( );

      if( segLengthBound < arcdelta )
	{
	  totlen += segLengthBound;
	}
      else
	{
	  Concrete::Time dt = MetaPDF::computeDt( segLengthBound );
	  Concrete::Length tmpSum_l = Concrete::ZERO_LENGTH;
	  for( Concrete::Time t = 0; t < 1; t += dt )
	    {
	      Concrete::Time tc = Concrete::UNIT_TIME - t; /* complement to t */
	      Physical< 0, 2 > kv0 = -3 * tc * tc;
	      Physical< 0, 2 > kv1 = 3 * tc * tc - 6 * tc * t;
	      Physical< 0, 2 > kv2 = 6 * tc * t - 3 * t * t;
	      Physical< 0, 2 > kv3 = 3 * t * t;
	      Concrete::Speed vx = x0 * kv0 + x1 * kv1 + x2 * kv2 + x3 * kv3;
	      Concrete::Speed vy = y0 * kv0 + y1 * kv1 + y2 * kv2 + y3 * kv3;
	      Concrete::Speed vz = z0 * kv0 + z1 * kv1 + z2 * kv2 + z3 * kv3;
	      
	      Concrete::Length dl = hypotPhysical( vx, vy, vz ).offtype< 0, -1 >( );
	      tmpSum_l += dl;
	    }
	  totlen += tmpSum_l * dt.offtype< 0, 1 >( );
	}
    }

  return totlen;
}

Concrete::Length
Lang::ElementaryPath3D::arcLength( Concrete::Time tRemaining ) const
{
  if( size( ) == 0 )
    {
      throw Exceptions::OutOfRange( "The empty path has no defined arclength." );
    }
  if( tRemaining < 0 )
    {
      return negative_arcLength( -tRemaining );
    }
  if( isinfPhysical( tRemaining ) )
    {
      throw Exceptions::OutOfRange( "The arctime to infinity is not defined." );
    }
  if( tRemaining > duration( ) && ! closed_ )
    {
      throw Exceptions::OutOfRange( "Too big arctime for open path." );
    }
  /* Multiplication by dt is extracted to outside the integrals for efficiency.
   */
  Concrete::Length totlen = 0;

  const Concrete::Length arcdelta = Computation::the_arcdelta;

  {
    const double revolutions = floor( tRemaining / Concrete::Time( duration( ) ) );
    if( revolutions > 0 )
      {
	totlen += revolutions * arcLength( );
	tRemaining -= revolutions * Concrete::Time( duration( ) );
      }
  }

  const_iterator i1 = begin( );
  const_iterator i2 = i1;
  ++i2;
  for( ; tRemaining > 0; ++i1, ++i2, tRemaining -= Concrete::UNIT_TIME )
    {
      if( i1 == end( ) )
	{
	  /* This could be considered an error situation, but it may be due to numeric
	   * errors, so we take action accordingly.
	   */
	  break;
	}
      if( i2 == end( ) )
	{
	  /* This could also be an error situation...
	   */
	  i2 = begin( );
	}
      if( (*i1)->front_ == (*i1)->mid_ &&
	  (*i2)->rear_ == (*i2)->mid_ )
	{
	  if( tRemaining <= 1 )
	    {
	      Concrete::Time t1 = tRemaining;
	      Concrete::Time tc = Concrete::UNIT_TIME - t1; /* complement to t1 */
	      Concrete::Coords3D tmp =
		( tc * tc * ( tc + 3 * t1 ) ).offtype< 0, 3 >( ) * (*((*i1)->mid_)) +
		( t1 * t1 * ( t1 + 3 * tc ) ).offtype< 0, 3 >( ) * (*((*i2)->mid_));
	      const Concrete::Length segLength = hypotPhysical( (*i1)->mid_->x_ - tmp.x_, (*i1)->mid_->y_ - tmp.y_, (*i1)->mid_->z_ - tmp.z_ );
	      totlen += segLength;
	      break;
	    }
	  const Concrete::Length segLength = hypotPhysical( (*i1)->mid_->x_ - (*i2)->mid_->x_, (*i1)->mid_->y_ - (*i2)->mid_->y_, (*i1)->mid_->z_ - (*i2)->mid_->z_ );
	  totlen += segLength;
	  continue;
	}

      Concrete::Bezier x0 = (*i1)->mid_->x_.offtype< 0, 3 >( );
      Concrete::Bezier y0 = (*i1)->mid_->y_.offtype< 0, 3 >( );
      Concrete::Bezier z0 = (*i1)->mid_->z_.offtype< 0, 3 >( );
      Concrete::Bezier x1 = (*i1)->front_->x_.offtype< 0, 3 >( );
      Concrete::Bezier y1 = (*i1)->front_->y_.offtype< 0, 3 >( );
      Concrete::Bezier z1 = (*i1)->front_->z_.offtype< 0, 3 >( );
      Concrete::Bezier x2 = (*i2)->rear_->x_.offtype< 0, 3 >( );
      Concrete::Bezier y2 = (*i2)->rear_->y_.offtype< 0, 3 >( );
      Concrete::Bezier z2 = (*i2)->rear_->z_.offtype< 0, 3 >( );
      Concrete::Bezier x3 = (*i2)->mid_->x_.offtype< 0, 3 >( );
      Concrete::Bezier y3 = (*i2)->mid_->y_.offtype< 0, 3 >( );
      Concrete::Bezier z3 = (*i2)->mid_->z_.offtype< 0, 3 >( );

      const Concrete::Length segLengthBound = ( hypotPhysical( x1-x0, y1-y0, z1-z0 ) + hypotPhysical( x2-x1, y2-y1, z2-z1 ) + hypotPhysical( x3-x2, y3-y2, z3-z2 ) ).offtype< 0, -3 >( );

      if( segLengthBound < arcdelta )
	{
	  if( tRemaining <= 1 )
	    {
	      totlen += ( tRemaining / Concrete::UNIT_TIME ) * segLengthBound;
	      break;
	    }
	  totlen += segLengthBound;
	}
      else
	{
	  Concrete::Time dt = MetaPDF::computeDt( segLengthBound );
	  Concrete::Length tmpSum_l = Concrete::ZERO_LENGTH;
	  const Concrete::Time tend = min( Concrete::UNIT_TIME, tRemaining );
	  for( Concrete::Time t = 0; t < tend; t += dt )
	    {
	      Concrete::Time tc = Concrete::UNIT_TIME - t; /* complement to t */
	      Physical< 0, 2 > kv0 = -3 * tc * tc;
	      Physical< 0, 2 > kv1 = 3 * tc * tc - 6 * tc * t;
	      Physical< 0, 2 > kv2 = 6 * tc * t - 3 * t * t;
	      Physical< 0, 2 > kv3 = 3 * t * t;
	      Concrete::Speed vx = x0 * kv0 + x1 * kv1 + x2 * kv2 + x3 * kv3;
	      Concrete::Speed vy = y0 * kv0 + y1 * kv1 + y2 * kv2 + y3 * kv3;
	      Concrete::Speed vz = z0 * kv0 + z1 * kv1 + z2 * kv2 + z3 * kv3;
	      
	      Concrete::Length dl = hypotPhysical( vx, vy, vz ).offtype< 0, -1 >( );
	      tmpSum_l += dl;
	    }
	  totlen += tmpSum_l * dt.offtype< 0, 1 >( );
	}
    }

  return totlen;
}

Concrete::Length
Lang::ElementaryPath3D::negative_arcLength( Concrete::Time tRemaining ) const
{
  if( size( ) == 0 )
    {
      throw Exceptions::OutOfRange( "The empty path has no defined arclength." );
    }
  if( tRemaining < 0 )
    {
      throw Exceptions::InternalError( "Negative tRemaining in negative_arcLength." );
    }
  if( ! closed_ )
    {
      throw Exceptions::OutOfRange( "The arctime at negative lengths is not defined for open paths." );
    }
  if( isinfPhysical( tRemaining ) )
    {
      throw Exceptions::OutOfRange( "The arctime to infinity is not defined." );
    }

  /* Multiplication by dt is extracted to outside the integrals for efficiency.
   */
  Concrete::Length totlen = 0;

  const Concrete::Length arcdelta = Computation::the_arcdelta;

  {
    const double revolutions = floor( tRemaining / Concrete::Time( duration( ) ) );
    if( revolutions > 0 )
      {
	totlen -= revolutions * arcLength( );
	tRemaining -= revolutions * Concrete::Time( duration( ) );
      }
  }

  const_reverse_iterator i1 = rbegin( );
  const_reverse_iterator i2 = i1;
  if( closed_ )
    {
      i1 = rend( );
      --i1;
      i2 = rbegin( );
    }
  else
    {
      ++i2;
    }
  for( ; tRemaining > 0; ++i1, ++i2, tRemaining -= Concrete::UNIT_TIME )
    {
      if( i2 == rend( ) )
	{
	  /* This could be considered an error situation, but it may be due to numeric
	   * errors, so we take action accordingly.
	   */
	  break;
	}
      if( i1 == rend( ) )
	{
	  i1 = rbegin( );
	}
      if( (*i1)->rear_ == (*i1)->mid_ &&
	  (*i2)->front_ == (*i2)->mid_ )
	{
	  if( tRemaining <= 1 )
	    {
	      Concrete::Time t1 = tRemaining;
	      Concrete::Time tc = Concrete::UNIT_TIME - t1; /* complement to t1 */
	      Concrete::Coords3D tmp =
		( tc * tc * ( tc + 3 * t1 ) ).offtype< 0, 3 >( ) * (*((*i1)->mid_)) +
		( t1 * t1 * ( t1 + 3 * tc ) ).offtype< 0, 3 >( ) * (*((*i2)->mid_));
	      const Concrete::Length segLength = hypotPhysical( (*i1)->mid_->x_ - tmp.x_, (*i1)->mid_->y_ - tmp.y_, (*i1)->mid_->z_ - tmp.z_ );
	      totlen -= segLength;
	      break;
	    }
	  const Concrete::Length segLength = hypotPhysical( (*i1)->mid_->x_ - (*i2)->mid_->x_, (*i1)->mid_->y_ - (*i2)->mid_->y_, (*i1)->mid_->z_ - (*i2)->mid_->z_ );
	  totlen -= segLength;
	  continue;
	}

      Concrete::Bezier x0 = (*i1)->mid_->x_.offtype< 0, 3 >( );
      Concrete::Bezier y0 = (*i1)->mid_->y_.offtype< 0, 3 >( );
      Concrete::Bezier z0 = (*i1)->mid_->z_.offtype< 0, 3 >( );
      Concrete::Bezier x1 = (*i1)->rear_->x_.offtype< 0, 3 >( );
      Concrete::Bezier y1 = (*i1)->rear_->y_.offtype< 0, 3 >( );
      Concrete::Bezier z1 = (*i1)->rear_->z_.offtype< 0, 3 >( );
      Concrete::Bezier x2 = (*i2)->front_->x_.offtype< 0, 3 >( );
      Concrete::Bezier y2 = (*i2)->front_->y_.offtype< 0, 3 >( );
      Concrete::Bezier z2 = (*i2)->front_->z_.offtype< 0, 3 >( );
      Concrete::Bezier x3 = (*i2)->mid_->x_.offtype< 0, 3 >( );
      Concrete::Bezier y3 = (*i2)->mid_->y_.offtype< 0, 3 >( );
      Concrete::Bezier z3 = (*i2)->mid_->z_.offtype< 0, 3 >( );

      const Concrete::Length segLengthBound = ( hypotPhysical( x1-x0, y1-y0, z1-z0 ) + hypotPhysical( x2-x1, y2-y1, z2-z1 ) + hypotPhysical( x3-x2, y3-y2, z3-z2 ) ).offtype< 0, -3 >( );

      if( segLengthBound < arcdelta )
	{
	  if( tRemaining <= 1 )
	    {
	      totlen -= ( tRemaining / Concrete::UNIT_TIME ) * segLengthBound;
	      break;
	    }
	  totlen -= segLengthBound;
	}
      else
	{
	  Concrete::Time dt = MetaPDF::computeDt( segLengthBound );
	  Concrete::Length tmpSum_l = Concrete::ZERO_LENGTH;
	  const Concrete::Time tend = min( Concrete::UNIT_TIME, tRemaining );
	  for( Concrete::Time t = 0; t < tend; t += dt )
	    {
	      Concrete::Time tc = Concrete::UNIT_TIME - t; /* complement to t */
	      Physical< 0, 2 > kv0 = -3 * tc * tc;
	      Physical< 0, 2 > kv1 = 3 * tc * tc - 6 * tc * t;
	      Physical< 0, 2 > kv2 = 6 * tc * t - 3 * t * t;
	      Physical< 0, 2 > kv3 = 3 * t * t;
	      Concrete::Speed vx = x0 * kv0 + x1 * kv1 + x2 * kv2 + x3 * kv3;
	      Concrete::Speed vy = y0 * kv0 + y1 * kv1 + y2 * kv2 + y3 * kv3;
	      Concrete::Speed vz = z0 * kv0 + z1 * kv1 + z2 * kv2 + z3 * kv3;
	      
	      Concrete::Length dl = hypotPhysical( vx, vy, vz ).offtype< 0, -1 >( );
	      tmpSum_l += dl;
	    }
	  totlen -= tmpSum_l * dt.offtype< 0, 1 >( );
	}
    }

  return totlen;
}

Concrete::SplineTime
Lang::ElementaryPath3D::arcTime( const Concrete::Length & t, Concrete::Time t0 ) const
{
  if( size( ) == 0 )
    {
      throw Exceptions::OutOfRange( "The empty path has no arctimes defined." );
    }
  if( duration( ) == 0 )
    {
      throw Exceptions::OutOfRange( "The singleton path has no arctimes defined." );
    }
  if( t >= HUGE_VAL )
    {
      return Concrete::SplineTime( Concrete::Time( duration( ) ), true );
    }
  if( t < 0 )
    {
      return negative_arcTime( - t, t0 );
    }

  Concrete::Time splineTime = t0;
  t0 = modPhysical( t0, Concrete::Time( duration( ) ) );
  if( t0 < 0 )
    {
      t0 += Concrete::Time( duration( ) );
    }

  const_iterator i1 = begin( );
  while( t0 >= 1 )
    {
      t0 -= Concrete::UNIT_TIME;
      ++i1;
      if( i1 == end( ) )
	{
	  if( ! closed_ )
	    {
	      return Concrete::SplineTime( Concrete::Time( duration( ) ), true );
	    }
	  i1 = begin( );
	}
    }
  const_iterator i2 = i1;
  ++i2;

  const Concrete::Length arcdelta = Computation::the_arcdelta;
  Concrete::Length remainingLength = t;

  if( t0 > 0 )
    {
      if( i2 == end( ) )
	{
	  if( ! closed_ )
	    {
	      return Concrete::SplineTime( Concrete::Time( duration( ) ), true );
	    }
	  i2 = begin( );
	}
      if( (*i1)->front_ == (*i1)->mid_ &&
	  (*i2)->rear_ == (*i2)->mid_ )
	{
	  Concrete::Time tc = Concrete::UNIT_TIME - t0; /* complement to t0 */
	  Concrete::Coords3D tmp =
	    ( tc * tc * ( tc + 3 * t0 ) ).offtype< 0, 3 >( ) * (*((*i1)->mid_)) +
	    ( t0 * t0 * ( t0 + 3 * tc ) ).offtype< 0, 3 >( ) * (*((*i2)->mid_));
	  const Concrete::Length segRestLength = hypotPhysical( tmp.x_ - (*i2)->mid_->x_, tmp.y_ - (*i2)->mid_->y_, tmp.z_ - (*i2)->mid_->z_ );
	  if( segRestLength < remainingLength )
	    {
	      remainingLength -= segRestLength;
	      goto beginLoop;
	    }
	  const Concrete::Length segPastLength = hypotPhysical( tmp.x_ - (*i1)->mid_->x_, tmp.y_ - (*i1)->mid_->y_, tmp.z_ - (*i1)->mid_->z_ );
	  const Concrete::Length segLength = hypotPhysical( (*i1)->mid_->x_ - (*i2)->mid_->x_, (*i1)->mid_->y_ - (*i2)->mid_->y_, (*i1)->mid_->z_ - (*i2)->mid_->z_ );
	  splineTime += MetaPDF::straightLineArcTime( ( segPastLength + remainingLength ) / segLength ) - t0;
	  goto done;
	}

      {
	Concrete::Bezier x0 = (*i1)->mid_->x_.offtype< 0, 3 >( );
	Concrete::Bezier y0 = (*i1)->mid_->y_.offtype< 0, 3 >( );
	Concrete::Bezier z0 = (*i1)->mid_->z_.offtype< 0, 3 >( );
	Concrete::Bezier x1 = (*i1)->front_->x_.offtype< 0, 3 >( );
	Concrete::Bezier y1 = (*i1)->front_->y_.offtype< 0, 3 >( );
	Concrete::Bezier z1 = (*i1)->front_->z_.offtype< 0, 3 >( );
	Concrete::Bezier x2 = (*i2)->rear_->x_.offtype< 0, 3 >( );
	Concrete::Bezier y2 = (*i2)->rear_->y_.offtype< 0, 3 >( );
	Concrete::Bezier z2 = (*i2)->rear_->z_.offtype< 0, 3 >( );
	Concrete::Bezier x3 = (*i2)->mid_->x_.offtype< 0, 3 >( );
	Concrete::Bezier y3 = (*i2)->mid_->y_.offtype< 0, 3 >( );
	Concrete::Bezier z3 = (*i2)->mid_->z_.offtype< 0, 3 >( );
	
	const Concrete::Length segLengthBound = ( hypotPhysical( x1-x0, y1-y0, z1-z0 ) + hypotPhysical( x2-x1, y2-y1, z2-z1 ) + hypotPhysical( x3-x2, y3-y2, z3-z2 ) ).offtype< 0, -3 >( );
	
	if( segLengthBound < arcdelta )
	  {
	    remainingLength -= segLengthBound;
	    if( remainingLength <= 0 )
	      {
		splineTime += 0.5;
		goto done;
	      }
	  }
	else
	  {
	    Concrete::Time dt = MetaPDF::computeDt( segLengthBound );
	    
	    const Concrete::Length remainingDiv_dt = remainingLength / dt.offtype< 0, 1 >( );
	    Concrete::Length tmpSum_l = Concrete::ZERO_LENGTH;
	    for( Concrete::Time t = t0; t < 1; t += dt )
	      {
		Concrete::Time tc = Concrete::UNIT_TIME - t; /* complement to t */
		Physical< 0, 2 > kv0 = -3 * tc * tc;
		Physical< 0, 2 > kv1 = 3 * tc * tc - 6 * tc * t;
		Physical< 0, 2 > kv2 = 6 * tc * t - 3 * t * t;
		Physical< 0, 2 > kv3 = 3 * t * t;
		Concrete::Speed vx = x0 * kv0 + x1 * kv1 + x2 * kv2 + x3 * kv3;
		Concrete::Speed vy = y0 * kv0 + y1 * kv1 + y2 * kv2 + y3 * kv3;
		Concrete::Speed vz = z0 * kv0 + z1 * kv1 + z2 * kv2 + z3 * kv3;
		
		Concrete::Length dl = hypotPhysical( vx, vy, vz ).offtype< 0, -1 >( );
		tmpSum_l += dl;
		if( tmpSum_l >= remainingDiv_dt )
		  {
		    splineTime += t - t0;
		    goto done;
		  }
	      }
	    remainingLength -= tmpSum_l * dt.offtype< 0, 1 >( );
	  }
      }
      
    beginLoop:
      splineTime = Concrete::Time( ceil( splineTime.offtype< 0, 1 >( ) ) );
      ++i1;
      if( i1 == end( ) )
	{
	  if( ! closed_ )
	    {
	      return Concrete::SplineTime( Concrete::Time( duration( ) ), true );
	    }
	  i1 = begin( );
	}
      ++i2;
    }

  for( ; ; ++i1, ++i2, splineTime += Concrete::UNIT_TIME )
    {
      if( i1 == end( ) )
	{
	  /* If not closed, then the break occurs when i2 reaches end
	   */
	  i1 = begin( );
	}
      if( i2 == end( ) )
	{
	  if( ! closed_ )
	    {
	      return Concrete::SplineTime( Concrete::Time( duration( ) ), true );
	    }
	  i2 = begin( );
	}
      if( (*i1)->front_ == (*i1)->mid_ &&
	  (*i2)->rear_ == (*i2)->mid_ )
	{
	  const Concrete::Length segLength = hypotPhysical( (*i1)->mid_->x_ - (*i2)->mid_->x_, (*i1)->mid_->y_ - (*i2)->mid_->y_, (*i1)->mid_->z_ - (*i2)->mid_->z_ );
	  if( segLength < remainingLength )
	    {
	      remainingLength -= segLength;
	      continue;
	    }
	  splineTime += MetaPDF::straightLineArcTime( remainingLength / segLength );
	  goto done;
	}

      Concrete::Bezier x0 = (*i1)->mid_->x_.offtype< 0, 3 >( );
      Concrete::Bezier y0 = (*i1)->mid_->y_.offtype< 0, 3 >( );
      Concrete::Bezier z0 = (*i1)->mid_->z_.offtype< 0, 3 >( );
      Concrete::Bezier x1 = (*i1)->front_->x_.offtype< 0, 3 >( );
      Concrete::Bezier y1 = (*i1)->front_->y_.offtype< 0, 3 >( );
      Concrete::Bezier z1 = (*i1)->front_->z_.offtype< 0, 3 >( );
      Concrete::Bezier x2 = (*i2)->rear_->x_.offtype< 0, 3 >( );
      Concrete::Bezier y2 = (*i2)->rear_->y_.offtype< 0, 3 >( );
      Concrete::Bezier z2 = (*i2)->rear_->z_.offtype< 0, 3 >( );
      Concrete::Bezier x3 = (*i2)->mid_->x_.offtype< 0, 3 >( );
      Concrete::Bezier y3 = (*i2)->mid_->y_.offtype< 0, 3 >( );
      Concrete::Bezier z3 = (*i2)->mid_->z_.offtype< 0, 3 >( );

      const Concrete::Length segLengthBound = ( hypotPhysical( x1-x0, y1-y0, z1-z0 ) + hypotPhysical( x2-x1, y2-y1, z2-z1 ) + hypotPhysical( x3-x2, y3-y2, z3-z2 ) ).offtype< 0, -3 >( );

      if( segLengthBound < arcdelta )
	{
	  remainingLength -= segLengthBound;
	  if( remainingLength <= 0 )
	    {
	      splineTime += 0.5;
	      goto done;
	    }
	}
      else
	{
	  Concrete::Time dt = MetaPDF::computeDt( segLengthBound );
	  
	  const Concrete::Length remainingDiv_dt = remainingLength / dt.offtype< 0, 1 >( );
	  Concrete::Length tmpSum_l = Concrete::ZERO_LENGTH;
	  for( Concrete::Time t = 0; t < 1; t += dt )
	    {
	      Concrete::Time tc = Concrete::UNIT_TIME - t; /* complement to t */
	      Physical< 0, 2 > kv0 = -3 * tc * tc;
	      Physical< 0, 2 > kv1 = 3 * tc * tc - 6 * tc * t;
	      Physical< 0, 2 > kv2 = 6 * tc * t - 3 * t * t;
	      Physical< 0, 2 > kv3 = 3 * t * t;
	      Concrete::Speed vx = x0 * kv0 + x1 * kv1 + x2 * kv2 + x3 * kv3;
	      Concrete::Speed vy = y0 * kv0 + y1 * kv1 + y2 * kv2 + y3 * kv3;
	      Concrete::Speed vz = z0 * kv0 + z1 * kv1 + z2 * kv2 + z3 * kv3;
	      
	      Concrete::Length dl = hypotPhysical( vx, vy, vz ).offtype< 0, -1 >( );
	      tmpSum_l += dl;
	      if( tmpSum_l >= remainingDiv_dt )
		{
		  splineTime += t;
		  goto done;
		}
	    }
	  remainingLength -= tmpSum_l * dt.offtype< 0, 1 >( );
	}
    }

 done:
  return splineTime;
}

Concrete::SplineTime
Lang::ElementaryPath3D::negative_arcTime( const Concrete::Length deltaLen, Concrete::Time t0 ) const
{
  if( deltaLen < 0 )
    {
      throw Exceptions::InternalError( "Negative t in negative_arcTime." );
    }
  if( size( ) == 0 )
    {
      throw Exceptions::OutOfRange( "The empty path has no arctimes defined." );
    }
  if( duration( ) == 0 )
    {
      throw Exceptions::OutOfRange( "The singleton path has no arctimes defined." );
    }

  if( deltaLen >= HUGE_VAL )
    {
      return Concrete::SplineTime( 0, true );
    }

  Concrete::Time splineTime = t0;
  if( isClosed( ) )
    {
      t0 = modPhysical( t0, Concrete::Time( duration( ) ) );
      if( t0 < 0 )
	{
	  t0 += Concrete::Time( duration( ) );
	}
    }
  else
    {
      if( t0 < 0 )
	{
	  t0 = 0;
	}
      else if( t0 > Concrete::Time( duration( ) ) )
	{
	  t0 = Concrete::Time( duration( ) );
	}
    }
  t0 = Concrete::Time( duration( ) ) - t0;
  // From here on, t0 is the (positive) backwards-time

  const_reverse_iterator i1 = rbegin( );
  if( closed_ )
    {
      i1 = rend( );
      --i1;
    }
  while( t0 >= 1 )
    {
      t0 -= Concrete::UNIT_TIME;
      ++i1;
      if( i1 == rend( ) )
	{
	  if( ! closed_ )
	    {
	      return Concrete::SplineTime( 0, true );
	    }
	  i1 = rbegin( );
	}
    }
  const_reverse_iterator i2 = i1;
  ++i2;

  const Concrete::Length arcdelta = Computation::the_arcdelta;
  Concrete::Length remainingLength = deltaLen;

  if( t0 > 0 )
    {
      if( i2 == rend( ) )
	{
	  if( ! closed_ )
	    {
	      return Concrete::SplineTime( 0, true );
	    }
	  i2 = rbegin( );
	}
      if( (*i1)->rear_ == (*i1)->mid_ &&
	  (*i2)->front_ == (*i2)->mid_ )
	{
	  Concrete::Time tc = Concrete::UNIT_TIME - t0; /* complement to t0 */
	  Concrete::Coords3D tmp =
	    ( tc * tc * ( tc + 3 * t0 ) ).offtype< 0, 3 >( ) * (*((*i1)->mid_)) +
	    ( t0 * t0 * ( t0 + 3 * tc ) ).offtype< 0, 3 >( ) * (*((*i2)->mid_));
	  const Concrete::Length segRestLength = hypotPhysical( tmp.x_ - (*i2)->mid_->x_, tmp.y_ - (*i2)->mid_->y_, tmp.z_ - (*i2)->mid_->z_ );
	  if( segRestLength < remainingLength )
	    {
	      remainingLength -= segRestLength;
	      goto beginLoop;
	    }
	  const Concrete::Length segPastLength = hypotPhysical( tmp.x_ - (*i1)->mid_->x_, tmp.y_ - (*i1)->mid_->y_, tmp.z_ - (*i1)->mid_->z_ );
	  const Concrete::Length segLength = hypotPhysical( (*i1)->mid_->x_ - (*i2)->mid_->x_, (*i1)->mid_->y_ - (*i2)->mid_->y_, (*i1)->mid_->z_ - (*i2)->mid_->z_ );
	  splineTime -= MetaPDF::straightLineArcTime( ( segPastLength + remainingLength ) / segLength ) - t0;
	  goto done;
	}

      {
	Concrete::Bezier x0 = (*i1)->mid_->x_.offtype< 0, 3 >( );
	Concrete::Bezier y0 = (*i1)->mid_->y_.offtype< 0, 3 >( );
	Concrete::Bezier z0 = (*i1)->mid_->z_.offtype< 0, 3 >( );
	Concrete::Bezier x1 = (*i1)->rear_->x_.offtype< 0, 3 >( );
	Concrete::Bezier y1 = (*i1)->rear_->y_.offtype< 0, 3 >( );
	Concrete::Bezier z1 = (*i1)->rear_->z_.offtype< 0, 3 >( );
	Concrete::Bezier x2 = (*i2)->front_->x_.offtype< 0, 3 >( );
	Concrete::Bezier y2 = (*i2)->front_->y_.offtype< 0, 3 >( );
	Concrete::Bezier z2 = (*i2)->front_->z_.offtype< 0, 3 >( );
	Concrete::Bezier x3 = (*i2)->mid_->x_.offtype< 0, 3 >( );
	Concrete::Bezier y3 = (*i2)->mid_->y_.offtype< 0, 3 >( );
	Concrete::Bezier z3 = (*i2)->mid_->z_.offtype< 0, 3 >( );
	
	const Concrete::Length segLengthBound = ( hypotPhysical( x1-x0, y1-y0, z1-z0 ) + hypotPhysical( x2-x1, y2-y1, z2-z1 ) + hypotPhysical( x3-x2, y3-y2, z3-z2 ) ).offtype< 0, -3 >( );
	
	if( segLengthBound < arcdelta )
	  {
	    remainingLength -= segLengthBound;
	    if( remainingLength <= 0 )
	      {
		splineTime -= 0.5;
		goto done;
	      }
	  }
	else
	  {
	    Concrete::Time dt = MetaPDF::computeDt( segLengthBound );
	    
	    const Concrete::Length remainingDiv_dt = remainingLength / dt.offtype< 0, 1 >( );
	    Concrete::Length tmpSum_l = Concrete::ZERO_LENGTH;
	    for( Concrete::Time t = t0; t < 1; t += dt )
	      {
		Concrete::Time tc = Concrete::UNIT_TIME - t; /* complement to t */
		Physical< 0, 2 > kv0 = -3 * tc * tc;
		Physical< 0, 2 > kv1 = 3 * tc * tc - 6 * tc * t;
		Physical< 0, 2 > kv2 = 6 * tc * t - 3 * t * t;
		Physical< 0, 2 > kv3 = 3 * t * t;
		Concrete::Speed vx = x0 * kv0 + x1 * kv1 + x2 * kv2 + x3 * kv3;
		Concrete::Speed vy = y0 * kv0 + y1 * kv1 + y2 * kv2 + y3 * kv3;
		Concrete::Speed vz = z0 * kv0 + z1 * kv1 + z2 * kv2 + z3 * kv3;
		
		Concrete::Length dl = hypotPhysical( vx, vy, vz ).offtype< 0, -1 >( );
		tmpSum_l += dl;
		if( tmpSum_l >= remainingDiv_dt )
		  {
		    splineTime -= t - t0;
		    goto done;
		  }
	      }
	    remainingLength -= tmpSum_l * dt.offtype< 0, 1 >( );
	  }
      }
      
    beginLoop:
      splineTime = Concrete::Time( floor( splineTime.offtype< 0, 1 >( ) ) );
      ++i1;
      if( i1 == rend( ) )
	{
	  if( ! closed_ )
	    {
	      return Concrete::SplineTime( 0, true );
	    }
	  i1 = rbegin( );
	}
      ++i2;
    }

  for( ; ; ++i1, ++i2, splineTime -= Concrete::UNIT_TIME )
    {
      if( i1 == rend( ) )
	{
	  /* If not closed, then the break occurs when i2 reaches end
	   */
	  i1 = rbegin( );
	}
      if( i2 == rend( ) )
	{
	  if( ! closed_ )
	    {
	      return Concrete::SplineTime( 0, true );
	    }
	  i2 = rbegin( );
	}
      if( (*i1)->rear_ == (*i1)->mid_ &&
	  (*i2)->front_ == (*i2)->mid_ )
	{
	  const Concrete::Length segLength = hypotPhysical( (*i1)->mid_->x_ - (*i2)->mid_->x_, (*i1)->mid_->y_ - (*i2)->mid_->y_, (*i1)->mid_->z_ - (*i2)->mid_->z_ );
	  if( segLength < remainingLength )
	    {
	      remainingLength -= segLength;
	      continue;
	    }
	  splineTime -= MetaPDF::straightLineArcTime( remainingLength / segLength );
	  goto done;
	}
      
      Concrete::Bezier x0 = (*i1)->mid_->x_.offtype< 0, 3 >( );
      Concrete::Bezier y0 = (*i1)->mid_->y_.offtype< 0, 3 >( );
      Concrete::Bezier z0 = (*i1)->mid_->z_.offtype< 0, 3 >( );
      Concrete::Bezier x1 = (*i1)->rear_->x_.offtype< 0, 3 >( );
      Concrete::Bezier y1 = (*i1)->rear_->y_.offtype< 0, 3 >( );
      Concrete::Bezier z1 = (*i1)->rear_->z_.offtype< 0, 3 >( );
      Concrete::Bezier x2 = (*i2)->front_->x_.offtype< 0, 3 >( );
      Concrete::Bezier y2 = (*i2)->front_->y_.offtype< 0, 3 >( );
      Concrete::Bezier z2 = (*i2)->front_->z_.offtype< 0, 3 >( );
      Concrete::Bezier x3 = (*i2)->mid_->x_.offtype< 0, 3 >( );
      Concrete::Bezier y3 = (*i2)->mid_->y_.offtype< 0, 3 >( );
      Concrete::Bezier z3 = (*i2)->mid_->z_.offtype< 0, 3 >( );

      const Concrete::Length segLengthBound = ( hypotPhysical( x1-x0, y1-y0, z1-z0 ) + hypotPhysical( x2-x1, y2-y1, z2-z1 ) + hypotPhysical( x3-x2, y3-y2, z3-z2 ) ).offtype< 0, -3 >( );
      
      if( segLengthBound < arcdelta )
	{
	  remainingLength -= segLengthBound;
	  if( remainingLength <= 0 )
	    {
	      splineTime -= 0.5;
	      goto done;
	    }
	}
      else
	{
	  Concrete::Time dt = MetaPDF::computeDt( segLengthBound );
	  
	  const Concrete::Length remainingDiv_dt = remainingLength / dt.offtype< 0, 1 >( );
	  Concrete::Length tmpSum_l = Concrete::ZERO_LENGTH;
	  for( Concrete::Time t = 0; t < 1; t += dt )
	    {
	      Concrete::Time tc = Concrete::UNIT_TIME - t; /* complement to t */
	      Physical< 0, 2 > kv0 = -3 * tc * tc;
	      Physical< 0, 2 > kv1 = 3 * tc * tc - 6 * tc * t;
	      Physical< 0, 2 > kv2 = 6 * tc * t - 3 * t * t;
	      Physical< 0, 2 > kv3 = 3 * t * t;
	      Concrete::Speed vx = x0 * kv0 + x1 * kv1 + x2 * kv2 + x3 * kv3;
	      Concrete::Speed vy = y0 * kv0 + y1 * kv1 + y2 * kv2 + y3 * kv3;
	      Concrete::Speed vz = z0 * kv0 + z1 * kv1 + z2 * kv2 + z3 * kv3;
	      
	      Concrete::Length dl = hypotPhysical( vx, vy, vz ).offtype< 0, -1 >( );
	      tmpSum_l += dl;
	      if( tmpSum_l >= remainingDiv_dt )
		{
		  splineTime -= t;
		  goto done;
		}
	    }
	  remainingLength -= tmpSum_l * dt.offtype< 0, 1 >( );
	}
    }

 done:
  return splineTime;
}

RefCountPtr< const Lang::ElementaryPath3D >
Lang::ElementaryPath3D::subpath( const Concrete::SplineTime splt1, const Concrete::SplineTime splt2 ) const
{
  if( size( ) == 0 )
    {
      throw Exceptions::OutOfRange( "The empty path has no subpaths." );
    }
  Lang::ElementaryPath3D * res = new Lang::ElementaryPath3D;

  if( splt2.t( ) < splt1.t( ) )
    {
      throw Exceptions::OutOfRange( "The path-times must come in ascending order." );
    }
  if( splt2.t( ) != HUGE_VAL && splt2.t( ) > splt1.t( ) + size( ) )
    {
      std::cerr << splt1.t( ).offtype< 0, 1 >( ) << " " << size( ) << " " << splt2.t( ).offtype< 0, 1 >( ) << std::endl ;
      throw Exceptions::OutOfRange( "It is forbidden (although not logically necessary) to cycle more than one revolution.." );
    }

  Concrete::Time gt1 = timeCheck( splt1.t( ) );   /* "g" as in global.  t1 refers to a time on an elementary segment */
  Concrete::Time gt2 = timeCheck( splt2.t( ) );
  if( gt2 <= gt1 && splt2.t( ) > splt1.t( ) )
    {
      gt2 += size( );
    }

  /* First, we treat the case where both cuts are on the same elementary segment.
   * The reason is twofold.  First, it is more efficient, as it is almost only half
   * the amount of computation this way.  Secondly, cutting sequentially on the same
   * segment requires a bit of extra manipulation of the time arguments.
   */
  if( fmod( floor( gt1.offtype< 0, 1 >( ) ) + 1, size( ) ) == ceil( gt2.offtype< 0, 1 >( ) ) )
    {
      Concrete::Time t1;
      const Concrete::PathPoint3D * p1;
      const Concrete::PathPoint3D * p2;
      findSegment( gt1, &t1, &p1, &p2 );  /* note that the stored pointers must not be deleted */
      Concrete::Time t2 = gt2 - ceil( gt2.offtype< 0, 1 >( ) ) + 1;
      Concrete::Bezier x0 = p1->mid_->x_.offtype< 0, 3 >( );
      Concrete::Bezier y0 = p1->mid_->y_.offtype< 0, 3 >( );
      Concrete::Bezier z0 = p1->mid_->z_.offtype< 0, 3 >( );
      Concrete::Bezier x1 = p1->front_->x_.offtype< 0, 3 >( );
      Concrete::Bezier y1 = p1->front_->y_.offtype< 0, 3 >( );
      Concrete::Bezier z1 = p1->front_->z_.offtype< 0, 3 >( );
      Concrete::Bezier x2 = p2->rear_->x_.offtype< 0, 3 >( );
      Concrete::Bezier y2 = p2->rear_->y_.offtype< 0, 3 >( );
      Concrete::Bezier z2 = p2->rear_->z_.offtype< 0, 3 >( );
      Concrete::Bezier x3 = p2->mid_->x_.offtype< 0, 3 >( );
      Concrete::Bezier y3 = p2->mid_->y_.offtype< 0, 3 >( );
      Concrete::Bezier z3 = p2->mid_->z_.offtype< 0, 3 >( );
  
      Concrete::Time tc = t1 - Concrete::UNIT_TIME;  /* Note the sign here! */
      Concrete::Time td = t1 - t2;

      /* Compute new x polynomial using variable change t = t1 - td * tNew, where tNew is a time parameter in in the range [ 0, 1 ].
       */
      Concrete::Length k0x = - tc * tc * tc * x0 + t1 * ( 3 * tc * tc * x1 + t1 * ( 3 * Concrete::UNIT_TIME * x2 - 3 * t1 * x2 + t1 * x3 ) );
      Concrete::Length k1x = 3 * td * ( tc * tc * x0 + ( -Concrete::UNIT_TIME*Concrete::UNIT_TIME + 4 * Concrete::UNIT_TIME * t1 - 3 * t1 * t1 ) * x1 + t1 * ( -2 * Concrete::UNIT_TIME * x2 + 3 * t1 * x2 - t1 * x3 ) );
      Concrete::Length k2x = -3 * td * td * ( tc * x0 + 2 * Concrete::UNIT_TIME * x1 - 3 * t1 * x1 - Concrete::UNIT_TIME * x2 + 3 * t1 * x2 - t1 * x3 );
      Concrete::Length k3x = td * td * td * ( x0 - 3 * x1 + 3 * x2 - x3 );

      /* Compute new coefficients in the standard polynomial base */
      Concrete::Length x0new = k0x;
      Concrete::Length x1new = k0x + k1x / 3;
      Concrete::Length x2new = k0x + ( 2 * k1x + k2x ) / 3;
      Concrete::Length x3new = k0x + k1x + k2x + k3x;
      
      /* Same thing for y */
      
      Concrete::Length k0y = - tc * tc * tc * y0 + t1 * ( 3 * tc * tc * y1 + t1 * ( 3 * Concrete::UNIT_TIME * y2 - 3 * t1 * y2 + t1 * y3 ) );
      Concrete::Length k1y = 3 * td * ( tc * tc * y0 + ( -Concrete::UNIT_TIME*Concrete::UNIT_TIME + 4 * Concrete::UNIT_TIME * t1 - 3 * t1 * t1 ) * y1 + t1 * ( -2 * Concrete::UNIT_TIME * y2 + 3 * t1 * y2 - t1 * y3 ) );
      Concrete::Length k2y = -3 * td * td * ( tc * y0 + 2 * Concrete::UNIT_TIME * y1 - 3 * t1 * y1 - Concrete::UNIT_TIME * y2 + 3 * t1 * y2 - t1 * y3 );
      Concrete::Length k3y = td * td * td * ( y0 - 3 * y1 + 3 * y2 - y3 );
      
      Concrete::Length y0new = k0y;
      Concrete::Length y1new = k0y + k1y / 3;
      Concrete::Length y2new = k0y + ( 2 * k1y + k2y ) / 3;
      Concrete::Length y3new = k0y + k1y + k2y + k3y;

      /* Same thing for z */
      
      Concrete::Length k0z = - tc * tc * tc * z0 + t1 * ( 3 * tc * tc * z1 + t1 * ( 3 * Concrete::UNIT_TIME * z2 - 3 * t1 * z2 + t1 * z3 ) );
      Concrete::Length k1z = 3 * td * ( tc * tc * z0 + ( -Concrete::UNIT_TIME*Concrete::UNIT_TIME + 4 * Concrete::UNIT_TIME * t1 - 3 * t1 * t1 ) * z1 + t1 * ( -2 * Concrete::UNIT_TIME * z2 + 3 * t1 * z2 - t1 * z3 ) );
      Concrete::Length k2z = -3 * td * td * ( tc * z0 + 2 * Concrete::UNIT_TIME * z1 - 3 * t1 * z1 - Concrete::UNIT_TIME * z2 + 3 * t1 * z2 - t1 * z3 );
      Concrete::Length k3z = td * td * td * ( z0 - 3 * z1 + 3 * z2 - z3 );
      
      Concrete::Length z0new = k0z;
      Concrete::Length z1new = k0z + k1z / 3;
      Concrete::Length z2new = k0z + ( 2 * k1z + k2z ) / 3;
      Concrete::Length z3new = k0z + k1z + k2z + k3z;

      Concrete::PathPoint3D * ptmp = new Concrete::PathPoint3D( x0new, y0new, z0new );
      ptmp->front_ = new Concrete::Coords3D( x1new, y1new, z1new );
      res->push_back( ptmp );
      ptmp = new Concrete::PathPoint3D( x3new, y3new, z3new );
      ptmp->rear_ = new Concrete::Coords3D( x2new, y2new, z2new );
      res->push_back( ptmp );

      return RefCountPtr< const Lang::ElementaryPath3D >( res );
    }


  /* Now, we treat the other case, where the cuts are on different segments.  This results in a three phase
   * algorithm.
   * Since "cutting away after" requires much simpler formulas than straight-forward "cutting away before",
   * the latter is treated as the former, but with time reversed.
   */

  Concrete::Time t = 0;
  const_iterator i1 = begin( );
  for( ; t + 1 <= gt1; t += Concrete::UNIT_TIME, ++i1 )
    ;
  const_iterator i2 = i1;
  ++i2;
  if( i2 == end( ) )
    {
      i2 = begin( );
    }

  Concrete::Time t1 = gt1 - t;
  Concrete::PathPoint3D * p1;
  Concrete::PathPoint3D * p2;
  if( t1 == 0 )
    {
      p1 = new Concrete::PathPoint3D( **i1 );
      p2 = new Concrete::PathPoint3D( **i2 );
    }
  else
    {
      /* Reverse time! */
      Concrete::Bezier x3 = (*i1)->mid_->x_.offtype< 0, 3 >( );
      Concrete::Bezier y3 = (*i1)->mid_->y_.offtype< 0, 3 >( );
      Concrete::Bezier z3 = (*i1)->mid_->z_.offtype< 0, 3 >( );
      Concrete::Bezier x2 = (*i1)->front_->x_.offtype< 0, 3 >( );
      Concrete::Bezier y2 = (*i1)->front_->y_.offtype< 0, 3 >( );
      Concrete::Bezier z2 = (*i1)->front_->z_.offtype< 0, 3 >( );
      Concrete::Bezier x1 = (*i2)->rear_->x_.offtype< 0, 3 >( );
      Concrete::Bezier y1 = (*i2)->rear_->y_.offtype< 0, 3 >( );
      Concrete::Bezier z1 = (*i2)->rear_->z_.offtype< 0, 3 >( );
      Concrete::Bezier x0 = (*i2)->mid_->x_.offtype< 0, 3 >( );
      Concrete::Bezier y0 = (*i2)->mid_->y_.offtype< 0, 3 >( );
      Concrete::Bezier z0 = (*i2)->mid_->z_.offtype< 0, 3 >( );

      t1 = Concrete::UNIT_TIME - t1;

      /* Compute new x polynomial using variable change t = t1 * tNew, where tNew is a time parameter in in the range [ 0, 1 ].
       */
      Concrete::Bezier k0x = x0;
      Concrete::Bezier k1x = 3 * t1 * ( x1 - x0 ).offtype< 0, 1 >( );
      Concrete::Bezier k2x = 3 * t1 * t1 * ( x0 - 2 * x1 + x2 ).offtype< 0, 2 >( );
      Concrete::Bezier k3x = t1 * t1 * t1 * ( -x0 + 3 * x1 - 3 * x2 + x3 ).offtype< 0, 3 >( );
      /* Compute new coefficients in the standard polynomial base */
      Concrete::Bezier x0new = k0x;
      Concrete::Bezier x1new = k0x + k1x / 3;
      Concrete::Bezier x2new = k0x + ( 2 * k1x + k2x ) / 3;
      Concrete::Bezier x3new = k0x + k1x + k2x + k3x;
      
      Concrete::Bezier k0y = y0;
      Concrete::Bezier k1y = 3 * t1 * ( y1 - y0 ).offtype< 0, 1 >( );
      Concrete::Bezier k2y = 3 * t1 * t1 * ( y0 - 2 * y1 + y2 ).offtype< 0, 2 >( );
      Concrete::Bezier k3y = t1 * t1 * t1 * ( -y0 + 3 * y1 - 3 * y2 + y3 ).offtype< 0, 3 >( );
      /* Compute new coefficients in the standard polynomial base */
      Concrete::Bezier y0new = k0y;
      Concrete::Bezier y1new = k0y + k1y / 3;
      Concrete::Bezier y2new = k0y + ( 2 * k1y + k2y ) / 3;
      Concrete::Bezier y3new = k0y + k1y + k2y + k3y;

      Concrete::Bezier k0z = z0;
      Concrete::Bezier k1z = 3 * t1 * ( z1 - z0 ).offtype< 0, 1 >( );
      Concrete::Bezier k2z = 3 * t1 * t1 * ( z0 - 2 * z1 + z2 ).offtype< 0, 2 >( );
      Concrete::Bezier k3z = t1 * t1 * t1 * ( -z0 + 3 * z1 - 3 * z2 + z3 ).offtype< 0, 3 >( );
      /* Compute new coefficients in the standard polynomial base */
      Concrete::Bezier z0new = k0z;
      Concrete::Bezier z1new = k0z + k1z / 3;
      Concrete::Bezier z2new = k0z + ( 2 * k1z + k2z ) / 3;
      Concrete::Bezier z3new = k0z + k1z + k2z + k3z;

      /* Now, reverse time again! */

      p1 = new Concrete::PathPoint3D( x3new.offtype< 0, -3 >( ), y3new.offtype< 0, -3 >( ), z3new.offtype< 0, -3 >( ) );
      p1->front_ = new Concrete::Coords3D( x2new.offtype< 0, -3 >( ), y2new.offtype< 0, -3 >( ), z2new.offtype< 0, -3 >( ) );
      p2 = new Concrete::PathPoint3D( x0new.offtype< 0, -3 >( ), y0new.offtype< 0, -3 >( ), z0new.offtype< 0, -3 >( ) );  /* This point must be the same as before. */
      p2->rear_ = new Concrete::Coords3D( x1new.offtype< 0, -3 >( ), y1new.offtype< 0, -3 >( ), z1new.offtype< 0, -3 >( ) );
      p2->front_ = new Concrete::Coords3D( *(*i2)->front_ );
  }
  
  /* At this point we know that the rest of this segment shall be included, since the cuts are not on the
   * same segment.
   */
  
  for( ; t + 1 < gt2; t += Concrete::UNIT_TIME )
    {
      res->push_back( p1 );
      p1 = p2;
      i1 = i2;
      ++i2;
      if( i2 == end( ) )
	{
	  i2 = begin( );
	}
      p2 = new Concrete::PathPoint3D( **i2 );      
    }

  /* Remember to delete p1 and p2 if unused! */

  Concrete::Time t2 = gt2 - t;
  if( t2 == 1 )
    {
      res->push_back( p1 );
      res->push_back( p2 );
    }
  else
    {
      Concrete::Length xn1 = p1->rear_->x_; /* "n" for negative index.  We save these values now, so that we can delete p1 and p2 soon. */
      Concrete::Length yn1 = p1->rear_->y_;
      Concrete::Length zn1 = p1->rear_->z_;

      Concrete::Bezier x0 = p1->mid_->x_.offtype< 0, 3 >( );
      Concrete::Bezier y0 = p1->mid_->y_.offtype< 0, 3 >( );
      Concrete::Bezier z0 = p1->mid_->z_.offtype< 0, 3 >( );
      Concrete::Bezier x1 = p1->front_->x_.offtype< 0, 3 >( );
      Concrete::Bezier y1 = p1->front_->y_.offtype< 0, 3 >( );
      Concrete::Bezier z1 = p1->front_->z_.offtype< 0, 3 >( );
      Concrete::Bezier x2 = p2->rear_->x_.offtype< 0, 3 >( );
      Concrete::Bezier y2 = p2->rear_->y_.offtype< 0, 3 >( );
      Concrete::Bezier z2 = p2->rear_->z_.offtype< 0, 3 >( );
      Concrete::Bezier x3 = p2->mid_->x_.offtype< 0, 3 >( );
      Concrete::Bezier y3 = p2->mid_->y_.offtype< 0, 3 >( );
      Concrete::Bezier z3 = p2->mid_->z_.offtype< 0, 3 >( );

      delete p1;
      delete p2;

      /* Compute new x polynomial using variable change t = t2 * tNew, where tNew is a time parameter in in the range [ 0, 1 ].
       */
      Concrete::Bezier k0x = x0;
      Concrete::Bezier k1x = 3 * t2 * ( x1 - x0 ).offtype< 0, 1 >( );
      Concrete::Bezier k2x = 3 * t2 * t2 * ( x0 - 2 * x1 + x2 ).offtype< 0, 2 >( );
      Concrete::Bezier k3x = t2 * t2 * t2 * ( -x0 + 3 * x1 - 3 * x2 + x3 ).offtype< 0, 3 >( );
      /* Compute new coefficients in the standard polynomial base */
      Concrete::Bezier x0new = k0x;
      Concrete::Bezier x1new = k0x + k1x / 3;
      Concrete::Bezier x2new = k0x + ( 2 * k1x + k2x ) / 3;
      Concrete::Bezier x3new = k0x + k1x + k2x + k3x;
      
      Concrete::Bezier k0y = y0;
      Concrete::Bezier k1y = 3 * t2 * ( y1 - y0 ).offtype< 0, 1 >( );
      Concrete::Bezier k2y = 3 * t2 * t2 * ( y0 - 2 * y1 + y2 ).offtype< 0, 2 >( );
      Concrete::Bezier k3y = t2 * t2 * t2 * ( -y0 + 3 * y1 - 3 * y2 + y3 ).offtype< 0, 3 >( );

      Concrete::Bezier y0new = k0y;
      Concrete::Bezier y1new = k0y + k1y / 3;
      Concrete::Bezier y2new = k0y + ( 2 * k1y + k2y ) / 3;
      Concrete::Bezier y3new = k0y + k1y + k2y + k3y;
      
      Concrete::Bezier k0z = z0;
      Concrete::Bezier k1z = 3 * t2 * ( z1 - z0 ).offtype< 0, 1 >( );
      Concrete::Bezier k2z = 3 * t2 * t2 * ( z0 - 2 * z1 + z2 ).offtype< 0, 2 >( );
      Concrete::Bezier k3z = t2 * t2 * t2 * ( -z0 + 3 * z1 - 3 * z2 + z3 ).offtype< 0, 3 >( );

      Concrete::Bezier z0new = k0z;
      Concrete::Bezier z1new = k0z + k1z / 3;
      Concrete::Bezier z2new = k0z + ( 2 * k1z + k2z ) / 3;
      Concrete::Bezier z3new = k0z + k1z + k2z + k3z;
      
      p1 = new Concrete::PathPoint3D( x0new.offtype< 0, -3 >( ), y0new.offtype< 0, -3 >( ), z0new.offtype< 0, -3 >( ) );
      p1->front_ = new Concrete::Coords3D( x1new.offtype< 0, -3 >( ), y1new.offtype< 0, -3 >( ), z1new.offtype< 0, -3 >( ) );
      p1->rear_ = new Concrete::Coords3D( xn1, yn1, zn1 );
      p2 = new Concrete::PathPoint3D( x3new.offtype< 0, -3 >( ), y3new.offtype< 0, -3 >( ), z3new.offtype< 0, -3 >( ) );
      p2->rear_ = new Concrete::Coords3D( x2new.offtype< 0, -3 >( ), y2new.offtype< 0, -3 >( ), z2new.offtype< 0, -3 >( ) );

      res->push_back( p1 );
      res->push_back( p2 );
  }


  return RefCountPtr< const Lang::ElementaryPath3D >( res );
}


RefCountPtr< const Lang::ElementaryPath3D >
Lang::ElementaryPath3D::reverse( ) const
{
  Lang::ElementaryPath3D * res = new Lang::ElementaryPath3D;
  if( closed_ )
    {
      res->close( );
    }

  for( const_iterator i = begin( ); i != end( ); ++i )
    {
      Concrete::PathPoint3D * newPoint = new Concrete::PathPoint3D( new Concrete::Coords3D( *((*i)->mid_) ) );
      if( (*i)->rear_ != (*i)->mid_ )
	{
	  newPoint->front_ = new Concrete::Coords3D( *((*i)->rear_) );
	}
      if( (*i)->front_ != (*i)->mid_ )
	{
	  newPoint->rear_ = new Concrete::Coords3D( *((*i)->front_) );
	}
      res->push_front( newPoint );
    }

  return RefCountPtr< const Lang::ElementaryPath3D >( res );
}

void
Lang::ElementaryPath3D::show( std::ostream & os ) const
{
  os << "Elementary 3D subpath with " << size( ) << " path points" ;
}

void
Lang::ElementaryPath3D::getRepresentativePoints( const Lang::Transform3D & tf, Concrete::Coords3D * p0, Concrete::Coords3D * p1, Concrete::Coords3D * p2 ) const
{
  // We do this by selecting three points that spans a large triangle
  // This procedure may be overly expensive...
  // ... perhaps because it is hard to make it smart.
  
  Concrete::Area bestArea( 0 );

  for( const_iterator i0 = begin( ); ; )
    {
      Concrete::Coords3D tmp0( (*i0)->mid_->transformed( tf ) );
      const_iterator i1 = i0;
      ++i1;
      for( ; ; )
	{
	  Concrete::Coords3D tmp1( (*i1)->mid_->transformed( tf ) );
	  const_iterator i2 = i1;
	  ++i2;
	  for( ; i2 != end( ); ++i2 )
	    {
	      Concrete::Coords3D tmp2( (*i2)->mid_->transformed( tf ) );
	      Concrete::Area a = Computation::triangleArea( tmp0, tmp1, tmp2 );
	      if( a > bestArea )
		{
		  *p0 = tmp0;
		  *p1 = tmp1;
		  *p2 = tmp2;
		  bestArea = a;
		}		    
	    }
	  --i2;  // now i2 points to the element before end
	  ++i1;
	  if( i1 == i2 )
	    {
	      break;
	    }
	}
      --i1;  // now i1 points to the second last element
      ++i0;
      if( i0 == i1 )
	{
	  break;
	}
    }
}


void
Lang::ElementaryPath3D::gcMark( Kernel::GCMarkedSet & marked )
{ }

RefCountPtr< const Lang::ElementaryPath2D >
Lang::ElementaryPath3D::make2D( Concrete::Length eyez ) const
{
  Lang::ElementaryPath2D * res = new Lang::ElementaryPath2D;
  if( closed_ )
    {
      res->close( );
    }

  if( eyez == HUGE_VAL )
    {
      for( const_iterator i = begin( ); i != end( ); ++i )
	{
	  Concrete::PathPoint2D * newPoint = new Concrete::PathPoint2D( (*i)->mid_->make2D( eyez ) );
	  if( (*i)->rear_ != (*i)->mid_ )
	    {
	      newPoint->rear_ = (*i)->rear_->make2D( eyez );
	    }
	  if( (*i)->front_ != (*i)->mid_ )
	    {
	      newPoint->front_ = (*i)->front_->make2D( eyez );
	    }
	  res->push_back( newPoint );
	}
    }
  else
    {
      const_iterator i1 = begin( );
      const_iterator i2 = i1;
      ++i2;

      Lang::ElementaryPath2D cycleSegment;
      Concrete::Coords2D * passedRear_ = 0;

      if( closed_ )
	{
	  const_iterator last = end( );
	  --last;
	  makeSegment2D( & cycleSegment, & passedRear_, (*last)->mid_, (*last)->front_, (*i1)->rear_, (*i1)->mid_, eyez );
	}

      for( ; i2 != end( ); ++i1, ++i2 )
	{
	  makeSegment2D( res, & passedRear_, (*i1)->mid_, (*i1)->front_, (*i2)->rear_, (*i2)->mid_, eyez );
	}

      if( closed_ )
	{
	  if( passedRear_ != 0 )
	    {
	      cycleSegment.front( )->rear_ = passedRear_;
	    }
	  while( cycleSegment.size( ) > 0 )
	    {
	      res->push_back( cycleSegment.front( ) );
	      cycleSegment.pop_front( );
	    }
	}
      else
	{
	  Concrete::PathPoint2D * newPoint = new Concrete::PathPoint2D( (*i1)->mid_->make2D( eyez ) );
	  if( passedRear_ != 0 )
	    {
	      newPoint->rear_ = passedRear_;
	    }
	  if( (*i1)->front_ != (*i1)->mid_ )
	    {
	      newPoint->front_ = (*i1)->front_->make2D( eyez );
	    }
	  res->push_back( newPoint );
	}
    }

  return RefCountPtr< const Lang::ElementaryPath2D >( res );  
}

void
Lang::ElementaryPath3D::makeSegment2D( Lang::ElementaryPath2D * dst, Concrete::Coords2D ** passedRear_, const Concrete::Coords3D * _p0, const Concrete::Coords3D * _p1, const Concrete::Coords3D * _p2, const Concrete::Coords3D * _p3, Concrete::Length eyez )
{
  if( _p1 == _p0 && _p2 == _p3 )
    {
      Concrete::PathPoint2D * newPoint = new Concrete::PathPoint2D( _p0->make2D( eyez ) );
      if( *passedRear_ != 0 )
	{
	  newPoint->rear_ = *passedRear_;
	  *passedRear_ = 0;
	}
      dst->push_back( newPoint );      
      return;
    }

  Concrete::Coords3D p0 = *_p0;
  std::stack< Concrete::Coords3D > pointStack;
  pointStack.push( *_p3 );
  pointStack.push( *_p2 );
  pointStack.push( *_p1 );

  while( pointStack.size( ) > 2 )
    {
      Concrete::Coords3D p1 = pointStack.top( );
      pointStack.pop( );
      Concrete::Coords3D p2 = pointStack.top( );
      pointStack.pop( );
      Concrete::Coords3D p3 = pointStack.top( );
      // Note that p3 is not popped!
      
      if( viewDistortionTooBig( p0, p1, p2, p3, eyez, 0.003 ) )
	{
	  Bezier::PolyCoeffs< Concrete::Coords3D > poly( Bezier::ControlPoints< Concrete::Coords3D >( p0, p1, p2, p3 ) );
	  Bezier::ControlPoints< Concrete::Coords3D > sub1( poly.subSection( 0, 0.5 ) );
	  Bezier::ControlPoints< Concrete::Coords3D > sub2( poly.subSection( 0.5, 1 ) );
	  pointStack.push( sub2.p2_ );
	  pointStack.push( sub2.p1_ );
	  pointStack.push( sub2.p0_ );
	  pointStack.push( sub1.p2_ );
	  pointStack.push( sub1.p1_ );
	}
      else
	{
	  Concrete::PathPoint2D * newPoint = new Concrete::PathPoint2D( p0.make2D( eyez ) );
	  if( *passedRear_ != 0 )
	    {
	      newPoint->rear_ = *passedRear_;
	    }
	  newPoint->front_ = p1.make2D( eyez );
	  dst->push_back( newPoint );
	  
	  *passedRear_ = p2.make2D( eyez );
	  p0 = p3;
	  pointStack.pop( );
	}
    }
}

double
Lang::ElementaryPath3D::viewDistortionTooBig( const Concrete::Coords3D & p0, const Concrete::Coords3D & p1, const Concrete::Coords3D & p2, const Concrete::Coords3D & p3, Concrete::Length eyez, double _threshold )
{
  Bezier::ControlPoints< Concrete::Coords3D > bezier3D( p0, p1, p2, p3 );

  Concrete::Coords2D p02D = p0.make2DAutomatic( eyez );
  Concrete::Coords2D p12D = p1.make2DAutomatic( eyez );
  Concrete::Coords2D p22D = p2.make2DAutomatic( eyez );
  Concrete::Coords2D p32D = p3.make2DAutomatic( eyez );

  Bezier::ControlPoints< Concrete::Coords2D > bezier2D( p02D, p12D, p22D, p32D );

  Concrete::Length threshold = 0;
  threshold += hypotPhysical( p12D.x_ - p02D.x_, p12D.y_ - p02D.y_ );
  threshold += hypotPhysical( p22D.x_ - p12D.x_, p22D.y_ - p12D.y_ );
  threshold += hypotPhysical( p32D.x_ - p22D.x_, p32D.y_ - p22D.y_ );
  if( threshold == Concrete::ZERO_LENGTH )
    {
      // The path is degenerate, so it isn't distorted at all.
      return false;
    }
  threshold *= _threshold;

  for( double t = 0.25; t < 0.9; t += 0.25 )
    {
      Concrete::Coords2D p3D = bezier3D.point( t ).make2DAutomatic( eyez );
      Concrete::Coords2D p2D = bezier2D.point( t );
      if( hypotPhysical( p3D.x_ - p2D.x_, p3D.y_ - p2D.y_ ) > threshold )
	{
	  return true;
	}
    }
  return false;
}

void
Lang::ElementaryPath3D::dashifyIn2D( RefCountPtr< const Lang::Group2D > * res, Concrete::Length eyez, const RefCountPtr< const Kernel::GraphicsState > & metaState ) const
{
  if( size( ) == 0 )
    {
      return;
    }

  Kernel::GraphicsState solidValue;
  solidValue.dash_ = Lang::THE_SOLID_DASH;
  RefCountPtr< const Kernel::GraphicsState > solidState( new Kernel::GraphicsState( solidValue, *metaState ) );

  Lang::Dash::Iterator dashi = metaState->dash_->begin( );
  
  Lang::ElementaryPath3D newPath;

  Concrete::Length remainingLength = dashi.getLength( );

  const_iterator i1 = begin( );

  Concrete::Coords3D * passedRear_ = 0;
  if( (*i1)->rear_ != (*i1)->mid_ )
    {
      passedRear_ = new Concrete::Coords3D( *( (*i1)->rear_ ) );
    }

  Concrete::Coords3D * lastMid_ = 0;

  const_iterator i2 = i1;
  ++i2;
  for( ; i1 != end( ); ++i1, ++i2 )
    {
      if( i2 == end( ) )
	{
	  if( closed_ )
	    {
	      i2 = begin( );
	      lastMid_ = new Concrete::Coords3D( *( (*i2)->mid_ ) );
	    }
	  else
	    {
	      lastMid_ = new Concrete::Coords3D( *( (*i1)->mid_ ) );
	      break;
	    }
	}
      dashifySegment( res, newPath, remainingLength, & passedRear_,
		      (*i1)->mid_, (*i1)->front_, (*i2)->rear_, (*i2)->mid_,
		      eyez,
		      solidState,
		      dashi );
    }
  
  if( dashi.isOn( ) )
    {
      Concrete::PathPoint3D * newPoint = new Concrete::PathPoint3D( lastMid_ );
      if( passedRear_ != 0 )
	{
	  newPoint->rear_ = passedRear_;
	  passedRear_ = 0;
	}
      newPath.push_back( newPoint );
      *res = RefCountPtr< const Lang::Group2D >( new Lang::GroupPair2D( RefCountPtr< Lang::Drawable2D >( new Lang::PaintedPath2D( solidState,
																	      newPath.make2D( eyez ),
																	      "S" ) ),
									      *res,
									      metaState ) );
      newPath.clear( );
    }
  else
    {
      delete lastMid_;
      if( passedRear_ != 0 )
	{
	  throw Exceptions::InternalError( "passedRear_ != 0, but the dash iterator is not on." );
	  // If this wasn't an error situation, passedRear_ should be deleted here.
	}
      if( newPath.size( ) > 0 )
	{
	  throw Exceptions::InternalError( "There are points in the newPath, but the dash iterator is not on." );
	}
    }
}

void
Lang::ElementaryPath3D::dashifySegment( RefCountPtr< const Lang::Group2D > * res,
					   Lang::ElementaryPath3D & newPath,
					   Concrete::Length remainingLength,
					   Concrete::Coords3D ** passedRear_,
					   const Concrete::Coords3D * p0, const Concrete::Coords3D * p1,
					   const Concrete::Coords3D * p2, const Concrete::Coords3D * p3,
					   Concrete::Length eyez,
					   const RefCountPtr< const Kernel::GraphicsState > & solidState,
					   Lang::Dash::Iterator & dashi )
{
  if( p1 == p0 &&
      p2 == p3 )
    {
      if( dashi.isOn( ) )
	{
	  Concrete::PathPoint3D * newPoint = new Concrete::PathPoint3D( new Concrete::Coords3D( *p0 ) );
	  if( *passedRear_ != 0 )
	    {
	      newPoint->rear_ = *passedRear_;
	      *passedRear_ = 0;
	    }
	  newPath.push_back( newPoint );
	  // Note that on a straight segment, we don't set **passedRear_ to *p2
	}
      
      const Concrete::Length segLength = hypotPhysical( p0->x_ - p3->x_, p0->y_ - p3->y_, p0->z_ - p3->z_ );
      if( segLength < remainingLength )
	{
	  remainingLength -= segLength;
	  return;
	}

      Concrete::Length l = remainingLength;
      {
	if( dashi.isOn( ) )
	  {
	    Concrete::PathPoint3D * newPoint = new Concrete::PathPoint3D( new Concrete::Coords3D( p0->x_ + ( p3->x_ - p0->x_ ) * l / segLength,
															     p0->y_ + ( p3->y_ - p0->y_ ) * l / segLength,
															     p0->z_ + ( p3->z_ - p0->z_ ) * l / segLength ) );
	    // there can be no passedRear_ here
	    newPath.push_back( newPoint );
	    *res = RefCountPtr< const Lang::Group2D >( new Lang::GroupPair2D( RefCountPtr< Lang::Drawable2D >( new Lang::PaintedPath2D( solidState,
																		    newPath.make2D( eyez ),
																		    "S" ) ),
										    *res,
										    solidState ) );
	    newPath.clear( );
	  }
      }
      
      while( l <= segLength )
	{
	  ++dashi;
	  if( newPath.size( ) > 0 )
	    {
	      Concrete::PathPoint3D * newPoint = new Concrete::PathPoint3D( new Concrete::Coords3D( p0->x_ + ( p3->x_ - p0->x_ ) * l / segLength,
															       p0->y_ + ( p3->y_ - p0->y_ ) * l / segLength,
															       p0->z_ + ( p3->z_ - p0->z_ ) * l / segLength ) );
	      // there can be no passedRear_ here
	      newPath.push_back( newPoint );
	      *res = RefCountPtr< const Lang::Group2D >( new Lang::GroupPair2D( RefCountPtr< Lang::Drawable2D >( new Lang::PaintedPath2D( solidState,
																		      newPath.make2D( eyez ),
																		      "S" ) ),
										      *res,
										      
										      solidState ) );
	      newPath.clear( );
	    }
	  
	  if( dashi.isOn( ) )
	    {
	      Concrete::PathPoint3D * newPoint = new Concrete::PathPoint3D( new Concrete::Coords3D( p0->x_ + ( p3->x_ - p0->x_ ) * l / segLength,
															       p0->y_ + ( p3->y_ - p0->y_ ) * l / segLength,
															       p0->z_ + ( p3->z_ - p0->z_ ) * l / segLength ) );
	      // there can be no passedRear_ here
	      newPath.push_back( newPoint );
	    }
	  
	  l += dashi.getLength( );
	}
      
      remainingLength = l - segLength;
      return;
    }
  
  Bezier::ControlPoints< Concrete::Coords3D > controls( *p0, *p1, *p2, *p3 );
  Bezier::PolyCoeffs< Concrete::Coords3D > poly( controls );

  Concrete::Bezier x0 = p0->x_.offtype< 0, 3 >( );
  Concrete::Bezier y0 = p0->y_.offtype< 0, 3 >( );
  Concrete::Bezier z0 = p0->z_.offtype< 0, 3 >( );
  Concrete::Bezier x1 = p1->x_.offtype< 0, 3 >( );
  Concrete::Bezier y1 = p1->y_.offtype< 0, 3 >( );
  Concrete::Bezier z1 = p1->z_.offtype< 0, 3 >( );
  Concrete::Bezier x2 = p2->x_.offtype< 0, 3 >( );
  Concrete::Bezier y2 = p2->y_.offtype< 0, 3 >( );
  Concrete::Bezier z2 = p2->z_.offtype< 0, 3 >( );
  Concrete::Bezier x3 = p3->x_.offtype< 0, 3 >( );
  Concrete::Bezier y3 = p3->y_.offtype< 0, 3 >( );
  Concrete::Bezier z3 = p3->z_.offtype< 0, 3 >( );
  
  const Concrete::Length segLengthBound = ( hypotPhysical( x1-x0, y1-y0, z1-z0 ) + hypotPhysical( x2-x1, y2-y1, z2-z1 ) + hypotPhysical( x3-x2, y3-y2, z3-z2 ) ).offtype< 0, -3 >( );
  const Concrete::Time dt = MetaPDF::computeDt( segLengthBound );
  
  Concrete::Time t1 = 0;
  laterArcTime( x0, y0, z0, x1, y1, z1, x2, y2, z2, x3, y3, z3, & t1, & remainingLength, dt );
  if( t1 > 1 )
    {
      if( dashi.isOn( ) )
	{
	  Concrete::PathPoint3D * newPoint = new Concrete::PathPoint3D( new Concrete::Coords3D( *p0 ) );
	  if( *passedRear_ != 0 )
	    {
	      newPoint->rear_ = *passedRear_;
	      *passedRear_ = 0;
	    }
	  newPoint->front_ = new Concrete::Coords3D( *p1 );
	  newPath.push_back( newPoint );
	  *passedRear_ = new Concrete::Coords3D( *p2 );
	}
      return;
    }

  if( dashi.isOn( ) )
    {
      Bezier::ControlPoints< Concrete::Coords3D > stroke( poly.subSection( 0, t1.offtype< 0, 1 >( ) ) );
      {
	Concrete::PathPoint3D * newPoint = new Concrete::PathPoint3D( new Concrete::Coords3D( *p0 ) );
	if( *passedRear_ != 0 )
	  {
	    newPoint->rear_ = *passedRear_;
	    *passedRear_ = 0;
	  }
	newPoint->front_ = new Concrete::Coords3D( stroke.p1_ );
	newPath.push_back( newPoint );
      }
      {
	Concrete::PathPoint3D * newPoint = new Concrete::PathPoint3D( new Concrete::Coords3D( stroke.p3_ ) );
	newPoint->rear_ = new Concrete::Coords3D( stroke.p2_ );
	newPath.push_back( newPoint );
      }
      *res = RefCountPtr< const Lang::Group2D >( new Lang::GroupPair2D( RefCountPtr< Lang::Drawable2D >( new Lang::PaintedPath2D( solidState,
																	      newPath.make2D( eyez ),
																	      "S" ) ),
									      *res,
									      solidState ) );
      newPath.clear( );
    }
  
  while( true )
    {
      ++dashi;
      remainingLength = dashi.getLength( );
      Concrete::Time t0 = t1;
      laterArcTime( x0, y0, z0, x1, y1, z1, x2, y2, z2, x3, y3, z3, & t1, & remainingLength, dt );
      if( t1 > 1 )
	{
	  if( dashi.isOn( ) )
	    {
	      Bezier::ControlPoints< Concrete::Coords3D > stroke( poly.subSection( t0.offtype< 0, 1 >( ), 1 ) );
	      Concrete::PathPoint3D * newPoint = new Concrete::PathPoint3D( new Concrete::Coords3D( stroke.p0_ ) );
	      newPoint->front_ = new Concrete::Coords3D( stroke.p1_ );
	      newPath.push_back( newPoint );
	      *passedRear_ = new Concrete::Coords3D( stroke.p2_ );
	    }  
	  return;
	}

      if( dashi.isOn( ) )
	{
	  Bezier::ControlPoints< Concrete::Coords3D > stroke( poly.subSection( t0.offtype< 0, 1 >( ), t1.offtype< 0, 1 >( ) ) );
	  {
	    Concrete::PathPoint3D * newPoint = new Concrete::PathPoint3D( new Concrete::Coords3D( stroke.p0_ ) );
	    // there can be no *passedRear_ here
	    newPoint->front_ = new Concrete::Coords3D( stroke.p1_ );
	    newPath.push_back( newPoint );
	  }
	  {
	    Concrete::PathPoint3D * newPoint = new Concrete::PathPoint3D( new Concrete::Coords3D( stroke.p3_ ) );
	    newPoint->rear_ = new Concrete::Coords3D( stroke.p2_ );
	    newPath.push_back( newPoint );
	  }
	  *res = RefCountPtr< const Lang::Group2D >( new Lang::GroupPair2D( RefCountPtr< Lang::Drawable2D >( new Lang::PaintedPath2D( solidState,
																		  newPath.make2D( eyez ),
																		  "S" ) ),
										  *res,
										  solidState ) );
	  newPath.clear( );
	}
    }
}

void
Lang::ElementaryPath3D::laterArcTime( const Concrete::Bezier & x0, const Concrete::Bezier & y0, const Concrete::Bezier & z0, const Concrete::Bezier & x1, const Concrete::Bezier & y1, const Concrete::Bezier & z1, const Concrete::Bezier & x2, const Concrete::Bezier & y2, const Concrete::Bezier & z2, const Concrete::Bezier & x3, const Concrete::Bezier & y3, const Concrete::Bezier & z3, Concrete::Time * _t, Concrete::Length * l, const Concrete::Time & dt )
{
  Physical< 1, -1 > tmpSum_l = *l / dt;
  for( Concrete::Time t = *_t; t < 1; t += dt )
    {
      Concrete::Time tc = Concrete::UNIT_TIME - t; /* complement to t */
      Physical< 0, 2 > kv0 = -3 * tc * tc;
      Physical< 0, 2 > kv1 = 3 * tc * tc - 6 * tc * t;
      Physical< 0, 2 > kv2 = 6 * tc * t - 3 * t * t;
      Physical< 0, 2 > kv3 = 3 * t * t;
      Concrete::Speed vx = x0 * kv0 + x1 * kv1 + x2 * kv2 + x3 * kv3;
      Concrete::Speed vy = y0 * kv0 + y1 * kv1 + y2 * kv2 + y3 * kv3;
      Concrete::Speed vz = z0 * kv0 + z1 * kv1 + z2 * kv2 + z3 * kv3;
      
      Concrete::Length dl = hypotPhysical( vx, vy, vz ).offtype< 0, -1 >( );
      tmpSum_l -= dl.offtype< 0, 1 >( );
      if( tmpSum_l < 0 )
	{
	  *_t = t - ( tmpSum_l / dl.offtype< 0, 1 >( ) ) * dt;
	  *l = 0;
	  return;
	}
    }
  *_t = HUGE_VAL;
  *l = tmpSum_l * dt;
}

