#include <cmath>

#include "metapdftypes.h"
#include "metapdfexceptions.h"
#include "astexpr.h"
#include "consts.h"
#include "globals.h"
#include "angleselect.h"
#include "bezier.h"
#include "upsamplers.h"
#include "constructorrepresentation.h"

#include <ctype.h>
#include <stack>
#include <algorithm>

using namespace MetaPDF;
using namespace std;


inline
bool
isinfPhysical( const double & p1 )
{
  return isinf( p1 );
}

inline
double
modPhysical( const double & p1, const double & p2 )
{
  return fmod( p1, p2 );
}

Lang::ElementaryPath2D::ElementaryPath2D( )
  : allComplete_( true )
{ }

DISPATCHIMPL( ElementaryPath2D );

Lang::ElementaryPath2D::~ElementaryPath2D( )
{ }

Kernel::VariableHandle
Lang::ElementaryPath2D::getField( const char * fieldID, const RefCountPtr< const Lang::Value > & selfRef ) const
{
  return getField( fieldID, selfRef.down_cast< const Lang::ElementaryPath2D >( ) );
}

Kernel::VariableHandle
Lang::ElementaryPath2D::getField( const char * fieldID, const RefCountPtr< const Lang::ElementaryPath2D > & selfRef ) const
{
  if( strcmp( fieldID, "begin" ) == 0 )
    {
      return Helpers::newValHandle( new Lang::PathSlider2D( selfRef, Concrete::ZERO_TIME ) );
    }
  if( strcmp( fieldID, "end" ) == 0 )
    {
      return Helpers::newValHandle( new Lang::PathSlider2D( selfRef, Concrete::Time( duration( ) ) ) );
    }
  if( strcmp( fieldID, "closed?" ) == 0 )
    {
      if( closed_ )
	{
	  return Kernel::THE_TRUE_VARIABLE;
	}
      return Kernel::THE_FALSE_VARIABLE;
    }
  if( strcmp( fieldID, "null?" ) == 0 )
    {
      if( size( ) == 0 )
	{
	  return Kernel::THE_TRUE_VARIABLE;
	}
      return Kernel::THE_FALSE_VARIABLE;
    }
  throw Exceptions::NonExistentMember( getTypeName( ), fieldID );
}

void
Lang::ElementaryPath2D::writePath( ostream & os ) const
{
  const_iterator i = begin( );
  if( i == end( ) )
    {
      throw Exceptions::InternalError( "Invoking writePath on the empty path." );
    }
  const Concrete::Coords2D * p0 = (*i)->mid_;
  os << *p0 << " m " ;
  const Concrete::Coords2D * p1 = (*i)->front_;
  ++i;
  for( ; i != end( ); ++i )
    {
      const Concrete::Coords2D * p2 = (*i)->rear_;
      const Concrete::Coords2D * p3 = (*i)->mid_;
      if( p1 != p0 && p2 != p3 )
	{
	  os << *p1 << " " << *p2 << " " << *p3 << " c " ;
	}
      else if( p1 != p0 )
	{
	  os << *p1 << " " << *p3 << " y " ;
	}
      else if( p2 != p3 )
	{
	  os << *p2 << " " << *p3 << " v " ;
	}
      else
	{
	  os << *p3 << " l " ;
	}
      p0 = p3;
      p1 = (*i)->front_;
    }
  if( closed_ )
    {
      i = begin( );
      const Concrete::Coords2D * p2 = (*i)->rear_;
      const Concrete::Coords2D * p3 = (*i)->mid_;
      if( p1 != p0 && p2 != p3 )
	{
	  os << *p1 << " " << *p2 << " " << *p3 << " c " ;
	}
      else if( p1 != p0 )
	{
	  os << *p1 << " " << *p3 << " y " ;
	}
      else if( p2 != p3 )
	{
	  os << *p2 << " " << *p3 << " v " ;
	}
      /* In case there is no handle along this segment, it is drawn by the h operator itself.
       */
      os << "h " ;
    }
}

void
Lang::ElementaryPath2D::writeInputForm( ostream & os ) const
{
  for( const_iterator i = begin( ); i != end( ); ++i )
    {
      if( i != begin( ) )
	{
	  os << "--" ;
	}
      if( (*i)->rear_ != (*i)->mid_ )
	{
	  os << Helpers::shapesFormat( *(*i)->rear_ ) << "<" ;
	}
      os << Helpers::shapesFormat( *(*i)->mid_ );
      if( (*i)->front_ != (*i)->mid_ )
	{
	  os << ">" << Helpers::shapesFormat( *(*i)->front_ );
	}
    }
  if( closed_ )
    {
      os << "--cycle" ;
    }
}

RefCountPtr< const Lang::ElementaryPath2D >
Lang::ElementaryPath2D::elementaryTransformed( const Lang::Transform2D & tf ) const
{
  Lang::ElementaryPath2D * res = new Lang::ElementaryPath2D;
  if( closed_ )
    {
      res->close( );
    }
  for( const_iterator i = begin( ); i != end( ); ++i )
    {
      res->push_back( (*i)->transformed( tf ) );
    }
  return RefCountPtr< const Lang::ElementaryPath2D >( res );
}

RefCountPtr< const Lang::ElementaryPath3D >
Lang::ElementaryPath2D::elementaryTransformed( const Lang::Transform3D & tf ) const
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

RefCountPtr< const Lang::SubPath2D >
Lang::ElementaryPath2D::typed_transformed( const Lang::Transform2D & tf ) const
{
  return elementaryTransformed( tf );
}

void
Lang::ElementaryPath2D::elementaryJob( std::stack< const Lang::Value * > * nodeStack, Lang::ElementaryPath2D * pth ) const
{
  for( const_iterator i = begin( ); i != end( ); ++i )
    {
      pth->push_back( new Concrete::PathPoint2D( **i ) );
    }
}

Concrete::Time
Lang::ElementaryPath2D::timeCheck( Concrete::Time t ) const
{
  if( closed_ )
    {
      if( isinfPhysical( t ) )
	{
	  throw Exceptions::OutOfRange( "The time argument must not be infinite for closed_ paths." );
	}
      Concrete::Time tMax( duration( ) );
      t = modPhysical( t, tMax );
      if( t < Concrete::ZERO_TIME )
	{
	  t += tMax;
	}
    }
  else
    {
      if( t < Concrete::ZERO_TIME )
	{
	  throw Exceptions::OutOfRange( "The time argument must be non-negative for open paths." );
	}
      if( t >= Concrete::HUGE_TIME )
	{
	  t = Concrete::Time( duration( ) );
	}
      else if( t > Concrete::Time( duration( ) ) )
	{
	  std::ostringstream msg;
	  msg << "The time argument must not be greater than the duration of the path: "
	      << Concrete::Time::offtype( t ) << " > " << duration( ) << "." ;
	  throw Exceptions::OutOfRange( strrefdup( msg ) );
	}
    }
  return t;
}

void
Lang::ElementaryPath2D::findSegment( Concrete::Time t, Concrete::Time * tRes, const Concrete::PathPoint2D ** p1, const Concrete::PathPoint2D ** p2 ) const
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
Lang::ElementaryPath2D::findSegmentReverse( Concrete::Time t, Concrete::Time * tRes, const Concrete::PathPoint2D ** p1, const Concrete::PathPoint2D ** p2 ) const
{
  t = timeCheck( t );

  const_iterator i = begin( );
  for( ; t > Concrete::ZERO_TIME; t -= Concrete::UNIT_TIME, ++i )
    { }
  *tRes = t + Concrete::UNIT_TIME;
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

RefCountPtr< const Lang::Coords2D >
Lang::ElementaryPath2D::point( Concrete::Time globalTime ) const
{
  if( size( ) == 0 )
    {
      throw Exceptions::OutOfRange( "The empty path has no points at all." );
    }
  Concrete::Time t;
  const Concrete::PathPoint2D * p1;
  const Concrete::PathPoint2D * p2;
  findSegment( globalTime, &t, &p1, &p2 );

  if( t == Concrete::ZERO_TIME )
    {
      return RefCountPtr< const Lang::Coords2D >( new Lang::Coords2D( * p1->mid_ ) );
    }
  if( t == Concrete::UNIT_TIME )
    {
      return RefCountPtr< const Lang::Coords2D >( new Lang::Coords2D( * p2->mid_ ) );
    }
  
  Concrete::Bezier x0 = p1->mid_->x_.offtype< 0, 3 >( );
  Concrete::Bezier y0 = p1->mid_->y_.offtype< 0, 3 >( );
  Concrete::Bezier x1 = p1->front_->x_.offtype< 0, 3 >( );
  Concrete::Bezier y1 = p1->front_->y_.offtype< 0, 3 >( );
  Concrete::Bezier x2 = p2->rear_->x_.offtype< 0, 3 >( );
  Concrete::Bezier y2 = p2->rear_->y_.offtype< 0, 3 >( );
  Concrete::Bezier x3 = p2->mid_->x_.offtype< 0, 3 >( );
  Concrete::Bezier y3 = p2->mid_->y_.offtype< 0, 3 >( );

  Concrete::Time tc = Concrete::UNIT_TIME - t; /* complement to t */
  Physical< 0, 3 > k0 =     tc * tc * tc;
  Physical< 0, 3 > k1 = Concrete::Scalar( 3 ) * tc * tc * t;
  Physical< 0, 3 > k2 = Concrete::Scalar( 3 ) * tc * t  * t;
  Physical< 0, 3 > k3 =     t  * t  * t;
  Concrete::Length x = x0 * k0 + x1 * k1 + x2 * k2 + x3 * k3;
  Concrete::Length y = y0 * k0 + y1 * k1 + y2 * k2 + y3 * k3;
  
  return RefCountPtr< const Lang::Coords2D >( new Lang::Coords2D( x, y ) );
}

RefCountPtr< const Lang::Length >
Lang::ElementaryPath2D::speed( Concrete::Time globalTime ) const
{
  if( size( ) == 0 )
    {
      throw Exceptions::OutOfRange( "The empty path has no speed at all." );
    }
  Concrete::Time t;
  const Concrete::PathPoint2D * p1;
  const Concrete::PathPoint2D * p2;
  findSegment( globalTime, &t, &p1, &p2 );

  if( t <= Concrete::ZERO_TIME )
    {
      /* here, t = 0, tc == 1, so
       */
      Concrete::Bezier x0 = p1->mid_->x_.offtype< 0, 3 >( );
      Concrete::Bezier y0 = p1->mid_->y_.offtype< 0, 3 >( );
      Concrete::Bezier x1 = p1->front_->x_.offtype< 0, 3 >( );
      Concrete::Bezier y1 = p1->front_->y_.offtype< 0, 3 >( );
      Concrete::Speed vx = ( x0 * (-3) + x1 * 3 ).offtype< 0, -2 >( );
      Concrete::Speed vy = ( y0 * (-3) + y1 * 3 ).offtype< 0, -2 >( );
      //      Concrete::Speed vx = x1 - x0;
      //      Concrete::Speed vy = y1 - y0;
      return RefCountPtr< const Lang::Length >( new Lang::Length( hypotPhysical( vx, vy ).offtype< 0, -1 >( ) ) );
    }
  if( t >= Concrete::UNIT_TIME )
    {
      throw Exceptions::InternalError( "t1 == 1 in speed calculation" );
    }
  
  Concrete::Bezier x0 = p1->mid_->x_.offtype< 0, 3 >( );
  Concrete::Bezier y0 = p1->mid_->y_.offtype< 0, 3 >( );
  Concrete::Bezier x1 = p1->front_->x_.offtype< 0, 3 >( );
  Concrete::Bezier y1 = p1->front_->y_.offtype< 0, 3 >( );
  Concrete::Bezier x2 = p2->rear_->x_.offtype< 0, 3 >( );
  Concrete::Bezier y2 = p2->rear_->y_.offtype< 0, 3 >( );
  Concrete::Bezier x3 = p2->mid_->x_.offtype< 0, 3 >( );
  Concrete::Bezier y3 = p2->mid_->y_.offtype< 0, 3 >( );
  
  Concrete::Time tc = Concrete::UNIT_TIME - t; /* complement to t */
  Physical< 0, 2 > kv0 = -3 * tc * tc;
  Physical< 0, 2 > kv1 = 3 * tc * tc - 6 * tc * t;
  Physical< 0, 2 > kv2 = 6 * tc * t - 3 * t * t;
  Physical< 0, 2 > kv3 = 3 * t * t;
  //  Physical< 0, 2 > kv0 = - tc * tc;
  //  Physical< 0, 2 > kv1 = tc * tc - 2 * tc * t;
  //  Physical< 0, 2 > kv2 = 2 * tc * t - t * t;
  //  Physical< 0, 2 > kv3 = t * t;
  Concrete::Speed vx = x0 * kv0 + x1 * kv1 + x2 * kv2 + x3 * kv3;
  Concrete::Speed vy = y0 * kv0 + y1 * kv1 + y2 * kv2 + y3 * kv3;

  return RefCountPtr< const Lang::Length >( new Lang::Length( hypotPhysical( vx, vy ).offtype< 0, -1 >( ) ) );
}

RefCountPtr< const Lang::Length >
Lang::ElementaryPath2D::reverse_speed( Concrete::Time globalTime ) const
{
  if( size( ) == 0 )
    {
      throw Exceptions::OutOfRange( "The empty path has no speed at all." );
    }
  Concrete::Time t;
  const Concrete::PathPoint2D * p1;
  const Concrete::PathPoint2D * p2;
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
      Concrete::Bezier x3 = p2->mid_->x_.offtype< 0, 3 >( );
      Concrete::Bezier y3 = p2->mid_->y_.offtype< 0, 3 >( );
      Concrete::Speed vx = ( x2 * (-3) + x3 * 3 ).offtype< 0, -2 >( );
      Concrete::Speed vy = ( y2 * (-3) + y3 * 3 ).offtype< 0, -2 >( );
      //      Concrete::Speed vx = x3 - x2;
      //      Concrete::Speed vy = y3 - y2;
      return RefCountPtr< const Lang::Length >( new Lang::Length( hypotPhysical( vx, vy ).offtype< 0, -1 >( ) ) );
    }
  
  Concrete::Bezier x0 = p1->mid_->x_.offtype< 0, 3 >( );
  Concrete::Bezier y0 = p1->mid_->y_.offtype< 0, 3 >( );
  Concrete::Bezier x1 = p1->front_->x_.offtype< 0, 3 >( );
  Concrete::Bezier y1 = p1->front_->y_.offtype< 0, 3 >( );
  Concrete::Bezier x2 = p2->rear_->x_.offtype< 0, 3 >( );
  Concrete::Bezier y2 = p2->rear_->y_.offtype< 0, 3 >( );
  Concrete::Bezier x3 = p2->mid_->x_.offtype< 0, 3 >( );
  Concrete::Bezier y3 = p2->mid_->y_.offtype< 0, 3 >( );
  
  Concrete::Time tc = Concrete::UNIT_TIME - t; /* complement to t */
  Physical< 0, 2 > kv0 = -3 * tc * tc;
  Physical< 0, 2 > kv1 = 3 * tc * tc - 6 * tc * t;
  Physical< 0, 2 > kv2 = 6 * tc * t - 3 * t * t;
  Physical< 0, 2 > kv3 = 3 * t * t;
  //  Physical< 0, 2 > kv0 = - tc * tc;
  //  Physical< 0, 2 > kv1 = tc * tc - 2 * tc * t;
  //  Physical< 0, 2 > kv2 = 2 * tc * t - t * t;
  //  Physical< 0, 2 > kv3 = t * t;
  Concrete::Speed vx = x0 * kv0 + x1 * kv1 + x2 * kv2 + x3 * kv3;
  Concrete::Speed vy = y0 * kv0 + y1 * kv1 + y2 * kv2 + y3 * kv3;

  return RefCountPtr< const Lang::Length >( new Lang::Length( hypotPhysical( vx, vy ).offtype< 0, -1 >( ) ) );
}

RefCountPtr< const Lang::FloatPair >
Lang::ElementaryPath2D::direction( Concrete::Time globalTime ) const
{
  if( size( ) == 0 )
    {
      throw Exceptions::OutOfRange( "The empty path has no directions at all." );
    }
  Concrete::Time t;
  const Concrete::PathPoint2D * p1;
  const Concrete::PathPoint2D * p2;
  findSegment( globalTime, &t, &p1, &p2 );

  Bezier::ControlPoints< Concrete::Coords2D > controls( *(p1->mid_), *(p1->front_), *(p2->rear_), *(p2->mid_) );
  Bezier::PolyCoeffs< Concrete::Coords2D > coeffs( controls );

  const Concrete::Length segLengthBound =
    ( controls.p1_ - controls.p0_ ).norm( ) + ( controls.p2_ - controls.p1_ ).norm( ) + ( controls.p3_ - controls.p2_ ).norm( );
  const Concrete::Length shortLength = 1.e-4 * segLengthBound;

  {
    Concrete::Coords2D d = coeffs.velocity( t.offtype< 0, 1 >( ) );
    Concrete::Length v = d.norm( );
    if( v > shortLength )
      {
	return RefCountPtr< const Lang::FloatPair >( new Lang::FloatPair( d.direction( v ) ) );
      }
  }

  /* We reach here if the velocity more or less was vanishing at t.  If we are at the beginning of a segment, the acceleration
   * will be in the direction of the limiting velocity.  If we are at the end, the acceleration will be in the opposite direction
   * of the limiting velocity.
   */

  {
    Concrete::Coords2D d = ( t < 0.5 ? 1 : -1 ) * coeffs.acceleration( t.offtype< 0, 1 >( ) );
    Concrete::Length v = d.norm( );
    if( v > shortLength )
      {
	return RefCountPtr< const Lang::FloatPair >( new Lang::FloatPair( d.direction( v ) ) );
      }
  }
  return RefCountPtr< const Lang::FloatPair >( new Lang::FloatPair( 0, 0 ) );
}

RefCountPtr< const Lang::FloatPair >
Lang::ElementaryPath2D::reverse_direction( Concrete::Time globalTime ) const
{
  if( size( ) == 0 )
    {
      throw Exceptions::OutOfRange( "The empty path has no directions at all." );
    }
  Concrete::Time t;
  const Concrete::PathPoint2D * p1;
  const Concrete::PathPoint2D * p2;
  findSegmentReverse( globalTime, &t, &p1, &p2 );

  Bezier::ControlPoints< Concrete::Coords2D > controls( *(p1->mid_), *(p1->front_), *(p2->rear_), *(p2->mid_) );
  Bezier::PolyCoeffs< Concrete::Coords2D > coeffs( controls );

  const Concrete::Length segLengthBound =
    ( controls.p1_ - controls.p0_ ).norm( ) + ( controls.p2_ - controls.p1_ ).norm( ) + ( controls.p3_ - controls.p2_ ).norm( );
  const Concrete::Length shortLength = 1.e-4 * segLengthBound;

  {
    Concrete::Coords2D d = (-1) * coeffs.velocity( t.offtype< 0, 1 >( ) );
    Concrete::Length v = d.norm( );
    if( v > shortLength )
      {
	return RefCountPtr< const Lang::FloatPair >( new Lang::FloatPair( d.direction( v ) ) );
      }
  }

  /* We reach here if the velocity more or less was vanishing at t.  If we are at the beginning of a segment, the acceleration
   * will be in the direction of the limiting velocity.  If we are at the end, the acceleration will be in the opposite direction
   * of the limiting velocity.
   */

  {
    Concrete::Coords2D d = ( t < 0.5 ? -1 : 1 ) * coeffs.acceleration( t.offtype< 0, 1 >( ) );
    Concrete::Length v = d.norm( );
    if( v > shortLength )
      {
	return RefCountPtr< const Lang::FloatPair >( new Lang::FloatPair( d.direction( v ) ) );
      }
  }
  return RefCountPtr< const Lang::FloatPair >( new Lang::FloatPair( 0, 0 ) );
}

RefCountPtr< const Lang::Length >
Lang::ElementaryPath2D::radiusOfCurvature( Concrete::Time globalTime ) const
{
  if( size( ) == 0 )
    {
      throw Exceptions::OutOfRange( "The empty path has no curvature at all." );
    }
  Concrete::Time t;
  const Concrete::PathPoint2D * p1;
  const Concrete::PathPoint2D * p2;
  findSegment( globalTime, &t, &p1, &p2 );

  if( t <= Concrete::ZERO_TIME )
    {
      /* here, t = 0, tc == 1, so
       */
      Concrete::Bezier x0 = p1->mid_->x_.offtype< 0, 3 >( );
      Concrete::Bezier y0 = p1->mid_->y_.offtype< 0, 3 >( );
      Concrete::Bezier x1 = p1->front_->x_.offtype< 0, 3 >( );
      Concrete::Bezier y1 = p1->front_->y_.offtype< 0, 3 >( );
      Concrete::Bezier x2 = p2->rear_->x_.offtype< 0, 3 >( );
      Concrete::Bezier y2 = p2->rear_->y_.offtype< 0, 3 >( );
      Concrete::Speed vx = ( x0 * (-3) + x1 * 3 ).offtype< 0, -2 >( );
      Concrete::Speed vy = ( y0 * (-3) + y1 * 3 ).offtype< 0, -2 >( );
      Concrete::Acceleration ax = ( x0 * 6 + x2 * 6 ).offtype< 0, -1 >( );
      Concrete::Acceleration ay = ( y0 * 6 + y2 * 6 ).offtype< 0, -1 >( );

      Physical< 2, -3 > denom = vx * ay - vy * ax;
      if( denom == Physical< 2, -3 >( 0 ) )
	{
	  return RefCountPtr< const Lang::Length >( new Lang::Length( Concrete::HUGE_LENGTH ) );
	}
      Concrete::Speed tmp = hypotPhysical( vx, vy );
      return RefCountPtr< const Lang::Length >( new Lang::Length( Concrete::Length( tmp * tmp * tmp / denom ) ) );
    }
  if( t >= Concrete::UNIT_TIME )
    {
      throw Exceptions::InternalError( "t1 == 1 in curvature calculation" );
    }
  
  Concrete::Bezier x0 = p1->mid_->x_.offtype< 0, 3 >( );
  Concrete::Bezier y0 = p1->mid_->y_.offtype< 0, 3 >( );
  Concrete::Bezier x1 = p1->front_->x_.offtype< 0, 3 >( );
  Concrete::Bezier y1 = p1->front_->y_.offtype< 0, 3 >( );
  Concrete::Bezier x2 = p2->rear_->x_.offtype< 0, 3 >( );
  Concrete::Bezier y2 = p2->rear_->y_.offtype< 0, 3 >( );
  Concrete::Bezier x3 = p2->mid_->x_.offtype< 0, 3 >( );
  Concrete::Bezier y3 = p2->mid_->y_.offtype< 0, 3 >( );
  
  Concrete::Time tc = Concrete::UNIT_TIME - t; /* complement to t */
  Physical< 0, 2 > kv0 = -3 * tc * tc;
  Physical< 0, 2 > kv1 = 3 * tc * tc - 6 * tc * t;
  Physical< 0, 2 > kv2 = 6 * tc * t - 3 * t * t;
  Physical< 0, 2 > kv3 = 3 * t * t;
  Concrete::Speed vx = x0 * kv0 + x1 * kv1 + x2 * kv2 + x3 * kv3;
  Concrete::Speed vy = y0 * kv0 + y1 * kv1 + y2 * kv2 + y3 * kv3;
  Physical< 0, 1 > ka0 = 6 * tc;
  Physical< 0, 1 > ka1 = -12 * tc + 6 * t;
  Physical< 0, 1 > ka2 = 6 * tc - 12 * t;
  Physical< 0, 1 > ka3 = 6 * t;
  Concrete::Acceleration ax = x0 * ka0 + x1 * ka1 + x2 * ka2 + x3 * ka3;
  Concrete::Acceleration ay = y0 * ka0 + y1 * ka1 + y2 * ka2 + y3 * ka3;

  Physical< 2, -3 > denom = vx * ay - vy * ax;
  if( denom == Physical< 2, -3 >( 0 ) )
    {
      return RefCountPtr< const Lang::Length >( new Lang::Length( Concrete::HUGE_LENGTH ) );
    }
  Concrete::Speed tmp = hypotPhysical( vx, vy );
  return RefCountPtr< const Lang::Length >( new Lang::Length( tmp * tmp * tmp / denom ) );
}

RefCountPtr< const Lang::Length >
Lang::ElementaryPath2D::reverse_radiusOfCurvature( Concrete::Time globalTime ) const
{
  if( size( ) == 0 )
    {
      throw Exceptions::OutOfRange( "The empty path has no curvature at all." );
    }
  Concrete::Time t;
  const Concrete::PathPoint2D * p1;
  const Concrete::PathPoint2D * p2;
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
      Concrete::Bezier x2 = p2->rear_->x_.offtype< 0, 3 >( );
      Concrete::Bezier y2 = p2->rear_->y_.offtype< 0, 3 >( );
      Concrete::Bezier x3 = p2->mid_->x_.offtype< 0, 3 >( );
      Concrete::Bezier y3 = p2->mid_->y_.offtype< 0, 3 >( );

      Concrete::Speed vx = ( x2 * (-3) + x3 * 3 ).offtype< 0, -2 >( );
      Concrete::Speed vy = ( y2 * (-3) + y3 * 3 ).offtype< 0, -2 >( );
      Concrete::Acceleration ax = ( x1 * 6 + x2 * (-12) + x3 * 6 ).offtype< 0, -1 >( );
      Concrete::Acceleration ay = ( y1 * 6 + y2 * (-12) + y3 * 6 ).offtype< 0, -1 >( );

      Physical< 2, -3 > denom = vx * ay - vy * ax;
      if( denom == Physical< 2, -3 >( 0 ) )
	{
	  return RefCountPtr< const Lang::Length >( new Lang::Length( Concrete::HUGE_LENGTH ) );
	}
      Concrete::Speed tmp = hypotPhysical( vx, vy );
      return RefCountPtr< const Lang::Length >( new Lang::Length( tmp * tmp * tmp / denom ) );
    }
  
  Concrete::Bezier x0 = p1->mid_->x_.offtype< 0, 3 >( );
  Concrete::Bezier y0 = p1->mid_->y_.offtype< 0, 3 >( );
  Concrete::Bezier x1 = p1->front_->x_.offtype< 0, 3 >( );
  Concrete::Bezier y1 = p1->front_->y_.offtype< 0, 3 >( );
  Concrete::Bezier x2 = p2->rear_->x_.offtype< 0, 3 >( );
  Concrete::Bezier y2 = p2->rear_->y_.offtype< 0, 3 >( );
  Concrete::Bezier x3 = p2->mid_->x_.offtype< 0, 3 >( );
  Concrete::Bezier y3 = p2->mid_->y_.offtype< 0, 3 >( );
  
  Concrete::Time tc = Concrete::UNIT_TIME - t; /* complement to t */
  Physical< 0, 2 > kv0 = -3 * tc * tc;
  Physical< 0, 2 > kv1 = 3 * tc * tc - 6 * tc * t;
  Physical< 0, 2 > kv2 = 6 * tc * t - 3 * t * t;
  Physical< 0, 2 > kv3 = 3 * t * t;
  Concrete::Speed vx = x0 * kv0 + x1 * kv1 + x2 * kv2 + x3 * kv3;
  Concrete::Speed vy = y0 * kv0 + y1 * kv1 + y2 * kv2 + y3 * kv3;
  Physical< 0, 1 > ka0 = 6 * tc;
  Physical< 0, 1 > ka1 = -12 * tc + 6 * t;
  Physical< 0, 1 > ka2 = 6 * tc - 12 * t;
  Physical< 0, 1 > ka3 = 6 * t;
  Concrete::Acceleration ax = x0 * ka0 + x1 * ka1 + x2 * ka2 + x3 * ka3;
  Concrete::Acceleration ay = y0 * ka0 + y1 * ka1 + y2 * ka2 + y3 * ka3;

  Physical< 2, -3 > denom = vx * ay - vy * ax;
  if( denom == Physical< 2, -3 >( 0 ) )
    {
      return RefCountPtr< const Lang::Length >( new Lang::Length( Concrete::HUGE_LENGTH ) );
    }
  Concrete::Speed tmp = hypotPhysical( vx, vy );
  return RefCountPtr< const Lang::Length >( new Lang::Length( tmp * tmp * tmp / denom ) );
}

size_t
Lang::ElementaryPath2D::duration( ) const
{
  if( closed_ )
    {
      return size( );
    }
  return size( ) - 1;
}

bool
Lang::ElementaryPath2D::controllingMaximizer( const Lang::FloatPair & d, Lang::Coords2D * dst ) const
{
  if( size( ) == 0 )
    {
      throw Exceptions::OutOfRange( "The empty path cannot be maximized along." );
    }
  Concrete::Length opt = -Concrete::HUGE_LENGTH;
  const double dx = d.x_;
  const double dy = d.y_;
  for( const_iterator i = begin( ); i != end( ); ++i )
    {
      if( (*i)->rear_ != 0 )
	{
	  Concrete::Length y = (*i)->rear_->x_ * dx + (*i)->rear_->y_ * dy;
	  if( y > opt )
	    {
	      opt = y;
	      *dst = *((*i)->rear_);
	    }
	}
      Concrete::Length y = (*i)->mid_->x_ * dx + (*i)->mid_->y_ * dy;
      if( y > opt )
	{
	  opt = y;
	  *dst = *((*i)->mid_);
	}
      if( (*i)->front_ != 0 )
	{
	  Concrete::Length y = (*i)->front_->x_ * dx + (*i)->front_->y_ * dy;
	  if( y > opt )
	    {
	      opt = y;
	      *dst = *((*i)->front_);
	    }
	}
    }
  if( opt == -Concrete::HUGE_LENGTH )
    {
      return false;
    }
  return true;
}

bool
Lang::ElementaryPath2D::boundingRectangle( Concrete::Coords2D * dstll, Concrete::Coords2D * dstur ) const
{
  if( size( ) == 0 )
    {
      throw Exceptions::OutOfRange( "The empty path has no bounding rectangle." );
    }
  Concrete::Length xmin = Concrete::HUGE_LENGTH;
  Concrete::Length ymin = Concrete::HUGE_LENGTH;
  Concrete::Length xmax = -Concrete::HUGE_LENGTH;
  Concrete::Length ymax = -Concrete::HUGE_LENGTH;
  for( const_iterator i = begin( ); i != end( ); ++i )
    {
      if( (*i)->rear_ != 0 )
	{
	  Concrete::Length x = (*i)->rear_->x_;
	  Concrete::Length y = (*i)->rear_->y_;
	  xmin = min( xmin, x );
	  ymin = min( ymin, y );
	  xmax = max( xmax, x );
	  ymax = max( ymax, y );
	}
      Concrete::Length x = (*i)->mid_->x_;
      Concrete::Length y = (*i)->mid_->y_;
      xmin = min( xmin, x );
      ymin = min( ymin, y );
      xmax = max( xmax, x );
      ymax = max( ymax, y );
      if( (*i)->front_ != 0 )
	{
	  Concrete::Length x = (*i)->front_->x_;
	  Concrete::Length y = (*i)->front_->y_;
	  xmin = min( xmin, x );
	  ymin = min( ymin, y );
	  xmax = max( xmax, x );
	  ymax = max( ymax, y );
	}
    }
  dstll->x_ = xmin;
  dstll->y_ = ymin;
  dstur->x_ = xmax;
  dstur->y_ = ymax;

  if( xmin == Concrete::HUGE_LENGTH )
    {
      return false;
    }
  return true;
}

bool
Lang::ElementaryPath2D::lineSegmentIntersection( Concrete::Time * dst, const Concrete::Coords2D & l0, const Concrete::Coords2D & l1 ) const
{
  Concrete::Coords2D rInv = ( l1 - l0 ) * ( 1. / Concrete::innerScalar( l1 - l0, l1 - l0 ) );
  Concrete::Coords2D n( l0.y_ - l1.y_, l1.x_ - l0.x_ );
  double offset = Concrete::innerScalar( n, l0 );
  const_iterator i1 = begin( );
  const_iterator i2 = i1;
  ++i2;
  Concrete::Time res = Concrete::HUGE_TIME;
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
	  Concrete::Coords2D n2( (*i1)->mid_->y_ - (*i2)->mid_->y_, (*i2)->mid_->x_ - (*i1)->mid_->x_ );
	  double offset2 = Concrete::innerScalar( n2, *(*i1)->mid_ );
	  Concrete::Coords2D intersection( 0, 0 ); // dummy initialization
	  double det = ( n.x_ * n2.y_ - n.y_ * n2.x_ ).offtype< 2, 0 >( );
	  if( fabs( det ) < 1e-14 )
	    {
	      if( fabs( offset ) + fabs( offset2 ) < 1e-14 )
		{
		  intersection = Concrete::Coords2D( 0, 0 );
		}
	      else
		{
		  continue;
		}
	    }
	  else
	    {
	      intersection = (1. / det ) * Concrete::Coords2D( n2.y_ * offset - n.y_ * offset2, n.x_ * offset2 - n2.x_ * offset );
	    }
	  Concrete::Time t = MetaPDF::straightLineArcTime( Concrete::innerScalar( rInv, intersection - l0 ) );
	  if( Concrete::ZERO_TIME <= t && t <= Concrete::UNIT_TIME )
	    {
	      Concrete::Time t2( Concrete::innerScalar( rInv, *(*i1)->mid_ + t.offtype< 0, 1 >( ) * *(*i2)->mid_ - l0 ) );
	      if( Concrete::ZERO_TIME <= t2 && t2 <= Concrete::UNIT_TIME )
		{
		  res = min( res, t );
		}
	    }
	  continue;
	}

      Bezier::ControlPoints< Concrete::Coords2D > controls( *(*i1)->mid_, *(*i1)->front_, *(*i2)->rear_, *(*i2)->mid_ );
      Bezier::PolyCoeffs< Concrete::Coords2D > coeffs( controls );
      double tmp2[4];
      coeffs.hyperplaneIntersections( tmp2, n, offset );
      for( const double * it2 = tmp2; *it2 != HUGE_VAL; ++it2 )
	{
	  Concrete::Time t = MetaPDF::straightLineArcTime( Concrete::innerScalar( rInv, coeffs.point( *it2 ) - l0 ) );
	  if( Concrete::ZERO_TIME <= t && t <= Concrete::UNIT_TIME )
	    {
	      res = min( res, t );
	    }
	}
    }

  if( res < Concrete::HUGE_TIME )
    {
      *dst = res;
      return true;
    }
  return false;
}

RefCountPtr< const Lang::Coords2D >
Lang::ElementaryPath2D::controllingMaximizer( const Lang::FloatPair & d ) const
{
  Lang::Coords2D * res = new Lang::Coords2D( Concrete::ZERO_LENGTH, Concrete::ZERO_LENGTH );
  controllingMaximizer( d, res );
  return RefCountPtr< const Lang::Coords2D >( res );
}

RefCountPtr< const Lang::Coords2D >
Lang::ElementaryPath2D::discreteMean( ) const
{
  double count = 0;
  Concrete::Length x( 0 );
  Concrete::Length y( 0 );
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
	}
      ++count;
      x = x + (*i)->mid_->x_;
      y = y + (*i)->mid_->y_;
      if( (*i)->front_ != 0 )
	{
	  ++count;
	  x = x + (*i)->front_->x_;
	  y = y + (*i)->front_->y_;
	}
    }
  return RefCountPtr< const Lang::Coords2D >( new Lang::Coords2D( x / count, y / count ) );
}

Concrete::SplineTime
Lang::ElementaryPath2D::discreteMaximizer( const Lang::FloatPair & d ) const
{
  if( size( ) == 0 )
    {
      throw Exceptions::OutOfRange( "The empty path cannot be maximized along." );
    }
  Concrete::Length opt = -Concrete::HUGE_LENGTH;
  const double dx = d.x_;
  const double dy = d.y_;
  Concrete::SplineTime res = Concrete::ZERO_TIME;
  Concrete::SplineTime t = Concrete::ZERO_TIME;
  for( const_iterator i = begin( ); i != end( ); ++i, ++t )
    {
      Concrete::Length y = (*i)->mid_->x_ * dx + (*i)->mid_->y_ * dy;
      if( y > opt )
	{
	  opt = y;
	  res = t;
	}
    }
  return res;
}

Concrete::SplineTime
Lang::ElementaryPath2D::discreteApproximator( const Lang::Coords2D & p ) const
{
  if( size( ) == 0 )
    {
      throw Exceptions::OutOfRange( "The empty path cannot be maximized along." );
    }
  Concrete::Length bestDist = Concrete::HUGE_LENGTH;
  const Concrete::Length px = p.x_.get( );
  const Concrete::Length py = p.y_.get( );
  Concrete::SplineTime res = Concrete::ZERO_TIME;
  Concrete::SplineTime t = Concrete::ZERO_TIME;
  for( const_iterator i = begin( ); i != end( ); ++i, ++t )
    {
      Concrete::Length dist = hypotPhysical( (*i)->mid_->x_ - px, (*i)->mid_->y_ - py );
      if( dist < bestDist )
	{
	  bestDist = dist;
	  res = t;
	}
    }
  return res;
}

RefCountPtr< const Lang::Coords2D >
Lang::ElementaryPath2D::continuousMean( ) const
{
  if( size( ) == 0 )
    {
      throw Exceptions::OutOfRange( "The empty path cannot be averaged along." );
    }
  Physical< 2, 0 > intx( 0 );
  Physical< 2, 0 > inty( 0 );
  Concrete::Length totlen( 0 );

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
	  Concrete::Length x3 = (*i2)->mid_->x_;
	  Concrete::Length y3 = (*i2)->mid_->y_;
	  Concrete::Length len = hypotPhysical( (x3-x0), (y3-y0) );
	  intx += len * 0.5 * ( x0 + x3 );
	  inty += len * 0.5 * ( y0 + y3 );
	  totlen += len;
	  continue;
	}

      Concrete::Bezier x0 = (*i1)->mid_->x_.offtype< 0, 3 >( );
      Concrete::Bezier y0 = (*i1)->mid_->y_.offtype< 0, 3 >( );
      Concrete::Bezier x1 = (*i1)->front_->x_.offtype< 0, 3 >( );
      Concrete::Bezier y1 = (*i1)->front_->y_.offtype< 0, 3 >( );
      Concrete::Bezier x2 = (*i2)->rear_->x_.offtype< 0, 3 >( );
      Concrete::Bezier y2 = (*i2)->rear_->y_.offtype< 0, 3 >( );
      Concrete::Bezier x3 = (*i2)->mid_->x_.offtype< 0, 3 >( );
      Concrete::Bezier y3 = (*i2)->mid_->y_.offtype< 0, 3 >( );
      
      Concrete::Time dt = MetaPDF::computeDt( hypotPhysical( (x1-x0), (y1-y0) ).offtype< 0, -3 >( ) +
						       hypotPhysical( (x2-x1), (y2-y1) ).offtype< 0, -3 >( ) +
						       hypotPhysical( (x3-x2), (y3-y2) ).offtype< 0, -3 >( ) );

      Physical< 2, 0 > tmpSum_x( 0 );
      Physical< 2, 0 > tmpSum_y( 0 );
      Physical< 1, 0 > tmpSum_l( 0 );
      for( Concrete::Time t = Concrete::ZERO_TIME; t < Concrete::UNIT_TIME; t += dt )
	{
	  Concrete::Time tc = Concrete::UNIT_TIME - t; /* complement to t */
	  Physical< 0, 3 > k0 =     tc * tc * tc;
	  Physical< 0, 3 > k1 = 3 * tc * tc * t;
	  Physical< 0, 3 > k2 = 3 * tc * t  * t;
	  Physical< 0, 3 > k3 =     t  * t  * t;
	  Concrete::Length x = x0 * k0 + x1 * k1 + x2 * k2 + x3 * k3;
	  Concrete::Length y = y0 * k0 + y1 * k1 + y2 * k2 + y3 * k3;
	  Physical< 0, 2 > kv0 = -3 * tc * tc;
	  Physical< 0, 2 > kv1 = 3 * tc * tc - 6 * tc * t;
	  Physical< 0, 2 > kv2 = 6 * tc * t - 3 * t * t;
	  Physical< 0, 2 > kv3 = 3 * t * t;
	  Concrete::Speed vx = x0 * kv0 + x1 * kv1 + x2 * kv2 + x3 * kv3;
	  Concrete::Speed vy = y0 * kv0 + y1 * kv1 + y2 * kv2 + y3 * kv3;

	  Concrete::Length dl = hypotPhysical( vx, vy ).offtype< 0, -1 >( );
	  tmpSum_x += x * dl;
	  tmpSum_y += y * dl;
	  tmpSum_l += dl;
	}
      intx += tmpSum_x * dt.offtype< 0, 1 >( );
      inty += tmpSum_y * dt.offtype< 0, 1 >( );
      totlen += tmpSum_l * dt.offtype< 0, 1 >( );
    }

  return RefCountPtr< const Lang::Coords2D >( new Lang::Coords2D( intx / totlen, inty / totlen ) );
}

Concrete::SplineTime
Lang::ElementaryPath2D::continuousMaximizer( const Lang::FloatPair & dNonUnit ) const
{
  if( size( ) == 0 )
    {
      throw Exceptions::OutOfRange( "The empty path cannot be maximized along." );
    }
  
  Concrete::UnitFloatPair d( dNonUnit.x_, dNonUnit.y_ );
  Concrete::Coords2D dBezier( d.x_, d.y_ ); // The Bezier functions requires the direction to be given with the same type as the spline space.

  Concrete::SplineTime res = Concrete::ZERO_TIME;
  Concrete::Length opt = -Concrete::HUGE_LENGTH;
  Concrete::Time steps = Concrete::ZERO_TIME;
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
	      Concrete::Length v = Concrete::inner( *(*i1)->mid_, d );
	      if( v > opt )
		{
		  opt = v;
		  res = steps;
		}
	      break;
	    }
	}
      Concrete::Length v = Concrete::inner( *(*i1)->mid_, d );
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

      /* First check the control points.  If none of the four control points improves the optimum,
       * searching further is meaningless.  Further, since the mid_ points are allways tested anyway,
       * we can assume that they already are, and conclude that they cannot improve the optimum further.
       * Therefore, there are only the two handles to check.
       */
      {
	if( Concrete::inner( *(*i1)->front_, d ) <= opt &&
	    Concrete::inner( *(*i2)->rear_, d ) <= opt )
	  {
	    continue;
	  }
      }

      Bezier::ControlPoints< Concrete::Coords2D > controls( *(*i1)->mid_, *(*i1)->front_, *(*i2)->rear_, *(*i2)->mid_ );
      Bezier::PolyCoeffs< Concrete::Coords2D > coeffs( controls );
      double optTimes[3];
      coeffs.stationaryPoints( optTimes, dBezier ); // A HUGE_VAL is used as terminator in the result.

      for( double * src = & optTimes[0]; *src != HUGE_VAL; ++src )
	{
	  Concrete::Length v = Concrete::inner( coeffs.point( *src ), d );
	  if( v > opt )
	    {
	      opt = v;
	      res = steps + *src;
	    }
	}
    }

  return res;
}

namespace MetaPDF
{
  class ApproximationPoly2D
  {
    Concrete::Coords2D p_;
    Bezier::PolyCoeffs< Concrete::Coords2D > polyCoeffs_;
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

    Concrete::Speed maxSpeed_;
  public:
    ApproximationPoly2D( const Concrete::Coords2D & p0, const Concrete::Coords2D & p1, const Concrete::Coords2D & p2, const Concrete::Coords2D & p3, const Concrete::Coords2D & p );
    Concrete::Time splitTime( const Concrete::Time t_low, const Concrete::Time t_high, const Concrete::Time t_tol ) const;
    bool isCircular( ) const;
    Concrete::Length distanceAt( Concrete::Time t ) const;
    Physical< 2, 0 > squaredDistanceAt( Concrete::Time t ) const;
    Concrete::Speed maxSpeed( ) const { return maxSpeed_; }
    Bezier::ControlPoints< Concrete::Coords2D > getControls( const Concrete::Time t_low, const Concrete::Time t_high ) const;
    const Concrete::Coords2D & getPoint( ) const;
  };
  class ApproximationSegmentSection2D
  {
    Bezier::ControlPoints< Concrete::Coords2D > controls_;
    const ApproximationPoly2D * baseSeg_;
    Concrete::Time steps_;
    Concrete::Time t0_;
    Concrete::Time t1_;
  public:
    
    ApproximationSegmentSection2D( const MetaPDF::ApproximationPoly2D * baseSeg, Concrete::Time steps, Concrete::Time t0, Concrete::Time t1 );
    ApproximationSegmentSection2D * cutAfter( Concrete::Time t ) const;
    ApproximationSegmentSection2D * cutBefore( Concrete::Time t ) const;
    
    Concrete::Length convexHullDistance( ) const;
    Concrete::Time splitTime( const Concrete::Time t_tol ) const;
    Concrete::Time globalTime( Concrete::Time t ) const;
    Concrete::Length distanceAt( Concrete::Time t ) const;
    Concrete::Speed maxSpeed( ) const;
    Concrete::Time duration( ) const;
  };
}

Concrete::SplineTime
Lang::ElementaryPath2D::continuousApproximator( const Lang::Coords2D & coordPoint ) const
{
  const Concrete::Length DISTANCE_TOL = 0.001 * Computation::the_arcdelta;

  if( size( ) == 0 )
    {
      throw Exceptions::OutOfRange( "The empty path cannot be approximated along." );
    }

  Concrete::Coords2D p( coordPoint.x_.get( ), coordPoint.y_.get( ) );
  PtrOwner_back_Access< std::list< const MetaPDF::ApproximationPoly2D * > > memory;
  typedef std::pair< MetaPDF::ApproximationSegmentSection2D *, Concrete::Length > WorkItem;
  std::list< WorkItem > work;

  Concrete::SplineTime res = Concrete::ZERO_TIME;
  Concrete::Length bestDist = Concrete::HUGE_LENGTH;
  Concrete::Time steps = Concrete::ZERO_TIME;
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
	      Concrete::Length dist = hypotPhysical( (*i1)->mid_->x_ - p.x_, (*i1)->mid_->y_ - p.y_ );
	      if( dist < bestDist )
		{
		  bestDist = dist;
		  res = steps;
		}
	      break;
	    }
	}

      Concrete::Length dist = hypotPhysical( (*i1)->mid_->x_ - p.x_, (*i1)->mid_->y_ - p.y_ );
      if( dist < bestDist )
	{
	  bestDist = dist;
	  res = steps;
	}

      Concrete::Bezier x0 = (*i1)->mid_->x_.offtype< 0, 3 >( );
      Concrete::Bezier y0 = (*i1)->mid_->y_.offtype< 0, 3 >( );
      Concrete::Bezier x3 = (*i2)->mid_->x_.offtype< 0, 3 >( );
      Concrete::Bezier y3 = (*i2)->mid_->y_.offtype< 0, 3 >( );

      /* if the segment is straight, finding the closest point is a linear least-squares problem.
       */
      if( (*i1)->front_ == (*i1)->mid_ &&
	  (*i2)->rear_ == (*i2)->mid_ )
	{
	  const Concrete::Length tx = ( x3 - x0 ).offtype< 0, -3 >( );
	  const Concrete::Length ty = ( y3 - y0 ).offtype< 0, -3 >( );
	  double s = ( tx * ( p.x_ - x0.offtype< 0, -3 >( ) ) + ty * ( p.y_ - y0.offtype< 0, -3 >( ) ) ) / ( tx*tx + ty*ty );
	  if( 0 < s && s < 1 )
	    {
	      Concrete::Length dist = hypotPhysical( x0.offtype< 0, -3 >( ) + s * tx - p.x_, y0.offtype< 0, -3 >( ) + s * ty - p.y_ );
	      if( dist < bestDist )
		{
		  bestDist = dist;
		  res = steps + MetaPDF::straightLineArcTime( s );
		}
	    }
	  continue;
	}

      MetaPDF::ApproximationPoly2D * coeffs =
	new MetaPDF::ApproximationPoly2D( *((*i1)->mid_), *((*i1)->front_), *((*i2)->rear_), *((*i2)->mid_), p );
      MetaPDF::ApproximationSegmentSection2D * seg = new MetaPDF::ApproximationSegmentSection2D( coeffs, steps, Concrete::ZERO_TIME, Concrete::UNIT_TIME );
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
	    Concrete::Length dist = coeffs->distanceAt( Concrete::ZERO_TIME );
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
      MetaPDF::ApproximationSegmentSection2D * seg = work.front( ).first;
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
		MetaPDF::ApproximationSegmentSection2D * part = seg->cutAfter( split );
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
		MetaPDF::ApproximationSegmentSection2D * part = seg->cutBefore( split );
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

MetaPDF::ApproximationPoly2D::ApproximationPoly2D( const Concrete::Coords2D & p0, const Concrete::Coords2D & p1, const Concrete::Coords2D & p2, const Concrete::Coords2D & p3, const Concrete::Coords2D & p )
  : p_( p ), polyCoeffs_( Bezier::PolyCoeffs< Concrete::Coords2D >( Bezier::ControlPoints< Concrete::Coords2D >( p0, p1, p2, p3 ) ) )
{
  kxD0_0_ = polyCoeffs_.z0_.x_ - p.x_;
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

  Concrete::Length tmp = max( hypotPhysical( p1.x_ - p0.x_, p1.y_ - p0.y_ ),
				       hypotPhysical( p2.x_ - p1.x_, p2.y_ - p1.y_ ) );
  maxSpeed_ = max( tmp, hypotPhysical( p3.x_ - p2.x_, p3.y_ - p2.y_ ) ).offtype< 0, 1 >( );
}

Concrete::Time
MetaPDF::ApproximationPoly2D::splitTime( const Concrete::Time t_low, const Concrete::Time t_high, const Concrete::Time t_tol ) const
{
  Concrete::Time t;
  Physical< 2, 0 > last_f( HUGE_VAL );
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
  Concrete::Time last_t = - Concrete::UNIT_TIME;
  bool lastOffBounds = false;
      
  while( t < last_t - t_tol || t > last_t + t_tol )
    {
      last_t = t;
      Physical< 2, -1 > objectiveD1( 0 );  // these are actually computed for the objective divided by two
      Physical< 2, -2 > objectiveD2( 0 );  // but since we will divide one by the other, it doesn't matter.

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

      Concrete::Time step;
      if( objectiveD2 <= Physical< 2, -2 >( 0 ) )
	{
	  /* The minimization problem is not convex, locally.
	   * Go to one of the extremes.
	   */
	  if( objectiveD1 < Physical< 2, -1 >( 0 ) )
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
	  step = -objectiveD1 / objectiveD2;  //  here's the division where the missing factors of 2 cancel.
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
      if( step > Concrete::ZERO_TIME )
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

      if( t < t_low )
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
      else if( t > t_high )
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

Bezier::ControlPoints< Concrete::Coords2D >
MetaPDF::ApproximationPoly2D::getControls( const Concrete::Time t_low, const Concrete::Time t_high ) const
{
  return Bezier::ControlPoints< Concrete::Coords2D >( polyCoeffs_.subSection( t_low.offtype< 0, 1 >( ), t_high.offtype< 0, 1 >( ) ) );
}

bool
MetaPDF::ApproximationPoly2D::isCircular( ) const
{
  Concrete::Length rmax = -Concrete::HUGE_LENGTH;
  Concrete::Length rmin = Concrete::HUGE_LENGTH;
  for( Concrete::Time t = Concrete::ZERO_TIME; t < Concrete::Time( 1.05 ); t += Concrete::Time( 0.33333 ) )
    {
      Concrete::Length r = distanceAt( t );
      rmax = max( rmax, r );
      rmin = min( rmin, r );
    }
  return rmax <= rmin * 1.0001;
}

const Concrete::Coords2D &
MetaPDF::ApproximationPoly2D::getPoint( ) const
{
  return p_;
}


Concrete::Length
MetaPDF::ApproximationPoly2D::distanceAt( Concrete::Time t ) const
{
  return hypotPhysical( kxD0_0_ + t * ( kxD0_1_ + t * ( kxD0_2_ + t * kxD0_3_ ) ),
		kyD0_0_ + t * ( kyD0_1_ + t * ( kyD0_2_ + t * kyD0_3_ ) ) );
}

Physical< 2, 0 >
MetaPDF::ApproximationPoly2D::squaredDistanceAt( Concrete::Time t ) const
{
  Concrete::Length deltax = kxD0_0_ + t * ( kxD0_1_ + t * ( kxD0_2_ + t * kxD0_3_ ) );
  Concrete::Length deltay = kyD0_0_ + t * ( kyD0_1_ + t * ( kyD0_2_ + t * kyD0_3_ ) );
  return deltax * deltax + deltay * deltay;
}

MetaPDF::ApproximationSegmentSection2D::ApproximationSegmentSection2D( const MetaPDF::ApproximationPoly2D * baseSeg, Concrete::Time steps, Concrete::Time t0, Concrete::Time t1 )
  : controls_( baseSeg->getControls( t0, t1 ) ), baseSeg_( baseSeg ), steps_( steps ), t0_( t0 ), t1_( t1 )
{ }

MetaPDF::ApproximationSegmentSection2D *
MetaPDF::ApproximationSegmentSection2D::cutAfter( Concrete::Time t ) const
{
  return new ApproximationSegmentSection2D( baseSeg_, steps_, t0_, t );
}

MetaPDF::ApproximationSegmentSection2D *
MetaPDF::ApproximationSegmentSection2D::cutBefore( Concrete::Time t ) const
{
  return new ApproximationSegmentSection2D( baseSeg_, steps_, t, t1_ );
}

Concrete::Length
MetaPDF::ApproximationSegmentSection2D::convexHullDistance( ) const
{
  Concrete::Coords2D p( baseSeg_->getPoint( ) );

  std::vector< const Concrete::Coords2D * > pointSet( 0 );
  pointSet.reserve( 4 );
  pointSet.push_back( & controls_.p0_ );
  pointSet.push_back( & controls_.p1_ );
  pointSet.push_back( & controls_.p2_ );
  pointSet.push_back( & controls_.p3_ );

  Concrete::Length res = Concrete::HUGE_LENGTH;

  for( std::vector< const Concrete::Coords2D * >::const_iterator i0 = pointSet.begin( ); i0 != pointSet.end( ); ++i0 )
    {
      std::vector< const Concrete::Coords2D * >::const_iterator i1 = i0;
      ++i1;
      for( ; i1 != pointSet.end( ); ++i1 )
	{
	  const Concrete::Coords2D & p0( **i0 );
	  const Concrete::Coords2D & p1( **i1 );

	  /* The two points bound the convex hull iff the other points are on the same side of the segment.
	   * If they are on the same side and p is on the other side, then the distance to p is of interest.
	   */
	  const Concrete::Length tx = p1.x_ - p0.x_;
	  const Concrete::Length ty = p1.y_ - p0.y_;
	  bool counterClockwise;  // true when ( p0, p1 ) are ordered counter-clockwise around the interior.
	  {
	    std::vector< const Concrete::Coords2D * >::const_iterator i2 = pointSet.begin( );
	    for( ; ; ++i2 ) // we don't have to check against pointSet.end( )
	      {
		if( i2 == i0 || i2 == i1 )
		  {
		    continue;
		  }
		const Concrete::Length dx = (*i2)->x_ - p0.x_;
		const Concrete::Length dy = (*i2)->y_ - p0.y_;
		counterClockwise = ( ty * dx - tx * dy < Physical< 2, 0 >( 0 ) );
		break;
	      }
	    ++i2;
	    for( ; ; ++i2 ) // we don't have to check against pointSet.end( )
	      {
		if( i2 == i0 || i2 == i1 )
		  {
		    continue;
		  }
		const Concrete::Length dx = (*i2)->x_ - p0.x_;
		const Concrete::Length dy = (*i2)->y_ - p0.y_;
		if( counterClockwise == ( ty * dx - tx * dy < Physical< 2, 0 >( 0 ) ) )
		  {
		    goto checkDistance;
		  }
		break;
	      }
	    continue; // the points where on different sides.
	  checkDistance:

	    const Concrete::Length dx = p.x_ - p0.x_;
	    const Concrete::Length dy = p.y_ - p0.y_;
	    if( ( ty * dx - tx * dy > Physical< 2, 0 >( 0 ) ) == counterClockwise )
	      {
		double s = ( tx * dx + ty * dy ) / ( tx*tx + ty*ty );
		Concrete::Length dist;
		if( s <= 0 )
		  {
		    dist = hypotPhysical( p0.x_ - p.x_, p0.y_ - p.y_ );
		  }
		else if( s >= 1 )
		  {
		    dist = hypotPhysical( p1.x_ - p.x_, p1.y_ - p.y_ );
		  }
		else
		  {
		    dist = hypotPhysical( s * tx - dx, s * ty - dy );
		  }
		res = min( res, dist );
	      }
	  }
	}
    }
  
  if( res == Concrete::HUGE_LENGTH )
    {
      /* This means that p was on the inside of each line, meaning it
       * is also inside the convex hull.
       */
      return Concrete::ZERO_LENGTH;
    }
  return res;
}

Concrete::Time
MetaPDF::ApproximationSegmentSection2D::splitTime( const Concrete::Time t_tol ) const
{
  Concrete::Time d = 0.2 * ( t1_ - t0_ );
  return baseSeg_->splitTime( t0_ + d, t1_ - d, t_tol );
}

Concrete::Time
MetaPDF::ApproximationSegmentSection2D::globalTime( Concrete::Time t ) const
{
  return steps_ + t;
}

Concrete::Length
MetaPDF::ApproximationSegmentSection2D::distanceAt( Concrete::Time t ) const
{
  return baseSeg_->distanceAt( t );
}

Concrete::Speed
MetaPDF::ApproximationSegmentSection2D::maxSpeed( ) const
{
  return baseSeg_->maxSpeed( );
}

Concrete::Time
MetaPDF::ApproximationSegmentSection2D::duration( ) const
{
  return t1_ - t0_;
}

namespace MetaPDF
{
  class HullSorter2D : public std::binary_function< const Concrete::Coords2D *, const Concrete::Coords2D *, bool >
  {
    const Concrete::Coords2D & p_;
  public:
    HullSorter2D( const Concrete::Coords2D & p ) : p_( p ) { }
    bool operator () ( const Concrete::Coords2D * p1, const Concrete::Coords2D * p2 )
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

RefCountPtr< const Lang::ElementaryPath2D >
Lang::ElementaryPath2D::controlling_hull( ) const
{
  if( size( ) == 0 )
    {
      return Lang::THE_EMPTYPATH2D;
    }

  if( size( ) == 1 )
    {
      Lang::ElementaryPath2D * res = new Lang::ElementaryPath2D( );
      res->push_back( new Concrete::PathPoint2D( *front( ) ) );
      res->close( );
      return RefCountPtr< const Lang::ElementaryPath2D >( res );
    }

  /* Compute convex hull using Graham scan algorithm.
   * The stack of points in the hull is split into the last point and the rest.
   */
  
  std::vector< const Concrete::Coords2D * > sortedPoints( 0 );
  sortedPoints.reserve( size( ) - 1 );

  /* Step 0: Place all distinct points in sortedPoints.  The sort will take place soon...
   */
  {
    const_iterator i = begin( );
    if( closed_ )
      {
	if( (*i)->rear_ != (*i)->mid_ )
	  {
	    sortedPoints.push_back( (*i)->rear_ );
	  }
      }
    sortedPoints.push_back( (*i)->mid_ );
    if( (*i)->front_ != (*i)->mid_ )
      {
	sortedPoints.push_back( (*i)->front_ );
      }

    const_iterator theEnd = end( );
    --theEnd;
    for( ; i != theEnd; ++i )
      {
	if( (*i)->rear_ != (*i)->mid_ )
	  {
	    sortedPoints.push_back( (*i)->rear_ );
	  }
	sortedPoints.push_back( (*i)->mid_ );
	if( (*i)->front_ != (*i)->mid_ )
	  {
	    sortedPoints.push_back( (*i)->front_ );
	  }
      }
    if( (*i)->rear_ != (*i)->mid_ )
      {
	sortedPoints.push_back( (*i)->rear_ );
      }
    sortedPoints.push_back( (*i)->mid_ );
    if( closed_ )
      {
	if( (*i)->front_ != (*i)->mid_ )
	  {
	    sortedPoints.push_back( (*i)->front_ );
	  }
      }
  }

  /* Step 1: Find leftmost of the lowest vertexes.  The sort will take place very soon...
   */
  const Concrete::Coords2D * last;
  const Concrete::Coords2D * start;
  {  
    std::vector< const Concrete::Coords2D * >::iterator i = sortedPoints.begin( );
    std::vector< const Concrete::Coords2D * >::iterator start_i = i;
    start = *start_i;
    ++i;
    for( ; i != sortedPoints.end( ); ++i )
      {
	if( (*i)->y_ > start->y_ )
	  {
	    continue;
	  }
	if( (*i)->y_ < start->y_ )
	  {
	    start_i = i;
	    start = *start_i;
	    continue;
	  }
	if( (*i)->x_ >= start->x_ )
	  {
	    continue;
	  }
	start_i = i;
	start = *start_i;
      }
    last = start;
    // Remove the starting points from the set by moving the last point to its position.
    *start_i = *sortedPoints.rbegin( );
    sortedPoints.pop_back( );
  }
  
  /* Step 2: Sort remaining points.
   */
  std::sort( sortedPoints.begin( ), sortedPoints.end( ), MetaPDF::HullSorter2D( *start ) );
  
  /* Step 3: Construct hull.  The pointers are first placed in a list, and only copied to the new path once we know
   * what points belong to the hull.
   */
  Lang::ElementaryPath2D * res = new Lang::ElementaryPath2D( );
  {
    std::list< const Concrete::Coords2D * > hullPoints;
    for( std::vector< const Concrete::Coords2D * >::const_iterator i = sortedPoints.begin( );
	 i != sortedPoints.end( );
	 ++i )
      {
	while( hullPoints.size( ) > 0 )  // Remember that size( ) does not count the last point.
	  {
	    /* If new point is to the right of the line from second last to last point,
	     * then the last point is removed.
	     * Let n be the outward pointing normal.
	     */
	    const Concrete::Coords2D * secondLast = hullPoints.back( );
	    const Concrete::Length nx = last->y_ - secondLast->y_;
	    const Concrete::Length ny = secondLast->x_ - last->x_;
	    const Concrete::Length dx = (*i)->x_ - last->x_;
	    const Concrete::Length dy = (*i)->y_ - last->y_;
	    if( nx * dx + ny * dy >= Physical< 2, 0 >( 0 ) )
	      {
		last = hullPoints.back( );
		hullPoints.pop_back( );
	      }
	    else
	      {
		break;
	      }
	  }
	hullPoints.push_back( last );
	last = *i;
      }
    hullPoints.push_back( last );
    
    for( std::list< const Concrete::Coords2D * >::const_iterator i = hullPoints.begin( );
	 i != hullPoints.end( );
	 ++i )
      {
	res->push_back( new Concrete::PathPoint2D( new Concrete::Coords2D( *(*i) ) ) );
      }

  }
  res->close( );
  return RefCountPtr< const Lang::ElementaryPath2D >( res );
}

RefCountPtr< const Lang::ElementaryPath2D >
Lang::ElementaryPath2D::upsample( const Computation::Upsampler2D & sampler ) const
{
  if( size( ) == 0 )
    {
      return Lang::THE_EMPTYPATH2D;
    }

  std::vector< double > sampleTimes;
  Concrete::Coords2D rearHandle( 0, 0 );

  Lang::ElementaryPath2D * res = new Lang::ElementaryPath2D( );
  if( closed_ )
    {
      res->close( );
    }

  if( size( ) == 1 )
    {
      if( ! closed_ )
	{
	  res->push_back( new Concrete::PathPoint2D( *front( ) ) );
	  return RefCountPtr< const Lang::ElementaryPath2D >( res );
	}

      const_iterator i1 = begin( );
      Bezier::ControlPoints< Concrete::Coords2D > controls( *(*i1)->mid_, *(*i1)->front_, *(*i1)->rear_, *(*i1)->mid_ );
      sampler( & sampleTimes, controls );
      if( sampleTimes.size( ) == 0 )
	{
	  res->push_back( new Concrete::PathPoint2D( *front( ) ) );
	  return RefCountPtr< const Lang::ElementaryPath2D >( res );
	}

      Bezier::PolyCoeffs< Concrete::Coords2D > coeffs( controls );
      {
	// We will compute the same sub section again later, but doing it twice makes the code cleaner and more similar to the general case below.
	Bezier::ControlPoints< Concrete::Coords2D > lastSeg( coeffs.subSection( sampleTimes.back( ), 1 ) );
	rearHandle = lastSeg.p2_;
      }
      sampleTimes.push_back( 1 );
      typedef typeof sampleTimes ListType;
      double t0 = 0;
      ListType::const_iterator ti = sampleTimes.begin( );
      double t1;
      for( ; ti != sampleTimes.end( ); t0 = t1, ++ti )
	{
	  t1 = *ti;
	  Bezier::ControlPoints< Concrete::Coords2D > seg( coeffs.subSection( t0, t1 ) );
	  Concrete::PathPoint2D * newPoint = new Concrete::PathPoint2D( new Concrete::Coords2D( seg.p0_ ) );
	  newPoint->front_ = new Concrete::Coords2D( seg.p1_ );
	  newPoint->rear_ = new Concrete::Coords2D( rearHandle );
	  res->push_back( newPoint );
	  rearHandle = seg.p2_;
	}

      return RefCountPtr< const Lang::ElementaryPath2D >( res );
    }

  const_iterator i1 = begin( );
  const_iterator i2 = i1;
  ++i2;

  // Determine the initial rear handle.
  if( closed_ )
    {
      const_iterator i0 = end( );
      --i0;
      Bezier::ControlPoints< Concrete::Coords2D > controls( *(*i0)->mid_, *(*i0)->front_, *(*i1)->rear_, *(*i1)->mid_ );
      sampleTimes.clear( );  // Not needed here, but included for symmetry with the general case below.
      sampler( & sampleTimes, controls );
      if( sampleTimes.size( ) == 0 )
	{
	  rearHandle = *(*i1)->rear_;
	}
      else
	{
	  Bezier::PolyCoeffs< Concrete::Coords2D > coeffs( controls );
	  Bezier::ControlPoints< Concrete::Coords2D > lastSeg( coeffs.subSection( sampleTimes.back( ), 1 ) );
	  rearHandle = lastSeg.p2_;
	}
    }
  else
    {
      rearHandle = *(*i1)->rear_;
    }
  
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
	      Concrete::PathPoint2D * newPoint = new Concrete::PathPoint2D( new Concrete::Coords2D( *(*i1)->mid_ ) );
	      newPoint->front_ = new Concrete::Coords2D( *(*i1)->front_ );
	      newPoint->rear_ = new Concrete::Coords2D( rearHandle );
	      res->push_back( newPoint );
	      break;
	    }
	}

      Bezier::ControlPoints< Concrete::Coords2D > controls( *(*i1)->mid_, *(*i1)->front_, *(*i2)->rear_, *(*i2)->mid_ );
      sampleTimes.clear( );
      sampler( & sampleTimes, controls );
      if( sampleTimes.size( ) == 0 )
	{
	  Concrete::PathPoint2D * newPoint = new Concrete::PathPoint2D( new Concrete::Coords2D( controls.p0_ ) );
	  newPoint->front_ = new Concrete::Coords2D( controls.p1_ );
	  newPoint->rear_ = new Concrete::Coords2D( rearHandle );
	  res->push_back( newPoint );
	  rearHandle = controls.p2_;
	}
      else
	{
	  Bezier::PolyCoeffs< Concrete::Coords2D > coeffs( controls );
	  sampleTimes.push_back( 1 );
	  typedef typeof sampleTimes ListType;
	  double t0 = 0;
	  ListType::const_iterator ti = sampleTimes.begin( );
	  double t1;
	  for( ; ti != sampleTimes.end( ); t0 = t1, ++ti )
	    {
	      t1 = *ti;
	      Bezier::ControlPoints< Concrete::Coords2D > seg( coeffs.subSection( t0, t1 ) );
	      Concrete::PathPoint2D * newPoint = new Concrete::PathPoint2D( new Concrete::Coords2D( seg.p0_ ) );
	      newPoint->front_ = new Concrete::Coords2D( seg.p1_ );
	      newPoint->rear_ = new Concrete::Coords2D( rearHandle );
	      res->push_back( newPoint );
	      rearHandle = seg.p2_;
	    }
	}
      
    }

  return RefCountPtr< const Lang::ElementaryPath2D >( res );
}


bool
Lang::ElementaryPath2D::isConvexPoly( Concrete::Length tol ) const
{
  for( const_iterator i = begin( ); i != end( ); ++i )
    {
      if( (*i)->front_ != (*i)->mid_ || (*i)->rear_ != (*i)->mid_ )
	{
	  return false;
	}
    }
  if( size( ) <= 3 )
    {
      return true;
    }
  for( const_iterator i = begin( ); i != end( ); ++i )
    {
      const_iterator i1 = i;
      ++i1;
      if( i1 == end( ) )
	{
	  i1 = begin( );
	}
      const_iterator i2 = i1;
      ++i2;
      if( i2 == end( ) )
	{
	  i2 = begin( );
	}
      const_iterator i3 = i2;
      ++i3;
      if( i3 == end( ) )
	{
	  i3 = begin( );
	}

      // The following test is a local convexity test which does not need an orientation, but checks that the two points neighboring an edge are on the same side of the edge.
      Concrete::UnitFloatPair n = (*i)->mid_->normalizedOrthogonal( *((*i1)->mid_) );
      Concrete::Length tmp = Concrete::inner( n, *((*i2)->mid_) - *((*i)->mid_) );
      if( tmp < - tol )
	{
	  if( Concrete::inner( n, *((*i3)->mid_) - *((*i)->mid_) ) > tol )
	    {
	      return false;
	    }
	}
      else if( tmp > tol )
	{
	  if( Concrete::inner( n, *((*i3)->mid_) - *((*i)->mid_) ) < - tol )
	    {
	      return false;
	    }
	}
    }
  return true;
}

bool
Lang::ElementaryPath2D::convexPolyContains( const Concrete::Coords2D & p, Concrete::Length tol ) const
{
  if( size( ) < 3 )
    {
      return false;
    }

  // Since we know that we are a convex poly, the same-side test used in isConvexPoly can now be used with *((*i3)->mid_) replaced by p
  for( const_iterator i = begin( ); i != end( ); ++i )
    {
      const_iterator i1 = i;
      ++i1;
      if( i1 == end( ) )
	{
	  i1 = begin( );
	}
      const_iterator i2 = i1;
      ++i2;
      if( i2 == end( ) )
	{
	  i2 = begin( );
	}

      Concrete::UnitFloatPair n = (*i)->mid_->normalizedOrthogonal( *((*i1)->mid_) );
      Concrete::Length tmp = Concrete::inner( n, *((*i2)->mid_) - *((*i)->mid_) );
      if( tmp < - tol )
	{
	  if( Concrete::inner( n, p - *((*i)->mid_) ) > tol )
	    {
	      return false;
	    }
	}
      else if( tmp > tol )
	{
	  if( Concrete::inner( n, p - *((*i)->mid_) ) < - tol )
	    {
	      return false;
	    }
	}
    }
  return true;
}


Concrete::Length
Lang::ElementaryPath2D::arcLength( ) const
{
  if( size( ) == 0 )
    {
      throw Exceptions::OutOfRange( "The empty path has no defined arclength." );
    }
  /* Multiplication by dt is extracted to outside the integrals for efficiency.
   */
  Concrete::Length totlen = Concrete::ZERO_LENGTH;

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
	  const Concrete::Length segLength = hypotPhysical( (*i1)->mid_->x_ - (*i2)->mid_->x_, (*i1)->mid_->y_ - (*i2)->mid_->y_ );
	  totlen += segLength;
	  continue;
	}

      Concrete::Bezier x0 = (*i1)->mid_->x_.offtype< 0, 3 >( );
      Concrete::Bezier y0 = (*i1)->mid_->y_.offtype< 0, 3 >( );
      Concrete::Bezier x1 = (*i1)->front_->x_.offtype< 0, 3 >( );
      Concrete::Bezier y1 = (*i1)->front_->y_.offtype< 0, 3 >( );
      Concrete::Bezier x2 = (*i2)->rear_->x_.offtype< 0, 3 >( );
      Concrete::Bezier y2 = (*i2)->rear_->y_.offtype< 0, 3 >( );
      Concrete::Bezier x3 = (*i2)->mid_->x_.offtype< 0, 3 >( );
      Concrete::Bezier y3 = (*i2)->mid_->y_.offtype< 0, 3 >( );

      const Concrete::Length segLengthBound =
	( hypotPhysical( x1-x0, y1-y0 ) + hypotPhysical( x2-x1, y2-y1 ) + hypotPhysical( x3-x2, y3-y2 ) ).offtype< 0, -3 >( );

      if( segLengthBound < arcdelta )
	{
	  totlen += segLengthBound;
	}
      else
	{
	  Concrete::Time dt = MetaPDF::computeDt( segLengthBound );
	  Concrete::Length tmpSum_l = Concrete::ZERO_LENGTH;
	  for( Concrete::Time t = Concrete::ZERO_TIME; t < Concrete::UNIT_TIME; t += dt )
	    {
	      Concrete::Time tc = Concrete::UNIT_TIME - t; /* complement to t */
	      Physical< 0, 2 > kv0 = -3 * tc * tc;
	      Physical< 0, 2 > kv1 = 3 * tc * tc - 6 * tc * t;
	      Physical< 0, 2 > kv2 = 6 * tc * t - 3 * t * t;
	      Physical< 0, 2 > kv3 = 3 * t * t;
	      Concrete::Speed vx = x0 * kv0 + x1 * kv1 + x2 * kv2 + x3 * kv3;
	      Concrete::Speed vy = y0 * kv0 + y1 * kv1 + y2 * kv2 + y3 * kv3;
	      
	      Concrete::Length dl = hypotPhysical( vx, vy ).offtype< 0, -1 >( );
	      tmpSum_l += dl;
	    }
	  totlen += tmpSum_l * dt.offtype< 0, 1 >( );
	}
    }

  return totlen;
}

Concrete::Length
Lang::ElementaryPath2D::arcLength( Concrete::Time tRemaining ) const
{
  if( size( ) == 0 )
    {
      throw Exceptions::OutOfRange( "The empty path has no defined arclength." );
    }
  if( tRemaining < Concrete::ZERO_TIME )
    {
      return negative_arcLength( -tRemaining );
    }
  if( isinfPhysical( tRemaining ) )
    {
      throw Exceptions::OutOfRange( "The arctime to infinity is not defined." );
    }
  if( tRemaining > Concrete::Time( duration( ) ) && ! closed_ )
    {
      throw Exceptions::OutOfRange( "Too big arctime for open path." );
    }
  /* Multiplication by dt is extracted to outside the integrals for efficiency.
   */
  Concrete::Length totlen = Concrete::ZERO_LENGTH;

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
  for( ; tRemaining > Concrete::ZERO_TIME; ++i1, ++i2, tRemaining -= Concrete::UNIT_TIME )
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
	  if( tRemaining <= Concrete::UNIT_TIME )
	    {
	      Concrete::Time t1 = tRemaining;
	      Concrete::Time tc = Concrete::UNIT_TIME - t1; /* complement to t1 */
	      Concrete::Coords2D tmp =
		( tc * tc * ( tc + 3 * t1 ) ).offtype< 0, 3 >( ) * (*((*i1)->mid_)) + 
		( t1 * t1 * ( t1 + 3 * tc ) ).offtype< 0, 3 >( ) * (*((*i2)->mid_));
	      const Concrete::Length segLength = hypotPhysical( (*i1)->mid_->x_ - tmp.x_, (*i1)->mid_->y_ - tmp.y_ );
	      totlen += segLength;
	      break;
	    }
	  const Concrete::Length segLength = hypotPhysical( (*i1)->mid_->x_ - (*i2)->mid_->x_, (*i1)->mid_->y_ - (*i2)->mid_->y_ );
	  totlen += segLength;
	  continue;
	}

      Concrete::Bezier x0 = (*i1)->mid_->x_.offtype< 0, 3 >( );
      Concrete::Bezier y0 = (*i1)->mid_->y_.offtype< 0, 3 >( );
      Concrete::Bezier x1 = (*i1)->front_->x_.offtype< 0, 3 >( );
      Concrete::Bezier y1 = (*i1)->front_->y_.offtype< 0, 3 >( );
      Concrete::Bezier x2 = (*i2)->rear_->x_.offtype< 0, 3 >( );
      Concrete::Bezier y2 = (*i2)->rear_->y_.offtype< 0, 3 >( );
      Concrete::Bezier x3 = (*i2)->mid_->x_.offtype< 0, 3 >( );
      Concrete::Bezier y3 = (*i2)->mid_->y_.offtype< 0, 3 >( );

      const Concrete::Length segLengthBound = 
	( hypotPhysical( x1-x0, y1-y0 ) + hypotPhysical( x2-x1, y2-y1 ) + hypotPhysical( x3-x2, y3-y2 ) ).offtype< 0, -3 >( );

      if( segLengthBound < arcdelta )
	{
	  if( tRemaining <= Concrete::UNIT_TIME )
	    {
	      totlen += double(tRemaining / Concrete::UNIT_TIME ) * segLengthBound;
	      break;
	    }
	  totlen += segLengthBound;
	}
      else
	{
	  Concrete::Time dt = MetaPDF::computeDt( segLengthBound );
	  Concrete::Length tmpSum_l = Concrete::ZERO_LENGTH;
	  const Concrete::Time tend = min( Concrete::UNIT_TIME, tRemaining );
	  for( Concrete::Time t = Concrete::ZERO_TIME; t < tend; t += dt )
	    {
	      Concrete::Time tc = Concrete::UNIT_TIME - t; /* complement to t */
	      Physical< 0, 2 > kv0 = -3 * tc * tc;
	      Physical< 0, 2 > kv1 = 3 * tc * tc - 6 * tc * t;
	      Physical< 0, 2 > kv2 = 6 * tc * t - 3 * t * t;
	      Physical< 0, 2 > kv3 = 3 * t * t;
	      Concrete::Speed vx = x0 * kv0 + x1 * kv1 + x2 * kv2 + x3 * kv3;
	      Concrete::Speed vy = y0 * kv0 + y1 * kv1 + y2 * kv2 + y3 * kv3;
	      
	      Concrete::Length dl = hypotPhysical( vx, vy ).offtype< 0, -1 >( );
	      tmpSum_l += dl;
	    }
	  totlen += tmpSum_l * dt.offtype< 0, 1 >( );
	}
    }

  return totlen;
}

Concrete::Length
Lang::ElementaryPath2D::negative_arcLength( Concrete::Time tRemaining ) const
{
  if( size( ) == 0 )
    {
      throw Exceptions::OutOfRange( "The empty path has no defined arclength." );
    }
  if( tRemaining < Concrete::ZERO_TIME )
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
  Concrete::Length totlen = Concrete::ZERO_LENGTH;

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
  for( ; tRemaining > Concrete::ZERO_TIME; ++i1, ++i2, tRemaining -= Concrete::UNIT_TIME )
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
	  if( tRemaining <= Concrete::UNIT_TIME )
	    {
	      Concrete::Time t1 = tRemaining;
	      Concrete::Time tc = Concrete::UNIT_TIME - t1; /* complement to t1 */
	      Concrete::Coords2D tmp =
		( tc * tc * ( tc + 3 * t1 ) ).offtype< 0, 3 >( ) * (*((*i1)->mid_)) +
		( t1 * t1 * ( t1 + 3 * tc ) ).offtype< 0, 3 >( ) * (*((*i2)->mid_));
	      const Concrete::Length segLength = hypotPhysical( (*i1)->mid_->x_ - tmp.x_, (*i1)->mid_->y_ - tmp.y_ );
	      totlen -= segLength;
	      break;
	    }
	  const Concrete::Length segLength = hypotPhysical( (*i1)->mid_->x_ - (*i2)->mid_->x_, (*i1)->mid_->y_ - (*i2)->mid_->y_ );
	  totlen -= segLength;
	  continue;
	}

      Concrete::Bezier x0 = (*i1)->mid_->x_.offtype< 0, 3 >( );
      Concrete::Bezier y0 = (*i1)->mid_->y_.offtype< 0, 3 >( );
      Concrete::Bezier x1 = (*i1)->rear_->x_.offtype< 0, 3 >( );
      Concrete::Bezier y1 = (*i1)->rear_->y_.offtype< 0, 3 >( );
      Concrete::Bezier x2 = (*i2)->front_->x_.offtype< 0, 3 >( );
      Concrete::Bezier y2 = (*i2)->front_->y_.offtype< 0, 3 >( );
      Concrete::Bezier x3 = (*i2)->mid_->x_.offtype< 0, 3 >( );
      Concrete::Bezier y3 = (*i2)->mid_->y_.offtype< 0, 3 >( );

      const Concrete::Length segLengthBound =
	( hypotPhysical( x1-x0, y1-y0 ) + hypotPhysical( x2-x1, y2-y1 ) + hypotPhysical( x3-x2, y3-y2 ) ).offtype< 0, -3 >( );

      if( segLengthBound < arcdelta )
	{
	  if( tRemaining <= Concrete::UNIT_TIME )
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
	  for( Concrete::Time t = Concrete::ZERO_TIME; t < tend; t += dt )
	    {
	      Concrete::Time tc = Concrete::UNIT_TIME - t; /* complement to t */
	      Physical< 0, 2 > kv0 = -3 * tc * tc;
	      Physical< 0, 2 > kv1 = 3 * tc * tc - 6 * tc * t;
	      Physical< 0, 2 > kv2 = 6 * tc * t - 3 * t * t;
	      Physical< 0, 2 > kv3 = 3 * t * t;
	      Concrete::Speed vx = x0 * kv0 + x1 * kv1 + x2 * kv2 + x3 * kv3;
	      Concrete::Speed vy = y0 * kv0 + y1 * kv1 + y2 * kv2 + y3 * kv3;
	      
	      Concrete::Length dl = hypotPhysical( vx, vy ).offtype< 0, -1 >( );
	      tmpSum_l += dl;
	    }
	  totlen -= tmpSum_l * dt.offtype< 0, 1 >( );
	}
    }

  return totlen;
}

Concrete::SplineTime
Lang::ElementaryPath2D::arcTime( const Concrete::Length & t, Concrete::Time t0 ) const
{
  if( size( ) == 0 )
    {
      throw Exceptions::OutOfRange( "The empty path has no arctimes defined." );
    }
  if( duration( ) == 0 )
    {
      throw Exceptions::OutOfRange( "The singleton path has no arctimes defined." );
    }
  if( t >= Concrete::HUGE_LENGTH )
    {
      return Concrete::SplineTime( Concrete::Time( duration( ) ), true );
    }
  if( t < Concrete::ZERO_LENGTH )
    {
      return negative_arcTime( - t, t0 );
    }
  
  Concrete::Time splineTime = t0;
  t0 = modPhysical( t0, Concrete::Time( duration( ) ) );
  if( t0 < Concrete::ZERO_TIME )
    {
      t0 += Concrete::Time( duration( ) );
    }

  const_iterator i1 = begin( );
  while( t0 >= Concrete::UNIT_TIME )
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

  if( t0 > Concrete::ZERO_TIME )
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
	  Concrete::Coords2D tmp =
	    ( tc * tc * ( tc + 3 * t0 ) ).offtype< 0, 3 >( ) * (*((*i1)->mid_)) +
	    ( t0 * t0 * ( t0 + 3 * tc ) ).offtype< 0, 3 >( ) * (*((*i2)->mid_));
	  const Concrete::Length segRestLength = hypotPhysical( tmp.x_ - (*i2)->mid_->x_, tmp.y_ - (*i2)->mid_->y_ );
	  if( segRestLength < remainingLength )
	    {
	      remainingLength -= segRestLength;
	      goto beginLoop;
	    }
	  const Concrete::Length segPastLength = hypotPhysical( tmp.x_ - (*i1)->mid_->x_, tmp.y_ - (*i1)->mid_->y_ );
	  const Concrete::Length segLength = hypotPhysical( (*i1)->mid_->x_ - (*i2)->mid_->x_, (*i1)->mid_->y_ - (*i2)->mid_->y_ );
	  splineTime += MetaPDF::straightLineArcTime( ( segPastLength + remainingLength ) / segLength ) - t0;
	  goto done;
	}

      {
	Concrete::Bezier x0 = (*i1)->mid_->x_.offtype< 0, 3 >( );
	Concrete::Bezier y0 = (*i1)->mid_->y_.offtype< 0, 3 >( );
	Concrete::Bezier x1 = (*i1)->front_->x_.offtype< 0, 3 >( );
	Concrete::Bezier y1 = (*i1)->front_->y_.offtype< 0, 3 >( );
	Concrete::Bezier x2 = (*i2)->rear_->x_.offtype< 0, 3 >( );
	Concrete::Bezier y2 = (*i2)->rear_->y_.offtype< 0, 3 >( );
	Concrete::Bezier x3 = (*i2)->mid_->x_.offtype< 0, 3 >( );
	Concrete::Bezier y3 = (*i2)->mid_->y_.offtype< 0, 3 >( );
	
	const Concrete::Length segLengthBound =
	  ( hypotPhysical( (x1-x0), (y1-y0) ) + hypotPhysical( (x2-x1), (y2-y1) ) + hypotPhysical( (x3-x2), (y3-y2) ) ).offtype< 0, -3 >( );
	
	if( segLengthBound < arcdelta )
	  {
	    remainingLength -= segLengthBound;
	    if( remainingLength <= Concrete::ZERO_LENGTH )
	      {
		splineTime += Concrete::Time( 0.5 );
		goto done;
	      }
	  }
	else
	  {
	    Concrete::Time dt = MetaPDF::computeDt( segLengthBound );
	    
	    const Concrete::Length remainingDiv_dt = remainingLength / dt.offtype< 0, 1 >( );
	    Concrete::Length tmpSum_l = Concrete::ZERO_LENGTH;
	    for( Concrete::Time t = t0; t < Concrete::UNIT_TIME; t += dt )
	      {
		Concrete::Time tc = Concrete::UNIT_TIME - t; /* complement to t */
		Physical< 0, 2 > kv0 = -3 * tc * tc;
		Physical< 0, 2 > kv1 = 3 * tc * tc - 6 * tc * t;
		Physical< 0, 2 > kv2 = 6 * tc * t - 3 * t * t;
		Physical< 0, 2 > kv3 = 3 * t * t;
		Concrete::Speed vx = x0 * kv0 + x1 * kv1 + x2 * kv2 + x3 * kv3;
		Concrete::Speed vy = y0 * kv0 + y1 * kv1 + y2 * kv2 + y3 * kv3;
		
		Concrete::Length dl = hypotPhysical( vx, vy ).offtype< 0, -1 >( );
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
	  const Concrete::Length segLength = hypotPhysical( (*i1)->mid_->x_ - (*i2)->mid_->x_, (*i1)->mid_->y_ - (*i2)->mid_->y_ );
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
      Concrete::Bezier x1 = (*i1)->front_->x_.offtype< 0, 3 >( );
      Concrete::Bezier y1 = (*i1)->front_->y_.offtype< 0, 3 >( );
      Concrete::Bezier x2 = (*i2)->rear_->x_.offtype< 0, 3 >( );
      Concrete::Bezier y2 = (*i2)->rear_->y_.offtype< 0, 3 >( );
      Concrete::Bezier x3 = (*i2)->mid_->x_.offtype< 0, 3 >( );
      Concrete::Bezier y3 = (*i2)->mid_->y_.offtype< 0, 3 >( );

      const Concrete::Length segLengthBound =
	( hypotPhysical( (x1-x0), (y1-y0) ) + hypotPhysical( (x2-x1), (y2-y1) ) + hypotPhysical( (x3-x2), (y3-y2) ) ).offtype< 0, -3 >( );

      if( segLengthBound < arcdelta )
	{
	  remainingLength -= segLengthBound;
	  if( remainingLength <= Concrete::ZERO_LENGTH )
	    {
	      splineTime += Concrete::Time( 0.5 );
	      goto done;
	    }
	}
      else
	{
	  Concrete::Time dt = MetaPDF::computeDt( segLengthBound );
	  
	  const Concrete::Length remainingDiv_dt = remainingLength / dt.offtype< 0, 1 >( );
	  Concrete::Length tmpSum_l = Concrete::ZERO_LENGTH;
	  for( Concrete::Time t = Concrete::ZERO_TIME; t < Concrete::UNIT_TIME; t += dt )
	    {
	      Concrete::Time tc = Concrete::UNIT_TIME - t; /* complement to t */
	      Physical< 0, 2 > kv0 = -3 * tc * tc;
	      Physical< 0, 2 > kv1 = 3 * tc * tc - 6 * tc * t;
	      Physical< 0, 2 > kv2 = 6 * tc * t - 3 * t * t;
	      Physical< 0, 2 > kv3 = 3 * t * t;
	      Concrete::Speed vx = x0 * kv0 + x1 * kv1 + x2 * kv2 + x3 * kv3;
	      Concrete::Speed vy = y0 * kv0 + y1 * kv1 + y2 * kv2 + y3 * kv3;
	      
	      Concrete::Length dl = hypotPhysical( vx, vy ).offtype< 0, -1 >( );
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
Lang::ElementaryPath2D::negative_arcTime( const Concrete::Length deltaLen, Concrete::Time t0 ) const
{
  if( deltaLen < Concrete::ZERO_LENGTH )
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

  if( deltaLen >= Concrete::HUGE_LENGTH )
    {
      return Concrete::SplineTime( Concrete::ZERO_TIME, true );
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
  while( t0 >= Concrete::UNIT_TIME )
    {
      t0 -= Concrete::UNIT_TIME;
      ++i1;
      if( i1 == rend( ) )
	{
	  if( ! closed_ )
	    {
	      return Concrete::SplineTime( Concrete::ZERO_TIME, true );
	    }
	  i1 = rbegin( );
	}
    }
  const_reverse_iterator i2 = i1;
  ++i2;

  const Concrete::Length arcdelta = Computation::the_arcdelta;
  Concrete::Length remainingLength = deltaLen;

  if( t0 > Concrete::ZERO_TIME )
    {
      if( i2 == rend( ) )
	{
	  if( ! closed_ )
	    {
	      return Concrete::SplineTime( Concrete::ZERO_TIME, true );
	    }
	  i2 = rbegin( );
	}
      if( (*i1)->rear_ == (*i1)->mid_ &&
	  (*i2)->front_ == (*i2)->mid_ )
	{
	  Concrete::Time tc = Concrete::UNIT_TIME - t0; /* complement to t0 */
	  Concrete::Coords2D tmp =
	    ( tc * tc * ( tc + 3 * t0 ) ).offtype< 0, 3 >( ) * (*((*i1)->mid_)) +
	    ( t0 * t0 * ( t0 + 3 * tc ) ).offtype< 0, 3 >( ) * (*((*i2)->mid_));
	  const Concrete::Length segRestLength = hypotPhysical( tmp.x_ - (*i2)->mid_->x_, tmp.y_ - (*i2)->mid_->y_ );
	  if( segRestLength < remainingLength )
	    {
	      remainingLength -= segRestLength;
	      goto beginLoop;
	    }
	  const Concrete::Length segPastLength = hypotPhysical( tmp.x_ - (*i1)->mid_->x_, tmp.y_ - (*i1)->mid_->y_ );
	  const Concrete::Length segLength = hypotPhysical( (*i1)->mid_->x_ - (*i2)->mid_->x_, (*i1)->mid_->y_ - (*i2)->mid_->y_ );
	  splineTime -= MetaPDF::straightLineArcTime( ( segPastLength + remainingLength ) / segLength ) - t0;
	  goto done;
	}

      {
	Concrete::Bezier x0 = (*i1)->mid_->x_.offtype< 0, 3 >( );
	Concrete::Bezier y0 = (*i1)->mid_->y_.offtype< 0, 3 >( );
	Concrete::Bezier x1 = (*i1)->rear_->x_.offtype< 0, 3 >( );
	Concrete::Bezier y1 = (*i1)->rear_->y_.offtype< 0, 3 >( );
	Concrete::Bezier x2 = (*i2)->front_->x_.offtype< 0, 3 >( );
	Concrete::Bezier y2 = (*i2)->front_->y_.offtype< 0, 3 >( );
	Concrete::Bezier x3 = (*i2)->mid_->x_.offtype< 0, 3 >( );
	Concrete::Bezier y3 = (*i2)->mid_->y_.offtype< 0, 3 >( );
	
	const Concrete::Length segLengthBound =
	  ( hypotPhysical( (x1-x0), (y1-y0) ) + hypotPhysical( (x2-x1), (y2-y1) ) + hypotPhysical( (x3-x2), (y3-y2) ) ).offtype< 0, -3 >( );
	
	if( segLengthBound < arcdelta )
	  {
	    remainingLength -= segLengthBound;
	    if( remainingLength <= Concrete::ZERO_LENGTH )
	      {
		splineTime -= Concrete::Time( 0.5 );
		goto done;
	      }
	  }
	else
	  {
	    Concrete::Time dt = MetaPDF::computeDt( segLengthBound );
	    
	    const Concrete::Length remainingDiv_dt = remainingLength / dt.offtype< 0, 1 >( );
	    Concrete::Length tmpSum_l = Concrete::ZERO_LENGTH;
	    for( Concrete::Time t = t0; t < Concrete::UNIT_TIME; t += dt )
	      {
		Concrete::Time tc = Concrete::UNIT_TIME - t; /* complement to t */
		Physical< 0, 2 > kv0 = -3 * tc * tc;
		Physical< 0, 2 > kv1 = 3 * tc * tc - 6 * tc * t;
		Physical< 0, 2 > kv2 = 6 * tc * t - 3 * t * t;
		Physical< 0, 2 > kv3 = 3 * t * t;
		Concrete::Speed vx = x0 * kv0 + x1 * kv1 + x2 * kv2 + x3 * kv3;
		Concrete::Speed vy = y0 * kv0 + y1 * kv1 + y2 * kv2 + y3 * kv3;
		
		Concrete::Length dl = hypotPhysical( vx, vy ).offtype< 0, -1 >( );
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
	      return Concrete::SplineTime( Concrete::ZERO_TIME, true );
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
	      return Concrete::SplineTime( Concrete::ZERO_TIME, true );
	    }
	  i2 = rbegin( );
	}
      if( (*i1)->rear_ == (*i1)->mid_ &&
	  (*i2)->front_ == (*i2)->mid_ )
	{
	  const Concrete::Length segLength = hypotPhysical( (*i1)->mid_->x_ - (*i2)->mid_->x_, (*i1)->mid_->y_ - (*i2)->mid_->y_ );
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
      Concrete::Bezier x1 = (*i1)->rear_->x_.offtype< 0, 3 >( );
      Concrete::Bezier y1 = (*i1)->rear_->y_.offtype< 0, 3 >( );
      Concrete::Bezier x2 = (*i2)->front_->x_.offtype< 0, 3 >( );
      Concrete::Bezier y2 = (*i2)->front_->y_.offtype< 0, 3 >( );
      Concrete::Bezier x3 = (*i2)->mid_->x_.offtype< 0, 3 >( );
      Concrete::Bezier y3 = (*i2)->mid_->y_.offtype< 0, 3 >( );
      
      const Concrete::Length segLengthBound =
	( hypotPhysical( (x1-x0), (y1-y0) ) + hypotPhysical( (x2-x1), (y2-y1) ) + hypotPhysical( (x3-x2), (y3-y2) ) ).offtype< 0, -3 >( );
	
      if( segLengthBound < arcdelta )
	{
	  remainingLength -= segLengthBound;
	  if( remainingLength <= Concrete::ZERO_LENGTH )
	    {
	      splineTime -= Concrete::Time( 0.5 );
	      goto done;
	    }
	}
      else
	{
	  Concrete::Time dt = MetaPDF::computeDt( segLengthBound );
	  
	  const Concrete::Length remainingDiv_dt = remainingLength / dt.offtype< 0, 1 >( );
	  Concrete::Length tmpSum_l = Concrete::ZERO_LENGTH;
	  for( Concrete::Time t = Concrete::ZERO_TIME; t < Concrete::UNIT_TIME; t += dt )
	    {
	      Concrete::Time tc = Concrete::UNIT_TIME - t; /* complement to t */
	      Physical< 0, 2 > kv0 = -3 * tc * tc;
	      Physical< 0, 2 > kv1 = 3 * tc * tc - 6 * tc * t;
	      Physical< 0, 2 > kv2 = 6 * tc * t - 3 * t * t;
	      Physical< 0, 2 > kv3 = 3 * t * t;
	      Concrete::Speed vx = x0 * kv0 + x1 * kv1 + x2 * kv2 + x3 * kv3;
	      Concrete::Speed vy = y0 * kv0 + y1 * kv1 + y2 * kv2 + y3 * kv3;
	      
	      Concrete::Length dl = hypotPhysical( vx, vy ).offtype< 0, -1 >( );
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

namespace MetaPDF
{
  namespace Computation
  {

  class IntersectionPolys2D
  {
    Bezier::PolyCoeffs< Concrete::Coords2D > polyCoeffs_a;

    Physical< 1, 0 > kxaD0_0;
    Physical< 1, -1 > kxaD0_1;
    Physical< 1, -2 > kxaD0_2;
    Physical< 1, -3 > kxaD0_3;
    Physical< 1, -1 > kxaD1_0;
    Physical< 1, -2 > kxaD1_1;
    Physical< 1, -3 > kxaD1_2;
    Physical< 1, -2 > kxaD2_0;
    Physical< 1, -3 > kxaD2_1;

    Physical< 1, 0 > kyaD0_0;
    Physical< 1, -1 > kyaD0_1;
    Physical< 1, -2 > kyaD0_2;
    Physical< 1, -3 > kyaD0_3;
    Physical< 1, -1 > kyaD1_0;
    Physical< 1, -2 > kyaD1_1;
    Physical< 1, -3 > kyaD1_2;
    Physical< 1, -2 > kyaD2_0;
    Physical< 1, -3 > kyaD2_1;

    Bezier::PolyCoeffs< Concrete::Coords2D > polyCoeffs_b;

    Physical< 1, 0 > kxbD0_0;
    Physical< 1, -1 > kxbD0_1;
    Physical< 1, -2 > kxbD0_2;
    Physical< 1, -3 > kxbD0_3;
    Physical< 1, -1 > kxbD1_0;
    Physical< 1, -2 > kxbD1_1;
    Physical< 1, -3 > kxbD1_2;
    Physical< 1, -2 > kxbD2_0;
    Physical< 1, -3 > kxbD2_1;

    Physical< 1, 0 > kybD0_0;
    Physical< 1, -1 > kybD0_1;
    Physical< 1, -2 > kybD0_2;
    Physical< 1, -3 > kybD0_3;
    Physical< 1, -1 > kybD1_0;
    Physical< 1, -2 > kybD1_1;
    Physical< 1, -3 > kybD1_2;
    Physical< 1, -2 > kybD2_0;
    Physical< 1, -3 > kybD2_1;

    Concrete::Speed maxSpeed_a_;
    Concrete::Speed maxSpeed_b_;
  public:
    IntersectionPolys2D( const Bezier::ControlPoints< Concrete::Coords2D > & seg_a, const Bezier::ControlPoints< Concrete::Coords2D > & seg_b );
    void splitTimes( Concrete::Time * dst_a, Concrete::Time * dst_b, const Concrete::Time t_alow, const Concrete::Time t_ahigh, const Concrete::Time t_atol, const Concrete::Time t_blow, const Concrete::Time t_bhigh, const Concrete::Time t_btol ) const;
    Concrete::Length distanceAt( Concrete::Time ta, Concrete::Time tb ) const;
    Physical< 2, 0 > squaredDistanceAt( Concrete::Time ta, Concrete::Time tb ) const;
    Concrete::Speed maxSpeed_a( ) const { return maxSpeed_a_; }
    Concrete::Speed maxSpeed_b( ) const { return maxSpeed_b_; }
    Bezier::ControlPoints< Concrete::Coords2D > getControls_a( const Concrete::Time t_low, const Concrete::Time t_high ) const;
    Bezier::ControlPoints< Concrete::Coords2D > getControls_b( const Concrete::Time t_low, const Concrete::Time t_high ) const;
  };
  class IntersectionSegmentSections2D
  {
    Bezier::ControlPoints< Concrete::Coords2D > controls_a;
    Bezier::ControlPoints< Concrete::Coords2D > controls_b;
    const IntersectionPolys2D * baseSegs;
    Concrete::Time steps_a;
    Concrete::Time t_a0;
    Concrete::Time t_a1;
    Concrete::Time t_b0;
    Concrete::Time t_b1;

  public:
    IntersectionSegmentSections2D( const IntersectionPolys2D * _baseSegs, Concrete::Time _steps_a, Concrete::Time _t_a0, Concrete::Time _t_a1, Concrete::Time _t_b0, Concrete::Time _t_b1 );
    IntersectionSegmentSections2D * cutAfterAfter( Concrete::Time ta, Concrete::Time tb, Concrete::Time tol_a, Concrete::Time tol_b ) const;
    IntersectionSegmentSections2D * cutAfterBefore( Concrete::Time ta, Concrete::Time tb, Concrete::Time tol_a, Concrete::Time tol_b ) const;
    IntersectionSegmentSections2D * cutBeforeAfter( Concrete::Time ta, Concrete::Time tb, Concrete::Time tol_a, Concrete::Time tol_b ) const;
    IntersectionSegmentSections2D * cutBeforeBefore( Concrete::Time ta, Concrete::Time tb, Concrete::Time tol_a, Concrete::Time tol_b ) const;
    
    void splitTimes( Concrete::Time * dst_a, Concrete::Time * dst_b, const Concrete::Time t_tol_a, const Concrete::Time t_tol_b ) const;
    Concrete::Time globalTime_a( Concrete::Time t ) const;
    Concrete::Length distanceAt( Concrete::Time ta, Concrete::Time tb ) const;
    Concrete::Speed maxSpeed_a( ) const;
    Concrete::Speed maxSpeed_b( ) const;
    bool convexHullOverlap( ) const;
    void update_t_a1( Concrete::Time t );
    bool isEmpty( ) const;

    void writeTimes( std::ostream & os ) const;
  private:
    static bool convexHullOneWayOverlap( const std::vector< const Concrete::Coords2D * > & poly1, const std::vector< const Concrete::Coords2D * > & poly2 );
  };

  }
}

Concrete::SplineTime
Lang::ElementaryPath2D::intersection( const Lang::ElementaryPath2D & p2 ) const
{
  using namespace Computation;

  const Concrete::Length DISTANCE_TOL = 0.001 * Computation::the_arcdelta;
  const double CUT_LOSS = 0.01;

  if( size( ) == 0 )
    {
      throw Exceptions::OutOfRange( "The empty path does not intersect with anying." );
    }

//   std::cerr << "p1: " ;
//   this->writeInputForm( std::cerr );
//   std::cerr << std::endl
// 	    << "p2: " ;
//   p2.writeInputForm( std::cerr );
//   std::cerr << std::endl ;

  std::list< Bezier::ControlPoints< Concrete::Coords2D > > curvedSegments;
  typedef std::pair< Concrete::Coords2D *, Concrete::Coords2D * > StraightSegType;
  std::list< StraightSegType > straightSegments;
  {
    const_iterator i1 = p2.begin( );
    const_iterator i2 = i1;
    ++i2;
    for( ; i1 != p2.end( ); ++i1, ++i2 )
      {
	if( i2 == p2.end( ) )
	  {
	    if( p2.isClosed( ) )
	      {
		i2 = p2.begin( );
	      }
	    else
	      {
		break;
	      }
	  }
      if( (*i1)->front_ == (*i1)->mid_ &&
	  (*i2)->rear_ == (*i2)->mid_ )
	{
	  straightSegments.push_back( StraightSegType( (*i1)->mid_, (*i2)->mid_ ) );
	}
      else
	{
	  curvedSegments.push_back( Bezier::ControlPoints< Concrete::Coords2D >( *((*i1)->mid_), *((*i1)->front_), *((*i2)->rear_), *((*i2)->mid_) ) );
	}
      }
  }

  Concrete::Time steps = Concrete::ZERO_TIME;
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
	      break;
	    }
	}

      /* if the segment is straight a dedicated intersection-finder is invoked
       */
      if( (*i1)->front_ == (*i1)->mid_ &&
	  (*i2)->rear_ == (*i2)->mid_ )
	{
	  Concrete::Time t;
	  if( p2.lineSegmentIntersection( & t, *(*i1)->mid_, *(*i2)->mid_ ) )
	    {
	      return steps + t;
	    }
	  continue;
	}

      Bezier::ControlPoints< Concrete::Coords2D > seg_a( *((*i1)->mid_), *((*i1)->front_), *((*i2)->rear_), *((*i2)->mid_) );
      Bezier::PolyCoeffs< Concrete::Coords2D > seg_a_coeffs( seg_a );

      /* A segment may intersect with the other path at several places, but only the first shall be returned.
       * This makes it useful to track the earliest intersection time we've seen so far.  Then segments that are
       * after this point doesn't have to be searched at all, and segments before this point need only be
       * searched up to this point.
       */
      Concrete::Time t = Concrete::HUGE_TIME;

      /* Otherwise, I first scan the straight parts of the other path in order to find the easy intersections
       * as soon as possible.
       */
      for( std::list< StraightSegType >::const_iterator i = straightSegments.begin( ); i != straightSegments.end( ); ++i )
	{
	  Concrete::Coords2D rInv = ( *(i->second) - *(i->first) ) * ( 1. / Concrete::innerScalar( *(i->second) - *(i->first), *(i->second) - *(i->first) ) );
	  Concrete::Coords2D n( i->first->y_ - i->second->y_, i->second->x_ - i->first->x_ );
	  double offset = Concrete::innerScalar( n, *i->first );
	  double tmp[4];
	  seg_a_coeffs.hyperplaneIntersections( tmp, n, offset );
	  for( double * tmpi = tmp; *tmpi != HUGE_VAL; ++tmpi )
	    {
	      Concrete::Time t2( Concrete::innerScalar( rInv, seg_a_coeffs.point( *tmpi ) - *(i->first) ) );
	      if( Concrete::ZERO_TIME <= t2 && t2 <= Concrete::UNIT_TIME )
		{
		  t = min( t, Concrete::Time( *tmpi ) );
		}
	    }
	}
      
      /* The remaining segments are pushed onto a queue
       */
      std::list< IntersectionSegmentSections2D * > work;

      /* Keep track of the memory we use:
       */
      PtrOwner_back_Access< std::list< const IntersectionPolys2D * > > baseSegsMemory;
      
      {
	typedef typeof curvedSegments ListType;
	for( ListType::const_iterator seg_b = curvedSegments.begin( ); seg_b != curvedSegments.end( ); ++seg_b )
	  {
	    IntersectionSegmentSections2D * workItem;
	    IntersectionPolys2D * baseSegs = new IntersectionPolys2D( seg_a, *seg_b );
	    baseSegsMemory.push_back( baseSegs );
	    if( t < Concrete::HUGE_TIME )
	      {
		workItem = new IntersectionSegmentSections2D( baseSegs, steps, Concrete::ZERO_TIME, t, Concrete::ZERO_TIME, Concrete::UNIT_TIME );
	      }
	    else
	      {
		workItem = new IntersectionSegmentSections2D( baseSegs, steps, Concrete::ZERO_TIME, Concrete::UNIT_TIME, Concrete::ZERO_TIME, Concrete::UNIT_TIME );
	      }

	    pushOverlappingDeleteOther( work, workItem );
	  }
      }
		  
      /* Then we do the work...
       */
      while( work.size( ) > 0 )
	{
	  IntersectionSegmentSections2D * workItem = work.front( );
	  work.pop_front( );
	  workItem->update_t_a1( t );
	  if( workItem->isEmpty( ) )
	    {
	      delete workItem;
	      continue;
	    }

	  const Concrete::Time t_tol_1 = Computation::the_arcdelta / workItem->maxSpeed_a( );
	  const Concrete::Time t_tol_2 = Computation::the_arcdelta / workItem->maxSpeed_b( );

	  Concrete::Time t1;
	  Concrete::Time t2;
	  workItem->splitTimes( & t1, & t2, 0.001 * t_tol_1, 0.001 * t_tol_2 );  // too low precision gives erraneous results!
	  if( workItem->distanceAt( t1, t2 ) < DISTANCE_TOL )
	    {
	      t = min( t, t1 );
	      // We have an intersection; only two children to consider
	      pushOverlappingDeleteOther( work, workItem->cutAfterBefore( t1, t2, CUT_LOSS * t_tol_1, CUT_LOSS * t_tol_2 ) );
	      pushOverlappingDeleteOther( work, workItem->cutAfterAfter( t1, t2, CUT_LOSS * t_tol_1, CUT_LOSS * t_tol_2 ) );
	    }
	  else
	    {
	      // No intersection; four children to consider
	      pushOverlappingDeleteOther( work, workItem->cutBeforeBefore( t1, t2, CUT_LOSS * t_tol_1, CUT_LOSS * t_tol_2 ) );
	      pushOverlappingDeleteOther( work, workItem->cutBeforeAfter( t1, t2, CUT_LOSS * t_tol_1, CUT_LOSS * t_tol_2 ) );
	      pushOverlappingDeleteOther( work, workItem->cutAfterBefore( t1, t2, CUT_LOSS * t_tol_1, CUT_LOSS * t_tol_2 ) );
	      pushOverlappingDeleteOther( work, workItem->cutAfterAfter( t1, t2, CUT_LOSS * t_tol_1, CUT_LOSS * t_tol_2 ) );
	    }
	  delete workItem;
	}

      if( t < Concrete::HUGE_TIME ) // HUGE_VAL means that no intersection was found along the current segment.
	{
	  return steps + t;
	}
    }
  
  throw Exceptions::OutOfRange( "Failed to find intersection.  Probably because there was no intersection." );
}

void
Lang::ElementaryPath2D::pushOverlappingDeleteOther( std::list< Computation::IntersectionSegmentSections2D * > & work, Computation::IntersectionSegmentSections2D * item )
{
  if( item->convexHullOverlap( ) )
    {
      work.push_back( item );
    }
  else
    {
      delete item;
    }
}


Computation::IntersectionPolys2D::IntersectionPolys2D( const Bezier::ControlPoints< Concrete::Coords2D > & seg_a, const Bezier::ControlPoints< Concrete::Coords2D > & seg_b )
  : polyCoeffs_a( seg_a ), polyCoeffs_b( seg_b )
{
  kxaD0_0 = polyCoeffs_a.z0_.x_;
  kxaD0_1 = polyCoeffs_a.z1_.x_.offtype< 0, 1 >( );
  kxaD0_2 = polyCoeffs_a.z2_.x_.offtype< 0, 2 >( );
  kxaD0_3 = polyCoeffs_a.z3_.x_.offtype< 0, 3 >( );

  kxaD1_0 = kxaD0_1;
  kxaD1_1 = 2 * kxaD0_2;
  kxaD1_2 = 3 * kxaD0_3;

  kxaD2_0 = kxaD1_1;
  kxaD2_1 = 2 * kxaD1_2;

  kyaD0_0 = polyCoeffs_a.z0_.y_;
  kyaD0_1 = polyCoeffs_a.z1_.y_.offtype< 0, 1 >( );
  kyaD0_2 = polyCoeffs_a.z2_.y_.offtype< 0, 2 >( );
  kyaD0_3 = polyCoeffs_a.z3_.y_.offtype< 0, 3 >( );

  kyaD1_0 = kyaD0_1;
  kyaD1_1 = 2 * kyaD0_2;
  kyaD1_2 = 3 * kyaD0_3;

  kyaD2_0 = kyaD1_1;
  kyaD2_1 = 2 * kyaD1_2;  

  kxbD0_0 = polyCoeffs_b.z0_.x_;
  kxbD0_1 = polyCoeffs_b.z1_.x_.offtype< 0, 1 >( );
  kxbD0_2 = polyCoeffs_b.z2_.x_.offtype< 0, 2 >( );
  kxbD0_3 = polyCoeffs_b.z3_.x_.offtype< 0, 3 >( );

  kxbD1_0 = kxbD0_1;
  kxbD1_1 = 2 * kxbD0_2;
  kxbD1_2 = 3 * kxbD0_3;

  kxbD2_0 = kxbD1_1;
  kxbD2_1 = 2 * kxbD1_2;

  kybD0_0 = polyCoeffs_b.z0_.y_;
  kybD0_1 = polyCoeffs_b.z1_.y_.offtype< 0, 1 >( );
  kybD0_2 = polyCoeffs_b.z2_.y_.offtype< 0, 2 >( );
  kybD0_3 = polyCoeffs_b.z3_.y_.offtype< 0, 3 >( );

  kybD1_0 = kybD0_1;
  kybD1_1 = 2 * kybD0_2;
  kybD1_2 = 3 * kybD0_3;

  kybD2_0 = kybD1_1;
  kybD2_1 = 2 * kybD1_2;

  {
    Concrete::Length tmp = max( hypotPhysical( seg_a.p1_.x_ - seg_a.p0_.x_, seg_a.p1_.y_ - seg_a.p0_.y_ ),
					 hypotPhysical( seg_a.p2_.x_ - seg_a.p1_.x_, seg_a.p2_.y_ - seg_a.p1_.y_ ) );
    maxSpeed_a_ = max( tmp, hypotPhysical( seg_a.p3_.x_ - seg_a.p2_.x_, seg_a.p3_.y_ - seg_a.p2_.y_ ) ).offtype< 0, 1 >( );
  }
  {
    Concrete::Length tmp = max( hypotPhysical( seg_b.p1_.x_ - seg_b.p0_.x_, seg_b.p1_.y_ - seg_b.p0_.y_ ),
					 hypotPhysical( seg_b.p2_.x_ - seg_b.p1_.x_, seg_b.p2_.y_ - seg_b.p1_.y_ ) );
    maxSpeed_b_ = max( tmp, hypotPhysical( seg_b.p3_.x_ - seg_b.p2_.x_, seg_b.p3_.y_ - seg_b.p2_.y_ ) ).offtype< 0, 1 >( );
  }
}

void
Computation::IntersectionPolys2D::splitTimes( Concrete::Time * dst_a, Concrete::Time * dst_b, const Concrete::Time t_alow, const Concrete::Time t_ahigh, const Concrete::Time t_atol, const Concrete::Time t_blow, const Concrete::Time t_bhigh, const Concrete::Time t_btol ) const
{
  Concrete::Time ta;
  Concrete::Time tb;
  Physical< 2, 0 > last_f( HUGE_VAL );
  {
    const double GRID = 7;
    const Concrete::Time STEP_A = ( t_ahigh - t_alow ) / GRID;
    const Concrete::Time STEP_B = ( t_bhigh - t_blow ) / GRID;
    for( Concrete::Time tta = t_alow + 0.5 * STEP_A; tta < t_ahigh; tta += STEP_A )
      {
	for( Concrete::Time ttb = t_blow + 0.5 * STEP_B; ttb < t_bhigh; ttb += STEP_B )
	  {
	    Physical< 2, 0 > tmp = squaredDistanceAt( tta, ttb );
	    if( tmp < last_f )
	      {
		last_f = tmp;
		ta = tta;
		tb = ttb;
	      }
	  }
      }
  }
  Concrete::Time last_ta = - Concrete::UNIT_TIME;
  Concrete::Time last_tb = - Concrete::UNIT_TIME;

  while( ta < last_ta - t_atol || ta > last_ta + t_atol ||
	 tb < last_tb - t_btol || tb > last_tb + t_btol )
    {
      last_ta = ta;
      last_tb = tb;
      /* The derivatives below are actually computed for the objective divided by two
       * but since we will divide one by the other, it doesn't matter.
       */
      Physical< 2, -1 > objectiveD1a( 0 );
      Physical< 2, -1 > objectiveD1b( 0 );
      Physical< 2, -2 > objectiveD2aa( 0 );
      Physical< 2, -2 > objectiveD2ab( 0 );
      Physical< 2, -2 > objectiveD2bb( 0 );

      {
	/* x-components */
	Physical< 1, 0 > faD0 = kxaD0_0 + ta * ( kxaD0_1 + ta * ( kxaD0_2 + ta * kxaD0_3 ) );
	Physical< 1, 0 > fbD0 = kxbD0_0 + tb * ( kxbD0_1 + tb * ( kxbD0_2 + tb * kxbD0_3 ) );
	Physical< 1, -1 > faD1 = kxaD1_0 + ta * ( kxaD1_1 + ta * kxaD1_2 );
	Physical< 1, -1 > fbD1 = kxbD1_0 + tb * ( kxbD1_1 + tb * kxbD1_2 );
	Physical< 1, -2 > faD2 = kxaD2_0 + ta * kxaD2_1;
	Physical< 1, -2 > fbD2 = kxbD2_0 + tb * kxbD2_1;
	
	Physical< 1, 0 > deltaD0 = faD0 - fbD0;

	objectiveD1a += deltaD0 * faD1;
	objectiveD1b += - deltaD0 * fbD1;
	objectiveD2aa += faD1 * faD1 + deltaD0 * faD2;
	objectiveD2bb += fbD1 * fbD1 - deltaD0 * fbD2;
	objectiveD2ab += - faD1 * fbD1;
      }

      {
	/* y-components */
	Physical< 1, 0 > faD0 = kyaD0_0 + ta * ( kyaD0_1 + ta * ( kyaD0_2 + ta * kyaD0_3 ) );
	Physical< 1, 0 > fbD0 = kybD0_0 + tb * ( kybD0_1 + tb * ( kybD0_2 + tb * kybD0_3 ) );
	Physical< 1, -1 > faD1 = kyaD1_0 + ta * ( kyaD1_1 + ta * kyaD1_2 );
	Physical< 1, -1 > fbD1 = kybD1_0 + tb * ( kybD1_1 + tb * kybD1_2 );
	Physical< 1, -2 > faD2 = kyaD2_0 + ta * kyaD2_1;
	Physical< 1, -2 > fbD2 = kybD2_0 + tb * kybD2_1;
	
	Physical< 1, 0 > deltaD0 = faD0 - fbD0;

	objectiveD1a += deltaD0 * faD1;
	objectiveD1b += - deltaD0 * fbD1;
	objectiveD2aa += faD1 * faD1 + deltaD0 * faD2;
	objectiveD2bb += fbD1 * fbD1 - deltaD0 * fbD2;
	objectiveD2ab += - faD1 * fbD1;
      }

      Concrete::Time step_a;
      Concrete::Time step_b;
      if( objectiveD2aa <= Physical< 2, -2 >( 0 ) || objectiveD2bb <= Physical< 2, -2 >( 0 ) || objectiveD2ab * objectiveD2ab >= objectiveD2aa * objectiveD2bb )
	{
	  //	  std::cerr << "At point: ( ta, tb ) = ( " << ta << ", " << tb << " )" << std::endl ;
	  /* The minimization problem is not convex, locally.
	   * Knowing that we have examined a grid of candidate points, we stop the minimization here.
	   */
	  
	  break;

	  //	  double D1norm = hypotPhysical( objecitveD1a, objectiveD1b );
	  //	  double boxDiameter = max( t_ahigh - t_alow, t_bhigh - t_blow );
	  //	  step_a = - objectiveD1a * ( boxDiameter / D1norm );
	  //	  step_b = - objectiveD1b * ( boxDiameter / D1norm );
	}
      else
	{
	  Physical< -4, 4 > invDet = 1. / ( objectiveD2aa * objectiveD2bb - objectiveD2ab * objectiveD2ab );
	  //  here's the division where the missing factors of 2 cancel.
	  step_a = - invDet * (   objectiveD2bb * objectiveD1a - objectiveD2ab * objectiveD1b );
	  step_b = - invDet * ( - objectiveD2ab * objectiveD1a + objectiveD2aa * objectiveD1b );
	}

      bool onBound = false;
      {
	double alpha = 1;

	if( ta + alpha * step_a < t_alow )
	  {
	    alpha = ( t_alow - ta ) / step_a;
	    onBound = true;
	  }
	else if( ta + alpha * step_a > t_ahigh )
	  {
	    alpha = ( t_ahigh - ta ) / step_a;
	    onBound = true;
	  }
	
	if( tb + alpha * step_b < t_blow )
	  {
	    alpha = ( t_blow - tb ) / step_b;
	    onBound = true;
	  }
	else if( tb + alpha * step_b > t_bhigh )
	  {
	    alpha = ( t_bhigh - tb ) / step_b;
	    onBound = true;
	  }
	
	step_a *= alpha;
	step_b *= alpha;
      }

      Physical< 2, 0 > f = squaredDistanceAt( ta + step_a, tb + step_b );
      while( f > last_f && ( step_a.abs( ) > t_atol || step_b.abs( ) > t_btol ) )
	{
	  step_a = step_a * 0.5;
	  step_b = step_b * 0.5;
	  f = squaredDistanceAt( ta + step_a, tb + step_b );
	  onBound = false;
	}
      ta += step_a;
      tb += step_b;
      last_f = f;

      if( onBound )
	{
	  break;
	}
    }

  *dst_a = ta;
  *dst_b = tb;
}

Physical< 2, 0 >
Computation::IntersectionPolys2D::squaredDistanceAt( Concrete::Time ta, Concrete::Time tb ) const
{
  Concrete::Length fxaD0 = kxaD0_0 + ta * ( kxaD0_1 + ta * ( kxaD0_2 + ta * kxaD0_3 ) );
  Concrete::Length fyaD0 = kyaD0_0 + ta * ( kyaD0_1 + ta * ( kyaD0_2 + ta * kyaD0_3 ) );
  Concrete::Length fxbD0 = kxbD0_0 + tb * ( kxbD0_1 + tb * ( kxbD0_2 + tb * kxbD0_3 ) );
  Concrete::Length fybD0 = kybD0_0 + tb * ( kybD0_1 + tb * ( kybD0_2 + tb * kybD0_3 ) );
  Concrete::Length deltax = fxaD0 - fxbD0;
  Concrete::Length deltay = fyaD0 - fybD0;
  return deltax * deltax + deltay * deltay;
}

Concrete::Length
Computation::IntersectionPolys2D::distanceAt( Concrete::Time ta, Concrete::Time tb ) const
{
  Concrete::Length fxaD0 = kxaD0_0 + ta * ( kxaD0_1 + ta * ( kxaD0_2 + ta * kxaD0_3 ) );
  Concrete::Length fyaD0 = kyaD0_0 + ta * ( kyaD0_1 + ta * ( kyaD0_2 + ta * kyaD0_3 ) );
  Concrete::Length fxbD0 = kxbD0_0 + tb * ( kxbD0_1 + tb * ( kxbD0_2 + tb * kxbD0_3 ) );
  Concrete::Length fybD0 = kybD0_0 + tb * ( kybD0_1 + tb * ( kybD0_2 + tb * kybD0_3 ) );
  Concrete::Length deltax = fxaD0 - fxbD0;
  Concrete::Length deltay = fyaD0 - fybD0;
  return hypotPhysical( deltax, deltay );
}

Bezier::ControlPoints< Concrete::Coords2D >
Computation::IntersectionPolys2D::getControls_a( const Concrete::Time t_low, const Concrete::Time t_high ) const
{
  return Bezier::ControlPoints< Concrete::Coords2D >( polyCoeffs_a.subSection( t_low.offtype< 0, 1 >( ), t_high.offtype< 0, 1 >( ) ) );
}

Bezier::ControlPoints< Concrete::Coords2D >
Computation::IntersectionPolys2D::getControls_b( const Concrete::Time t_low, const Concrete::Time t_high ) const
{
  return Bezier::ControlPoints< Concrete::Coords2D >( polyCoeffs_b.subSection( t_low.offtype< 0, 1 >( ), t_high.offtype< 0, 1 >( ) ) );
}
 
Computation::IntersectionSegmentSections2D::IntersectionSegmentSections2D( const IntersectionPolys2D * _baseSegs, Concrete::Time _steps_a, Concrete::Time _t_a0, Concrete::Time _t_a1, Concrete::Time _t_b0, Concrete::Time _t_b1 )
  : controls_a( _baseSegs->getControls_a( _t_a0, _t_a1 ) ), controls_b( _baseSegs->getControls_b( _t_b0, _t_b1 ) ),
    baseSegs( _baseSegs ), steps_a( _steps_a ), t_a0( _t_a0 ), t_a1( _t_a1 ), t_b0( _t_b0 ), t_b1( _t_b1 )
{ }

Computation::IntersectionSegmentSections2D *
Computation::IntersectionSegmentSections2D::cutAfterAfter( Concrete::Time ta, Concrete::Time tb, Concrete::Time tol_a, Concrete::Time tol_b ) const
{
  return new IntersectionSegmentSections2D( baseSegs, steps_a, t_a0, ta - tol_a, t_b0, tb - tol_a );
}

Computation::IntersectionSegmentSections2D *
Computation::IntersectionSegmentSections2D::cutAfterBefore( Concrete::Time ta, Concrete::Time tb, Concrete::Time tol_a, Concrete::Time tol_b ) const
{
  return new IntersectionSegmentSections2D( baseSegs, steps_a, t_a0, ta - tol_a, tb + tol_a, t_b1 );
}

Computation::IntersectionSegmentSections2D *
Computation::IntersectionSegmentSections2D::cutBeforeAfter( Concrete::Time ta, Concrete::Time tb, Concrete::Time tol_a, Concrete::Time tol_b ) const
{
  return new IntersectionSegmentSections2D( baseSegs, steps_a, ta + tol_a, t_a1, t_b0, tb - tol_a );
}

Computation::IntersectionSegmentSections2D *
Computation::IntersectionSegmentSections2D::cutBeforeBefore( Concrete::Time ta, Concrete::Time tb, Concrete::Time tol_a, Concrete::Time tol_b ) const
{
  return new IntersectionSegmentSections2D( baseSegs, steps_a, ta + tol_a, t_a1, tb + tol_a, t_b1 );
}

void
Computation::IntersectionSegmentSections2D::splitTimes( Concrete::Time * dst_a, Concrete::Time * dst_b, const Concrete::Time t_tol_a, const Concrete::Time t_tol_b ) const
{
  Concrete::Time da = 0.01 * ( t_a1 - t_a0 );
  Concrete::Time db = 0.01 * ( t_b1 - t_b0 );
  baseSegs->splitTimes( dst_a, dst_b,
			t_a0, t_a1, t_tol_a,
			t_b0, t_b1, t_tol_b );
  if( *dst_a < t_a0 + da && *dst_b < t_b0 + db )
    {
      *dst_a = t_a0 + da;
      *dst_b = t_b0 + db;
    }
  else if( *dst_a < t_a0 + da && *dst_b > t_b1 - db )
    {
      *dst_a = t_a0 + da;
      *dst_b = t_b1 - db;
    }
  else if( *dst_a > t_a1 - da && *dst_b < t_b0 + db )
    {
      *dst_a = t_a1 - da;
      *dst_b = t_b0 + db;
    }
  else if( *dst_a > t_a1 - da && *dst_b > t_b1 - db )
    {
      *dst_a = t_a1 - da;
      *dst_b = t_b1 - db;
    }
}

Concrete::Time
Computation::IntersectionSegmentSections2D::globalTime_a( Concrete::Time t ) const
{
  return steps_a + t;
}

Concrete::Length
Computation::IntersectionSegmentSections2D::distanceAt( Concrete::Time ta, Concrete::Time tb ) const
{
  return baseSegs->distanceAt( ta, tb );
}

bool
Computation::IntersectionSegmentSections2D::convexHullOverlap( ) const
{
  /* The implementation is based around the idea that if there is a separating line
   * (which there is exactly when the polygons don't overlap), there has to be
   * a direction of such a line that is also the direction from one point to another
   * within a polygon.  This gives some symmetry, which is implemented by
   * calling a function that searches such directions in only one of the polygons.
   * A quick check using enclosing circles is performed first to detect the simples cases.
   */
  std::vector< const Concrete::Coords2D * > pointSet_a( 0 );
  pointSet_a.reserve( 4 );
  pointSet_a.push_back( & controls_a.p0_ );
  pointSet_a.push_back( & controls_a.p1_ );
  pointSet_a.push_back( & controls_a.p2_ );
  pointSet_a.push_back( & controls_a.p3_ );
  std::vector< const Concrete::Coords2D * > pointSet_b( 0 );
  pointSet_b.reserve( 4 );
  pointSet_b.push_back( & controls_b.p0_ );
  pointSet_b.push_back( & controls_b.p1_ );
  pointSet_b.push_back( & controls_b.p2_ );
  pointSet_b.push_back( & controls_b.p3_ );

  /* Do the circle-circle check
   */
  {
    Concrete::Coords2D ca( 0, 0 );
    for( std::vector< const Concrete::Coords2D * >::const_iterator i = pointSet_a.begin( );
	 i != pointSet_a.end( );
	 ++i )
      {
	ca = ca + **i;
      }
    ca = ca * 0.25;

    Concrete::Coords2D cb( 0, 0 );
    for( std::vector< const Concrete::Coords2D * >::const_iterator i = pointSet_b.begin( );
	 i != pointSet_b.end( );
	 ++i )
      {
	cb = cb + **i;
      }
    cb = cb * 0.25;

    Concrete::Length ra = 0;
    for( std::vector< const Concrete::Coords2D * >::const_iterator i = pointSet_a.begin( );
	 i != pointSet_a.end( );
	 ++i )
      {
	ra = max( ra, hypotPhysical( ca.x_ - (*i)->x_, ca.y_ - (*i)->y_ ) );
      }

    Concrete::Length rb = 0;
    for( std::vector< const Concrete::Coords2D * >::const_iterator i = pointSet_b.begin( );
	 i != pointSet_b.end( );
	 ++i )
      {
	rb = max( rb, hypotPhysical( cb.x_ - (*i)->x_, cb.y_ - (*i)->y_ ) );
      }

    if( ( ra + rb ) * ( ra + rb ) < ( ca.x_ - cb.x_ ) * ( ca.x_ - cb.x_ ) + ( ca.y_ - cb.y_ ) * ( ca.y_ - cb.y_ ) )
      {
	return false;
      }
  }

  /* Now to the search for a separating line.
   */
  if( ! convexHullOneWayOverlap( pointSet_a, pointSet_b ) )
    {
      return false;
    }
  if( ! convexHullOneWayOverlap( pointSet_b, pointSet_a ) )
    {
      return false;
    }
  return true;
}

bool
Computation::IntersectionSegmentSections2D::convexHullOneWayOverlap( const std::vector< const Concrete::Coords2D * > & pointSet1, const std::vector< const Concrete::Coords2D * > & pointSet2 )
{
  Physical< 2, 0 > coincide_tol = 0.0001 * Computation::the_arcdelta * Computation::the_arcdelta;
  for( std::vector< const Concrete::Coords2D * >::const_iterator i0 = pointSet1.begin( ); i0 != pointSet1.end( ); ++i0 )
    {
      std::vector< const Concrete::Coords2D * >::const_iterator i1 = i0;
      ++i1;
      for( ; i1 != pointSet1.end( ); ++i1 )
	{
	  const Concrete::Coords2D & p0( **i0 );
	  const Concrete::Coords2D & p1( **i1 );

	  /* The two points bound the convex hull iff the other points are on the same side of the segment.
	   * If they are on the same side and p is on the other side, then the distance to p is of interest.
	   */
	  const Concrete::Length tx = p1.x_ - p0.x_;
	  const Concrete::Length ty = p1.y_ - p0.y_;
	  if( tx * tx + ty * ty < coincide_tol )
	    {
	      continue;
	    }
	  bool counterClockwise;  // true when ( p0, p1 ) are ordered counter-clockwise around the interior.
	  {
	    std::vector< const Concrete::Coords2D * >::const_iterator i2 = pointSet1.begin( );
	    for( ; ; ++i2 ) // we don't have to check against pointSet1.end( )
	      {
		if( i2 == i0 || i2 == i1 )
		  {
		    continue;
		  }
		const Concrete::Length dx = (*i2)->x_ - p0.x_;
		const Concrete::Length dy = (*i2)->y_ - p0.y_;
		counterClockwise = ( ty * dx - tx * dy < 0 );
		break;
	      }
	    ++i2;
	    for( ; ; ++i2 ) // we don't have to check against pointSet1.end( )
	      {
		if( i2 == i0 || i2 == i1 )
		  {
		    continue;
		  }
		const Concrete::Length dx = (*i2)->x_ - p0.x_;
		const Concrete::Length dy = (*i2)->y_ - p0.y_;
		if( counterClockwise == ( ty * dx - tx * dy < 0 ) )
		  {
		    goto checkDistance;
		  }
		break;
	      }
	    continue; // the points were on different sides.
	  checkDistance:

	    bool allOutside = true;
	    for( std::vector< const Concrete::Coords2D * >::const_iterator p = pointSet2.begin( ); p != pointSet2.end( ); ++p )
	      {
		const Concrete::Length dx = (*p)->x_ - p0.x_;
		const Concrete::Length dy = (*p)->y_ - p0.y_;
		if( ( ty * dx - tx * dy > 0 ) != counterClockwise )
		  {
		    allOutside = false;
		    break;
		  }
	      }
	    if( allOutside )
	      {
		return false;
	      }
	  }
	}
    }

  /* Note that this is only one part of the test, only if both symmetric tests return true do the
   * polygons overlap.
   */
  return true;
}


void
Computation::IntersectionSegmentSections2D::update_t_a1( Concrete::Time t )
{
  t_a1 = min( t_a1, t );
}

bool
Computation::IntersectionSegmentSections2D::isEmpty( ) const
{
  return t_a1 < t_a0;
}

void
Computation::IntersectionSegmentSections2D::writeTimes( std::ostream & os ) const
{
  os << Concrete::Time::offtype( t_a0 ) << "--" << Concrete::Time::offtype( t_a1 ) << ", "
     << Concrete::Time::offtype( t_b0 ) << "--" << Concrete::Time::offtype( t_b1 ) << std::endl ;
}


Concrete::Speed
Computation::IntersectionSegmentSections2D::maxSpeed_a( ) const
{
  return baseSegs->maxSpeed_a( );
}

Concrete::Speed
Computation::IntersectionSegmentSections2D::maxSpeed_b( ) const
{
  return baseSegs->maxSpeed_b( );
//   Concrete::Length lengthBound =
//     hypotPhysical( controls_b.p1_.x_ - controls_b.p0_.x_, controls_b.p1_.y_ - controls_b.p0_.y_ ) +
//     hypotPhysical( controls_b.p2_.x_ - controls_b.p1_.x_, controls_b.p2_.y_ - controls_b.p1_.y_ ) +
//     hypotPhysical( controls_b.p3_.x_ - controls_b.p2_.x_, controls_b.p3_.y_ - controls_b.p2_.y_ );

//   return arclen_tol * ( t_b1 - t_b0 ) / *lengthBound;
}

RefCountPtr< const Lang::ElementaryPath2D >
Lang::ElementaryPath2D::subpath( const Concrete::SplineTime splt1, const Concrete::SplineTime splt2 ) const
{
  if( size( ) == 0 )
    {
      throw Exceptions::OutOfRange( "The empty path has no subpaths." );
    }
  Lang::ElementaryPath2D * res = new Lang::ElementaryPath2D;

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
      const Concrete::PathPoint2D * p1;
      const Concrete::PathPoint2D * p2;
      findSegment( gt1, &t1, &p1, &p2 );  /* note that the stored pointers must not be deleted */
      Concrete::Time t2 = gt2 - ceil( gt2.offtype< 0, 1 >( ) ) + 1;
      Concrete::Bezier x0 = p1->mid_->x_.offtype< 0, 3 >( );
      Concrete::Bezier y0 = p1->mid_->y_.offtype< 0, 3 >( );
      Concrete::Bezier x1 = p1->front_->x_.offtype< 0, 3 >( );
      Concrete::Bezier y1 = p1->front_->y_.offtype< 0, 3 >( );
      Concrete::Bezier x2 = p2->rear_->x_.offtype< 0, 3 >( );
      Concrete::Bezier y2 = p2->rear_->y_.offtype< 0, 3 >( );
      Concrete::Bezier x3 = p2->mid_->x_.offtype< 0, 3 >( );
      Concrete::Bezier y3 = p2->mid_->y_.offtype< 0, 3 >( );
      
      Concrete::Time tc = t1 - 1;  /* Note the sign here! */
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

      Concrete::PathPoint2D * ptmp = new Concrete::PathPoint2D( x0new, y0new );
      ptmp->front_ = new Concrete::Coords2D( x1new, y1new );
      res->push_back( ptmp );
      ptmp = new Concrete::PathPoint2D( x3new, y3new );
      ptmp->rear_ = new Concrete::Coords2D( x2new, y2new );
      res->push_back( ptmp );

      return RefCountPtr< const Lang::ElementaryPath2D >( res );
    }


  /* Now, we treat the other case, where the cuts are on different segments.  This results in a three phase
   * algorithm.
   * Since "cutting away after" requires much simpler formulas than straight-forward "cutting away before",
   * the latter is treated as the former, but with time reversed.
   */

  Concrete::Time t = Concrete::ZERO_TIME;
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
  Concrete::PathPoint2D * p1;
  Concrete::PathPoint2D * p2;
  if( t1 == 0 )
    {
      p1 = new Concrete::PathPoint2D( **i1 );
      p2 = new Concrete::PathPoint2D( **i2 );
    }
  else
    {
      /* Reverse time! */
      Concrete::Bezier x3 = (*i1)->mid_->x_.offtype< 0, 3 >( );
      Concrete::Bezier y3 = (*i1)->mid_->y_.offtype< 0, 3 >( );
      Concrete::Bezier x2 = (*i1)->front_->x_.offtype< 0, 3 >( );
      Concrete::Bezier y2 = (*i1)->front_->y_.offtype< 0, 3 >( );
      Concrete::Bezier x1 = (*i2)->rear_->x_.offtype< 0, 3 >( );
      Concrete::Bezier y1 = (*i2)->rear_->y_.offtype< 0, 3 >( );
      Concrete::Bezier x0 = (*i2)->mid_->x_.offtype< 0, 3 >( );
      Concrete::Bezier y0 = (*i2)->mid_->y_.offtype< 0, 3 >( );
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

      Concrete::Bezier y0new = k0y;
      Concrete::Bezier y1new = k0y + k1y / 3;
      Concrete::Bezier y2new = k0y + ( 2 * k1y + k2y ) / 3;
      Concrete::Bezier y3new = k0y + k1y + k2y + k3y;

      /* Now, reverse time again! */

      p1 = new Concrete::PathPoint2D( x3new.offtype< 0, -3 >( ), y3new.offtype< 0, -3 >( ) );
      p1->front_ = new Concrete::Coords2D( x2new.offtype< 0, -3 >( ), y2new.offtype< 0, -3 >( ) );
      p2 = new Concrete::PathPoint2D( x0new.offtype< 0, -3 >( ), y0new.offtype< 0, -3 >( ) );  /* This point must be the same as before. */
      p2->rear_ = new Concrete::Coords2D( x1new.offtype< 0, -3 >( ), y1new.offtype< 0, -3 >( ) );
      p2->front_ = new Concrete::Coords2D( *(*i2)->front_ );
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
      p2 = new Concrete::PathPoint2D( **i2 );      
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

      Concrete::Bezier x0 = p1->mid_->x_.offtype< 0, 3 >( );
      Concrete::Bezier y0 = p1->mid_->y_.offtype< 0, 3 >( );
      Concrete::Bezier x1 = p1->front_->x_.offtype< 0, 3 >( );
      Concrete::Bezier y1 = p1->front_->y_.offtype< 0, 3 >( );
      Concrete::Bezier x2 = p2->rear_->x_.offtype< 0, 3 >( );
      Concrete::Bezier y2 = p2->rear_->y_.offtype< 0, 3 >( );
      Concrete::Bezier x3 = p2->mid_->x_.offtype< 0, 3 >( );
      Concrete::Bezier y3 = p2->mid_->y_.offtype< 0, 3 >( );

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

      p1 = new Concrete::PathPoint2D( x0new.offtype< 0, -3 >( ), y0new.offtype< 0, -3 >( ) );
      p1->front_ = new Concrete::Coords2D( x1new.offtype< 0, -3 >( ), y1new.offtype< 0, -3 >( ) );
      p1->rear_ = new Concrete::Coords2D( xn1, yn1 );
      p2 = new Concrete::PathPoint2D( x3new.offtype< 0, -3 >( ), y3new.offtype< 0, -3 >( ) );
      p2->rear_ = new Concrete::Coords2D( x2new.offtype< 0, -3 >( ), y2new.offtype< 0, -3 >( ) );

      res->push_back( p1 );
      res->push_back( p2 );
  }


  return RefCountPtr< const Lang::ElementaryPath2D >( res );
}

RefCountPtr< const Lang::ElementaryPath2D >
Lang::ElementaryPath2D::reverse( ) const
{
  /* Note that reversing a closed path is not quite as straight-forward as one might first think!
   */

  Lang::ElementaryPath2D * res = new Lang::ElementaryPath2D;
  if( closed_ )
    {
      res->close( );
    }

  const_iterator i = begin( );
  if( i == end( ) )
    {
      return RefCountPtr< const Lang::ElementaryPath2D >( res );
    }

  if( closed_ )
    {
      // The first path point remains first!
      ++i;
    }
  for( ; i != end( ); ++i )
    {
      Concrete::PathPoint2D * newPoint = new Concrete::PathPoint2D( new Concrete::Coords2D( *((*i)->mid_) ) );
      if( (*i)->rear_ != (*i)->mid_ )
	{
	  newPoint->front_ = new Concrete::Coords2D( *((*i)->rear_) );
	}
      if( (*i)->front_ != (*i)->mid_ )
	{
	  newPoint->rear_ = new Concrete::Coords2D( *((*i)->front_) );
	}
      res->push_front( newPoint );
    }
  if( closed_ )
    {
      i = begin( );
      Concrete::PathPoint2D * newPoint = new Concrete::PathPoint2D( new Concrete::Coords2D( *((*i)->mid_) ) );
      if( (*i)->rear_ != (*i)->mid_ )
	{
	  newPoint->front_ = new Concrete::Coords2D( *((*i)->rear_) );
	}
      if( (*i)->front_ != (*i)->mid_ )
	{
	  newPoint->rear_ = new Concrete::Coords2D( *((*i)->front_) );
	}
      res->push_front( newPoint );
    }

  return RefCountPtr< const Lang::ElementaryPath2D >( res );
}

void
Lang::ElementaryPath2D::show( std::ostream & os ) const
{
  os << "Elementary subpath with " << size( ) << " path points" ;
}

void
Lang::ElementaryPath2D::gcMark( Kernel::GCMarkedSet & marked )
{ }

RefCountPtr< const Lang::SubPath3D >
Lang::ElementaryPath2D::typed_to3D( const RefCountPtr< const Lang::SubPath2D > & self ) const
{
  typedef const Lang::ElementaryPath2D ArgType;
  RefCountPtr< ArgType > selfTyped = self.down_cast< ArgType >( );
  if( selfTyped == NullPtr< ArgType >( ) )
    {
      throw Exceptions::InternalError( "ElementaryPath2D::typed_to3D: self was of unexpected type." );
    }
  
  return RefCountPtr< const Lang::SubPath3D >( new Lang::SubPath2Din3D( selfTyped ) );
}


RefCountPtr< const Lang::Geometric3D >
Lang::ElementaryPath2D::to3D( const RefCountPtr< const Lang::Geometric2D > & self ) const
{
  typedef const Lang::ElementaryPath2D ArgType;
  RefCountPtr< ArgType > selfTyped = self.down_cast< ArgType >( );
  if( selfTyped == NullPtr< ArgType >( ) )
    {
      throw Exceptions::InternalError( "ElementaryPath2D::to3D: self was of unexpected type." );
    }
  
  return RefCountPtr< const Lang::Geometric3D >( new Lang::SubPath2Din3D( selfTyped ) );  
}
