#include "upsamplers.h"
#include "consts.h"
#include "pathtypes.h"

#include <cmath>

using namespace MetaPDF;

void
Computation::UpsampleInflections::operator () ( std::vector< double > * dst, const Bezier::ControlPoints< Concrete::Coords2D > & controls ) const
{
  if( controls.p1_ == controls.p0_ &&
      controls.p2_ == controls.p3_ )
    {
      // There are no inflections on a straight segment.
      return;
    }

  Bezier::PolyCoeffs< Concrete::Coords2D > coeffs( controls );
  double t[3];
  coeffs.inflections( t );
  // Now, we must just ensure that the times are sorted.
  double * src = & t[0];
  if( *src == HUGE_VAL )
    {
      // There were no inflections.
      return;
    }
  ++src;
  if( *src == HUGE_VAL )
    {
      // There were one inflections.
      dst->push_back( t[0] );
      return;
    }
  // There were two inflections.
  if( t[0] <= t[1] )
    {
      dst->push_back( t[0] );
      dst->push_back( t[1] );
    }
  else
    {
      dst->push_back( t[1] );
      dst->push_back( t[0] );
    }
}

void
Computation::UpsampleBends::operator () ( std::vector< double > * dst, const Bezier::ControlPoints< Concrete::Coords2D > & controls ) const
{
  /* We begin by constructing inflection-free segments, which can then be upsampled at given angles.
     See UpsampleInflections.
   */

  static Computation::UpsampleInflections inflectionSampler;

  if( controls.p1_ == controls.p0_ &&
      controls.p2_ == controls.p3_ )
    {
      // There are no bends on a straight segment.
      return;
    }

  std::vector< double > inflectionSamples;
  inflectionSampler( & inflectionSamples, controls );
  inflectionSamples.push_back( 1 );

  Concrete::Coords2D p0 = controls.p0_;
  double aStart;
  Concrete::Coords2D d1( 0, 0 );
  double aFinal;
  if( controls.p1_ == controls.p0_ )
    {
      d1 = controls.p2_ - p0;
      Concrete::Coords2D d2 = controls.p3_ - controls.p2_;
      aStart = atan2( d1.y_.offtype< 1, 0 >( ), d1.x_.offtype< 1, 0 >( ) );
      aFinal = atan2( d2.y_.offtype< 1, 0 >( ), d2.x_.offtype< 1, 0 >( ) );
    }
  else if( controls.p2_ == controls.p3_ )
    {
      d1 = controls.p1_ - p0;
      Concrete::Coords2D d2 = controls.p3_ - controls.p2_;
      aStart = atan2( d1.y_.offtype< 1, 0 >( ), d1.x_.offtype< 1, 0 >( ) );
      aFinal = atan2( d2.y_.offtype< 1, 0 >( ), d2.x_.offtype< 1, 0 >( ) );
    }
  else
    {
      d1 = controls.p1_ - p0;
      Concrete::Coords2D d2 = controls.p3_ - controls.p2_;
      aStart = atan2( d1.y_.offtype< 1, 0 >( ), d1.x_.offtype< 1, 0 >( ) );
      aFinal = atan2( d2.y_.offtype< 1, 0 >( ), d2.x_.offtype< 1, 0 >( ) );
    }
  
  Bezier::PolyCoeffs< Concrete::Coords2D > coeffs( controls );

  double a1 = aStart;
  double a2;
  double t1 = 0;
  double t2;
  Concrete::Coords2D d2( 0, 0 );
  Concrete::Coords2D p3( 0, 0 );
  typedef typeof inflectionSamples ListType;
  for( ListType::const_iterator inflection_i = inflectionSamples.begin( );
       inflection_i != inflectionSamples.end( );
       ++inflection_i, a1 = a2, t1 = t2, d1 = d2, p0 = p3 )
    {
      t2 = *inflection_i;
      p3 = coeffs.point( t2 );
      if( t2 >= 1 )
	{
	  a2 = aFinal;
	}
      else
	{
	  d2 = coeffs.velocity( t2 );
	  a2 = atan2( d2.y_.offtype< 1, 0 >( ), d2.x_.offtype< 1, 0 >( ) );
	  
	}
      /* Check if the turn is counter clockwise or not.
       */
      bool ccw = static_cast< double >( Concrete::inner( d1.quarterTurnCounterClockwise( ), p3 - p0 ).offtype< 2, 0 >( ) ) > 0.;
      if( ccw )
	{
	  if( a2 < a1 )
	    {
	      a2 += 2 * M_PI;
	    }
	}
      else
	{
	  if( a2 > a1 )
	    {
	      a2 -= 2 * M_PI;
	    }
	}

      double steps = ceil( fabs( a2 - a1 ) / maxAngle_ );
      double aStep = ( a2 - a1 ) / steps;
      double a = a1 + aStep;
      double lastTime = t1;
      for( double i = 1; i < steps; ++i, a += aStep )
	{
	  /* Locate the angle a by finding a point where the velocity is orthogonal to a vector orthogonal to a.
	   */
	  Concrete::UnitFloatPair da( cos( a ), sin( a ), bool( ) );
	  Concrete::Coords2D an( cos( a + M_PI_2 ), sin( a + M_PI_2 ) );
	  
	  double optTimes[3];
	  coeffs.stationaryPoints( optTimes, an ); // A HUGE_VAL is used as terminator in the result.
	  double best_t = HUGE_VAL;
	  for( double * src = & optTimes[0]; *src != HUGE_VAL; ++src )
	    {
	      if( *src <= lastTime ||
		  *src >= t2 )
		{
		  continue;
		}
	      Concrete::Coords2D v = coeffs.velocity( *src );
	      if( Concrete::inner( da, v ) > Concrete::ZERO_LENGTH )
		{
		  best_t = *src;
		}
	    }
	  if( best_t < HUGE_VAL )
	    {
	      dst->push_back( best_t );
	      lastTime = best_t;
	    }
	}
      if( t2 < 1 )
	{
	  dst->push_back( t2 );
	}
    }
    
}

void
Computation::UpsampleEvery2D::operator () ( std::vector< double > * dst, const Bezier::ControlPoints< Concrete::Coords2D > & controls ) const
{
  if( controls.p1_ == controls.p0_ &&
      controls.p2_ == controls.p3_ )
    {
      Concrete::Length totLen = ( controls.p3_ - controls.p0_ ).norm( );

      double steps = ceil( totLen / period_ );
      double sampleInterval = 1 / steps;
      double nextSample = sampleInterval;
      for( double i = 1; i < steps; ++i, nextSample += sampleInterval )
	{
	  dst->push_back( MetaPDF::straightLineArcTime( nextSample ).offtype< 0, 1 >( ) );
	}
    }
  else
    {
      Concrete::Bezier x0 = controls.p0_.x_.offtype< 0, 3 >( );
      Concrete::Bezier y0 = controls.p0_.y_.offtype< 0, 3 >( );
      Concrete::Bezier x1 = controls.p1_.x_.offtype< 0, 3 >( );
      Concrete::Bezier y1 = controls.p1_.y_.offtype< 0, 3 >( );
      Concrete::Bezier x2 = controls.p2_.x_.offtype< 0, 3 >( );
      Concrete::Bezier y2 = controls.p2_.y_.offtype< 0, 3 >( );
      Concrete::Bezier x3 = controls.p3_.x_.offtype< 0, 3 >( );
      Concrete::Bezier y3 = controls.p3_.y_.offtype< 0, 3 >( );

      const Concrete::Length segLengthBound =
	( hypotPhysical( x1-x0, y1-y0 ) + hypotPhysical( x2-x1, y2-y1 ) + hypotPhysical( x3-x2, y3-y2 ) ).offtype< 0, -3 >( );

      if( segLengthBound < period_ )
	{
	  return;
	}

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
      Concrete::Length totlen = tmpSum_l * dt.offtype< 0, 1 >( );

      double steps = ceil( totlen / period_ );
      Concrete::Length sampleInterval = totlen / steps;
      Concrete::Length nextSample = sampleInterval;
      {
	Concrete::Length tmpSum_l = Concrete::ZERO_LENGTH;
	Concrete::Time t = Concrete::ZERO_TIME;
	for( double i = 1; i < steps; ++i, nextSample += sampleInterval )
	  {
	    const Concrete::Length breakDiv_dt = nextSample / dt.offtype< 0, 1 >( );
	    for( ; ; t += dt )
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
		if( tmpSum_l >= breakDiv_dt )
		  {
		    dst->push_back( t.offtype< 0, 1 >( ) );
		    break;
		  }
	      }
	  }
	}
    }
}

void
Computation::UpsampleEvery3D::operator () ( std::vector< double > * dst, const Bezier::ControlPoints< Concrete::Coords3D > & controls ) const
{
  if( controls.p1_ == controls.p0_ &&
      controls.p2_ == controls.p3_ )
    {
      Concrete::Length totLen = ( controls.p3_ - controls.p0_ ).norm( );

      double steps = ceil( totLen / period_ );
      double sampleInterval = 1 / steps;
      double nextSample = sampleInterval;
      for( double i = 1; i < steps; ++i, nextSample += sampleInterval )
	{
	  dst->push_back( MetaPDF::straightLineArcTime( nextSample ).offtype< 0, 1 >( ) );
	}
    }
  else
    {
      Concrete::Bezier x0 = controls.p0_.x_.offtype< 0, 3 >( );
      Concrete::Bezier y0 = controls.p0_.y_.offtype< 0, 3 >( );
      Concrete::Bezier z0 = controls.p0_.z_.offtype< 0, 3 >( );
      Concrete::Bezier x1 = controls.p1_.x_.offtype< 0, 3 >( );
      Concrete::Bezier y1 = controls.p1_.y_.offtype< 0, 3 >( );
      Concrete::Bezier z1 = controls.p1_.z_.offtype< 0, 3 >( );
      Concrete::Bezier x2 = controls.p2_.x_.offtype< 0, 3 >( );
      Concrete::Bezier y2 = controls.p2_.y_.offtype< 0, 3 >( );
      Concrete::Bezier z2 = controls.p2_.z_.offtype< 0, 3 >( );
      Concrete::Bezier x3 = controls.p3_.x_.offtype< 0, 3 >( );
      Concrete::Bezier y3 = controls.p3_.y_.offtype< 0, 3 >( );
      Concrete::Bezier z3 = controls.p3_.z_.offtype< 0, 3 >( );

      const Concrete::Length segLengthBound =
	( hypotPhysical( x1-x0, y1-y0, z1-z0 ) + hypotPhysical( x2-x1, y2-y1, z2-z1 ) + hypotPhysical( x3-x2, y3-y2, z3-z2 ) ).offtype< 0, -3 >( );

      if( segLengthBound < period_ )
	{
	  return;
	}

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
	  Concrete::Speed vz = z0 * kv0 + z1 * kv1 + z2 * kv2 + z3 * kv3;
	  
	  Concrete::Length dl = hypotPhysical( vx, vy, vz ).offtype< 0, -1 >( );
	  tmpSum_l += dl;
	}
      Concrete::Length totlen = tmpSum_l * dt.offtype< 0, 1 >( );

      double steps = ceil( totlen / period_ );
      Concrete::Length sampleInterval = totlen / steps;
      Concrete::Length nextSample = sampleInterval;
      {
	Concrete::Length tmpSum_l = Concrete::ZERO_LENGTH;
	Concrete::Time t = Concrete::ZERO_TIME;
	for( double i = 1; i < steps; ++i, nextSample += sampleInterval )
	  {
	    const Concrete::Length breakDiv_dt = nextSample / dt.offtype< 0, 1 >( );
	    for( ; ; t += dt )
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
		if( tmpSum_l >= breakDiv_dt )
		  {
		    dst->push_back( t.offtype< 0, 1 >( ) );
		    break;
		  }
	      }
	  }
	}
    }
}
