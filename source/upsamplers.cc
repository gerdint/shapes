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

#include "upsamplers.h"
#include "consts.h"
#include "pathtypes.h"
#include "globals.h"

#include <cmath>
#include <algorithm>

using namespace Shapes;

void
Computation::UpsampleInflections::operator () ( std::vector< double > * dst, const Bezier::ControlPoints< Concrete::Coords2D > & controls ) const
{
	if( controls.p1_ == controls.p0_ &&
			controls.p2_ == controls.p3_ )
		{
			// There are no inflections on a straight segment.
			return;
		}

	/* It is sometimes a limiting case that the acceleration and velocity are parallel at one or both of the endpoints.
	 * However, it is no good to upsample at the endpoints, so we must work with a tolarance here.
	 */
	Concrete::Speed maxSpeed = std::max( std::max( ( controls.p1_ - controls.p0_ ).norm( ),
																									( controls.p2_ - controls.p1_ ).norm( ) ),
																				( controls.p3_ - controls.p2_ ).norm( ) ).offtype< 0, 1 >( );
	const double t_tol = ( Computation::the_arcdelta / maxSpeed ).offtype< 0, 1 >( );
	const double t_min = t_tol;
	const double t_max = 1 - t_tol;

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
			if( t_min < t[0] && t[0] < t_max )
				{
					dst->push_back( t[0] );
				}
			return;
		}
	// There were two inflections.
	if( t[0] <= t[1] )
		{
			if( t_min < t[0] && t[0] < t_max )
				{
					dst->push_back( t[0] );
				}
			if( t_min < t[1] && t[1] < t_max )
				{
					dst->push_back( t[1] );
				}
		}
	else
		{
			if( t_min < t[1] && t[1] < t_max )
				{
					dst->push_back( t[1] );
				}
			if( t_min < t[0] && t[0] < t_max )
				{
					dst->push_back( t[0] );
				}
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

	Bezier::PolyCoeffs< Concrete::Coords2D > coeffs( controls );

	std::vector< double > inflectionSamples;
	inflectionSampler( & inflectionSamples, controls );
	inflectionSamples.push_back( 1 );

	Concrete::Coords2D p0 = controls.p0_;
	double t1 = 0;
	double a1;
	Concrete::UnitFloatPair d1( 1, 0, bool( ) );
	{
		Concrete::Length shortLength = 1.e-4 * ( controls.p3_ - controls.p0_ ).norm( );
		Concrete::Coords2D tmp = coeffs.velocity( t1 );
		if( tmp.norm( ) < shortLength )
			{
				tmp = coeffs.acceleration( t1 );
			}
		d1 = tmp.direction( );
		a1 = atan2( d1.y_, d1.x_ );
	}

	double a2;
	double t2;
	Concrete::UnitFloatPair d2( 1, 0, bool( ) );
	typedef typeof inflectionSamples ListType;
	for( ListType::const_iterator inflection_i = inflectionSamples.begin( );
			 inflection_i != inflectionSamples.end( );
			 ++inflection_i, a1 = a2, t1 = t2, d1 = d2 )
		{
			t2 = *inflection_i;
			Bezier::ControlPoints< Concrete::Coords2D > subControls( coeffs.subSection( t1, t2 ) );
			Concrete::Length shortLength = 1.e-4 * ( subControls.p3_ - subControls.p0_ ).norm( );
			{
				Concrete::Coords2D tmp = coeffs.velocity( t2 );
				if( tmp.norm( ) < shortLength )
					{
						tmp = (-1) * coeffs.acceleration( t2 ); // Note the sign!
					}
				d2 = tmp.direction( );
				a2 = atan2( d2.y_, d2.x_ );
			}
			/* Check if the turn is counter clockwise or not.
			 * Note that it is tempting to use the acceleration at t1, but this is a bad idea since it is parallel with the
			 * velocity at the points of inflection (and t1 will often be such a point).
			 */
			bool ccw;
			{
				Concrete::Length tmp = Concrete::inner( d1.quarterTurnCounterClockwise( ), subControls.p2_ - subControls.p0_ );
				if( tmp.abs( ) > shortLength )
					{
						ccw = tmp > Concrete::ZERO_LENGTH;
					}
				else
					{
						// If p2_ was in line with d1, then we use p3_ instead.	As this is our last resort, we use it unconditionally.
						ccw = Concrete::inner( d1.quarterTurnCounterClockwise( ), subControls.p3_ - subControls.p0_ ) > Concrete::ZERO_LENGTH;
					}
			}
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
			//			std::cerr << "a1: " << a1*(180/M_PI) << "	 a2: " << a2*(180/M_PI) << std::endl ;
			for( double i = 1; i < steps; ++i, a += aStep )
				{
					//					std::cerr << "	" << a*(180/M_PI) ;
					/* Locate the angle a by finding a point where the velocity is orthogonal to a vector orthogonal to a.
					 */
					Concrete::UnitFloatPair da( cos( a ), sin( a ), bool( ) );
					Concrete::Coords2D an( cos( a + M_PI_2 ), sin( a + M_PI_2 ) );

					double optTimes[3];
					coeffs.stationaryPoints( optTimes, an ); // A HUGE_VAL is used as terminator in the result.
					double best_t = t2;
					for( double * src = & optTimes[0]; *src != HUGE_VAL; ++src )
						{
							if( *src <= lastTime ||
									*src >= best_t )
								{
									continue;
								}
							Concrete::Coords2D v = coeffs.velocity( *src );
							if( Concrete::inner( da, v ) > Concrete::ZERO_LENGTH )
								{
									best_t = *src;
								}
						}
					if( best_t < t2 )
						{
							dst->push_back( best_t );
							lastTime = best_t;
						}
					//					std::cerr << "	(" << ( best_t < t2 ) << ")	" << best_t << std::endl ;
				}
			if( t2 < 1 )
				{
					dst->push_back( t2 );
					//					std::cerr << "	final t2: " << t2 << std::endl ;
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
					dst->push_back( Shapes::straightLineArcTime( nextSample ).offtype< 0, 1 >( ) );
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

			Concrete::Time dt = Shapes::computeDt( segLengthBound );
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
					dst->push_back( Shapes::straightLineArcTime( nextSample ).offtype< 0, 1 >( ) );
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

			Concrete::Time dt = Shapes::computeDt( segLengthBound );
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

void
Computation::UpsampleDifferentiably2D::operator () ( std::vector< double > * dst, const Bezier::ControlPoints< Concrete::Coords2D > & controls ) const
{
	/* I might have shown this some time before, but as I write this I have forgotten if there was a simple argument.	Today, I just
	 * note that the differentiable sample point is always at 0.5.	I leave the code to reproduce in case someone is curious about this.
	 */

	dst->push_back( 0.5 );

//	 if( controls.p1_ == controls.p0_ &&
//			 controls.p2_ == controls.p3_ )
//		 {
//			 // By symmetry, we sample the mid point.
//			 dst->push_back( 0.5 );
//			 return;
//		 }

//	 Bezier::PolyCoeffs< Concrete::Coords2D > coeffs( controls );

//	 Concrete::Speed maxSpeed = std::max( std::max( ( controls.p1_ - controls.p0_ ).norm( ),
//																									( controls.p2_ - controls.p1_ ).norm( ) ),
//																				( controls.p3_ - controls.p2_ ).norm( ) ).offtype< 0, 1 >( );
//	 double t_tol = ( Computation::the_arcdelta / maxSpeed ).offtype< 0, 1 >( );

//	 double tLow = 0;
//	 double tHigh = 1;

//	 while( tHigh - tLow > t_tol )
//		 {
//			 double tMid = 0.5 * ( tLow + tHigh );
//			 Concrete::Length speedLow ( coeffs.subSection( 0, tMid ).velocity( 1 ).norm( ) );
//			 Concrete::Length speedHigh( coeffs.subSection( tMid, 1 ).velocity( 0 ).norm( ) );
//			 if( speedLow < speedHigh )
//				 {
//					 tLow = tMid;
//				 }
//			 else
//				 {
//					 tHigh = tMid;
//				 }
//		 }
//	 std::cerr << 0.5 * ( tLow + tHigh ) << std::endl ;
//	 dst->push_back( 0.5 * ( tLow + tHigh ) );
}

void
Computation::UpsampleDifferentiably3D::operator () ( std::vector< double > * dst, const Bezier::ControlPoints< Concrete::Coords3D > & controls ) const
{
	/* See comments in UpsampleDifferentiably2D.
	 */
	dst->push_back( 0.5 );
}
