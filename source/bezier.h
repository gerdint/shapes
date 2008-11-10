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

#ifndef bezier_h
#define bezier_h

#include <cmath>
#include <iostream>

namespace Bezier
{
	template< class T >
		class PolyCoeffs;

	template< class T >
		class ControlPoints
		{
		public:
			T p0_;
			T p1_;
			T p2_;
			T p3_;

			ControlPoints( const T & p0, const T & p1, const T & p2, const T & p3 );
			ControlPoints( const PolyCoeffs< T > & polyCoeffs );

			T point( double t ) const;
		};

	template< class T >
		class PolyCoeffs
		{
		public:
			T z0_;
			T z1_;
			T z2_;
			T z3_;

			PolyCoeffs( const T & z0, const T & z1, const T & z2, const T & z3 );
			PolyCoeffs( const ControlPoints< T > & controlPoints );
			PolyCoeffs< T > subSection( double t0, double t1 ) const;

			T point( double t ) const;
			T velocity( double t ) const;
			T acceleration( double t ) const;
			// The number of values returned in the destination argument by the following functions may vary.	A HUGE_VAL is used for termination.
			void hyperplaneIntersections( double t[4], const T & n, const double d ) const;	// Finds all t such that inner( point( t ), n ) == d.	Note that there can be at most 3 intersections.
			void stationaryPoints( double t[3], const T & d ) const;	// Finds all t such that inner( velocity( t ), d ) == d.	Note that there can be at most 2 stationary points.
			void inflections( double t[3] ) const;	// Finds all t such that acceleration is parallel to velocity.	Only defined for Coords2D, which makes sense since there are generally no such points in higher dimensions.	Note that there can be at most 2 points of inflection (this is not obvious at first glance)!
		};

	/* By "Bezier roots" I mean real roots in the interval [ 0, 1 ].
	 */
	void bezierRootsOfPolynomial( double t[2], double k0, double k1 );
	void bezierRootsOfPolynomial( double t[3], double k0, double k1, double k2 );
	void bezierRootsOfPolynomial( double t[4], double k0, double k1, double k2, double k3 );

	template< class T >
		ControlPoints< T >::ControlPoints( const T & p0, const T & p1, const T & p2, const T & p3 )
		: p0_( p0 ), p1_( p1 ), p2_( p2 ), p3_( p3 )
		{ }

	template< class T >
		ControlPoints< T >::ControlPoints( const PolyCoeffs< T > & pc )
		: p0_( pc.z0_ ),
			p1_( pc.z0_ + (1.0/3) * pc.z1_ ),
			p2_( pc.z0_ + (1.0/3) * ( 2 * pc.z1_ + pc.z2_ ) ),
			p3_( pc.z0_ + pc.z1_ + pc.z2_ + pc.z3_ )
		{ }

	template< class T >
		T
		ControlPoints< T >::point( double t ) const
		{
			double ct = 1 - t;
			double ct2 = ct * ct;
			double t2 = t * t;
			return ct2 * ct * p0_ + 3 * ct2 * t * p1_ + 3 * ct * t2 * p2_ + t2 * t * p3_;
		}


	template< class T >
		std::ostream &
		operator << ( std::ostream & os, const ControlPoints< T > & self )
		{
			os << "t -> (1-t)^{3} * " << self.p0_ << " + 3*(1-t)^{2}*t * " << self.p1_ << " + 3*(1-t)*t^{2} * " << self.p2_ << " + t^{3} * " << self.p3_ ;
			return os;
		}


	template< class T >
		PolyCoeffs< T >::PolyCoeffs( const T & z0, const T & z1, const T & z2, const T & z3 )
		: z0_( z0 ), z1_( z1 ), z2_( z2 ), z3_( z3 )
		{ }

	template< class T >
		PolyCoeffs< T >::PolyCoeffs( const ControlPoints< T > & cp )
		: z0_( cp.p0_ ),
			z1_( 3 * (cp.p1_ - cp.p0_) ),
			z2_( 3 * ( cp.p0_ - 2 * cp.p1_ + cp.p2_) ),
			z3_( 3 * cp.p1_ - 3 * cp.p2_ + cp.p3_ - cp.p0_ ) // put cp_.0 last since we don't rely un unary negation
			{ }

	template< class T >
		T
		PolyCoeffs< T >::point( double t ) const
		{
			return ( ( z3_ * t + z2_ ) * t + z1_ ) * t + z0_;
		}

	template< class T >
		T
		PolyCoeffs< T >::velocity( double t ) const
		{
			return ( z3_ * ( ( 3./2.) * t ) + z2_ ) * ( 2 * t ) + z1_;
		}

	template< class T >
		T
		PolyCoeffs< T >::acceleration( double t ) const
		{
			return z3_ * ( 6 * t ) + z2_ * 2;
		}

	template< class T >
		PolyCoeffs< T >
		PolyCoeffs< T >::subSection( double t0, double t1 ) const
		{
			double dt = t1 - t0;
			double dt2 = dt * dt;
			return PolyCoeffs<T>( ( ( z3_ * t0 + z2_ ) * t0 + z1_ ) * t0 + z0_,
														dt * ( ( 3 * t0 * z3_ + 2 * z2_ ) * t0 + z1_ ),
														dt2 * ( z2_ + 3 * t0 * z3_ ),
														dt2 * dt * z3_ );
		}

	template< class T >
		void
		PolyCoeffs< T >::hyperplaneIntersections( double t[4], const T & n, const double d ) const
		{
			double k3 = innerScalar( n, z3_ );
			double k2 = innerScalar( n, z2_ );
			double k1 = innerScalar( n, z1_ );
			double k0 = innerScalar( n, z0_ ) - d;

			bezierRootsOfPolynomial( t, k0, k1, k2, k3 );
		}

	template< class T >
		void
		PolyCoeffs< T >::stationaryPoints( double t[3], const T & d ) const
		{
			double k0 = innerScalar( z1_, d );
			double k1 = 2 * innerScalar( z2_, d );
			double k2 = 3 * innerScalar( z3_, d );

			bezierRootsOfPolynomial( t, k0, k1, k2 );
		}

	template< class T >
	void
	PolyCoeffs< T >::inflections( double t[3] ) const
	{
		/* We begin by constructing the polynomial coefficients of the scalar product betwenn the velocity and a normal to the acceleration.
		 */

		/* Note that we cannot just take any orthogonal vector for z2_ and z3_ separately; they must be constructed using the same transform.
		 * For generality, we avoid mentioning the components, although the method quarterTurnCounterClockwise is generally defined for classes
		 * with components named x_ and y_.
		 */
		T z2n = z2_.quarterTurnCounterClockwise( );
		T z3n = z3_.quarterTurnCounterClockwise( );

		//		double k3 = 6 * innerScalar( z3_, z3n ); // Note that this vanishes!
		double k2 = -6 * innerScalar( z3_, z2n ); // (After some simplification.)
		double k1 = 6 * innerScalar( z1_, z3n ); // (After some simplification.)
		double k0 = 2 * innerScalar( z1_, z2n );

		/* If the acceleration ever vanishes, or is close to vanish, then z2_ is parallel to z3_, and k2 becomes very small.
		 * When this happens, it is a very small difference between having an inflection and not having one, and in this case we prefer
		 * to find the inflection.
		 */
		if( k2*k2 < 0.00001 * 36 * ( innerScalar( z3_, z3_ ) * innerScalar( z2_, z2_ ) ) )
			{
				bezierRootsOfPolynomial( t, k0, k1 );
				/* In this case we also add the point where the acceleration vanishes to the points of inflection.
				 * We do this by projecting the acceleration on z3_ to get a scalar equation.
				 */
				double * tmp = & t[0];
				for( ; *tmp != HUGE_VAL; ++tmp )
					;
				bezierRootsOfPolynomial( tmp, innerScalar( z3_, z2_ ), 3 * innerScalar( z3_, z3_ ) ); // A factor of 2 was taken out.
			}
		else
			{
				bezierRootsOfPolynomial( t, k0, k1, k2 );
			}
	}

	template< class T >
		std::ostream &
		operator << ( std::ostream & os, const PolyCoeffs< T > & self )
		{
			os << "t -> " << self.z0 << " + t * " << self.z1 << " + t^{2} * " << self.z2 << " + t^{3} * " << self.z3 ;
			return os;
		}


}


#endif
