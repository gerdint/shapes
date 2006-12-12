#ifndef bezier_h
#define bezier_h

#include <cmath>
#include <complex>
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
      void hyperplaneIntersection( double * t, const T & n, const double d ) const;  // finds the first t such that inner( point( t ), n ) == d
    };

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
    PolyCoeffs< T >::hyperplaneIntersection( double t[3], const T & n, const double d ) const
    {
      double k3 = innerScalar( n, z3_ );
      double k2 = innerScalar( n, z2_ );
      double k1 = innerScalar( n, z1_ );
      double k0 = innerScalar( n, z0_ ) - d;
      
      if( k3 == 0 )
	{
	  if( k2 == 0 )
	    {
	      if( k1 == 0 )
		{
		  // If we reach here, the equation is identically solved for all t.
		  if( k0 == 0 )
		    {
		      *t = 0;
		    }
		  return;
		}
	      else
		{
		  // Here k1 != 0
		  double tmp = - k0 / k1;
		  if( 0 <= tmp && tmp <= 1 )
		    {
		      *t = tmp;
		    }
		}
	    }
	  else
	    {
	      // Here k2 != 0
	      k1 /= k2;
	      k0 /= k2;
	      double tmp_c = - 0.5 * k1;
	      double r2 = tmp_c * tmp_c - k0;
	      if( r2 >= 0 )
		{
		  double r = sqrt( r2 );
		  {
		    double tmp = tmp_c - r;
		    if( 0 <= tmp && tmp <= 1 )
		      {
			*t = tmp;
			return;
		      }
		  }
		  {
		    double tmp = tmp_c + r;
		    if( 0 <= tmp && tmp <= 1 )
		      {
			*t = tmp;
			return;
		      }
		  }
		}
	    }
	  return;
	}
      
      k2 /= k3;
      k1 /= k3;
      k0 /= k3;

      /* The solution that now follows is an implementation of the first cubic formula
       * presented on MathWorld.
       */

      typedef std::complex< double > Complex;

      Complex Q( ( 3 * k1 - k2 * k2 ) / 9, 0 );
      Complex R( 0.5 * ( ( 9 * k1 * k2 - 2 * k2 * k2 * k2 ) / 27 - k0 ), 0 );
      Complex r = sqrt( R * R + Q * Q * Q );

      Complex wCube;
      wCube = R + r;  // ( R - r ) is also an option
      Complex w1 = pow( wCube, 1./3 );
      Complex w2 = w1 * exp( Complex( 0, 2 * M_PI / 3 ) );
      Complex w3 = w1 * exp( Complex( 0, -2 * M_PI / 3 ) );
      
      Complex t1 = w1 - Q / w1 - k2 / 3;
      Complex t2 = w2 - Q / w2 - k2 / 3;
      Complex t3 = w3 - Q / w3 - k2 / 3;

      t[0] = HUGE_VAL;
      t[1] = HUGE_VAL;
      t[2] = HUGE_VAL;
      
      double * dst = & t[0];

      const double IMAG_TOL = 1e-10; // This is "relative" to the interesting t-range being 0..1
      if( fabs( t1.imag( ) ) < IMAG_TOL )
	{
	  *dst = t1.real( );
	  ++dst;
	}
      if( fabs( t2.imag( ) ) < IMAG_TOL )
	{
	  *dst = t2.real( );
	  ++dst;
	}
      if( fabs( t3.imag( ) ) < IMAG_TOL )
	{
	  *dst = t3.real( );
	  ++dst;
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
