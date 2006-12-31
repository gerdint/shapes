#ifndef elementarycoords_h
#define elementarycoords_h

#include <cmath>

#include "MetaPDF_Concrete_decls.h"
#include "MetaPDF_Lang_decls.h"

#include "elementarylength.h"
#include "metapdfexceptions.h"


namespace MetaPDF
{

  namespace Concrete
  {

    class UnitFloatPair
    {
    public:
      double x_;
      double y_;
      UnitFloatPair( double x, double y )
      {
	if( x == 0 && y == 0 )
	  {
	    throw Exceptions::InternalError( "A UnitFloatPair was initialized with a zero vector." );
	  }
	double tmp = 1. / hypot( x, y );
	x_ = tmp * x;
	y_ = tmp * y;
      }
      UnitFloatPair( bool ) : x_( 1 ), y_( 0 ) { }
      UnitFloatPair( bool, bool ) : x_( 0 ), y_( 1 ) { }
      explicit UnitFloatPair( double x, double y, double precomputedNorm )
      {
	double tmp = 1. / precomputedNorm;
	x_ = tmp * x;
	y_ = tmp * y;
      }
      explicit UnitFloatPair( double x, double y, bool )
      {
	x_ = x;
	y_ = y;
      }
      double normSquaredThatOughtToBeOne( ) const
      {
	return x_ * x_ + y_ * y_;
      }
      Concrete::UnitFloatPair unnormalizedScaling( double a ) const;
      Concrete::UnitFloatPair reverse( ) const
      {
	return Concrete::UnitFloatPair( - x_, - y_ );
      }
    };

    class Coords2D
    {
    public:
      Length x_;
      Length y_;
      Coords2D( Concrete::Length x, Concrete::Length y ) : x_( x ), y_( y ) { }
      Coords2D( double x, double y ) : x_( x ), y_( y ) { }
      Coords2D( const Concrete::Coords2D & orig ) : x_( orig.x_ ), y_( orig.y_ ) { }
      Coords2D( const Lang::Coords2D & orig );
      Coords2D( const Concrete::Coords2D & base, const Lang::Coords2D & orig );
      operator Lang::Coords2D ( ) const;
      Coords2D * transformedPtr( const Lang::Transform2D & tf ) const;
      Coords2D transformed( const Lang::Transform2D & tf ) const;
      Concrete::Coords3D * transformedPtr( const Lang::Transform3D & tf ) const; // treat as z = 0
      friend std::ostream & operator << ( std::ostream & os, const Coords2D & self );
      Concrete::Length norm( ) const;
      double normScalar( ) const;
      Concrete::UnitFloatPair direction( ) const;
      Concrete::UnitFloatPair direction( Concrete::Length precomputedNorm ) const;
      Concrete::UnitFloatPair normalizedOrthogonal( const Coords2D & b ) const
      {
	// We return the counter-clockwise orthogonal to the normalized vector from ourselves to b
	return UnitFloatPair( y_.offtype< 1, 0 >( ) - b.y_.offtype< 1, 0 >( ),
			      b.x_.offtype< 1, 0 >( ) - x_.offtype< 1, 0 >( ) );
      }
      Concrete::UnitFloatPair unNormalizedOrthogonal( const Coords2D & b ) const
      {
	// Warning!  This is the same as normalizedOrthogonal, but the result is really not normalized!
	return UnitFloatPair( y_.offtype< 1, 0 >( ) - b.y_.offtype< 1, 0 >( ),
			      b.x_.offtype< 1, 0 >( ) - x_.offtype< 1, 0 >( ),
			      bool( ) );
      }
      Concrete::Length distanceTo( const Concrete::Coords2D & other ) const
      {
	return hypotPhysical( x_ - other.x_, y_ - other.y_ );
      }
    };

    /* The Bezier template classes requires the interface to a vector space with inner product.
     */
    Concrete::Coords2D operator * ( double scalar, const Concrete::Coords2D & coords );
    Concrete::Coords2D operator * ( const Concrete::Coords2D & coords, double scalar );
    Concrete::Coords2D operator + ( const Concrete::Coords2D & coords1, const Concrete::Coords2D & coords2 );
    Concrete::Coords2D operator - ( const Concrete::Coords2D & coords1, const Concrete::Coords2D & coords2 );

    Concrete::Coords2D operator * ( Concrete::Length length, const Concrete::UnitFloatPair & direction );
    Concrete::Coords2D operator * ( const Concrete::UnitFloatPair & direction, Concrete::Length length );

    //  Concrete::UnitFloatPair operator + ( const Concrete::UnitFloatPair & coords1, const Concrete::UnitFloatPair & coords2 );
    //  Concrete::UnitFloatPair operator - ( const Concrete::UnitFloatPair & coords1, const Concrete::UnitFloatPair & coords2 );

    inline Concrete::Coords2D operator / ( const Concrete::Coords2D & coords, double denom )
    {
      double tmp = 1. / denom;
      return Concrete::Coords2D( tmp * coords.x_, tmp * coords.y_ );
    }
    //   inline Concrete::UnitFloatPair operator / ( const Concrete::Coords2D & coords, const Concrete::Length & denom )
    //   {
    //     double tmp = 1. / denom.x_;
    //     return Concrete::UnitFloatPair( tmp * coords.x_.x_, tmp * coords.y_.x_ );
    //   }
    //   inline Concrete::UnitFloatPair operator / ( const Concrete::UnitFloatPair & coords, double denom )
    //   {
    //     double tmp = 1. / denom;
    //     return Concrete::UnitFloatPair( tmp * coords.x_, tmp * coords.y_ );
    //   }

    inline bool operator == ( const Concrete::Coords2D & coords1, const Concrete::Coords2D & coords2 )
    {
      return coords1.x_ == coords2.x_ && coords1.y_ == coords2.y_;
    }
    inline bool operator != ( const Concrete::Coords2D & coords1, const Concrete::Coords2D & coords2 )
    {
      return ! ( coords1 == coords2 );
    }

    inline double inner( const UnitFloatPair & a, const UnitFloatPair & b )
    {
      return a.x_ * b.x_ + a.y_ * b.y_;
    }

    inline Length inner( const UnitFloatPair & a, const Coords2D & b )
    {
      return a.x_ * b.x_ + a.y_ * b.y_;
    }

    inline Length inner( const Coords2D & a, const UnitFloatPair & b )
    {
      return a.x_ * b.x_ + a.y_ * b.y_;
    }

    inline double innerScalar( const UnitFloatPair & a, const Coords2D & b )
    {
      return ( a.x_ * b.x_ + a.y_ * b.y_ ).offtype< 1, 0 >( );
    }

    inline double innerScalar( const Coords2D & a, const UnitFloatPair & b )
    {
      return ( a.x_ * b.x_ + a.y_ * b.y_ ).offtype< 1, 0 >( );
    }

    inline double innerScalar( const Coords2D & a, const Coords2D & b )
    {
      return ( a.x_ * b.x_ + a.y_ * b.y_ ).offtype< 2, 0 >( );
    }


    class UnitFloatTriple
    {
    public:
      double x_;
      double y_;
      double z_;
      UnitFloatTriple( double x, double y, double z );
      UnitFloatTriple( bool ) : x_( 1 ), y_( 0 ), z_( 0 ) { }
      UnitFloatTriple( bool, bool ) : x_( 0 ), y_( 1 ), z_( 0 ) { }
      UnitFloatTriple( bool, bool, bool ) : x_( 0 ), y_( 0 ), z_( 1 ) { }
      explicit UnitFloatTriple( double x, double y, double z, double precomputedNorm )
      {
	double tmp = 1. / precomputedNorm;
	x_ = tmp * x;
	y_ = tmp * y;
	z_ = tmp * z;
      }
      explicit UnitFloatTriple( double x, double y, double z, bool )
      {
	x_ = x;
	y_ = y;
	z_ = z;
      }
      double normSquaredThatOughtToBeOne( ) const
      {
	return x_ * x_ + y_ * y_ + z_ * z_;
      }
      Concrete::UnitFloatTriple reflect( const Concrete::UnitFloatTriple dir ) const;
      Concrete::UnitFloatTriple unnormalizedScaling( double a ) const;
      Concrete::UnitFloatTriple reverse( ) const
      {
	return Concrete::UnitFloatTriple( - x_, - y_, - z_ );
      }
      Concrete::UnitFloatTriple rotate( const Concrete::UnitFloatTriple dir, const double angle ) const;
    };

    class Coords3D
    {
    public:
      Length x_;
      Length y_;
      Length z_;
      Coords3D( Concrete::Length x, Concrete::Length y, Concrete::Length z ) : x_( x ), y_( y ), z_( z ) { }
      Coords3D( double x, double y, double z ) : x_( x ), y_( y ), z_( z ) { }
      Coords3D( const Concrete::Coords3D & orig ) : x_( orig.x_ ), y_( orig.y_ ), z_( orig.z_ ) { }
      Coords3D( const Concrete::Coords2D & orig ) : x_( orig.x_ ), y_( orig.y_ ), z_( 0 ) { }
      Coords3D( const Lang::Coords3D & orig );
      Coords3D( const Concrete::Coords3D & base, const Lang::Coords3D & orig );
      operator Lang::Coords3D ( ) const;
      Coords3D * transformedPtr( const Lang::Transform3D & tf ) const;
      Coords3D transformed( const Lang::Transform3D & tf ) const;
      Coords2D * make2D( Concrete::Length eyez ) const;
      Coords2D make2DAutomatic( Concrete::Length eyez ) const;
      friend std::ostream & operator << ( std::ostream & os, const Coords3D & self );
      void negate( ) { x_ = -x_; y_ = -y_; z_ = -z_; }
      double normalizedInner( const Coords3D & b ) const;
      Concrete::Length norm( ) const;
      double normScalar( ) const;
      UnitFloatTriple direction( ) const;
      Concrete::UnitFloatTriple direction( Concrete::Length precomputedNorm ) const;
    };
  
    /* The Bezier template classes requires the interface to a vector space.
     */
    Concrete::Coords3D operator * ( double scalar, const Concrete::Coords3D & coords );
    Concrete::Coords3D operator * ( const Concrete::Coords3D & coords, double scalar );
    Concrete::Coords3D operator + ( const Concrete::Coords3D & coords1, const Concrete::Coords3D & coords2 );
    Concrete::Coords3D operator - ( const Concrete::Coords3D & coords1, const Concrete::Coords3D & coords2 );

    Concrete::Coords3D operator * ( Concrete::Length length, const Concrete::UnitFloatTriple & direction );
    Concrete::Coords3D operator * ( const Concrete::UnitFloatTriple & direction, Concrete::Length length );

    //  Concrete::UnitFloatTriple operator + ( const Concrete::UnitFloatTriple & coords1, const Concrete::UnitFloatTriple & coords2 );
    //  Concrete::UnitFloatTriple operator - ( const Concrete::UnitFloatTriple & coords1, const Concrete::UnitFloatTriple & coords2 );

    inline Concrete::Coords3D operator / ( const Concrete::Coords3D & coords, double denom )
    {
      double tmp = 1. / denom;
      return Concrete::Coords3D( tmp * coords.x_, tmp * coords.y_, tmp * coords.z_ );
    }
    //   inline Concrete::UnitFloatTriple operator / ( const Concrete::Coords3D & coords, const Concrete::Length & denom )
    //   {
    //     double tmp = 1. / denom.x_;
    //     return Concrete::UnitFloatTriple( tmp * coords.x_.x_, tmp * coords.y_.x_, tmp * coords.z_.x_ );
    //   }
    //   inline Concrete::UnitFloatTriple operator / ( const Concrete::UnitFloatTriple & coords, double denom )
    //   {
    //     double tmp = 1. / denom;
    //     return Concrete::UnitFloatTriple( tmp * coords.x_, tmp * coords.y_, tmp * coords.z_ );
    //   }


    inline double inner( const UnitFloatTriple & a, const UnitFloatTriple & b )
    {
      return a.x_ * b.x_ + a.y_ * b.y_ + a.z_ * b.z_;
    }

    inline Length inner( const UnitFloatTriple & a, const Coords3D & b )
    {
      return a.x_ * b.x_ + a.y_ * b.y_ + a.z_ * b.z_;
    }

    inline Length inner( const Coords3D & a, const UnitFloatTriple & b )
    {
      return a.x_ * b.x_ + a.y_ * b.y_ + a.z_ * b.z_;
    }

    inline Physical< 2, 0 > inner( const Coords3D & a, const Coords3D & b )
    {
      return a.x_ * b.x_ + a.y_ * b.y_ + a.z_ * b.z_;
    }

    inline double innerScalar( const UnitFloatTriple & a, const Coords3D & b )
    {
      return ( a.x_ * b.x_ + a.y_ * b.y_ + a.z_ * b.z_ ).offtype< 1, 0 >( );
    }

    inline double innerScalar( const Coords3D & a, const UnitFloatTriple & b )
    {
      return ( a.x_ * b.x_ + a.y_ * b.y_ + a.z_ * b.z_ ).offtype< 1, 0 >( );
    }

    inline double innerScalar( const Coords3D & a, const Coords3D & b )
    {
      return ( a.x_ * b.x_ + a.y_ * b.y_ + a.z_ * b.z_ ).offtype< 2, 0 >( );
    }

    /*   inline Coords3D cross( const Coords3D & a, const Coords3D & b ) */
    /*   { */
    /*     return Coords3D( a.y_ * b.z_ - a.z_ * b.y_, */
    /* 			       a.z_ * b.x_ - a.x_ * b.z_, */
    /* 			       a.x_ * b.y_ - a.y_ * b.x_ ); */
    /*   } */

    inline UnitFloatTriple crossDirection( const UnitFloatTriple & a, const UnitFloatTriple & b )
    {
      try
	{
	  return UnitFloatTriple( a.y_ * b.z_ - a.z_ * b.y_,
				  a.z_ * b.x_ - a.x_ * b.z_,
				  a.x_ * b.y_ - a.y_ * b.x_ );
	}
      catch( const Exceptions::InternalError & ball )
	{
	  throw NonLocalExit::CrossDirectionOfParallel( );
	}
    }

    inline Coords3D cross( const UnitFloatTriple & a, const Coords3D & b )
    {
      return Coords3D( a.y_ * b.z_ - a.z_ * b.y_,
		       a.z_ * b.x_ - a.x_ * b.z_,
		       a.x_ * b.y_ - a.y_ * b.x_ );
    }

    inline Coords3D cross( const Coords3D & a, const UnitFloatTriple & b )
    {
      return Coords3D( a.y_ * b.z_ - a.z_ * b.y_,
		       a.z_ * b.x_ - a.x_ * b.z_,
		       a.x_ * b.y_ - a.y_ * b.x_ );
    }

    inline UnitFloatTriple crossDirection( const Coords3D & a, const Coords3D & b )
    {
      try
	{
	  return UnitFloatTriple( ( a.y_ * b.z_ - a.z_ * b.y_ ).offtype< 2, 0 >( ),
				  ( a.z_ * b.x_ - a.x_ * b.z_ ).offtype< 2, 0 >( ),
				  ( a.x_ * b.y_ - a.y_ * b.x_ ).offtype< 2, 0 >( ) );
	}
      catch( const Exceptions::InternalError & ball )
	{
	  throw NonLocalExit::CrossDirectionOfParallel( );
	}
    }

    //   inline Coords3D crossCoords3D( const Coords3D & a, const Coords3D & b )
    //   {
    //     return Coords3D( a.y_.x_ * b.z_.x_ - a.z_.x_ * b.y_.x_,
    // 			       a.z_.x_ * b.x_.x_ - a.x_.x_ * b.z_.x_,
    // 			       a.x_.x_ * b.y_.x_ - a.y_.x_ * b.x_.x_ );
    //   }

    Area crossMagnitude( const Coords3D & a, const Coords3D & b );

  }
}

#endif
