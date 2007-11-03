#include "elementarytypes.h"
#include "functiontypes.h"     // this is a trick to avoid #include problems...
#include "elementarycoords.h"


using namespace Shapes;

Concrete::UnitFloatPair
Concrete::UnitFloatPair::unnormalizedScaling( double a ) const
{
  return Concrete::UnitFloatPair( a * x_, a * y_, true );
}

Concrete::Coords2D::Coords2D( const Lang::Coords2D & orig )
  : x_( orig.x_.get( ) ), y_( orig.y_.get( ) )
{ }

Concrete::Coords2D::Coords2D( const Concrete::Coords2D & base, const Lang::Coords2D & orig )
  : x_( orig.x_.get( base.x_ ) ), y_( orig.y_.get( base.y_ ) )
{ }

Concrete::Coords2D::operator Lang::Coords2D ( ) const
{
  return Lang::Coords2D( Lang::Length( x_ ), Lang::Length( y_ ) );
}

Concrete::Coords2D *
Concrete::Coords2D::transformedPtr( const Lang::Transform2D & tf ) const
{
  return new Concrete::Coords2D( tf.xx_ * x_ + tf.xy_ * y_ + tf.xt_, tf.yx_ * x_ + tf.yy_ * y_ + tf.yt_ );
}

Concrete::Coords2D
Concrete::Coords2D::transformed( const Lang::Transform2D & tf ) const
{
  return Concrete::Coords2D( tf.xx_ * x_ + tf.xy_ * y_ + tf.xt_, tf.yx_ * x_ + tf.yy_ * y_ + tf.yt_ );
}

Concrete::Coords3D *
Concrete::Coords2D::transformedPtr( const Lang::Transform3D & tf ) const
{
  return new Concrete::Coords3D( tf.xx_ * x_ + tf.xy_ * y_ + tf.xt_,
					  tf.yx_ * x_ + tf.yy_ * y_ + tf.yt_,
					  tf.zx_ * x_ + tf.zy_ * y_ + tf.zt_ );
}

Concrete::Length
Concrete::Coords2D::norm( ) const
{
  return hypotPhysical( x_, y_ );
}

double
Concrete::Coords2D::normScalar( ) const
{
  return hypotPhysical( x_, y_ ).offtype< 1, 0 >( );
}

Concrete::UnitFloatPair
Concrete::Coords2D::direction( ) const
{
  return Concrete::UnitFloatPair( x_.offtype< 1, 0 >( ), y_.offtype< 1, 0 >( ) );
}

Concrete::UnitFloatPair
Concrete::Coords2D::direction( Concrete::Length precomputedLength ) const
{
  return Concrete::UnitFloatPair( x_.offtype< 1, 0 >( ), y_.offtype< 1, 0 >( ), precomputedLength.offtype< 1, 0 >( ) );
}


std::ostream &
Concrete::operator << ( std::ostream & os, const Concrete::Coords2D & self )
{
  static char buf[25];
  sprintf( buf, "%.2f %.2f", double( self.x_.offtype< 1, 0 >( ) ), double( self.y_.offtype< 1, 0 >( ) ) );
  os << buf ;
  return os;
}

Concrete::Coords2D
Concrete::operator * ( double scalar, const Concrete::Coords2D & coords )
{
  return Concrete::Coords2D( scalar * coords.x_, scalar * coords.y_ );
}

Concrete::Coords2D
Concrete::operator * ( const Concrete::Coords2D & coords, double scalar )
{
  return Concrete::Coords2D( scalar * coords.x_, scalar * coords.y_ );
}

Concrete::Coords2D
Concrete::operator + ( const Concrete::Coords2D & coords1, const Concrete::Coords2D & coords2 )
{
  return Concrete::Coords2D( coords1.x_ + coords2.x_, coords1.y_ + coords2.y_ );
}

Concrete::Coords2D
Concrete::operator - ( const Concrete::Coords2D & coords1, const Concrete::Coords2D & coords2 )
{
  return Concrete::Coords2D( coords1.x_ - coords2.x_, coords1.y_ - coords2.y_ );
}



Concrete::Coords2D
Concrete::operator * ( Concrete::Length length, const Concrete::UnitFloatPair & direction )
{
  return Concrete::Coords2D( length * direction.x_, length * direction.y_ );
}

Concrete::Coords2D
Concrete::operator * ( const Concrete::UnitFloatPair & direction, Concrete::Length length )
{
  return Concrete::Coords2D( length * direction.x_, length * direction.y_ );
}

// Concrete::UnitFloatPair
// Concrete::operator + ( const Concrete::UnitFloatPair & coords1, const Concrete::UnitFloatPair & coords2 )
// {
//   return Concrete::UnitFloatPair( coords1.x_ + coords2.x_, coords1.y_ + coords2.y_ );
// }

// Concrete::UnitFloatPair
// Concrete::operator - ( const Concrete::UnitFloatPair & coords1, const Concrete::UnitFloatPair & coords2 )
// {
//   return Concrete::UnitFloatPair( coords1.x_ - coords2.x_, coords1.y_ - coords2.y_ );
// }



Concrete::Coords3D *
Concrete::Coords3D::transformedPtr( const Lang::Transform3D & tf ) const
{
  return new Concrete::Coords3D( tf.xx_ * x_ + tf.xy_ * y_ + tf.xz_ * z_ + tf.xt_,
				 tf.yx_ * x_ + tf.yy_ * y_ + tf.yz_ * z_ + tf.yt_,
				 tf.zx_ * x_ + tf.zy_ * y_ + tf.zz_ * z_ + tf.zt_ );
}

Concrete::Coords3D
Concrete::Coords3D::transformed( const Lang::Transform3D & tf ) const
{
  return Concrete::Coords3D( tf.xx_ * x_ + tf.xy_ * y_ + tf.xz_ * z_ + tf.xt_,
			     tf.yx_ * x_ + tf.yy_ * y_ + tf.yz_ * z_ + tf.yt_,
			     tf.zx_ * x_ + tf.zy_ * y_ + tf.zz_ * z_ + tf.zt_ );
}

Concrete::Coords2D *
Concrete::Coords3D::make2D( Concrete::Length eyez ) const
{
  if( eyez == HUGE_LENGTH )
    {
      return new Concrete::Coords2D( x_, y_ );
    }

  return new Concrete::Coords2D( x_ * ( eyez / ( eyez - z_ ) ),
				 y_ * ( eyez / ( eyez - z_ ) ) );
}

Concrete::Coords2D
Concrete::Coords3D::make2DAutomatic( Concrete::Length eyez ) const
{
  if( eyez == HUGE_LENGTH )
    {
      return Concrete::Coords2D( x_, y_ );
    }

  return Concrete::Coords2D( x_ * eyez / ( eyez - z_ ),
			     y_ * eyez / ( eyez - z_ ) );
}

std::ostream &
Concrete::operator << ( std::ostream & os, const Concrete::Coords3D & self )
{
  static char buf[38];
  sprintf( buf, "%.2f %.2f %.2f", double( self.x_.offtype< 1, 0 >( ) ), double( self.y_.offtype< 1, 0 >( ) ), double( self.z_.offtype< 1, 0 >( ) ) );
  os << buf ;
  return os;
}

Concrete::Coords3D::Coords3D( const Lang::Coords3D & orig )
  : x_( orig.x_.getScalar( ) ), y_( orig.y_.getScalar( ) ), z_( orig.z_.getScalar( ) )
{ }

Concrete::Coords3D::Coords3D( const Concrete::Coords3D & base, const Lang::Coords3D & orig )
  : x_( orig.x_.getScalar( base.x_ ) ), y_( orig.y_.getScalar( base.y_ ) ), z_( orig.z_.getScalar( base.z_ ) )
{ }

Concrete::Coords3D::operator Lang::Coords3D ( ) const
{
  return Lang::Coords3D( Lang::Length( x_ ), Lang::Length( y_ ), Lang::Length( z_ ) );
}


Concrete::Coords3D
Concrete::operator * ( double scalar, const Concrete::Coords3D & coords )
{
  return Concrete::Coords3D( scalar * coords.x_, scalar * coords.y_, scalar * coords.z_ );
}

Concrete::Coords3D
Concrete::operator * ( const Concrete::Coords3D & coords, double scalar )
{
  return Concrete::Coords3D( scalar * coords.x_, scalar * coords.y_, scalar * coords.z_ );
}

Concrete::Coords3D
Concrete::operator + ( const Concrete::Coords3D & coords1, const Concrete::Coords3D & coords2 )
{
  return Concrete::Coords3D( coords1.x_ + coords2.x_, coords1.y_ + coords2.y_, coords1.z_ + coords2.z_ );
}

Concrete::Coords3D
Concrete::operator - ( const Concrete::Coords3D & coords1, const Concrete::Coords3D & coords2 )
{
  return Concrete::Coords3D( coords1.x_ - coords2.x_, coords1.y_ - coords2.y_, coords1.z_ - coords2.z_ );
}

double
Concrete::Coords3D::normalizedInner( const Concrete::Coords3D & b ) const
{
  return ( ( x_ * b.x_ + y_ * b.y_ + z_ * b.z_ ) / hypotPhysical( x_, y_, z_ ) ).offtype< 1, 0 >( );
}

Concrete::Length
Concrete::Coords3D::norm( ) const
{
  return hypotPhysical( x_, y_, z_ );
}

double
Concrete::Coords3D::normScalar( ) const
{
  return hypotPhysical( x_, y_, z_ ).offtype< 1, 0 >( );
}

Concrete::UnitFloatTriple
Concrete::Coords3D::direction( ) const
{
  return Concrete::UnitFloatTriple( x_.offtype< 1, 0 >( ), y_.offtype< 1, 0 >( ), z_.offtype< 1, 0 >( ) );
}

Concrete::UnitFloatTriple
Concrete::Coords3D::direction( Concrete::Length precomputedLength ) const
{
  return Concrete::UnitFloatTriple( x_.offtype< 1, 0 >( ), y_.offtype< 1, 0 >( ), z_.offtype< 1, 0 >( ), precomputedLength.offtype< 1, 0 >( ) );
}



Concrete::UnitFloatTriple::UnitFloatTriple( double x, double y, double z )
{
  double len = Concrete::Scalar::hypot3( x, y, z );
  if( len < 1e-5 )
    {
      throw Exceptions::InternalError( "A concrete UnitFloatTriple was initialized with a very short vector." );
    }
  double tmp = 1. / Concrete::Scalar::hypot3( x, y, z );
  x_ = tmp * x;
  y_ = tmp * y;
  z_ = tmp * z;
}

Concrete::UnitFloatTriple
Concrete::UnitFloatTriple::unnormalizedScaling( double a ) const
{
  return Concrete::UnitFloatTriple( a * x_, a * y_, a * z_, true );
}


Concrete::UnitFloatTriple
Concrete::UnitFloatTriple::reflect( const Concrete::UnitFloatTriple dir ) const
{
  double a = 2 * Concrete::inner( *this, dir );
  return Concrete::UnitFloatTriple( a * x_ - dir.x_,
				    a * y_ - dir.y_,
				    a * z_ - dir.z_,
				    true );  // true means it's already normalized
}

Concrete::UnitFloatTriple
Concrete::UnitFloatTriple::rotate( const Concrete::UnitFloatTriple dir, const double angle ) const
{
  // The following may not be efficient, but it was easy to code by copying code from Core_rotate3D.
  // I've got the feeling that this is much better done using geometric algebra based on Clifford algebra.

  double x = dir.x_;
  double y = dir.y_;
  double z = dir.z_;
  double x2 = x * x;
  double y2 = y * y;
  double z2 = z * z;
  double c = cos( angle );
  double s = sin( angle );
  return Concrete::UnitFloatTriple( (x2+(y2+z2)*c)*x_ + (x*y*(1-c)+z*s)*y_ + (x*z*(1-c)-y*s)*z_,
				    (x*y*(1-c)-z*s)*x_ + (y2+(x2+z2)*c)*y_ + (y*z*(1-c)+x*s)*z_,
				    (x*z*(1-c)+y*s)*x_ + (y*z*(1-c)-x*s)*y_ + (z2+(x2+y2)*c)*z_,
				    bool( ) );
}



Concrete::Coords3D
Concrete::operator * ( Concrete::Length length, const Concrete::UnitFloatTriple & direction )
{
  return Concrete::Coords3D( length * direction.x_, length * direction.y_, length * direction.z_ );
}

Concrete::Coords3D
Concrete::operator * ( const Concrete::UnitFloatTriple & direction, Concrete::Length length )
{
  return Concrete::Coords3D( length * direction.x_, length * direction.y_, length * direction.z_ );
}

// Concrete::UnitFloatTriple
// Concrete::operator + ( const Concrete::UnitFloatTriple & coords1, const Concrete::UnitFloatTriple & coords2 )
// {
//   return Concrete::UnitFloatTriple( coords1.x_ + coords2.x_, coords1.y_ + coords2.y_, coords1.z_ + coords2.z_ );
// }

// Concrete::UnitFloatTriple
// Concrete::operator - ( const Concrete::UnitFloatTriple & coords1, const Concrete::UnitFloatTriple & coords2 )
// {
//   return Concrete::UnitFloatTriple( coords1.x_ - coords2.x_, coords1.y_ - coords2.y_, coords1.z_ - coords2.z_ );
// }

Concrete::Area
Concrete::crossMagnitude( const Concrete::Coords3D & a, const Concrete::Coords3D & b )
{
  return hypotPhysical( a.y_ * b.z_ - a.z_ * b.y_,
			a.z_ * b.x_ - a.x_ * b.z_,
			a.x_ * b.y_ - a.y_ * b.x_ );
}
