#include <cmath>

#include "metapdftypes.h"
#include "metapdfexceptions.h"
#include "metapdfastexpr.h"
#include "consts.h"
#include "angleselect.h"
#include "metapdfastvar.h"
#include "metapdfastclass.h"
#include "elementarycoords.h"
#include "globals.h"

#include <ctype.h>
#include <stack>

using namespace MetaPDF;
using namespace std;


Lang::Void::Void( )
{ }

RefCountPtr< const Lang::Class > Lang::Void::TypeID( new Lang::SystemFinalClass( strrefdup( "Void" ) ) );
TYPEINFOIMPL( Void );

void
Lang::Void::show( std::ostream & os ) const
{
}


Lang::Symbol::NameTableType Lang::Symbol::nameTable;
Lang::Symbol::ReverseTableType Lang::Symbol::reverseTable;
int Lang::Symbol::nextUnique = -1;

/* The following global variables used to be in globals.cc, but now they are here to ensure they get initialized after the static variables in Lang::Symbol.
 */
RefCountPtr< const Lang::Symbol > Kernel::THE_NAVIGATION_SYMBOL( ".navigation" ); /* Note that the leading dot puts this symbol aside all user-symbols. */
RefCountPtr< const Lang::Symbol > Kernel::THE_ANNOTATION_SYMBOL( ".annotation" ); /* Note that the leading dot puts this symbol aside all user-symbols. */

Lang::Symbol::Symbol( )
{
  key_ = nextUnique;
  --nextUnique;
}

DISPATCHIMPL( Symbol );

Lang::Symbol::Symbol( int key )
{
  if( key != 0 )
    {
      throw Exceptions::InternalError( "Only the key 0 may be used when creating symbols with a given key." );
    }
  key_ = 0;
}

Lang::Symbol::Symbol( const char * name )
{
  NameTableType::const_iterator i = nameTable.find( name );
  if( i != nameTable.end( ) )
    {
      key_ = i->second;
    }
  else
    {
      const char * nameCopy = strdup( name );
      key_ = nameTable.size( ) + 1;
      nameTable[ nameCopy ] = key_;
      reverseTable.insert( ReverseTableType::value_type( key_, RefCountPtr< const char >( nameCopy ) ) );
    }
}

bool
Lang::Symbol::operator == ( const Symbol & other ) const
{
  return key_ == other.key_;
}

bool
Lang::Symbol::operator != ( const Symbol & other ) const
{
  return key_ != other.key_;
}

bool
Lang::Symbol::operator < ( const Symbol & other ) const
{
  return key_ < other.key_;
}

bool
Lang::Symbol::operator > ( const Symbol & other ) const
{
  return key_ > other.key_;
}

bool
Lang::Symbol::operator <= ( const Symbol & other ) const
{
  return key_ <= other.key_;
}

bool
Lang::Symbol::operator >= ( const Symbol & other ) const
{
  return key_ >= other.key_;
}

bool
Lang::Symbol::isUnique( ) const
{
  return key_ < 0;
}

RefCountPtr< const char >
Lang::Symbol::name( ) const
{
  if( key_ > 0 )
    {
      ReverseTableType::const_iterator i = reverseTable.find( key_ );
      if( i == reverseTable.end( ) )
	{
	  throw Exceptions::InternalError( "The reverse symbol table did not include the sought key." );
	}
      return i->second;
    }
  else if( key_ < 0 )
    {
      return strrefdup( "<unique>" );
    }
  return strrefdup( "<dummy>" );
}

RefCountPtr< const char >
Lang::Symbol::nameFromKey( KeyType key )
{
  if( key <= 0 )
    {
      throw Exceptions::MiscellaneousRequirement( "If is forbidden to ask for the name of a unique symbol." );
    }
  ReverseTableType::const_iterator i = reverseTable.find( key );
  if( i == reverseTable.end( ) )
    {
      throw Exceptions::InternalError( "The reverse symbol table did not include the sought key." );
    }
  return i->second;
}


RefCountPtr< const Lang::Class > Lang::Symbol::TypeID( new Lang::SystemFinalClass( strrefdup( "Symbol" ) ) );
TYPEINFOIMPL( Symbol );

void
Lang::Symbol::show( std::ostream & os ) const
{
  if( isUnique( ) )
    {
      os << "< unique >" ;
    }
  else
    {
      os << name( ) ;
    }
}


DISPATCHIMPL( Float );

RefCountPtr< const Lang::Class > Lang::Float::TypeID( new Lang::SystemFinalClass( strrefdup( "Float" ) ) );
TYPEINFOIMPL( Float );

void
Lang::Float::show( std::ostream & os ) const
{
  os << val_ ;
}


DISPATCHIMPL( Int );

RefCountPtr< const Lang::Class > Lang::Integer::TypeID( new Lang::SystemFinalClass( strrefdup( "Integer" ) ) );
TYPEINFOIMPL( Integer );

void
Lang::Integer::show( std::ostream & os ) const
{
  os << "'" << val_ ;
}

/*
Lang::Length::Length( double val )
  : isOffset_( false ), val_( val )
{ }

Lang::Length::Length( bool isOffset, double val )
  : isOffset_( _isOffset ), val_( _val )
{ }
*/
DISPATCHIMPL( Length );

Concrete::Length
Lang::Length::get( Concrete::Length baseLength ) const
{
  if( isOffset_ )
    {
      return baseLength + val_;
    }
  return val_;
}

Concrete::Length
Lang::Length::get( ) const
{
  if( isOffset_ )
    {
      throw Exceptions::MiscellaneousRequirement( "Offset lengths are not allowed here." );
    }
  return val_;
}

double
Lang::Length::getScalar( Concrete::Length baseLength ) const
{
  if( isOffset_ )
    {
      return ( baseLength + val_ ).offtype< 1, 0 >( );
    }
  return val_.offtype< 1, 0 >( );
}

double
Lang::Length::getScalar( ) const
{
  if( isOffset_ )
    {
      throw Exceptions::MiscellaneousRequirement( "Offset lengths are not allowed here." );
    }
  return val_.offtype< 1, 0 >( );
}

Lang::Length
Lang::Length::operator + ( const Lang::Length & term ) const
{
  if( term.isOffset_ )
    {
      throw Exceptions::MiscellaneousRequirement( "The right term in a length addition must not be offset." );
    }
  return Lang::Length( isOffset_, val_ + term.val_ );
}

Lang::Length
Lang::Length::operator - ( const Lang::Length & term ) const
{
  if( term.isOffset_ )
    {
      throw Exceptions::MiscellaneousRequirement( "The right term in a length subtraction must not be offset." );
    }
  return Lang::Length( isOffset_, val_ - term.val_ );
}

RefCountPtr< const Lang::Class > Lang::Length::TypeID( new Lang::SystemFinalClass( strrefdup( "Length" ) ) );
TYPEINFOIMPL( Length );

void
Lang::Length::show( std::ostream & os ) const
{
  os << *this ;
}


std::ostream &
Lang::operator << ( std::ostream & os, const Lang::Length & self )
{
  if( self.isOffset_ )
    {
      os << "(+" ;
    }
  os << double( self.val_.offtype< 1, 0 >( ) ) * Interaction::displayUnitFactor << Interaction::displayUnitName ;
  if( self.isOffset_ )
    {
      os << ")" ;
    }
  return os;
}


DISPATCHIMPL( Boolean );

RefCountPtr< const Lang::Class > Lang::Boolean::TypeID( new Lang::SystemFinalClass( strrefdup( "Boolean" ) ) );
TYPEINFOIMPL( Boolean );

void
Lang::Boolean::show( std::ostream & os ) const
{
  if( val_ )
    {
      os << "true" ;
    }
  else
    {
      os << "false" ;
    }
}


Lang::String::~String( )
{ }

DISPATCHIMPL( String );

RefCountPtr< const Lang::Class > Lang::String::TypeID( new Lang::SystemFinalClass( strrefdup( "String" ) ) );
TYPEINFOIMPL( String );

void
Lang::String::show( std::ostream & os ) const
{
  os << val_.getPtr( ) ;
}

Lang::FloatPair::FloatPair( const Concrete::UnitFloatPair & orig )
  : x_( orig.x_ ), y_( orig.y_ )
{ }


DISPATCHIMPL( FloatPair );

Kernel::VariableHandle
Lang::FloatPair::getField( const char * fieldID, const RefCountPtr< const Lang::Value > & selfRef ) const
{
  if( strcmp( fieldID, "x" ) == 0 )
    {
      return Helpers::newValHandle( new Lang::Float( x_ ) );
    }
  if( strcmp( fieldID, "y" ) == 0 )
    {
      return Helpers::newValHandle( new Lang::Float( y_ ) );
    }
  throw Exceptions::NonExistentMember( getTypeName( ), fieldID );
}

RefCountPtr< const Lang::Class > Lang::FloatPair::TypeID( new Lang::SystemFinalClass( strrefdup( "FloatPair" ) ) );
TYPEINFOIMPL( FloatPair );

void
Lang::FloatPair::show( std::ostream & os ) const
{
  os << "( " << x_ << ", " << y_ << " )" ;
}

Lang::FloatTriple::FloatTriple( const Concrete::UnitFloatTriple & orig )
  : x_( orig.x_ ), y_( orig.y_ ), z_( orig.z_ )
{ }

DISPATCHIMPL( FloatTriple );

Kernel::VariableHandle
Lang::FloatTriple::getField( const char * fieldID, const RefCountPtr< const Lang::Value > & selfRef ) const
{
  if( strcmp( fieldID, "x" ) == 0 )
    {
      return Helpers::newValHandle( new Lang::Float( x_ ) );
    }
  if( strcmp( fieldID, "y" ) == 0 )
    {
      return Helpers::newValHandle( new Lang::Float( y_ ) );
    }
  if( strcmp( fieldID, "z" ) == 0 )
    {
      return Helpers::newValHandle( new Lang::Float( z_ ) );
    }
  throw Exceptions::NonExistentMember( getTypeName( ), fieldID );
}

RefCountPtr< const Lang::Class > Lang::FloatTriple::TypeID( new Lang::SystemFinalClass( strrefdup( "FloatTriple" ) ) );
TYPEINFOIMPL( FloatTriple );

void
Lang::FloatTriple::show( std::ostream & os ) const
{
  os << "( " << x_ << ", " << y_ << ", " << z_ << " )" ;
}


Lang::Coords2D::Coords2D( const Lang::Coords2D & orig )
  : x_( orig.x_ ), y_( orig.y_ )
{ }

Lang::Coords2D::Coords2D( const Lang::Length & x, const Lang::Length & y )
  : x_( x ), y_( y )
{ }

Lang::Coords2D::Coords2D( const Concrete::Length & x, const Concrete::Length & y )
  : x_( x ), y_( y )
{ }

DISPATCHIMPL( Coords2D );

Kernel::VariableHandle
Lang::Coords2D::getField( const char * fieldID, const RefCountPtr< const Lang::Value > & selfRef ) const
{
  if( strcmp( fieldID, "x" ) == 0 )
    {
      return Helpers::newValHandle( new Lang::Length( x_ ) );
    }
  if( strcmp( fieldID, "y" ) == 0 )
    {
      return Helpers::newValHandle( new Lang::Length( y_ ) );
    }
  throw Exceptions::NonExistentMember( getTypeName( ), fieldID );
}

Lang::Coords2D *
Lang::Coords2D::transformedPtr( const Lang::Transform2D & tf ) const
{
  Concrete::Length tmpx = x_.get( );
  Concrete::Length tmpy = y_.get( );
  return new Lang::Coords2D( tf.xx_ * tmpx + tf.xy_ * tmpy + tf.xt_, tf.yx_ * tmpx + tf.yy_ * tmpy + tf.yt_ );
}

RefCountPtr< const Lang::Geometric2D >
Lang::Coords2D::transformed( const Lang::Transform2D & tf, const RefCountPtr< const Lang::Geometric2D > & self ) const
{
  return RefCountPtr< const Lang::Geometric2D >( transformedPtr( tf ) );
}

RefCountPtr< const Lang::Geometric3D >
Lang::Coords2D::to3D( const RefCountPtr< const Lang::Geometric2D > & self ) const
{
  return RefCountPtr< const Lang::Coords3D >( new Lang::Coords3D( x_, y_, Lang::Length( Concrete::Length( 0 ) ) ) );
}

RefCountPtr< const Lang::Class > Lang::Coords2D::TypeID( new Lang::SystemFinalClass( strrefdup( "Coords2D" ) ) );
TYPEINFOIMPL( Coords2D );

void
Lang::Coords2D::show( std::ostream & os ) const
{
  os << "( " << x_ << ", " << y_ << " )" ;
}

std::ostream &
Lang::operator << ( std::ostream & os, const Lang::Coords2D & self )
{
  os << "( " << self.x_ << ", " << self.y_ << " )" ;
  return os;
}


Lang::CornerCoords2D::CornerCoords2D( const Lang::Length & x, const Lang::Length & y, double a )
  : Lang::Coords2D( x, y ), a_( a )
{ }

Lang::CornerCoords2D::CornerCoords2D( const Concrete::Length & x, const Concrete::Length & y, double a )
  : Lang::Coords2D( x, y ), a_( a )
{ }

RefCountPtr< const Lang::Class > Lang::CornerCoords2D::TypeID( new Lang::SystemFinalClass( strrefdup( "CornerCoords2D" ) ) );
DISPATCHIMPL( CornerCoords2D );

Kernel::VariableHandle
Lang::CornerCoords2D::getField( const char * fieldID, const RefCountPtr< const Lang::Value > & selfRef ) const
{
  if( strcmp( fieldID, "x" ) == 0 )
    {
      return Helpers::newValHandle( new Lang::Length( x_ ) );
    }
  if( strcmp( fieldID, "y" ) == 0 )
    {
      return Helpers::newValHandle( new Lang::Length( y_ ) );
    }
  if( strcmp( fieldID, "a" ) == 0 )
    {
      return Helpers::newValHandle( new Lang::Float( a_ ) );
    }
  throw Exceptions::NonExistentMember( getTypeName( ), fieldID );
}

Lang::CornerCoords2D *
Lang::CornerCoords2D::transformedPtr( const Lang::Transform2D & tf ) const
{
  Concrete::Length tmpx = x_.get( );
  Concrete::Length tmpy = y_.get( );
  return new Lang::CornerCoords2D( tf.xx_ * tmpx + tf.xy_ * tmpy + tf.xt_, tf.yx_ * tmpx + tf.yy_ * tmpy + tf.yt_, a_ );
}

RefCountPtr< const Lang::Geometric2D >
Lang::CornerCoords2D::transformed( const Lang::Transform2D & tf, const RefCountPtr< const Lang::Geometric2D > & self ) const
{
  return RefCountPtr< const Lang::Geometric2D >( transformedPtr( tf ) );
}

RefCountPtr< const Lang::Geometric3D >
Lang::CornerCoords2D::to3D( const RefCountPtr< const Lang::Geometric2D > & self ) const
{
  throw Exceptions::MiscellaneousRequirement( "Corner coordinates cannot move into 3D space." );
}

TYPEINFOIMPL( CornerCoords2D );


Lang::Coords3D::Coords3D( const Lang::Coords3D & orig )
  : x_( orig.x_ ), y_( orig.y_ ), z_( orig.z_ )
{ }

Lang::Coords3D::Coords3D( const Lang::Length & x, const Lang::Length & y, const Lang::Length & z )
  : x_( x ), y_( y ), z_( z )
{ }

Lang::Coords3D::Coords3D( const Concrete::Length & x, const Concrete::Length & y, const Concrete::Length & z )
  : x_( x ), y_( y ), z_( z )
{ }

DISPATCHIMPL( Coords3D );

Kernel::VariableHandle
Lang::Coords3D::getField( const char * fieldID, const RefCountPtr< const Lang::Value > & selfRef ) const
{
  if( strcmp( fieldID, "x" ) == 0 )
    {
      return Helpers::newValHandle( new Lang::Length( x_ ) );
    }
  if( strcmp( fieldID, "y" ) == 0 )
    {
      return Helpers::newValHandle( new Lang::Length( y_ ) );
    }
  if( strcmp( fieldID, "z" ) == 0 )
    {
      return Helpers::newValHandle( new Lang::Length( z_ ) );
    }
  throw Exceptions::NonExistentMember( getTypeName( ), fieldID );
}

Lang::Coords3D *
Lang::Coords3D::transformedPtr( const Lang::Transform3D & tf ) const
{
  Concrete::Length tmpx = x_.get( );
  Concrete::Length tmpy = y_.get( );
  Concrete::Length tmpz = z_.get( );
  return new Lang::Coords3D( tf.xx_ * tmpx + tf.xy_ * tmpy + tf.xz_ * tmpz + tf.xt_,
				tf.yx_ * tmpx + tf.yy_ * tmpy + tf.yz_ * tmpz + tf.yt_,
				tf.zx_ * tmpx + tf.zy_ * tmpy + tf.zz_ * tmpz + tf.zt_ );
}

RefCountPtr< const Lang::Geometric3D >
Lang::Coords3D::transformed( const Lang::Transform3D & tf, const RefCountPtr< const Lang::Geometric3D > & self ) const
{
  return RefCountPtr< const Lang::Geometric3D >( transformedPtr( tf ) );
}

RefCountPtr< const Lang::Coords2D >
Lang::Coords3D::make2D( Concrete::Length eyez ) const
{
  if( isinf( eyez.offtype< 1, 0 >( ) ) )
    {
      return RefCountPtr< const Lang::Coords2D >( new Lang::Coords2D( x_.get( ),
									    y_.get( ) ) );
    }

  return RefCountPtr< const Lang::Coords2D >( new Lang::Coords2D( x_.get( ) * ( eyez / ( eyez - z_.get( ) ) ),
									y_.get( ) * ( eyez / ( eyez - z_.get( ) ) ) ) );
}


RefCountPtr< const Lang::Geometric2D >
Lang::Coords3D::to2D( const Kernel::PassedDyn & dyn, const RefCountPtr< const Lang::Geometric3D > & self ) const
{
  return make2D( dyn->getEyeZ( ) );
}

RefCountPtr< const Lang::Class > Lang::Coords3D::TypeID( new Lang::SystemFinalClass( strrefdup( "Coords3D" ) ) );
TYPEINFOIMPL( Coords3D );

void
Lang::Coords3D::show( std::ostream & os ) const
{
  os << "( " << x_ << ", " << y_ << ", " << z_ << " )" ;
}

std::ostream &
Lang::operator << ( std::ostream & os, const Lang::Coords3D & self )
{
  os << "( " << self.x_ << ", " << self.y_ << ", " << self.z_ << " )" ;
  return os;
}
