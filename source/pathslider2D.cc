#include <cmath>

#include "metapdftypes.h"
#include "metapdfexceptions.h"
#include "astexpr.h"
#include "consts.h"
#include "globals.h"
#include "bezier.h"

#include <ctype.h>
#include <stack>
#include <algorithm>

using namespace MetaPDF;
using namespace std;


Lang::PathSlider2D::PathSlider2D( const Lang::PathSlider2D & orig )
  : path_( orig.getPath( ) ), t_( orig.getTime( ) )
{ }

Lang::PathSlider2D::PathSlider2D( const RefCountPtr< const Lang::ElementaryPath2D > & path )
  : path_( path ), t_( 0 )
{ }

Lang::PathSlider2D::PathSlider2D( const RefCountPtr< const Lang::ElementaryPath2D > & path, Concrete::SplineTime t )
  : path_( path ), t_( t )
{ }

DISPATCHIMPL( PathSlider2D );

Lang::PathSlider2D::~PathSlider2D( )
{ }

RefCountPtr< const Lang::Class > Lang::PathSlider2D::TypeID( new Lang::SystemFinalClass( strrefdup( "PathSlider2D" ) ) );
TYPEINFOIMPL( PathSlider2D );

Kernel::VariableHandle
Lang::PathSlider2D::getField( const char * fieldID, const RefCountPtr< const Lang::Value > & selfRef ) const
{
  if( strcmp( fieldID, "p" ) == 0 )
    {
      return Kernel::VariableHandle( new Kernel::Variable( point( ) ) );
    }
  if( strcmp( fieldID, "v" ) == 0 )
    {
      return Kernel::VariableHandle( new Kernel::Variable( speed( ) ) );
    }
  if( strcmp( fieldID, "rv" ) == 0 )
    {
      return Kernel::VariableHandle( new Kernel::Variable( reverse_speed( ) ) );
    }
  if( strcmp( fieldID, "t" ) == 0 )
    {
      return Kernel::VariableHandle( new Kernel::Variable( direction( ) ) );
    }
  if( strcmp( fieldID, "rt" ) == 0 )
    {
      return Kernel::VariableHandle( new Kernel::Variable( reverse_direction( ) ) );
    }
  if( strcmp( fieldID, "n" ) == 0 )
    {
      return Kernel::VariableHandle( new Kernel::Variable( normal( ) ) );
    }
  if( strcmp( fieldID, "rn" ) == 0 )
    {
      return Kernel::VariableHandle( new Kernel::Variable( reverse_normal( ) ) );
    }
  if( strcmp( fieldID, "ik" ) == 0 )
    {
      return Kernel::VariableHandle( new Kernel::Variable( radiusOfCurvature( ) ) );
    }
  if( strcmp( fieldID, "rik" ) == 0 )
    {
      return Kernel::VariableHandle( new Kernel::Variable( reverse_radiusOfCurvature( ) ) );
    }
  if( strcmp( fieldID, "time" ) == 0 )
    {
      return Kernel::VariableHandle( new Kernel::Variable( time( ) ) );
    }
  if( strcmp( fieldID, "length" ) == 0 )
    {
      return Kernel::VariableHandle( new Kernel::Variable( length( ) ) );
    }
  if( strcmp( fieldID, "past" ) == 0 )
    {
      if( t_.isPast( ) )
	{
	  return Kernel::THE_TRUE_VARIABLE;
	}
      return Kernel::THE_FALSE_VARIABLE;
    }
  if( strcmp( fieldID, "looped" ) == 0 )
    {
      if( t_.t( ) > Concrete::Time( path_->duration( ) ) ||
	  t_.t( ) < Concrete::Time( 0 ) )
	{
	  return Kernel::THE_TRUE_VARIABLE;
	}
      return Kernel::THE_FALSE_VARIABLE;
    }
  if( strcmp( fieldID, "mod" ) == 0 )
    {
      return Helpers::newValHandle( new Lang::PathSlider2D( path_, modPhysical( t_.t( ), Concrete::Time( path_->duration( ) ) ) ) );
    }
  throw Exceptions::NonExistentMember( getTypeName( ), fieldID );
}

RefCountPtr< Lang::PathSlider2D >
Lang::PathSlider2D::move_time( const Concrete::Time delta ) const
{
  Concrete::Time newTime = t_.t( ) + delta;
  if( path_->isClosed( ) )
    {
      return RefCountPtr< Lang::PathSlider2D >( new Lang::PathSlider2D( path_, newTime ) );
    }
  return RefCountPtr< Lang::PathSlider2D >( new Lang::PathSlider2D( path_,
								    Concrete::SplineTime( newTime,
											  Concrete::Time( 0 ) <= newTime &&
											  newTime <= Concrete::Time( path_->duration( ) ) ) ) );
}

RefCountPtr< Lang::PathSlider2D >
Lang::PathSlider2D::move_length( const Concrete::Length delta ) const
{
  return RefCountPtr< Lang::PathSlider2D >( new Lang::PathSlider2D( path_, path_->arcTime( delta, t_.t( ) ) ) );
}

RefCountPtr< Lang::PathSlider2D >
Lang::PathSlider2D::seek_time( const Concrete::Time newTime ) const
{
  if( path_->isClosed( ) )
    {
      return RefCountPtr< Lang::PathSlider2D >( new Lang::PathSlider2D( path_, newTime ) );
    }
  return RefCountPtr< Lang::PathSlider2D >( new Lang::PathSlider2D( path_,
								    Concrete::SplineTime( newTime,
											  Concrete::Time( 0 ) <= newTime &&
											  newTime <= Concrete::Time( path_->duration( ) ) ) ) );
}

RefCountPtr< Lang::PathSlider2D >
Lang::PathSlider2D::seek_start( ) const
{
  return RefCountPtr< Lang::PathSlider2D >( new Lang::PathSlider2D( path_, Concrete::ZERO_TIME ) );
}

RefCountPtr< Lang::PathSlider2D >
Lang::PathSlider2D::seek_end( ) const
{
  return RefCountPtr< Lang::PathSlider2D >( new Lang::PathSlider2D( path_, Concrete::Time( path_->duration( ) ) ) );
}

RefCountPtr< Lang::PathSlider2D >
Lang::PathSlider2D::seek_length( const Concrete::Length val ) const
{
  return RefCountPtr< Lang::PathSlider2D >( new Lang::PathSlider2D( path_, path_->arcTime( val ) ) );
}


RefCountPtr< const Lang::Coords2D >
Lang::PathSlider2D::point( ) const
{
  return path_->point( t_.t( ) );
}

RefCountPtr< const Lang::Length >
Lang::PathSlider2D::speed( ) const
{
  return path_->speed( t_.t( ) );
}

RefCountPtr< const Lang::Length >
Lang::PathSlider2D::reverse_speed( ) const
{
  return path_->reverse_speed( t_.t( ) );
}

RefCountPtr< const Lang::FloatPair >
Lang::PathSlider2D::direction( ) const
{
  return path_->direction( t_.t( ) );
}

RefCountPtr< const Lang::FloatPair >
Lang::PathSlider2D::reverse_direction( ) const
{
  return path_->reverse_direction( t_.t( ) );
}

RefCountPtr< const Lang::FloatPair >
Lang::PathSlider2D::normal( ) const
{
  RefCountPtr< const Lang::FloatPair > tmp = path_->direction( t_.t( ) );
  return RefCountPtr< const Lang::FloatPair >( new Lang::FloatPair( -tmp->y_, tmp->x_ ) );
}

RefCountPtr< const Lang::FloatPair >
Lang::PathSlider2D::reverse_normal( ) const
{
  RefCountPtr< const Lang::FloatPair > tmp = path_->reverse_direction( t_.t( ) );
  return RefCountPtr< const Lang::FloatPair >( new Lang::FloatPair( -tmp->y_, tmp->x_ ) );
}

RefCountPtr< const Lang::Length >
Lang::PathSlider2D::radiusOfCurvature( ) const
{
  return path_->radiusOfCurvature( t_.t( ) );
}

RefCountPtr< const Lang::Length >
Lang::PathSlider2D::reverse_radiusOfCurvature( ) const
{
  return path_->reverse_radiusOfCurvature( t_.t( ) );
}

RefCountPtr< const Lang::Float >
Lang::PathSlider2D::time( ) const
{
  return RefCountPtr< const Lang::Float >( new Lang::Float( t_.t( ).offtype< 0, 1 >( ) ) );
}

RefCountPtr< const Lang::Length >
Lang::PathSlider2D::length( ) const
{
  return RefCountPtr< const Lang::Length >( new Lang::Length( path_->arcLength( t_.t( ) ) ) );
}


void
Lang::PathSlider2D::show( std::ostream & os ) const
{
  os << "Path slider " ;
  if( this->getRear( ) != NullPtr< const Lang::Value >( ) )
    {
      os << "(with rear) " ;
    }
  if( this->getFront( ) != NullPtr< const Lang::Value >( ) )
    {
      os << "(with front) " ;
    }  
  os << "at " << t_.t( ).offtype< 0, 1 >( ) ;
  if( t_.isPast( ) )
    {
      os << " (past)" ;
    }
  os << " / " << path_->duration( ) ;
}

void
Lang::PathSlider2D::gcMark( Kernel::GCMarkedSet & marked )
{
  const_cast< Lang::ElementaryPath2D * >( path_.getPtr( ) )->gcMark( marked );
}


RefCountPtr< const Lang::Value >
Lang::PathSlider2D::getRear( ) const
{
  return RefCountPtr< const Lang::Value >( NullPtr< const Lang::Value >( ) );
}

RefCountPtr< const Lang::Value >
Lang::PathSlider2D::getFront( ) const
{
  return RefCountPtr< const Lang::Value >( NullPtr< const Lang::Value >( ) );
}


Lang::PathSlider2D_rear::PathSlider2D_rear( const Lang::PathSlider2D & orig, const RefCountPtr< const Lang::Value > & rear )
  : PathSlider2D( orig ), rear_( rear )
{ }

RefCountPtr< const Lang::Value >
Lang::PathSlider2D_rear::getRear( ) const
{
  return rear_;
}

Lang::PathSlider2D_front::PathSlider2D_front( const Lang::PathSlider2D & orig, const RefCountPtr< const Lang::Value > & front )
  : PathSlider2D( orig ), front_( front )
{ }

RefCountPtr< const Lang::Value >
Lang::PathSlider2D_front::getFront( ) const
{
  return front_;
}
