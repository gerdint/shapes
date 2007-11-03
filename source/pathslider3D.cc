#include <cmath>

#include "shapestypes.h"
#include "shapesexceptions.h"
#include "astexpr.h"
#include "consts.h"
#include "globals.h"
#include "bezier.h"

#include <ctype.h>
#include <stack>
#include <algorithm>

using namespace Shapes;
using namespace std;


Lang::PathSlider3D::PathSlider3D( const Lang::PathSlider3D & orig )
  : path_( orig.getPath( ) ), t_( orig.getTime( ) )
{ }

Lang::PathSlider3D::PathSlider3D( const RefCountPtr< const Lang::ElementaryPath3D > & path )
  : path_( path ), t_( 0 )
{ }

Lang::PathSlider3D::PathSlider3D( const RefCountPtr< const Lang::ElementaryPath3D > & path, Concrete::SplineTime t )
  : path_( path ), t_( t )
{ }

DISPATCHIMPL( PathSlider3D );

Lang::PathSlider3D::~PathSlider3D( )
{ }

RefCountPtr< const Lang::Class > Lang::PathSlider3D::TypeID( new Lang::SystemFinalClass( strrefdup( "PathSlider3D" ) ) );
TYPEINFOIMPL( PathSlider3D );

Kernel::VariableHandle
Lang::PathSlider3D::getField( const char * fieldID, const RefCountPtr< const Lang::Value > & selfRef ) const
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
  if( strcmp( fieldID, "b" ) == 0 )
    {
      return Kernel::VariableHandle( new Kernel::Variable( binormal( ) ) );
    }
  if( strcmp( fieldID, "rb" ) == 0 )
    {
      return Kernel::VariableHandle( new Kernel::Variable( reverse_binormal( ) ) );
    }
  if( strcmp( fieldID, "ik" ) == 0 )
    {
      return Kernel::VariableHandle( new Kernel::Variable( radiusOfCurvature( ) ) );
    }
  if( strcmp( fieldID, "rik" ) == 0 )
    {
      return Kernel::VariableHandle( new Kernel::Variable( reverse_radiusOfCurvature( ) ) );
    }
  if( strcmp( fieldID, "it" ) == 0 )
    {
      return Kernel::VariableHandle( new Kernel::Variable( radiusOfTorsion( ) ) );
    }
  if( strcmp( fieldID, "rit" ) == 0 )
    {
      return Kernel::VariableHandle( new Kernel::Variable( reverse_radiusOfTorsion( ) ) );
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
      return Helpers::newValHandle( new Lang::PathSlider3D( path_, modPhysical( t_.t( ), Concrete::Time( path_->duration( ) ) ) ) );
    }
  throw Exceptions::NonExistentMember( getTypeName( ), fieldID );
}

RefCountPtr< Lang::PathSlider3D >
Lang::PathSlider3D::move_time( const Concrete::Time delta ) const
{
  Concrete::Time newTime = t_.t( ) + delta;
  if( path_->isClosed( ) )
    {
      return RefCountPtr< Lang::PathSlider3D >( new Lang::PathSlider3D( path_, newTime ) );
    }
  return RefCountPtr< Lang::PathSlider3D >( new Lang::PathSlider3D( path_,
								    Concrete::SplineTime( newTime,
											  Concrete::Time( 0 ) <= newTime &&
											  newTime <= Concrete::Time( path_->duration( ) ) ) ) );
}

RefCountPtr< Lang::PathSlider3D >
Lang::PathSlider3D::move_length( const Concrete::Length delta ) const
{
  return RefCountPtr< Lang::PathSlider3D >( new Lang::PathSlider3D( path_, path_->arcTime( delta, t_.t( ) ) ) );
}

RefCountPtr< Lang::PathSlider3D >
Lang::PathSlider3D::seek_time( const Concrete::Time newTime ) const
{
  if( path_->isClosed( ) )
    {
      return RefCountPtr< Lang::PathSlider3D >( new Lang::PathSlider3D( path_, newTime ) );
    }
  return RefCountPtr< Lang::PathSlider3D >( new Lang::PathSlider3D( path_,
								    Concrete::SplineTime( newTime,
											  Concrete::Time( 0 ) <= newTime &&
											  newTime <= Concrete::Time( path_->duration( ) ) ) ) );
}

RefCountPtr< Lang::PathSlider3D >
Lang::PathSlider3D::seek_start( ) const
{
  return RefCountPtr< Lang::PathSlider3D >( new Lang::PathSlider3D( path_, Concrete::Time( 0. ) ) );
}

RefCountPtr< Lang::PathSlider3D >
Lang::PathSlider3D::seek_end( ) const
{
  return RefCountPtr< Lang::PathSlider3D >( new Lang::PathSlider3D( path_, Concrete::Time( path_->duration( ) ) ) );
}

RefCountPtr< Lang::PathSlider3D >
Lang::PathSlider3D::seek_length( const Concrete::Length val ) const
{
  return RefCountPtr< Lang::PathSlider3D >( new Lang::PathSlider3D( path_, path_->arcTime( val ) ) );
}


RefCountPtr< const Lang::Coords3D >
Lang::PathSlider3D::point( ) const
{
  return path_->point( t_.t( ) );
}

RefCountPtr< const Lang::Length >
Lang::PathSlider3D::speed( ) const
{
  return path_->speed( t_.t( ) );
}

RefCountPtr< const Lang::Length >
Lang::PathSlider3D::reverse_speed( ) const
{
  return path_->reverse_speed( t_.t( ) );
}

RefCountPtr< const Lang::FloatTriple >
Lang::PathSlider3D::direction( ) const
{
  return path_->direction( t_.t( ) );
}

RefCountPtr< const Lang::FloatTriple >
Lang::PathSlider3D::reverse_direction( ) const
{
  return path_->reverse_direction( t_.t( ) );
}

RefCountPtr< const Lang::FloatTriple >
Lang::PathSlider3D::normal( ) const
{
  return path_->normal( t_.t( ) );
}

RefCountPtr< const Lang::FloatTriple >
Lang::PathSlider3D::reverse_normal( ) const
{
  return path_->reverse_normal( t_.t( ) );
}

RefCountPtr< const Lang::FloatTriple >
Lang::PathSlider3D::binormal( ) const
{
  RefCountPtr< const Lang::FloatTriple > tmp_T = path_->direction( t_.t( ) );
  RefCountPtr< const Lang::FloatTriple > tmp_N = path_->normal( t_.t( ) );
  return RefCountPtr< const Lang::FloatTriple >( new Lang::FloatTriple( Lang::cross( *tmp_T, *tmp_N ) ) );
}

RefCountPtr< const Lang::FloatTriple >
Lang::PathSlider3D::reverse_binormal( ) const
{
  RefCountPtr< const Lang::FloatTriple > tmp_T = path_->reverse_direction( t_.t( ) );
  RefCountPtr< const Lang::FloatTriple > tmp_N = path_->reverse_normal( t_.t( ) );
  return RefCountPtr< const Lang::FloatTriple >( new Lang::FloatTriple( Lang::cross( *tmp_T, *tmp_N ) ) );
}

RefCountPtr< const Lang::Length >
Lang::PathSlider3D::radiusOfCurvature( ) const
{
  return path_->radiusOfCurvature( t_.t( ) );
}

RefCountPtr< const Lang::Length >
Lang::PathSlider3D::reverse_radiusOfCurvature( ) const
{
  return path_->reverse_radiusOfCurvature( t_.t( ) );
}

RefCountPtr< const Lang::Length >
Lang::PathSlider3D::radiusOfTorsion( ) const
{
  return path_->radiusOfTorsion( t_.t( ) );
}

RefCountPtr< const Lang::Length >
Lang::PathSlider3D::reverse_radiusOfTorsion( ) const
{
  return path_->reverse_radiusOfTorsion( t_.t( ) );
}

RefCountPtr< const Lang::Float >
Lang::PathSlider3D::time( ) const
{
  return RefCountPtr< const Lang::Float >( new Lang::Float( t_.t( ).offtype< 0, 1 >( ) ) );
}

RefCountPtr< const Lang::Length >
Lang::PathSlider3D::length( ) const
{
  return RefCountPtr< const Lang::Length >( new Lang::Length( path_->arcLength( t_.t( ) ) ) );
}


void
Lang::PathSlider3D::show( std::ostream & os ) const
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
  os << " at " << t_.t( ).offtype< 0, 1 >( ) ;
  if( t_.isPast( ) )
    {
      os << " (past)" ;
    }
  os << " / " << path_->duration( );
}

void
Lang::PathSlider3D::gcMark( Kernel::GCMarkedSet & marked )
{
  const_cast< Lang::ElementaryPath3D * >( path_.getPtr( ) )->gcMark( marked );
}


RefCountPtr< const Lang::Value >
Lang::PathSlider3D::getRear( ) const
{
  return RefCountPtr< const Lang::Value >( NullPtr< const Lang::Value >( ) );
}

RefCountPtr< const Lang::Value >
Lang::PathSlider3D::getFront( ) const
{
  return RefCountPtr< const Lang::Value >( NullPtr< const Lang::Value >( ) );
}


Lang::PathSlider3D_rear::PathSlider3D_rear( const Lang::PathSlider3D & orig, const RefCountPtr< const Lang::Value > & rear )
  : PathSlider3D( orig ), rear_( rear )
{ }

RefCountPtr< const Lang::Value >
Lang::PathSlider3D_rear::getRear( ) const
{
  return rear_;
}

Lang::PathSlider3D_front::PathSlider3D_front( const Lang::PathSlider3D & orig, const RefCountPtr< const Lang::Value > & front )
  : PathSlider3D( orig ), front_( front )
{ }

RefCountPtr< const Lang::Value >
Lang::PathSlider3D_front::getFront( ) const
{
  return front_;
}
