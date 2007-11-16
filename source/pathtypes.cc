#include <cmath>

#include "shapestypes.h"
#include "shapesexceptions.h"
#include "astexpr.h"
#include "consts.h"
#include "globals.h"
#include "angleselect.h"
#include "astvar.h"
#include "astclass.h"
#include "continuations.h"
#include "dynamicenvironment.h"

#include <complex>
#include <ctype.h>
#include <stack>

using namespace Shapes;
using namespace std;


Concrete::Time
Shapes::computeDt( Concrete::Length segLength )
{
  double dt = Computation::the_arcdelta / segLength;
  if( dt < Computation::the_dtMin )
    {
      if( Computation::dtMinIsError )
	{
	  throw Exceptions::DtMinError( dt );
	}
      dt = Computation::the_dtMin;
    }
  return dt;
}

Concrete::Time
Shapes::straightLineArcTime( double fraction )
{
  typedef std::complex< double > Complex;
  Complex s( fraction, 0 );
  const Complex add1iSqrt3( 1, sqrt( 3 ) );
  const Complex sub1iSqrt3( 1, -sqrt( 3 ) );
  const Complex tmp = pow( Complex( -1, 0 ) + Complex( 2, 0 ) * ( s + sqrt( ( Complex( -1, 0 ) + s ) * s ) ), 1./3 );
  return 0.5 + 0.25 * ( (sub1iSqrt3 / tmp ).real( ) + ( add1iSqrt3 * tmp ).real( ) );
  //  const Complex cRes = Complex( 0.5, 0 ) + Complex( 0.25, 0 ) * ( sub1iSqrt3 / tmp + add1iSqrt3 * tmp );
  //  return cRes.real( );
}

Kernel::PolarHandlePromise::PolarHandlePromise( )
{ }

Kernel::PolarHandlePromise::~PolarHandlePromise( )
{ }


Kernel::PolarHandleEmptyPromise::PolarHandleEmptyPromise( )
{ }

Kernel::PolarHandleEmptyPromise::~PolarHandleEmptyPromise( )
{ }

double
Kernel::PolarHandleEmptyPromise::force( const Concrete::PathPoint2D * specialUnitP0, const Concrete::PathPoint2D * specialUnitP1, bool reverse ) const
{
  throw Exceptions::MiscellaneousRequirement( strrefdup( "The empty promise must be replaced before use.  Probably related to the default unit." ) );
}

void
Kernel::PolarHandleEmptyPromise::gcMark( Kernel::GCMarkedSet & marked ) const
{

}


Kernel::PolarHandleTruePromise::PolarHandleTruePromise( Kernel::Thunk * thunk )
  : thunk_( thunk )
{ }

Kernel::PolarHandleTruePromise::~PolarHandleTruePromise( )
{
  delete thunk_;
}


double
Kernel::PolarHandleTruePromise::force( const Concrete::PathPoint2D * specialUnitP0, const Concrete::PathPoint2D * specialUnitP1, bool reverse ) const
{
  Kernel::ValueRef valUntyped = NullPtr< const Lang::Value >( );
  
  /* Note that the use of a StoreValueContinuation relies on valUntyped being alive at the time the continuation is invoked.
   */
  bool done = false;
  Kernel::EvalState evalState( 0,
				0,
				NullPtr< Kernel::DynamicEnvironment >( ),
				Kernel::ContRef( new Kernel::StoreValueContinuation( & valUntyped, 
										       Kernel::ContRef( new Kernel::ExitContinuation( & done, thunk_->getExpr( )->loc( ) ) ),
										       thunk_->getExpr( )->loc( ) ) ) );

  thunk_->force( & evalState, false );  /* note that the dynamic environment is set below, before evaluation begins. "false" means that the thunk may be forced repeatedly */

  Kernel::SpecialUnitVariables * dynVars = new Kernel::SpecialUnitVariables;
  dynVars->reverseDirection_ = reverse;
  dynVars->p0_ = specialUnitP0;
  dynVars->p1_ = specialUnitP1;
  evalState.dyn_ = Kernel::PassedDyn( new Kernel::DynamicEnvironment( evalState.dyn_, dynVars ) );

  while( ! done )
    {
      evalState.expr_->eval( & evalState );
    }

  typedef const Lang::Length ArgType;
  ArgType * valPtr = dynamic_cast< ArgType * >( valUntyped.getPtr( ) );
  if( valPtr == 0 )
    {
      throw Exceptions::TypeMismatch( thunk_->getExpr( )->loc( ), valUntyped->getTypeName( ), ArgType::staticTypeName( ) );
    }

  return valPtr->getScalar( );
}

void
Kernel::PolarHandleTruePromise::gcMark( Kernel::GCMarkedSet & marked ) const
{
  if( thunk_ != 0 )
    {
      thunk_->gcMark( marked );
    }
}


Lang::PolarHandleBase::PolarHandleBase( )
{ }

DISPATCHIMPL( PolarHandleBase );

Lang::PolarHandleBase::~PolarHandleBase( )
{ }

RefCountPtr< const Lang::Class > Lang::PolarHandleBase::TypeID( new Lang::SystemFinalClass( strrefdup( "PolarHandle" ) ) );
TYPEINFOIMPL( PolarHandleBase );


Lang::PolarHandle2D::PolarHandle2D( const RefCountPtr< const Kernel::PolarHandlePromise > & rPromise, double a )
  : rPromise_( rPromise ), a_( a )
{ }

DISPATCHIMPL( PolarHandle2D );

Lang::PolarHandle2D::~PolarHandle2D( )
{ }

void
Lang::PolarHandle2D::show( std::ostream & os ) const
{
  os << "Completely specified" ;
}

void
Lang::PolarHandle2D::gcMark( Kernel::GCMarkedSet & marked )
{
  rPromise_->gcMark( marked );
}


Lang::PolarHandle2DFree_a::PolarHandle2DFree_a( const RefCountPtr< const Kernel::PolarHandlePromise > & rPromise )
  : rPromise_( rPromise )
{ }

DISPATCHIMPL( PolarHandle2DFree_a );

Lang::PolarHandle2DFree_a::~PolarHandle2DFree_a( )
{ }

void
Lang::PolarHandle2DFree_a::show( std::ostream & os ) const
{
  os << "Free angle, promise pointer: " << rPromise_.getPtr( ) ;
}

void
Lang::PolarHandle2DFree_a::gcMark( Kernel::GCMarkedSet & marked )
{
  rPromise_->gcMark( marked );
}


Lang::PolarHandle2DFree_r::PolarHandle2DFree_r( const RefCountPtr< const Kernel::PolarHandlePromise > & defaultModulus, double a )
  : defaultModulus_( defaultModulus ), a_( a )
{ }

DISPATCHIMPL( PolarHandle2DFree_r );

Lang::PolarHandle2DFree_r::~PolarHandle2DFree_r( )
{ }

void
Lang::PolarHandle2DFree_r::show( std::ostream & os ) const
{
  os << "Free modulus, angle = " << a_ ;
}

void
Lang::PolarHandle2DFree_r::gcMark( Kernel::GCMarkedSet & marked )
{
  defaultModulus_->gcMark( marked );
}


Lang::PolarHandle2DFree_ra::PolarHandle2DFree_ra( const RefCountPtr< const Kernel::PolarHandlePromise > & defaultModulus )
  : defaultModulus_( defaultModulus )
{ }

DISPATCHIMPL( PolarHandle2DFree_ra );

Lang::PolarHandle2DFree_ra::~PolarHandle2DFree_ra( )
{ }

void
Lang::PolarHandle2DFree_ra::show( std::ostream & os ) const
{
  os << "Completely free" ;
}

void
Lang::PolarHandle2DFree_ra::gcMark( Kernel::GCMarkedSet & marked )
{
  defaultModulus_->gcMark( marked );
}


Concrete::PathPoint2D::PathPoint2D( const Concrete::PathPoint2D & orig )
  : rearState_( orig.rearState_ ), rearAngle_( orig.rearAngle_ ), rearModulus_( orig.rearModulus_ ), 
    rearModulusPromise_( NullPtr< Kernel::PolarHandlePromise >( ) ), 
    frontState_( orig.frontState_ ), frontAngle_( orig.frontAngle_ ), frontModulus_( orig.frontModulus_ ),
    frontModulusPromise_( NullPtr< Kernel::PolarHandlePromise >( ) ),
    defaultAngle_( orig.defaultAngle_ )
{
  mid_ = new Concrete::Coords2D( *orig.mid_ );

  if( orig.rear_ != orig.mid_ )
    {
      rear_ = new Concrete::Coords2D( *orig.rear_ );
    }
  else
    {
      rear_ = mid_;
    }
  if( orig.front_ != orig.mid_ )
    {
      front_ = new Concrete::Coords2D( *orig.front_ );
    }
  else
    {
      front_ = mid_;
    }
}

Concrete::PathPoint2D::PathPoint2D( Concrete::Coords2D * mid )
  : rearState_( COMPLETE ), rearModulusPromise_( NullPtr< Kernel::PolarHandlePromise >( ) ), 
    frontState_( COMPLETE ), frontModulusPromise_( NullPtr< Kernel::PolarHandlePromise >( ) ),
    defaultAngle_( 0 ),
    rear_( mid ), mid_( mid ), front_( mid )
{ }

Concrete::PathPoint2D::PathPoint2D( Concrete::Length midx, Concrete::Length midy )
  : rearState_( COMPLETE ), rearModulusPromise_( NullPtr< Kernel::PolarHandlePromise >( ) ), 
    frontState_( COMPLETE ), frontModulusPromise_( NullPtr< Kernel::PolarHandlePromise >( ) ),
    defaultAngle_( 0 ),
    rear_( new Concrete::Coords2D( midx, midy ) ), mid_( rear_ ), front_( rear_ )
{ }

Concrete::PathPoint2D::~PathPoint2D( )
{
  if( rear_ != 0 && rear_ != mid_ )
    {
      delete rear_;
    }
  if( mid_ != 0 )
    {
      delete mid_;
    }
  if( front_ != 0 && front_ != mid_ )
    {
      delete front_;
    }
}

Concrete::PathPoint2D *
Concrete::PathPoint2D::transformed( const Lang::Transform2D & tf ) const
{
  Concrete::PathPoint2D * res = new Concrete::PathPoint2D( mid_->transformedPtr( tf ) );
  if( rear_ != mid_ )
    {
      res->rear_ = rear_->transformedPtr( tf );
    }
  if( front_ != mid_ )
    {
      res->front_ = front_->transformedPtr( tf );
    }
  return res;
}

Concrete::PathPoint3D *
Concrete::PathPoint2D::transformed( const Lang::Transform3D & tf ) const
{
  Concrete::PathPoint3D * res = new Concrete::PathPoint3D( mid_->transformedPtr( tf ) );
  if( rear_ != mid_ )
    {
      res->rear_ = rear_->transformedPtr( tf );
    }
  if( front_ != mid_ )
    {
      res->front_ = front_->transformedPtr( tf );
    }
  return res;
}

Concrete::PathPoint3D *
Concrete::PathPoint2D::typed_to3D( ) const
{
  Concrete::PathPoint3D * res = new Concrete::PathPoint3D( *mid_ );
  if( rear_ != mid_ )
    {
      res->rear_ = new Concrete::Coords3D( *rear_ );
    }
  if( front_ != mid_ )
    {
      res->front_ = new Concrete::Coords3D( *front_ );
    }
  return res;
}


Concrete::PathPoint3D::PathPoint3D( const Concrete::PathPoint3D & orig )
  : mid_( new Concrete::Coords3D( *orig.mid_ ) )
{
  if( orig.rear_ != orig.mid_ )
    {
      rear_ = new Concrete::Coords3D( *orig.rear_ );
    }
  else
    {
      rear_ = mid_;
    }

  if( orig.front_ != orig.mid_ )
    {
      front_ = new Concrete::Coords3D( *orig.front_ );
    }
  else
    {
      front_ = mid_;
    }
}

Concrete::PathPoint3D::PathPoint3D( Concrete::Coords3D * mid )
  : rear_( mid ), mid_( mid ), front_( mid )
{ }

Concrete::PathPoint3D::PathPoint3D( const Concrete::Coords2D & mid )
  : rear_( new Concrete::Coords3D( mid ) ), mid_( rear_ ), front_( rear_ )
{ }

Concrete::PathPoint3D::PathPoint3D( Concrete::Length midx, Concrete::Length midy, Concrete::Length midz )
  : rear_( new Concrete::Coords3D( midx, midy, midz ) ), mid_( rear_ ), front_( rear_ )
{ }

Concrete::PathPoint3D::~PathPoint3D( )
{
  if( rear_ != 0 && rear_ != mid_ )
    {
      delete rear_;
    }
  if( mid_ != 0 )
    {
      delete mid_;
    }
  if( front_ != 0 && front_ != mid_ )
    {
      delete front_;
    }
}

Concrete::PathPoint3D *
Concrete::PathPoint3D::transformed( const Lang::Transform3D & tf ) const
{
  Concrete::PathPoint3D * res = new Concrete::PathPoint3D( mid_->transformedPtr( tf ) );
  if( rear_ != mid_ )
    {
      res->rear_ = rear_->transformedPtr( tf );
    }
  if( front_ != mid_ )
    {
      res->front_ = front_->transformedPtr( tf );
    }
  return res;
}

Concrete::PathPoint2D *
Concrete::PathPoint3D::make2D( Concrete::Length eyez ) const
{
  Concrete::PathPoint2D * res = new Concrete::PathPoint2D( mid_->make2D( eyez ) );
  if( rear_ != mid_ )
    {
      res->rear_ = rear_->make2D( eyez );
    }
  if( front_ != mid_ )
    {
      res->front_ = front_->make2D( eyez );
    }
  return res;  
}

Lang::PathPoint2D::PathPoint2D( const Lang::PathPoint2D & orig )
  : rear_( orig.rear_ ), mid_( orig.mid_ ), front_( orig.front_ )
{ }

DISPATCHIMPL( PathPoint2D );

Lang::PathPoint2D::PathPoint2D( RefCountPtr< const Lang::Coords2D > mid )
  : rear_( NullPtr< Lang::Value >( ) ), mid_( mid ), front_( NullPtr< Lang::Value >( ) )
{ }

Kernel::VariableHandle
Lang::PathPoint2D::getField( const char * fieldID, const RefCountPtr< const Lang::Value > & selfRef ) const
{
  if( strcmp( fieldID, "mid" ) == 0 )
    {
      return Kernel::VariableHandle( new Kernel::Variable( mid_ ) );
    }
  if( strcmp( fieldID, "rear" ) == 0 )
    {
      return Kernel::VariableHandle( new Kernel::Variable( rear_ ) );
    }
  if( strcmp( fieldID, "front" ) == 0 )
    {
      return Kernel::VariableHandle( new Kernel::Variable( front_ ) );
    }
  throw Exceptions::NonExistentMember( getTypeName( ), fieldID );
}

RefCountPtr< const Lang::Geometric2D >
Lang::PathPoint2D::transformed( const Lang::Transform2D & tf, const RefCountPtr< const Lang::Geometric2D > & self ) const
{
  throw Exceptions::MiscellaneousRequirement( strrefdup( "A pathpoint by itself cannot be transformed." ) );
}

RefCountPtr< const Lang::Geometric3D >
Lang::PathPoint2D::to3D( const RefCountPtr< const Lang::Geometric2D > & self ) const
{
  throw Exceptions::MiscellaneousRequirement( strrefdup( "A 2D pathpoint by itself cannot go 3D." ) );
}

void
Lang::PathPoint2D::gcMark( Kernel::GCMarkedSet & marked )
{
  {
    Lang::Value * tmp = const_cast< Lang::Value * >( rear_.getPtr( ) );
    if( tmp != 0 )
      {
	tmp->gcMark( marked );
      }
  }

  const_cast< Lang::Coords2D * >( mid_.getPtr( ) )->gcMark( marked );
  
  {
    Lang::Value * tmp = const_cast< Lang::Value * >( front_.getPtr( ) );
    if( tmp != 0 )
      {
	tmp->gcMark( marked );
      }
  }
}


RefCountPtr< const Lang::Class > Lang::PathPoint2D::TypeID( new Lang::SystemFinalClass( strrefdup( "PathPoint" ) ) );
TYPEINFOIMPL( PathPoint2D );


Lang::PathPoint3D::PathPoint3D( const Lang::PathPoint3D & orig )
  : rear_( orig.rear_ ), mid_( orig.mid_ ), front_( orig.front_ )
{ }

DISPATCHIMPL( PathPoint3D );

Lang::PathPoint3D::PathPoint3D( const RefCountPtr< const Lang::Coords3D > & mid )
  : rear_( NullPtr< Lang::Coords3D >( ) ), mid_( mid ), front_( NullPtr< Lang::Coords3D >( ) )
{ }

Kernel::VariableHandle
Lang::PathPoint3D::getField( const char * fieldID, const RefCountPtr< const Lang::Value > & selfRef ) const
{
  if( strcmp( fieldID, "mid" ) == 0 )
    {
      return Kernel::VariableHandle( new Kernel::Variable( mid_ ) );
    }
  if( strcmp( fieldID, "rear" ) == 0 )
    {
      return Kernel::VariableHandle( new Kernel::Variable( rear_ ) );
    }
  if( strcmp( fieldID, "front" ) == 0 )
    {
      return Kernel::VariableHandle( new Kernel::Variable( front_ ) );
    }
  throw Exceptions::NonExistentMember( getTypeName( ), fieldID );
}

void
Lang::PathPoint3D::elementaryJob( Lang::ElementaryPath3D * pth, Concrete::Coords3D * basePoint ) const
{
  Concrete::Coords3D * newMid = new Concrete::Coords3D( *basePoint, *mid_ );
  *basePoint = *newMid;
  Concrete::PathPoint3D * newPoint = new Concrete::PathPoint3D( newMid );

  if( rear_ != NullPtr< const Lang::Coords3D >( ) )
    {
      newPoint->rear_ = new Concrete::Coords3D( *basePoint, *rear_ );
    }
  
  if( front_ != NullPtr< const Lang::Coords3D >( ) )
    {
      newPoint->front_ = new Concrete::Coords3D( *basePoint, *front_ );
    }
  
  pth->push_back( newPoint );
}

RefCountPtr< const Lang::Geometric3D >
Lang::PathPoint3D::transformed( const Lang::Transform3D & tf, const RefCountPtr< const Lang::Geometric3D > & self ) const
{
  PathPoint3D * res = new Lang::PathPoint3D( RefCountPtr< const Lang::Coords3D >( mid_->transformedPtr( tf ) ) );

  if( rear_ != NullPtr< const Lang::Coords3D >( ) )
    {
      res->rear_ = RefCountPtr< const Lang::Coords3D >( rear_->transformedPtr( tf ) );
    }
  
  if( front_ != NullPtr< const Lang::Coords3D >( ) )
    {
      res->front_ = RefCountPtr< const Lang::Coords3D >( front_->transformedPtr( tf ) );
    }

  return RefCountPtr< const Lang::Geometric3D >( res );
}

RefCountPtr< const Lang::Geometric2D >
Lang::PathPoint3D::to2D( const Kernel::PassedDyn & dyn, const RefCountPtr< const Lang::Geometric3D > & self ) const
{
  Concrete::Length eyez = dyn->getEyeZ( );
  PathPoint2D * res = new Lang::PathPoint2D( mid_->make2D( eyez ) );

  if( rear_ != NullPtr< const Lang::Coords3D >( ) )
    {
      res->rear_ = rear_->make2D( eyez );
    }
  
  if( front_ != NullPtr< const Lang::Coords3D >( ) )
    {
      res->front_ = front_->make2D( eyez );
    }
  
  return RefCountPtr< const Lang::Geometric2D >( res );
}

void
Lang::PathPoint3D::gcMark( Kernel::GCMarkedSet & marked )
{
  {
    Lang::Coords3D * tmp = const_cast< Lang::Coords3D * >( rear_.getPtr( ) );
    if( tmp != 0 )
      {
	tmp->gcMark( marked );
      }
  }

  const_cast< Lang::Coords3D * >( mid_.getPtr( ) )->gcMark( marked );
  
  {
    Lang::Coords3D * tmp = const_cast< Lang::Coords3D * >( front_.getPtr( ) );
    if( tmp != 0 )
      {
	tmp->gcMark( marked );
      }
  }
}

RefCountPtr< const Lang::Class > Lang::PathPoint3D::TypeID( new Lang::SystemFinalClass( strrefdup( "PathPoint" ) ) );
TYPEINFOIMPL( PathPoint3D );


Lang::Path2D::Path2D( )
  : closed_( false )
{ }

DISPATCHIMPL( Path2D );

Lang::Path2D::~Path2D( )
{ }

void
Lang::Path2D::close( )
{
  closed_ = true;
}
bool
Lang::Path2D::isClosed( ) const
{
  return closed_;
}

RefCountPtr< const Lang::Geometric2D >
Lang::Path2D::transformed( const Lang::Transform2D & tf, const RefCountPtr< const Lang::Geometric2D > & self ) const
{
  typedef const Lang::Path2D ArgType;
  RefCountPtr< ArgType > selfTyped = self.down_cast< ArgType >( );
  if( selfTyped == NullPtr< ArgType >( ) )
    {
      throw Exceptions::InternalError( strrefdup( "Path2D::to3D: self was of unexpected type." ) );
    }
  
  return selfTyped->typed_transformed( tf );
}

RefCountPtr< const Lang::Geometric3D >
Lang::Path2D::to3D( const RefCountPtr< const Lang::Geometric2D > & self ) const
{
  typedef const Lang::Path2D ArgType;
  RefCountPtr< ArgType > selfTyped = self.down_cast< ArgType >( );
  if( selfTyped == NullPtr< ArgType >( ) )
    {
      throw Exceptions::InternalError( strrefdup( "Path2D::to3D: self was of unexpected type." ) );
    }
  
  return selfTyped->typed_to3D( selfTyped );
}

RefCountPtr< const Lang::Class > Lang::Path2D::TypeID( new Lang::SystemFinalClass( strrefdup( "SubPath" ) ) );
TYPEINFOIMPL( Path2D );


Lang::CompositePath2D::CompositePath2D( )
  : elementaryPath_( NullPtr< const ElementaryPath2D >( ) )
{ }

DISPATCHIMPL( CompositePath2D );

Lang::CompositePath2D::~CompositePath2D( )
{ }

Kernel::VariableHandle
Lang::CompositePath2D::getField( const char * fieldID, const RefCountPtr< const Lang::Value > & selfRef ) const
{
  computeElementaryPath( );
  return elementaryPath_->getField( fieldID, elementaryPath_ );
}

RefCountPtr< const Lang::Path2D >
Lang::CompositePath2D::typed_transformed( const Lang::Transform2D & tf ) const
{
  computeElementaryPath( );
  return elementaryPath_->typed_transformed( tf );
}

RefCountPtr< const Lang::Path3D >
Lang::CompositePath2D::typed_to3D( const RefCountPtr< const Lang::Path2D > & self ) const
{
  computeElementaryPath( );
  return RefCountPtr< const Lang::Path3D >( new Lang::Path2Din3D( elementaryPath_ ) );
}

void
Lang::CompositePath2D::writePath( ostream & os ) const
{
  computeElementaryPath( );
  elementaryPath_->writePath( os );
}

RefCountPtr< const Lang::ElementaryPath2D >
Lang::CompositePath2D::getElementaryPath( ) const
{
  computeElementaryPath( );
  return elementaryPath_;
}

void
Lang::CompositePath2D::show( std::ostream & os ) const
{
  os << "Composite subpath" ;
}


Lang::ClosedPath2D::ClosedPath2D( RefCountPtr< const Lang::Path2D > openPath )
  : openPath_( openPath )
{
  close( );
}

Lang::ClosedPath2D::~ClosedPath2D( )
{ }

DISPATCHIMPL( ClosedPath2D );

void
Lang::ClosedPath2D::elementaryJob( std::stack< const Lang::Value * > * nodeStack, Lang::ElementaryPath2D * pth ) const
{
  openPath_->elementaryJob( nodeStack, pth );
}

void
Lang::ClosedPath2D::gcMark( Kernel::GCMarkedSet & marked )
{
  const_cast< Lang::Path2D * >( openPath_.getPtr( ) )->gcMark( marked );
}


Lang::Connection2D::Connection2D( Kernel::ValueRef rear, Kernel::ValueRef front )
  : rear_( rear ), front_( front )
{ }

DISPATCHIMPL( Connection2D );

Lang::Connection2D::~Connection2D( )
{ }

void
Lang::Connection2D::elementaryJob( std::stack< const Lang::Value * > * nodeStack, Lang::ElementaryPath2D * pth ) const
{
  nodeStack->push( front_.getPtr( ) );
  nodeStack->push( rear_.getPtr( ) );
}

void
Lang::Connection2D::gcMark( Kernel::GCMarkedSet & marked )
{
  const_cast< Lang::Value * >( rear_.getPtr( ) )->gcMark( marked );
  const_cast< Lang::Value * >( front_.getPtr( ) )->gcMark( marked );
}


Lang::SinglePointPath2D::SinglePointPath2D( Kernel::ValueRef thePoint )
  : thePoint_( thePoint )
{ }

DISPATCHIMPL( SinglePointPath2D );

Lang::SinglePointPath2D::~SinglePointPath2D( )
{ }

void
Lang::SinglePointPath2D::elementaryJob( std::stack< const Lang::Value * > * nodeStack, Lang::ElementaryPath2D * pth ) const
{
  nodeStack->push( thePoint_.getPtr( ) );
}

void
Lang::SinglePointPath2D::gcMark( Kernel::GCMarkedSet & marked )
{
  const_cast< Lang::Value * >( thePoint_.getPtr( ) )->gcMark( marked );
}


Lang::HeadedPath2D::HeadedPath2D( Kernel::ValueRef rear, const RefCountPtr< const Lang::ElementaryPath2D > & bodyPath, Kernel::ValueRef front )
  : bodyPath_( new Lang::HeadedPath2D_helper( bodyPath ) ), rearPathPoint_( NullPtr< const Lang::Value >( ) ), frontPathPoint_( NullPtr< const Lang::Value >( ) )
{
  if( bodyPath->isClosed( ) )
    {
      throw Exceptions::InternalError( strrefdup( "Attempt to create closed but headed path." ) );
    }
  if( bodyPath->size( ) == 0 )
    {
      throw Exceptions::InternalError( strrefdup( "Attempt to create empty but headed path." ) );
    }

  if( bodyPath->size( ) == 1 )
    {
      Lang::ElementaryPath2D::const_iterator i = bodyPath->begin( );
      Lang::PathPoint2D * newPoint = new Lang::PathPoint2D( RefCountPtr< const Lang::Coords2D >( new Lang::Coords2D( *( (*i)->mid_ ) ) ) );
      newPoint->rear_ = rear;
      newPoint->front_ = front;
      rearPathPoint_ = Kernel::ValueRef( newPoint );

      return;
    }

  // We reach here if there's more than 1 point in bodyPath

  {
    Lang::ElementaryPath2D::const_reverse_iterator i = bodyPath->rbegin( );
    Lang::PathPoint2D * newPoint = new Lang::PathPoint2D( RefCountPtr< const Lang::Coords2D >( new Lang::Coords2D( *( (*i)->mid_ ) ) ) );
    if( (*i)->rear_ != 0 )
      {
	newPoint->rear_ = RefCountPtr< const Lang::Coords2D >( new Lang::Coords2D( *( (*i)->rear_ ) ) );
      }
    newPoint->front_ = front;
    frontPathPoint_ = Kernel::ValueRef( newPoint );
  }
  {
    Lang::ElementaryPath2D::const_iterator i = bodyPath->begin( );
    Lang::PathPoint2D * newPoint = new Lang::PathPoint2D( RefCountPtr< const Lang::Coords2D >( new Lang::Coords2D( *( (*i)->mid_ ) ) ) );
    if( (*i)->front_ != 0 )
      {
	newPoint->front_ = RefCountPtr< const Lang::Coords2D >( new Lang::Coords2D( *( (*i)->front_ ) ) );
      }
    newPoint->rear_ = rear;
    rearPathPoint_ = Kernel::ValueRef( newPoint );
  }
}

DISPATCHIMPL( HeadedPath2D );

Lang::HeadedPath2D::~HeadedPath2D( )
{ }

void
Lang::HeadedPath2D::elementaryJob( std::stack< const Lang::Value * > * nodeStack, Lang::ElementaryPath2D * pth ) const
{
  if( frontPathPoint_ == NullPtr< const Lang::Value >( ) )
    {
      // This means that there was just a single point in bodyPath

      nodeStack->push( rearPathPoint_.getPtr( ) );
      return;
    }
  else
    {    
      nodeStack->push( frontPathPoint_.getPtr( ) );
      nodeStack->push( bodyPath_.getPtr( ) );
      nodeStack->push( rearPathPoint_.getPtr( ) );
    }
}

void
Lang::HeadedPath2D::gcMark( Kernel::GCMarkedSet & marked )
{
  const_cast< Lang::HeadedPath2D_helper * >( bodyPath_.getPtr( ) )->gcMark( marked );
  const_cast< Lang::Value * >( rearPathPoint_.getPtr( ) )->gcMark( marked );
  const_cast< Lang::Value * >( frontPathPoint_.getPtr( ) )->gcMark( marked );
}


Lang::HeadedPath2D_helper::HeadedPath2D_helper( const RefCountPtr< const Lang::ElementaryPath2D > & bodyPath )
  : bodyPath_( bodyPath )
{ }

Lang::HeadedPath2D_helper::~HeadedPath2D_helper( )
{ }

void
Lang::HeadedPath2D_helper::elementaryJob( std::stack< const Lang::Value * > * nodeStack, Lang::ElementaryPath2D * pth ) const
{
  if( bodyPath_->size( ) <= 1 )
    {
      throw Exceptions::InternalError( strrefdup( "HeadedPath2D_helper::elementaryJob was unexpectedly called with a short path." ) );	  
    }
  
  Lang::ElementaryPath2D::const_iterator i = bodyPath_->begin( );
  ++i;
  Lang::ElementaryPath2D::const_iterator end = bodyPath_->end( );
  --end;
  for( ; i != end; ++i )
    {
      pth->push_back( new Concrete::PathPoint2D( **i ) );
    }
}

void
Lang::HeadedPath2D_helper::gcMark( Kernel::GCMarkedSet & marked )
{
  const_cast< Lang::ElementaryPath2D * >( bodyPath_.getPtr( ) )->gcMark( marked );
}



Lang::MultiPath2D::MultiPath2D( )
{ }

DISPATCHIMPL( MultiPath2D );

Lang::MultiPath2D::~MultiPath2D( )
{ }

Lang::MultiPath2D *
Lang::MultiPath2D::clone( ) const
{
  Lang::MultiPath2D * res = new Lang::MultiPath2D( );
  for( const_iterator i = begin( ); i != end( ); ++i )
    {
      res->push_back( *i );
    }
  return res;
}

RefCountPtr< const Lang::Class > Lang::MultiPath2D::TypeID( new Lang::SystemFinalClass( strrefdup( "Path" ) ) );
TYPEINFOIMPL( MultiPath2D );

void
Lang::MultiPath2D::show( std::ostream & os ) const
{
  os << "Path with " << size( ) << " subpaths" ;
}

RefCountPtr< const Lang::Geometric2D >
Lang::MultiPath2D::transformed( const Lang::Transform2D & tf, const RefCountPtr< const Lang::Geometric2D > & self ) const
{
  Lang::MultiPath2D * res = new Lang::MultiPath2D( );
  for( const_iterator i = begin( ); i != end( ); ++i )
    {
      res->push_back( (*i)->typed_transformed( tf ) );
    }
  return RefCountPtr< Lang::Geometric2D >( res );
}

RefCountPtr< const Lang::Geometric3D >
Lang::MultiPath2D::to3D( const RefCountPtr< const Lang::Geometric2D > & self ) const
{
  Lang::MultiPath3D * res = new Lang::MultiPath3D( );
  for( const_iterator i = begin( ); i != end( ); ++i )
    {
      res->push_back( (*i)->typed_to3D( *i ) );
    }
  return RefCountPtr< const Lang::Geometric3D >( res );
}

void
Lang::MultiPath2D::writePath( ostream & os ) const
{
  for( const_iterator i = begin( ); i != end( ); ++i )
    {
      (*i)->writePath( os );
    }
}

void
Lang::MultiPath2D::gcMark( Kernel::GCMarkedSet & marked )
{
  for( const_iterator i = begin( ); i != end( ); ++i )
    {
      const_cast< Path2D * >( i->getPtr( ) )->gcMark( marked );
    }
}


Lang::MultiPath3D::MultiPath3D( )
{ }

DISPATCHIMPL( MultiPath3D );

Lang::MultiPath3D::~MultiPath3D( )
{ }

Lang::MultiPath3D *
Lang::MultiPath3D::clone( ) const
{
  Lang::MultiPath3D * res = new Lang::MultiPath3D( );
  for( const_iterator i = begin( ); i != end( ); ++i )
    {
      res->push_back( *i );
    }
  return res;
}

RefCountPtr< const Lang::Class > Lang::MultiPath3D::TypeID( new Lang::SystemFinalClass( strrefdup( "path3D" ) ) );
TYPEINFOIMPL( MultiPath3D );

void
Lang::MultiPath3D::show( std::ostream & os ) const
{
  os << "3D path with " << size( ) << " subpaths" ;
}

RefCountPtr< const Lang::Geometric3D >
Lang::MultiPath3D::transformed( const Lang::Transform3D & tf, const RefCountPtr< const Lang::Geometric3D > & self ) const
{
  Lang::MultiPath3D * res = new Lang::MultiPath3D( );
  for( const_iterator i = begin( ); i != end( ); ++i )
    {
      res->push_back( (*i)->typed_transformed( tf ) );
    }
  return RefCountPtr< Lang::Geometric3D >( res );
}

RefCountPtr< const Lang::Geometric2D >
Lang::MultiPath3D::to2D( const Kernel::PassedDyn & dyn, const RefCountPtr< const Lang::Geometric3D > & self ) const
{
  Concrete::Length eyez = dyn->getEyeZ( );
  Lang::MultiPath2D * res = new Lang::MultiPath2D( );
  for( const_iterator i = begin( ); i != end( ); ++i )
    {
      res->push_back( (*i)->make2D( eyez ) );
    }
  return RefCountPtr< const Lang::Geometric2D >( res );
}

void
Lang::MultiPath3D::gcMark( Kernel::GCMarkedSet & marked )
{
  for( const_iterator i = begin( ); i != end( ); ++i )
    {
      const_cast< Path3D * >( i->getPtr( ) )->gcMark( marked );
    }
}


Lang::Path3D::Path3D( )
  : closed_( false )
{ }

DISPATCHIMPL( Path3D );

Lang::Path3D::~Path3D( )
{ }

void
Lang::Path3D::close( )
{
  closed_ = true;
}
bool
Lang::Path3D::isClosed( ) const
{
  return closed_;
}

RefCountPtr< const Lang::Geometric3D >
Lang::Path3D::transformed( const Lang::Transform3D & tf, const RefCountPtr< const Lang::Geometric3D > & self ) const
{
  typedef const Lang::Path3D ArgType;
  RefCountPtr< ArgType > selfTyped = self.down_cast< ArgType >( );
  if( selfTyped == NullPtr< ArgType >( ) )
    {
      throw Exceptions::InternalError( strrefdup( "Path3D::to3D: self was of unexpected type." ) );
    }
  
  return selfTyped->typed_transformed( tf );
}

RefCountPtr< const Lang::Geometric2D >
Lang::Path3D::to2D( const Kernel::PassedDyn & dyn, const RefCountPtr< const Lang::Geometric3D > & self ) const
{
  Concrete::Length eyez = dyn->getEyeZ( );
  return this->make2D( eyez );
}

RefCountPtr< const Lang::Class > Lang::Path3D::TypeID( new Lang::SystemFinalClass( strrefdup( "Path3D" ) ) );
TYPEINFOIMPL( Path3D );


Lang::Path2Din3D::Path2Din3D( const RefCountPtr< const Lang::ElementaryPath2D > & elementaryPath2D )
  : elementaryPath2D_( elementaryPath2D )
{
  if( elementaryPath2D_->isClosed( ) )
    {
      close( );
    }
}

Lang::Path2Din3D::~Path2Din3D( )
{ }

RefCountPtr< const Lang::ElementaryPath2D >
Lang::Path2Din3D::make2D( Concrete::Length eyez ) const
{
  // Since this path has not been transformed, it's "already" in 2D.

  return elementaryPath2D_;
}

RefCountPtr< const Lang::Path3D >
Lang::Path2Din3D::typed_transformed( const Lang::Transform3D & tf ) const
{
  return elementaryPath2D_->elementaryTransformed( tf );
}

void
Lang::Path2Din3D::elementaryJob( std::stack< const Lang::Path3D * > * nodeStack, Lang::ElementaryPath3D * pth, Concrete::Coords3D * basePoint ) const
{
  for( Lang::ElementaryPath2D::const_iterator i = elementaryPath2D_->begin( ); i != elementaryPath2D_->end( ); ++i )
    {
      pth->push_back( (*i)->typed_to3D( ) );
    }
}

void
Lang::Path2Din3D::gcMark( Kernel::GCMarkedSet & marked )
{
  const_cast< Lang::ElementaryPath2D * >( elementaryPath2D_.getPtr( ) )->gcMark( marked );
}



Lang::ClosedPath3D::ClosedPath3D( RefCountPtr< const Lang::Path3D > openPath )
  : openPath_( openPath )
{
  close( );
}

Lang::ClosedPath3D::~ClosedPath3D( )
{ }

DISPATCHIMPL( ClosedPath3D );

void
Lang::ClosedPath3D::elementaryJob( std::stack< const Lang::Path3D * > * nodeStack, Lang::ElementaryPath3D * pth, Concrete::Coords3D * basePoint ) const
{
  openPath_->elementaryJob( nodeStack, pth, basePoint );
}

void
Lang::ClosedPath3D::gcMark( Kernel::GCMarkedSet & marked )
{
  const_cast< Lang::Path3D * >( openPath_.getPtr( ) )->gcMark( marked );
}


Lang::Connection3D::Connection3D( const RefCountPtr< const Lang::Path3D > & rear, const RefCountPtr< const Lang::Path3D > & front )
  : rear_( rear ), front_( front )
{ }

Lang::Connection3D::~Connection3D( )
{ }

void
Lang::Connection3D::elementaryJob( std::stack< const Lang::Path3D * > * nodeStack, Lang::ElementaryPath3D * pth, Concrete::Coords3D * basePoint ) const
{
  nodeStack->push( front_.getPtr( ) );
  nodeStack->push( rear_.getPtr( ) );
}

void
Lang::Connection3D::gcMark( Kernel::GCMarkedSet & marked )
{
  const_cast< Lang::Path3D * >( rear_.getPtr( ) )->gcMark( marked );
  const_cast< Lang::Path3D * >( front_.getPtr( ) )->gcMark( marked );
}

DISPATCHIMPL( Connection3D );


Lang::SinglePointPath3D::SinglePointPath3D( const RefCountPtr< const Lang::PathPoint3D > & thePoint )
  : thePoint_( thePoint )
{ }

Lang::SinglePointPath3D::SinglePointPath3D( const RefCountPtr< const Lang::Coords3D > & mid )
  : thePoint_( new Lang::PathPoint3D( mid ) )
{ }

DISPATCHIMPL( SinglePointPath3D );

Lang::SinglePointPath3D::~SinglePointPath3D( )
{ }

void
Lang::SinglePointPath3D::elementaryJob( std::stack< const Lang::Path3D * > * nodeStack, Lang::ElementaryPath3D * pth, Concrete::Coords3D * basePoint ) const
{
  thePoint_->elementaryJob( pth, basePoint );
}

void
Lang::SinglePointPath3D::gcMark( Kernel::GCMarkedSet & marked )
{
  const_cast< Lang::PathPoint3D * >( thePoint_.getPtr( ) )->gcMark( marked );
}


Lang::CompositePath3D::CompositePath3D( )
  : elementaryPath_( NullPtr< const ElementaryPath3D >( ) )
{ }

DISPATCHIMPL( CompositePath3D );

Lang::CompositePath3D::~CompositePath3D( )
{ }

Kernel::VariableHandle
Lang::CompositePath3D::getField( const char * fieldID, const RefCountPtr< const Lang::Value > & selfRef ) const
{
  computeElementaryPath( );
  return elementaryPath_->getField( fieldID, elementaryPath_ );
}

RefCountPtr< const Lang::Path3D >
Lang::CompositePath3D::typed_transformed( const Lang::Transform3D & tf ) const
{
  computeElementaryPath( );
  return elementaryPath_->typed_transformed( tf );
}

RefCountPtr< const Lang::ElementaryPath2D >
Lang::CompositePath3D::make2D( Concrete::Length eyez ) const
{
  computeElementaryPath( );
  return elementaryPath_->make2D( eyez );
}

RefCountPtr< const Lang::ElementaryPath3D >
Lang::CompositePath3D::getElementaryPath( ) const
{
  computeElementaryPath( );

  return elementaryPath_;
}

void
Lang::CompositePath3D::show( std::ostream & os ) const
{
  os << "Composite subpath in 3D" ;
}

void
Lang::CompositePath3D::computeElementaryPath( ) const
{
  if( elementaryPath_ != NullPtr< const Lang::ElementaryPath3D >( ) )
    {
      return;
    }
  Lang::ElementaryPath3D * pth = new Lang::ElementaryPath3D;

  Concrete::Coords3D basePoint( 0, 0, 0 );

  std::stack< const Lang::Path3D * > nodeStack;
  nodeStack.push( this );
  while( nodeStack.size( ) > 0 )
    {
      const Lang::Path3D * node = nodeStack.top( );
      nodeStack.pop( );
      node->elementaryJob( & nodeStack, pth, & basePoint );
    }

  if( closed_ )
    {
      pth->close( );
    }
  
  elementaryPath_ = RefCountPtr< const Lang::ElementaryPath3D >( pth );
}


Lang::HeadedPath3D::HeadedPath3D( Kernel::ValueRef rear, const RefCountPtr< const Lang::ElementaryPath3D > & bodyPath, Kernel::ValueRef front )
  : bodyPath_( new Lang::HeadedPath3D_helper( bodyPath ) ),
    rearPathPoint_( NullPtr< const Lang::SinglePointPath3D >( ) ),
    frontPathPoint_( NullPtr< const Lang::SinglePointPath3D >( ) )
{
  if( bodyPath->isClosed( ) )
    {
      throw Exceptions::InternalError( strrefdup( "Attempt to create closed but headed path." ) );
    }
  if( bodyPath->size( ) == 0 )
    {
      throw Exceptions::InternalError( strrefdup( "Attempt to create empty but headed path." ) );
    }
  if( bodyPath->size( ) == 1 )
    {
      Lang::ElementaryPath3D::const_iterator i = bodyPath->begin( );
      Lang::PathPoint3D * newPoint = new Lang::PathPoint3D( RefCountPtr< const Lang::Coords3D >( new Lang::Coords3D( *( (*i)->mid_ ) ) ) );
      newPoint->rear_ = rear;
      newPoint->front_ = front;
      rearPathPoint_ = RefCountPtr< const Lang::SinglePointPath3D >( new Lang::SinglePointPath3D( RefCountPtr< const Lang::PathPoint3D >( newPoint ) ) );
      
      return;
    }
  
  // We reach here if there's more than one point in the body.
      
  {
    Lang::ElementaryPath3D::const_reverse_iterator i = bodyPath->rbegin( );
    Lang::PathPoint3D * newPoint = new Lang::PathPoint3D( RefCountPtr< const Lang::Coords3D >( new Lang::Coords3D( *( (*i)->mid_ ) ) ) );
    if( (*i)->rear_ != 0 )
      {
	newPoint->rear_ = RefCountPtr< const Lang::Coords3D >( new Lang::Coords3D( *( (*i)->rear_ ) ) );
      }
    newPoint->front_ = front;
    frontPathPoint_ = RefCountPtr< const Lang::SinglePointPath3D >( new Lang::SinglePointPath3D( RefCountPtr< const Lang::PathPoint3D >( newPoint ) ) );
  }
  {
    Lang::ElementaryPath3D::const_iterator i = bodyPath->begin( );
    Lang::PathPoint3D * newPoint = new Lang::PathPoint3D( RefCountPtr< const Lang::Coords3D >( new Lang::Coords3D( *( (*i)->mid_ ) ) ) );
    if( (*i)->front_ != 0 )
      {
	newPoint->front_ = RefCountPtr< const Lang::Coords3D >( new Lang::Coords3D( *( (*i)->front_ ) ) );
      }
    newPoint->rear_ = rear;
    rearPathPoint_ = RefCountPtr< const Lang::SinglePointPath3D >( new Lang::SinglePointPath3D( RefCountPtr< const Lang::PathPoint3D >( newPoint ) ) );
  }
      
}

DISPATCHIMPL( HeadedPath3D );

Lang::HeadedPath3D::~HeadedPath3D( )
{ }

void
Lang::HeadedPath3D::elementaryJob( std::stack< const Lang::Path3D * > * nodeStack, Lang::ElementaryPath3D * pth, Concrete::Coords3D * basePoint ) const
{
  if( frontPathPoint_ == NullPtr< const Lang::SinglePointPath3D >( ) )
    {
      // This means that there was just a single point in bodyPath

      nodeStack->push( rearPathPoint_.getPtr( ) );
      return;
    }
  else
    {    
      nodeStack->push( frontPathPoint_.getPtr( ) );
      nodeStack->push( bodyPath_.getPtr( ) );
      nodeStack->push( rearPathPoint_.getPtr( ) );
    }
}
      
void
Lang::HeadedPath3D::gcMark( Kernel::GCMarkedSet & marked )
{
  const_cast< Lang::HeadedPath3D_helper * >( bodyPath_.getPtr( ) )->gcMark( marked );
  const_cast< Lang::SinglePointPath3D * >( rearPathPoint_.getPtr( ) )->gcMark( marked );
  const_cast< Lang::SinglePointPath3D * >( frontPathPoint_.getPtr( ) )->gcMark( marked );
}


Lang::HeadedPath3D_helper::HeadedPath3D_helper( const RefCountPtr< const Lang::ElementaryPath3D > & bodyPath )
  : bodyPath_( bodyPath )
{ }

Lang::HeadedPath3D_helper::~HeadedPath3D_helper( )
{ }

void
Lang::HeadedPath3D_helper::elementaryJob( std::stack< const Lang::Path3D * > * nodeStack, Lang::ElementaryPath3D * pth, Concrete::Coords3D * basePoint ) const
{
  if( bodyPath_->size( ) <= 1 )
    {
      throw Exceptions::InternalError( strrefdup( "HeadedPath3D_helper::elementaryJob was unexpectedly called with a short path." ) );	  
    }
  
  Lang::ElementaryPath3D::const_iterator i = bodyPath_->begin( );
  ++i;
  Lang::ElementaryPath3D::const_iterator end = bodyPath_->end( );
  --end;
  for( ; i != end; ++i )
    {
      pth->push_back( new Concrete::PathPoint3D( **i ) );
    }
}

void
Lang::HeadedPath3D_helper::gcMark( Kernel::GCMarkedSet & marked )
{
  const_cast< Lang::ElementaryPath3D * >( bodyPath_.getPtr( ) )->gcMark( marked );
}
