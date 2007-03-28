#include <cmath>

#include "MetaPDF_Helpers_decls.h"

#include "dynamicenvironment.h"
#include "hottypes.h"
#include "globals.h"
#include "metapdfcore.h"
#include "metapdfast.h"
#include "isnan.h"

#include <limits>

using namespace MetaPDF;


void
Kernel::SpecialUnitVariables::specialUnitService( Concrete::Length * d, double * a0, double * a1 )
{
  if( p0_ == 0 || p1_ == 0 )
    {
      throw Exceptions::InternalError( "The path points were not setup before calling specialUnitService" );
    }
  Concrete::Length x0;
  Concrete::Length y0;
  Concrete::Length x3;
  Concrete::Length y3;
  double ag0; /* global angles that will later be compared with aRef */
  double ag1;

  if( reverseDirection_ )
    {
      x0 = p1_->mid_->x_;
      y0 = p1_->mid_->y_;
      x3 = p0_->mid_->x_;
      y3 = p0_->mid_->y_;
      ag0 = p1_->rearAngle_;
      ag1 = p0_->frontAngle_;
    }
  else
    {
      x0 = p0_->mid_->x_;
      y0 = p0_->mid_->y_;
      x3 = p1_->mid_->x_;
      y3 = p1_->mid_->y_;
      ag0 = p0_->frontAngle_;
      ag1 = p1_->rearAngle_;
    }

  Concrete::Length dx = x3 - x0;
  Concrete::Length dy = y3 - y0;

  *d = hypotPhysical( dx, dy );
  
  double aRef = atan2( dy.offtype< 1, 0 >( ), dx.offtype< 1, 0 >( ) );  /* Angles will be relative to the angle pointing to the other mid point */

  if( p0_->frontState_ & Concrete::PathPoint2D::FREE_ANGLE )
    {
      throw Exceptions::InternalError( "Found a free angle in specialUnitService" );
    }
    
  if( p0_->rearState_ & Concrete::PathPoint2D::FREE_ANGLE )
    {
      throw Exceptions::InternalError( "Found a free angle in specialUnitService" );
    }

  double ar0 = aRef - ag0;
  if( ar0 < - M_PI )
    {
      do
	{
	  ar0 += 2 * M_PI;
	}
      while( ar0 < - M_PI );
    }
  else
    {
      while( ar0 > M_PI )
	{
	  ar0 -= 2 * M_PI;
	}
    }

  double ar1 = ag1 - aRef - M_PI;
  if( ar1 < - M_PI )
    {
      do
	{
	  ar1 += 2 * M_PI;
	}
      while( ar1 < - M_PI );
    }
  else
    {
      while( ar1 > M_PI )
	{
	  ar1 -= 2 * M_PI;
	}
    }

  if( ar0 < 0 )
    {
      ar0 = - ar0;
      ar1 = - ar1;
    }
  
  ar1 -= ar0;
  
  if( ar1 < - M_PI )
    {
      ar1 += 2 * M_PI;
    }
  else if( ar1 > M_PI )
    {
      ar1 -= 2 * M_PI;
    }

  *a0 = ar0;
  *a1 = ar1;
}

Kernel::SystemDynamicVariables::SystemDynamicVariables( )
  : graphicsState_( NullPtr< const Kernel::GraphicsState >( ) ),
    eyez_( std::numeric_limits< double >::signaling_NaN( ) ),
    defaultUnit_( NullPtr< const Kernel::PolarHandlePromise >( ) ),
    //    defaultDestination_( NullPtr< Kernel::Variable >( ) ),
    blendSpace_( NullPtr< const Lang::ColorSpace >( ) )
{ }

Kernel::SystemDynamicVariables::SystemDynamicVariables( const RefCountPtr< const Kernel::GraphicsState > & graphicsState )
  : graphicsState_( graphicsState ),
    facetState_( true ),
    textState_( true ),
    eyez_( 50 * 72 / 2.54 ), /* 50 cm */
    defaultUnit_( new Kernel::PolarHandleEmptyPromise( ) ),
    //    defaultDestination_( new Kernel::Variable( defaultDestination ) ),
    blendSpace_( Lang::THE_INHERITED_COLOR_SPACE )
{ }

Kernel::DynamicEnvironment::DynamicEnvironment( const RefCountPtr< const Kernel::GraphicsState > & graphicsState )
  : parent_( NullPtr< Kernel::DynamicEnvironment >( ) ), sysBindings_( new Kernel::SystemDynamicVariables( graphicsState ) ), specialBindings_( 0 ),
    contId_( 0 ), contVal_( NullPtr< Kernel::Continuation >( ) )
{ }

Kernel::DynamicEnvironment::DynamicEnvironment( RefCountPtr< Kernel::DynamicEnvironment > parent, const Lang::DynamicBindings & bindings )
  : parent_( NullPtr< Kernel::DynamicEnvironment >( ) ), sysBindings_( 0 ), specialBindings_( 0 ),
    contId_( 0 ), contVal_( NullPtr< Kernel::Continuation >( ) )
{
  bindings.bind( bindings_, & sysBindings_ );
  if( sysBindings_ != 0 &&
      sysBindings_->graphicsState_ != NullPtr< const Kernel::GraphicsState >( ) )
    {
      sysBindings_->graphicsState_ =
	RefCountPtr< const Kernel::GraphicsState >( new Kernel::GraphicsState( *(sysBindings_->graphicsState_),
									       *(parent->getGraphicsState( )) ) );
    }
  if( sysBindings_ != 0 &&
      sysBindings_->facetState_ != NullPtr< const Kernel::FacetState >( ) )
    {
      sysBindings_->facetState_ =
	RefCountPtr< const Kernel::FacetState >( new Kernel::FacetState( *(sysBindings_->facetState_),
									 *(parent->getFacetState( )) ) );
    }
  if( sysBindings_ != 0 &&
      sysBindings_->textState_ != NullPtr< const Kernel::TextState >( ) )
    {
      sysBindings_->textState_ =
	RefCountPtr< const Kernel::TextState >( new Kernel::TextState( *(sysBindings_->textState_),
								       *(parent->getTextState( )) ) );
    }
  parent_ = parent->selectParent( parent, bindings_ );
}

Kernel::DynamicEnvironment::DynamicEnvironment( RefCountPtr< Kernel::DynamicEnvironment > parent, const RefCountPtr< const Kernel::GraphicsState > & graphicsState )
  : parent_( parent ), sysBindings_( new Kernel::SystemDynamicVariables( ) ), specialBindings_( 0 ),
    contId_( 0 ), contVal_( NullPtr< Kernel::Continuation >( ) )
{
  sysBindings_->graphicsState_ = graphicsState;
}

Kernel::DynamicEnvironment::DynamicEnvironment( RefCountPtr< Kernel::DynamicEnvironment > parent, Kernel::SystemDynamicVariables * sysBindings )
  : parent_( parent ), sysBindings_( sysBindings ), specialBindings_( 0 ),
    contId_( 0 ), contVal_( NullPtr< Kernel::Continuation >( ) )
{ }

Kernel::DynamicEnvironment::DynamicEnvironment( RefCountPtr< Kernel::DynamicEnvironment > parent, Kernel::SpecialUnitVariables * specialBindings )
  : parent_( parent ), sysBindings_( 0 ), specialBindings_( specialBindings ),
    contId_( 0 ), contVal_( NullPtr< Kernel::Continuation >( ) )
{ }

Kernel::DynamicEnvironment::DynamicEnvironment( RefCountPtr< Kernel::DynamicEnvironment > parent, const char * contId, const Kernel::ContRef & contVal )
  : parent_( parent ), sysBindings_( 0 ), specialBindings_( 0 ),
    contId_( contId ), contVal_( contVal )
{ }

Kernel::DynamicEnvironment::~DynamicEnvironment( )
{
  if( sysBindings_ != 0 )
    {
      delete sysBindings_;
    }
  if( specialBindings_ != 0 )
    {
      delete specialBindings_;
    }
}

void
Kernel::DynamicEnvironment::tackOn( const KeyType & key, Kernel::EvalState * evalState, const RefCountPtr< const Lang::Value > & piece, const Ast::SourceLocation & callLoc )
{
  throw Exceptions::NotImplemented( "DynamicEnvironment::tackOn" );
//   MapType::iterator i = bindings_.find( key );
//   if( i == bindings_.end( ) )
//     {
//       if( isBaseEnvironment( ) )
// 	{
// 	  throw Exceptions::InternalError( "Key of dynamic variable was not found in dynamic environment." );
// 	}
//       return parent_->tackOn( key, evalState, piece, callLoc );
//     }
//   return i->second.first->tackOn( evalState, piece, evalState->dyn_, callLoc );
}

void
Kernel::DynamicEnvironment::lookup( const KeyType & key, Kernel::EvalState * evalState ) const
{
  MapType::const_iterator i = bindings_.find( key );
  if( i == bindings_.end( ) )
    {
      if( isBaseEnvironment( ) )
	{
	  throw NonLocalExit::DynamicBindingNotFound( );
	}
      return parent_->lookup( key, evalState );
    }

  Kernel::ContRef cont = evalState->cont_;
  cont->takeHandle( i->second.first,
		    evalState );
}

Kernel::VariableHandle
Kernel::DynamicEnvironment::getVarHandle( const KeyType & key ) const
{
  MapType::const_iterator i = bindings_.find( key );
  if( i == bindings_.end( ) )
    {
      if( isBaseEnvironment( ) )
	{
	  throw NonLocalExit::DynamicBindingNotFound( );
	}
      return parent_->getVarHandle( key );
    }
  return i->second.first;
}

RefCountPtr< Kernel::DynamicEnvironment >
Kernel::DynamicEnvironment::selectParent( RefCountPtr< Kernel::DynamicEnvironment > & self, const MapType & newBindings )
{
  if( parent_ == NullPtr< Kernel::DynamicEnvironment >( ) ||
      sysBindings_ != 0 ||
      specialBindings_ != 0 ||
      contId_ != 0 )
    {
      return self;
    }

  MapType::const_iterator hint = newBindings.begin( );
  for( MapType::const_iterator i = bindings_.begin( ); i != bindings_.end( ); ++i )
    {
      hint = newBindings.find( i->first );
      if( hint == newBindings.end( ) )
	{
	  return self;
	}
    }

  return parent_->selectParent( parent_, newBindings );
}

void
Kernel::DynamicEnvironment::gcMark( Kernel::GCMarkedSet & marked )
{
  for( MapType::iterator i = bindings_.begin( ); i != bindings_.end( ); ++i )
    {
      i->second.first->gcMark( marked );
    }
}


RefCountPtr< const Kernel::GraphicsState >
Kernel::DynamicEnvironment::getGraphicsState( ) const
{
  if( sysBindings_ == 0 ||
      sysBindings_->graphicsState_ == NullPtr< const Kernel::GraphicsState >( ) )
    {
      if( parent_ == NullPtr< Kernel::DynamicEnvironment >( ) )
	{
	  throw Exceptions::InternalError( "The graphics state was needed but not defined." );
	}
      return parent_->getGraphicsState( );
    }

  return sysBindings_->graphicsState_;
}

RefCountPtr< const Kernel::FacetState >
Kernel::DynamicEnvironment::getFacetState( ) const
{
  if( sysBindings_ == 0 ||
      sysBindings_->facetState_ == NullPtr< const Kernel::FacetState >( ) )
    {
      if( parent_ == NullPtr< Kernel::DynamicEnvironment >( ) )
	{
	  throw Exceptions::InternalError( "The facet state was needed but not defined." );
	}
      return parent_->getFacetState( );
    }

  return sysBindings_->facetState_;
}

RefCountPtr< const Kernel::TextState >
Kernel::DynamicEnvironment::getTextState( ) const
{
  if( sysBindings_ == 0 ||
      sysBindings_->textState_ == NullPtr< const Kernel::TextState >( ) )
    {
      if( parent_ == NullPtr< Kernel::DynamicEnvironment >( ) )
	{
	  throw Exceptions::InternalError( "The text state was needed but not defined." );
	}
      return parent_->getTextState( );
    }

  return sysBindings_->textState_;
}

Concrete::Length
Kernel::DynamicEnvironment::getEyeZ( ) const
{
  if( sysBindings_ == 0 ||
      IS_NAN( sysBindings_->eyez_ ) )
    {
      if( parent_ == NullPtr< Kernel::DynamicEnvironment >( ) )
	{
	  throw Exceptions::InternalError( "@eyez should allways be bound." );
	}
      return parent_->getEyeZ( );
    }

  return sysBindings_->eyez_;
}

RefCountPtr< const Kernel::PolarHandlePromise >
Kernel::DynamicEnvironment::getDefaultUnit( ) const
{
  if( sysBindings_ == 0 ||
      sysBindings_->defaultUnit_ == NullPtr< const Kernel::PolarHandlePromise >( ) )
    {
      if( parent_ == NullPtr< Kernel::DynamicEnvironment >( ) )
	{
	  throw Exceptions::InternalError( "The default unit should allways be defined." );
	}
      return parent_->getDefaultUnit( );
    }

  return sysBindings_->defaultUnit_;
}

// Kernel::VariableHandle &
// Kernel::DynamicEnvironment::getDefaultDestination( ) const
// {
//   if( sysBindings_ == 0 ||
//       sysBindings_->defaultDestination_ == NullPtr< Kernel::Variable >( ) )
//     {
//       if( parent_ == NullPtr< Kernel::DynamicEnvironment >( ) )
// 	{
// 	  throw Exceptions::InternalError( "The default destination should allways be defined." );
// 	}
//       return parent_->getDefaultDestination( );
//     }

//   return sysBindings_->defaultDestination_;
// }

Kernel::ContRef
Kernel::DynamicEnvironment::getEscapeContinuation( const char * id, const Ast::SourceLocation & loc ) const
{
  if( contId_ == 0 ||
      strcmp( contId_, id ) != 0 )
    {
      if( parent_ == NullPtr< Kernel::DynamicEnvironment >( ) )
	{
	  throw Exceptions::UndefinedEscapeContinuation( id, loc );
	}
      return parent_->getEscapeContinuation( id, loc );
    }

  return contVal_;
}

RefCountPtr< const Lang::ColorSpace >
Kernel::DynamicEnvironment::getBlendSpace( ) const
{
  if( sysBindings_ == 0 ||
      sysBindings_->blendSpace_ == NullPtr< const Lang::ColorSpace >( ) )
    {
      if( parent_ == NullPtr< Kernel::DynamicEnvironment >( ) )
	{
	  throw Exceptions::InternalError( "The blend space should allways be defined." );
	}
      return parent_->getBlendSpace( );
    }

  return sysBindings_->blendSpace_;
}

void
Kernel::DynamicEnvironment::specialUnitService( Concrete::Length * d, double * a0, double * a1 )
{
  if( specialBindings_ != 0 )
    {
      specialBindings_->specialUnitService( d, a0, a1 );
    }
  else
    {
      if( parent_ == NullPtr< Kernel::DynamicEnvironment >( ) )
	{
	  throw Exceptions::InternalError( "The special unit dynamic context was needed but not defined." );
	}
      parent_->specialUnitService( d, a0, a1 );
    }
}

bool
Kernel::DynamicEnvironment::isBaseEnvironment( ) const
{
  return parent_ == NullPtr< Kernel::DynamicEnvironment >( );
}


// Lang::DefaultDestinationBinding::DefaultDestinationBinding( const Ast::SourceLocation & loc, Kernel::VariableHandle & val )
//   : loc_( loc ), val_( val )
// { }

// Lang::DefaultDestinationBinding::~DefaultDestinationBinding( )
// { }

// void
// Lang::DefaultDestinationBinding::bind( MapType & bindings, Kernel::SystemDynamicVariables ** sysBindings ) const
// {
//   if( *sysBindings == 0 )
//     {
//       *sysBindings = new Kernel::SystemDynamicVariables( );
//       (*sysBindings)->defaultDestination_ = val_;
//       return;
//     }
  
//   if( (*sysBindings)->defaultDestination_ != NullPtr< Kernel::Variable >( ) )
//     {
//       throw Exceptions::MultipleDynamicBind( "@<<", loc_, Ast::THE_UNKNOWN_LOCATION );
//     }

//   (*sysBindings)->defaultDestination_ = val_;
// }

// void
// Lang::DefaultDestinationBinding::gcMark( Kernel::GCMarkedSet & marked )
// {
//   val_->gcMark( marked );
// }


// Kernel::DefaultDestinationDynamicVariableProperties::DefaultDestinationDynamicVariableProperties( const char * name )
//   : Kernel::DynamicVariableProperties( name )
// { }

// Kernel::DefaultDestinationDynamicVariableProperties::~DefaultDestinationDynamicVariableProperties( )
// { }

// Kernel::VariableHandle
// Kernel::DefaultDestinationDynamicVariableProperties::fetch( const Kernel::PassedDyn & dyn ) const
// {
//   throw Exceptions::MiscellaneousRequirement( "The default destination cannot be evaluated as a variable." );
// }

// void
// Kernel::DefaultDestinationDynamicVariableProperties::makeBinding( Kernel::VariableHandle val, Ast::SourceLocation loc, Kernel::EvalState * evalState ) const
// {
//   if( ! val->isWarm( ) )
//     {
//       throw Exceptions::InternalError( "The default destination was not warm." );
//     }

//   Kernel::ContRef cont = evalState->cont_;
//   cont->takeValue( Kernel::ValueRef( new Lang::DefaultDestinationBinding( loc, val ) ),
// 		   evalState );
//   return;
// }


Lang::EyeZBinding::EyeZBinding( const Ast::SourceLocation & loc, Concrete::Length val )
  : loc_( loc ), val_( val )
{ }

Lang::EyeZBinding::~EyeZBinding( )
{ }

void
Lang::EyeZBinding::bind( MapType & bindings, Kernel::SystemDynamicVariables ** sysBindings ) const
{
  if( *sysBindings == 0 )
    {
      *sysBindings = new Kernel::SystemDynamicVariables( );
      (*sysBindings)->eyez_ = val_;
      return;
    }
  
  if( ! IS_NAN( (*sysBindings)->eyez_ ) )
    {
      throw Exceptions::MultipleDynamicBind( "< eye's z-coordinate >", loc_, Ast::THE_UNKNOWN_LOCATION );
    }

  (*sysBindings)->eyez_ = val_;
}

void
Lang::EyeZBinding::gcMark( Kernel::GCMarkedSet & marked )
{ }



Kernel::EyeZDynamicVariableProperties::EyeZDynamicVariableProperties( const char * _name )
  : Kernel::DynamicVariableProperties( _name )
{ }

Kernel::EyeZDynamicVariableProperties::~EyeZDynamicVariableProperties( )
{ }

Kernel::VariableHandle
Kernel::EyeZDynamicVariableProperties::fetch( const Kernel::PassedDyn & dyn ) const
{
  return Helpers::newValHandle( new Lang::Length( dyn->getEyeZ( ) ) );
}

void
Kernel::EyeZDynamicVariableProperties::makeBinding( Kernel::VariableHandle val, Ast::SourceLocation loc, Kernel::EvalState * evalState ) const
{
  try
    {
      RefCountPtr< const Lang::Length > len = val->tryVal< const Lang::Length >( );
      Kernel::ContRef cont = evalState->cont_;
      cont->takeValue( Kernel::ValueRef( new Lang::EyeZBinding( loc, len->get( ) ) ),
		       evalState );
      return;
    }
  catch( const NonLocalExit::NotThisType & ball )
    {
      /* never mind */
    }

  try
    {
      RefCountPtr< const Lang::Float > maybeInfinity = val->tryVal< const Lang::Float >( );
      if( maybeInfinity->val_ < HUGE_VAL )
	{
	  throw Exceptions::OutOfRange( loc, strrefdup( "The only float value allowed here is infinity." ) );
	}
      Kernel::ContRef cont = evalState->cont_;
      cont->takeValue( Kernel::ValueRef( new Lang::EyeZBinding( loc, Concrete::HUGE_LENGTH ) ),
		       evalState );
      return;
    }
  catch( const NonLocalExit::NotThisType & ball )
    {
      /* never mind */
    }

  throw Exceptions::TypeMismatch( loc, val->getUntyped( )->getTypeName( ), Helpers::typeSetString( Lang::Length::staticTypeName( ), Lang::Float::staticTypeName( ) ) );
}


Lang::DefaultUnitBinding::DefaultUnitBinding( const Ast::SourceLocation & loc, const RefCountPtr< const Kernel::PolarHandlePromise > & val )
  : loc_( loc ), val_( val )
{ }

Lang::DefaultUnitBinding::~DefaultUnitBinding( )
{ }

void
Lang::DefaultUnitBinding::bind( MapType & bindings, Kernel::SystemDynamicVariables ** sysBindings ) const
{
  if( *sysBindings == 0 )
    {
      *sysBindings = new Kernel::SystemDynamicVariables( );
      (*sysBindings)->defaultUnit_ = val_;
      return;
    }
  
  if( (*sysBindings)->defaultUnit_ != NullPtr< const Kernel::PolarHandlePromise >( ) )
    {
      throw Exceptions::MultipleDynamicBind( "< default unit >", loc_, Ast::THE_UNKNOWN_LOCATION );
    }

  (*sysBindings)->defaultUnit_ = val_;
}

void
Lang::DefaultUnitBinding::gcMark( Kernel::GCMarkedSet & marked )
{
  val_->gcMark( marked );
}


Kernel::DefaultUnitDynamicVariableProperties::DefaultUnitDynamicVariableProperties( const char * _name )
  : Kernel::DynamicVariableProperties( _name )
{ }

Kernel::DefaultUnitDynamicVariableProperties::~DefaultUnitDynamicVariableProperties( )
{ }

Kernel::VariableHandle
Kernel::DefaultUnitDynamicVariableProperties::fetch( const Kernel::PassedDyn & dyn ) const
{
  throw Exceptions::MiscellaneousRequirement( "The default unit cannot be evaluated as a variable." );
}

void
Kernel::DefaultUnitDynamicVariableProperties::makeBinding( Kernel::VariableHandle val, Ast::SourceLocation loc, Kernel::EvalState * evalState ) const
{
  if( ! val->isThunk( ) )
    {
      throw Exceptions::InternalError( "The default unit handle was not a thunk." );
    }

  Kernel::ContRef cont = evalState->cont_;
  cont->takeValue( Kernel::ValueRef( new Lang::DefaultUnitBinding( loc, RefCountPtr< const Kernel::PolarHandlePromise >( new Kernel::PolarHandleTruePromise( val->copyThunk( ) ) ) ) ),
		   evalState );
  return;
}


Lang::BlendSpaceBinding::BlendSpaceBinding( const Ast::SourceLocation & loc, const RefCountPtr< const Lang::ColorSpace > & space )
  : loc_( loc ), space_( space )
{ }

Lang::BlendSpaceBinding::~BlendSpaceBinding( )
{ }

void
Lang::BlendSpaceBinding::bind( MapType & bindings, Kernel::SystemDynamicVariables ** sysBindings ) const
{
  if( *sysBindings == 0 )
    {
      *sysBindings = new Kernel::SystemDynamicVariables( );
      (*sysBindings)->blendSpace_ = space_;
      return;
    }
  
  if( (*sysBindings)->blendSpace_ != NullPtr< const Lang::ColorSpace >( ) )
    {
      throw Exceptions::MultipleDynamicBind( "< blend space >", loc_, Ast::THE_UNKNOWN_LOCATION );
    }

  (*sysBindings)->blendSpace_ = space_;
}

void
Lang::BlendSpaceBinding::gcMark( Kernel::GCMarkedSet & marked )
{ }



Kernel::BlendSpaceDynamicVariableProperties::BlendSpaceDynamicVariableProperties( const char * _name )
  : Kernel::DynamicVariableProperties( _name )
{ }

Kernel::BlendSpaceDynamicVariableProperties::~BlendSpaceDynamicVariableProperties( )
{ }

Kernel::VariableHandle
Kernel::BlendSpaceDynamicVariableProperties::fetch( const Kernel::PassedDyn & dyn ) const
{
  return Kernel::VariableHandle( new Kernel::Variable(  dyn->getBlendSpace( ) ) );
}

void
Kernel::BlendSpaceDynamicVariableProperties::makeBinding( Kernel::VariableHandle val, Ast::SourceLocation loc, Kernel::EvalState * evalState ) const
{
  RefCountPtr< const Lang::ColorSpace > space = val->getVal< const Lang::ColorSpace >( loc );
  if( ! space->isBlendable( ) )
    {
      throw Exceptions::OutOfRange( loc, strrefdup( "This color space cannot be used in blending." ) );
    }
  
  Kernel::ContRef cont = evalState->cont_;
  cont->takeValue( Kernel::ValueRef( new Lang::BlendSpaceBinding( loc, space ) ),
		   evalState );
}


Kernel::DynamicEnvironment::KeyType Kernel::DynamicEnvironment::nextKey( 0 );

Kernel::DynamicEnvironment::KeyType
Kernel::DynamicEnvironment::getFreshKey( )
{
  ++nextKey;
  return nextKey;
}



Lang::DynamicExpression::DynamicExpression( Kernel::PassedEnv env, Ast::Expression * expr )
  : env_( env ), expr_( expr )
{ }

Lang::DynamicExpression::~DynamicExpression( )
{ }

RefCountPtr< const Lang::Class > Lang::DynamicExpression::TypeID( new Lang::SystemFinalClass( strrefdup( "DynamicExpression" ) ) );
TYPEINFOIMPL( DynamicExpression );

void
Lang::DynamicExpression::eval( Kernel::EvalState * evalState ) const
{
  evalState->env_ = env_;
  evalState->expr_ = expr_;
}

void
Lang::DynamicExpression::gcMark( Kernel::GCMarkedSet & marked )
{
  env_->gcMark( marked );
}
