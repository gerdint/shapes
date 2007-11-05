#include <cmath>

#include "Shapes_Helpers_decls.h"

#include "shapestypes.h"
#include "shapesexceptions.h"
#include "astexpr.h"
#include "consts.h"
#include "globals.h"
#include "angleselect.h"
#include "astvar.h"
#include "astclass.h"
#include "shapescore.h"
#include "simplepdfo.h"
#include "isnan.h"
#include "pdffunctiontypes.h"

#include <limits>
#include <ctype.h>
#include <stack>

using namespace Shapes;
using namespace std;


Lang::DynamicBindings::DynamicBindings( )
{ }
Lang::DynamicBindings::~DynamicBindings( )
{ }

RefCountPtr< const Lang::Class > Lang::DynamicBindings::TypeID( new Lang::SystemFinalClass( strrefdup( "DynamicBindings" ) ) );
TYPEINFOIMPL( DynamicBindings );
DISPATCHIMPL( DynamicBindings );


Lang::DynamicBindingsPair::DynamicBindingsPair( const RefCountPtr< const Lang::DynamicBindings > & car, const RefCountPtr< const Lang::DynamicBindings > & cdr )
  : car_( car ), cdr_( cdr )
{ }

Lang::DynamicBindingsPair::~DynamicBindingsPair( )
{ }

void
Lang::DynamicBindingsPair::bind( MapType & bindings, Kernel::SystemDynamicVariables ** sysBindings ) const
{
  car_->bind( bindings, sysBindings );
  cdr_->bind( bindings, sysBindings );
}

void
Lang::DynamicBindingsPair::gcMark( Kernel::GCMarkedSet & marked )
{
  const_cast< Lang::DynamicBindings * >( car_.getPtr( ) )->gcMark( marked );
  const_cast< Lang::DynamicBindings * >( cdr_.getPtr( ) )->gcMark( marked );
}


Lang::UserDynamicBinding::UserDynamicBinding( const Kernel::DynamicEnvironmentKeyType & _key, const char * _id, const Ast::SourceLocation & _loc, const Kernel::VariableHandle & _var )
  : key_( _key ), id_( _id ), loc_( _loc ), var_( _var )
{ }

Lang::UserDynamicBinding::~UserDynamicBinding( )
{ }

void
Lang::UserDynamicBinding::bind( MapType & bindings, Kernel::SystemDynamicVariables ** sysBindings ) const
{
  MapType::iterator i = bindings.find( key_ );
  if( i != bindings.end( ) )
    {
      throw Exceptions::MultipleDynamicBind( id_, loc_, i->second.second );
    }
  bindings.insert( MapType::value_type( key_, MapType::value_type::second_type( var_, loc_ ) ) );
}

void
Lang::UserDynamicBinding::gcMark( Kernel::GCMarkedSet & marked )
{
  var_->gcMark( marked );
}


namespace Shapes
{
  namespace Kernel
  {

  class UserDynamicBindingContinuation : public Kernel::Continuation
  {
    Kernel::DynamicEnvironmentKeyType key_;
    const char * name_;
    Kernel::ContRef cont_;
  public:
    UserDynamicBindingContinuation( const Ast::SourceLocation & traceLoc, const Kernel::DynamicEnvironmentKeyType & key, const char * name, const Kernel::ContRef & cont )
      : Kernel::Continuation( traceLoc ), key_( key ), name_( name ), cont_( cont )
    { }
    virtual ~UserDynamicBindingContinuation( )
    { }
    virtual void takeHandle( Kernel::VariableHandle val, Kernel::EvalState * evalState, bool dummy ) const
    {
      std::cerr << "Warning: I changed from evalState->cont_ to this->cont_! in UserDynamicBindingContinuation::takeHandle" << std::endl ;
      cont_->takeValue( Kernel::ValueRef( new Lang::UserDynamicBinding( key_, name_, traceLoc_, val ) ),
			evalState );
    }
    virtual void backTrace( std::list< Kernel::Continuation::BackTraceElem > * trace ) const
    {
      trace->push_front( Kernel::Continuation::BackTraceElem( this, "make user dynamic binding" ) );
      cont_->backTrace( trace );
    }
    virtual void gcMark( Kernel::GCMarkedSet & marked )
    {
      cont_->gcMark( marked );
    }
  };

  }
}


Kernel::UserDynamicVariableProperties::UserDynamicVariableProperties( const char * name, const Kernel::DynamicEnvironmentKeyType & key, const RefCountPtr< const Lang::Function > & filter, const Kernel::VariableHandle & defaultVal )
  : DynamicVariableProperties( name ), key_( key ), filter_( filter ), defaultVal_( defaultVal )
{ }

Kernel::UserDynamicVariableProperties::~UserDynamicVariableProperties( )
{ }

Kernel::VariableHandle
Kernel::UserDynamicVariableProperties::fetch( const Kernel::PassedDyn & dyn ) const
{
  try
    {
      return dyn->getVarHandle( key_ );
    }
  catch( const NonLocalExit::DynamicBindingNotFound & ball )
    {
      return defaultVal_;
    }
}

void
Kernel::UserDynamicVariableProperties::makeBinding( Kernel::VariableHandle val, Ast::SourceLocation loc, Kernel::EvalState * evalState ) const
{
  if( filter_ == Lang::THE_IDENTITY )
    {
      Kernel::ContRef cont = evalState->cont_;
      cont->takeValue( Kernel::ValueRef( new Lang::UserDynamicBinding( key_, name_, loc, val ) ),
		       evalState );
      return;
    }
  
  evalState->cont_ = Kernel::ContRef( new Kernel::UserDynamicBindingContinuation( loc, key_, name_, evalState->cont_ ) );

  filter_->call( filter_, evalState, val, loc );
}


Kernel::UserDynamicStateProperties::UserDynamicStateProperties( const char * name, const Kernel::DynamicEnvironmentKeyType & key, const Kernel::PassedEnv & defaultStateEnv, Kernel::PassedDyn defaultStateDyn, Ast::StateReference * defaultState )
  : DynamicStateProperties( name ), key_( key ),
    defaultStateEnv_( defaultStateEnv ), defaultStateDyn_( defaultStateDyn ), defaultState_( defaultState )
{ }

Kernel::UserDynamicStateProperties::~UserDynamicStateProperties( )
{ }

Kernel::StateHandle
Kernel::UserDynamicStateProperties::fetch( const Kernel::PassedDyn & dyn ) const
{
  try
    {
      throw Exceptions::NotImplemented( "Fetching dynamic states" );
      //      return dyn->getStateHandle( key_ );
    }
  catch( const NonLocalExit::DynamicBindingNotFound & ball )
    {
      return defaultState_->getHandle( defaultStateEnv_, defaultStateDyn_ );
    }
}

void
Kernel::UserDynamicStateProperties::makeBinding( Kernel::StateHandle val, Ast::SourceLocation loc, Kernel::EvalState * evalState ) const
{
  throw Exceptions::NotImplemented( "Creation of dynamic state bindings" );
}



Lang::WidthBinding::WidthBinding( const Ast::SourceLocation & loc, Concrete::Length val )
  : loc_( loc ), val_( val )
{ }

Lang::WidthBinding::~WidthBinding( )
{ }

void
Lang::WidthBinding::bind( MapType & bindings, Kernel::SystemDynamicVariables ** sysBindings ) const
{
  if( *sysBindings == 0 )
    {
      *sysBindings = new Kernel::SystemDynamicVariables( );
      Kernel::GraphicsState * newState = new Kernel::GraphicsState( );
      newState->width_ = val_;
      (*sysBindings)->graphicsState_ = RefCountPtr< const Kernel::GraphicsState >( newState );
      return;
    }
  
  if( (*sysBindings)->graphicsState_ == NullPtr< const Kernel::GraphicsState >( ) )
    {
      Kernel::GraphicsState * newState = new Kernel::GraphicsState( );
      newState->width_ = val_;
      (*sysBindings)->graphicsState_ = RefCountPtr< const Kernel::GraphicsState >( newState );      
      return;
    }
  
  Kernel::GraphicsState * newState = new Kernel::GraphicsState( *((*sysBindings)->graphicsState_) );

  if( ! IS_NAN( newState->width_ ) )
    {
      throw Exceptions::MultipleDynamicBind( "< graphics state width >", loc_, Ast::THE_UNKNOWN_LOCATION );
    }

  newState->width_ = val_;
  (*sysBindings)->graphicsState_ = RefCountPtr< const Kernel::GraphicsState >( newState );      
}

void
Lang::WidthBinding::gcMark( Kernel::GCMarkedSet & marked )
{ }



Kernel::WidthDynamicVariableProperties::WidthDynamicVariableProperties( const char * _name )
  : Kernel::DynamicVariableProperties( _name )
{ }

Kernel::WidthDynamicVariableProperties::~WidthDynamicVariableProperties( )
{ }

Kernel::VariableHandle
Kernel::WidthDynamicVariableProperties::fetch( const Kernel::PassedDyn & dyn ) const
{
  RefCountPtr< const Kernel::GraphicsState > graphicsState = dyn->getGraphicsState( );
  return Kernel::VariableHandle( new Kernel::Variable( RefCountPtr< const Lang::Value >( new Lang::Length( graphicsState->width_ ) ) ) );
}

void
Kernel::WidthDynamicVariableProperties::makeBinding( Kernel::VariableHandle val, Ast::SourceLocation loc, Kernel::EvalState * evalState ) const
{
  try
    {
      RefCountPtr< const Lang::Length > len = val->tryVal< const Lang::Length >( );
      if( len->getScalar( ) < 0 )
	{
	  throw Exceptions::OutOfRange( loc, strrefdup( "The length must be non-negative." ) );
	}
      Kernel::ContRef cont = evalState->cont_;
      cont->takeValue( Kernel::ValueRef( new Lang::WidthBinding( loc, len->get( ) ) ),
		       evalState );
      return;
    }
  catch( const NonLocalExit::NotThisType & ball )
    {
      /* never mind */
    }

  try
    {
      RefCountPtr< const Lang::Void > dummy = val->tryVal< const Lang::Void >( );
      Kernel::ContRef cont = evalState->cont_;
      cont->takeValue( Kernel::ValueRef( new Lang::WidthBinding( loc, Concrete::Length( -1 ) ) ),
		       evalState );
      return;
    }
  catch( const NonLocalExit::NotThisType & ball )
    {
      /* never mind */
    }

  throw Exceptions::TypeMismatch( loc, val->getUntyped( )->getTypeName( ), Helpers::typeSetString( Lang::Length::staticTypeName( ), Lang::Void::staticTypeName( ) ) );
}


Lang::MiterLimitBinding::MiterLimitBinding( const Ast::SourceLocation & loc, Concrete::Length val )
  : loc_( loc ), val_( val )
{ }

Lang::MiterLimitBinding::~MiterLimitBinding( )
{ }

void
Lang::MiterLimitBinding::bind( MapType & bindings, Kernel::SystemDynamicVariables ** sysBindings ) const
{
  if( *sysBindings == 0 )
    {
      *sysBindings = new Kernel::SystemDynamicVariables( );
      Kernel::GraphicsState * newState = new Kernel::GraphicsState( );
      newState->miterLimit_ = val_;
      (*sysBindings)->graphicsState_ = RefCountPtr< const Kernel::GraphicsState >( newState );
      return;
    }
  
  if( (*sysBindings)->graphicsState_ == NullPtr< const Kernel::GraphicsState >( ) )
    {
      Kernel::GraphicsState * newState = new Kernel::GraphicsState( );
      newState->miterLimit_ = val_;
      (*sysBindings)->graphicsState_ = RefCountPtr< const Kernel::GraphicsState >( newState );      
      return;
    }
  
  Kernel::GraphicsState * newState = new Kernel::GraphicsState( *((*sysBindings)->graphicsState_) );

  if( ! IS_NAN( newState->miterLimit_ ) )
    {
      throw Exceptions::MultipleDynamicBind( "< graphics state miter limit >", loc_, Ast::THE_UNKNOWN_LOCATION );
    }

  newState->miterLimit_ = val_;
  (*sysBindings)->graphicsState_ = RefCountPtr< const Kernel::GraphicsState >( newState );      
}

void
Lang::MiterLimitBinding::gcMark( Kernel::GCMarkedSet & marked )
{ }



Kernel::MiterLimitDynamicVariableProperties::MiterLimitDynamicVariableProperties( const char * name )
  : Kernel::DynamicVariableProperties( name )
{ }

Kernel::MiterLimitDynamicVariableProperties::~MiterLimitDynamicVariableProperties( )
{ }

Kernel::VariableHandle
Kernel::MiterLimitDynamicVariableProperties::fetch( const Kernel::PassedDyn & dyn ) const
{
  RefCountPtr< const Kernel::GraphicsState > graphicsState = dyn->getGraphicsState( );
  return Kernel::VariableHandle( new Kernel::Variable( RefCountPtr< const Lang::Value >( new Lang::Length( graphicsState->miterLimit_ ) ) ) );
}

void
Kernel::MiterLimitDynamicVariableProperties::makeBinding( Kernel::VariableHandle val, Ast::SourceLocation loc, Kernel::EvalState * evalState ) const
{
  try
    {
      RefCountPtr< const Lang::Length > len = val->tryVal< const Lang::Length >( );
      if( len->getScalar( ) < 0 )
	{
	  throw Exceptions::OutOfRange( loc, strrefdup( "The length must be non-negative." ) );
	}
      Kernel::ContRef cont = evalState->cont_;
      cont->takeValue( Kernel::ValueRef( new Lang::MiterLimitBinding( loc, len->get( ) ) ),
		       evalState );
      return;
    }
  catch( const NonLocalExit::NotThisType & ball )
    {
      /* never mind */
    }

  try
    {
      RefCountPtr< const Lang::Void > dummy = val->tryVal< const Lang::Void >( );
      Kernel::ContRef cont = evalState->cont_;
      cont->takeValue( Kernel::ValueRef( new Lang::MiterLimitBinding( loc, Concrete::Length( -1 ) ) ),
		       evalState );
      return;
    }
  catch( const NonLocalExit::NotThisType & ball )
    {
      /* never mind */
    }

  throw Exceptions::TypeMismatch( loc, val->getUntyped( )->getTypeName( ), Helpers::typeSetString( Lang::Length::staticTypeName( ), Lang::Void::staticTypeName( ) ) );
}


Lang::CapStyle::CapStyle( const Lang::CapStyle::ValueType & cap )
  : cap_( cap )
{ }

RefCountPtr< const Lang::Class > Lang::CapStyle::TypeID( new Lang::SystemFinalClass( strrefdup( "CapStyle" ) ) );
TYPEINFOIMPL( CapStyle );


Lang::JoinStyle::JoinStyle( const Lang::JoinStyle::ValueType & join )
  : join_( join )
{ }

RefCountPtr< const Lang::Class > Lang::JoinStyle::TypeID( new Lang::SystemFinalClass( strrefdup( "JoinStyle" ) ) );
TYPEINFOIMPL( JoinStyle );


Lang::BlendMode::BlendMode( const Lang::BlendMode::ValueType & mode )
  : mode_( mode )
{ }

RefCountPtr< const Lang::Class > Lang::BlendMode::TypeID( new Lang::SystemFinalClass( strrefdup( "BlendMode" ) ) );
TYPEINFOIMPL( BlendMode );

std::map< Lang::BlendMode::ValueType, RefCountPtr< SimplePDF::PDF_Object > > Lang::BlendMode::resourceMap;

void
Lang::BlendMode::applyGraphicsState( std::ostream & os, SimplePDF::PDF_Resources * resources, const Lang::BlendMode::ValueType & mode )
{
  typedef typeof resourceMap MapType;
  MapType::const_iterator i = resourceMap.find( mode );
  if( i != resourceMap.end( ) )
    {
      os << resources->nameofGraphicsState( i->second ) << " gs " ;
    }
  else
    {
      RefCountPtr< SimplePDF::PDF_Dictionary > dic;
      (*dic)[ "Type" ] = SimplePDF::PDF_out::newName( "ExtGState" );

      RefCountPtr< SimplePDF::PDF_Object > indirection = Kernel::the_pdfo->indirect( dic );
      resourceMap.insert( MapType::value_type( mode, indirection ) );
      switch( mode )
	{
	case NORMAL:
	  (*dic)[ "BM" ] = SimplePDF::PDF_out::newName( "Normal" );
	  break;
	case MULTIPLY:
	  (*dic)[ "BM" ] = SimplePDF::PDF_out::newName( "Multiply" );
	  break;
	case SCREEN:
	  (*dic)[ "BM" ] = SimplePDF::PDF_out::newName( "Screen" );
	  break;
	case OVERLAY:
	  (*dic)[ "BM" ] = SimplePDF::PDF_out::newName( "Overlay" );
	  break;
	case DARKEN:
	  (*dic)[ "BM" ] = SimplePDF::PDF_out::newName( "Darken" );
	  break;
	case LIGHTEN:
	  (*dic)[ "BM" ] = SimplePDF::PDF_out::newName( "Lighten" );
	  break;
	case COLOR_DODGE:
	  (*dic)[ "BM" ] = SimplePDF::PDF_out::newName( "ColorDodge" );
	  break;
	case COLOR_BURN:
	  (*dic)[ "BM" ] = SimplePDF::PDF_out::newName( "ColorBurn" );
	  break;
	case HARD_LIGHT:
	  (*dic)[ "BM" ] = SimplePDF::PDF_out::newName( "HardLight" );
	  break;
	case SOFT_LIGHT:
	  (*dic)[ "BM" ] = SimplePDF::PDF_out::newName( "SoftLight" );
	  break;
	case DIFFERENCE:
	  (*dic)[ "BM" ] = SimplePDF::PDF_out::newName( "Difference" );
	  break;
	case EXCLUSION:
	  (*dic)[ "BM" ] = SimplePDF::PDF_out::newName( "Exclusion" );
	  break;
	case HUE:
	  (*dic)[ "BM" ] = SimplePDF::PDF_out::newName( "Hue" );
	  break;
	case SATURATION:
	  (*dic)[ "BM" ] = SimplePDF::PDF_out::newName( "Saturation" );
	  break;
	case COLOR:
	  (*dic)[ "BM" ] = SimplePDF::PDF_out::newName( "Color" );
	  break;
	case LUMINOSITY:
	  (*dic)[ "BM" ] = SimplePDF::PDF_out::newName( "Luminosity" );
	  break;
	case BLEND_SAME:
	  // We leave the graphics state dictionary empty!
	  break;
	default:
	  throw Exceptions::InternalError( strrefdup( "BlendMode switch out of range." ) );
	}
      os << resources->nameofGraphicsState( indirection ) << " gs " ;
    }
}


Kernel::GraphicsState::GraphicsState( )
  : strokingColor_( NullPtr< const Lang::Color >( ) ),
    nonStrokingColor_( NullPtr< const Lang::Color >( ) ),
    width_( std::numeric_limits< double >::signaling_NaN( ) ),
    cap_( Lang::CapStyle::CAP_UNDEFINED ),
    join_( Lang::JoinStyle::JOIN_UNDEFINED ),
    miterLimit_( std::numeric_limits< double >::signaling_NaN( ) ),
    dash_( NullPtr< const Lang::Dash >( ) ),
    blend_( Lang::BlendMode::BLEND_UNDEFINED ),
    alphaIsShape_( true ),
    strokingAlpha_( NullPtr< const Lang::Alpha >( ) ),
    nonStrokingAlpha_( NullPtr< const Lang::Alpha >( ) )
{ }

Kernel::GraphicsState::GraphicsState( const Kernel::GraphicsState & orig )
  : strokingColor_( orig.strokingColor_ ),
    nonStrokingColor_( orig.nonStrokingColor_ ),
    width_( orig.width_ ),
    cap_( orig.cap_ ),
    join_( orig.join_ ),
    miterLimit_( orig.miterLimit_ ),
    dash_( orig.dash_ ),
    blend_( orig.blend_ ),
    alphaIsShape_( orig.alphaIsShape_ ),
    strokingAlpha_( orig.strokingAlpha_ ),
    nonStrokingAlpha_( orig.nonStrokingAlpha_ )
{ }

Kernel::GraphicsState::GraphicsState( const Kernel::GraphicsState & newValues, const Kernel::GraphicsState & oldValues )
  : strokingColor_( oldValues.strokingColor_ ),
    nonStrokingColor_( oldValues.nonStrokingColor_ ),
    width_( oldValues.width_ ),
    cap_( oldValues.cap_ ),
    join_( oldValues.join_ ),
    miterLimit_( oldValues.miterLimit_ ),
    dash_( oldValues.dash_ ),
    blend_( oldValues.blend_ ),
    alphaIsShape_( oldValues.alphaIsShape_ ),
    strokingAlpha_( oldValues.strokingAlpha_ ),
    nonStrokingAlpha_( oldValues.nonStrokingAlpha_ )
{
  if( newValues.strokingColor_ != NullPtr< const Lang::Color >( ) )
    {
      strokingColor_ = newValues.strokingColor_;
    }
  if( newValues.nonStrokingColor_ != NullPtr< const Lang::Color >( ) )
    {
      nonStrokingColor_ = newValues.nonStrokingColor_;
    }
  if( ! IS_NAN( newValues.width_ ) )
    {
      width_ = newValues.width_;
    }
  if( newValues.cap_ != Lang::CapStyle::CAP_UNDEFINED )
    {
      cap_ = newValues.cap_;
    }
  if( newValues.join_ != Lang::JoinStyle::JOIN_UNDEFINED )
    {
      join_ = newValues.join_;
    }
  if( ! IS_NAN( newValues.miterLimit_ ) )
    {
      miterLimit_ = newValues.miterLimit_;
    }
  if( newValues.dash_ != NullPtr< const Lang::Dash >( ) )
    {
      dash_ = newValues.dash_;
    }
  if( newValues.blend_ != Lang::BlendMode::BLEND_UNDEFINED )
    {
      blend_ = newValues.blend_;
    }
  if( newValues.strokingAlpha_ != NullPtr< const Lang::Alpha >( ) )
    {
      strokingAlpha_ = newValues.strokingAlpha_;
      alphaIsShape_ = newValues.alphaIsShape_;
    }
  if( newValues.nonStrokingAlpha_ != NullPtr< const Lang::Alpha >( ) )
    {
      nonStrokingAlpha_ = newValues.nonStrokingAlpha_;
      alphaIsShape_ = newValues.alphaIsShape_;
    }
}

Kernel::GraphicsState::GraphicsState( bool setDefaults )
  : strokingColor_( Lang::THE_BLACK ),
    nonStrokingColor_( Lang::THE_BLACK ),
    width_( 1 ),
    cap_( Lang::CapStyle::CAP_BUTT ),
    join_( Lang::JoinStyle::JOIN_MITER ),
    miterLimit_( 10 ),
    dash_( Lang::THE_SOLID_DASH ),
    blend_( Lang::BlendMode::NORMAL ),
    alphaIsShape_( false ),
    strokingAlpha_( Lang::THE_OPAQUE ),
    nonStrokingAlpha_( Lang::THE_OPAQUE )
{
  if( ! setDefaults )
    {
      throw Exceptions::InternalError( strrefdup( "setDefaults must be true in GraphicsState::GraphicsState." ) );
    }
}

Kernel::GraphicsState::~GraphicsState( )
{ }

bool
Kernel::GraphicsState::synchStrokingColor( std::ostream & os, const GraphicsState * ref, SimplePDF::PDF_Resources * resources, bool force )
{
  if( force || strokingColor_ != ref->strokingColor_ )
    {
      if( ref->strokingColor_ == NullPtr< const Lang::Color >( ) )
	{
	  return false;
	}
      strokingColor_ = ref->strokingColor_;
      strokingColor_->setStroking( os );
      return true;
    }
  return false;
}

bool
Kernel::GraphicsState::synchNonStrokingColor( std::ostream & os, const GraphicsState * ref, SimplePDF::PDF_Resources * resources, bool force )
{
  if( force || nonStrokingColor_ != ref->nonStrokingColor_ )
    {
      if( ref->nonStrokingColor_ == NullPtr< const Lang::Color >( ) )
	{
	  return false;
	}
      nonStrokingColor_ = ref->nonStrokingColor_;
      nonStrokingColor_->setNonStroking( os );
      return true;
    }
  return false;
}

bool
Kernel::GraphicsState::synchStrokingAlpha( std::ostream & os, const GraphicsState * ref, SimplePDF::PDF_Resources * resources, bool force )
{
  if( force || strokingAlpha_ != ref->strokingAlpha_ || alphaIsShape_ != ref->strokingAlpha_->isShape_ )
    {
      if( ref->strokingAlpha_ == NullPtr< const Lang::Alpha >( ) )
	{
	  return false;
	}
      strokingAlpha_ = ref->strokingAlpha_;
      alphaIsShape_ = strokingAlpha_->isShape_;

      const SimplePDF::PDF_out::Version MIN_VERSION = SimplePDF::PDF_out::PDF_1_4;
      if( Kernel::the_pdfo->versionGreaterOrEqual( MIN_VERSION ) )
	{
	  Lang::Alpha::applyGraphicsState( os, resources, *strokingAlpha_, true );
	  return true;
	}
      else
	{
	  Kernel::the_pdfo->versionMessage( MIN_VERSION, "The graphics state stroking alpha setting was ignored." );
	}
    }
  return false;
}

bool
Kernel::GraphicsState::synchNonStrokingAlpha( std::ostream & os, const GraphicsState * ref, SimplePDF::PDF_Resources * resources, bool force )
{
  if( force || nonStrokingAlpha_ != ref->nonStrokingAlpha_ || alphaIsShape_ != ref->nonStrokingAlpha_->isShape_  )
    {
      if( ref->nonStrokingAlpha_ == NullPtr< const Lang::Alpha >( ) )
	{
	  return false;
	}
      nonStrokingAlpha_ = ref->nonStrokingAlpha_;
      alphaIsShape_ = nonStrokingAlpha_->isShape_;

      const SimplePDF::PDF_out::Version MIN_VERSION = SimplePDF::PDF_out::PDF_1_4;
      if( Kernel::the_pdfo->versionGreaterOrEqual( MIN_VERSION ) )
	{
	  Lang::Alpha::applyGraphicsState( os, resources, *nonStrokingAlpha_, false );
	  return true;
	}
      else
	{
	  Kernel::the_pdfo->versionMessage( MIN_VERSION, "The graphics state non-stroking alpha setting was ignored." );
	}
    }
  return false;
}

bool
Kernel::GraphicsState::synchWidth( std::ostream & os, const GraphicsState * ref, SimplePDF::PDF_Resources * resources, bool force )
{
  if( force || width_ != ref->width_ )
    {
      if( IS_NAN( ref->width_ ) ||
	  ref->width_ < Concrete::ZERO_LENGTH )
	{
	  return false;
	}
      width_ = ref->width_;
      os << width_.offtype< 1, 0 >( ) << " w " ;
      return true;
    }
  return false;
}

bool
Kernel::GraphicsState::synchCap( std::ostream & os, const GraphicsState * ref, SimplePDF::PDF_Resources * resources, bool force )
{
  if( force || cap_ != ref->cap_ )
    {
      if( ref->cap_ != Lang::CapStyle::CAP_UNDEFINED &&
	  ref->cap_ != Lang::CapStyle::CAP_SAME )
	{
	  cap_ = ref->cap_;
	  os << cap_ << " J " ;
	  return true;
	}
    }
  return false;
}

bool
Kernel::GraphicsState::synchJoin( std::ostream & os, const GraphicsState * ref, SimplePDF::PDF_Resources * resources, bool force )
{
  if( force || join_ != ref->join_ )
    {
      if( ref->join_ != Lang::JoinStyle::JOIN_UNDEFINED &&
	  ref->join_ != Lang::JoinStyle::JOIN_SAME )
	{
	  join_ = ref->join_;
	  os << join_ << " j " ;
	  return true;
	}
    }
  return false;
}

bool
Kernel::GraphicsState::synchMiterLimit( std::ostream & os, const GraphicsState * ref, SimplePDF::PDF_Resources * resources, bool force )
{
  if( force || miterLimit_ != ref->miterLimit_ )
    {
      if( IS_NAN( ref->miterLimit_ ) ||
	  ref->miterLimit_ < Concrete::ZERO_LENGTH )
	{
	  return false;
	}
      miterLimit_ = ref->miterLimit_;
      os << miterLimit_.offtype< 1, 0 >( ) << " M " ;
      return true;
    }
  return false;
}

bool
Kernel::GraphicsState::synchDash( std::ostream & os, const GraphicsState * ref, SimplePDF::PDF_Resources * resources, bool force )
{
  if( force || dash_ != ref->dash_ )
    {
      if( ref->dash_ == NullPtr< const Lang::Dash >( ) )
	{
	  return false;
	}
      dash_ = ref->dash_;
      dash_->setDash( os );
      return true;
    }
  return false;
}

bool
Kernel::GraphicsState::synchBlend( std::ostream & os, const GraphicsState * ref, SimplePDF::PDF_Resources * resources, bool force )
{
  if( force || blend_ != ref->blend_ )
    {
      if( ref->blend_ == Lang::BlendMode::BLEND_UNDEFINED )
	{
	  return false;
	}
      blend_ = ref->blend_;
      const SimplePDF::PDF_out::Version MIN_VERSION = SimplePDF::PDF_out::PDF_1_4;
      if( Kernel::the_pdfo->versionGreaterOrEqual( MIN_VERSION ) )
	{
	  Lang::BlendMode::applyGraphicsState( os, resources, blend_ );
	  return true;
	}
      else
	{
	  Kernel::the_pdfo->versionMessage( MIN_VERSION, "The graphics state blend mode setting was ignored." );
	}
    }
  return false;
}


bool
Kernel::GraphicsState::synchForStroke( std::ostream & os, const GraphicsState * ref, SimplePDF::PDF_Resources * resources, bool force )
{
  bool anyChange = false;
  anyChange = synchStrokingColor( os, ref, resources, force ) || anyChange;
  anyChange = synchStrokingAlpha( os, ref, resources, force ) || anyChange;
  anyChange = synchWidth( os, ref, resources, force ) || anyChange;
  anyChange = synchCap( os, ref, resources, force ) || anyChange;
  anyChange = synchJoin( os, ref, resources, force ) || anyChange;
  anyChange = synchMiterLimit( os, ref, resources, force ) || anyChange;
  anyChange = synchDash( os, ref, resources, force ) || anyChange;
  if( anyChange )
    {
      os << std::endl ;
    }
  return anyChange;
}

bool
Kernel::GraphicsState::synchForNonStroke( std::ostream & os, const GraphicsState * ref, SimplePDF::PDF_Resources * resources, bool force )
{
  bool anyChange = false;
  anyChange = synchNonStrokingColor( os, ref, resources, force ) || anyChange;
  anyChange = synchNonStrokingAlpha( os, ref, resources, force ) || anyChange;
  if( anyChange )
    {
      os << std::endl ;
    }
  return anyChange;
}

bool
Kernel::GraphicsState::synchStrokingColorWithNonStrokingColor( std::ostream & os, SimplePDF::PDF_Resources * resources, Concrete::Length width )
{
  bool anyChange = false;

  if( ! IS_NAN( width ) && width_ != width )
    {
      width_ = width;
      os << width_.offtype< 1, 0 >( ) << " w " ;
      anyChange = true;
    }

  if( strokingColor_ != nonStrokingColor_ )
    {
      strokingColor_ = nonStrokingColor_;
      strokingColor_->setStroking( os );
    }

  if( anyChange )
    {
      os << std::endl ;
    }
  return anyChange;
}


Lang::Alpha::Alpha( bool isShape, double a )
  : isShape_( isShape ), a_( a )
{ }

Lang::Alpha::~Alpha( )
{ }

RefCountPtr< const Lang::Class > Lang::Alpha::TypeID( new Lang::SystemFinalClass( strrefdup( "Alpha" ) ) );
TYPEINFOIMPL( Alpha );

void
Lang::Alpha::applyGraphicsState( std::ostream & os, SimplePDF::PDF_Resources * resources, const Lang::Alpha & self, bool isStroking )
{
  typedef typeof strokingShapeResourcemap MapType;
  MapType * resourceMap;
  if( self.isShape_ )
    {
      if( isStroking )
	{
	  resourceMap = & strokingShapeResourcemap;
	}
      else
	{
	  resourceMap = & nonStrokingShapeResourcemap;
	}
    }
  else
    {
      if( isStroking )
	{
	  resourceMap = & strokingOpacityResourcemap;
	}
      else
	{
	  resourceMap = & nonStrokingOpacityResourcemap;
	}
    }
  MapType::const_iterator i = resourceMap->find( self.a_ );
  if( i != resourceMap->end( ) )
    {
      os << resources->nameofGraphicsState( i->second ) << " gs " ;
    }
  else
    {
      RefCountPtr< SimplePDF::PDF_Dictionary > dic;
      (*dic)[ "Type" ] = SimplePDF::PDF_out::newName( "ExtGState" );
      RefCountPtr< SimplePDF::PDF_Object > indirection = Kernel::the_pdfo->indirect( dic );

      resourceMap->insert( MapType::value_type( self.a_, indirection ) );
      if( self.a_ >= 0 )
	{
	  (*dic)[ "AIS" ] = SimplePDF::PDF_out::newBoolean( self.isShape_ );
	  if( isStroking )
	    {
	      (*dic)[ "CA" ] = SimplePDF::PDF_out::newFloat( self.a_ );
	    }
	  else
	    {
	      (*dic)[ "ca" ] = SimplePDF::PDF_out::newFloat( self.a_ );
	    }
	}
      // If self.a_ < 0 we just leave the graphics state dictionary empty

      os << resources->nameofGraphicsState( indirection ) << " gs " ;
    }
}

std::map< double, RefCountPtr< SimplePDF::PDF_Object > > Lang::Alpha::strokingShapeResourcemap;
std::map< double, RefCountPtr< SimplePDF::PDF_Object > > Lang::Alpha::strokingOpacityResourcemap;
std::map< double, RefCountPtr< SimplePDF::PDF_Object > > Lang::Alpha::nonStrokingShapeResourcemap;
std::map< double, RefCountPtr< SimplePDF::PDF_Object > > Lang::Alpha::nonStrokingOpacityResourcemap;


Lang::AlphaBinding::AlphaBinding( const Ast::SourceLocation & loc, const RefCountPtr< const Lang::Alpha > & alpha, bool isStroking )
  : loc_( loc ), alpha_( alpha ), isStroking_( isStroking )
{ }

Lang::AlphaBinding::~AlphaBinding( )
{ }

void
Lang::AlphaBinding::bind( MapType & bindings, Kernel::SystemDynamicVariables ** sysBindings ) const
{
  if( *sysBindings == 0 ) {
      *sysBindings = new Kernel::SystemDynamicVariables( );
      Kernel::GraphicsState * newState = new Kernel::GraphicsState( );
      if( isStroking_ )
	{
	  newState->strokingAlpha_ = alpha_;
	}
      else
	{
	  newState->nonStrokingAlpha_ = alpha_;
	}
      newState->alphaIsShape_ = alpha_->isShape_;
      (*sysBindings)->graphicsState_ = RefCountPtr< const Kernel::GraphicsState >( newState );
      return;
    }
  
  if( (*sysBindings)->graphicsState_ == NullPtr< const Kernel::GraphicsState >( ) )
    {
      Kernel::GraphicsState * newState = new Kernel::GraphicsState( );
      if( isStroking_ )
	{
	  newState->strokingAlpha_ = alpha_;
	}
      else
	{
	  newState->nonStrokingAlpha_ = alpha_;
	}
      newState->alphaIsShape_ = alpha_->isShape_;
      (*sysBindings)->graphicsState_ = RefCountPtr< const Kernel::GraphicsState >( newState );      
      return;
    }
  
  Kernel::GraphicsState * newState = new Kernel::GraphicsState( *((*sysBindings)->graphicsState_) );

  if( isStroking_ )
    {
      if( newState->strokingAlpha_ != NullPtr< const Lang::Alpha >( ) )
	{
	  throw Exceptions::MultipleDynamicBind( "< graphics state stroking alpha >", loc_, Ast::THE_UNKNOWN_LOCATION );
	}
      newState->strokingAlpha_ = alpha_;
    }
  else
    {
      if( newState->nonStrokingAlpha_ != NullPtr< const Lang::Alpha >( ) )
	{
	  throw Exceptions::MultipleDynamicBind( "< graphics state non-stroking alpha >", loc_, Ast::THE_UNKNOWN_LOCATION );
	}
      newState->nonStrokingAlpha_ = alpha_;
    }
  newState->alphaIsShape_ = alpha_->isShape_;
  (*sysBindings)->graphicsState_ = RefCountPtr< const Kernel::GraphicsState >( newState );      
}

void
Lang::AlphaBinding::gcMark( Kernel::GCMarkedSet & marked )
{
  const_cast< Lang::Alpha * >( alpha_.getPtr( ) )->gcMark( marked );
}



Kernel::AlphaDynamicVariableProperties::AlphaDynamicVariableProperties( const char * name, bool isStroking )
  : Kernel::DynamicVariableProperties( name ), isStroking_( isStroking )
{ }

Kernel::AlphaDynamicVariableProperties::~AlphaDynamicVariableProperties( )
{ }

Kernel::VariableHandle
Kernel::AlphaDynamicVariableProperties::fetch( const Kernel::PassedDyn & dyn ) const
{
  RefCountPtr< const Kernel::GraphicsState > graphicsState = dyn->getGraphicsState( );
  if( isStroking_ )
    {
      return Kernel::VariableHandle( new Kernel::Variable( graphicsState->strokingAlpha_ ) );
    }
  return Kernel::VariableHandle( new Kernel::Variable( graphicsState->nonStrokingAlpha_ ) );
}

void
Kernel::AlphaDynamicVariableProperties::makeBinding( Kernel::VariableHandle val, Ast::SourceLocation loc, Kernel::EvalState * evalState ) const
{
  try
    {
      RefCountPtr< const Lang::Alpha > alpha = val->tryVal< const Lang::Alpha >( );
      Kernel::ContRef cont = evalState->cont_;
      cont->takeValue( Kernel::ValueRef( new Lang::AlphaBinding( loc, alpha, isStroking_ ) ),
		       evalState );
      return;
    }
  catch( const NonLocalExit::NotThisType & ball )
    {
      /* never mind */
    }

  try
    {
      RefCountPtr< const Lang::Void > dummy = val->tryVal< const Lang::Void >( );
      Kernel::ContRef cont = evalState->cont_;
      cont->takeValue( Kernel::ValueRef( new Lang::AlphaBinding( loc, RefCountPtr< const Lang::Alpha >( new Lang::Alpha( true, -1 ) ), isStroking_ ) ),
		       evalState );
      return;
    }
  catch( const NonLocalExit::NotThisType & ball )
    {
      /* never mind */
    }

  throw Exceptions::TypeMismatch( loc, val->getUntyped( )->getTypeName( ), Helpers::typeSetString( Lang::Alpha::staticTypeName( ), Lang::Void::staticTypeName( ) ) );
}


Lang::SoftMask::SoftMask( )
  : graphicsStateResource_( NullPtr< SimplePDF::PDF_Object >( ) )
{
  static size_t callCount = 0;
  ++callCount;
  if( callCount > 2 )
    {
      throw Exceptions::InternalError( strrefdup( "The None soft mask should be generated at most twice!" ) );
    }

  RefCountPtr< SimplePDF::PDF_Dictionary > dic;
  graphicsStateResource_ = Kernel::the_pdfo->indirect( dic );

  (*dic)[ "Type" ] = SimplePDF::PDF_out::newName( "ExtGState" );
  (*dic)[ "SMask" ] = SimplePDF::PDF_out::newName( "None" );
}

Lang::SoftMask::SoftMask( SubType subType, const RefCountPtr< const Lang::TransparencyGroup > & tpGroup, const RefCountPtr< const Lang::Color > & background, const RefCountPtr< const Lang::PDF_Function > & transfer )
  : graphicsStateResource_( NullPtr< SimplePDF::PDF_Object >( ) )
{
  RefCountPtr< SimplePDF::PDF_Dictionary > gsDic;
  graphicsStateResource_ = Kernel::the_pdfo->indirect( gsDic );

  RefCountPtr< SimplePDF::PDF_Dictionary > smDic;

  (*gsDic)[ "Type" ] = SimplePDF::PDF_out::newName( "ExtGState" );
  (*gsDic)[ "SMask" ] = smDic;
  (*smDic)[ "G" ] = tpGroup->getPDF_Object( );
  
  switch( subType )
  {
  case ALPHA:
    {
      (*smDic)[ "S" ] = SimplePDF::PDF_out::newName( "Alpha" );
      if( background != NullPtr< const Lang::Color >( ) )
	{
	  throw Exceptions::InternalError( strrefdup( "Attempt to create Alpha soft mask with BC." ) );
	}
    }
    break;
  case LUMINOSITY:
    {
      (*smDic)[ "S" ] = SimplePDF::PDF_out::newName( "Luminosity" );
      if( background != NullPtr< const Lang::Color >( ) )
	{
	  (*smDic)[ "BC" ] = background->componentVector( );
	  if( ! tpGroup->colorSpace( )->containsColor( background.getPtr( ) ) )
	  {
	    throw Exceptions::OutOfRange( "The background color is not in the transparency groups color space." );
	  }
	}
    }
    break;
  default:
    throw Exceptions::InternalError( strrefdup( "SoftMask::SoftMask:  Enum switch out of range." ) );
  }
  if( transfer != NullPtr< const Lang::PDF_Function >( ) )
    {
      if( ! transfer->matchesDimensions( 1, 1 ) )
	{
	  throw Exceptions::InternalError( strrefdup( "SoftMask::SoftMask:  The transfer function arity should be correct at this point." ) );
	}
      (*smDic)[ "TR" ] = transfer->getFunction( );
    }
}

Lang::SoftMask::~SoftMask( )
{ }

RefCountPtr< const Lang::Class > Lang::SoftMask::TypeID( new Lang::SystemFinalClass( strrefdup( "SoftMask" ) ) );
TYPEINFOIMPL( SoftMask );


Lang::Dash::Iterator::Iterator( RefCountPtr< std::list< Concrete::Length > > _pattern, double _scale, std::list< Concrete::Length >::const_iterator _i, bool _on, Concrete::Length _length )
  : mem_( _pattern ), begin_( _pattern->begin( ) ), end_( _pattern->end( ) ), scale_( _scale ), i_( _i ), on_( _on ), length_( _scale * _length )
{ }

Lang::Dash::Iterator &
Lang::Dash::Iterator::operator ++ ( )
{
  ++i_;
  if( i_ == end_ )
    {
      i_ = begin_;
      on_ = true;
    }
  else
    {
      on_ = ! on_;
    }
  length_ = scale_ * (*i_);
  return *this;
}

bool
Lang::Dash::Iterator::isOn( ) const
{
  return on_;
}

Concrete::Length
Lang::Dash::Iterator::getLength( ) const
{
  return length_;
}


Lang::Dash::Dash( )
  : pattern_( new list< Concrete::Length >( ) ), phase_( 0 ), scale_( 1 )
{ }

Lang::Dash::Dash( RefCountPtr< std::list< Concrete::Length > > _pattern, Concrete::Length _phase, double _scale )
  : pattern_( _pattern ), phase_( _phase ), scale_( _scale ), myLength_( 0 )
{
  for( list< Concrete::Length >::const_iterator i = pattern_->begin( ); i != pattern_->end( ); ++i )
    {
      myLength_ += (*i);
    }
}

Lang::Dash::Dash( RefCountPtr< std::list< Concrete::Length > > _pattern, Concrete::Length _phase, double _scale, Concrete::Length _length )
  : pattern_( _pattern ), phase_( _phase ), scale_( _scale ), myLength_( _length )
{ }

DISPATCHIMPL( Dash );

Lang::Dash::~Dash( )
{ }

RefCountPtr< const Lang::Class > Lang::Dash::TypeID( new Lang::SystemFinalClass( strrefdup( "Dash" ) ) );
TYPEINFOIMPL( Dash );


void
Lang::Dash::setDash( ostream & os ) const
{
  // Negative scale means that the dash shall not be altered
  if( scale_ < 0 )
    {
      return;
    }

  os << "[ " ;
  for( list< Concrete::Length >::const_iterator i = pattern_->begin( ); i != pattern_->end( ); ++i )
    {
      os << Concrete::Length::offtype( *i ) * scale_ << " " ;
    }
  os << "] " << Concrete::Length::offtype( phase_ ) * scale_ << " d " ;
}

RefCountPtr< SimplePDF::PDF_Vector >
Lang::Dash::getDashArray( ) const
{
  if( scale_ < 0 )
    {
      throw Exceptions::MiscellaneousRequirement( "The same-as-before dash cannot be represented in PDF." );
    }

  RefCountPtr< SimplePDF::PDF_Vector > res = RefCountPtr< SimplePDF::PDF_Vector >( new SimplePDF::PDF_Vector );
  for( list< Concrete::Length >::const_iterator i = pattern_->begin( ); i != pattern_->end( ); ++i )
    {
      res->vec.push_back( SimplePDF::PDF_out::newFloat( Concrete::Length::offtype( *i ) * scale_ ) );
    }
  
  return res;
}

RefCountPtr< const Lang::Dash >
Lang::Dash::scaled( double factor ) const
{
  return RefCountPtr< const Lang::Dash >( new Lang::Dash( pattern_, phase_, scale_ * factor, myLength_ ) );
}

RefCountPtr< const Lang::Dash >
Lang::Dash::shifted( Concrete::Length dist ) const
{
  return RefCountPtr< const Lang::Dash >( new Lang::Dash( pattern_, phase_ + dist / scale_, scale_, myLength_ ) );
}

Concrete::Length
Lang::Dash::length( ) const
{
  return myLength_ * scale_;
}

bool
Lang::Dash::isSolid( ) const
{
  return pattern_->size( ) == 0;
}

Lang::Dash::Iterator
Lang::Dash::begin( ) const
{
  Concrete::Length tmpPhase = modPhysical( phase_, myLength_ );
  if( tmpPhase < 0 )
    {
      tmpPhase += myLength_;
    }
  std::list< Concrete::Length >::const_iterator i = pattern_->begin( );
  bool on = true;
  while( tmpPhase >= *i )
    {
      tmpPhase -= *i;
      on = ! on;
      ++i;
    }
  return Lang::Dash::Iterator( pattern_, scale_, i, on, *i - tmpPhase );
}

Lang::Color::Color( )
{ }

DISPATCHIMPL( Color );

Lang::Color::~Color( )
{ }

RefCountPtr< const Lang::Class > Lang::Color::TypeID = NullPtr< const Lang::Class >( ); /* The value is set in main */

RefCountPtr< const char >
Lang::Color::staticTypeName( )
{
  return TypeID->getPrettyName( );
}


Lang::Gray::Gray( const Concrete::Gray & components )
  : components_( components )
{ }

DISPATCHIMPL( Gray );

Lang::Gray::~Gray( )
{ }

RefCountPtr< const Lang::Class > Lang::Gray::TypeID( new Lang::SystemFinalClass( strrefdup( "Gray" ) ) );
TYPEINFOIMPL( Gray );

void
Lang::Gray::setStroking( ostream & os ) const
{
  components_.setStroking( os );
}

void
Lang::Gray::setNonStroking( ostream & os ) const
{
  components_.setNonStroking( os );
}

RefCountPtr< SimplePDF::PDF_Vector >
Lang::Gray::componentVector( ) const
{
  return components_.componentVector( );
}


Lang::RGB::RGB( const Concrete::RGB & components )
  : components_( components )
{ }

DISPATCHIMPL( RGB );

Lang::RGB::~RGB( )
{ }

RefCountPtr< const Lang::Class > Lang::RGB::TypeID( new Lang::SystemFinalClass( strrefdup( "RGB" ) ) );
TYPEINFOIMPL( RGB );

void
Lang::RGB::setStroking( ostream & os ) const
{
  components_.setStroking( os );
}

void
Lang::RGB::setNonStroking( ostream & os ) const
{
  components_.setNonStroking( os );
}

RefCountPtr< SimplePDF::PDF_Vector >
Lang::RGB::componentVector( ) const
{
  return components_.componentVector( );
}


Lang::CMYK::CMYK( const Concrete::CMYK & components )
  : components_( components )
{ }

DISPATCHIMPL( CMYK );

Lang::CMYK::~CMYK( )
{ }

RefCountPtr< const Lang::Class > Lang::CMYK::TypeID( new Lang::SystemFinalClass( strrefdup( "CMYK" ) ) );
TYPEINFOIMPL( CMYK );

void
Lang::CMYK::setStroking( ostream & os ) const
{
  components_.setStroking( os );
}

void
Lang::CMYK::setNonStroking( ostream & os ) const
{
  components_.setNonStroking( os );
}

RefCountPtr< SimplePDF::PDF_Vector >
Lang::CMYK::componentVector( ) const
{
  return components_.componentVector( );
}


Lang::StrokingBinding::StrokingBinding( const Ast::SourceLocation & loc, RefCountPtr< const Lang::Color > color )
  : loc_( loc ), color_( color )
{ }

Lang::StrokingBinding::~StrokingBinding( )
{ }

void
Lang::StrokingBinding::bind( MapType & bindings, Kernel::SystemDynamicVariables ** sysBindings ) const
{
  if( *sysBindings == 0 )
    {
      *sysBindings = new Kernel::SystemDynamicVariables( );
      Kernel::GraphicsState * newState = new Kernel::GraphicsState( );
      newState->strokingColor_ = color_;
      (*sysBindings)->graphicsState_ = RefCountPtr< const Kernel::GraphicsState >( newState );
      return;
    }
  
  if( (*sysBindings)->graphicsState_ == NullPtr< const Kernel::GraphicsState >( ) )
    {
      Kernel::GraphicsState * newState = new Kernel::GraphicsState( );
      newState->strokingColor_ = color_;
      (*sysBindings)->graphicsState_ = RefCountPtr< const Kernel::GraphicsState >( newState );      
      return;
    }
  
  Kernel::GraphicsState * newState = new Kernel::GraphicsState( *((*sysBindings)->graphicsState_) );

  if( newState->strokingColor_ != NullPtr< const Lang::Color >( ) )
    {
      throw Exceptions::MultipleDynamicBind( "< graphics state stroking >", loc_, Ast::THE_UNKNOWN_LOCATION );
    }

  newState->strokingColor_ = color_;
  (*sysBindings)->graphicsState_ = RefCountPtr< const Kernel::GraphicsState >( newState );      
}

void
Lang::StrokingBinding::gcMark( Kernel::GCMarkedSet & marked )
{
  const_cast< Lang::Color * >( color_.getPtr( ) )->gcMark( marked );
}



Kernel::StrokingDynamicVariableProperties::StrokingDynamicVariableProperties( const char * name )
  : Kernel::DynamicVariableProperties( name )
{ }

Kernel::StrokingDynamicVariableProperties::~StrokingDynamicVariableProperties( )
{ }

Kernel::VariableHandle
Kernel::StrokingDynamicVariableProperties::fetch( const Kernel::PassedDyn & dyn ) const
{
  RefCountPtr< const Kernel::GraphicsState > graphicsState = dyn->getGraphicsState( );
  return Kernel::VariableHandle( new Kernel::Variable( graphicsState->strokingColor_ ) );
}

void
Kernel::StrokingDynamicVariableProperties::makeBinding( Kernel::VariableHandle val, Ast::SourceLocation loc, Kernel::EvalState * evalState ) const
{
  try
    {
      RefCountPtr< const Lang::Color > color = val->tryVal< const Lang::Color >( );
      Kernel::ContRef cont = evalState->cont_;
      cont->takeValue( Kernel::ValueRef( new Lang::StrokingBinding( loc, color ) ),
		       evalState );
      return;
    }
  catch( const NonLocalExit::NotThisType & ball )
    {
      /* never mind */
    }

  try
    {
      RefCountPtr< const Lang::Void > dummy = val->tryVal< const Lang::Void >( );
      Kernel::ContRef cont = evalState->cont_;
      cont->takeValue( Kernel::ValueRef( new Lang::StrokingBinding( loc, RefCountPtr< const Lang::Color >( new Lang::Gray( -1 ) ) ) ),
		       evalState );
      return;
    }
  catch( const NonLocalExit::NotThisType & ball )
    {
      /* never mind */
    }

  throw Exceptions::TypeMismatch( loc, val->getUntyped( )->getTypeName( ), Helpers::typeSetString( Lang::Color::staticTypeName( ), Lang::Void::staticTypeName( ) ) );
}


Lang::NonStrokingBinding::NonStrokingBinding( const Ast::SourceLocation & loc, RefCountPtr< const Lang::Color > color )
  : loc_( loc ), color_( color )
{ }

Lang::NonStrokingBinding::~NonStrokingBinding( )
{ }

void
Lang::NonStrokingBinding::bind( MapType & bindings, Kernel::SystemDynamicVariables ** sysBindings ) const
{
  if( *sysBindings == 0 )
    {
      *sysBindings = new Kernel::SystemDynamicVariables( );
      Kernel::GraphicsState * newState = new Kernel::GraphicsState( );
      newState->nonStrokingColor_ = color_;
      (*sysBindings)->graphicsState_ = RefCountPtr< const Kernel::GraphicsState >( newState );
      return;
    }
  
  if( (*sysBindings)->graphicsState_ == NullPtr< const Kernel::GraphicsState >( ) )
    {
      Kernel::GraphicsState * newState = new Kernel::GraphicsState( );
      newState->nonStrokingColor_ = color_;
      (*sysBindings)->graphicsState_ = RefCountPtr< const Kernel::GraphicsState >( newState );      
      return;
    }
  
  Kernel::GraphicsState * newState = new Kernel::GraphicsState( *((*sysBindings)->graphicsState_) );

  if( newState->nonStrokingColor_ != NullPtr< const Lang::Color >( ) )
    {
      throw Exceptions::MultipleDynamicBind( "< graphics state non-stroking >", loc_, Ast::THE_UNKNOWN_LOCATION );
    }

  newState->nonStrokingColor_ = color_;
  (*sysBindings)->graphicsState_ = RefCountPtr< const Kernel::GraphicsState >( newState );      
}

void
Lang::NonStrokingBinding::gcMark( Kernel::GCMarkedSet & marked )
{
  const_cast< Lang::Color * >( color_.getPtr( ) )->gcMark( marked );
}



Kernel::NonStrokingDynamicVariableProperties::NonStrokingDynamicVariableProperties( const char * name )
  : Kernel::DynamicVariableProperties( name )
{ }

Kernel::NonStrokingDynamicVariableProperties::~NonStrokingDynamicVariableProperties( )
{ }

Kernel::VariableHandle
Kernel::NonStrokingDynamicVariableProperties::fetch( const Kernel::PassedDyn & dyn ) const
{
  RefCountPtr< const Kernel::GraphicsState > graphicsState = dyn->getGraphicsState( );
  return Kernel::VariableHandle( new Kernel::Variable( graphicsState->nonStrokingColor_ ) );
}

void
Kernel::NonStrokingDynamicVariableProperties::makeBinding( Kernel::VariableHandle val, Ast::SourceLocation loc, Kernel::EvalState * evalState ) const
{
  try
    {
      RefCountPtr< const Lang::Color > color = val->tryVal< const Lang::Color >( );
      Kernel::ContRef cont = evalState->cont_;
      cont->takeValue( Kernel::ValueRef( new Lang::NonStrokingBinding( loc, color ) ),
		       evalState );
      return;
    }
  catch( const NonLocalExit::NotThisType & ball )
    {
      /* never mind */
    }

  try
    {
      RefCountPtr< const Lang::Void > dummy = val->tryVal< const Lang::Void >( );
      Kernel::ContRef cont = evalState->cont_;
      cont->takeValue( Kernel::ValueRef( new Lang::NonStrokingBinding( loc, RefCountPtr< const Lang::Color >( new Lang::Gray( -1 ) ) ) ),
		       evalState );
      return;
    }
  catch( const NonLocalExit::NotThisType & ball )
    {
      /* never mind */
    }

  throw Exceptions::TypeMismatch( loc, val->getUntyped( )->getTypeName( ), Helpers::typeSetString( Lang::Color::staticTypeName( ), Lang::Void::staticTypeName( ) ) );
}


Lang::DashBinding::DashBinding( const Ast::SourceLocation & loc, RefCountPtr< const Lang::Dash > dash )
  : loc_( loc ), dash_( dash )
{ }

Lang::DashBinding::~DashBinding( )
{ }

void
Lang::DashBinding::bind( MapType & bindings, Kernel::SystemDynamicVariables ** sysBindings ) const
{
  if( *sysBindings == 0 )
    {
      *sysBindings = new Kernel::SystemDynamicVariables( );
      Kernel::GraphicsState * newState = new Kernel::GraphicsState( );
      newState->dash_ = dash_;
      (*sysBindings)->graphicsState_ = RefCountPtr< const Kernel::GraphicsState >( newState );
      return;
    }
  
  if( (*sysBindings)->graphicsState_ == NullPtr< const Kernel::GraphicsState >( ) )
    {
      Kernel::GraphicsState * newState = new Kernel::GraphicsState( );
      newState->dash_ = dash_;
      (*sysBindings)->graphicsState_ = RefCountPtr< const Kernel::GraphicsState >( newState );      
      return;
    }
  
  Kernel::GraphicsState * newState = new Kernel::GraphicsState( *((*sysBindings)->graphicsState_) );

  if( newState->dash_ != NullPtr< const Lang::Dash >( ) )
    {
      throw Exceptions::MultipleDynamicBind( "< graphics state dash >", loc_, Ast::THE_UNKNOWN_LOCATION );
    }

  newState->dash_ = dash_;
  (*sysBindings)->graphicsState_ = RefCountPtr< const Kernel::GraphicsState >( newState );      
}

void
Lang::DashBinding::gcMark( Kernel::GCMarkedSet & marked )
{
  const_cast< Lang::Dash * >( dash_.getPtr( ) )->gcMark( marked );
}



Kernel::DashDynamicVariableProperties::DashDynamicVariableProperties( const char * _name )
  : Kernel::DynamicVariableProperties( _name )
{ }

Kernel::DashDynamicVariableProperties::~DashDynamicVariableProperties( )
{ }

Kernel::VariableHandle
Kernel::DashDynamicVariableProperties::fetch( const Kernel::PassedDyn & dyn ) const
{
  RefCountPtr< const Kernel::GraphicsState > graphicsState = dyn->getGraphicsState( );
  return Kernel::VariableHandle( new Kernel::Variable( graphicsState->dash_ ) );
}

void
Kernel::DashDynamicVariableProperties::makeBinding( Kernel::VariableHandle val, Ast::SourceLocation loc, Kernel::EvalState * evalState ) const
{
  try
    {
      RefCountPtr< const Lang::Dash > dash = val->tryVal< const Lang::Dash >( );
      Kernel::ContRef cont = evalState->cont_;
      cont->takeValue( Kernel::ValueRef( new Lang::DashBinding( loc, dash ) ),
		       evalState );
      return;
    }
  catch( const NonLocalExit::NotThisType & ball )
    {
      /* never mind */
    }

  try
    {
      RefCountPtr< const Lang::Void > dummy = val->tryVal< const Lang::Void >( );
      Kernel::ContRef cont = evalState->cont_;
      cont->takeValue( Kernel::ValueRef( new Lang::DashBinding( loc, RefCountPtr< const Lang::Dash >( new Lang::Dash( RefCountPtr< std::list< Concrete::Length > >( new std::list< Concrete::Length >( ) ), 0, -1 ) ) ) ),
		       evalState );
      return;
    }
  catch( const NonLocalExit::NotThisType & ball )
    {
      /* never mind */
    }

  throw Exceptions::TypeMismatch( loc, val->getUntyped( )->getTypeName( ), Helpers::typeSetString( Lang::Dash::staticTypeName( ), Lang::Void::staticTypeName( ) ) );
}



Lang::CapStyleBinding::CapStyleBinding( const Ast::SourceLocation & loc, const Lang::CapStyle::ValueType & cap )
  : loc_( loc ), cap_( cap )
{ }

Lang::CapStyleBinding::~CapStyleBinding( )
{ }

void
Lang::CapStyleBinding::bind( MapType & bindings, Kernel::SystemDynamicVariables ** sysBindings ) const
{
  if( *sysBindings == 0 ) {
      *sysBindings = new Kernel::SystemDynamicVariables( );
      Kernel::GraphicsState * newState = new Kernel::GraphicsState( );
      newState->cap_ = cap_;
      (*sysBindings)->graphicsState_ = RefCountPtr< const Kernel::GraphicsState >( newState );
      return;
    }
  
  if( (*sysBindings)->graphicsState_ == NullPtr< const Kernel::GraphicsState >( ) )
    {
      Kernel::GraphicsState * newState = new Kernel::GraphicsState( );
      newState->cap_ = cap_;
      (*sysBindings)->graphicsState_ = RefCountPtr< const Kernel::GraphicsState >( newState );      
      return;
    }
  
  Kernel::GraphicsState * newState = new Kernel::GraphicsState( *((*sysBindings)->graphicsState_) );

  if( newState->cap_ != Lang::CapStyle::CAP_UNDEFINED )
    {
      throw Exceptions::MultipleDynamicBind( "< graphics state cap >", loc_, Ast::THE_UNKNOWN_LOCATION );
    }

  newState->cap_ = cap_;
  (*sysBindings)->graphicsState_ = RefCountPtr< const Kernel::GraphicsState >( newState );      
}

void
Lang::CapStyleBinding::gcMark( Kernel::GCMarkedSet & marked )
{ }



Kernel::CapStyleDynamicVariableProperties::CapStyleDynamicVariableProperties( const char * name )
  : Kernel::DynamicVariableProperties( name )
{ }

Kernel::CapStyleDynamicVariableProperties::~CapStyleDynamicVariableProperties( )
{ }

Kernel::VariableHandle
Kernel::CapStyleDynamicVariableProperties::fetch( const Kernel::PassedDyn & dyn ) const
{
  RefCountPtr< const Kernel::GraphicsState > graphicsState = dyn->getGraphicsState( );
  return Helpers::newValHandle( new Lang::CapStyle( graphicsState->cap_ ) );
}

void
Kernel::CapStyleDynamicVariableProperties::makeBinding( Kernel::VariableHandle val, Ast::SourceLocation loc, Kernel::EvalState * evalState ) const
{
  try
    {
      RefCountPtr< const Lang::CapStyle > cap = val->tryVal< const Lang::CapStyle >( );
      Kernel::ContRef cont = evalState->cont_;
      cont->takeValue( Kernel::ValueRef( new Lang::CapStyleBinding( loc, cap->cap_ ) ),
		       evalState );
      return;
    }
  catch( const NonLocalExit::NotThisType & ball )
    {
      /* never mind */
    }

  try
    {
      RefCountPtr< const Lang::Void > dummy = val->tryVal< const Lang::Void >( );
      Kernel::ContRef cont = evalState->cont_;
      cont->takeValue( Kernel::ValueRef( new Lang::CapStyleBinding( loc, Lang::CapStyle::CAP_SAME ) ),
		       evalState );
      return;
    }
  catch( const NonLocalExit::NotThisType & ball )
    {
      /* never mind */
    }

  throw Exceptions::TypeMismatch( loc, val->getUntyped( )->getTypeName( ), Helpers::typeSetString( Lang::CapStyle::staticTypeName( ), Lang::Void::staticTypeName( ) ) );
}


Lang::JoinStyleBinding::JoinStyleBinding( const Ast::SourceLocation & loc, const Lang::JoinStyle::ValueType & join )
  : loc_( loc ), join_( join )
{ }

Lang::JoinStyleBinding::~JoinStyleBinding( )
{ }

void
Lang::JoinStyleBinding::bind( MapType & bindings, Kernel::SystemDynamicVariables ** sysBindings ) const
{
  if( *sysBindings == 0 ) {
      *sysBindings = new Kernel::SystemDynamicVariables( );
      Kernel::GraphicsState * newState = new Kernel::GraphicsState( );
      newState->join_ = join_;
      (*sysBindings)->graphicsState_ = RefCountPtr< const Kernel::GraphicsState >( newState );
      return;
    }
  
  if( (*sysBindings)->graphicsState_ == NullPtr< const Kernel::GraphicsState >( ) )
    {
      Kernel::GraphicsState * newState = new Kernel::GraphicsState( );
      newState->join_ = join_;
      (*sysBindings)->graphicsState_ = RefCountPtr< const Kernel::GraphicsState >( newState );      
      return;
    }
  
  Kernel::GraphicsState * newState = new Kernel::GraphicsState( *((*sysBindings)->graphicsState_) );

  if( newState->join_ != Lang::JoinStyle::JOIN_UNDEFINED )
    {
      throw Exceptions::MultipleDynamicBind( "< graphics state join >", loc_, Ast::THE_UNKNOWN_LOCATION );
    }

  newState->join_ = join_;
  (*sysBindings)->graphicsState_ = RefCountPtr< const Kernel::GraphicsState >( newState );      
}

void
Lang::JoinStyleBinding::gcMark( Kernel::GCMarkedSet & marked )
{ }



Kernel::JoinStyleDynamicVariableProperties::JoinStyleDynamicVariableProperties( const char * name )
  : Kernel::DynamicVariableProperties( name )
{ }

Kernel::JoinStyleDynamicVariableProperties::~JoinStyleDynamicVariableProperties( )
{ }

Kernel::VariableHandle
Kernel::JoinStyleDynamicVariableProperties::fetch( const Kernel::PassedDyn & dyn ) const
{
  RefCountPtr< const Kernel::GraphicsState > graphicsState = dyn->getGraphicsState( );
  return Helpers::newValHandle( new Lang::JoinStyle( graphicsState->join_ ) );
}

void
Kernel::JoinStyleDynamicVariableProperties::makeBinding( Kernel::VariableHandle val, Ast::SourceLocation loc, Kernel::EvalState * evalState ) const
{
  try
    {
      RefCountPtr< const Lang::JoinStyle > join = val->tryVal< const Lang::JoinStyle >( );
      Kernel::ContRef cont = evalState->cont_;
      cont->takeValue( Kernel::ValueRef( new Lang::JoinStyleBinding( loc, join->join_ ) ),
		       evalState );
      return;
    }
  catch( const NonLocalExit::NotThisType & ball )
    {
      /* never mind */
    }

  try
    {
      RefCountPtr< const Lang::Void > dummy = val->tryVal< const Lang::Void >( );
      Kernel::ContRef cont = evalState->cont_;
      cont->takeValue( Kernel::ValueRef( new Lang::JoinStyleBinding( loc, Lang::JoinStyle::JOIN_SAME ) ),
		       evalState );
      return;
    }
  catch( const NonLocalExit::NotThisType & ball )
    {
      /* never mind */
    }

  throw Exceptions::TypeMismatch( loc, val->getUntyped( )->getTypeName( ), Helpers::typeSetString( Lang::JoinStyle::staticTypeName( ), Lang::Void::staticTypeName( ) ) );
}


Lang::BlendModeBinding::BlendModeBinding( const Ast::SourceLocation & loc, const Lang::BlendMode::ValueType & blend )
  : loc_( loc ), blend_( blend )
{ }

Lang::BlendModeBinding::~BlendModeBinding( )
{ }

void
Lang::BlendModeBinding::bind( MapType & bindings, Kernel::SystemDynamicVariables ** sysBindings ) const
{
  if( *sysBindings == 0 ) {
      *sysBindings = new Kernel::SystemDynamicVariables( );
      Kernel::GraphicsState * newState = new Kernel::GraphicsState( );
      newState->blend_ = blend_;
      (*sysBindings)->graphicsState_ = RefCountPtr< const Kernel::GraphicsState >( newState );
      return;
    }
  
  if( (*sysBindings)->graphicsState_ == NullPtr< const Kernel::GraphicsState >( ) )
    {
      Kernel::GraphicsState * newState = new Kernel::GraphicsState( );
      newState->blend_ = blend_;
      (*sysBindings)->graphicsState_ = RefCountPtr< const Kernel::GraphicsState >( newState );      
      return;
    }
  
  Kernel::GraphicsState * newState = new Kernel::GraphicsState( *((*sysBindings)->graphicsState_) );

  if( newState->blend_ != Lang::BlendMode::BLEND_UNDEFINED )
    {
      throw Exceptions::MultipleDynamicBind( "< graphics state blend >", loc_, Ast::THE_UNKNOWN_LOCATION );
    }

  newState->blend_ = blend_;
  (*sysBindings)->graphicsState_ = RefCountPtr< const Kernel::GraphicsState >( newState );      
}

void
Lang::BlendModeBinding::gcMark( Kernel::GCMarkedSet & marked )
{ }



Kernel::BlendModeDynamicVariableProperties::BlendModeDynamicVariableProperties( const char * _name )
  : Kernel::DynamicVariableProperties( _name )
{ }

Kernel::BlendModeDynamicVariableProperties::~BlendModeDynamicVariableProperties( )
{ }

Kernel::VariableHandle
Kernel::BlendModeDynamicVariableProperties::fetch( const Kernel::PassedDyn & dyn ) const
{
  RefCountPtr< const Kernel::GraphicsState > graphicsState = dyn->getGraphicsState( );
  return Helpers::newValHandle( new Lang::BlendMode( graphicsState->blend_ ) );
}

void
Kernel::BlendModeDynamicVariableProperties::makeBinding( Kernel::VariableHandle val, Ast::SourceLocation loc, Kernel::EvalState * evalState ) const
{
  try
    {
      RefCountPtr< const Lang::BlendMode > blend = val->tryVal< const Lang::BlendMode >( );
      Kernel::ContRef cont = evalState->cont_;
      cont->takeValue( Kernel::ValueRef( new Lang::BlendModeBinding( loc, blend->mode_ ) ),
		       evalState );
      return;
    }
  catch( const NonLocalExit::NotThisType & ball )
    {
      /* never mind */
    }

  try
    {
      RefCountPtr< const Lang::Void > dummy = val->tryVal< const Lang::Void >( );
      Kernel::ContRef cont = evalState->cont_;
      cont->takeValue( Kernel::ValueRef( new Lang::BlendModeBinding( loc, Lang::BlendMode::BLEND_SAME ) ),
		       evalState );
      return;
    }
  catch( const NonLocalExit::NotThisType & ball )
    {
      /* never mind */
    }

  throw Exceptions::TypeMismatch( loc, val->getUntyped( )->getTypeName( ), Helpers::typeSetString( Lang::BlendMode::staticTypeName( ), Lang::Void::staticTypeName( ) ) );
}



Lang::ColorSpace::ColorSpace( )
{ }

Lang::ColorSpace::~ColorSpace( )
{ }

RefCountPtr< const Lang::Class > Lang::ColorSpace::TypeID( new Lang::SystemFinalClass( strrefdup( "ColorSpace" ) ) );
TYPEINFOIMPL( ColorSpace );



Lang::InheritedColorSpace::InheritedColorSpace( )
{ }

Lang::InheritedColorSpace::~InheritedColorSpace( )
{ }

RefCountPtr< SimplePDF::PDF_Name >
Lang::InheritedColorSpace::name( ) const
{
  throw Exceptions::InternalError( "InheritedColorSpace::name was invoked." );
}

size_t
Lang::InheritedColorSpace::numberOfComponents( ) const
{
  throw Exceptions::InternalError( "InheritedColorSpace::numerOfComponents was invoked." );
}

bool
Lang::InheritedColorSpace::containsColor( const Lang::Color * col ) const
{
  throw Exceptions::InternalError( "InheritedColorSpace::containsColor was invoked." );
}


Kernel::Auto_qQ::Auto_qQ( Kernel::GraphicsState * state, std::ostream & os, bool active )
  : state_( state ), enterState_( 0 ), os_( os ), activated_( false )
{
  if( active )
    {
      activate( );
    }
}

void
Kernel::Auto_qQ::activate( )
{
  if( activated_ )
    {
      throw Exceptions::InternalError( "Multiple activation of Auto_qQ." );
    }
  activated_ = true;
  enterState_ = new Kernel::GraphicsState( *state_ );
  os_ << "q" << std::endl ;
}

Kernel::Auto_qQ::~Auto_qQ( )
{
  if( activated_ )
    {
      *state_ = *enterState_;
      delete enterState_;
      os_ << "Q" << std::endl ;
    }
}
