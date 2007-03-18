#include <cmath>

#include "environment.h"
#include "metapdfexceptions.h"
#include "metapdfcore.h"
#include "consts.h"
#include "globals.h"
#include "metapdfvalue.h"
#include "classtypes.h"
#include "hottypes.h"
#include "continuations.h"
#include "statetypes.h"
#include "errorhandlers.h"

using namespace MetaPDF;
using namespace std;


size_t Kernel::Environment::createdCount = 0;
size_t Kernel::Environment::liveCount = 0;


template< class T >
class Binary
{
public:
  T val;
  Binary( const T & _val ) : val( _val ) { }
};

template< class T >
std::ostream &
operator << ( std::ostream & os, const Binary< T > & self )
{
  for( int i = 8*sizeof( T ) - 1; i >= 0; --i )
    {
      if( (( self.val & (T(1)<<i) )) != 0 )
	{
	  os << 1 ;
	}
      else
	{
	  os << '_' ;
	}
    }
  return os;
}



Kernel::Thunk::Thunk( Kernel::PassedEnv env, Kernel::PassedDyn dyn, Ast::Expression * expr )
  : env_( env ), dyn_( dyn ), expr_( expr ), forced_( false )
{ }

void
Kernel::Thunk::force( Kernel::EvalState * evalState, bool onlyOnce ) const
{
  if( onlyOnce )
    {
      if( forced_ )
	{
	  throw Exceptions::InternalError( "It's unwise to force a thunk twice!  It should be deleted right after the first force." );
	}
      forced_ = true;
    }

  evalState->expr_ = expr_;
  evalState->env_ = env_;
  evalState->dyn_ = dyn_;

  /* The continuation is not se by the thunk! */
}

Kernel::Thunk *
Kernel::Thunk::deepCopy( )
{
  return new Thunk( env_, dyn_, expr_ );
}


void
Kernel::Thunk::gcMark( Kernel::GCMarkedSet & marked )
{
  if( ! forced_ )
    {
      env_->gcMark( marked );
      dyn_->gcMark( marked );
    }
}

Ast::Expression *
Kernel::Thunk::getExpr( )
{
  return expr_;
}

void
Kernel::Thunk::printEnv( std::ostream & os ) const
{
  env_->print( os );
}


Kernel::Variable::Variable( const RefCountPtr< const Lang::Value > & val )
  : thunk_( 0 ), val_( val ), state_( Kernel::Variable::COLD )
{ }

Kernel::Variable::Variable( Kernel::Thunk * thunk )
  : thunk_( thunk ), val_( NullPtr< const Lang::Value >( ) ), state_( Kernel::Variable::THUNK )
{ }

Kernel::Variable::~Variable( )
{
  if( thunk_ != 0 )
    {
      delete thunk_;
    }
}

Kernel::State::State( )
  : alive_( true )
{ }

Kernel::State::~State( )
{ }

void
Kernel::State::tackOn( Kernel::EvalState * evalState, const RefCountPtr< const Lang::Value > & piece, const Kernel::PassedDyn & dyn, const Ast::SourceLocation & callLoc )
{
  if( ! alive_ )
    {
      throw Exceptions::DeadStateAccess( );      
    }
  this->tackOnImpl( evalState, piece, dyn, callLoc );
}

void
Kernel::State::peek( Kernel::EvalState * evalState, const Ast::SourceLocation & callLoc )
{
  if( ! alive_ )
    {
      throw Exceptions::DeadStateAccess( );      
    }
  this->peekImpl( evalState, callLoc );
}

void
Kernel::State::freeze( Kernel::EvalState * evalState, const Ast::SourceLocation & callLoc )
{
  if( ! alive_ )
    {
      throw Exceptions::DeadStateAccess( );      
    }
  /* It would make sense to have an intermediate state here.
   */
  alive_ = false;

  // Perhaps, it would be smart to use a continuation to erase the implementation once it is used...
  //  evalState->cont_ = Kernel::ContRef( new Kernel::StmtStoreVariableContinuation( selfRef, evalState->cont_, callLoc ) );

  this->freezeImpl( evalState, callLoc );
}

bool
Kernel::State::isAlive( ) const
{
  return alive_;
}


void
Kernel::Variable::force( Kernel::VariableHandle & selfRef, Kernel::EvalState * evalState ) const
{
  if( state_ == Kernel::Variable::THUNK )
    {
      state_ = Kernel::Variable::FORCING;
      evalState->cont_ = Kernel::ContRef( new Kernel::StoreVariableContinuation( selfRef, evalState->cont_, thunk_->getExpr( )->loc( ) ) );
      thunk_->force( evalState );
      delete thunk_;
      thunk_ = 0;
      return;
    }
  if( val_ == NullPtr< const Lang::Value >( ) )
    {
      if( state_ == Kernel::Variable::FORCING )
	{
	  throw Exceptions::MiscellaneousRequirement( "Cyclic forcing." );
	}
      throw Exceptions::InternalError( "Force failed.  No value, not forcing." );
    }
  Kernel::ContRef cont = evalState->cont_;
  cont->takeHandle( selfRef, evalState );
}

RefCountPtr< const Lang::Value > &
Kernel::Variable::getUntyped( ) const
{
  if( val_ == NullPtr< const Lang::Value >( ) )
    {
      throw Exceptions::InternalError( "The value is not ready to be get without continuation." );
    }
  return val_;
}


bool
Kernel::Variable::isThunk( ) const
{
  return ( state_ & Kernel::Variable::FORCING ) != 0;
}

Kernel::Thunk *
Kernel::Variable::copyThunk( ) const
{
  if( thunk_ == 0 )
    {
      throw Exceptions::InternalError( "Variable::copyThunk: There was no thunk to copy." );
    }
  return thunk_->deepCopy( );
}

void
Kernel::Variable::gcMark( Kernel::GCMarkedSet & marked )
{
  if( thunk_ != 0 )
    {
      thunk_->gcMark( marked );
      return;
    }
  if( val_ != NullPtr< const Lang::Value >( ) )
    {
      const_cast< Lang::Value * >( val_.getPtr( ) )->gcMark( marked );
    }
}

void
Kernel::Variable::setValue( const RefCountPtr< const Lang::Value > & val ) const
{
  val_ = val;
  state_ = Kernel::Variable::COLD;
}


Kernel::DynamicVariableProperties::~DynamicVariableProperties( )
{ }


void
Kernel::Environment::selfDefineCoreFunction( Lang::CoreFunction * fun )
{
  selfDefineHandle( fun->getTitle( ), Kernel::VariableHandle( new Kernel::Variable( RefCountPtr< const Lang::Value >( fun ) ) ) );
}

void
Kernel::Environment::selfDefineCoreFunction( RefCountPtr< const Lang::CoreFunction > fun )
{
  selfDefineHandle( fun->getTitle( ), Kernel::VariableHandle( new Kernel::Variable( fun ) ) );
}

void
Kernel::Environment::selfDefineHandle( const char * id, const Kernel::VariableHandle & val )
{
  if( bindings_->find( id ) != bindings_->end( ) )
    {
      throw Exceptions::IntroducingExisting( Ast::SourceLocation( "< Initialization >" ), id );
    }

  (*bindings_)[ id ] = values_->size( );

  values_->push_back( val );
}

void
Kernel::Environment::selfDefine( const char * id, const RefCountPtr< const Lang::Value > & val )
{
  selfDefineHandle( id, Kernel::VariableHandle( new Kernel::Variable( val ) ) );
}

void
Kernel::Environment::selfDefine( const char * id, Kernel::StateHandle state )
{
  if( stateBindings_->find( id ) != stateBindings_->end( ) )
    {
      throw Exceptions::IntroducingExisting( Ast::SourceLocation( "< Initialization >" ), id );
    }

  (*stateBindings_)[ id ] = states_->size( );

  states_->push_back( state );
}

void
Kernel::Environment::selfDefineClass( const RefCountPtr< const Lang::Class > & cls )
{
  RefCountPtr< const char > idRef = cls->getPrettyName( );
  const char * id = strdup( idRef.getPtr( ) );
  charPtrDeletionList_.push_back( id );
  selfDefineHandle( id, Kernel::VariableHandle( new Kernel::Variable( cls ) ) );
}

void
Kernel::Environment::selfDefineDynamic( DynamicVariableProperties * dynProps )
{
  if( dynamicKeyBindings_->find( dynProps->getName( ) ) != dynamicKeyBindings_->end( ) )
    {
      throw Exceptions::IntroducingExisting( Ast::SourceLocation( "< System dynamic variable initialization >" ), dynProps->getName( ) );
    }
  
  (*dynamicKeyBindings_)[ dynProps->getName( ) ] = dynamicKeyValues_->size( );
  
  dynamicKeyValues_->push_back( dynProps );
}

void
Kernel::Environment::selfDefineDynamic( const char * id, const RefCountPtr< const Lang::Function > & filter, const Kernel::VariableHandle & defaultVal )
{
  if( dynamicKeyBindings_->find( id ) != dynamicKeyBindings_->end( ) )
    {
      throw Exceptions::IntroducingExisting( Ast::SourceLocation( "< System dynamic variable initialization >" ), id );
    }
  
  Kernel::DynamicEnvironmentKeyType key = dynamicKeyValues_->size( );
  (*dynamicKeyBindings_)[ id ] = key;
  
  dynamicKeyValues_->push_back( new Kernel::UserDynamicVariableProperties( id, key, filter, defaultVal ) );
}

void
Kernel::Environment::selfDefineDynamicHandler( const char * id, const char * msg )
{
  selfDefineDynamic( id,
		     Lang::THE_IDENTITY,
		     Helpers::newValHandle( new Lang::ExceptionWrapper< Exceptions::HandlerError >( id, msg ) ) );
}

Kernel::Environment::Environment( std::list< Kernel::Environment * > & garbageArea )
  : parent_( 0 ),
    bindings_( new Kernel::Environment::MapType ),
    values_( new std::vector< Kernel::VariableHandle >( ) ),
    dynamicKeyBindings_( new Kernel::Environment::MapType ),
    dynamicKeyValues_( new std::vector< DynamicVariableProperties * > ),
    stateBindings_( new Kernel::Environment::MapType ),
    states_( new std::vector< Kernel::StateHandle >( ) )
{
  garbageArea.push_back( this );
  ++createdCount;
  ++liveCount;
  selfDefine( "CAP_BUTT", RefCountPtr< const Lang::CapStyle >( new Lang::CapStyle( Lang::CapStyle::CAP_BUTT ) ) );
  selfDefine( "CAP_ROUND", RefCountPtr< const Lang::CapStyle >( new Lang::CapStyle( Lang::CapStyle::CAP_ROUND ) ) );
  selfDefine( "CAP_SQUARE", RefCountPtr< const Lang::CapStyle >( new Lang::CapStyle( Lang::CapStyle::CAP_SQUARE ) ) );
  selfDefine( "JOIN_MITER", RefCountPtr< const Lang::JoinStyle >( new Lang::JoinStyle( Lang::JoinStyle::JOIN_MITER ) ) );
  selfDefine( "JOIN_ROUND", RefCountPtr< const Lang::JoinStyle >( new Lang::JoinStyle( Lang::JoinStyle::JOIN_ROUND ) ) );
  selfDefine( "JOIN_BEVEL", RefCountPtr< const Lang::JoinStyle >( new Lang::JoinStyle( Lang::JoinStyle::JOIN_BEVEL ) ) );
  selfDefine( "GRAY_BLACK", Lang::THE_BLACK );
  selfDefine( "GRAY_WHITE", RefCountPtr< const Lang::Gray >( new Lang::Gray( Concrete::Gray( 1 ) ) ) );
  selfDefine( "RGB_BLACK", RefCountPtr< const Lang::RGB >( new Lang::RGB( Concrete::RGB( 0, 0, 0 ) ) ) );
  selfDefine( "RGB_WHITE", RefCountPtr< const Lang::RGB >( new Lang::RGB( Concrete::RGB( 1, 1, 1 ) ) ) );
  selfDefine( "RGB_RED", RefCountPtr< const Lang::RGB >( new Lang::RGB( Concrete::RGB( 1, 0, 0 ) ) ) );
  selfDefine( "RGB_GREEN", RefCountPtr< const Lang::RGB >( new Lang::RGB( Concrete::RGB( 0, 1, 0 ) ) ) );
  selfDefine( "RGB_BLUE", RefCountPtr< const Lang::RGB >( new Lang::RGB( Concrete::RGB( 0, 0, 1 ) ) ) );
  selfDefine( "RGB_YELLOW", RefCountPtr< const Lang::RGB >( new Lang::RGB( Concrete::RGB( 1, 1, 0 ) ) ) );
  selfDefine( "RGB_CYAN", RefCountPtr< const Lang::RGB >( new Lang::RGB( Concrete::RGB( 0, 1, 1 ) ) ) );
  selfDefine( "RGB_MAGENTA", RefCountPtr< const Lang::RGB >( new Lang::RGB( Concrete::RGB( 1, 0, 1 ) ) ) );
  selfDefine( "BLEND_NORMAL", RefCountPtr< const Lang::BlendMode >( new Lang::BlendMode( Lang::BlendMode::NORMAL ) ) );
  selfDefine( "BLEND_MULTIPLY", RefCountPtr< const Lang::BlendMode >( new Lang::BlendMode( Lang::BlendMode::MULTIPLY ) ) );
  selfDefine( "BLEND_SCREEN", RefCountPtr< const Lang::BlendMode >( new Lang::BlendMode( Lang::BlendMode::SCREEN ) ) );
  selfDefine( "BLEND_OVERLAY", RefCountPtr< const Lang::BlendMode >( new Lang::BlendMode( Lang::BlendMode::OVERLAY ) ) );
  selfDefine( "BLEND_DARKEN", RefCountPtr< const Lang::BlendMode >( new Lang::BlendMode( Lang::BlendMode::DARKEN ) ) );
  selfDefine( "BLEND_LIGHTEN", RefCountPtr< const Lang::BlendMode >( new Lang::BlendMode( Lang::BlendMode::LIGHTEN ) ) );
  selfDefine( "BLEND_COLOR_DODGE", RefCountPtr< const Lang::BlendMode >( new Lang::BlendMode( Lang::BlendMode::COLOR_DODGE ) ) );
  selfDefine( "BLEND_COLOR_BURN", RefCountPtr< const Lang::BlendMode >( new Lang::BlendMode( Lang::BlendMode::COLOR_BURN ) ) );
  selfDefine( "BLEND_HARD_LIGHT", RefCountPtr< const Lang::BlendMode >( new Lang::BlendMode( Lang::BlendMode::HARD_LIGHT ) ) );
  selfDefine( "BLEND_SOFT_LIGHT", RefCountPtr< const Lang::BlendMode >( new Lang::BlendMode( Lang::BlendMode::SOFT_LIGHT ) ) );
  selfDefine( "BLEND_DIFFERENCE", RefCountPtr< const Lang::BlendMode >( new Lang::BlendMode( Lang::BlendMode::DIFFERENCE ) ) );
  selfDefine( "BLEND_EXCLUSION", RefCountPtr< const Lang::BlendMode >( new Lang::BlendMode( Lang::BlendMode::EXCLUSION ) ) );
  selfDefine( "BLEND_HUE", RefCountPtr< const Lang::BlendMode >( new Lang::BlendMode( Lang::BlendMode::HUE ) ) );
  selfDefine( "BLEND_SATURATION", RefCountPtr< const Lang::BlendMode >( new Lang::BlendMode( Lang::BlendMode::SATURATION ) ) );
  selfDefine( "BLEND_COLOR", RefCountPtr< const Lang::BlendMode >( new Lang::BlendMode( Lang::BlendMode::COLOR ) ) );
  selfDefine( "BLEND_LUMINOSITY", RefCountPtr< const Lang::BlendMode >( new Lang::BlendMode( Lang::BlendMode::LUMINOSITY ) ) );
  selfDefine( "DEVICE_GRAY", Lang::THE_COLOR_SPACE_DEVICE_GRAY );
  selfDefine( "DEVICE_RGB",  Lang::THE_COLOR_SPACE_DEVICE_RGB );
  selfDefine( "DEVICE_CMYK", RefCountPtr< const Lang::ColorSpace >( new Lang::DeviceColorSpace< Lang::CMYK >( "DeviceCMYK", 4 ) ) );

  selfDefine( "FONT_TIMES_ROMAN", RefCountPtr< const Lang::Font >( new Lang::Font( BuiltInFonts::TIMES_ROMAN ) ) );
  selfDefine( "FONT_TIMES_BOLD", RefCountPtr< const Lang::Font >( new Lang::Font( BuiltInFonts::TIMES_BOLD ) ) );
  selfDefine( "FONT_TIMES_ITALIC", RefCountPtr< const Lang::Font >( new Lang::Font( BuiltInFonts::TIMES_ITALIC ) ) );
  selfDefine( "FONT_TIMES_BOLDITALIC", RefCountPtr< const Lang::Font >( new Lang::Font( BuiltInFonts::TIMES_BOLDITALIC ) ) );
  selfDefine( "FONT_HELVETICA", Lang::THE_FONT_HELVETICA );
  selfDefine( "FONT_HELVETICA_BOLD", RefCountPtr< const Lang::Font >( new Lang::Font( BuiltInFonts::HELVETICA_BOLD ) ) );
  selfDefine( "FONT_HELVETICA_OBLIQUE", RefCountPtr< const Lang::Font >( new Lang::Font( BuiltInFonts::HELVETICA_OBLIQUE ) ) );
  selfDefine( "FONT_HELVETICA_BOLDOBLIQUE", RefCountPtr< const Lang::Font >( new Lang::Font( BuiltInFonts::HELVETICA_BOLDOBLIQUE ) ) );
  selfDefine( "FONT_COURIER", RefCountPtr< const Lang::Font >( new Lang::Font( BuiltInFonts::COURIER ) ) );
  selfDefine( "FONT_COURIER_BOLD", RefCountPtr< const Lang::Font >( new Lang::Font( BuiltInFonts::COURIER_BOLD ) ) );
  selfDefine( "FONT_COURIER_OBLIQUE", RefCountPtr< const Lang::Font >( new Lang::Font( BuiltInFonts::COURIER_OBLIQUE ) ) );
  selfDefine( "FONT_COURIER_BOLDOBLIQUE", RefCountPtr< const Lang::Font >( new Lang::Font( BuiltInFonts::COURIER_BOLDOBLIQUE ) ) );
  selfDefine( "FONT_SYMBOL", RefCountPtr< const Lang::Font >( new Lang::Font( BuiltInFonts::SYMBOL ) ) );
  selfDefine( "FONT_ZAPFDINGBATS", RefCountPtr< const Lang::Font >( new Lang::Font( BuiltInFonts::ZAPFDINGBATS ) ) );

  selfDefine( "NULL", static_cast< RefCountPtr< const Lang::Geometric2D > >( Lang::THE_NULL2D ) );
  selfDefine( "VOID", Lang::THE_VOID );
  selfDefine( "POINTPICTURE", static_cast< RefCountPtr< const Lang::Geometric2D > >( Lang::THE_POINTPICTURE ) );
  selfDefine( "EMPTYPATH2D", Lang::THE_EMPTYPATH2D );
  selfDefine( "EMPTYPATH3D", Lang::THE_EMPTYPATH3D );
  selfDefine( "NOMASK", Lang::THE_NONE_MASK );

  selfDefine( "stdout", new Kernel::WarmOstream( std::cout ) );
  selfDefine( "stderr", new Kernel::WarmOstream( std::cerr ) );

  selfDefine( "Hot2D", Kernel::ValueRef( new Lang::HotDefault< Kernel::WarmGroup2D > ) );
  selfDefine( "Hot3D", Kernel::ValueRef( new Lang::HotDefault< Kernel::WarmGroup3D > ) );
  selfDefine( "HotZBuf", Kernel::ValueRef( new Lang::HotDefault< Kernel::WarmZBuf > ) );
  selfDefine( "HotZSorter", Kernel::ValueRef( new Lang::HotDefault< Kernel::WarmZSorter > ) );
  selfDefine( "HotString", Kernel::ValueRef( new Lang::HotDefault< Kernel::Warm_ostringstream > ) );
  selfDefine( "HotLights", Kernel::ValueRef( new Lang::HotDefault< Kernel::WarmGroupLights > ) );
  selfDefine( "HotTimer", Kernel::ValueRef( new Lang::HotDefault< Kernel::WarmTimer > ) );
  selfDefine( "HotText", Kernel::ValueRef( new Lang::HotDefault< Kernel::WarmText > ) );
  selfDefine( "HotFont", Kernel::ValueRef( new Lang::HotDefault< Kernel::WarmType3Font > ) );

  selfDefineClass( Lang::THE_OBJECT );

  selfDefineClass( Lang::Class::TypeID );

  selfDefineClass( Lang::Void::TypeID );
  selfDefineClass( Lang::Symbol::TypeID );
  selfDefineClass( Lang::Float::TypeID );
  selfDefineClass( Lang::Length::TypeID );
  selfDefineClass( Lang::Boolean::TypeID );
  selfDefineClass( Lang::String::TypeID );
  selfDefineClass( Lang::FloatPair::TypeID );
  selfDefineClass( Lang::Coords2D::TypeID );
  selfDefineClass( Lang::CornerCoords2D::TypeID );
 
  selfDefineClass( Lang::Function::TypeID );
  selfDefineClass( Lang::Transform2D::TypeID );
  selfDefineClass( Lang::Transform3D::TypeID );

  selfDefineClass( Lang::Drawable2D::TypeID );
  selfDefineClass( Lang::Drawable3D::TypeID );
  selfDefineClass( Lang::Color::TypeID );

  selfDefineCoreFunction( new Lang::Core_typeof( "typeof" ) );
  selfDefineCoreFunction( new Lang::Core_error( "error" ) );
  selfDefineCoreFunction( new Lang::Core_show( "show" ) );
  selfDefineCoreFunction( new Lang::Core_if( "if" ) );
  selfDefineCoreFunction( new Lang::NullFunction( "ignore" ) );
  selfDefineCoreFunction( new Lang::Core_rectangle( "rectangle" ) );
  selfDefineCoreFunction( new Lang::Core_memoryinfo( "memoryinfo" ) );
  selfDefineCoreFunction( new Lang::Core_hot( "hot" ) );

  selfDefineCoreFunction( new Lang::Core_gensym( "gensym" ) );
  selfDefineCoreFunction( new Lang::Core_tag( "tag" ) );
  selfDefineCoreFunction( new Lang::Core_find( "find" ) );
  selfDefineCoreFunction( new Lang::Core_findall( "findall" ) );

  selfDefineCoreFunction( new Lang::Core_mod( "mod" ) );
  selfDefineCoreFunction( new Lang::Core_ceil( "ceil" ) );
  selfDefineCoreFunction( new Lang::Core_floor( "floor" ) );
  selfDefineCoreFunction( new Lang::Core_rint( "round" ) );
  selfDefineCoreFunction( new Lang::Core_cos( "cos" ) );
  selfDefineCoreFunction( new Lang::Core_sin( "sin" ) );
  selfDefineCoreFunction( new Lang::Core_tan( "tan" ) );
  selfDefineCoreFunction( new Lang::Core_cot( "cot" ) );
  selfDefineCoreFunction( new Lang::Core_arccos( "arccos" ) );
  selfDefineCoreFunction( new Lang::Core_arcsin( "arcsin" ) );
  selfDefineCoreFunction( new Lang::Core_arctan( "arctan" ) );
  selfDefineCoreFunction( new Lang::Core_min( "min" ) );
  selfDefineCoreFunction( new Lang::Core_max( "max" ) );
  selfDefineCoreFunction( new Lang::Core_sqrt( "sqrt" ) );
  selfDefineCoreFunction( new Lang::Core_abs( "abs" ) );
  selfDefineCoreFunction( new Lang::Core_angle( "angle" ) );
  selfDefineCoreFunction( new Lang::Core_dir( "dir" ) );
  selfDefineCoreFunction( new Lang::Core_normalized( "normalized" ) );
  selfDefineCoreFunction( new Lang::Core_cross( "cross" ) );
  selfDefineCoreFunction( new Lang::Core_orthogonal( "orthogonal" ) );

  selfDefineCoreFunction( new Lang::Core_randomBall1D( "random1D" ) );
  selfDefineCoreFunction( new Lang::Core_randomBall2D( "random2D" ) );
  selfDefineCoreFunction( new Lang::Core_randomBall3D( "random3D" ) );

  selfDefineCoreFunction( new Lang::Core_stroke( "stroke" ) );
  selfDefineCoreFunction( new Lang::Core_fill( "fill" ) );
  selfDefineCoreFunction( new Lang::Core_fillstar( "fillodd" ) );
  selfDefineCoreFunction( new Lang::Core_facetnormal( "facetnormal" ) );
  selfDefineCoreFunction( new Lang::Core_facet( "facet" ) );

  selfDefineCoreFunction( new Lang::Core_formxo( "formxo" ) );
  selfDefineCoreFunction( new Lang::Core_transparencygroup( "tgroup" ) );

  selfDefineDynamic( new Kernel::WidthDynamicVariableProperties( "width" ) );
  selfDefineDynamic( new Kernel::CapStyleDynamicVariableProperties( "cap" ) );
  selfDefineDynamic( new Kernel::JoinStyleDynamicVariableProperties( "join" ) );
  selfDefineDynamic( new Kernel::MiterLimitDynamicVariableProperties( "miterlimit" ) );
  selfDefineDynamic( new Kernel::StrokingDynamicVariableProperties( Lang::DYNAMIC_VARIABLE_ID_STROKING ) );
  selfDefineDynamic( new Kernel::NonStrokingDynamicVariableProperties( Lang::DYNAMIC_VARIABLE_ID_NONSTROKING ) );
  selfDefineDynamic( new Kernel::DashDynamicVariableProperties( "dash" ) );
  selfDefineDynamic( new Kernel::BlendModeDynamicVariableProperties( "blend" ) );
  selfDefineDynamic( new Kernel::AlphaDynamicVariableProperties( "nonstrokingalpha", false ) );
  selfDefineDynamic( new Kernel::AlphaDynamicVariableProperties( "strokingalpha", true ) );

  selfDefineDynamic( new Kernel::ReflectionsDynamicVariableProperties( "reflections" ) );
  selfDefineDynamic( new Kernel::AutoIntensityDynamicVariableProperties( Lang::DYNAMIC_VARIABLE_ID_AUTOINTENSITY ) );
  selfDefineDynamic( new Kernel::AutoScatteringDynamicVariableProperties( "autoscattering" ) );
  selfDefineDynamic( new Kernel::ViewResolutionDynamicVariableProperties( "facetresolution" ) );
  selfDefineDynamic( new Kernel::ShadeOrderDynamicVariableProperties( "shadeorder" ) );

  selfDefineDynamic( new Kernel::CharacterSpacingDynamicVariableProperties( "text_characterspacing" ) );
  selfDefineDynamic( new Kernel::WordSpacingDynamicVariableProperties( "text_wordspacing" ) );
  selfDefineDynamic( new Kernel::HorizontalScalingDynamicVariableProperties( "text_horizontalscaling" ) );
  selfDefineDynamic( new Kernel::LeadingDynamicVariableProperties( "text_leading" ) );
  selfDefineDynamic( new Kernel::FontDynamicVariableProperties( "text_font" ) );
  selfDefineDynamic( new Kernel::TextSizeDynamicVariableProperties( "text_size" ) );
  selfDefineDynamic( new Kernel::TextRenderingModeDynamicVariableProperties( "text_rendering" ) );
  selfDefineDynamic( new Kernel::TextRiseDynamicVariableProperties( "text_rise" ) );
  selfDefineDynamic( new Kernel::TextKnockoutDynamicVariableProperties( "text_knockout" ) );

  selfDefineDynamic( new Kernel::EyeZDynamicVariableProperties( Lang::DYNAMIC_VARIABLE_ID_EYEZ ) );
  selfDefineDynamic( new Kernel::DefaultUnitDynamicVariableProperties( "defaultunit" ) );
  //  selfDefineDynamic( new Kernel::DefaultDestinationDynamicVariableProperties( "<<" ) );
  selfDefineDynamic( new Kernel::BlendSpaceDynamicVariableProperties( "blendspace" ) );

  selfDefineDynamicHandler( Lang::HANDLER_NO_INTERSECTION, "Failed to find intersection." );

  selfDefineCoreFunction( new Lang::Core_dashpattern( "dashpattern" ) );
  selfDefineCoreFunction( new Lang::Core_gray( "gray" ) );
  selfDefineCoreFunction( new Lang::Core_rgb( "rgb" ) );
  selfDefineCoreFunction( new Lang::Core_cmyk( "cmyk" ) );
  selfDefineCoreFunction( new Lang::Core_shape( "shape" ) );
  selfDefineCoreFunction( new Lang::Core_opacity( "opacity" ) );
  selfDefineCoreFunction( new Lang::Core_alphamask( "alphamask" ) );
  selfDefineCoreFunction( new Lang::Core_luminositymask( "luminositymask" ) );
  selfDefineCoreFunction( new Lang::Core_textrenderingmode( "textmode" ) );
  selfDefineCoreFunction( new Lang::Core_manualkern( "kerning" ) );
  selfDefineCoreFunction( new Lang::Core_automatickern( "kern" ) );
  selfDefineCoreFunction( new Lang::Core_phong( "phong" ) );
  selfDefineCoreFunction( Lang::THE_NO_ARROW );
  selfDefineCoreFunction( Lang::THE_IDENTITY );

  selfDefineCoreFunction( Ast::THE_FUNCTION_coords2D );
  selfDefineCoreFunction( Ast::THE_FUNCTION_cornercoords2D );
  selfDefineCoreFunction( Ast::THE_FUNCTION_coords3D );
  selfDefineCoreFunction( Ast::THE_FUNCTION_TeX );

  selfDefineCoreFunction( new Lang::Core_cons( "cons" ) );
  selfDefineCoreFunction( new Lang::Core_list( "list" ) );
  selfDefineCoreFunction( new Lang::Core_isnull( "isnull" ) );
  selfDefineCoreFunction( new Lang::Core_range( "range" ) );
  selfDefineCoreFunction( new Lang::Core_affinetransform( "affinetransform" ) );
  selfDefineCoreFunction( new Lang::Core_shift( "shift" ) );
  selfDefineCoreFunction( new Lang::Core_rotate( "rotate" ) );
  selfDefineCoreFunction( new Lang::Core_rotate3d( "rotate3D" ) );
  selfDefineCoreFunction( new Lang::Core_scale( "scale" ) );
  selfDefineCoreFunction( new Lang::Core_scale3d( "scale3D" ) );
  selfDefineCoreFunction( new Lang::Core_affinetransform3D( "affinetransform3D" ) );
  selfDefineCoreFunction( new Lang::Core_inverse( "inverse" ) );
  selfDefineCoreFunction( new Lang::Core_controlling( "controlling" ) );
  selfDefineCoreFunction( new Lang::Core_controlling_hull( "controlling_hull" ) );
  selfDefineCoreFunction( new Lang::Core_bbox( "bbox" ) );
  selfDefineCoreFunction( new Lang::Core_bboxed( "bboxed" ) );
  selfDefineCoreFunction( new Lang::Core_clip( "clip", "W" ) );
  selfDefineCoreFunction( new Lang::Core_clip( "clipodd", "W*" ) );
  selfDefineCoreFunction( new Lang::Core_from3Dto2D( "view" ) );
  selfDefineCoreFunction( new Lang::Core_from2Dto3D( "immerse" ) );
  selfDefineCoreFunction( new Lang::Core_facing2Din3D( "facing" ) );

  selfDefineCoreFunction( new Lang::Core_duration( "duration" ) );
  selfDefineCoreFunction( new Lang::Core_controlling_maximizer( "controlling_maximizer" ) );
  selfDefineCoreFunction( new Lang::Core_discrete_mean( "discrete_mean" ) );
  selfDefineCoreFunction( new Lang::Core_discrete_maximizer( "discrete_maximizer" ) );
  selfDefineCoreFunction( new Lang::Core_discrete_approximator( "discrete_approximator" ) );
  selfDefineCoreFunction( new Lang::Core_continuous_mean( "continuous_mean" ) );
  selfDefineCoreFunction( new Lang::Core_continuous_maximizer( "continuous_maximizer" ) );
  selfDefineCoreFunction( new Lang::Core_continuous_approximator( "continuous_approximator" ) );
  //  selfDefineCoreFunction( new Lang::Core_subpath( "subpath" ) );
  selfDefineCoreFunction( new Lang::Core_reverse( "reverse" ) );
  selfDefineCoreFunction( new Lang::Core_meetpaths( "meetpaths" ) );
  selfDefineCoreFunction( new Lang::Core_intersection( "intersection" ) );

  selfDefineCoreFunction( new Lang::Core_ambient_light( "ambient_light" ) );
  selfDefineCoreFunction( new Lang::Core_specular_light( "specular_light" ) );
  selfDefineCoreFunction( new Lang::Core_distant_light( "distant_light" ) );

  selfDefineCoreFunction( new Lang::Core_makeglyph( "basicglyph", Lang::Type3Glyph::BASIC ) );
  selfDefineCoreFunction( new Lang::Core_makeglyph( "coloredglyph", Lang::Type3Glyph::COLORED ) );


  /* Obsolete functions; use path sliders instead! */
//   selfDefineCoreFunction( new Lang::Core_point( "point" ) );
//   selfDefineCoreFunction( new Lang::Core_speed( "speed" ) );
//   selfDefineCoreFunction( new Lang::Core_reversespeed( "reversespeed" ) );
//   selfDefineCoreFunction( new Lang::Core_direction( "direction" ) );
//   selfDefineCoreFunction( new Lang::Core_reversedirection( "reversedirection" ) );
//   selfDefineCoreFunction( new Lang::Core_arctime( "arctime" ) );

  
  /* subpath functions yet to be implemented */
  //  selfDefineCoreFunction( new Lang::Core_directiontime( "directiontime" ) );
  //  selfDefineCoreFunction( new Lang::Core_nearesttimes( "nearesttimes" ) ); /* generalizes distance between subpaths */
  //  selfDefineCoreFunction( new Lang::Core_slidetimes( "slidetimes" ) ); /* "directional distance" between subpaths */
  //  selfDefineCoreFunction( new Lang::Core_sidepath( "sidepath" ) );

  //  selfDefineCoreFunction( new Lang::Core_convhull( "convhull" ) ); /* convex hull of a (full) path */

  selfDefineCoreFunction( new Lang::Core_vector( "vector" ) );
  selfDefineCoreFunction( new Lang::Core_importPDFpages( "import" ) );

  selfDefineCoreFunction( new Lang::Core_sprintf( "sprintf" ) );
  selfDefineCoreFunction( new Lang::Core_clocktime( "clocktime" ) );

}

Kernel::Environment::Environment( std::list< Kernel::Environment * > & garbageArea, Environment * parent, MapType * bindings, const RefCountPtr< std::vector< VariableHandle > > & values, MapType * stateBindings, const RefCountPtr< std::vector< StateHandle > > & states )
  : parent_( parent ), bindings_( bindings ), values_( values ), dynamicKeyBindings_( 0 ), stateBindings_( stateBindings ), states_( states )
				 //, unitMap_( NullPtr< Kernel::Environment::UnitMapType >( ) )
{
  garbageArea.push_back( this );
  //  if( parent_ != 0 )
  //    {
  //      unitMap_ = parent->unitMap_;
  //    }
  ++createdCount;
  ++liveCount;
}

Kernel::Environment::~Environment( )
{
  clear( );
  if( dynamicKeyBindings_ != 0 )
    {
      if( parent_ == 0 )
	{
	  /* The condition means that this is the global evironment, which created its own map.
	   */
	  delete dynamicKeyBindings_;
	}
      /* However, the values will always be owned by the environment itself, and be defined whenever dynamicKeyBindings != 0
       */
      for( std::vector< DynamicVariableProperties * >::iterator i = dynamicKeyValues_->begin( ); i != dynamicKeyValues_->end( ); ++i )
	{
	  if( *i != 0 )
	    {
	      delete *i;
	    }
	}
      delete dynamicKeyValues_;
    }
  if( stateBindings_ != 0 )
    {
      if( parent_ == 0 )
	{
	  /* The condition means that this is the global evironment, which created its own map.
	   */
	  delete stateBindings_;
	}
      /* However, the values will always be owned by the environment itself, and be defined whenever dynamicKeyBindings != 0
       */
      for( std::vector< StateHandle >::iterator i = states_->begin( ); i != states_->end( ); ++i )
	{
	  if( *i != 0 )
	    {
	      delete *i;
	    }
	}
    }
  --liveCount;
}

void
Kernel::Environment::setParent( Kernel::Environment * parent )
{
  parent_ = parent;
  //  unitMap_ = parent.unitMap_;
}

Kernel::Environment *
Kernel::Environment::getParent( )
{
  if( isBaseEnvironment( ) )
    {
      throw Exceptions::MiscellaneousRequirement( "Trying to find the parent of the top level environment." );
    }
  return parent_;
}

const Kernel::Environment *
Kernel::Environment::getParent( ) const
{
  if( isBaseEnvironment( ) )
    {
      throw Exceptions::MiscellaneousRequirement( "Trying to find the parent of the top level environment." );
    }
  return parent_;
}

void
Kernel::Environment::setupDynamicKeyVariables( MapType * dynamicKeyBindings )
{
  dynamicKeyBindings_ = dynamicKeyBindings;
  dynamicKeyValues_ = new std::vector< DynamicVariableProperties * >;
  size_t theSize = dynamicKeyBindings_->size( );
  dynamicKeyValues_->reserve( theSize );
  while( dynamicKeyValues_->size( ) < theSize )
    {
      dynamicKeyValues_->push_back( 0 );
    }
}

void
Kernel::Environment::activateFunctionBoundary( )
{
  functionBoundary_ = true;
}

void
Kernel::Environment::clear( )
{
  /* Strange that this is empty...
   * The idea, though, is that the reference counting of this->values shall take care of deletion.
   */
}

void
Kernel::Environment::gcMark( Kernel::GCMarkedSet & marked )
{
  if( gcMarked_ )
    {
      return;
    }
  gcMarked_ = true;
  marked.insert( marked.begin( ), this );
  
  values_->clear( );
}

void
Kernel::Environment::collect( std::list< Kernel::Environment * > & garbageArea )
{
  for( std::list< Kernel::Environment * >::iterator i = garbageArea.begin( ); i != garbageArea.end( ); ++i )
    {
      (*i)->clear_gcMarked( );
    }

  Kernel::GCMarkedSet marked;
  for( std::list< Kernel::Environment * >::iterator i = garbageArea.begin( ); i != garbageArea.end( ); ++i )
    {
      (*i)->gcMark( marked );
    }

  for( std::list< Kernel::Environment * >::iterator i = garbageArea.begin( ); i != garbageArea.end( ); )
    {
      if( (*i)->gcMarked( ) )
	{
	  ++i;
	  continue;
	}
      std::list< Kernel::Environment * >::iterator tmp = i;
      ++i;
      delete *tmp;
      garbageArea.erase( tmp );
    }
}


size_t
Kernel::Environment::findLocalVariablePosition( const Ast::SourceLocation & loc, const char * id ) const
{
  MapType::const_iterator i = bindings_->find( id );
  if( i == bindings_->end( ) )
    {
      throw Exceptions::InternalError( loc, "Environment::findLocalPosition failed" );
    }
  return i->second;
}

void
Kernel::Environment::define( size_t pos, const Kernel::VariableHandle & val )
{
  if( (*values_)[ pos ] != NullPtr< Kernel::Variable >( ) )
    {
      throw Exceptions::RedefiningLexical( reverseMapVariable( pos ) );
    }
  
  (*values_)[ pos ] = val;
}

Kernel::Environment::LexicalKey
Kernel::Environment::findLexicalVariableKey( const Ast::SourceLocation & loc, const char * id ) const
{
  MapType::const_iterator i = bindings_->find( id );
  if( i == bindings_->end( ) )
    {
      if( isBaseEnvironment( ) )
	{
	  throw Exceptions::LookupUnknown( loc, strrefdup( id ), Exceptions::LookupUnknown::VARIABLE );
	}
      return parent_->findLexicalVariableKey( loc, id ).oneAbove( );
    }
  
  return LexicalKey( 0, i->second );
}

void
Kernel::Environment::lookup( const Kernel::Environment::LexicalKey & lexKey, Kernel::EvalState * evalState ) const
{
  const Environment * env = this;
  for( size_t i = lexKey.up_; i > 0; --i )
    {
      env = env->getParent( );
    }
  
  env->lookup( lexKey.pos_, evalState );
}

void
Kernel::Environment::lookup( size_t pos, Kernel::EvalState * evalState ) const
{
  Kernel::VariableHandle res = (*values_)[ pos ];
  if( res == NullPtr< Kernel::Variable >( ) )
    {
      throw Exceptions::UninitializedAccess( );
    }
  
  Kernel::ContRef cont = evalState->cont_;
  cont->takeHandle( res, evalState );
}

Kernel::VariableHandle
Kernel::Environment::getVarHandle( const Kernel::Environment::LexicalKey & lexKey )
{ 
  Environment * env = this;
  for( size_t i = lexKey.up_; i > 0; --i )
    {
      env = env->getParent( );
    }
  
  return env->getVarHandle( lexKey.pos_ );
}

Kernel::VariableHandle
Kernel::Environment::getVarHandle( size_t pos )
{
  return (*values_)[ pos ];
}

size_t
Kernel::Environment::findLocalStatePosition( const Ast::SourceLocation & loc, const char * id ) const
{
  MapType::const_iterator i = stateBindings_->find( id );
  if( i == stateBindings_->end( ) )
    {
      throw Exceptions::InternalError( loc, "Environment::findLocalStatePosition failed" );
    }
  return i->second;
}

Kernel::Environment::LexicalKey
Kernel::Environment::findLexicalStateKey( const Ast::SourceLocation & loc, const char * id ) const
{
  MapType::const_iterator i = stateBindings_->find( id );
  if( i == stateBindings_->end( ) )
    {
      if( isBaseEnvironment( ) )
	{
	  throw Exceptions::LookupUnknown( loc, strrefdup( id ), Exceptions::LookupUnknown::STATE );
	}
      if( functionBoundary_ )
	{
	  // If the state is not found at all, this will throw an error.
	  return parent_->findLexicalStateKey( loc, id ).oneAbove( );
	  // If no error is thrown, we inform the user that the state is outside a function boundary.
	  throw Exceptions::StateBeyondFunctionBoundary( loc, strrefdup( id ) );	  
	}
      return parent_->findLexicalStateKey( loc, id ).oneAbove( );
    }
  
  return LexicalKey( 0, i->second );
}

void
Kernel::Environment::introduceState( size_t pos, Kernel::State * state )
{
  if( (*states_)[ pos ] != NullPtr< Kernel::State >( ) )
    {
      throw Exceptions::InternalError( "Better error message needed when a state is introduced more than once." );
      //      throw Exceptions::RedefiningLexical( reverseMap( pos ) );
    }
  
  (*states_)[ pos ] = state;
}

void
Kernel::Environment::freeze( size_t pos, Kernel::EvalState * evalState, const Ast::SourceLocation & loc )
{
  if( (*states_)[ pos ] == NullPtr< Kernel::State >( ) )
    {
      throw Exceptions::FreezingUndefined( Ast::SourceLocation( "< to be determined... >" ), reverseMapState( pos ) );
    }
  
  Kernel::StateHandle & state = (*states_)[ pos ];

  state->freeze( evalState, loc );
}

void
Kernel::Environment::peek( const LexicalKey & lexKey, Kernel::EvalState * evalState, const Ast::SourceLocation & loc )
{
  getStateHandle( lexKey )->peek( evalState, loc );
}

void
Kernel::Environment::tackOn( const LexicalKey & lexKey, Kernel::EvalState * evalState, const RefCountPtr< const Lang::Value > & piece, const Ast::SourceLocation & callLoc )
{
  getStateHandle( lexKey )->tackOn( evalState, piece, evalState->dyn_, callLoc );
}

Kernel::StateHandle
Kernel::Environment::getStateHandle( const LexicalKey & lexKey )
{
  Environment * env = this;
  for( size_t i = lexKey.up_; i > 0; --i )
    {
      env = env->getParent( );
    }
  
  std::cerr << "Warning: Not checking that state is in dynamic context." << std::endl ;

  return env->getStateHandle( lexKey.pos_ );
}

Kernel::StateHandle
Kernel::Environment::getStateHandle( size_t pos )
{
  return (*states_)[ pos ];
}


size_t
Kernel::Environment::findLocalDynamicPosition( const Ast::SourceLocation & loc, const char * id ) const
{
  if( dynamicKeyBindings_ == 0 )
    {
      throw Exceptions::InternalError( "Environment::findLocalDynamicPosition called with dynamicKeyBindings_ == 0." );
    }
  MapType::const_iterator i = dynamicKeyBindings_->find( id );
  if( i == dynamicKeyBindings_->end( ) )
    {
      throw Exceptions::InternalError( loc, "Environment::findLocalDynamicPosition failed" );
    }
  return i->second;
}

void
Kernel::Environment::defineDynamic( const char * debugName, size_t pos, const RefCountPtr< const Lang::Function > & filter, const Kernel::VariableHandle & defaultVal )
{
  if( dynamicKeyValues_ == 0 )
    {
      throw Exceptions::InternalError( "Environment::defineDynamic called with dynamicKeyValues_ == 0." );
    }
  if( pos > dynamicKeyValues_->size( ) )
    {
      throw Exceptions::InternalError( "Environment::defineDynamic called with pos out of range." );
    }
  if( (*dynamicKeyValues_)[ pos ] != 0 )
    {
      throw Exceptions::RedefiningDynamic( reverseMapDynamic( pos ) );
    }
  
  (*dynamicKeyValues_)[ pos ] = new Kernel::UserDynamicVariableProperties( debugName,
									   Kernel::DynamicEnvironment::getFreshKey( ),
									   filter,
									   defaultVal );
}

Kernel::Environment::LexicalKey
Kernel::Environment::findLexicalDynamicKey( const Ast::SourceLocation & loc, const char * id ) const
{
  if( dynamicKeyBindings_ == 0 )
    {
      return parent_->findLexicalDynamicKey( loc, id ).oneAbove( );
    }

  MapType::const_iterator i = dynamicKeyBindings_->find( id );
  if( i == dynamicKeyBindings_->end( ) )
    {
      if( isBaseEnvironment( ) )
	{
	  char * msg = new char[ strlen( id ) + 2 ];
	  strcpy( msg, "@" );
	  strcpy( msg + 1, id );
	  throw Exceptions::LookupUnknown( loc, RefCountPtr< const char >( msg ), Exceptions::LookupUnknown::DYNAMIC_VARIABLE );
	}
      return parent_->findLexicalDynamicKey( loc, id ).oneAbove( );
    }
  
  return LexicalKey( 0, i->second );
}

const Kernel::DynamicVariableProperties &
Kernel::Environment::lookupDynamicVariable( const LexicalKey & lexKey ) const
{
  const Environment * env = this;
  for( size_t i = lexKey.up_; i > 0; --i )
    {
      env = env->getParent( );
    }
  
  return env->lookupDynamicVariable( lexKey.pos_ );  
}

const Kernel::DynamicVariableProperties &
Kernel::Environment::lookupDynamicVariable( size_t pos ) const
{
  const DynamicVariableProperties * res = (*dynamicKeyValues_)[ pos ];
  if( res == 0 )
    {
      throw Exceptions::UninitializedAccess( );
    }
  return *res;
}

const char *
Kernel::Environment::reverseMapVariable( size_t pos ) const
{
  for( MapType::const_iterator i = bindings_->begin( ); i != bindings_->end( ); ++i )
    {
      if( i->second == pos )
	{
	  return i->first;
	}
    }
  throw Exceptions::InternalError( "Environment::reverseMapVariable failure." );
}

const char *
Kernel::Environment::reverseMapDynamic( size_t pos ) const
{
  for( MapType::const_iterator i = dynamicKeyBindings_->begin( ); i != dynamicKeyBindings_->end( ); ++i )
    {
      if( i->second == pos )
	{
	  return i->first;
	}
    }
  throw Exceptions::InternalError( "Environment::reverseMapDynamic failure." );
}

const char *
Kernel::Environment::reverseMapState( size_t pos ) const
{
  for( MapType::const_iterator i = stateBindings_->begin( ); i != stateBindings_->end( ); ++i )
    {
      if( i->second == pos )
	{
	  return i->first;
	}
    }
  throw Exceptions::InternalError( "Environment::reverseMapState failure." );
}


size_t
Kernel::Environment::size( ) const
{
  return bindings_->size( );
}


void
Kernel::Environment::print( std::ostream & os ) const
{
  std::set< MapType::key_type > shadowed;
  recursivePrint( os, & shadowed );
}

size_t
Kernel::Environment::recursivePrint( std::ostream & os, std::set< MapType::key_type > * shadowed ) const
{
  std::set< MapType::key_type > shadowedBefore( *shadowed );

  size_t depth = 0;
  if( ! isBaseEnvironment( ) )
    {
      for( MapType::const_iterator i = bindings_->begin( ); i != bindings_->end( ); ++i )
	{
	  shadowed->insert( shadowed->begin( ), i->first );
	}
      depth = parent_->recursivePrint( os, shadowed ) + 1;
    }
  
  std::string indentation = string( depth, ' ' );

  os << indentation << "--------------------" << endl ;
  if( ! isBaseEnvironment( ) )
    {
      for( MapType::const_iterator i = bindings_->begin( ); i != bindings_->end( ); ++i )
	{
	  os << indentation << "| " ;
	  if( shadowedBefore.find( i->first ) != shadowedBefore.end( ) )
	    {
	      os << "#" ;
	    }
	  else
	    {
	      os << " " ;
	    }
	  os << " " << i->first << " : " ;
	  if( (*values_)[ i->second ] == NullPtr< Kernel::Variable >( ) )
	    {
	      os << "< Uninitialized >" ;
	    }
	  else if( (*values_)[ i->second ]->isThunk( ) )
	    {
	      os << "< thunk >" ;
	    }
	  else if( dynamic_cast< const Lang::Instance * >( (*values_)[ i->second ]->getUntyped( ).getPtr( ) ) == 0 )
	    {
	      (*values_)[ i->second ]->getUntyped( )->show( os );
	    }
	  else
	    {
	      os << "..." ;
	    }
	  os << endl ;
	}
    }
  else
    {
      os << indentation << "<< global env >>" << endl ;
    }

  os << indentation << "--------------------" << endl ;

  return depth;
}

