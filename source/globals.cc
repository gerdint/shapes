#include "globals.h"

#include "shapesscanner.h"
#include "shapestypes.h"
#include "ast.h"
#include "astfun.h"
#include "refcount.h"
#include "texlabelmanager.h"
#include "statetypes.h"
#include "pdfstructure.h"
#include "functiontypes.h"
#include "containertypes.h"
#include "shapescore.h"
#include "consts.h"
#include "basicsimplex.h"
#include "debuglog.h"
#include "shapesexceptions.h"

#include <climits>

using namespace std;
using namespace Shapes;

ShapesScanner Ast::theShapesScanner;
Kernel::PassedEnv Kernel::theGlobalEnvironment = 0;
std::list< Kernel::Environment * > Kernel::theEnvironmentList;
SimplePDF::PDF_Version Kernel::the_PDF_version;
bool Kernel::allowTransparency = true;
size_t Kernel::theIndirectObjectCount = 1; /* Never use 0! */
SimplePDF::DocumentInfo Kernel::theDocInfo;
Kernel::Import Kernel::thePDFImporter;
Kernel::DebugLog Kernel::theDebugLog;
Kernel::TeXLabelManager Kernel::theTeXLabelManager;
RefCountPtr< const Kernel::GraphicsState > Kernel::THE_NO_STATE = NullPtr< const Kernel::GraphicsState >( );
RefCountPtr< const Kernel::GraphicsState > Kernel::THE_DEFAULT_STATE( new Kernel::GraphicsState( true ) );
RefCountPtr< const Lang::Function > Kernel::THE_NO_FUNCTION = NullPtr< const Lang::Function >( );

Concrete::Length Computation::the_arcdelta( 0.1 );	/* that is, 0.1bp */
double Computation::the_dtMin = 1e-4;
bool Computation::dtMinIsError = true;
Concrete::Length Computation::theTrixelizeSplicingTol( 1e-5 );
Concrete::Length Computation::theTrixelizeOverlapTol( 1e-3 );
RefCountPtr< const Computation::PaintedPolygon3D > Computation::THE_NULL_POLYGON3D( new Computation::NullPolygon3D( ) );
bool Computation::fontMetricGuessIsError = false;
Computation::BasicSimplex Computation::theTwoTriangleSimplex( 3, 6 ); // 3 variables ( x, y, r ), 6 equations.

Ast::Expression * Ast::theProgram;
PtrOwner_back_Access< std::list< Ast::AnalysisEnvironment * > > Ast::theAnalysisEnvironmentList;
Ast::AnalysisEnvironment * Ast::theGlobalAnalysisEnvironment;
Ast::SourceLocation Ast::THE_UNKNOWN_LOCATION( "" );	// This is a special value.	See SourceLocation.
Ast::DummyExpression Ast::THE_CORE_DEFAULT_VALUE_EXPRESSION = Ast::DummyExpression( Ast::SourceLocation( "< core function default value >" ) );
Ast::DummyExpression Ast::THE_INTERNAL_VALUE_EXPRESSION = Ast::DummyExpression( Ast::SourceLocation( "< internally passed value >" ) );

PtrOwner_back_Access< std::list< Exceptions::Exception * > > Kernel::thePostCheckErrorsList;
PtrOwner_back_Access< std::list< Exceptions::Exception * > > Ast::theAnalysisErrorsList;


bool Interaction::debugBacktrace = true;
bool Interaction::debugSystem = false;
size_t Interaction::debugStep = INT_MAX;
const char * Interaction::displayUnitName = "cm";
double Interaction::displayUnitFactor = -1;
bool Interaction::fontMetricMessages = false;
bool Interaction::fontMetricDebug = false;
bool Interaction::characterColumnInBytes = false;
bool Interaction::pdfLaTeXInteractionTo_stderr = false;

RefCountPtr< const Lang::GroupNull2D > Lang::THE_NULL2D( new Lang::GroupNull2D( ) );
RefCountPtr< const Lang::GroupNull3D > Lang::THE_NULL3D( new Lang::GroupNull3D( ) );
RefCountPtr< const Lang::DynamicBindings > Lang::THE_NULL_DYNAMIC_BINDINGS( new Lang::DynamicBindingsNull( ) );
RefCountPtr< const Lang::LightGroup > Lang::THE_NULL_LIGHTS( new Lang::LightNull( ) );
RefCountPtr< const Lang::SingleListNull > Lang::THE_CONS_NULL( new Lang::SingleListNull( ) );
RefCountPtr< const Lang::SpecularReflectionNull > Lang::THE_SPECULARREFLECTION_NULL( new Lang::SpecularReflectionNull( ) );
//RefCountPtr< const Lang::PaintedPolygon2D > Computation::THE_NULL_POLYGON2D( new Computation::NullPolygon2D( ) );
RefCountPtr< const Lang::ColorSpace > Lang::THE_INHERITED_COLOR_SPACE( new Lang::InheritedColorSpace( ) );
RefCountPtr< const Lang::ColorSpace > Lang::THE_COLOR_SPACE_DEVICE_GRAY( new Lang::DeviceColorSpace< Lang::Gray >( "DeviceGray", 1 ) );
RefCountPtr< const Lang::ColorSpace > Lang::THE_COLOR_SPACE_DEVICE_RGB( new Lang::DeviceColorSpace< Lang::RGB >( "DeviceRGB", 3 ) );
RefCountPtr< const Lang::Drawable2D > Lang::THE_POINTPICTURE = NullPtr< const Lang::Drawable2D >( );
RefCountPtr< const Lang::Void > Lang::THE_VOID( new Lang::Void( ) );
RefCountPtr< const Lang::ElementaryPath2D > Lang::THE_EMPTYPATH2D( new Lang::ElementaryPath2D( ) );
RefCountPtr< const Lang::ElementaryPath3D > Lang::THE_EMPTYPATH3D( new Lang::ElementaryPath3D( ) );
RefCountPtr< const Lang::Boolean > Lang::THE_FALSE( new Lang::Boolean( false ) );
RefCountPtr< const Lang::Boolean > Lang::THE_TRUE( new Lang::Boolean( true ) );
RefCountPtr< const Lang::Gray > Lang::THE_BLACK( new Lang::Gray( Concrete::Gray( 0 ) ) );
RefCountPtr< const Lang::Gray > Lang::THE_OCCLUDING_WHITE( new Lang::Gray( Concrete::Gray( 1 ) ) );
RefCountPtr< const Lang::Alpha > Lang::THE_OPAQUE( new Lang::Alpha( false, 1 ) );
RefCountPtr< const Lang::Class > Lang::THE_OBJECT( new Lang::Object( ) );
RefCountPtr< const Lang::Dash > Lang::THE_SOLID_DASH( new Lang::Dash( ) );
RefCountPtr< const Lang::SoftMask > Lang::THE_NONE_MASK( new Lang::SoftMask( ) );
RefCountPtr< const Lang::SoftMask > Lang::THE_SAME_MASK( new Lang::SoftMask( ) );

RefCountPtr< const char > BuiltInFonts::HELVETICA = strrefdup( "Helvetica" );
RefCountPtr< const Lang::Font > Lang::THE_FONT_HELVETICA( new Lang::Font( BuiltInFonts::HELVETICA ) );

Kernel::Arguments Kernel::EMPTY_ARGLIST( new Kernel::EvaluatedFormals( "< the empty arg list >" ) );
RefCountPtr< const Lang::Structure > Lang::THE_EMPTY_STRUCT( new Lang::Structure( new Ast::ArgListExprs( true ), Lang::THE_CONS_NULL, true ) );
Kernel::VariableHandle Kernel::THE_SLOT_VARIABLE( new Kernel::Variable( RefCountPtr< const Lang::Value >( new Lang::Void ) ) );
Kernel::VariableHandle Kernel::THE_VOID_VARIABLE( new Kernel::Variable( RefCountPtr< const Lang::Value >( new Lang::Void ) ) );
Kernel::VariableHandle Kernel::THE_FALSE_VARIABLE( new Kernel::Variable( Lang::THE_FALSE ) );
Kernel::VariableHandle Kernel::THE_TRUE_VARIABLE( new Kernel::Variable( Lang::THE_TRUE ) );
Kernel::StateHandle Kernel::THE_SLOT_STATE = Kernel::StateHandle( NullPtr< Kernel::State >( ) );

/*
 * Two global Symbol objects were moved to elementarytypes.cc to ensure they get initialized after the static variables in Lang::Symbol.
*/

Lang::Transform2D Lang::THE_2D_IDENTITY( 1, 0,
																				 0, 1,
																				 Concrete::ZERO_LENGTH, Concrete::ZERO_LENGTH );
Lang::Transform3D Lang::THE_3D_IDENTITY( 1, 0, 0,
																				 0, 1, 0,
																				 0, 0, 1,
																				 Concrete::ZERO_LENGTH, Concrete::ZERO_LENGTH, Concrete::ZERO_LENGTH );


void
Shapes::Kernel::registerGlobals( Kernel::Environment * env )
{
	env->initDefine( "CAP_BUTT", RefCountPtr< const Lang::CapStyle >( new Lang::CapStyle( Lang::CapStyle::CAP_BUTT ) ) );
	env->initDefine( "CAP_ROUND", RefCountPtr< const Lang::CapStyle >( new Lang::CapStyle( Lang::CapStyle::CAP_ROUND ) ) );
	env->initDefine( "CAP_SQUARE", RefCountPtr< const Lang::CapStyle >( new Lang::CapStyle( Lang::CapStyle::CAP_SQUARE ) ) );
	env->initDefine( "JOIN_MITER", RefCountPtr< const Lang::JoinStyle >( new Lang::JoinStyle( Lang::JoinStyle::JOIN_MITER ) ) );
	env->initDefine( "JOIN_ROUND", RefCountPtr< const Lang::JoinStyle >( new Lang::JoinStyle( Lang::JoinStyle::JOIN_ROUND ) ) );
	env->initDefine( "JOIN_BEVEL", RefCountPtr< const Lang::JoinStyle >( new Lang::JoinStyle( Lang::JoinStyle::JOIN_BEVEL ) ) );
	env->initDefine( "GRAY_BLACK", Lang::THE_BLACK );
	env->initDefine( "OCCLUDING", Lang::THE_OCCLUDING_WHITE );
	env->initDefine( "GRAY_WHITE", RefCountPtr< const Lang::Gray >( new Lang::Gray( Concrete::Gray( 1 ) ) ) );
	env->initDefine( "RGB_BLACK", RefCountPtr< const Lang::RGB >( new Lang::RGB( Concrete::RGB( 0, 0, 0 ) ) ) );
	env->initDefine( "RGB_WHITE", RefCountPtr< const Lang::RGB >( new Lang::RGB( Concrete::RGB( 1, 1, 1 ) ) ) );
	env->initDefine( "RGB_RED", RefCountPtr< const Lang::RGB >( new Lang::RGB( Concrete::RGB( 1, 0, 0 ) ) ) );
	env->initDefine( "RGB_GREEN", RefCountPtr< const Lang::RGB >( new Lang::RGB( Concrete::RGB( 0, 1, 0 ) ) ) );
	env->initDefine( "RGB_BLUE", RefCountPtr< const Lang::RGB >( new Lang::RGB( Concrete::RGB( 0, 0, 1 ) ) ) );
	env->initDefine( "RGB_YELLOW", RefCountPtr< const Lang::RGB >( new Lang::RGB( Concrete::RGB( 1, 1, 0 ) ) ) );
	env->initDefine( "RGB_CYAN", RefCountPtr< const Lang::RGB >( new Lang::RGB( Concrete::RGB( 0, 1, 1 ) ) ) );
	env->initDefine( "RGB_MAGENTA", RefCountPtr< const Lang::RGB >( new Lang::RGB( Concrete::RGB( 1, 0, 1 ) ) ) );
	env->initDefine( "BLEND_NORMAL", RefCountPtr< const Lang::BlendMode >( new Lang::BlendMode( Lang::BlendMode::NORMAL ) ) );
	env->initDefine( "BLEND_MULTIPLY", RefCountPtr< const Lang::BlendMode >( new Lang::BlendMode( Lang::BlendMode::MULTIPLY ) ) );
	env->initDefine( "BLEND_SCREEN", RefCountPtr< const Lang::BlendMode >( new Lang::BlendMode( Lang::BlendMode::SCREEN ) ) );
	env->initDefine( "BLEND_OVERLAY", RefCountPtr< const Lang::BlendMode >( new Lang::BlendMode( Lang::BlendMode::OVERLAY ) ) );
	env->initDefine( "BLEND_DARKEN", RefCountPtr< const Lang::BlendMode >( new Lang::BlendMode( Lang::BlendMode::DARKEN ) ) );
	env->initDefine( "BLEND_LIGHTEN", RefCountPtr< const Lang::BlendMode >( new Lang::BlendMode( Lang::BlendMode::LIGHTEN ) ) );
	env->initDefine( "BLEND_COLOR_DODGE", RefCountPtr< const Lang::BlendMode >( new Lang::BlendMode( Lang::BlendMode::COLOR_DODGE ) ) );
	env->initDefine( "BLEND_COLOR_BURN", RefCountPtr< const Lang::BlendMode >( new Lang::BlendMode( Lang::BlendMode::COLOR_BURN ) ) );
	env->initDefine( "BLEND_HARD_LIGHT", RefCountPtr< const Lang::BlendMode >( new Lang::BlendMode( Lang::BlendMode::HARD_LIGHT ) ) );
	env->initDefine( "BLEND_SOFT_LIGHT", RefCountPtr< const Lang::BlendMode >( new Lang::BlendMode( Lang::BlendMode::SOFT_LIGHT ) ) );
	env->initDefine( "BLEND_DIFFERENCE", RefCountPtr< const Lang::BlendMode >( new Lang::BlendMode( Lang::BlendMode::DIFFERENCE ) ) );
	env->initDefine( "BLEND_EXCLUSION", RefCountPtr< const Lang::BlendMode >( new Lang::BlendMode( Lang::BlendMode::EXCLUSION ) ) );
	env->initDefine( "BLEND_HUE", RefCountPtr< const Lang::BlendMode >( new Lang::BlendMode( Lang::BlendMode::HUE ) ) );
	env->initDefine( "BLEND_SATURATION", RefCountPtr< const Lang::BlendMode >( new Lang::BlendMode( Lang::BlendMode::SATURATION ) ) );
	env->initDefine( "BLEND_COLOR", RefCountPtr< const Lang::BlendMode >( new Lang::BlendMode( Lang::BlendMode::COLOR ) ) );
	env->initDefine( "BLEND_LUMINOSITY", RefCountPtr< const Lang::BlendMode >( new Lang::BlendMode( Lang::BlendMode::LUMINOSITY ) ) );
	env->initDefine( "DEVICE_GRAY", Lang::THE_COLOR_SPACE_DEVICE_GRAY );
	env->initDefine( "DEVICE_RGB",	Lang::THE_COLOR_SPACE_DEVICE_RGB );
	env->initDefine( "DEVICE_CMYK", RefCountPtr< const Lang::ColorSpace >( new Lang::DeviceColorSpace< Lang::CMYK >( "DeviceCMYK", 4 ) ) );

	env->initDefineDynamicHandler( Lang::HANDLER_NO_INTERSECTION, "Failed to find intersection." );

	env->initDefine( "void", Lang::THE_VOID );
	env->initDefine( "null", static_cast< RefCountPtr< const Lang::Geometric2D > >( Lang::THE_NULL2D ) );
	env->initDefine( "null3D", static_cast< RefCountPtr< const Lang::Geometric3D > >( Lang::THE_NULL3D ) );
	env->initDefine( "nullbind", Lang::THE_NULL_DYNAMIC_BINDINGS );
	env->initDefine( "emptypath", Lang::THE_EMPTYPATH2D );
	env->initDefine( "emptypath3D", Lang::THE_EMPTYPATH3D );
	env->initDefine( "pointpicture", static_cast< RefCountPtr< const Lang::Geometric2D > >( Lang::THE_POINTPICTURE ) );
	env->initDefine( "NO_MASK", Lang::THE_NONE_MASK );

	env->initDefineCoreFunction( Lang::THE_NO_ARROW );
	env->initDefineCoreFunction( Lang::THE_IDENTITY );

	env->initDefineCoreFunction( Ast::THE_FUNCTION_coords2D );
	env->initDefineCoreFunction( Ast::THE_FUNCTION_cornercoords2D );
	env->initDefineCoreFunction( Ast::THE_FUNCTION_coords3D );
	env->initDefineCoreFunction( Ast::THE_FUNCTION_TeX );
}

