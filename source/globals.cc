#include "globals.h"

#include "metapdfscanner.h"
#include "metapdftypes.h"
#include "metapdfast.h"
#include "metapdfastfun.h"
#include "refcount.h"
#include "texlabelmanager.h"
#include "statetypes.h"
#include "pdfstructure.h"
#include "functiontypes.h"
#include "containertypes.h"
#include "metapdfcore.h"
#include "consts.h"
#include "basicsimplex.h"

#include <climits>

using namespace std;
using namespace MetaPDF;

MetaPDFScanner Ast::theMetaPDFScanner;
std::list< Kernel::Environment * > Kernel::theEnvironmentList;
SimplePDF::PDF_out * Kernel::the_pdfo = new SimplePDF::PDF_out( & cout );
Kernel::TeXLabelManager Kernel::theTeXLabelManager;
RefCountPtr< const Kernel::GraphicsState > Kernel::THE_NO_STATE = NullPtr< const Kernel::GraphicsState >( );
RefCountPtr< const Kernel::GraphicsState > Kernel::THE_DEFAULT_STATE( new Kernel::GraphicsState( true ) );
RefCountPtr< const Lang::Function > Kernel::THE_NO_FUNCTION = NullPtr< const Lang::Function >( );

Concrete::Length Computation::the_arcdelta( 0.1 );  /* that is, 0.1bp */
double Computation::the_dtMin = 1e-4;
bool Computation::dtMinIsError = true;
Concrete::Length Computation::theTrixelizeSplicingTol( 1e-5 );
Concrete::Length Computation::theTrixelizeOverlapTol( 1e-3 );
RefCountPtr< const Computation::PaintedPolygon3D > Computation::THE_NULL_POLYGON3D( new Computation::NullPolygon3D( ) );
bool Computation::fontMetricGuessIsError = false;
Computation::BasicSimplex Computation::theTwoTriangleSimplex( 3, 6 ); // 3 variables ( x, y, r ), 6 equations.  

Ast::Expression * Ast::theProgram;
Ast::SourceLocation Ast::THE_UNKNOWN_LOCATION( "< unknown location >" );
Ast::DummyExpression Ast::THE_CORE_DEFAULT_VALUE_EXPRESSION = Ast::DummyExpression( Ast::SourceLocation( "< core function default value >" ) );
Ast::DummyExpression Ast::THE_INTERNAL_VALUE_EXPRESSION = Ast::DummyExpression( Ast::SourceLocation( "< internally passed value >" ) );

bool Interaction::debugBacktrace = true;
bool Interaction::debugSystem = false;
size_t Interaction::debugStep = INT_MAX;
const char * Interaction::displayUnitName = "cm";
double Interaction::displayUnitFactor = -1;
bool Interaction::fontMetricMessages = false;
bool Interaction::fontMetricDebug = false;

RefCountPtr< const Lang::GroupNull2D > Lang::THE_NULL2D( new Lang::GroupNull2D( ) );
RefCountPtr< const Lang::GroupNull3D > Lang::THE_NULL3D( new Lang::GroupNull3D( ) );
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
RefCountPtr< const Lang::Alpha > Lang::THE_OPAQUE( new Lang::Alpha( false, 1 ) );
RefCountPtr< const Lang::Class > Lang::THE_OBJECT( new Lang::Object( ) );
RefCountPtr< const Lang::CoreFunction > Lang::THE_NO_ARROW( new Lang::Core_noArrow( "NO_ARROW" ) );
RefCountPtr< const Lang::CoreFunction > Lang::THE_IDENTITY( new Lang::Core_identity( "I" ) );
RefCountPtr< const Lang::Dash > Lang::THE_SOLID_DASH( new Lang::Dash( ) );
RefCountPtr< const Lang::SoftMask > Lang::THE_NONE_MASK( new Lang::SoftMask( ) );
RefCountPtr< const Lang::SoftMask > Lang::THE_SAME_MASK( new Lang::SoftMask( ) );

RefCountPtr< const char > BuiltInFonts::HELVETICA = strrefdup( "Helvetica" );
RefCountPtr< const Lang::Font > Lang::THE_FONT_HELVETICA( new Lang::Font( BuiltInFonts::HELVETICA ) );

RefCountPtr< const Lang::CoreFunction > Ast::THE_FUNCTION_coords2D( new Lang::Core_coords2D( "coords2D" ) );
RefCountPtr< const Lang::CoreFunction > Ast::THE_FUNCTION_cornercoords2D( new Lang::Core_cornercoords2D( "cornercoords2D" ) );
RefCountPtr< const Lang::CoreFunction > Ast::THE_FUNCTION_coords3D( new Lang::Core_coords3D( "coords3D" ) );
RefCountPtr< const Lang::CoreFunction > Ast::THE_FUNCTION_polarHandle2DFree_r( new Lang::Core_polarHandle2DFree_r( "polarHandle2DFree_r" ) );
RefCountPtr< const Lang::CoreFunction > Ast::THE_FUNCTION_polarHandle2DFree_ra( new Lang::Core_polarHandle2DFree_ra( "polarHandle2DFree_ra" ) );
RefCountPtr< const Lang::CoreFunction > Ast::THE_FUNCTION_TeX( new Lang::Core_TeX( "TeX" ) );

Kernel::Arguments Kernel::EMPTY_ARGLIST( new Kernel::EvaluatedFormals( "< the empty arg list >" ) );
Kernel::HandleType Kernel::THE_SLOT_VARIABLE( new Kernel::Variable( RefCountPtr< const Lang::Value >( new Lang::Void ) ) );
Kernel::HandleType Kernel::THE_VOID_VARIABLE( new Kernel::Variable( RefCountPtr< const Lang::Value >( new Lang::Void ) ) );
Kernel::HandleType Kernel::THE_FALSE_VARIABLE( new Kernel::Variable( Lang::THE_FALSE ) );
Kernel::HandleType Kernel::THE_TRUE_VARIABLE( new Kernel::Variable( Lang::THE_TRUE ) );
Kernel::StateHandleType Kernel::THE_SLOT_STATE( NullPtr< Kernel::State >( ) );

Lang::Transform2D Lang::THE_2D_IDENTITY( 1, 0,
					 0, 1,
					 Concrete::ZERO_LENGTH, Concrete::ZERO_LENGTH );
Lang::Transform3D Lang::THE_3D_IDENTITY( 1, 0, 0,
					 0, 1, 0,
					 0, 0, 1,
					 Concrete::ZERO_LENGTH, Concrete::ZERO_LENGTH, Concrete::ZERO_LENGTH );
