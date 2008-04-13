#ifndef globals_h
#define globals_h

#include "Shapes_Lang_decls.h"
#include "Shapes_Kernel_decls.h"
#include "Shapes_Computation_decls.h"
#include "Shapes_Concrete_decls.h"
#include "Shapes_Helpers_decls.h"
#include "Shapes_Exceptions_decls.h"
#include "SimplePDF_decls.h"
#include "FontMetrics_decls.h"

#include "elementarylength.h"
#include "refcount.h"
#include "ptrowner.h"

// The following types do not belong to any particular namespace for some reason, and are therefore declared like this:
class ShapesScanner;


namespace Shapes
{

	namespace Interaction
	{

		extern bool debugBacktrace;
		extern bool debugSystem;
		extern size_t debugStep;
		extern const char * displayUnitName;
		extern double displayUnitFactor;
		extern bool fontMetricMessages;
		extern bool fontMetricDebug;
		extern bool characterColumnInBytes;
		extern bool pdfLaTeXInteractionTo_stderr;

	}

	namespace Computation
	{

		extern Concrete::Length the_arcdelta;
		extern double the_dtMin;
		extern bool dtMinIsError;
		extern Concrete::Length theTrixelizeSplicingTol;
		extern Concrete::Length theTrixelizeOverlapTol;
		extern RefCountPtr< const Computation::PaintedPolygon3D > THE_NULL_POLYGON3D;
		extern bool fontMetricGuessIsError;
		extern Computation::BasicSimplex theTwoTriangleSimplex;
	}

	namespace Kernel
	{

		extern Kernel::PassedEnv theGlobalEnvironment; // Useful for functions in the core refering to their home environment.
		extern std::list< Kernel::Environment * > theEnvironmentList;
		extern Kernel::TeXLabelManager theTeXLabelManager;
		extern SimplePDF::PDF_out * the_pdfo;
		extern Kernel::DebugLog theDebugLog;

		extern RefCountPtr< const Kernel::GraphicsState > THE_NO_STATE;
		extern RefCountPtr< const Kernel::GraphicsState > THE_DEFAULT_STATE;

		extern RefCountPtr< const Lang::Function > THE_NO_FUNCTION;

		extern Arguments EMPTY_ARGLIST;
		extern VariableHandle THE_SLOT_VARIABLE;
		extern VariableHandle THE_FALSE_VARIABLE;
		extern VariableHandle THE_TRUE_VARIABLE;
		extern VariableHandle THE_VOID_VARIABLE;

		extern StateHandle THE_SLOT_STATE;

		extern RefCountPtr< const Lang::Symbol > THE_NAVIGATION_SYMBOL;
		extern RefCountPtr< const Lang::Symbol > THE_ANNOTATION_SYMBOL;

		extern PtrOwner_back_Access< std::list< Exceptions::Exception * > > thePostCheckErrorsList;
	}

	namespace Lang
	{

		extern RefCountPtr< const Lang::GroupNull2D > THE_NULL2D;
		extern RefCountPtr< const Lang::GroupNull3D > THE_NULL3D;
		extern RefCountPtr< const Lang::DynamicBindings > THE_NULL_DYNAMIC_BINDINGS;
		extern RefCountPtr< const Lang::LightGroup > THE_NULL_LIGHTS;
		extern RefCountPtr< const Lang::SingleListNull > THE_CONS_NULL;
		extern RefCountPtr< const Lang::Structure > THE_EMPTY_STRUCT;
		extern RefCountPtr< const Lang::SpecularReflectionNull > THE_SPECULARREFLECTION_NULL;
		//		extern RefCountPtr< const Lang::PaintedPoltgon2D > THE_NULL_POLYGON2D;
		extern RefCountPtr< const Lang::ColorSpace > THE_INHERITED_COLOR_SPACE;
		extern RefCountPtr< const Lang::ColorSpace > THE_COLOR_SPACE_DEVICE_GRAY;
		extern RefCountPtr< const Lang::ColorSpace > THE_COLOR_SPACE_DEVICE_RGB;
		extern RefCountPtr< const Lang::Drawable2D > THE_POINTPICTURE;
		extern RefCountPtr< const Lang::Void > THE_VOID;
		extern RefCountPtr< const Lang::ElementaryPath2D > THE_EMPTYPATH2D;
		extern RefCountPtr< const Lang::ElementaryPath3D > THE_EMPTYPATH3D;
		extern RefCountPtr< const Lang::Boolean > THE_FALSE;
		extern RefCountPtr< const Lang::Boolean > THE_TRUE;
		extern RefCountPtr< const Lang::Gray > THE_BLACK;
		extern RefCountPtr< const Lang::Gray > THE_OCCLUDING_WHITE;
		extern RefCountPtr< const Lang::Alpha > THE_OPAQUE;
		extern RefCountPtr< const Lang::Class > THE_OBJECT;
		extern RefCountPtr< const Lang::CoreFunction > THE_NO_ARROW;
		extern RefCountPtr< const Lang::CoreFunction > THE_IDENTITY;
		extern RefCountPtr< const Lang::Dash > THE_SOLID_DASH;
		extern RefCountPtr< const Lang::SoftMask > THE_NONE_MASK;
		extern RefCountPtr< const Lang::SoftMask > THE_SAME_MASK;
		extern RefCountPtr< const Lang::Font > THE_FONT_HELVETICA;

		extern Transform2D THE_2D_IDENTITY;
		extern Transform3D THE_3D_IDENTITY;

		extern RefCountPtr< const Lang::BinaryOperatorFunction > THE_OPERATOR_MINUSMINUS;
		extern RefCountPtr< const Lang::BinaryOperatorFunction > THE_OPERATOR_PLUSPLUS;
		extern RefCountPtr< const Lang::BinaryOperatorFunction > THE_OPERATOR_AMPERSAND;
		extern RefCountPtr< const Lang::BinaryOperatorFunction > THE_OPERATOR_PLUS;
		extern RefCountPtr< const Lang::BinaryOperatorFunction > THE_OPERATOR_MINUS;
		extern RefCountPtr< const Lang::BinaryOperatorFunction > THE_OPERATOR_ANGLE;
		extern RefCountPtr< const Lang::BinaryOperatorFunction > THE_OPERATOR_STAR;
		extern RefCountPtr< const Lang::BinaryOperatorFunction > THE_OPERATOR_PROJECTION;
		extern RefCountPtr< const Lang::BinaryOperatorFunction > THE_OPERATOR_SLASH;
		extern RefCountPtr< const Lang::BinaryOperatorFunction > THE_OPERATOR_COMPOSE;
		extern RefCountPtr< const Lang::UnaryOperatorFunction > THE_OPERATOR_NEG;
		extern RefCountPtr< const Lang::BinaryOperatorFunction > THE_OPERATOR_LESS;
		extern RefCountPtr< const Lang::BinaryOperatorFunction > THE_OPERATOR_GREATER;
		extern RefCountPtr< const Lang::BinaryOperatorFunction > THE_OPERATOR_EQEQ;
		extern RefCountPtr< const Lang::BinaryOperatorFunction > THE_OPERATOR_EQNEQ;
		extern RefCountPtr< const Lang::BinaryOperatorFunction > THE_OPERATOR_LESSEQ;
		extern RefCountPtr< const Lang::BinaryOperatorFunction > THE_OPERATOR_GREATEREQ;
		extern RefCountPtr< const Lang::UnaryOperatorFunction > THE_OPERATOR_NOT;
		extern RefCountPtr< const Lang::CoreFunction > THE_FUNCTION_AND;
		extern RefCountPtr< const Lang::CoreFunction > THE_FUNCTION_OR;
		extern RefCountPtr< const Lang::BinaryOperatorFunction > THE_OPERATOR_XOR;
	}

	namespace Ast
	{

		extern ShapesScanner theShapesScanner;
		extern Ast::Expression * theProgram;
		extern PtrOwner_back_Access< std::list< Ast::AnalysisEnvironment * > > theAnalysisEnvironmentList;
		extern Ast::AnalysisEnvironment * theGlobalAnalysisEnvironment;
		extern Ast::DummyExpression THE_CORE_DEFAULT_VALUE_EXPRESSION;
		extern Ast::DummyExpression THE_INTERNAL_VALUE_EXPRESSION;

		extern PtrOwner_back_Access< std::list< Exceptions::Exception * > > theAnalysisErrorsList;

		extern RefCountPtr< const Lang::CoreFunction > THE_FUNCTION_coords2D;
		extern RefCountPtr< const Lang::CoreFunction > THE_FUNCTION_cornercoords2D;
		extern RefCountPtr< const Lang::CoreFunction > THE_FUNCTION_coords3D;
		extern RefCountPtr< const Lang::CoreFunction > THE_FUNCTION_polarHandle2DFree_r;
		extern RefCountPtr< const Lang::CoreFunction > THE_FUNCTION_polarHandle2DFree_ra;
		extern RefCountPtr< const Lang::CoreFunction > THE_FUNCTION_TeX;

	}
}

#endif
