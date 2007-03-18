#include "consts.h"
#include "metapdfexceptions.h"
#include "classtypes.h"
#include "statetypes.h"

using namespace MetaPDF;

RefCountPtr< const char > Interaction::SEVERAL_TYPES = strrefdup( "(several types)" );
RefCountPtr< const char > Interaction::PUBLIC_SCOPE_NAME = strrefdup( "public" );
RefCountPtr< const char > Interaction::PROTECTED_SCOPE_NAME = strrefdup( "protected" );

RefCountPtr< const char > Lang::ARCDELTA_ID = strrefdup( "arcdelta" );
RefCountPtr< const char > Lang::TEX_SYNTAX_ID = strrefdup( "TeX" );

const char * Lang::CANVAS_ID = "page";
const char * Lang::SELF_ID = "self";
const char * Kernel::SEQUENTIAL_EXPR_VAR_ID = ".seqvar"; /* Note that the leading dot puts this variable aside all user-variables. */

const char * Lang::DYNAMIC_VARIABLE_ID_EYEZ = "eyez";
const char * Lang::DYNAMIC_VARIABLE_ID_STROKING = "stroking";
const char * Lang::DYNAMIC_VARIABLE_ID_NONSTROKING = "nonstroking";
const char * Lang::DYNAMIC_VARIABLE_ID_AUTOINTENSITY = "autointensity";

const Ast::MemberMode Ast::MEMBER_ACCESS_BITS = 0x000F;
const Ast::MemberMode Ast::MEMBER_ACCESS_PRIVATE = 0x0000;
const Ast::MemberMode Ast::MEMBER_ACCESS_PUBLIC_GET = 0x0001;
const Ast::MemberMode Ast::MEMBER_ACCESS_PUBLIC_INSERT = 0x0002;
const Ast::MemberMode Ast::MEMBER_ACCESS_PROTECTED_GET = 0x0004;
const Ast::MemberMode Ast::MEMBER_ACCESS_PROTECTED_INSERT = 0x0008;
const Ast::MemberMode Ast::MEMBER_CONST = 0x0010;
const Ast::MemberMode Ast::MEMBER_METHOD = 0x0020;
const Ast::MemberMode Ast::MEMBER_ABSTRACT = 0x0040;
const Ast::MemberMode Ast::MEMBER_FINAL = 0x0080;
const Ast::MemberMode Ast::MEMBER_TRANSFORMING = 0x0100;

const Ast::ClassMode Ast::CLASS_MODE_ABSTRACT = 0x0001;
const Ast::ClassMode Ast::CLASS_MODE_FINAL = 0x0002;

const Ast::FunctionMode Ast::FUNCTION_TRANSFORMING = 0x0001;
const Ast::FunctionMode Ast::FUNCTION_PROCEDURAL = 0x0002;

const char * Lang::MESSAGE_DRAWABLE_DRAW_ID = "draw";

const char * Lang::HANDLER_NO_INTERSECTION = "handler_NoIntersection";

const double Computation::SINGULAR_TRANSFORM_LIMIT = 1e-8;


RefCountPtr< const char > BuiltInFonts::TIMES_ROMAN = strrefdup( "Times-Roman" );
RefCountPtr< const char > BuiltInFonts::TIMES_BOLD = strrefdup( "Times-Bold" );
RefCountPtr< const char > BuiltInFonts::TIMES_ITALIC = strrefdup( "Times-Italic" );
RefCountPtr< const char > BuiltInFonts::TIMES_BOLDITALIC = strrefdup( "Times-BoldItalic" );
// HELVETICA is initialized in globals.cc to ensure correct order of initialization.
RefCountPtr< const char > BuiltInFonts::HELVETICA_BOLD = strrefdup( "Helvetica-Bold" );
RefCountPtr< const char > BuiltInFonts::HELVETICA_OBLIQUE = strrefdup( "Helvetica-Oblique" );
RefCountPtr< const char > BuiltInFonts::HELVETICA_BOLDOBLIQUE = strrefdup( "Helvetica-BoldOblique" );
RefCountPtr< const char > BuiltInFonts::COURIER = strrefdup( "Courier" );
RefCountPtr< const char > BuiltInFonts::COURIER_BOLD = strrefdup( "Courier-Bold" );
RefCountPtr< const char > BuiltInFonts::COURIER_OBLIQUE = strrefdup( "Courier-Oblique" );
RefCountPtr< const char > BuiltInFonts::COURIER_BOLDOBLIQUE = strrefdup( "Courier-BoldOblique" );
RefCountPtr< const char > BuiltInFonts::SYMBOL = strrefdup( "Symbol" );
RefCountPtr< const char > BuiltInFonts::ZAPFDINGBATS = strrefdup( "ZapfDingbats" );


const size_t Computation::RREL_SIZE = 33;
const double Computation::RREL_TH_STEP = 0.1;
const double Computation::RREL_TABLE[RREL_SIZE] =
  { 1/3,0.3341686344960354,0.33668841107954134,
   0.3409422096583605,0.34701519474761894,
   0.35502910405887494,0.36514854168289823,
   0.3775911942845651,0.3926341399960872,
   0.41062983298282885,0.43202211298939736,
   0.45737337400783484,0.4873946524706331,
   0.522992662410542,0.5653317708668038,
   0.6159221561762755,0.6767424374123584,
   0.7504184408114359,0.8404852622717847,
   0.9517742868557572,1.0910034113732654,
   1.267683405767762,1.4955440295942208,
   1.7947857010553352,2.1956618428196286,
   2.7440092614738933,3.5090279091817442,
   4.590771723726686,6.112526284299868,8.144027798714662,
   10.441038625842998,12.076454051457787,13};

