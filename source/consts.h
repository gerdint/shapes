#ifndef consts_h
#define consts_h

#include <cmath>

#include "elementarylength.h"

#include "refcount.h"
#include <cstddef> // For size_t.



namespace Shapes
{
	namespace Interaction
	{

		extern RefCountPtr< const char > SEVERAL_TYPES;
		extern RefCountPtr< const char > PUBLIC_SCOPE_NAME;
		extern RefCountPtr< const char > PROTECTED_SCOPE_NAME;

	}

	namespace Lang
	{

		extern RefCountPtr< const char > ARCDELTA_ID;
		extern RefCountPtr< const char > TEX_SYNTAX_ID;

		extern const char * CANVAS_ID;
		extern const char * CATALOG_ID;

		extern const char * SELF_ID;

		extern const char * MESSAGE_DRAWABLE_DRAW_ID;

		extern const char * HANDLER_NO_INTERSECTION;

		extern const char * DYNAMIC_VARIABLE_ID_EYEZ;
		extern const char * DYNAMIC_VARIABLE_ID_STROKING;
		extern const char * DYNAMIC_VARIABLE_ID_NONSTROKING;
		extern const char * DYNAMIC_VARIABLE_ID_AUTOINTENSITY;

	}

	namespace Kernel
	{

		extern const char * SEQUENTIAL_EXPR_VAR_ID;
		extern const char * SPLIT_VAR_PREFIX;

	}

	namespace Computation
	{

		extern const size_t RREL_SIZE;
		extern const double RREL_TH_STEP;
		extern const double RREL_TABLE[];

		extern const double SINGULAR_TRANSFORM_LIMIT;

	}

	namespace Ast
	{

		typedef unsigned short int MemberMode;
		extern const MemberMode MEMBER_ACCESS_BITS;
		extern const MemberMode MEMBER_ACCESS_PRIVATE;
		extern const MemberMode MEMBER_ACCESS_PUBLIC_GET;
		extern const MemberMode MEMBER_ACCESS_PUBLIC_INSERT;
		extern const MemberMode MEMBER_ACCESS_PROTECTED_GET;
		extern const MemberMode MEMBER_ACCESS_PROTECTED_INSERT;
		extern const MemberMode MEMBER_CONST;
		extern const MemberMode MEMBER_METHOD;
		extern const MemberMode MEMBER_ABSTRACT;
		extern const MemberMode MEMBER_FINAL;
		extern const MemberMode MEMBER_TRANSFORMING;
		
		typedef unsigned short int ClassMode;
		extern const ClassMode CLASS_MODE_ABSTRACT;
		extern const ClassMode CLASS_MODE_FINAL;

		typedef unsigned short int FunctionMode;
		extern const FunctionMode FUNCTION_TRANSFORMING;
		extern const FunctionMode FUNCTION_PROCEDURAL;

	}

	namespace Concrete
	{

		const Length HUGE_LENGTH( HUGE_VAL );
		const Length ZERO_LENGTH( 0 );
		const Length SOME_LENGTH( 1 );
		
		const Speed ZERO_SPEED( 0 );
		
		const Time ZERO_TIME( 0 );
		const Time UNIT_TIME( 1 );
		const Time HUGE_TIME( HUGE_VAL );

	}

	namespace BuiltInFonts
	{

		extern RefCountPtr< const char > TIMES_ROMAN;
		extern RefCountPtr< const char > TIMES_BOLD;
		extern RefCountPtr< const char > TIMES_ITALIC;
		extern RefCountPtr< const char > TIMES_BOLDITALIC;
		extern RefCountPtr< const char > HELVETICA;
		extern RefCountPtr< const char > HELVETICA_BOLD;
		extern RefCountPtr< const char > HELVETICA_OBLIQUE;
		extern RefCountPtr< const char > HELVETICA_BOLDOBLIQUE;
		extern RefCountPtr< const char > COURIER;
		extern RefCountPtr< const char > COURIER_BOLD;
		extern RefCountPtr< const char > COURIER_OBLIQUE;
		extern RefCountPtr< const char > COURIER_BOLDOBLIQUE;
		extern RefCountPtr< const char > SYMBOL;
		extern RefCountPtr< const char > ZAPFDINGBATS;
		extern RefCountPtr< const char > NUM_BUILTIN_FONTS;

	}
}


#endif
