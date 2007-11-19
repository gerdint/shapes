#ifndef classtreemacros_h
#define classtreemacros_h


#define DISPATCHSTYLE_NONE 0
#define DISPATCHSTYLE_VOID 1
#define DISPATCHSTYLE_VTBL 2
#define DISPATCHSTYLE_CASE 3

/* The closest to the one level recursion needed is to start from two sets of symmetric macros.
 * One set ends in "1", the other ends in "2".
 */

/* Define the class heierarcy in macros:
 */
#include "classtree1.h"
#include "classtree2.h"

#include "Shapes_Ast_decls.h"
#include "Shapes_Kernel_decls.h"

#include "SimplePDF_decls.h"

/* Define convenient macros for looping.
 */
#define SINGLELOOP1_( Sb, M, T ) M( T )
#define SINGLELOOP1( Sa, M ) Sa( SINGLELOOP1_,, M )
#define DOUBLELOOP1__( M, Ta, Tb ) M( Ta, Tb )
#define DOUBLELOOP1_( Sb, M, Ta ) Sb( DOUBLELOOP1__, M, Ta )
#define DOUBLELOOP1( Sa, Sb, M ) Sa( DOUBLELOOP1_, Sb, M )

#define SINGLELOOP2_( Sb, M, T ) M( T )
#define SINGLELOOP2( Sa, M ) Sa( SINGLELOOP2_,, M )
#define DOUBLELOOP2__( M, Ta, Tb ) M( Ta, Tb )
#define DOUBLELOOP2_( Sb, M, Ta ) Sb( DOUBLELOOP2__, M, Ta )
#define DOUBLELOOP2( Sa, Sb, M ) Sa( DOUBLELOOP2_, Sb, M )

/* Define a macro that only uses the first argument,
 * resulting in a macro that calls another macro for each class.
 */
#define FORALLCLASSESM( M ) SINGLELOOP1( CLASSTREE1_ROOT, M )

/* Define a macro that calls another macro with a given first argument,
 * varying the second argument over all classes.
 */
#define FORALLCLASSESMT( M, T ) CLASSTREE2_ROOT( DOUBLELOOP2__, M, T )


/* Declare all the Lang::Value children classes
 */
#define DECLARECLASS_( T ) class T;
namespace Shapes
{
	namespace Lang
	{
		FORALLCLASSESM( DECLARECLASS_ )
	}
}

#define QUICKTYPECASE( T ) case Kernel::TYPEID_ ## T :
#define DUMMYANDREF( T ) T *, const RefCountPtr< T > &

#if DISPATCHSTYLE != DISPATCHSTYLE_NONE
#include "operatorside.h"
#else
#include "operatorside_none.h"
#endif


/* Now define macros to be used in the Value derivatives.
 */

#if DISPATCHSTYLE == DISPATCHSTYLE_NONE
#include "dispatch_none.h"
#elif DISPATCHSTYLE == DISPATCHSTYLE_VOID
#include "dispatch_void.h"
#elif DISPATCHSTYLE == DISPATCHSTYLE_VTBL
#include "dispatch_vtbl.h"
#elif DISPATCHSTYLE == DISPATCHSTYLE_CASE
#include "dispatch_case.h"
#else
#error DISPATCHSTYLE switch out of range
#endif


#define GETTYPEIDDECL virtual Kernel::QuickTypeID getTypeID( ) const;

/* To be used in each value type:
 */

#define DISPATCHBASEDECL UNARYDISPATCHBASEDECL BINARYDISPATCHBASEDECL
#define DISPATCHBASEIMPL UNARYDISPATCHBASEIMPL BINARYDISPATCHBASEIMPL
#define DISPATCHDECL UNARYDISPATCHDECL BINARYDISPATCHDECL GETTYPEIDDECL
#define DISPATCHIMPL( Ts ) UNARYDISPATCHIMPL( Ts ) BINARYDISPATCHIMPL( Ts )

#endif
