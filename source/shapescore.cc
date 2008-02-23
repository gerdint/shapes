#include <cmath>

#include "Shapes_Helpers_decls.h"

#include "shapescore.h"
#include "globals.h"
#include "shapesexceptions.h"
#include "astexpr.h"
#include "astfun.h"
#include "consts.h"
#include "simplepdfi.h"

#include <iostream>
#include <sstream>
#include <fstream>
#include <vector>
#include <stdio.h>

using namespace Shapes;


RefCountPtr< const Lang::ElementaryPath2D >
Helpers::elementaryPathCast2D( const char * title, Kernel::Arguments & args, size_t argNo, const Ast::SourceLocation & callLoc )
{
	RefCountPtr< const Lang::Value > ptr = args.getValue( argNo );
	typedef const Lang::ElementaryPath2D ArgType;
	RefCountPtr< ArgType > res = ptr.down_cast< ArgType >( );
	if( res != NullPtr< ArgType >( ) )
		{
			return res;
		}

	typedef const Lang::CompositePath2D ArgType2;
	ArgType2 * res2 = dynamic_cast< ArgType2 * >( ptr.getPtr( ) );
	if( res2 == 0 )
		{
			throw Exceptions::CoreTypeMismatch( callLoc, title, args, argNo, ArgType::staticTypeName( ) );
		}

	return res2->getElementaryPath( );
}

RefCountPtr< const Lang::ElementaryPath2D >
Helpers::elementaryPathCast2D( const RefCountPtr< const Lang::Value > & ptr, const Kernel::Continuation * loc )
{
	typedef const Lang::ElementaryPath2D ArgType;
	RefCountPtr< ArgType > res = ptr.down_cast< ArgType >( );
	if( res != NullPtr< ArgType >( ) )
		{
			return res;
		}

	typedef const Lang::CompositePath2D ArgType2;
	ArgType2 * res2 = dynamic_cast< ArgType2 * >( ptr.getPtr( ) );
	if( res2 == 0 )
		{
			throw Exceptions::ContinuationTypeMismatch( loc, ptr->getTypeName( ), ArgType::staticTypeName( ) );
		}

	return res2->getElementaryPath( );
}

RefCountPtr< const Lang::ElementaryPath2D >
Helpers::elementaryPathTry2D( const RefCountPtr< const Lang::Value > & ptr )
{
	typedef const Lang::ElementaryPath2D ArgType;
	RefCountPtr< ArgType > res = ptr.down_cast< ArgType >( );
	if( res != NullPtr< ArgType >( ) )
		{
			return res;
		}

	typedef const Lang::CompositePath2D ArgType2;
	ArgType2 * res2 = dynamic_cast< ArgType2 * >( ptr.getPtr( ) );
	if( res2 == 0 )
		{
			throw NonLocalExit::NotThisType( );
		}

	return res2->getElementaryPath( );
}


Concrete::SplineTime
Helpers::pathTimeCast( const char * title, const RefCountPtr< const Lang::ElementaryPath2D > & pRef, Kernel::Arguments & args, size_t argNo, const Ast::SourceLocation & callLoc )
{
	const Lang::ElementaryPath2D * p = pRef.getPtr( );
	const Lang::Value * tPtr = args.getValue( argNo ).getPtr( );
	typedef const Lang::Float ArgType;
	ArgType * res = dynamic_cast< ArgType * >( tPtr );
	if( res != 0 )
		{
			return Concrete::Time( res->val_ );
		}

	typedef const Lang::Length ArgType2;
	ArgType2 * res2 = dynamic_cast< ArgType2 * >( tPtr );
	if( res2 == 0 )
		{
			throw Exceptions::CoreTypeMismatch( callLoc, title, args, argNo, typeSetString( ArgType::staticTypeName( ), ArgType::staticTypeName( ) ) );
		}

	return p->arcTime( res2->get( ) );
}

Concrete::SplineTime
Helpers::pathTimeCast( const Lang::ElementaryPath2D * p, const Lang::Value * tPtr, const Kernel::Continuation * loc )
{
	typedef const Lang::Float ArgType;
	ArgType * res = dynamic_cast< ArgType * >( tPtr );
	if( res != 0 )
		{
			return Concrete::Time( res->val_ );
		}

	typedef const Lang::Length ArgType2;
	ArgType2 * res2 = dynamic_cast< ArgType2 * >( tPtr );
	if( res2 == 0 )
		{
			throw Exceptions::ContinuationTypeMismatch( loc, tPtr->getTypeName( ), typeSetString( ArgType::staticTypeName( ), ArgType2::staticTypeName( ) ) );
		}

	return p->arcTime( res2->get( ) );
}


RefCountPtr< const Lang::ElementaryPath3D >
Helpers::elementaryPathCast3D( const char * title, Kernel::Arguments & args, size_t argNo, const Ast::SourceLocation & callLoc )
{
	RefCountPtr< const Lang::Value > ptr = args.getValue( argNo );
	typedef const Lang::ElementaryPath3D ArgType;
	RefCountPtr< ArgType > res = ptr.down_cast< ArgType >( );
	if( res != NullPtr< ArgType >( ) )
		{
			return res;
		}

	typedef const Lang::CompositePath3D ArgType2;
	ArgType2 * res2 = dynamic_cast< ArgType2 * >( ptr.getPtr( ) );
	if( res2 == 0 )
		{
			throw Exceptions::CoreTypeMismatch( callLoc, title, args, argNo, ArgType::staticTypeName( ) );
		}

	return res2->getElementaryPath( );
}

RefCountPtr< const Lang::ElementaryPath3D >
Helpers::elementaryPathCast3D( const RefCountPtr< const Lang::Value > & ptr, const Kernel::Continuation * loc )
{
	typedef const Lang::ElementaryPath3D ArgType;
	RefCountPtr< ArgType > res = ptr.down_cast< ArgType >( );
	if( res != NullPtr< ArgType >( ) )
		{
			return res;
		}

	typedef const Lang::CompositePath3D ArgType2;
	ArgType2 * res2 = dynamic_cast< ArgType2 * >( ptr.getPtr( ) );
	if( res2 == 0 )
		{
			throw Exceptions::ContinuationTypeMismatch( loc, ptr->getTypeName( ), ArgType::staticTypeName( ) );
		}

	return res2->getElementaryPath( );
}

RefCountPtr< const Lang::ElementaryPath3D >
Helpers::elementaryPathTry3D( const RefCountPtr< const Lang::Value > & ptr )
{
	typedef const Lang::ElementaryPath3D ArgType;
	RefCountPtr< ArgType > res = ptr.down_cast< ArgType >( );
	if( res != NullPtr< ArgType >( ) )
		{
			return res;
		}

	typedef const Lang::CompositePath3D ArgType2;
	ArgType2 * res2 = dynamic_cast< ArgType2 * >( ptr.getPtr( ) );
	if( res2 == 0 )
		{
			throw NonLocalExit::NotThisType( );
		}

	return res2->getElementaryPath( );
}


Concrete::SplineTime
Helpers::pathTimeCast( const char * title, const RefCountPtr< const Lang::ElementaryPath3D > & pRef, Kernel::Arguments & args, size_t argNo, const Ast::SourceLocation & callLoc )
{
	const Lang::ElementaryPath3D * p = pRef.getPtr( );
	const Lang::Value * tPtr = args.getValue( argNo ).getPtr( );
	typedef const Lang::Float ArgType;
	ArgType * res = dynamic_cast< ArgType * >( tPtr );
	if( res != 0 )
		{
			return Concrete::Time( res->val_ );
		}

	typedef const Lang::Length ArgType2;
	ArgType2 * res2 = dynamic_cast< ArgType2 * >( tPtr );
	if( res2 == 0 )
		{
			throw Exceptions::CoreTypeMismatch( callLoc, title, args, argNo, typeSetString( ArgType::staticTypeName( ), ArgType::staticTypeName( ) ) );
		}

	return p->arcTime( res2->get( ) );
}

Concrete::SplineTime
Helpers::pathTimeCast( const Lang::ElementaryPath3D * p, const Lang::Value * tPtr, const Kernel::Continuation * loc )
{
	typedef const Lang::Float ArgType;
	ArgType * res = dynamic_cast< ArgType * >( tPtr );
	if( res != 0 )
		{
			return Concrete::Time( res->val_ );
		}

	typedef const Lang::Length ArgType2;
	ArgType2 * res2 = dynamic_cast< ArgType2 * >( tPtr );
	if( res2 == 0 )
		{
			throw Exceptions::ContinuationTypeMismatch( loc, tPtr->getTypeName( ), typeSetString( ArgType::staticTypeName( ), ArgType2::staticTypeName( ) ) );
		}

	return p->arcTime( res2->get( ) );
}


RefCountPtr< const char >
Helpers::typeSetString( RefCountPtr< const char > type1, RefCountPtr< const char > type2 )
{
	std::list< RefCountPtr< const char > > types;
	types.push_back( type1 );
	types.push_back( type2 );
	return typeSetString( types );
}

RefCountPtr< const char >
Helpers::typeSetString( RefCountPtr< const char > type1, RefCountPtr< const char > type2, RefCountPtr< const char > type3 )
{
	std::list< RefCountPtr< const char > > types;
	types.push_back( type1 );
	types.push_back( type2 );
	types.push_back( type3 );
	return typeSetString( types );
}

RefCountPtr< const char >
Helpers::typeSetString( RefCountPtr< const char > type1, RefCountPtr< const char > type2, RefCountPtr< const char > type3, RefCountPtr< const char > type4 )
{
	std::list< RefCountPtr< const char > > types;
	types.push_back( type1 );
	types.push_back( type2 );
	types.push_back( type3 );
	types.push_back( type4 );
	return typeSetString( types );
}

RefCountPtr< const char >
Helpers::typeSetString( RefCountPtr< const char > type1, RefCountPtr< const char > type2, RefCountPtr< const char > type3, RefCountPtr< const char > type4, RefCountPtr< const char > type5 )
{
	std::list< RefCountPtr< const char > > types;
	types.push_back( type1 );
	types.push_back( type2 );
	types.push_back( type3 );
	types.push_back( type4 );
	types.push_back( type5 );
	return typeSetString( types );
}

RefCountPtr< const char >
Helpers::typeSetString( const std::list< RefCountPtr< const char > > types )
{
	std::ostringstream oss;
	oss << "any of {" ;
	if( ! types.empty( ) )
		{
			typedef typeof types ListType;
			ListType::const_iterator i = types.begin( );
			oss << " " << *i ;
			++i;
			for( ; i != types.end( ); ++i )
				{
					oss << ", " << *i ;
				}
		}
	oss << " }" ;
	return strrefdup( oss );
}

Lang::CoreFunction::CoreFunction( const char * title )
	: Lang::Function( new Kernel::EvaluatedFormals( title, true ) ), title_( title )
{ }

Lang::CoreFunction::CoreFunction( const char * title, Kernel::EvaluatedFormals * formals )
	: Lang::Function( formals ), title_( title )
{ }

void
Lang::CoreFunction::analyze( Ast::Node * parent, const Ast::AnalysisEnvironment * env )
{
	/* Nothing to be done.
	 */
}

bool
Lang::CoreFunction::isTransforming( ) const
{
	return false;
}

const char *
Lang::CoreFunction::getTitle( ) const
{
	return title_;
}

