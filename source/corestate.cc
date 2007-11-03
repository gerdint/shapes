#include <cmath>

#include "metapdfcore.h"
#include "ast.h"
#include "globals.h"
#include "metapdfexceptions.h"
#include "consts.h"
#include "simplepdfi.h"
#include "pdffunctiontypes.h"

#include <iostream>
#include <sstream>
#include <fstream>
#include <vector>
#include <stdio.h>

using namespace MetaPDF;


namespace MetaPDF
{
  namespace Lang
  {

    class Core_dashpattern : public Lang::CoreFunction
    {
    public:
      Core_dashpattern( const char * title ) : CoreFunction( title ) { }
      virtual void
      call( Kernel::EvalState * evalState, Kernel::Arguments & args, const Ast::SourceLocation & callLoc ) const
      {
	RefCountPtr< std::list< Concrete::Length > > pat;

	if( args.size( ) > 0 )
	  {
	    Concrete::Length totalLength = 0;
      
	    typedef const Lang::Length ArgType;
      
	    for( size_t i = 0;
		 i < args.size( );
		 ++i )
	      {
		RefCountPtr< ArgType > arg = Helpers::down_cast_CoreArgument< ArgType >( title_, args, i, callLoc );
		if( arg->get( ) < 0 )
		  {
		    throw Exceptions::CoreOutOfRange( title_, args, i, "This dashpattern length is negative." );
		  }
		totalLength += arg->get( );
		pat->push_back( arg->get( ) );
	      }
      
	    if( totalLength == 0 )
	      {
		throw Exceptions::CoreOutOfRange( title_, args, args.size( ) - 1, "The total length of the dashpattern is 0." );
	      }
	  }

	Kernel::ContRef cont = evalState->cont_;
	cont->takeValue( Kernel::ValueRef( new Lang::Dash( pat, 0, 1 ) ),
			 evalState );
      }
    };

    class Core_noArrow : public Lang::CoreFunction
    {
    public:
      Core_noArrow( const char * title ) : CoreFunction( title ) { }
      virtual void
      call( Kernel::EvalState * evalState, Kernel::Arguments & args, const Ast::SourceLocation & callLoc ) const
      {
	const size_t ARITY = 1;
	CHECK_ARITY( args, ARITY, title_ );
  
	// We don't even check the type here, even though we could have done so...

	Kernel::ContRef cont = evalState->cont_;
	cont->takeValue( Kernel::ValueRef( new Lang::Length( 0 ) ),
			 evalState );
      }
    };

    class Core_gray : public Lang::CoreFunction
    {
    public:
      Core_gray( const char * title ) : CoreFunction( title ) { }
      virtual void
      call( Kernel::EvalState * evalState, Kernel::Arguments & args, const Ast::SourceLocation & callLoc ) const
      {
	const size_t ARITY = 1;
	CHECK_ARITY( args, ARITY, title_ );
  
	typedef const Lang::Float ArgType;
	RefCountPtr< ArgType > arg = Helpers::down_cast_CoreArgument< ArgType >( title_, args, 0, callLoc );

	if( arg->val_ < 0 )
	  {
	    throw Exceptions::CoreOutOfRange( title_, args, 0, "The gray level is less than 0." );
	  }
	if( arg->val_ > 1 )
	  {
	    throw Exceptions::CoreOutOfRange( title_, args, 0, "The gray level is greater than 1." );
	  }

	Kernel::ContRef cont = evalState->cont_;
	cont->takeValue( Kernel::ValueRef( new Lang::Gray( Concrete::Gray( arg->val_ ) ) ),
			 evalState );
      }
    };

    class Core_rgb : public Lang::CoreFunction
    {
    public:
      Core_rgb( const char * title ) : CoreFunction( title ) { }
      virtual void
      call( Kernel::EvalState * evalState, Kernel::Arguments & args, const Ast::SourceLocation & callLoc ) const
      {
	const size_t ARITY = 3;
	CHECK_ARITY( args, ARITY, title_ );
  
	double parts[ ARITY ];

	typedef const Lang::Float ArgType;
	double * dst( parts );
	for( size_t i = 0;
	     i < args.size( );
	     ++i, ++dst )
	  {
	    RefCountPtr< ArgType > arg = Helpers::down_cast_CoreArgument< ArgType >( title_, args, i, callLoc );
	    if( arg->val_ < 0 )
	      {
		throw Exceptions::CoreOutOfRange( title_, args, i, "This RGB level is less than 0." );
	      }
	    if( arg->val_ > 1 )
	      {
		throw Exceptions::CoreOutOfRange( title_, args, i, "This RGB level is greater than 1." );
	      }
	    *dst = arg->val_;
	  }

	Kernel::ContRef cont = evalState->cont_;
	cont->takeValue( Kernel::ValueRef( new Lang::RGB( Concrete::RGB( parts[0], parts[1], parts[2] ) ) ),
			 evalState );
      }
    };

    class Core_cmyk : public Lang::CoreFunction
    {
    public:
      Core_cmyk( const char * title ) : CoreFunction( title ) { }
      virtual void
      call( Kernel::EvalState * evalState, Kernel::Arguments & args, const Ast::SourceLocation & callLoc ) const
      {
	const size_t ARITY = 4;
	CHECK_ARITY( args, ARITY, title_ );
  
	double parts[ ARITY ];

	typedef const Lang::Float ArgType;
	double * dst( parts );
	for( size_t i = 0;
	     i < args.size( );
	     ++i, ++dst )
	  {
	    RefCountPtr< ArgType > arg = Helpers::down_cast_CoreArgument< ArgType >( title_, args, i, callLoc );
	    if( arg->val_ < 0 )
	      {
		throw Exceptions::CoreOutOfRange( title_, args, i, "This CMYK level is less than 0." );
	      }
	    if( arg->val_ > 1 )
	      {
		throw Exceptions::CoreOutOfRange( title_, args, i, "This CMYK level is greater than 1." );
	      }
	    *dst = arg->val_;
	  }

	Kernel::ContRef cont = evalState->cont_;
	cont->takeValue( Kernel::ValueRef( new Lang::CMYK( Concrete::CMYK( parts[0], parts[1], parts[2], parts[3] ) ) ),
			 evalState );
      }
    };

    class Core_shape : public Lang::CoreFunction
    {
    public:
      Core_shape( const char * title ) : CoreFunction( title ) { }
      virtual void
      call( Kernel::EvalState * evalState, Kernel::Arguments & args, const Ast::SourceLocation & callLoc ) const
      {
	const size_t ARITY = 1;
	CHECK_ARITY( args, ARITY, title_ );
  
	typedef const Lang::Float ArgType;
	RefCountPtr< ArgType > arg = Helpers::down_cast_CoreArgument< ArgType >( title_, args, 0, callLoc );

	if( arg->val_ < 0 )
	  {
	    throw Exceptions::CoreOutOfRange( title_, args, 0, "The alpha level is less than 0." );
	  }
	if( arg->val_ > 1 )
	  {
	    throw Exceptions::CoreOutOfRange( title_, args, 0, "The alpha level is greater than 1." );
	  }

	Kernel::ContRef cont = evalState->cont_;
	cont->takeValue( Kernel::ValueRef( new Lang::Alpha( true, arg->val_ ) ),
			 evalState );
      }
    };

    class Core_opacity : public Lang::CoreFunction
    {
    public:
      Core_opacity( const char * title ) : CoreFunction( title ) { }
      virtual void
      call( Kernel::EvalState * evalState, Kernel::Arguments & args, const Ast::SourceLocation & callLoc ) const
      {
	const size_t ARITY = 1;
	CHECK_ARITY( args, ARITY, title_ );
  
	typedef const Lang::Float ArgType;
	RefCountPtr< ArgType > arg = Helpers::down_cast_CoreArgument< ArgType >( title_, args, 0, callLoc );

	if( arg->val_ < 0 )
	  {
	    throw Exceptions::CoreOutOfRange( title_, args, 0, "The alpha level is less than 0." );
	  }
	if( arg->val_ > 1 )
	  {
	    throw Exceptions::CoreOutOfRange( title_, args, 0, "The alpha level is greater than 1." );
	  }

	Kernel::ContRef cont = evalState->cont_;
	cont->takeValue( Kernel::ValueRef( new Lang::Alpha( false, arg->val_ ) ),
			 evalState );
      }
    };

    class Core_alphamask : public Lang::CoreFunction
    {
    public:
      Core_alphamask( const char * title )
	: CoreFunction( title, new Kernel::EvaluatedFormals( title, true ) )
      {
	formals_->appendEvaluatedCoreFormal( "group", Kernel::THE_SLOT_VARIABLE );
	formals_->appendEvaluatedCoreFormal( "transform", Kernel::THE_VOID_VARIABLE );
      }
      virtual void
      call( Kernel::EvalState * evalState, Kernel::Arguments & args, const Ast::SourceLocation & callLoc ) const
      {
	args.applyDefaults( );

	size_t i = 0;
  
	typedef const Lang::TransparencyGroup ArgType0;
	RefCountPtr< ArgType0 > group = Helpers::down_cast_CoreArgument< ArgType0 >( title_, args, i, callLoc );

	++i;
	typedef const Lang::PDF_Function ArgType1;
	RefCountPtr< ArgType1 > transform = Helpers::down_cast_CoreArgument< ArgType1 >( title_, args, i, callLoc, true );
	if( transform != NullPtr< ArgType1 >( ) &&
	    ! transform->matchesDimensions( 1, 1 ) )
	  {
	    throw Exceptions::CoreOutOfRange( title_, args, i, "The transform function must have dimensions ( 1, 1 )." );
	  }
  
	Kernel::ContRef cont = evalState->cont_;
	cont->takeValue( Kernel::ValueRef( new Lang::SoftMask( Lang::SoftMask::ALPHA,
							       group,
							       RefCountPtr< const Lang::Color >( NullPtr< const Lang::Color >( ) ),
							       transform ) ),
			 evalState );
      }
    };

    class Core_luminositymask : public Lang::CoreFunction
    {
    public:
      Core_luminositymask( const char * title )
	: CoreFunction( title, new Kernel::EvaluatedFormals( title, true ) )
      {
	formals_->appendEvaluatedCoreFormal( "group", Kernel::THE_SLOT_VARIABLE );
	formals_->appendEvaluatedCoreFormal( "background", Kernel::THE_VOID_VARIABLE );
	formals_->appendEvaluatedCoreFormal( "transform", Kernel::THE_VOID_VARIABLE );
      }
      virtual void
      call( Kernel::EvalState * evalState, Kernel::Arguments & args, const Ast::SourceLocation & callLoc ) const
      {
	args.applyDefaults( );

	size_t i = 0;
  
	typedef const Lang::TransparencyGroup ArgType0;
	RefCountPtr< ArgType0 > group = Helpers::down_cast_CoreArgument< ArgType0 >( title_, args, i, callLoc );
	if( group->colorSpace( )->isInherent( ) )
	  {
	    throw Exceptions::CoreOutOfRange( title_, args, i, "The transparency group must not use an inherited blend space." );
	  }

	++i;
	typedef const Lang::Color ArgType1;
	RefCountPtr< ArgType1 > background = Helpers::down_cast_CoreArgument< ArgType1 >( title_, args, i, callLoc, true );

	++i;
	typedef const Lang::PDF_Function ArgType2;
	RefCountPtr< ArgType2 > transform = Helpers::down_cast_CoreArgument< ArgType2 >( title_, args, i, callLoc, true );
	if( transform != NullPtr< ArgType2 >( ) &&
	    ! transform->matchesDimensions( 1, 1 ) )
	  {
	    throw Exceptions::CoreOutOfRange( title_, args, i, "The transform function must have dimensions ( 1, 1 )." );
	  }
  
	Kernel::ContRef cont = evalState->cont_;
	cont->takeValue( Kernel::ValueRef( new Lang::SoftMask( Lang::SoftMask::LUMINOSITY,
							       group,
							       background,
							       transform ) ),
			 evalState );
      }
    };

  }
}


RefCountPtr< const Lang::CoreFunction > Lang::THE_NO_ARROW( new Lang::Core_noArrow( "no_arrow" ) );

void
Kernel::registerCore_state( Kernel::Environment * env )
{
  env->initDefineCoreFunction( new Lang::Core_dashpattern( "dashpattern" ) );
  env->initDefineCoreFunction( new Lang::Core_gray( "gray" ) );
  env->initDefineCoreFunction( new Lang::Core_rgb( "rgb" ) );
  env->initDefineCoreFunction( new Lang::Core_cmyk( "cmyk" ) );
  env->initDefineCoreFunction( new Lang::Core_shape( "shape" ) );
  env->initDefineCoreFunction( new Lang::Core_opacity( "opacity" ) );
  env->initDefineCoreFunction( new Lang::Core_alphamask( "alphamask" ) );
  env->initDefineCoreFunction( new Lang::Core_luminositymask( "luminositymask" ) );
}

