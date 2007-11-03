#include <cmath>

#include "Shapes_Helpers_decls.h"

#include "shapescore.h"
#include "ast.h"
#include "globals.h"
#include "shapesexceptions.h"
#include "consts.h"
#include "simplepdfi.h"

#include <iostream>
#include <sstream>
#include <fstream>
#include <vector>
#include <stdio.h>

using namespace Shapes;


namespace Shapes
{
  namespace Lang
  {
    class Core_duration : public Lang::CoreFunction
    {
    public:
      Core_duration( const char * title ) : CoreFunction( title ) { }
      virtual void
      call( Kernel::EvalState * evalState, Kernel::Arguments & args, const Ast::SourceLocation & callLoc ) const
      {
	const size_t ARITY = 1;
	CHECK_ARITY( args, ARITY, title_ );
  
	try
	  {
	    typedef const Lang::ElementaryPath2D ArgType;
	    RefCountPtr< ArgType > arg = Helpers::elementaryPathTry2D( args.getValue( 0 ) );
	    Kernel::ContRef cont = evalState->cont_;
	    cont->takeValue( Kernel::ValueRef( new Lang::Integer( arg->duration( ) ) ),
			     evalState );
	    return;
	  }
	catch( const NonLocalExit::NotThisType & ball )
	  {
	    /* Wrong type; never mind!.. but see below!
	     */
	  }

	try
	  {
	    typedef const Lang::ElementaryPath3D ArgType;
	    RefCountPtr< ArgType > arg = Helpers::elementaryPathTry3D( args.getValue( 0 ) );
	    Kernel::ContRef cont = evalState->cont_;
	    cont->takeValue( Kernel::ValueRef( new Lang::Integer( arg->duration( ) ) ),
			     evalState );
	    return;
	  }
	catch( const NonLocalExit::NotThisType & ball )
	  {
	    /* Wrong type; never mind!.. but see below!
	     */
	  }
  
	throw Exceptions::CoreTypeMismatch( callLoc, title_, args, 0, Helpers::typeSetString( Lang::ElementaryPath2D::staticTypeName( ), Lang::ElementaryPath3D::staticTypeName( ) ) );
      }
    };

    class Core_controlling_maximizer : public Lang::CoreFunction
    {
    public:
      Core_controlling_maximizer( const char * title ) : CoreFunction( title ) { }
      virtual void
      call( Kernel::EvalState * evalState, Kernel::Arguments & args, const Ast::SourceLocation & callLoc ) const
      {
	const size_t ARITY = 2;
	CHECK_ARITY( args, ARITY, title_ );
  
	{
	  typedef const Lang::ElementaryPath2D ArgType0;
	  RefCountPtr< ArgType0 > p = NullPtr< ArgType0 >( );
	  size_t argsi = 0;
	  try
	    {
	      p = Helpers::elementaryPathTry2D( args.getValue( argsi ) );
	    }
	  catch( const NonLocalExit::NotThisType & ball )
	    {
	      goto nextType1;
	    }

	  ++argsi;
  
	  typedef const Lang::FloatPair ArgType1;
	  RefCountPtr< ArgType1 > d = Helpers::down_cast_CoreArgument< ArgType1 >( title_, args, argsi, callLoc );
  
	  Kernel::ContRef cont = evalState->cont_;
	  cont->takeValue( p->controllingMaximizer( *d ),
			   evalState );
	  return;
	}

      nextType1:

	{
	  typedef const Lang::ElementaryPath3D ArgType0;
	  RefCountPtr< ArgType0 > p = NullPtr< ArgType0 >( );
	  size_t argsi = 0;
	  try
	    {
	      p = Helpers::elementaryPathTry3D( args.getValue( argsi ) );
	    }
	  catch( const NonLocalExit::NotThisType & ball )
	    {
	      goto nextType2;
	    }

	  ++argsi;
  
	  typedef const Lang::FloatTriple ArgType1;
	  RefCountPtr< ArgType1 > d = Helpers::down_cast_CoreArgument< ArgType1 >( title_, args, argsi, callLoc );
  
	  Kernel::ContRef cont = evalState->cont_;
	  cont->takeValue( p->controllingMaximizer( *d ),
			   evalState );
	  return;
	}

      nextType2:
	throw Exceptions::CoreTypeMismatch( callLoc, title_, args, 0, Helpers::typeSetString( Lang::ElementaryPath2D::staticTypeName( ), Lang::ElementaryPath3D::staticTypeName( ) ) );
      }
    };

    class Core_discrete_mean : public Lang::CoreFunction
    {
    public:
      Core_discrete_mean( const char * title ) : CoreFunction( title ) { }
      virtual void
      call( Kernel::EvalState * evalState, Kernel::Arguments & args, const Ast::SourceLocation & callLoc ) const
      {
	const size_t ARITY = 1;
	CHECK_ARITY( args, ARITY, title_ );
  
	size_t argsi = 0;

	try
	  {
	    typedef const Lang::ElementaryPath2D ArgType;
	    RefCountPtr< ArgType > p = Helpers::elementaryPathTry2D( args.getValue( argsi ) );
	    Kernel::ContRef cont = evalState->cont_;
	    cont->takeValue( p->discreteMean( ),
			     evalState );
	    return;
	  }
	catch( const NonLocalExit::NotThisType & ball )
	  {
	    /* Wrong type; never mind!.. but see below!
	     */
	  }

	try
	  {
	    typedef const Lang::ElementaryPath3D ArgType;
	    RefCountPtr< ArgType > p = Helpers::elementaryPathTry3D( args.getValue( argsi ) );
	    Kernel::ContRef cont = evalState->cont_;
	    cont->takeValue( p->discreteMean( ),
			     evalState );
	    return;
	  }
	catch( const NonLocalExit::NotThisType & ball )
	  {
	    /* Wrong type; never mind!.. but see below!
	     */
	  }
  
	throw Exceptions::CoreTypeMismatch( callLoc, title_, args, 0, Helpers::typeSetString( Lang::ElementaryPath2D::staticTypeName( ), Lang::ElementaryPath3D::staticTypeName( ) ) );
      }
    };

    class Core_discrete_maximizer : public Lang::CoreFunction
    {
    public:
      Core_discrete_maximizer( const char * title ) : CoreFunction( title ) { }
      virtual void
      call( Kernel::EvalState * evalState, Kernel::Arguments & args, const Ast::SourceLocation & callLoc ) const
      {
	const size_t ARITY = 2;
	CHECK_ARITY( args, ARITY, title_ );
  
	{
	  typedef const Lang::ElementaryPath2D ArgType0;
	  RefCountPtr< ArgType0 > p = NullPtr< ArgType0 >( );
	  size_t argsi = 0;
	  try
	    {
	      p = Helpers::elementaryPathTry2D( args.getValue( argsi ) );
	    }
	  catch( const NonLocalExit::NotThisType & ball )
	    {
	      goto nextType1;
	    }

	  ++argsi;
  
	  typedef const Lang::FloatPair ArgType1;
	  RefCountPtr< ArgType1 > d = Helpers::down_cast_CoreArgument< ArgType1 >( title_, args, argsi, callLoc );
  
	  Kernel::ContRef cont = evalState->cont_;
	  typedef const Lang::PathSlider2D ResultType;
	  cont->takeValue( RefCountPtr< ResultType >( new ResultType( p, p->discreteMaximizer( *d ) ) ),
			   evalState );
	  return;
	}

      nextType1:

	{
	  typedef const Lang::ElementaryPath3D ArgType0;
	  RefCountPtr< ArgType0 > p = NullPtr< ArgType0 >( );
	  size_t argsi = 0;
	  try
	    {
	      p = Helpers::elementaryPathTry3D( args.getValue( argsi ) );
	    }
	  catch( const NonLocalExit::NotThisType & ball )
	    {
	      goto nextType2;
	    }

	  ++argsi;
  
	  typedef const Lang::FloatTriple ArgType1;
	  RefCountPtr< ArgType1 > d = Helpers::down_cast_CoreArgument< ArgType1 >( title_, args, argsi, callLoc );
  
	  Kernel::ContRef cont = evalState->cont_;
	  typedef const Lang::PathSlider3D ResultType;
	  cont->takeValue( RefCountPtr< ResultType >( new ResultType( p, p->discreteMaximizer( *d ) ) ),
			   evalState );
	  return;
	}

      nextType2:
	throw Exceptions::CoreTypeMismatch( callLoc, title_, args, 0, Helpers::typeSetString( Lang::ElementaryPath2D::staticTypeName( ), Lang::ElementaryPath3D::staticTypeName( ) ) );
      }
    };

    class Core_discrete_approximator : public Lang::CoreFunction
    {
    public:
      Core_discrete_approximator( const char * title ) : CoreFunction( title ) { }
      virtual void
      call( Kernel::EvalState * evalState, Kernel::Arguments & args, const Ast::SourceLocation & callLoc ) const
      {
	const size_t ARITY = 2;
	CHECK_ARITY( args, ARITY, title_ );
  
	{
	  typedef const Lang::ElementaryPath2D ArgType0;
	  RefCountPtr< ArgType0 > p = NullPtr< ArgType0 >( );
	  size_t argsi = 0;
	  try
	    {
	      p = Helpers::elementaryPathTry2D( args.getValue( argsi ) );
	    }
	  catch( const NonLocalExit::NotThisType & ball )
	    {
	      goto nextType1;
	    }

	  ++argsi;
  
	  typedef const Lang::Coords2D ArgType1;
	  RefCountPtr< ArgType1 > d = Helpers::down_cast_CoreArgument< ArgType1 >( title_, args, argsi, callLoc );
  
	  Kernel::ContRef cont = evalState->cont_;
	  typedef const Lang::PathSlider2D ResultType;
	  cont->takeValue( RefCountPtr< ResultType >( new ResultType( p, p->discreteApproximator( *d ) ) ),
			   evalState );
	  return;
	}

      nextType1:

	{
	  typedef const Lang::ElementaryPath3D ArgType0;
	  RefCountPtr< ArgType0 > p = NullPtr< ArgType0 >( );
	  size_t argsi = 0;
	  try
	    {
	      p = Helpers::elementaryPathTry3D( args.getValue( argsi ) );
	    }
	  catch( const NonLocalExit::NotThisType & ball )
	    {
	      goto nextType2;
	    }

	  ++argsi;
  
	  typedef const Lang::Coords3D ArgType1;
	  RefCountPtr< ArgType1 > d = Helpers::down_cast_CoreArgument< ArgType1 >( title_, args, argsi, callLoc );
  
	  Kernel::ContRef cont = evalState->cont_;
	  typedef const Lang::PathSlider3D ResultType;
	  cont->takeValue( RefCountPtr< ResultType >( new ResultType( p, p->discreteApproximator( *d ) ) ),
			   evalState );
	  return;
	}

      nextType2:
	throw Exceptions::CoreTypeMismatch( callLoc, title_, args, 0, Helpers::typeSetString( Lang::ElementaryPath2D::staticTypeName( ), Lang::ElementaryPath3D::staticTypeName( ) ) );
      }
    };

    class Core_continuous_mean : public Lang::CoreFunction
    {
    public:
      Core_continuous_mean( const char * title ) : CoreFunction( title ) { }
      virtual void
      call( Kernel::EvalState * evalState, Kernel::Arguments & args, const Ast::SourceLocation & callLoc ) const
      {
	const size_t ARITY = 1;
	CHECK_ARITY( args, ARITY, title_ );
  
	size_t argsi = 0;

	try
	  {
	    typedef const Lang::ElementaryPath2D ArgType;
	    RefCountPtr< ArgType > p = Helpers::elementaryPathTry2D( args.getValue( argsi ) );
	    Kernel::ContRef cont = evalState->cont_;
	    cont->takeValue( p->continuousMean( ),
			     evalState );
	    return;
	  }
	catch( const NonLocalExit::NotThisType & ball )
	  {
	    /* Wrong type; never mind!.. but see below!
	     */
	  }

	try
	  {
	    typedef const Lang::ElementaryPath3D ArgType;
	    RefCountPtr< ArgType > p = Helpers::elementaryPathTry3D( args.getValue( argsi ) );
	    Kernel::ContRef cont = evalState->cont_;
	    cont->takeValue( p->continuousMean( ),
			     evalState );
	    return;
	  }
	catch( const NonLocalExit::NotThisType & ball )
	  {
	    /* Wrong type; never mind!.. but see below!
	     */
	  }
  
	throw Exceptions::CoreTypeMismatch( callLoc, title_, args, 0, Helpers::typeSetString( Lang::ElementaryPath2D::staticTypeName( ), Lang::ElementaryPath3D::staticTypeName( ) ) );
      }
    };

    class Core_continuous_maximizer : public Lang::CoreFunction
    {
    public:
      Core_continuous_maximizer( const char * title ) : CoreFunction( title ) { }
      virtual void
      call( Kernel::EvalState * evalState, Kernel::Arguments & args, const Ast::SourceLocation & callLoc ) const
      {
	const size_t ARITY = 2;
	CHECK_ARITY( args, ARITY, title_ );
  
	{
	  typedef const Lang::ElementaryPath2D ArgType0;
	  RefCountPtr< ArgType0 > p = NullPtr< ArgType0 >( );
	  size_t argsi = 0;
	  try
	    {
	      p = Helpers::elementaryPathTry2D( args.getValue( argsi ) );
	    }
	  catch( const NonLocalExit::NotThisType & ball )
	    {
	      goto nextType1;
	    }

	  ++argsi;
  
	  typedef const Lang::FloatPair ArgType1;
	  RefCountPtr< ArgType1 > d = Helpers::down_cast_CoreArgument< ArgType1 >( title_, args, argsi, callLoc );
  
	  Kernel::ContRef cont = evalState->cont_;
	  typedef const Lang::PathSlider2D ResultType;
	  cont->takeValue( RefCountPtr< ResultType >( new ResultType( p, p->continuousMaximizer( *d ) ) ),
			   evalState );
	  return;
	}

      nextType1:

	{
	  typedef const Lang::ElementaryPath3D ArgType0;
	  RefCountPtr< ArgType0 > p = NullPtr< ArgType0 >( );
	  size_t argsi = 0;
	  try
	    {
	      p = Helpers::elementaryPathTry3D( args.getValue( argsi ) );
	    }
	  catch( const NonLocalExit::NotThisType & ball )
	    {
	      goto nextType2;
	    }

	  ++argsi;
  
	  typedef const Lang::FloatTriple ArgType1;
	  RefCountPtr< ArgType1 > d = Helpers::down_cast_CoreArgument< ArgType1 >( title_, args, argsi, callLoc );
  
	  Kernel::ContRef cont = evalState->cont_;
	  typedef const Lang::PathSlider3D ResultType;
	  cont->takeValue( RefCountPtr< ResultType >( new ResultType( p, p->continuousMaximizer( *d ) ) ),
			   evalState );
	  return;
	}

      nextType2:
	throw Exceptions::CoreTypeMismatch( callLoc, title_, args, 0, Helpers::typeSetString( Lang::ElementaryPath2D::staticTypeName( ), Lang::ElementaryPath3D::staticTypeName( ) ) );
      }
    };

    class Core_continuous_approximator : public Lang::CoreFunction
    {
    public:
      Core_continuous_approximator( const char * title ) : CoreFunction( title ) { }
      virtual void
      call( Kernel::EvalState * evalState, Kernel::Arguments & args, const Ast::SourceLocation & callLoc ) const
      {
	const size_t ARITY = 2;
	CHECK_ARITY( args, ARITY, title_ );
  
	{
	  typedef const Lang::ElementaryPath2D ArgType0;
	  RefCountPtr< ArgType0 > p = NullPtr< ArgType0 >( );
	  size_t argsi = 0;
	  try
	    {
	      p = Helpers::elementaryPathTry2D( args.getValue( argsi ) );
	    }
	  catch( const NonLocalExit::NotThisType & ball )
	    {
	      goto nextType1;
	    }

	  ++argsi;
  
	  typedef const Lang::Coords2D ArgType1;
	  RefCountPtr< ArgType1 > d = Helpers::down_cast_CoreArgument< ArgType1 >( title_, args, argsi, callLoc );
  
	  Kernel::ContRef cont = evalState->cont_;
	  typedef const Lang::PathSlider2D ResultType;
	  cont->takeValue( RefCountPtr< ResultType >( new ResultType( p, p->continuousApproximator( *d ) ) ),
			   evalState );
	  return;
	}

      nextType1:

	{
	  typedef const Lang::ElementaryPath3D ArgType0;
	  RefCountPtr< ArgType0 > p = NullPtr< ArgType0 >( );
	  size_t argsi = 0;
	  try
	    {
	      p = Helpers::elementaryPathTry3D( args.getValue( argsi ) );
	    }
	  catch( const NonLocalExit::NotThisType & ball )
	    {
	      goto nextType2;
	    }

	  ++argsi;
  
	  typedef const Lang::Coords3D ArgType1;
	  RefCountPtr< ArgType1 > d = Helpers::down_cast_CoreArgument< ArgType1 >( title_, args, argsi, callLoc );
  
	  Kernel::ContRef cont = evalState->cont_;
	  typedef const Lang::PathSlider3D ResultType;
	  cont->takeValue( RefCountPtr< ResultType >( new ResultType( p, p->continuousApproximator( *d ) ) ),
			   evalState );
	  return;
	}

      nextType2:
	throw Exceptions::CoreTypeMismatch( callLoc, title_, args, 0, Helpers::typeSetString( Lang::ElementaryPath2D::staticTypeName( ), Lang::ElementaryPath3D::staticTypeName( ) ) );
      }
    };


    class Core_intersection : public Lang::CoreFunction
    {
      mutable Kernel::Environment::LexicalKey * idKey;
    public:
      Core_intersection( const char * title ) : CoreFunction( title ), idKey( 0 ) { }
      virtual ~Core_intersection( ) { if( idKey != 0 ){ delete idKey; } }
      virtual void
      call( Kernel::EvalState * evalState, Kernel::Arguments & args, const Ast::SourceLocation & callLoc ) const
      {
	const size_t ARITY = 2;
	CHECK_ARITY( args, ARITY, title_ );
  
	typedef const Lang::ElementaryPath2D ArgType1;
	size_t argsi = 0;
	RefCountPtr< ArgType1 > p = Helpers::elementaryPathCast2D( title_, args, argsi, callLoc );
  
	++argsi;
  
	typedef const Lang::ElementaryPath2D ArgType2;
	RefCountPtr< ArgType2 > p2 = Helpers::elementaryPathCast2D( title_, args, argsi, callLoc );
  
	Kernel::ContRef cont = evalState->cont_;
	try
	  {
	    typedef const Lang::PathSlider2D ResultType;
	    cont->takeValue( RefCountPtr< ResultType >( new ResultType( p, p->intersection( *p2 ) ) ),
			     evalState );
	  }
	catch( const Exceptions::OutOfRange & ball )
	  {
	    if( idKey == 0 )
	      {
		idKey = new Kernel::Environment::LexicalKey( Ast::theGlobalAnalysisEnvironment->findLexicalDynamicKey( callLoc, Lang::HANDLER_NO_INTERSECTION ) );
	      }

	    const Kernel::DynamicVariableProperties & dynProps = Kernel::theGlobalEnvironment->lookupDynamicVariable( *idKey );
      
	    Kernel::VariableHandle handler = dynProps.fetch( evalState->dyn_ );

	    typedef const Lang::Function HandlerType;

	    RefCountPtr< HandlerType > fun( handler->getVal< HandlerType >( callLoc ) );
	    fun->call( evalState, args.getValue( 0 ), args.getValue( 1 ), callLoc );
	  }
      }
    };
  }
}


void
Kernel::registerCore_point( Kernel::Environment * env )
{
  env->initDefineCoreFunction( new Lang::Core_duration( "duration" ) );
  env->initDefineCoreFunction( new Lang::Core_controlling_maximizer( "controlling_maximizer" ) );
  env->initDefineCoreFunction( new Lang::Core_discrete_mean( "discrete_mean" ) );
  env->initDefineCoreFunction( new Lang::Core_discrete_maximizer( "discrete_maximizer" ) );
  env->initDefineCoreFunction( new Lang::Core_discrete_approximator( "discrete_approximator" ) );
  env->initDefineCoreFunction( new Lang::Core_continuous_mean( "continuous_mean" ) );
  env->initDefineCoreFunction( new Lang::Core_continuous_maximizer( "continuous_maximizer" ) );
  env->initDefineCoreFunction( new Lang::Core_continuous_approximator( "continuous_approximator" ) );
  env->initDefineCoreFunction( new Lang::Core_intersection( "intersection" ) );
}
