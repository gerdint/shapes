#include <cmath>

#include "MetaPDF_Helpers_decls.h"

#include "metapdfcore.h"
#include "metapdfast.h"
#include "globals.h"
#include "metapdfexceptions.h"
#include "consts.h"
#include "simplepdfi.h"

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
    class Core_bbox : public Lang::CoreFunction
    {
    public:
      Core_bbox( const char * title ) : CoreFunction( title ) { }
      virtual void
      call( Kernel::EvalState * evalState, Kernel::Arguments & args, const Ast::SourceLocation & callLoc ) const
      {
	const size_t ARITY = 1;
	CHECK_ARITY( args, ARITY, title_ );
  
	typedef const Lang::Drawable2D ArgType;
	RefCountPtr< ArgType > arg = args.getValue( 0 ).down_cast< ArgType >( );
	if( arg == NullPtr< ArgType >( ) )
	  {
	    throw Exceptions::CoreTypeMismatch( callLoc, title_, args, 0, Interaction::SEVERAL_TYPES );
	  }

	Kernel::ContRef cont = evalState->cont_;
	cont->takeValue( arg->bbox( ),
			 evalState );
      }
    };
  
    class Core_controlling : public Lang::CoreFunction
    {
    public:
      Core_controlling( const char * title ) : CoreFunction( title ) { }
      virtual void
      call( Kernel::EvalState * evalState, Kernel::Arguments & args, const Ast::SourceLocation & callLoc ) const
      {
	const size_t ARITY = 1;
	CHECK_ARITY( args, ARITY, title_ );

	{
	  typedef const Lang::ElementaryPath2D ArgType;
	  RefCountPtr< ArgType > path = NullPtr< ArgType >( );
	  try
	    {
	      path = Helpers::elementaryPathTry2D( args.getValue( 0 ) );
	    }
	  catch( const NonLocalExit::NotThisType & ball )
	    {
	      goto nextType1;
	    }
	  Lang::Path2D * res = new Lang::Path2D( );

	  typedef typeof *path ListType;
	  typedef ListType::const_iterator I;
	  for( I i = path->begin( ); i != path->end( ); ++i )
	    {
	      const Concrete::Coords2D * h = (*i)->rear_;
	      if( h != 0 )
		{
		  Lang::ElementaryPath2D * handlePath = new Lang::ElementaryPath2D;
		  handlePath->push_back( new Concrete::PathPoint2D( new Concrete::Coords2D( *( (*i)->mid_ ) ) ) );
		  handlePath->push_back( new Concrete::PathPoint2D( new Concrete::Coords2D( *h ) ) );
		  res->push_back( RefCountPtr< const Lang::ElementaryPath2D >( handlePath ) );
		}
	      h = (*i)->front_;
	      if( h != 0 )
		{
		  Lang::ElementaryPath2D * handlePath = new Lang::ElementaryPath2D;
		  handlePath->push_back( new Concrete::PathPoint2D( new Concrete::Coords2D( *( (*i)->mid_ ) ) ) );
		  handlePath->push_back( new Concrete::PathPoint2D( new Concrete::Coords2D( *h ) ) );
		  res->push_back( RefCountPtr< const Lang::ElementaryPath2D >( handlePath ) );
		}
	    }
    
	  Kernel::ContRef cont = evalState->cont_;
	  cont->takeValue( Kernel::ValueRef( res ),
			   evalState );
	  return;
	}

      nextType1:
	{
	  typedef const Lang::ElementaryPath3D ArgType;
	  RefCountPtr< ArgType > path = NullPtr< ArgType >( );
	  try
	    {
	      path = Helpers::elementaryPathTry3D( args.getValue( 0 ) );
	    }
	  catch( const NonLocalExit::NotThisType & ball )
	    {
	      goto nextType2;
	    }
	  Lang::Path3D * res = new Lang::Path3D( );

	  typedef typeof *path ListType;
	  typedef ListType::const_iterator I;
	  for( I i = path->begin( ); i != path->end( ); ++i )
	    {
	      const Concrete::Coords3D * h = (*i)->rear_;
	      if( h != 0 )
		{
		  Lang::ElementaryPath3D * handlePath = new Lang::ElementaryPath3D;
		  handlePath->push_back( new Concrete::PathPoint3D( new Concrete::Coords3D( *( (*i)->mid_ ) ) ) );
		  handlePath->push_back( new Concrete::PathPoint3D( new Concrete::Coords3D( *h ) ) );
		  res->push_back( RefCountPtr< const Lang::ElementaryPath3D >( handlePath ) );
		}
	      h = (*i)->front_;
	      if( h != 0 )
		{
		  Lang::ElementaryPath3D * handlePath = new Lang::ElementaryPath3D;
		  handlePath->push_back( new Concrete::PathPoint3D( new Concrete::Coords3D( *( (*i)->mid_ ) ) ) );
		  handlePath->push_back( new Concrete::PathPoint3D( new Concrete::Coords3D( *h ) ) );
		  res->push_back( RefCountPtr< const Lang::ElementaryPath3D >( handlePath ) );
		}
	    }
    
	  Kernel::ContRef cont = evalState->cont_;
	  cont->takeValue( Kernel::ValueRef( res ),
			   evalState );
	  return;
	}

      nextType2:
	throw Exceptions::CoreTypeMismatch( callLoc, title_, args, 0, Helpers::typeSetString( Lang::ElementaryPath2D::staticTypeName( ), Lang::ElementaryPath3D::staticTypeName( ) ) );
      }
    };

    class Core_controlling_hull : public Lang::CoreFunction
    {
    public:
      Core_controlling_hull( const char * title ) : CoreFunction( title ) { }
      virtual void
      call( Kernel::EvalState * evalState, Kernel::Arguments & args, const Ast::SourceLocation & callLoc ) const
      {
	const size_t ARITY = 1;
	CHECK_ARITY( args, ARITY, title_ );

	typedef const Lang::ElementaryPath2D ArgType;
	RefCountPtr< ArgType > path = Helpers::elementaryPathCast2D( title_, args, 0, callLoc );

	Kernel::ContRef cont = evalState->cont_;
	cont->takeValue( path->controlling_hull( ),
			 evalState );
      }
    };

    class Core_subpath : public Lang::CoreFunction
    {
    public:
      Core_subpath( const char * title ) : CoreFunction( title ) { }
      virtual void
      call( Kernel::EvalState * evalState, Kernel::Arguments & args, const Ast::SourceLocation & callLoc ) const
      {
	const size_t ARITY = 3;
	CHECK_ARITY( args, ARITY, title_ );

	{
	  typedef const Lang::ElementaryPath2D ArgType;
	  RefCountPtr< ArgType > path = NullPtr< ArgType >( );
	  size_t argsi = 0;
	  try
	    {
	      path = Helpers::elementaryPathTry2D( args.getValue( argsi ) );
	    }
	  catch( const NonLocalExit::NotThisType & ball )
	    {
	      goto nextType1;
	    }
	  ++argsi;
	  Concrete::SplineTime t1 = Helpers::pathTimeCast( title_, path, args, argsi, callLoc );
	  ++argsi;
	  Concrete::SplineTime t2 = Helpers::pathTimeCast( title_, path, args, argsi, callLoc );

	  Kernel::ContRef cont = evalState->cont_;
	  cont->takeValue( path->subpath( t1, t2 ),
			   evalState );
	  return;
	}
  
      nextType1:
	{
	  typedef const Lang::ElementaryPath3D ArgType;
	  RefCountPtr< ArgType > path = NullPtr< ArgType >( );
	  size_t argsi = 0;
	  try
	    {
	      path = Helpers::elementaryPathTry3D( args.getValue( argsi ) );
	    }
	  catch( const NonLocalExit::NotThisType & ball )
	    {
	      goto nextType2;
	    }
	  ++argsi;
	  Concrete::SplineTime t1 = Helpers::pathTimeCast( title_, path, args, argsi, callLoc );
	  ++argsi;
	  Concrete::SplineTime t2 = Helpers::pathTimeCast( title_, path, args, argsi, callLoc );

	  Kernel::ContRef cont = evalState->cont_;
	  cont->takeValue( path->subpath( t1, t2 ),
			   evalState );
	  return;
	}

      nextType2:
	throw Exceptions::CoreTypeMismatch( callLoc, title_, args, 0, Helpers::typeSetString( Lang::ElementaryPath2D::staticTypeName( ), Lang::ElementaryPath3D::staticTypeName( ) ) );
      }
    };

    class Core_reverse : public Lang::CoreFunction
    {
    public:
      Core_reverse( const char * title ) : CoreFunction( title ) { }
      virtual void
      call( Kernel::EvalState * evalState, Kernel::Arguments & args, const Ast::SourceLocation & callLoc ) const
      {
	const size_t ARITY = 1;
	CHECK_ARITY( args, ARITY, title_ );
  
	size_t argsi = 0;

	try
	  {
	    typedef const Lang::ElementaryPath2D ArgType;
	    RefCountPtr< ArgType > arg = Helpers::elementaryPathTry2D( args.getValue( argsi ) );
	    Kernel::ContRef cont = evalState->cont_;
	    cont->takeValue( arg->reverse( ),
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
	    RefCountPtr< ArgType > arg = Helpers::elementaryPathTry3D( args.getValue( argsi ) );
	    Kernel::ContRef cont = evalState->cont_;
	    cont->takeValue( arg->reverse( ),
			     evalState );
	    return;
	  }
	catch( const NonLocalExit::NotThisType & ball )
	  {
	    /* Wrong type; never mind!.. but see below!
	     */
	  }

	throw Exceptions::CoreTypeMismatch( callLoc, title_, args, argsi, Helpers::typeSetString( Lang::ElementaryPath2D::staticTypeName( ), Lang::ElementaryPath3D::staticTypeName( ) ) );
      }
    };

    class Core_meetpaths : public Lang::CoreFunction
    {
    public:
      Core_meetpaths( const char * title ) : CoreFunction( title ) { }
      virtual void
      call( Kernel::EvalState * evalState, Kernel::Arguments & args, const Ast::SourceLocation & callLoc ) const
      {
	const size_t ARITY = 2;
	CHECK_ARITY( args, ARITY, title_ );
  
	{
	  typedef const Lang::ElementaryPath2D ArgType;
	  RefCountPtr< ArgType > path1 = NullPtr< ArgType >( );
	  size_t argsi = 0;
	  try
	    {
	      path1 = Helpers::elementaryPathTry2D( args.getValue( argsi ) );
	    }
	  catch( const NonLocalExit::NotThisType & ball )
	    {
	      goto nextType1;
	    }
	  ++argsi;
	  RefCountPtr< ArgType > path2 = Helpers::elementaryPathCast2D( title_, args, argsi, callLoc );

	  Lang::ElementaryPath2D * res = new Lang::ElementaryPath2D( );
    
	  Concrete::PathPoint2D * meetPoint;

	  {
	    Lang::ElementaryPath2D::const_iterator i = path1->begin( );
	    Lang::ElementaryPath2D::const_iterator end = path1->end( );
	    --end;
	    for( ; i != end; ++i )
	      {
		Concrete::PathPoint2D * newPoint = new Concrete::PathPoint2D( new Concrete::Coords2D( *( (*i)->mid_ ) ) );
		res->push_back( newPoint );
		const Concrete::Coords2D * h = (*i)->rear_;
		if( h != 0 )
		  {
		    newPoint->rear_ = new Concrete::Coords2D( *( (*i)->rear_ ) );
		  }
		h = (*i)->front_;
		if( h != 0 )
		  {
		    newPoint->front_ = new Concrete::Coords2D( *( (*i)->front_ ) );
		  }
	      }
	    meetPoint = new Concrete::PathPoint2D( new Concrete::Coords2D( *( (*i)->mid_ ) ) );
	    if( (*i)->rear_ != 0 )
	      {
		meetPoint->rear_ = new Concrete::Coords2D( *( (*i)->rear_ ) );
	      }
	  }

	  res->push_back( meetPoint );

	  {
	    Lang::ElementaryPath2D::const_iterator i = path2->begin( );

	    *(meetPoint->mid_) = 0.5 * ( *( (*i)->mid_ ) + *(meetPoint->mid_) );

	    if( (*i)->front_ != 0 )
	      {
		meetPoint->front_ = new Concrete::Coords2D( *( (*i)->front_ ) );
	      }

	    ++i;
	    Lang::ElementaryPath2D::const_iterator end = path2->end( );
	    for( ; i != end; ++i )
	      {
		Concrete::PathPoint2D * newPoint = new Concrete::PathPoint2D( new Concrete::Coords2D( *( (*i)->mid_ ) ) );
		res->push_back( newPoint );
		const Concrete::Coords2D * h = (*i)->rear_;
		if( h != 0 )
		  {
		    newPoint->rear_ = new Concrete::Coords2D( *( (*i)->rear_ ) );
		  }
		h = (*i)->front_;
		if( h != 0 )
		  {
		    newPoint->front_ = new Concrete::Coords2D( *( (*i)->front_ ) );
		  }
	      }
	  }
    
	  Kernel::ContRef cont = evalState->cont_;
	  cont->takeValue( Kernel::ValueRef( res ),
			   evalState );
	  return;
	}
  
      nextType1:
	{
	  typedef const Lang::ElementaryPath3D ArgType;
	  RefCountPtr< ArgType > path1 = NullPtr< ArgType >( );
	  size_t argsi = 0;
	  try
	    {
	      path1 = Helpers::elementaryPathTry3D( args.getValue( argsi ) );
	    }
	  catch( const NonLocalExit::NotThisType & ball )
	    {
	      goto nextType2;
	    }
	  ++argsi;
	  RefCountPtr< ArgType > path2 = Helpers::elementaryPathCast3D( title_, args, argsi, callLoc );

	  Lang::ElementaryPath3D * res = new Lang::ElementaryPath3D( );
    
	  Concrete::PathPoint3D * meetPoint;

	  {
	    Lang::ElementaryPath3D::const_iterator i = path1->begin( );
	    Lang::ElementaryPath3D::const_iterator end = path1->end( );
	    --end;
	    for( ; i != end; ++i )
	      {
		Concrete::PathPoint3D * newPoint = new Concrete::PathPoint3D( new Concrete::Coords3D( *( (*i)->mid_ ) ) );
		res->push_back( newPoint );
		const Concrete::Coords3D * h = (*i)->rear_;
		if( h != 0 )
		  {
		    newPoint->rear_ = new Concrete::Coords3D( *( (*i)->rear_ ) );
		  }
		h = (*i)->front_;
		if( h != 0 )
		  {
		    newPoint->front_ = new Concrete::Coords3D( *( (*i)->front_ ) );
		  }
	      }
	    meetPoint = new Concrete::PathPoint3D( new Concrete::Coords3D( *( (*i)->mid_ ) ) );
	    if( (*i)->rear_ != 0 )
	      {
		meetPoint->rear_ = new Concrete::Coords3D( *( (*i)->rear_ ) );
	      }
	  }

	  res->push_back( meetPoint );

	  {
	    Lang::ElementaryPath3D::const_iterator i = path2->begin( );

	    *(meetPoint->mid_) = 0.5 * ( *( (*i)->mid_ ) + *(meetPoint->mid_) );

	    if( (*i)->front_ != 0 )
	      {
		meetPoint->front_ = new Concrete::Coords3D( *( (*i)->front_ ) );
	      }

	    ++i;
	    Lang::ElementaryPath3D::const_iterator end = path2->end( );
	    for( ; i != end; ++i )
	      {
		Concrete::PathPoint3D * newPoint = new Concrete::PathPoint3D( new Concrete::Coords3D( *( (*i)->mid_ ) ) );
		res->push_back( newPoint );
		const Concrete::Coords3D * h = (*i)->rear_;
		if( h != 0 )
		  {
		    newPoint->rear_ = new Concrete::Coords3D( *( (*i)->rear_ ) );
		  }
		h = (*i)->front_;
		if( h != 0 )
		  {
		    newPoint->front_ = new Concrete::Coords3D( *( (*i)->front_ ) );
		  }
	      }
	  }
    
	  Kernel::ContRef cont = evalState->cont_;
	  cont->takeValue( Kernel::ValueRef( res ),
			   evalState );
	  return;
	}

      nextType2:
	throw Exceptions::CoreTypeMismatch( callLoc, title_, args, 0, Helpers::typeSetString( Lang::ElementaryPath2D::staticTypeName( ), Lang::ElementaryPath3D::staticTypeName( ) ) );
      }
    };

    class Core_upsampleinflections : public Lang::CoreFunction
    {
    public:
      Core_upsampleinflections( const char * title ) : CoreFunction( title ) { }
      virtual void
      call( Kernel::EvalState * evalState, Kernel::Arguments & args, const Ast::SourceLocation & callLoc ) const
      {
	const size_t ARITY = 1;
	CHECK_ARITY( args, ARITY, title_ );
  
	size_t argsi = 0;

	try
	  {
	    typedef const Lang::ElementaryPath2D ArgType;
	    RefCountPtr< ArgType > arg = Helpers::elementaryPathTry2D( args.getValue( argsi ) );
	    Kernel::ContRef cont = evalState->cont_;
	    cont->takeValue( arg->upsample_inflections( ),
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
	    RefCountPtr< ArgType > arg = Helpers::elementaryPathTry3D( args.getValue( argsi ) );
	    Kernel::ContRef cont = evalState->cont_;
	    cont->takeValue( arg->upsample_inflections( ),
			     evalState );
	    return;
	  }
	catch( const NonLocalExit::NotThisType & ball )
	  {
	    /* Wrong type; never mind!.. but see below!
	     */
	  }

	throw Exceptions::CoreTypeMismatch( callLoc, title_, args, argsi, Helpers::typeSetString( Lang::ElementaryPath2D::staticTypeName( ), Lang::ElementaryPath3D::staticTypeName( ) ) );
      }
    };

  }
}



void
Kernel::registerCore_path( Kernel::Environment * env )
{
  env->initDefineCoreFunction( new Lang::Core_bbox( "bbox" ) );
  env->initDefineCoreFunction( new Lang::Core_controlling( "controlling" ) );
  env->initDefineCoreFunction( new Lang::Core_controlling_hull( "controlling_hull" ) );

  env->initDefineCoreFunction( new Lang::Core_reverse( "reverse" ) );
  env->initDefineCoreFunction( new Lang::Core_meetpaths( "meetpaths" ) );

  env->initDefineCoreFunction( new Lang::Core_upsampleinflections( "upsample_inflections" ) );

  /* subpath functions yet to be implemented */
  //  env->initDefineCoreFunction( new Lang::Core_directiontime( "directiontime" ) );
  //  env->initDefineCoreFunction( new Lang::Core_nearesttimes( "nearesttimes" ) ); /* generalizes distance between subpaths */
  //  env->initDefineCoreFunction( new Lang::Core_slidetimes( "slidetimes" ) ); /* "directional distance" between subpaths */
  //  env->initDefineCoreFunction( new Lang::Core_sidepath( "sidepath" ) );

  //  env->initDefineCoreFunction( new Lang::Core_convhull( "convhull" ) ); /* convex hull of a (full) path */
}
