#include <cmath>

#include "MetaPDF_Helpers_decls.h"

#include "metapdfcore.h"
#include "globals.h"
#include "metapdfexceptions.h"
#include "consts.h"
#include "simplepdfi.h"
#include "autoonoff.h"
#include "hottypes.h"
#include "metapdfastfun.h"

#include <iostream>
#include <sstream>
#include <fstream>
#include <vector>
#include <stdio.h>

using namespace MetaPDF;


namespace MetaPDF
{
  namespace Helpers
  {
    RefCountPtr< const Lang::TransparencyGroup >
    newSolidTransparencyGroup( const RefCountPtr< const Lang::Drawable2D > & obj2, const RefCountPtr< const Lang::Drawable2D > & obj1 );
    RefCountPtr< const Lang::TransparencyGroup >
    newSolidTransparencyGroup( const RefCountPtr< const Lang::Drawable2D > & obj3, const RefCountPtr< const Lang::Drawable2D > & obj2, const RefCountPtr< const Lang::Drawable2D > & obj1 );
    
    void stroke_helper_2D( Kernel::EvalState * evalState, const RefCountPtr< const Lang::ElementaryPath2D > & path, Kernel::Arguments & args, const Ast::SourceLocation & callLoc );
    void stroke_helper_3D( Kernel::EvalState * evalState, const RefCountPtr< const Lang::ElementaryPath3D > & path, Kernel::Arguments & args, const Ast::SourceLocation & callLoc );
  }
}

Lang::Core_stroke::Core_stroke( const char * title )
  : CoreFunction( title, new Kernel::EvaluatedFormals( title, true ) )
{
  formals_->appendEvaluatedCoreFormal( "path", Kernel::THE_SLOT_VARIABLE );
  formals_->appendEvaluatedCoreFormal( "head", Kernel::VariableHandle( new Kernel::Variable( Lang::THE_NO_ARROW ) ) );
  formals_->appendEvaluatedCoreFormal( "tail", Kernel::VariableHandle( new Kernel::Variable( Lang::THE_NO_ARROW ) ) );
}

void
Lang::Core_stroke::call( Kernel::EvalState * evalState, Kernel::Arguments & args, const Ast::SourceLocation & callLoc ) const
{
  args.applyDefaults( );

  try
    {
      typedef const Lang::ElementaryPath2D ArgType;
      RefCountPtr< ArgType > arg = Helpers::elementaryPathTry2D( args.getValue( 0 ) );
      Helpers::stroke_helper_2D( evalState, arg, args, callLoc );
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
      Helpers::stroke_helper_3D( evalState, arg, args, callLoc );
      return;
    }
  catch( const NonLocalExit::NotThisType & ball )
    {
      /* Wrong type; never mind!.. but see below!
       */
    }
  
  try
    {
      typedef const Lang::Path2D ArgType;
      RefCountPtr< ArgType > path = Helpers::try_cast_CoreArgument< ArgType >( args.getValue( 0 ) );
      RefCountPtr< const Lang::Function > arrowHead = Helpers::down_cast_CoreArgument< const Lang::Function >( title_, args, 1, callLoc );
      RefCountPtr< const Lang::Function > arrowTail = Helpers::down_cast_CoreArgument< const Lang::Function >( title_, args, 2, callLoc );
      if( arrowHead != Lang::THE_NO_ARROW ||
	  arrowTail != Lang::THE_NO_ARROW )
	{
	  throw Exceptions::MiscellaneousRequirement( strrefdup( "Arrowheads/tails are not supported for composite paths." ) );
	}
      Kernel::ContRef cont = evalState->cont_;
      cont->takeValue( Kernel::ValueRef( new Lang::PaintedPath2D( evalState->dyn_->getGraphicsState( ), path, "S" ) ),
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
      typedef const Lang::Path3D ArgType;
      RefCountPtr< ArgType > path = Helpers::try_cast_CoreArgument< ArgType >( args.getValue( 0 ) );
      RefCountPtr< const Lang::Function > arrowHead = Helpers::down_cast_CoreArgument< const Lang::Function >( title_, args, 1, callLoc );
      RefCountPtr< const Lang::Function > arrowTail = Helpers::down_cast_CoreArgument< const Lang::Function >( title_, args, 2, callLoc );
      if( arrowHead != Lang::THE_NO_ARROW ||
	  arrowTail != Lang::THE_NO_ARROW )
	{
	  throw Exceptions::MiscellaneousRequirement( strrefdup( "Arrowheads/tails are not supported for composite paths." ) );
	}
      Kernel::ContRef cont = evalState->cont_;
      cont->takeValue( Kernel::ValueRef( new Lang::PaintedPath3D( evalState->dyn_->getGraphicsState( ), path, "S" ) ),
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

namespace MetaPDF
{
  namespace Kernel
  {
    class ArrowheadReceiverFormals2D : public Kernel::EvaluatedFormals
    {
    public:
      ArrowheadReceiverFormals2D( )
	: Kernel::EvaluatedFormals( "< Arrowhead receiver >", true )
      {
	appendEvaluatedCoreFormal( "picture", Kernel::THE_SLOT_VARIABLE );
	appendEvaluatedCoreFormal( "cut", Kernel::VariableHandle( new Kernel::Variable( RefCountPtr< const Lang::Value >( new Lang::Length( 0 ) ) ) ) );
      }
    };

    ArrowheadReceiverFormals2D theArrowheadReceiverFormals2D;
    
    class Stroke2DCont_tail : public Kernel::ForcedStructureContinuation
    {
      RefCountPtr< const Kernel::GraphicsState > graphicsState_;
      RefCountPtr< const Lang::ElementaryPath2D > path_;
      Kernel::PassedEnv env_;
      Kernel::PassedDyn dyn_;
      Kernel::ContRef cont_;
    public:
      Stroke2DCont_tail( const RefCountPtr< const Kernel::GraphicsState > & graphicsState, const RefCountPtr< const Lang::ElementaryPath2D > & path,
			 const Kernel::PassedEnv & env, const Kernel::PassedDyn & dyn, Kernel::ContRef cont, const Ast::SourceLocation & traceLoc )
	: Kernel::ForcedStructureContinuation( "Stroke's arrow tail receiver", traceLoc ), graphicsState_( graphicsState ), path_( path ), env_( env ), dyn_( dyn ), cont_( cont )
      { }
      virtual ~Stroke2DCont_tail( ) { }
      virtual void takeStructure( const RefCountPtr< const Lang::Structure > & structure, Kernel::EvalState * evalState ) const
      {
	/* Argument 0: picture
	 * Argument 1: cut
	 */
	Kernel::Arguments args( & theArrowheadReceiverFormals2D );
	structure->argList_->bind( & args, structure->values_, env_, dyn_ );
	args.applyDefaults( );
	
	typedef const Lang::Drawable2D ArgType0;
	RefCountPtr< ArgType0 > picture = Helpers::down_cast_CoreArgument< ArgType0 >( continuationName_, args, 0, traceLoc_ );
	
	typedef const Lang::Length ArgType1;
	RefCountPtr< ArgType1 > cutTail = Helpers::down_cast_CoreArgument< ArgType1 >( continuationName_, args, 1, traceLoc_ );
	
	if( cutTail->get( ) < 0 )
	  {
	    throw Exceptions::MiscellaneousRequirement( strrefdup( "Arrow tail cut length was negative." ) );
	  }
	else if( cutTail->get( ) == 0 )
	  {
	    cont_->takeValue( Helpers::newSolidTransparencyGroup( picture,
								  RefCountPtr< const Lang::Drawable2D >( new Lang::PaintedPath2D( graphicsState_, path_, "S" ) ) ),
			      evalState );
	  }
	else
	  {
	    Concrete::SplineTime t1 = path_->arcTime( cutTail->get( ) );
	    Concrete::SplineTime t2( HUGE_VAL );
	    RefCountPtr< const Lang::ElementaryPath2D > subpath = path_->subpath( t1, t2 );
	    if( subpath->size( ) > 0 )
	      {
		cont_->takeValue( Helpers::newSolidTransparencyGroup( picture,
								      RefCountPtr< const Lang::Drawable2D >( new Lang::PaintedPath2D( graphicsState_, subpath, "S" ) ) ),
			       evalState );
	      }
	    else
	      {
		cont_->takeValue( picture,
				  evalState );
	      }
	    
	  }
      }
      virtual void backTrace( std::list< Kernel::Continuation::BackTraceElem > * trace ) const
      {
	trace->push_front( Kernel::Continuation::BackTraceElem( this, "2D stroke's tail" ) );
	cont_->backTrace( trace );
      }
      virtual void gcMark( Kernel::GCMarkedSet & marked )
      {
	cont_->gcMark( marked );
      }
    };

    class Stroke2DCont_head : public Kernel::ForcedStructureContinuation
    {
      RefCountPtr< const Kernel::GraphicsState > graphicsState_;
      RefCountPtr< const Lang::ElementaryPath2D > path_;
      Kernel::PassedEnv env_;
      Kernel::PassedDyn dyn_;
      Kernel::ContRef cont_;
    public:
      Stroke2DCont_head( const RefCountPtr< const Kernel::GraphicsState > & graphicsState, const RefCountPtr< const Lang::ElementaryPath2D > & path,
			 const Kernel::PassedEnv & env, const Kernel::PassedDyn & dyn, Kernel::ContRef cont, const Ast::SourceLocation & traceLoc )
	: Kernel::ForcedStructureContinuation( "Stroke's arrow tail receiver", traceLoc ), graphicsState_( graphicsState ), path_( path ), env_( env ), dyn_( dyn ), cont_( cont )
      { }
      virtual ~Stroke2DCont_head( ) { }
      virtual void takeStructure( const RefCountPtr< const Lang::Structure > & structure, Kernel::EvalState * evalState ) const
      {
	/* Argument 0: picture
	 * Argument 1: cut
	 */
	Kernel::Arguments args( & theArrowheadReceiverFormals2D );
	structure->argList_->bind( & args, structure->values_, env_, dyn_ );
	args.applyDefaults( );
	
	typedef const Lang::Drawable2D ArgType0;
	RefCountPtr< ArgType0 > picture = Helpers::down_cast_CoreArgument< ArgType0 >( continuationName_, args, 0, traceLoc_ );
	
	typedef const Lang::Length ArgType1;
	RefCountPtr< ArgType1 > cutHead = Helpers::down_cast_CoreArgument< ArgType1 >( continuationName_, args, 1, traceLoc_ );

	if( cutHead->get( ) < 0 )
	  {
	    throw Exceptions::MiscellaneousRequirement( strrefdup( "Arrow head cut length was negative." ) );
	  }
	else if( cutHead->get( ) == 0 )
	  {
	    cont_->takeValue( Helpers::newSolidTransparencyGroup( picture,
								  RefCountPtr< const Lang::Drawable2D >( new Lang::PaintedPath2D( graphicsState_, path_, "S" ) ) ),
			      evalState );
	  }
	else
	  {
	    Concrete::SplineTime t1( 0 );
	    Concrete::SplineTime t2 = path_->arcTime( path_->arcLength( ) - cutHead->get( ) );
	    RefCountPtr< const Lang::ElementaryPath2D > subpath = path_->subpath( t1, t2 );
	    if( subpath->size( ) > 0 )
	      {
		cont_->takeValue( Helpers::newSolidTransparencyGroup( picture,
								      RefCountPtr< const Lang::Drawable2D >( new Lang::PaintedPath2D( graphicsState_, subpath, "S" ) ) ),
				  evalState );
	      }
	    else
	      {
		cont_->takeValue( picture,
				  evalState );
	      }
	  }
      }
      virtual void backTrace( std::list< Kernel::Continuation::BackTraceElem > * trace ) const
      {
	trace->push_front( Kernel::Continuation::BackTraceElem( this, "2D stroke's head" ) );
	cont_->backTrace( trace );
      }
      virtual void gcMark( Kernel::GCMarkedSet & marked )
      {
	cont_->gcMark( marked );
      }
    };

    class Stroke2DCont_both2 : public Kernel::ForcedStructureContinuation
    {
      RefCountPtr< const Kernel::GraphicsState > graphicsState_;
      RefCountPtr< const Lang::ElementaryPath2D > path_;
      RefCountPtr< const Lang::Drawable2D > tail_;
      Lang::Length cutTail_;
      Kernel::PassedEnv env_;
      Kernel::PassedDyn dyn_;
      Kernel::ContRef cont_;
    public:
      Stroke2DCont_both2( const RefCountPtr< const Kernel::GraphicsState > & graphicsState, const RefCountPtr< const Lang::ElementaryPath2D > & path,
			  const RefCountPtr< const Lang::Drawable2D > & tail, Lang::Length cutTail,
			  const Kernel::PassedEnv & env, const Kernel::PassedDyn & dyn, Kernel::ContRef cont, const Ast::SourceLocation & traceLoc )
	: Kernel::ForcedStructureContinuation( "Stroke's arrow head receiver", traceLoc ), graphicsState_( graphicsState ), path_( path ), tail_( tail ), cutTail_( cutTail ), env_( env ), dyn_( dyn ), cont_( cont )
      { }
      virtual ~Stroke2DCont_both2( ) { }
      virtual void takeStructure( const RefCountPtr< const Lang::Structure > & structure, Kernel::EvalState * evalState ) const
      {
	/* Argument 0: picture
	 * Argument 1: cut
	 */
	Kernel::Arguments args( & theArrowheadReceiverFormals2D );
	structure->argList_->bind( & args, structure->values_, env_, dyn_ );
	args.applyDefaults( );
	
	typedef const Lang::Drawable2D ArgType0;
	RefCountPtr< ArgType0 > picture = Helpers::down_cast_CoreArgument< ArgType0 >( continuationName_, args, 0, traceLoc_ );
	
	typedef const Lang::Length ArgType1;
	RefCountPtr< ArgType1 > cutHead = Helpers::down_cast_CoreArgument< ArgType1 >( continuationName_, args, 1, traceLoc_ );

	if( cutHead->get( ) < 0 )
	  {
	    throw Exceptions::MiscellaneousRequirement( strrefdup( "Aarrow head cut length was negative." ) );
	  }
	
	if( cutTail_.get( ) == 0 && cutHead->get( ) == 0 )
	  {
	    cont_->takeValue( Helpers::newSolidTransparencyGroup( picture,
								  tail_,
								  RefCountPtr< const Lang::Drawable2D >( new Lang::PaintedPath2D( graphicsState_, path_, "S" ) ) ),
			      evalState );
	  }
	else
	  {
	    Concrete::SplineTime t1( 0 );
	    if( cutTail_.get( ) > 0 )
	      {
		t1 = path_->arcTime( cutTail_.get( ) );
	      }
	    
	    Concrete::SplineTime t2( HUGE_VAL );
	    if( cutHead->get( ) > 0 )
	      {
		t2 = path_->arcTime( path_->arcLength( ) - cutHead->get( ) );
	      }
	    
	    RefCountPtr< const Lang::ElementaryPath2D > subpath = path_->subpath( t1, t2 );
	    
	    if( subpath->size( ) > 0 )
	      {
		cont_->takeValue( Helpers::newSolidTransparencyGroup( picture,
								      tail_,
								      RefCountPtr< const Lang::Drawable2D >( new Lang::PaintedPath2D( graphicsState_, subpath, "S" ) ) ),
				  evalState );
	      }
	    else
	      {
		cont_->takeValue( Helpers::newSolidTransparencyGroup( picture,
								      tail_ ),
				  evalState );
	      }
	  }
      }
      virtual void backTrace( std::list< Kernel::Continuation::BackTraceElem > * trace ) const
      {
	trace->push_front( Kernel::Continuation::BackTraceElem( this, "2D stroke's head & tail, second" ) );
	cont_->backTrace( trace );
      }
      virtual void gcMark( Kernel::GCMarkedSet & marked )
      {
	const_cast< Lang::Drawable2D * >( tail_.getPtr( ) )->gcMark( marked );
	cont_->gcMark( marked );
      }
    };

    class Stroke2DCont_both1 : public Kernel::ForcedStructureContinuation
    {
      RefCountPtr< const Kernel::GraphicsState > graphicsState_;
      RefCountPtr< const Lang::ElementaryPath2D > path_;
      RefCountPtr< const Lang::Function > headFunction_;
      Kernel::PassedEnv env_;
      Kernel::PassedDyn dyn_;
      Kernel::ContRef cont_;
    public:
      Stroke2DCont_both1( const RefCountPtr< const Kernel::GraphicsState > & graphicsState, const RefCountPtr< const Lang::ElementaryPath2D > & path,
			  const RefCountPtr< const Lang::Function > & headFunction,
			  const Kernel::PassedEnv & env, const Kernel::PassedDyn & dyn, Kernel::ContRef cont, const Ast::SourceLocation & traceLoc )
	: Kernel::ForcedStructureContinuation( "Stroke's arrow tail receiver", traceLoc ), graphicsState_( graphicsState ), path_( path ), headFunction_( headFunction ), env_( env ), dyn_( dyn ), cont_( cont )
      { }
      virtual ~Stroke2DCont_both1( ) { }
      virtual void takeStructure( const RefCountPtr< const Lang::Structure > & structure, Kernel::EvalState * evalState ) const
      {
	/* Argument 0: picture
	 * Argument 1: cut
	 */
	Kernel::Arguments args( & theArrowheadReceiverFormals2D );
	structure->argList_->bind( & args, structure->values_, env_, dyn_ );
	args.applyDefaults( );
	
	typedef const Lang::Drawable2D ArgType0;
	RefCountPtr< ArgType0 > picture = Helpers::down_cast_CoreArgument< ArgType0 >( continuationName_, args, 0, traceLoc_ );
	
	typedef const Lang::Length ArgType1;
	RefCountPtr< ArgType1 > cutTail = Helpers::down_cast_CoreArgument< ArgType1 >( continuationName_, args, 1, traceLoc_ );
	
	if( cutTail->get( ) < 0 )
	  {
	    throw Exceptions::MiscellaneousRequirement( strrefdup( "Arrow tail cut length was negative." ) );
	  }
	
	evalState->env_ = env_;
	evalState->dyn_ = dyn_;
	evalState->cont_ = Kernel::ContRef( new Kernel::Stroke2DCont_both2( graphicsState_, path_, picture, *cutTail, env_, dyn_, cont_, traceLoc_ ) );
	headFunction_->call( evalState, path_->reverse( ), traceLoc_ );
      }
      virtual void backTrace( std::list< Kernel::Continuation::BackTraceElem > * trace ) const
      {
	trace->push_front( Kernel::Continuation::BackTraceElem( this, "2D stroke's head & tail, first" ) );
	cont_->backTrace( trace );
      }
      virtual void gcMark( Kernel::GCMarkedSet & marked )
      {
	const_cast< Lang::Function * >( headFunction_.getPtr( ) )->gcMark( marked );
	dyn_->gcMark( marked );
	cont_->gcMark( marked );
      }
    };
    
  }
}



void
Helpers::stroke_helper_2D( Kernel::EvalState * evalState, const RefCountPtr< const Lang::ElementaryPath2D > & path, Kernel::Arguments & args, const Ast::SourceLocation & callLoc )
{
  RefCountPtr< const Lang::Function > arrowHead = args.getHandle( 1 )->getVal< const Lang::Function >( "< core function stroke: head >" );
  RefCountPtr< const Lang::Function > arrowTail = args.getHandle( 2 )->getVal< const Lang::Function >( "< core function stroke: tail >" );
  if( ( arrowHead == Lang::THE_NO_ARROW &&
	arrowTail == Lang::THE_NO_ARROW ) ||
      path->size( ) < 2 )
    {
      Kernel::ContRef cont = evalState->cont_;
      cont->takeValue( Kernel::ValueRef( new Lang::PaintedPath2D( evalState->dyn_->getGraphicsState( ), path, "S" ) ),
		       evalState );
      return;
    }
  else
    {
      /* The computation must continue outside here since functions are to be called, and resulting graphics collected.
       */

      if( arrowTail == Lang::THE_NO_ARROW )
	{
	  /* There's only an arrow at the head.
	   */
	  evalState->cont_ = Kernel::ContRef( new Kernel::Stroke2DCont_head( evalState->dyn_->getGraphicsState( ), path, evalState->env_, evalState->dyn_, evalState->cont_, callLoc ) );
	  arrowHead->call( evalState, path->reverse( ), callLoc );
	  return;
	}

      if( arrowHead == Lang::THE_NO_ARROW )
	{
	  /* There's only an arrow at the tail.
	   */
	  evalState->cont_ = Kernel::ContRef( new Kernel::Stroke2DCont_tail( evalState->dyn_->getGraphicsState( ), path, evalState->env_, evalState->dyn_, evalState->cont_, callLoc ) );
	  arrowHead->call( arrowTail, evalState, args.getHandle( 0 ), callLoc );
	  return;
	}

      evalState->cont_ = Kernel::ContRef( new Kernel::Stroke2DCont_both1( evalState->dyn_->getGraphicsState( ), path, arrowHead, evalState->env_, evalState->dyn_, evalState->cont_, callLoc ) );
      arrowTail->call( arrowTail, evalState, args.getHandle( 0 ), callLoc );
      return;
    }
}


namespace MetaPDF
{
  namespace Kernel
  {
    class ArrowheadReceiverFormals3D : public Kernel::EvaluatedFormals
    {
    public:
      ArrowheadReceiverFormals3D( )
	: Kernel::EvaluatedFormals( "< Arrowhead receiver >", true )
      {
	appendEvaluatedCoreFormal( "picture", Kernel::THE_SLOT_VARIABLE );
	appendEvaluatedCoreFormal( "cut", Kernel::VariableHandle( new Kernel::Variable( RefCountPtr< const Lang::Value >( new Lang::Length( 0 ) ) ) ) );
      }
    };

    ArrowheadReceiverFormals3D theArrowheadReceiverFormals3D;
    
    class Stroke3DCont_tail : public Kernel::ForcedStructureContinuation
    {
      RefCountPtr< const Kernel::GraphicsState > graphicsState_;
      RefCountPtr< const Lang::ElementaryPath3D > path_;
      Kernel::PassedEnv env_;
      Kernel::PassedDyn dyn_;
      Kernel::ContRef cont_;
    public:
      Stroke3DCont_tail( const RefCountPtr< const Kernel::GraphicsState > & graphicsState, const RefCountPtr< const Lang::ElementaryPath3D > & path,
			 const Kernel::PassedEnv & env, const Kernel::PassedDyn & dyn, Kernel::ContRef cont, const Ast::SourceLocation & traceLoc )
	: Kernel::ForcedStructureContinuation( "Stroke's arrow tail receiver", traceLoc ), graphicsState_( graphicsState ), path_( path ), env_( env ), dyn_( dyn ), cont_( cont )
      { }
      virtual ~Stroke3DCont_tail( ) { }
      virtual void takeStructure( const RefCountPtr< const Lang::Structure > & structure, Kernel::EvalState * evalState ) const
      {
	/* Argument 0: picture
	 * Argument 1: cut
	 */
	Kernel::Arguments args( & theArrowheadReceiverFormals3D );
	structure->argList_->bind( & args, structure->values_, env_, dyn_ );
	args.applyDefaults( );
	
	typedef const Lang::Drawable3D ArgType0;
	RefCountPtr< ArgType0 > picture = Helpers::down_cast_CoreArgument< ArgType0 >( continuationName_, args, 0, traceLoc_ );
	
	typedef const Lang::Length ArgType1;
	RefCountPtr< ArgType1 > cutTail = Helpers::down_cast_CoreArgument< ArgType1 >( continuationName_, args, 1, traceLoc_ );
	
	if( cutTail->get( ) < 0 )
	  {
	    throw Exceptions::MiscellaneousRequirement( strrefdup( "Arrow tail cut length was negative." ) );
	  }
	else if( cutTail->get( ) == 0 )
	  {
	    cont_->takeValue( Helpers::newGroup3D( evalState->dyn_->getGraphicsState( ),
						   picture,
						   RefCountPtr< const Lang::Drawable3D >( new Lang::PaintedPath3D( graphicsState_, path_, "S" ) ) ),
			      evalState );
	  }
	else
	  {
	    Concrete::SplineTime t1 = path_->arcTime( cutTail->get( ) );
	    Concrete::SplineTime t2( HUGE_VAL );
	    RefCountPtr< const Lang::ElementaryPath3D > subpath = path_->subpath( t1, t2 );
	    if( subpath->size( ) > 0 )
	      {
		cont_->takeValue( Helpers::newGroup3D( evalState->dyn_->getGraphicsState( ),
						       picture,
						       RefCountPtr< const Lang::Drawable3D >( new Lang::PaintedPath3D( graphicsState_, subpath, "S" ) ) ),
			       evalState );
	      }
	    else
	      {
		cont_->takeValue( picture,
				  evalState );
	      }
	    
	  }
      }
      virtual void backTrace( std::list< Kernel::Continuation::BackTraceElem > * trace ) const
      {
	trace->push_front( Kernel::Continuation::BackTraceElem( this, "3D stroke's tail" ) );
	cont_->backTrace( trace );
      }
      virtual void gcMark( Kernel::GCMarkedSet & marked )
      {
	cont_->gcMark( marked );
      }
    };

    class Stroke3DCont_head : public Kernel::ForcedStructureContinuation
    {
      RefCountPtr< const Kernel::GraphicsState > graphicsState_;
      RefCountPtr< const Lang::ElementaryPath3D > path_;
      Kernel::PassedEnv env_;
      Kernel::PassedDyn dyn_;
      Kernel::ContRef cont_;
    public:
      Stroke3DCont_head( const RefCountPtr< const Kernel::GraphicsState > & graphicsState, const RefCountPtr< const Lang::ElementaryPath3D > & path,
			 const Kernel::PassedEnv & env, const Kernel::PassedDyn & dyn, Kernel::ContRef cont, const Ast::SourceLocation & traceLoc )
	: Kernel::ForcedStructureContinuation( "Stroke's arrow tail receiver", traceLoc ), graphicsState_( graphicsState ), path_( path ), env_( env ), dyn_( dyn ), cont_( cont )
      { }
      virtual ~Stroke3DCont_head( ) { }
      virtual void takeStructure( const RefCountPtr< const Lang::Structure > & structure, Kernel::EvalState * evalState ) const
      {
	/* Argument 0: picture
	 * Argument 1: cut
	 */
	Kernel::Arguments args( & theArrowheadReceiverFormals3D );
	structure->argList_->bind( & args, structure->values_, env_, dyn_ );
	args.applyDefaults( );
	
	typedef const Lang::Drawable3D ArgType0;
	RefCountPtr< ArgType0 > picture = Helpers::down_cast_CoreArgument< ArgType0 >( continuationName_, args, 0, traceLoc_ );
	
	typedef const Lang::Length ArgType1;
	RefCountPtr< ArgType1 > cutHead = Helpers::down_cast_CoreArgument< ArgType1 >( continuationName_, args, 1, traceLoc_ );

	if( cutHead->get( ) < 0 )
	  {
	    throw Exceptions::MiscellaneousRequirement( strrefdup( "Arrow head cut length was negative." ) );
	  }
	else if( cutHead->get( ) == 0 )
	  {
	    cont_->takeValue( Helpers::newGroup3D( evalState->dyn_->getGraphicsState( ),
						   picture,
						   RefCountPtr< const Lang::Drawable3D >( new Lang::PaintedPath3D( graphicsState_, path_, "S" ) ) ),
			      evalState );
	  }
	else
	  {
	    Concrete::SplineTime t1( 0 );
	    Concrete::SplineTime t2 = path_->arcTime( path_->arcLength( ) - cutHead->get( ) );
	    RefCountPtr< const Lang::ElementaryPath3D > subpath = path_->subpath( t1, t2 );
	    if( subpath->size( ) > 0 )
	      {
		cont_->takeValue( Helpers::newGroup3D( evalState->dyn_->getGraphicsState( ),
						       picture,
						       RefCountPtr< const Lang::Drawable3D >( new Lang::PaintedPath3D( graphicsState_, subpath, "S" ) ) ),
				  evalState );
	      }
	    else
	      {
		cont_->takeValue( picture,
				  evalState );
	      }
	  }
      }
      virtual void backTrace( std::list< Kernel::Continuation::BackTraceElem > * trace ) const
      {
	trace->push_front( Kernel::Continuation::BackTraceElem( this, "3D stroke's head" ) );
	cont_->backTrace( trace );
      }
      virtual void gcMark( Kernel::GCMarkedSet & marked )
      {
	cont_->gcMark( marked );
      }
    };

    class Stroke3DCont_both2 : public Kernel::ForcedStructureContinuation
    {
      RefCountPtr< const Kernel::GraphicsState > graphicsState_;
      RefCountPtr< const Lang::ElementaryPath3D > path_;
      RefCountPtr< const Lang::Drawable3D > tail_;
      Lang::Length cutTail_;
      Kernel::PassedEnv env_;
      Kernel::PassedDyn dyn_;
      Kernel::ContRef cont_;
    public:
      Stroke3DCont_both2( const RefCountPtr< const Kernel::GraphicsState > & graphicsState, const RefCountPtr< const Lang::ElementaryPath3D > & path,
			  const RefCountPtr< const Lang::Drawable3D > & tail, Lang::Length cutTail,
			  const Kernel::PassedEnv & env, const Kernel::PassedDyn & dyn, Kernel::ContRef cont, const Ast::SourceLocation & traceLoc )
	: Kernel::ForcedStructureContinuation( "Stroke's arrow head receiver", traceLoc ), graphicsState_( graphicsState ), path_( path ), tail_( tail ), cutTail_( cutTail ), env_( env ), dyn_( dyn ), cont_( cont )
      { }
      virtual ~Stroke3DCont_both2( ) { }
      virtual void takeStructure( const RefCountPtr< const Lang::Structure > & structure, Kernel::EvalState * evalState ) const
      {
	/* Argument 0: picture
	 * Argument 1: cut
	 */
	Kernel::Arguments args( & theArrowheadReceiverFormals3D );
	structure->argList_->bind( & args, structure->values_, env_, dyn_ );
	args.applyDefaults( );
	
	typedef const Lang::Drawable3D ArgType0;
	RefCountPtr< ArgType0 > picture = Helpers::down_cast_CoreArgument< ArgType0 >( continuationName_, args, 0, traceLoc_ );
	
	typedef const Lang::Length ArgType1;
	RefCountPtr< ArgType1 > cutHead = Helpers::down_cast_CoreArgument< ArgType1 >( continuationName_, args, 1, traceLoc_ );

	if( cutHead->get( ) < 0 )
	  {
	    throw Exceptions::MiscellaneousRequirement( strrefdup( "Aarrow head cut length was negative." ) );
	  }
	
	if( cutTail_.get( ) == 0 && cutHead->get( ) == 0 )
	  {
	    cont_->takeValue( Helpers::newGroup3D( evalState->dyn_->getGraphicsState( ),
						   picture,
						   tail_,
						   RefCountPtr< const Lang::Drawable3D >( new Lang::PaintedPath3D( graphicsState_, path_, "S" ) ) ),
			      evalState );
	  }
	else
	  {
	    Concrete::SplineTime t1( 0 );
	    if( cutTail_.get( ) > 0 )
	      {
		t1 = path_->arcTime( cutTail_.get( ) );
	      }
	    
	    Concrete::SplineTime t2( HUGE_VAL );
	    if( cutHead->get( ) > 0 )
	      {
		t2 = path_->arcTime( path_->arcLength( ) - cutHead->get( ) );
	      }
	    
	    RefCountPtr< const Lang::ElementaryPath3D > subpath = path_->subpath( t1, t2 );
	    
	    if( subpath->size( ) > 0 )
	      {
		cont_->takeValue( Helpers::newGroup3D( evalState->dyn_->getGraphicsState( ),
						       picture,
						       tail_,
						       RefCountPtr< const Lang::Drawable3D >( new Lang::PaintedPath3D( graphicsState_, subpath, "S" ) ) ),
				  evalState );
	      }
	    else
	      {
		cont_->takeValue( Helpers::newGroup3D( evalState->dyn_->getGraphicsState( ),
						       picture,
						       tail_ ),
				  evalState );
	      }
	  }
      }
      virtual void backTrace( std::list< Kernel::Continuation::BackTraceElem > * trace ) const
      {
	trace->push_front( Kernel::Continuation::BackTraceElem( this, "3D stroke's head & tail, second" ) );
	cont_->backTrace( trace );
      }
      virtual void gcMark( Kernel::GCMarkedSet & marked )
      {
	const_cast< Lang::Drawable3D * >( tail_.getPtr( ) )->gcMark( marked );
	cont_->gcMark( marked );
      }
    };

    class Stroke3DCont_both1 : public Kernel::ForcedStructureContinuation
    {
      RefCountPtr< const Kernel::GraphicsState > graphicsState_;
      RefCountPtr< const Lang::ElementaryPath3D > path_;
      RefCountPtr< const Lang::Function > headFunction_;
      Kernel::PassedEnv env_;
      Kernel::PassedDyn dyn_;
      Kernel::ContRef cont_;
    public:
      Stroke3DCont_both1( const RefCountPtr< const Kernel::GraphicsState > & graphicsState, const RefCountPtr< const Lang::ElementaryPath3D > & path,
			  const RefCountPtr< const Lang::Function > & headFunction,
			  const Kernel::PassedEnv & env, const Kernel::PassedDyn & dyn, Kernel::ContRef cont, const Ast::SourceLocation & traceLoc )
	: Kernel::ForcedStructureContinuation( "Stroke's arrow tail receiver", traceLoc ), graphicsState_( graphicsState ), path_( path ), headFunction_( headFunction ), env_( env ), dyn_( dyn ), cont_( cont )
      { }
      virtual ~Stroke3DCont_both1( ) { }
      virtual void takeStructure( const RefCountPtr< const Lang::Structure > & structure, Kernel::EvalState * evalState ) const
      {
	/* Argument 0: picture
	 * Argument 1: cut
	 */
	Kernel::Arguments args( & theArrowheadReceiverFormals3D );
	structure->argList_->bind( & args, structure->values_, env_, dyn_ );
	args.applyDefaults( );
	
	typedef const Lang::Drawable3D ArgType0;
	RefCountPtr< ArgType0 > picture = Helpers::down_cast_CoreArgument< ArgType0 >( continuationName_, args, 0, traceLoc_ );
	
	typedef const Lang::Length ArgType1;
	RefCountPtr< ArgType1 > cutTail = Helpers::down_cast_CoreArgument< ArgType1 >( continuationName_, args, 1, traceLoc_ );
	
	if( cutTail->get( ) < 0 )
	  {
	    throw Exceptions::MiscellaneousRequirement( strrefdup( "Arrow tail cut length was negative." ) );
	  }
	
	evalState->env_ = env_;
	evalState->dyn_ = dyn_;
	evalState->cont_ = Kernel::ContRef( new Kernel::Stroke3DCont_both2( graphicsState_, path_, picture, *cutTail, env_, dyn_, cont_, traceLoc_ ) );
	headFunction_->call( evalState, path_->reverse( ), traceLoc_ );
      }
      virtual void backTrace( std::list< Kernel::Continuation::BackTraceElem > * trace ) const
      {
	trace->push_front( Kernel::Continuation::BackTraceElem( this, "3D stroke's head & tail, first" ) );
	cont_->backTrace( trace );
      }
      virtual void gcMark( Kernel::GCMarkedSet & marked )
      {
	const_cast< Lang::Function * >( headFunction_.getPtr( ) )->gcMark( marked );
	dyn_->gcMark( marked );
	cont_->gcMark( marked );
      }
    };
    
  }
}


void
Helpers::stroke_helper_3D( Kernel::EvalState * evalState, const RefCountPtr< const Lang::ElementaryPath3D > & path, Kernel::Arguments & args, const Ast::SourceLocation & callLoc )
{
  RefCountPtr< const Lang::Function > arrowHead = args.getHandle( 1 )->getVal< const Lang::Function >( "< core function stroke: head >" );
  RefCountPtr< const Lang::Function > arrowTail = args.getHandle( 2 )->getVal< const Lang::Function >( "< core function stroke: tail >" );
  if( ( arrowHead == Lang::THE_NO_ARROW &&
	arrowTail == Lang::THE_NO_ARROW ) ||
      path->size( ) < 2 )
    {
      Kernel::ContRef cont = evalState->cont_;
      cont->takeValue( Kernel::ValueRef( new Lang::PaintedPath3D( evalState->dyn_->getGraphicsState( ), path, "S" ) ),
		       evalState );
      return;
    }
  else
    {
      /* The computation must continue outside here since functions are to be called, and resulting graphics collected.
       */

      if( arrowTail == Lang::THE_NO_ARROW )
	{
	  /* There's only an arrow at the head.
	   */
	  evalState->cont_ = Kernel::ContRef( new Kernel::Stroke3DCont_head( evalState->dyn_->getGraphicsState( ), path, evalState->env_, evalState->dyn_, evalState->cont_, callLoc ) );
	  arrowHead->call( evalState, path->reverse( ), callLoc );
	  return;
	}
      
      if( arrowHead == Lang::THE_NO_ARROW )
	{
	  /* There's only an arrow at the tail.
	   */
	  evalState->cont_ = Kernel::ContRef( new Kernel::Stroke3DCont_tail( evalState->dyn_->getGraphicsState( ), path, evalState->env_, evalState->dyn_, evalState->cont_, callLoc ) );
	  arrowHead->call( arrowTail, evalState, args.getHandle( 0 ), callLoc );
	  return;
	}

      evalState->cont_ = Kernel::ContRef( new Kernel::Stroke3DCont_both1( evalState->dyn_->getGraphicsState( ), path, arrowHead, evalState->env_, evalState->dyn_, evalState->cont_, callLoc ) );
      arrowTail->call( arrowTail, evalState, args.getHandle( 0 ), callLoc );
      return;
    }
}

Lang::Core_fill::Core_fill( const char * title )
  : CoreFunction( title, new Kernel::EvaluatedFormals( title, true ) )
{
  formals_->appendEvaluatedCoreFormal( "path", Kernel::THE_SLOT_VARIABLE );
  formals_->appendEvaluatedCoreFormal( "tiebreaker", Kernel::THE_VOID_VARIABLE );
}

void
Lang::Core_fill::call( Kernel::EvalState * evalState, Kernel::Arguments & args, const Ast::SourceLocation & callLoc ) const
{
  args.applyDefaults( );

  RefCountPtr< const Lang::Length > tiebreaker = Helpers::down_cast_CoreArgument< const Lang::Length >( title_, args, 1, callLoc, true );

  try
    {
      typedef const Lang::ElementaryPath2D ArgType;
      RefCountPtr< ArgType > path = Helpers::elementaryPathTry2D( args.getValue( 0 ) );
      if( tiebreaker != NullPtr< const Lang::Length >( ) )
	{
	  throw Exceptions::CoreOutOfRange( title_, args, 1, "The tiebreaker may not specified for 2D paths." );
	}
      Kernel::ContRef cont = evalState->cont_;
      cont->takeValue( Kernel::ValueRef( new Lang::PaintedPath2D( evalState->dyn_->getGraphicsState( ), path, "f" ) ),
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
      RefCountPtr< ArgType > path = Helpers::elementaryPathTry3D( args.getValue( 0 ) );
      Concrete::Length tiebreakerVal = Concrete::ZERO_LENGTH;
      if( tiebreaker != NullPtr< const Lang::Length >( ) )
	{
	  tiebreakerVal = tiebreaker->get( );
	}
      Kernel::ContRef cont = evalState->cont_;
      cont->takeValue( Kernel::ValueRef( new Lang::PaintedPath3D( evalState->dyn_->getGraphicsState( ), path, "f", tiebreakerVal ) ),
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
      typedef const Lang::Path2D ArgType;
      RefCountPtr< ArgType > path = Helpers::try_cast_CoreArgument< ArgType >( args.getValue( 0 ) );
      if( tiebreaker != NullPtr< const Lang::Length >( ) )
	{
	  throw Exceptions::CoreOutOfRange( title_, args, 1, "The tiebreaker may not specified for 2D paths." );
	}
      Kernel::ContRef cont = evalState->cont_;
      cont->takeValue( Kernel::ValueRef( new Lang::PaintedPath2D( evalState->dyn_->getGraphicsState( ), path, "f" ) ),
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
      typedef const Lang::Path3D ArgType;
      RefCountPtr< ArgType > path = Helpers::try_cast_CoreArgument< ArgType >( args.getValue( 0 ) );
      Concrete::Length tiebreakerVal = Concrete::ZERO_LENGTH;
      if( tiebreaker != NullPtr< const Lang::Length >( ) )
	{
	  tiebreakerVal = tiebreaker->get( );
	}
      Kernel::ContRef cont = evalState->cont_;
      cont->takeValue( Kernel::ValueRef( new Lang::PaintedPath3D( evalState->dyn_->getGraphicsState( ), path, "f", tiebreakerVal ) ),
		       evalState );
      return;
    }
  catch( const NonLocalExit::NotThisType & ball )
    {
      /* Wrong type; never mind!.. but see below!
       */
    }

  std::cerr << "Throwing in fill." << std::endl ;
  throw Exceptions::CoreTypeMismatch( callLoc, title_, args, 0, Helpers::typeSetString( Lang::Path2D::staticTypeName( ), Lang::Path3D::staticTypeName( ) ) );
}

void
Lang::Core_fillstar::call( Kernel::EvalState * evalState, Kernel::Arguments & args, const Ast::SourceLocation & callLoc ) const
{
  const size_t ARITY = 1;
  CHECK_ARITY( args, ARITY, title_ );
  
  try
    {
      typedef const Lang::ElementaryPath2D ArgType;
      RefCountPtr< ArgType > path = Helpers::elementaryPathTry2D( args.getValue( 0 ) );
      Kernel::ContRef cont = evalState->cont_;
      cont->takeValue( Kernel::ValueRef( new Lang::PaintedPath2D( evalState->dyn_->getGraphicsState( ), path, "f*" ) ),
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
      typedef const Lang::Path2D ArgType;
      RefCountPtr< ArgType > path = Helpers::try_cast_CoreArgument< ArgType >( args.getValue( 0 ) );
      Kernel::ContRef cont = evalState->cont_;
      cont->takeValue( Kernel::ValueRef( new Lang::PaintedPath2D( evalState->dyn_->getGraphicsState( ), path, "f*" ) ),
		       evalState );
      return;
    }
  catch( const NonLocalExit::NotThisType & ball )
    {
      /* Wrong type; never mind!.. but see below!
       */
    }

  throw Exceptions::CoreTypeMismatch( callLoc, title_, args, 0, Helpers::typeSetString( Lang::ElementaryPath2D::staticTypeName( ), Lang::Path2D::staticTypeName( ) ) );
}


void
Lang::Core_bboxed::call( Kernel::EvalState * evalState, Kernel::Arguments & args, const Ast::SourceLocation & callLoc ) const
{
  const size_t ARITY = 2;
  CHECK_ARITY( args, ARITY, title_ );
  
  size_t argsi = 0;

  typedef const Lang::Drawable2D ArgType1;
  RefCountPtr< ArgType1 > obj = Helpers::down_cast_CoreArgument< ArgType1 >( title_, args, argsi, callLoc );

  ++argsi;

  typedef const Lang::ElementaryPath2D ArgType2;
  RefCountPtr< ArgType2 > p = Helpers::elementaryPathCast2D( title_, args, argsi, callLoc );

  Kernel::ContRef cont = evalState->cont_;
  cont->takeValue( Kernel::ValueRef( new Lang::BBoxed2D( obj, p ) ),
		   evalState );
}

void
Lang::Core_clip::call( Kernel::EvalState * evalState, Kernel::Arguments & args, const Ast::SourceLocation & callLoc ) const
{
  const size_t ARITY = 2;
  CHECK_ARITY( args, ARITY, title_ );
  
  size_t argsi = 0;

  typedef const Lang::Drawable2D ArgType1;
  RefCountPtr< ArgType1 > obj = Helpers::down_cast_CoreArgument< ArgType1 >( title_, args, argsi, callLoc );

  RefCountPtr< Lang::Clipped2D > res( new Lang::Clipped2D( obj, clipCommand_ ) );

  ++argsi;
  try
    {
      typedef const Lang::ElementaryPath2D ArgType;
      RefCountPtr< ArgType > path = Helpers::elementaryPathTry2D( args.getValue( argsi ) );
      res->addSubPath( path );
      goto done;
    }
  catch( const NonLocalExit::NotThisType & ball )
    {
      /* Wrong type; never mind!.. but see below!
       */
    }
  
  try
    {
      typedef const Lang::SoftMask ArgType;
      RefCountPtr< ArgType > mask = Helpers::try_cast_CoreArgument< ArgType >( args.getValue( argsi ) );
      Kernel::ContRef cont = evalState->cont_;
      cont->takeValue( RefCountPtr< Lang::SoftMasked2D >( new Lang::SoftMasked2D( obj, mask ) ),
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
      typedef const Lang::Path2D ArgType;
      RefCountPtr< ArgType > path = Helpers::try_cast_CoreArgument< ArgType >( args.getValue( argsi ) );
      for( Lang::Path2D::const_iterator i = path->begin( ); i != path->end( ); ++i )
	{
	  {
	    typedef const Lang::ElementaryPath2D ArgType;
	    RefCountPtr< ArgType > subpath = (*i).down_cast< ArgType >( );
	    if( subpath != NullPtr< ArgType >( ) )
	      {
		res->addSubPath( subpath );
		continue;
	      }
	  }
	    
	  {
	    typedef const Lang::Connection2D ArgType;
	    ArgType * subpath = dynamic_cast< ArgType * >( (*i).getPtr( ) );
	    if( subpath != 0 )
	      {
		res->addSubPath( subpath->getElementaryPath( ) );
		continue;
	      }
	  }
	  throw Exceptions::InternalError( "clip: Encountered a subpath of unexpected type" );
	}
      goto done;
    }
  catch( const NonLocalExit::NotThisType & ball )
    {
      /* Wrong type; never mind!.. but see below!
       */
    }

  throw Exceptions::CoreTypeMismatch( callLoc, title_, args, 0, Helpers::typeSetString( Lang::ElementaryPath2D::staticTypeName( ), Lang::Path2D::staticTypeName( ) ) );

 done:
  Kernel::ContRef cont = evalState->cont_;
  cont->takeValue( res,
		   evalState );
}

void
Lang::Core_from3Dto2D::call( Kernel::EvalState * evalState, Kernel::Arguments & args, const Ast::SourceLocation & callLoc ) const
{
  const size_t ARITY = 1;
  CHECK_ARITY( args, ARITY, title_ );
  
  typedef const Lang::Geometric3D ArgType;
  RefCountPtr< ArgType > obj = Helpers::down_cast_CoreArgument< ArgType >( title_, args, 0, callLoc );

  Kernel::ContRef cont = evalState->cont_;
  cont->takeValue( obj->to2D( evalState->dyn_, obj ),
		   evalState );
}

void
Lang::Core_from2Dto3D::call( Kernel::EvalState * evalState, Kernel::Arguments & args, const Ast::SourceLocation & callLoc ) const
{
  const size_t ARITY = 1;
  CHECK_ARITY( args, ARITY, title_ );
  
  typedef const Lang::Geometric2D ArgType;
  RefCountPtr< ArgType > obj = Helpers::down_cast_CoreArgument< ArgType >( title_, args, 0, callLoc );

  Kernel::ContRef cont = evalState->cont_;
  cont->takeValue( obj->to3D( obj ),
		   evalState );
}

Lang::Core_facing2Din3D::Core_facing2Din3D( const char * title )
  : CoreFunction( title, new Kernel::EvaluatedFormals( title, true ) )
{
  formals_->appendEvaluatedCoreFormal( "obj", Kernel::THE_SLOT_VARIABLE );
  formals_->appendEvaluatedCoreFormal( "scale", Kernel::VariableHandle( new Kernel::Variable( Lang::THE_FALSE ) ) );
  formals_->appendEvaluatedCoreFormal( "distort", Kernel::VariableHandle( new Kernel::Variable( Lang::THE_FALSE ) ) );
}

void
Lang::Core_facing2Din3D::call( Kernel::EvalState * evalState, Kernel::Arguments & args, const Ast::SourceLocation & callLoc ) const
{
  args.applyDefaults( );

  Kernel::ContRef cont = evalState->cont_;
  cont->takeValue( Kernel::ValueRef( new Lang::Facing2Din3D( Helpers::down_cast_CoreArgument< const Lang::Drawable2D >( title_, args, 0, callLoc ),
								 Helpers::down_cast_CoreArgument< const Lang::Boolean >( title_, args, 1, callLoc )->val_,
								 Helpers::down_cast_CoreArgument< const Lang::Boolean >( title_, args, 2, callLoc )->val_ ) ),
		   evalState );
}

Lang::Core_facetnormal::Core_facetnormal( const char * title )
  : CoreFunction( title, new Kernel::EvaluatedFormals( title, true ) )
{
  formals_->appendEvaluatedCoreFormal( "location", Kernel::THE_SLOT_VARIABLE );
  formals_->appendEvaluatedCoreFormal( "normal", Kernel::THE_SLOT_VARIABLE );
}

void
Lang::Core_facetnormal::call( Kernel::EvalState * evalState, Kernel::Arguments & args, const Ast::SourceLocation & callLoc ) const
{
  args.applyDefaults( );

  typedef const Lang::Coords3D LocationType;
  typedef const Lang::FloatTriple NormalType;

  RefCountPtr< LocationType > location = Helpers::down_cast_CoreArgument< LocationType >( title_, args, 0, callLoc, true );
  RefCountPtr< NormalType > normal = Helpers::down_cast_CoreArgument< NormalType >( title_, args, 1, callLoc, true );

  RefCountPtr< const Kernel::FacetState > facetState = evalState->dyn_->getFacetState( );

  Kernel::ContRef cont = evalState->cont_;

  RefCountPtr< const Lang::Color > nonStroking = evalState->dyn_->getGraphicsState( )->nonStrokingColor_;
  try
    {
      typedef const Lang::Gray ColorType;
      RefCountPtr< ColorType > lightMultiply = Helpers::try_cast_CoreArgument< ColorType >( nonStroking );

      cont->takeValue( Kernel::ValueRef( new Lang::FacetNormalGray( Concrete::Coords3D( location->x_.get( ),
											location->y_.get( ),
											location->z_.get( ) ),
								    facetState->reflections_,
								    Concrete::UnitFloatTriple( normal->x_, normal->y_, normal->z_ ),
								    lightMultiply->components( ),
								    facetState->autoScattering_,
								    Helpers::down_cast_CoreDynamic< ColorType >( title_, Lang::DYNAMIC_VARIABLE_ID_NONSTROKING, facetState->autoIntensity_, callLoc )->components( ) ) ),
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
      typedef const Lang::RGB ColorType;
      RefCountPtr< ColorType > lightMultiply = Helpers::try_cast_CoreArgument< ColorType >( nonStroking );

      cont->takeValue( Kernel::ValueRef( new Lang::FacetNormalRGB( Concrete::Coords3D( location->x_.get( ),
										       location->y_.get( ),
										       location->z_.get( ) ),
								   facetState->reflections_,
								   Concrete::UnitFloatTriple( normal->x_, normal->y_, normal->z_ ),
								   lightMultiply,
								   facetState->autoScattering_,
								   Helpers::down_cast_CoreDynamic< ColorType >( title_, Lang::DYNAMIC_VARIABLE_ID_NONSTROKING, facetState->autoIntensity_, callLoc ) ) ),
		       evalState );
      return;
    }
  catch( const NonLocalExit::NotThisType & ball )
    {
      /* Wrong type; never mind!.. but see below!
       */
    }
  
  throw Exceptions::CoreDynamicTypeMismatch( callLoc, title_, Lang::DYNAMIC_VARIABLE_ID_NONSTROKING,
					     nonStroking->getTypeName( ),
					     Helpers::typeSetString( Lang::Gray::staticTypeName( ), Lang::RGB::staticTypeName( ) ) );
}


Lang::Core_facet::Core_facet( const char * title )
  : CoreFunction( title, new Kernel::EvaluatedFormals( title, true ) )
{
  formals_->appendEvaluatedCoreFormal( "path", Kernel::THE_SLOT_VARIABLE );
  formals_->appendEvaluatedCoreFormal( "n1", Kernel::THE_VOID_VARIABLE );
  formals_->appendEvaluatedCoreFormal( "n2", Kernel::THE_VOID_VARIABLE );
  formals_->appendEvaluatedCoreFormal( "n3", Kernel::THE_VOID_VARIABLE );
  formals_->appendEvaluatedCoreFormal( "tiebreaker", Kernel::VariableHandle( new Kernel::Variable( RefCountPtr< const Lang::Value >( new Lang::Length( Concrete::ZERO_LENGTH ) ) ) ) );
  formals_->appendEvaluatedCoreFormal( "double", Kernel::THE_VOID_VARIABLE );
}

void
Lang::Core_facet::call( Kernel::EvalState * evalState, Kernel::Arguments & args, const Ast::SourceLocation & callLoc ) const
{
  args.applyDefaults( );

  // Note that the <double> defaults to false if and only if there is at least one normal specifyed, and all normals agree on what is the outward normal.

  typedef const Lang::ElementaryPath3D PathType;
  typedef const Lang::FacetNormalGray NormalType;
  typedef const Lang::Length TiebreakerType;
  typedef const Lang::Boolean DoubleSidedType;

  RefCountPtr< PathType > path = Helpers::elementaryPathCast3D( title_, args, 0, callLoc );
  RefCountPtr< NormalType > n1 = Helpers::down_cast_CoreArgument< NormalType >( title_, args, 1, callLoc, true );
  RefCountPtr< NormalType > n2 = Helpers::down_cast_CoreArgument< NormalType >( title_, args, 2, callLoc, true );
  RefCountPtr< NormalType > n3 = Helpers::down_cast_CoreArgument< NormalType >( title_, args, 3, callLoc, true );
  RefCountPtr< TiebreakerType > tiebreaker = Helpers::down_cast_CoreArgument< TiebreakerType >( title_, args, 4, callLoc );
  RefCountPtr< DoubleSidedType > doubleSided = Helpers::down_cast_CoreArgument< DoubleSidedType >( title_, args, 5, callLoc, true );

  if( path->size( ) < 3 )
    {
      throw Exceptions::CoreOutOfRange( title_, args, 0, "A facet path must have at least 3 points." );
    }
  if( ! path->isClosed( ) )
    {
      throw Exceptions::CoreOutOfRange( title_, args, 0, "A facet path must be closed." );
    }
  {
    for( PathType::const_iterator i = path->begin( ); i != path->end( ); ++i )
      {
	if( (*i)->front_ != (*i)->mid_ || (*i)->rear_ != (*i)->mid_ )
	  {
	    throw Exceptions::CoreOutOfRange( title_, args, 0, "A facet path must be a polygon." );
	  }
      }
  }

  size_t numNormals = 0;

  if( n1 == NullPtr< NormalType >( ) )
    {
      if( n2 != NullPtr< NormalType >( ) )
	{
	  throw Exceptions::CoreOutOfRange( title_, args, 2, "The normal n1 must be provided before providing n2." );
	}
      if( n3 != NullPtr< NormalType >( ) )
	{
	  throw Exceptions::CoreOutOfRange( title_, args, 3, "The normal n1 must be provided before providing n3." );
	}
    }
  else
    {
      ++numNormals;
    }
  if( n2 == NullPtr< NormalType >( ) )
    {
      if( n3 != NullPtr< NormalType >( ) )
	{
	  throw Exceptions::CoreOutOfRange( title_, args, 3, "The normal n2 must be provided before providing n3." );
	}
    }
  else
    {
      ++numNormals;
    }

  if( n3 != NullPtr< NormalType >( ) )
    {
      ++numNormals;
    }

  Kernel::ContRef cont = evalState->cont_;

  Concrete::Coords3D p0( 0, 0, 0 );
  Concrete::Coords3D p1( 0, 0, 0 );
  Concrete::Coords3D p2( 0, 0, 0 );
  path->getRepresentativePoints( Lang::THE_3D_IDENTITY, & p0, & p1, & p2 );

  Concrete::UnitFloatTriple normal( 1., 0., 0., 1. );
  try
    {
      normal = Concrete::crossDirection( p2 - p0, p1 - p0 );
    }
  catch( const NonLocalExit::CrossDirectionOfParallel & ball )
    {
      // This means that the crossDirection called failed because the vectors were parallel.
      // A polygon of lower dimension is invisible, so we may just return an empty object.
      cont->takeValue( Lang::THE_NULL3D,
		       evalState );
      return;
    }

  bool isDoubleSided;
  if( doubleSided != NullPtr< DoubleSidedType >( ) )
    {
      isDoubleSided = doubleSided->val_;
    }
  else
    {
      if( numNormals == 0 )
	{
	  isDoubleSided = true;
	}
      else
	{
	  bool allAgree = true;
	  bool n1Agree = Concrete::inner( normal, n1->normal( ) ) > 0;
	  if( n2 != NullPtr< NormalType >( ) )
	    {
	      allAgree = allAgree && ( ( Concrete::inner( normal, n2->normal( ) ) > 0 ) == n1Agree );
	    }
	  if( n3 != NullPtr< NormalType >( ) )
	    {
	      allAgree = allAgree && ( ( Concrete::inner( normal, n3->normal( ) ) > 0 ) == n1Agree );
	    }
	  if( allAgree )
	    {
	      if( ! n1Agree )
		{
		  normal = normal.reverse( );
		}
	      isDoubleSided = false;
	    }
	  else
	    {
	      std::cerr << "Warning: A facet without explicitly specified double-sidedness had normals pointing in crazy directions, making it double sided." << std::endl ;
	      isDoubleSided = true;
	    }
	}
    }


  RefCountPtr< const Computation::FacetInterpolatorGray > interpolator = RefCountPtr< const Computation::FacetInterpolatorGray >( NullPtr< const Computation::FacetInterpolatorGray >( ) );
  if( numNormals == 0 )
    {
      RefCountPtr< const Lang::Color > nonStroking = evalState->dyn_->getGraphicsState( )->nonStrokingColor_;
      try
	{
	  typedef const Lang::Gray ColorType;
	  RefCountPtr< ColorType > lightMultiply = Helpers::try_cast_CoreArgument< ColorType >( nonStroking );

	  RefCountPtr< const Kernel::FacetState > facetState = evalState->dyn_->getFacetState( );

	  interpolator = RefCountPtr< const Computation::FacetInterpolatorGray >
	    ( new Computation::FacetInterpolatorGray1
	      ( RefCountPtr< const Lang::FacetNormalGray >
		( new Lang::FacetNormalGray
		  ( (1./3)*( p0 + p1 + p2 ),
		    facetState->reflections_,
		    normal,
		    lightMultiply->components( ),
		    facetState->autoScattering_,
		    Helpers::down_cast_CoreDynamic< ColorType >( title_, Lang::DYNAMIC_VARIABLE_ID_AUTOINTENSITY, facetState->autoIntensity_, callLoc )->components( ) ) ) ) );
	  
	  goto done;
	}
      catch( const NonLocalExit::NotThisType & ball )
	{
	  /* Wrong type; never mind!.. but see below!
	   */
	}
      
      throw Exceptions::CoreDynamicTypeMismatch( callLoc, title_, Lang::DYNAMIC_VARIABLE_ID_NONSTROKING,
						 nonStroking->getTypeName( ),
						 Helpers::typeSetString( Lang::Gray::staticTypeName( ), Lang::RGB::staticTypeName( ) ) );
    }
  else if( numNormals == 1 )
    {
      interpolator = RefCountPtr< const Computation::FacetInterpolatorGray >
	( new Computation::FacetInterpolatorGray1( n1 ) );
    }
  else if( numNormals == 2 )
    {
      interpolator = RefCountPtr< const Computation::FacetInterpolatorGray >
	( new Computation::FacetInterpolatorGray2( n1, n2 ) );
    }
  else if( numNormals == 3 )
    {
      interpolator = RefCountPtr< const Computation::FacetInterpolatorGray >
	( new Computation::FacetInterpolatorGray3( n1, n2, n3 ) );
    }
  else
    {
      throw Exceptions::InternalError( "Number of facet normals is out of range!" );
    }

 done:
  RefCountPtr< const Kernel::FacetState > facetState = evalState->dyn_->getFacetState( );
 
  cont->takeValue( Kernel::ValueRef( new Lang::SingleSided3DGray( path, interpolator,
								  ! isDoubleSided,  // Note that this argument refers to single-sidedness
								  normal, Concrete::inner( normal, p0 ),
								  tiebreaker->get( ),
								  facetState->viewResolution_,
								  facetState->shadeOrder_ ) ),
		   evalState );
}
