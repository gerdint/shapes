#include "MetaPDF_Helpers_decls.h"

#include "astfun.h"
#include "metapdfexceptions.h"
#include "metapdfcore.h"
#include "consts.h"
#include "globals.h"
#include "continuations.h"
#include "astvar.h"

#include <sstream>

using namespace MetaPDF;
using namespace std;


Kernel::Formals::Formals( )
  : seenDefault_( false ), argumentOrder_( new std::map< const char *, size_t, charPtrLess > ),
    sink_( 0 ),
    stateOrder_( new std::map< const char *, size_t, charPtrLess > )
{ }

Kernel::Formals::Formals( size_t numberOfDummyDefaultExprs )
  : seenDefault_( false ), argumentOrder_( new std::map< const char *, size_t, charPtrLess > ),
    sink_( 0 ),
    stateOrder_( new std::map< const char *, size_t, charPtrLess > )
{
  // Pushing null expressions is just a way of specifying how many non-sink arguments there are.
  for( size_t i = 0; i < numberOfDummyDefaultExprs; ++i )
    {
      defaultExprs_.push_back( 0 );
    }
}

Kernel::Formals::~Formals( )
{
  delete argumentOrder_;
  delete stateOrder_;
  if( sink_ != 0  )
    {
      delete sink_;
    }
}

void
Kernel::Formals::setLoc( Ast::SourceLocation loc )
{
  loc_ = loc;
}


void
Kernel::Formals::push_exprs( Ast::ArgListExprs * args ) const
{
  typedef typeof defaultExprs_ ListType;
  for( ListType::const_iterator i = defaultExprs_.begin( ); i != defaultExprs_.end( ); ++i )
    {
      if( *i != 0 )
	{
	  args->orderedExprs_->push_back( *i );
	}
    }
}

Kernel::EvaluatedFormals *
Kernel::Formals::newEvaluatedFormals( Kernel::Arguments & args ) const
{
  size_t pos = 0;
  return newEvaluatedFormals( args, & pos );
}

Kernel::EvaluatedFormals *
Kernel::Formals::newEvaluatedFormals( Kernel::Arguments & args, size_t * pos ) const
{
  Kernel::EvaluatedFormals * res = new Kernel::EvaluatedFormals( const_cast< Kernel::Formals * >( this ) );
  res->isSink_ = false; // The formals created here belong to user functions, which are exactly those which are not sinks.

  res->defaults_.reserve( defaultExprs_.size( ) );
  res->locations_.reserve( defaultExprs_.size( ) );

  typedef typeof defaultExprs_ ListType;
  for( ListType::const_iterator i = defaultExprs_.begin( ); i != defaultExprs_.end( ); ++i )
    {
      if( *i != 0 )
	{
	  res->defaults_.push_back( args.getHandle( *pos ) );
	  res->locations_.push_back( args.getNode( *pos ) );
	  ++(*pos);
	}
      else
	{
	  res->defaults_.push_back( Kernel::THE_SLOT_VARIABLE );
	  res->locations_.push_back( 0 );                               // I really hope this is never dereferenced!
	}
    }

  return res;
}

std::vector< bool > *
Kernel::Formals::newArgListForcePos( const Ast::ArgListExprs * argList ) const
{
  /* Here, we use the knowledge that ordered arguments are evaluated backwards, and named arguments
   * in the natural order (the lexiographic order of std::map).
   */

  std::vector< bool > * res = new std::vector< bool >;
  res->resize( argList->orderedExprs_->size( ) );
  res->reserve( argList->orderedExprs_->size( ) + argList->namedExprs_->size( ) );
  
    if( argList->orderedExprs_->size( ) > 0 )
      {
	typedef typeof forcePos_ SrcType;
	SrcType::const_iterator src = forcePos_.begin( );
	for( size_t arg = argList->orderedExprs_->size( ) - 1; ; --arg, ++src )
	  {
	    (*res)[ arg ] = *src;
	    if( arg == 0 )
	      {
		break;
	      }
	  }
      }

  {
    typedef typeof *argList->namedExprs_ MapType;
    MapType::const_iterator end = argList->namedExprs_->end( );
    for( MapType::const_iterator arg = argList->namedExprs_->begin( ); arg != end; ++arg )
      {
	res->push_back( forcePos_[ (*argumentOrder_)[ arg->first ] ] );
      }
  }

  return res;
}

std::vector< bool > *
Kernel::Formals::newArgListForcePos( const Ast::ArgListExprs * argList, const Kernel::Arguments & curryArgs ) const
{
  /* Compare with the non-curry version!
   */

  std::vector< bool > * res = new std::vector< bool >;
  res->resize( argList->orderedExprs_->size( ) );
  res->reserve( argList->orderedExprs_->size( ) + argList->namedExprs_->size( ) );
  
  if( argList->orderedExprs_->size( ) > 0 )
    {
      typedef typeof forcePos_ SrcType;
      SrcType::const_iterator src = forcePos_.begin( );
      size_t curryPos = 0;
      while( curryPos < curryArgs.size( ) &&
	     ! curryArgs.isSlot( curryPos ) )
	{
	  ++src;
	  ++curryPos;
	}
      for( size_t arg = argList->orderedExprs_->size( ) - 1; ; --arg )
	{
	  (*res)[ arg ] = *src;
	  if( arg == 0 )
	    {
	      break;
	    }
	  ++src;
	  ++curryPos;
	  while( curryPos < curryArgs.size( ) &&
		 ! curryArgs.isSlot( curryPos ) )
	    {
	      ++src;
	      ++curryPos;
	    }
	}
    }

  {
    typedef typeof *argList->namedExprs_ MapType;
    MapType::const_iterator end = argList->namedExprs_->end( );
    for( MapType::const_iterator arg = argList->namedExprs_->begin( ); arg != end; ++arg )
      {
	res->push_back( forcePos_[ (*argumentOrder_)[ arg->first ] ] );
      }
  }

  return res;
}

const Ast::SourceLocation &
Kernel::Formals::loc( ) const
{
  return loc_;
}



Ast::ArgListExprs::ConstIterator::ConstIterator( std::list< Ast::Expression * >::const_reverse_iterator i1, std::map< const char *, Ast::Expression *, charPtrLess >::const_iterator i2, const size_t & index )
  : i1_( i1 ), i2_( i2 ), index_( index )
{ }

Ast::ArgListExprs::ConstIterator::ConstIterator( const Ast::ArgListExprs::ConstIterator & orig )
  : i1_( orig.i1_ ), i2_( orig.i2_ ), index_( orig.index_ )
{ }

Ast::ArgListExprs::ArgListExprs( bool exprOwner )
  : exprOwner_( exprOwner ), orderedExprs_( new std::list< Ast::Expression * > ), namedExprs_( new std::map< const char *, Ast::Expression *, charPtrLess > ),
    orderedStates_( new std::list< Ast::StateReference * > ), namedStates_( new std::map< const char *, Ast::StateReference *, charPtrLess > )
{
  //  if( exprOwner_ )
  //    {
  //      throw Exceptions::InternalError( strrefdup( "Ast::ArgListExprs( bool _exprOwner ) can only be called with false." ) );
  //    }
}

Ast::ArgListExprs::ArgListExprs( std::list< Ast::Expression * > * orderedExprs, std::map< const char *, Ast::Expression *, charPtrLess > * namedExprs, std::list< Ast::StateReference * > * orderedStates, std::map< const char *, Ast::StateReference *, charPtrLess > * namedStates )
  : exprOwner_( true ), orderedExprs_( orderedExprs ), namedExprs_( namedExprs ), orderedStates_( orderedStates ), namedStates_( namedStates )
{ }

Ast::ArgListExprs::~ArgListExprs( )
{
  {
    if( exprOwner_ )
      {
	typedef list< Ast::Expression * >::iterator I;
	for( I i = orderedExprs_->begin( ); i != orderedExprs_->end( ); ++i )
	  {
	    delete *i;
	  }
      }
    delete orderedExprs_;
  }

  {
    if( exprOwner_ )
      {
	typedef std::map< const char *, Ast::Expression *, charPtrLess >::const_iterator I;
	for( I i = namedExprs_->begin( ); i != namedExprs_->end( ); ++i )
	  {
	    delete i->first;
	    delete i->second;
	  }
      }
    delete namedExprs_;
  }

  {
    if( exprOwner_ )
      {
	typedef std::list< Ast::StateReference * >::iterator I;
	for( I i = orderedStates_->begin( ); i != orderedStates_->end( ); ++i )
	  {
	    delete *i;
	  }
      }
    delete orderedStates_;
  }

  {
    if( exprOwner_ )
      {
	typedef std::map< const char *, Ast::StateReference *, charPtrLess >::const_iterator I;
	for( I i = namedStates_->begin( ); i != namedStates_->end( ); ++i )
	  {
	    delete i->first;
	    delete i->second;
	  }
      }
    delete namedStates_;
  }
}

Ast::ArgListExprs::ArgListExprs( size_t numberOfOrderedDummyExprs )
  : exprOwner_( true ), orderedExprs_( new std::list< Ast::Expression * > ), namedExprs_( new std::map< const char *, Ast::Expression *, charPtrLess > ), orderedStates_( new typeof *orderedStates_ ), namedStates_( new typeof *namedStates_ )
{
  for( size_t i = 0; i < numberOfOrderedDummyExprs; ++i )
    {
      orderedExprs_->push_back( new Ast::DummyExpression );
    }
}


void
Ast::ArgListExprs::evaluate( const RefCountPtr< const Kernel::CallContInfo > & info, const Ast::ArgListExprs::ConstIterator & pos, const RefCountPtr< const Lang::SingleList > & vals, Kernel::EvalState * evalState ) const
{
  std::list< Ast::Expression * >::const_reverse_iterator i1end = orderedExprs_->rend( );
  
  if( pos.i1_ == i1end &&
      pos.i2_ == namedExprs_->end( ) )
    {
      evalState->cont_ = info->cont_;
      info->cont_->takeValue( vals, evalState );
      return;
    }
  
  if( pos.i1_ != i1end )
    {
      if( info->force( pos.index_ ) || (*(pos.i1_))->immediate_ )
	{
	  typedef typeof const_cast< Ast::ArgListExprs::ConstIterator & >( pos ).i1_ Iterator;
	  Iterator next = pos.i1_;
	  ++next;
	  evalState->expr_ = *(pos.i1_);
	  evalState->env_ = info->env_;
	  evalState->dyn_ = info->dyn_;
	  evalState->cont_ = Kernel::ContRef( new Kernel::CallCont_n( evalState->expr_->loc( ),
								      info,
								      Ast::ArgListExprs::ConstIterator( next, pos.i2_, pos.index_ + 1 ),
								      vals ) );
	  return;
	}
      else
	{
	  /* Delay evaluation of this argument by just putting a thunk in the list.
	   */
	  typedef typeof const_cast< Ast::ArgListExprs::ConstIterator & >( pos ).i1_ Iterator;
	  Iterator next = pos.i1_;
	  ++next;
	  evaluate( info,
		    Ast::ArgListExprs::ConstIterator( next, pos.i2_, pos.index_ + 1 ),
		    RefCountPtr< const Lang::SingleListPair >( new Lang::SingleListPair( Kernel::VariableHandle( new Kernel::Variable( new Kernel::Thunk( info->env_, info->dyn_, *pos.i1_ ) ) ),
											       vals ) ),
		    evalState );
	  return;  /* It is not really important that the above compiles to a tail call.  Hopefully, it does, but otherwise it is easy to see that the chain of recursive calls to this function
		    * will always completes without evaluation of expressions. */
	}
    }
  else
    {
      if( info->force( pos.index_ ) || pos.i2_->second->immediate_ )
	{
	  typedef typeof const_cast< Ast::ArgListExprs::ConstIterator & >( pos ).i2_ Iterator;
	  Iterator next = pos.i2_;
	  ++next;
	  evalState->expr_ = pos.i2_->second;
	  evalState->env_ = info->env_;
	  evalState->dyn_ = info->dyn_;
	  evalState->cont_ = Kernel::ContRef( new Kernel::CallCont_n( evalState->expr_->loc( ),
								      info,
								      Ast::ArgListExprs::ConstIterator( pos.i1_, next, pos.index_ + 1 ),
								      vals ) );
	  return;
	}
      else
	{
	  /* Delay evaluation of this argument by just putting a thunk in the list.
	   */
	  typedef typeof const_cast< Ast::ArgListExprs::ConstIterator & >( pos ).i2_ Iterator;
	  Iterator next = pos.i2_;
	  ++next;
	  evaluate( info,
		    Ast::ArgListExprs::ConstIterator( pos.i1_, next, pos.index_ + 1 ),
		    RefCountPtr< const Lang::SingleListPair >( new Lang::SingleListPair( Kernel::VariableHandle( new Kernel::Variable( new Kernel::Thunk( info->env_, info->dyn_, pos.i2_->second ) ) ),
											       vals ) ),
		    evalState );
	  return;  /* It is not really important that the above compiles to a tail call.  Hopefully, it does, but otherwise it is easy to see that the chain of recursive calls to this function
		    * will always complete without evaluation of expressions. */
	}
    }
}

void
Ast::ArgListExprs::bind( Kernel::Arguments * dst, RefCountPtr< const Lang::SingleList > vals, Kernel::PassedEnv env, Kernel::PassedDyn dyn ) const
{
  typedef const Lang::SingleListPair ConsType;

  /* Note that the arguments are bound in backwards-order, since that is how the values are accessed.
   */
  
  {
    Ast::SourceLocation dummy;
    typedef std::map< const char *, Ast::Expression *, charPtrLess >::const_reverse_iterator I;
    I i = namedExprs_->rbegin( );
    I end = namedExprs_->rend( );
    for( ; i != end; ++i )
      {
	RefCountPtr< ConsType > lst = vals.down_cast< ConsType >( );
	if( lst == NullPtr< ConsType >( ) )
	  {
	    throw Exceptions::InternalError( strrefdup( "Out of argument values when binding application." ) );
	  }	
	dst->addNamedArgument( i->first, lst->car_, i->second );
	vals = lst->cdr_;
      }
  }

  {
    typedef list< Ast::Expression * >::const_iterator I;
    I i = orderedExprs_->begin( );
    I end = orderedExprs_->end( );
    for( ; i != end; ++i )
      {
	RefCountPtr< ConsType > lst = vals.down_cast< ConsType >( );
	if( lst == NullPtr< ConsType >( ) )
	  {
	    throw Exceptions::InternalError( strrefdup( "Out of argument values when binding application." ) );
	  }	
	dst->addOrderedArgument( lst->car_, *i );
	vals = lst->cdr_;
      }
  }

  /* Here, it could/should be verified that vals is null.  However, it only isn't in case of an internal error...
   */

  /* Next, we turn to the states.  The states need no evaluation, as they are always passed by reference.
   */

  {
    Ast::SourceLocation dummy;
    typedef std::map< const char *, Ast::StateReference *, charPtrLess >::const_iterator I;
    I i = namedStates_->begin( );
    I end = namedStates_->end( );
    for( ; i != end; ++i )
      {
	dst->addNamedState( i->first, i->second->getHandle( env, dyn ) , i->second );
      }
  }

  {
    typedef list< Ast::StateReference * >::const_iterator I;
    I i = orderedStates_->begin( );
    I end = orderedStates_->end( );
    for( ; i != end; ++i )
      {
	dst->addOrderedState( (*i)->getHandle( env, dyn ), *i );
      }
  }

}


Kernel::VariableHandle
Ast::ArgListExprs::findNamed( RefCountPtr< const Lang::SingleList > vals, const char * name ) const
{
  /* This function is called when a Lang::Structure is asked for a field by name.
   * This is reflected in the generated error in case the name is not found.
   */

  typedef const Lang::SingleListPair ConsType;

  /* Note that the arguments are bound in backwards-order, since that is how the values are accessed.
   */
  
  Ast::SourceLocation dummy;
  typedef std::map< const char *, Ast::Expression *, charPtrLess >::const_reverse_iterator I;
  I i = namedExprs_->rbegin( );
  I end = namedExprs_->rend( );
  for( ; i != end; ++i )
    {
      RefCountPtr< ConsType > lst = vals.down_cast< ConsType >( );
      if( lst == NullPtr< ConsType >( ) )
	{
	  throw Exceptions::InternalError( strrefdup( "Out of argument values when searching fields." ) );
	}
      if( strcmp( i->first, name ) == 0 )
	{
	  return lst->car_;
	}
      vals = lst->cdr_;
    }

  throw Exceptions::NonExistentMember( strrefdup( "< user union >" ), name );
}

Kernel::VariableHandle
Ast::ArgListExprs::getOrdered( RefCountPtr< const Lang::SingleList > vals, size_t pos ) const
{
  /* This function is called when a Lang::Structure is asked for a field by position.
   * This is reflected in the generated error in case the position is not found.
   */

  typedef const Lang::SingleListPair ConsType;

  /* Note that the arguments are bound in backwards-order, since that is how the values are accessed.
   */

  size_t i = 0;
  {
    typedef list< Ast::Expression * >::const_iterator I;
    while( true )
      {
	RefCountPtr< ConsType > lst = vals.down_cast< ConsType >( );
	if( lst == NullPtr< ConsType >( ) )
	  {
	    break;
	  }	
	if( i == pos )
	  {
	    return lst->car_;
	  }
	vals = lst->cdr_;
	++i;
      }
  }

  throw Exceptions::NonExistentPosition( pos, i - 1 );
}

void
Ast::ArgListExprs::analyze( Ast::Node * parent, const Ast::AnalysisEnvironment * env )
{
  parent_ = parent;

  /* Traverse children
   */
  {
    typedef typeof *orderedExprs_ ListType;
    for( ListType::iterator i = orderedExprs_->begin( ); i != orderedExprs_->end( ); ++i )
      {
	(*i)->analyze( parent_, env );
      }
  }
  {
    typedef typeof *namedExprs_ ListType;
    for( ListType::iterator i = namedExprs_->begin( ); i != namedExprs_->end( ); ++i )
      {
	i->second->analyze( parent_, env );
      }
  }
  {
    typedef typeof *orderedStates_ ListType;
    for( ListType::iterator i = orderedStates_->begin( ); i != orderedStates_->end( ); ++i )
      {
	(*i)->analyze( parent_, env );
      }
  }
  {
    typedef typeof *namedStates_ ListType;
    for( ListType::iterator i = namedStates_->begin( ); i != namedStates_->end( ); ++i )
      {
	i->second->analyze( parent_, env );
      }
  }
  
  /* Analyze this node given that the children have been analyzed.
   */
  imperative_ = orderedStates_->size( ) > 0 || namedStates_->size( ) > 0;
  {
    typedef typeof *orderedExprs_ ListType;
    for( ListType::const_iterator i = orderedExprs_->begin( ); i != orderedExprs_->end( ); ++i )
      {
	imperative_ = imperative_ || (*i)->imperative_;
      }
  }
  {typedef typeof *namedExprs_ ListType;
    for( ListType::const_iterator i = namedExprs_->begin( ); i != namedExprs_->end( ); ++i )
      {
	imperative_ = imperative_ || i->second->imperative_;
      }
  }
}

Ast::ArgListExprs::ConstIterator
Ast::ArgListExprs::begin( ) const
{
  return Ast::ArgListExprs::ConstIterator( orderedExprs_->rbegin( ), namedExprs_->begin( ), 0 );
}


Ast::FunctionFunction::FunctionFunction( const Ast::SourceLocation & loc, const Kernel::Formals * formals, Ast::Expression * body, const Ast::FunctionMode & functionMode )
  : Lang::Function( new Kernel::EvaluatedFormals( "< function construction >", false ) ), loc_( loc ), formals_( formals ), body_( body ), functionMode_( functionMode )
{ }

Ast::FunctionFunction::~FunctionFunction( )
{
  delete formals_;
  delete body_;
}

void
Ast::FunctionFunction::push_exprs( Ast::ArgListExprs * args ) const
{
  formals_->push_exprs( args );
}

void
Ast::FunctionFunction::analyze( Ast::Node * parent, const Ast::AnalysisEnvironment * parentEnv )
{
  Ast::AnalysisEnvironment * env( new Ast::AnalysisEnvironment( Ast::theAnalysisEnvironmentList, parentEnv, formals_->argumentOrder_, formals_->stateOrder_ ) );
  if( ( functionMode_ & Ast::FUNCTION_PROCEDURAL ) == 0 )
    {
      env->activateFunctionBoundary( );
    }

  body_->analyze( parent, env );
}

void
Ast::FunctionFunction::call( Kernel::EvalState * evalState, Kernel::Arguments & args, const Ast::SourceLocation & callLoc ) const
{
  Kernel::ContRef cont = evalState->cont_;
  cont->takeValue( Kernel::ValueRef( new Lang::UserFunction( formals_->newEvaluatedFormals( args ),
							     body_, evalState->env_, functionMode_ ) ),
		   evalState );
}


Kernel::CallCont_last::CallCont_last( const RefCountPtr< const Lang::Function > & fun, const Ast::ArgListExprs * argList, bool curry, const Kernel::PassedEnv & env, const Kernel::PassedDyn & dyn, const Kernel::ContRef & cont, const Ast::SourceLocation & callLoc )
  : Kernel::Continuation( callLoc ), fun_( fun ), argList_( argList ), curry_( curry ), env_( env ), dyn_( dyn ), cont_( cont )
{ }

Kernel::CallCont_last::~CallCont_last( )
{ }

void
Kernel::CallCont_last::takeValue( const RefCountPtr< const Lang::Value > & valsUntyped, Kernel::EvalState * evalState, bool dummy ) const
{
  typedef const Lang::SingleList ArgType;
  RefCountPtr< ArgType > vals = Helpers::down_cast< ArgType >( valsUntyped, "< Internal error situation in CallCont_last >" );
  
  Kernel::Arguments args = fun_->newCurriedArguments( );
  argList_->bind( & args, vals, env_, dyn_ );

  if( curry_ )
    {
      evalState->cont_ = cont_;
      cont_->takeValue( Kernel::ValueRef( new Lang::CuteFunction( fun_, args ) ), evalState );
    }
  else
    {
      evalState->dyn_ = dyn_;
      evalState->cont_ = cont_;
      fun_->call( evalState, args, traceLoc_ );
    }
}

void
Kernel::CallCont_last::backTrace( std::list< Kernel::Continuation::BackTraceElem > * trace ) const
{
  trace->push_front( Kernel::Continuation::BackTraceElem( this, "function call's application" ) );
  cont_->backTrace( trace );
}

void
Kernel::CallCont_last::gcMark( Kernel::GCMarkedSet & marked )
{
  const_cast< Lang::Function * >( fun_.getPtr( ) )->gcMark( marked );
  dyn_->gcMark( marked );
  cont_->gcMark( marked );
}

Kernel::CallContInfo::CallContInfo( const Ast::ArgListExprs * argList, const Kernel::EvalState & evalState, std::vector< bool > * forcePos )
  : forcePos_( forcePos ), forceAll_( false ), argList_( argList ), env_( evalState.env_ ), dyn_( evalState.dyn_ ), cont_( evalState.cont_ )
{ }

Kernel::CallContInfo::CallContInfo( const Ast::ArgListExprs * argList, const Kernel::EvalState & evalState, bool forceAll )
  : forcePos_( 0 ), forceAll_( forceAll ), argList_( argList ), env_( evalState.env_ ), dyn_( evalState.dyn_ ), cont_( evalState.cont_ )
{ }

Kernel::CallContInfo::~CallContInfo( )
{
  if( forcePos_ != 0 )
    {
      delete forcePos_;
    }
}

bool
Kernel::CallContInfo::force( const size_t & pos ) const
{
  if( forcePos_ != 0 )
    {
      return (*forcePos_)[ pos ];
    }
  return forceAll_;
}

bool
Kernel::CallContInfo::isSelective( ) const
{
  return forcePos_ != 0;
}

void
Kernel::CallContInfo::gcMark( Kernel::GCMarkedSet & marked )
{
  env_->gcMark( marked );
  dyn_->gcMark( marked );
  cont_->gcMark( marked );
}

Kernel::CallCont_n::CallCont_n( const Ast::SourceLocation & traceLoc, const RefCountPtr< const Kernel::CallContInfo > & info, const Ast::ArgListExprs::ConstIterator & pos, RefCountPtr< const Lang::SingleList > vals )
  : Kernel::Continuation( traceLoc ), info_( info ), pos_( pos ), vals_( vals )
{ }

Kernel::CallCont_n::~CallCont_n( )
{ }

void
Kernel::CallCont_n::takeHandle( Kernel::VariableHandle val, Kernel::EvalState * evalState, bool dummy ) const
{
  /* This continuation really seeks forced arguments, for otherwise a thunk would have been generated directly.
   * However, this continuation takes handles anyway, since handles is what goes into the argument list.
   */

  if( val->isThunk( ) )
    {
      val->force( val, evalState );
      return;
    }
  info_->argList_->evaluate( info_,
			     pos_,
			     RefCountPtr< const Lang::SingleListPair >( new Lang::SingleListPair( val, vals_ ) ),
			     evalState );
}

void
Kernel::CallCont_n::backTrace( std::list< Kernel::Continuation::BackTraceElem > * trace ) const
{
  trace->push_front( Kernel::Continuation::BackTraceElem( this, "function call's argument" ) );
  info_->cont_->backTrace( trace );
}

void
Kernel::CallCont_n::gcMark( Kernel::GCMarkedSet & marked )
{
  const_cast< Kernel::CallContInfo * >( info_.getPtr( ) )->gcMark( marked );
  const_cast< Lang::SingleList * >( vals_.getPtr( ) )->gcMark( marked );
}


Kernel::CallCont_1::CallCont_1( const Ast::SourceLocation & traceLoc, const Ast::ArgListExprs * argList, bool curry, bool procedural, const Kernel::EvalState & evalState, const Ast::SourceLocation & callLoc )
  : Kernel::Continuation( traceLoc ), argList_( argList ), curry_( curry ), procedural_( procedural ), env_( evalState.env_ ), dyn_( evalState.dyn_ ), cont_( evalState.cont_ ), callLoc_( callLoc )
{ }

Kernel::CallCont_1::~CallCont_1( )
{ }

void
Kernel::CallCont_1::takeValue( const RefCountPtr< const Lang::Value > & funUntyped, Kernel::EvalState * evalState, bool dummy ) const
{
  {
    typedef const Lang::Function ArgType;
    RefCountPtr< ArgType > fun = funUntyped.down_cast< const Lang::Function >( );
    if( fun != NullPtr< ArgType >( ) )
      {
	if( procedural_ != fun->isProcedural( ) )
	  {
	    if( procedural_ )
	      {
		throw Exceptions::OutOfRange( traceLoc_, "Expected procedure." );
	      }
	    throw Exceptions::OutOfRange( traceLoc_, "Expected function." );
	  }
	evalState->env_ = env_;
	evalState->dyn_ = dyn_;
	evalState->cont_ = Kernel::ContRef( new Kernel::CallCont_last( fun, argList_, curry_, env_, dyn_, cont_, callLoc_ ) );
	argList_->evaluate( fun->newCallContInfo( argList_, *evalState ),
			    argList_->begin( ), Lang::THE_CONS_NULL,
			    evalState );
	return;
      }
  }
  {
    typedef const Lang::Transform2D ArgType;
    ArgType * transformVal = dynamic_cast< ArgType * >( funUntyped.getPtr( ) );
    if( transformVal != 0 )
      {
	if( curry_ )
	  {
	    throw Exceptions::MiscellaneousRequirement( strrefdup( "Don't Curry transform applications.  It's useless anyway!" ) );
	  }
	if( argList_->orderedExprs_->size( ) != 1 )
	  {
	    throw Exceptions::CoreArityMismatch( "<transform application>", 1, argList_->orderedExprs_->size( ) );
	  }
	if( argList_->namedExprs_->size( ) != 0 )
	  {
	    throw Exceptions::CoreNoNamedFormals( "<transform application>" );
	  }
	evalState->expr_ = argList_->orderedExprs_->front( );
	evalState->env_ = env_;
	evalState->dyn_ = dyn_;
	evalState->cont_ = Kernel::ContRef( new Kernel::Transform2DCont( *transformVal, cont_, callLoc_ ) );
	return;
      }
  }
  {
    typedef const Lang::Transform3D ArgType;
    ArgType * transformVal = dynamic_cast< ArgType * >( funUntyped.getPtr( ) );
    if( transformVal != 0 )
      {
	if( curry_ )
	  {
	    throw Exceptions::MiscellaneousRequirement( strrefdup( "Don't Curry transform applications.  It's useless anyway!" ) );
	  }
	if( argList_->orderedExprs_->size( ) != 1 )
	  {
	    throw Exceptions::CoreArityMismatch( "<transform application>", 1, argList_->orderedExprs_->size( ) );
	  }
	if( argList_->namedExprs_->size( ) != 0 )
	  {
	    throw Exceptions::CoreNoNamedFormals( "<transform application>" );
	  }
	evalState->expr_ = argList_->orderedExprs_->front( );
	evalState->env_ = env_;
	evalState->dyn_ = dyn_;
	evalState->cont_ = Kernel::ContRef( new Kernel::Transform3DCont( *transformVal, cont_, callLoc_ ) );
	return;
      }
  }

  {
    typedef const Lang::ElementaryPath2D ArgType;
    RefCountPtr< ArgType > path = NullPtr< ArgType >( );
    try
      {
	path = Helpers::elementaryPathCast2D( funUntyped, this );
      }
    catch( const Exceptions::ContinuationTypeMismatch & ball )
      {
	goto nextType1;
      }

    if( curry_ )
      {
	throw Exceptions::MiscellaneousRequirement( strrefdup( "Don't Curry path point selection.  It's useless anyway!" ) );
      }
    if( argList_->orderedExprs_->size( ) != 1 )
      {
	throw Exceptions::CoreArityMismatch( "<path point selection>", 1, argList_->orderedExprs_->size( ) );
      }
    if( argList_->namedExprs_->size( ) != 0 )
      {
	throw Exceptions::CoreNoNamedFormals( "<path point selection>" );
      }
    
    evalState->expr_ = argList_->orderedExprs_->front( );
    evalState->env_ = env_;
    evalState->dyn_ = dyn_;
    evalState->cont_ = Kernel::ContRef( new Kernel::PathApplication2DCont( path,
									   cont_,
									   callLoc_ ) );
    return;
  }
 nextType1:

  {
    typedef const Lang::ElementaryPath3D ArgType;
    RefCountPtr< ArgType > path = NullPtr< ArgType >( );
    try
      {
	path = Helpers::elementaryPathCast3D( funUntyped, this );
      }
    catch( const Exceptions::ContinuationTypeMismatch & ball )
      {
	goto nextType2;
      }
    
    if( curry_ )
      {
	throw Exceptions::MiscellaneousRequirement( strrefdup( "Don't Curry path point selection.  It's useless anyway!" ) );
      }
    if( argList_->orderedExprs_->size( ) != 1 )
      {
	throw Exceptions::CoreArityMismatch( "<path point selection>", 1, argList_->orderedExprs_->size( ) );
      }
    if( argList_->namedExprs_->size( ) != 0 )
      {
	throw Exceptions::CoreNoNamedFormals( "<path point selection>" );
      }
    
    evalState->expr_ = argList_->orderedExprs_->front( );
    evalState->env_ = env_;
    evalState->dyn_ = dyn_;
    evalState->cont_ = Kernel::ContRef( new Kernel::PathApplication3DCont( path,
									   cont_,
									   callLoc_ ) );
    return;
  }
 nextType2:

  throw Exceptions::TypeMismatch( traceLoc_,
				  funUntyped->getTypeName( ),
				  Helpers::typeSetString( Lang::Function::staticTypeName( ),
							  Lang::Transform2D::staticTypeName( ),
							  Lang::Transform3D::staticTypeName( ),
							  Lang::ElementaryPath2D::staticTypeName( ),
							  Lang::ElementaryPath3D::staticTypeName( ) ) );
}

void
Kernel::CallCont_1::backTrace( std::list< Kernel::Continuation::BackTraceElem > * trace ) const
{
  trace->push_front( Kernel::Continuation::BackTraceElem( this, "function call's function" ) );
  cont_->backTrace( trace );
}

void
Kernel::CallCont_1::gcMark( Kernel::GCMarkedSet & marked )
{
  dyn_->gcMark( marked );
  cont_->gcMark( marked );
}



Ast::CallExpr::CallExpr( const Ast::SourceLocation & loc, Ast::Expression * funExpr, Ast::ArgListExprs * argList, bool curry, bool procedural )
  : Ast::Expression( loc ), curry_( curry ), procedural_( procedural ), constFun_( Kernel::THE_NO_FUNCTION ), funExpr_( funExpr ), argList_( argList )
{ }

Ast::CallExpr::CallExpr( const Ast::SourceLocation & loc, const RefCountPtr< const Lang::Function > & constFun, Ast::ArgListExprs * argList, bool curry, bool procedural )
  : Ast::Expression( loc ), curry_( curry ), procedural_( procedural ), constFun_( constFun ), funExpr_( 0 ), argList_( argList )
{ }

Ast::CallExpr::~CallExpr( )
{
  if( funExpr_ != 0 )
    {
      delete funExpr_;
    }
  delete argList_;
}


void
Ast::CallExpr::analyze( Ast::Node * parent, const Ast::AnalysisEnvironment * env )
{
  parent_ = parent;

  if( funExpr_ != 0 )
    {
      funExpr_->analyze( this, env );
    }
  else
    {
      const_cast< Lang::Function * >( constFun_.getPtr( ) )->analyze( this, env );
    }
  argList_->analyze( this, env );

  imperative_ = procedural_ || argList_->imperative_;
  if( funExpr_ != 0 )
    {
      imperative_ = imperative_ || funExpr_->imperative_;
    }
}

void
Ast::CallExpr::eval( Kernel::EvalState * evalState ) const
{
  if( funExpr_ != 0 )
    {
      evalState->expr_ = funExpr_;
      evalState->cont_ = Kernel::ContRef( new Kernel::CallCont_1( evalState->expr_->loc( ), argList_, curry_, procedural_, *evalState, loc_ ) );
    }
  else
    {
      if( procedural_ )
	{
	  throw Exceptions::InternalError( "Function calling syntax should not be procedural." );
	}
      evalState->cont_ = Kernel::ContRef( new Kernel::CallCont_last( constFun_, argList_, curry_, evalState->env_, evalState->dyn_, evalState->cont_, loc_ ) );
      argList_->evaluate( constFun_->newCallContInfo( argList_, *evalState ),
			  argList_->begin( ), Lang::THE_CONS_NULL,
			  evalState );
    }
}

Kernel::UnionCont_last::UnionCont_last( const Ast::ArgListExprs * argList, const Kernel::ContRef & cont, const Ast::SourceLocation & callLoc )
  : Kernel::Continuation( callLoc ), argList_( argList ), cont_( cont )
{ }

Kernel::UnionCont_last::~UnionCont_last( )
{ }

void
Kernel::UnionCont_last::takeValue( const RefCountPtr< const Lang::Value > & valsUntyped, Kernel::EvalState * evalState, bool dummy ) const
{
  typedef const Lang::SingleList ArgType;
  RefCountPtr< ArgType > vals = Helpers::down_cast< ArgType >( valsUntyped, "< Internal error situation in UnionCont_last >" );
  
  evalState->cont_ = cont_;
  cont_->takeValue( Kernel::ValueRef( new Lang::Structure( argList_, vals ) ),
		    evalState );
}

void
Kernel::UnionCont_last::backTrace( std::list< Kernel::Continuation::BackTraceElem > * trace ) const
{
  trace->push_front( Kernel::Continuation::BackTraceElem( this, "union" ) );
  cont_->backTrace( trace );
}

void
Kernel::UnionCont_last::gcMark( Kernel::GCMarkedSet & marked )
{
  cont_->gcMark( marked );
}


Ast::UnionExpr::UnionExpr( const Ast::SourceLocation & loc, Ast::ArgListExprs * argList )
  : Ast::Expression( loc ), argList_( argList )
{ }

Ast::UnionExpr::~UnionExpr( )
{
  delete argList_;
}


void
Ast::UnionExpr::analyze( Ast::Node * parent, const Ast::AnalysisEnvironment * env )
{
  parent_ = parent;
  
  argList_->analyze( this, env );

  imperative_ = argList_->imperative_;
}

void
Ast::UnionExpr::eval( Kernel::EvalState * evalState ) const
{
  evalState->cont_ = Kernel::ContRef( new Kernel::UnionCont_last( argList_, evalState->cont_, loc_ ) );
  
  argList_->evaluate( RefCountPtr< Kernel::CallContInfo >( new Kernel::CallContInfo( argList_, *evalState, false ) ),
		      argList_->begin( ), Lang::THE_CONS_NULL,
		      evalState );
}


Kernel::SplitCont_1::SplitCont_1( const Ast::SourceLocation & traceLoc, Ast::Expression * argList, bool curry, bool procedural, const Kernel::EvalState & evalState, const Ast::SourceLocation & callLoc )
  : Kernel::Continuation( traceLoc ), argList_( argList ), curry_( curry ), procedural_( procedural ), env_( evalState.env_ ), dyn_( evalState.dyn_ ), cont_( evalState.cont_ ), callLoc_( callLoc )
{ }

Kernel::SplitCont_1::~SplitCont_1( )
{ }

void
Kernel::SplitCont_1::takeValue( const RefCountPtr< const Lang::Value > & funUntyped, Kernel::EvalState * evalState, bool dummy ) const
{
  typedef const Lang::Function ArgType;
  RefCountPtr< ArgType > fun = Helpers::down_cast< ArgType >( funUntyped, "Split's function" );
  
  if( procedural_ != fun->isProcedural( ) )
    {
      if( procedural_ )
	{
	  throw Exceptions::OutOfRange( traceLoc_, "Expected procedure." );
	}
      throw Exceptions::OutOfRange( traceLoc_, "Expected function." );
    }

  evalState->env_ = env_;
  evalState->dyn_ = dyn_;
  evalState->cont_ = Kernel::ContRef( new Kernel::SplitCont_2( fun, curry_, env_, dyn_, cont_, callLoc_ ) );
  evalState->expr_ = argList_;
}

void
Kernel::SplitCont_1::backTrace( std::list< Kernel::Continuation::BackTraceElem > * trace ) const
{
  trace->push_front( Kernel::Continuation::BackTraceElem( this, "split call's function" ) );
  cont_->backTrace( trace );
}

void
Kernel::SplitCont_1::gcMark( Kernel::GCMarkedSet & marked )
{
  dyn_->gcMark( marked );
  cont_->gcMark( marked );
}


Kernel::SplitCont_2::SplitCont_2( const RefCountPtr< const Lang::Function > & fun, bool curry, const Kernel::PassedEnv & env, const Kernel::PassedDyn & dyn, const Kernel::ContRef & cont, const Ast::SourceLocation & callLoc )
  : Kernel::Continuation( callLoc ), fun_( fun ), curry_( curry ), env_( env ), dyn_( dyn ), cont_( cont )
{ }

Kernel::SplitCont_2::~SplitCont_2( )
{ }

void
Kernel::SplitCont_2::takeValue( const RefCountPtr< const Lang::Value > & valsUntyped, Kernel::EvalState * evalState, bool dummy ) const
{
  typedef const Lang::Structure ArgType;
  RefCountPtr< ArgType > structure = Helpers::down_cast< ArgType >( valsUntyped, "Split" );

  Kernel::Arguments args = fun_->newCurriedArguments( );
  structure->argList_->bind( & args, structure->values_, env_, dyn_ );

  if( curry_ )
    {
      evalState->cont_ = cont_;
      cont_->takeValue( Kernel::ValueRef( new Lang::CuteFunction( fun_, args ) ),
			evalState );
    }
  else
    {
      evalState->dyn_ = dyn_;
      evalState->cont_ = cont_;
      fun_->call( evalState, args, traceLoc_ );
    }
}

void
Kernel::SplitCont_2::backTrace( std::list< Kernel::Continuation::BackTraceElem > * trace ) const
{
  trace->push_front( Kernel::Continuation::BackTraceElem( this, "Split/splice" ) );
  cont_->backTrace( trace );
}

void
Kernel::SplitCont_2::gcMark( Kernel::GCMarkedSet & marked )
{
  cont_->gcMark( marked );
}


Ast::CallSplitExpr::CallSplitExpr( const Ast::SourceLocation & loc, Ast::Expression * funExpr, Ast::Expression * argList, bool curry, bool procedural )
  : Ast::Expression( loc ), curry_( curry ), procedural_( procedural ), funExpr_( funExpr ), argList_( argList )
{ }

Ast::CallSplitExpr::~CallSplitExpr( )
{
  delete funExpr_;
  delete argList_;
}


void
Ast::CallSplitExpr::analyze( Ast::Node * parent, const Ast::AnalysisEnvironment * env )
{
  parent_ = parent;

  funExpr_->analyze( this, env );
  argList_->analyze( this, env );

  imperative_ = procedural_ || funExpr_->imperative_ || argList_->imperative_;
}

void
Ast::CallSplitExpr::eval( Kernel::EvalState * evalState ) const
{
  evalState->expr_ = funExpr_;
  evalState->cont_ = Kernel::ContRef( new Kernel::SplitCont_1( evalState->expr_->loc( ), argList_, curry_, procedural_, *evalState, loc_ ) );
}


Ast::DummyExpression::DummyExpression( )
  : Ast::Expression( Ast::SourceLocation( ) )
{ }

Ast::DummyExpression::DummyExpression( const Ast::SourceLocation & loc )
  : Ast::Expression( loc )
{ }

Ast::DummyExpression::~DummyExpression( )
{ }

void
Ast::DummyExpression::analyze( Ast::Node * parent, const Ast::AnalysisEnvironment * env )
{
  parent_ = parent;

  imperative_ = false;
}

void
Ast::DummyExpression::eval( Kernel::EvalState * evalState ) const
{
  throw Exceptions::InternalError( strrefdup( "A DummyExpression must never be evaluated." ) );
}

