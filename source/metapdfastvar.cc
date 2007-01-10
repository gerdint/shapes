#include "metapdfastvar.h"
#include "metapdfexceptions.h"
#include "globals.h"
#include "autoonoff.h"
#include "specialunits.h"
#include "metapdfastfun.h"
#include "continuations.h"

using namespace MetaPDF;
using namespace std;


Ast::CodeBracket::CodeBracket( const Ast::SourceLocation & loc, std::list< Ast::Node * > * nodes )
  : Ast::Expression( loc ), nodes_( nodes ), argumentOrder_( new typeof *argumentOrder_ ), dynamicMap_( 0 )
{
  for( std::list< Ast::Node * >::const_iterator i = nodes_->begin( );
       i != nodes_->end( );
       ++i )
    {
      typedef const Ast::BindNode T;
      T * tmp = dynamic_cast< T * >( *i );
      if( tmp != 0 )
	{
	  const char * name = tmp->id( );
	  {
	    typedef const Ast::DynamicVariableDecl T;
	    T * dynDecl = dynamic_cast< T * >( tmp );
	    if( dynDecl != 0 )
	      {
		if( dynamicMap_ == 0 )
		  {
		    dynamicMap_ = new std::map< const char *, size_t, charPtrLess >;
		  }
		if( dynamicMap_->find( name ) != dynamicMap_->end( ) )
		  {
		    throw Exceptions::IntroducingExisting( tmp->idLoc( ), name );
		  }
		dynamicMap_->insert( std::pair< const char *, size_t >( name, dynamicMap_->size( ) ) );
		continue;
	      }
	  }
	  if( argumentOrder_->find( name ) != argumentOrder_->end( ) )
	    {
	      throw Exceptions::IntroducingExisting( tmp->idLoc( ), name );
	    }
	  argumentOrder_->insert( std::pair< const char *, size_t >( name, argumentOrder_->size( ) ) );
	}
    }
}

Ast::CodeBracket::~CodeBracket( )
{
  typedef list< Ast::Node * >::iterator I;
  for( I i = nodes_->begin( ); i != nodes_->end( ); ++i )
    {
      delete *i;
    }
  delete nodes_;
  delete argumentOrder_;
  if( dynamicMap_ != 0 )
    {
      delete dynamicMap_;
    }
}


void
Ast::CodeBracket::eval( Kernel::EvalState * evalState ) const
{
  if( nodes_->begin( ) == nodes_->end( ) )
    {
      return;
    }

  std::vector< Kernel::HandleType > * envValues = new std::vector< Kernel::HandleType >;
  envValues->reserve( argumentOrder_->size( ) );
  while( envValues->size( ) < argumentOrder_->size( ) )
    {
      envValues->push_back( NullPtr< Kernel::Variable >( ) );
    }
  evalState->env_ = new Kernel::Environment( Kernel::theEnvironmentList, evalState->env_, argumentOrder_, RefCountPtr< std::vector< Kernel::HandleType > >( envValues ) );
  if( dynamicMap_ != 0 )
    {
      evalState->env_->setupDynamicKeyVariables( dynamicMap_ );
    }

  RefCountPtr< const Kernel::CodeBracketContInfo > info( new Kernel::CodeBracketContInfo( this, *evalState ) );
  
  evalAt( info, nodes_->begin( ), evalState );
}

void
Ast::CodeBracket::evalAt( const RefCountPtr< const Kernel::CodeBracketContInfo > & info, const std::list< Ast::Node * >::const_iterator & pos, Kernel::EvalState * evalState ) const
{
  {
    std::list< Ast::Node * >::const_iterator next = pos;
    ++next;
    if( next == nodes_->end( ) )
      {
	const Ast::Expression * e = dynamic_cast< const Ast::Expression * >( *pos );
	if( e != 0 &&
	    e->immediate_ )
	  {
	    evalState->cont_ = Kernel::ContRef( new Kernel::ForcingContinuation( info->cont_, (*pos)->loc( ) ) );
	  }
	else
	  {
	    evalState->cont_ = info->cont_;
	  }
      }
    else
      {
	evalState->cont_ = Kernel::ContRef( new Kernel::CodeBracketContinuation( (*pos)->loc( ), info, next ) );
      }
  }
  evalState->env_ = info->env_;
  evalState->dyn_ = info->dyn_;
  (*pos)->eval( evalState );
}


Kernel::CodeBracketContInfo::CodeBracketContInfo( const Ast::CodeBracket * bracketExpr, const Kernel::EvalState & evalState )
  : bracketExpr_( bracketExpr ), env_( evalState.env_ ), dyn_( evalState.dyn_ ), cont_( evalState.cont_ )
{ }

Kernel::CodeBracketContInfo::~CodeBracketContInfo( )
{ }

void
Kernel::CodeBracketContInfo::gcMark( Kernel::GCMarkedSet & marked )
{
  env_->gcMark( marked );
  dyn_->gcMark( marked );
  cont_->gcMark( marked );
}

Kernel::CodeBracketContinuation::CodeBracketContinuation( const Ast::SourceLocation & traceLoc, const RefCountPtr< const Kernel::CodeBracketContInfo > & info, const std::list< Ast::Node * >::const_iterator & pos )
  : Kernel::Continuation( traceLoc ), info_( info ), pos_( pos )
{ }

Kernel::CodeBracketContinuation::~CodeBracketContinuation( )
{ }

void
Kernel::CodeBracketContinuation::takeValue( const RefCountPtr< const Lang::Value > & val, Kernel::EvalState * evalState, bool dummy ) const
{
  if( val.down_cast< const Lang::Void >( ) == NullPtr< const Lang::Void >( ) )
    {
      throw Exceptions::NonVoidStatement( traceLoc_, val );
    }
  info_->bracketExpr_->evalAt( info_,
			       pos_,
			       evalState );
}

void
Kernel::CodeBracketContinuation::backTrace( std::list< Kernel::Continuation::BackTraceElem > * trace ) const
{
  trace->push_front( Kernel::Continuation::BackTraceElem( this, "code bracket" ) );
  info_->cont_->backTrace( trace );
}

void
Kernel::CodeBracketContinuation::gcMark( Kernel::GCMarkedSet & marked )
{
  const_cast< Kernel::CodeBracketContInfo * >( info_.getPtr( ) )->gcMark( marked );
}


Ast::LexiographicVariable::LexiographicVariable( const Ast::SourceLocation & loc, const char * id, Kernel::Environment::LexicalKey ** idKey, bool warm )
  : Ast::Expression( loc ), id_( id ), idKey_( idKey ), warm_( warm )
{
  immediate_ = true;
}

Ast::LexiographicVariable::~LexiographicVariable( )
{
  delete id_;
  if( *idKey_ != 0 )
    {
      delete *idKey_;
    }
  delete idKey_;     //  This can be done only as long as this is not shared!
}

void
Ast::LexiographicVariable::eval( Kernel::EvalState * evalState ) const
{
  if( *idKey_ == 0 )
    {
      *idKey_ = new Kernel::Environment::LexicalKey( evalState->env_->findLexicalKey( loc_, id_ ) );
    }

  evalState->env_->lookup( **idKey_, warm_, evalState );
}


Ast::EvalOutsideExpr::EvalOutsideExpr( const Ast::SourceLocation & loc, Ast::Expression * expr )
  : Ast::Expression( loc ), expr_( expr )
{
  immediate_ = true;
}

Ast::EvalOutsideExpr::~EvalOutsideExpr( )
{
  delete expr_;
}

void
Ast::EvalOutsideExpr::eval( Kernel::EvalState * evalState ) const
{
  evalState->expr_ = expr_;
  evalState->env_ = evalState->env_->getParent( );
}


Ast::MemberReferenceFunction::MemberReferenceFunction( const Ast::SourceLocation & loc, Ast::Expression * variable, const char * fieldID )
  : Lang::Function( new Kernel::EvaluatedFormals( "<>.<>)", true ) ), loc_( loc ), variable_( variable ), fieldID_( fieldID )
{ }

Ast::MemberReferenceFunction::~MemberReferenceFunction( )
{
  delete variable_;
  delete fieldID_;
}

void
Ast::MemberReferenceFunction::push_exprs( Ast::ArgListExprs * args ) const
{
  args->orderedExprs_->push_back( variable_ );
}

void
Ast::MemberReferenceFunction::call( Kernel::EvalState * evalState, Kernel::Arguments & args, const Ast::SourceLocation & callLoc ) const
{
  Kernel::ContRef cont = evalState->cont_;
  RefCountPtr< const Lang::Value > arg = args.getValue( 0 );
  cont->takeHandle( arg->getField( fieldID_, arg ),
		    evalState );
}



Ast::SpecialLength::SpecialLength( const Ast::SourceLocation & loc, double val,  int sort )
  : Ast::Expression( loc ), val_( val ), sort_( sort )
{ }

Ast::SpecialLength::~SpecialLength( )
{ }

void
Ast::SpecialLength::eval( Kernel::EvalState * evalState ) const
{
  Concrete::Length d;
  double a0;
  double a1;
  
  evalState->dyn_->specialUnitService( & d, & a0, & a1 );
  
  if( sort_ == Computation::SPECIALU_NOINFLEX )
    {
      Kernel::ContRef cont = evalState->cont_;
      cont->takeValue( Kernel::ValueRef( new Lang::Length( val_ * d * Computation::specialUnitNoInflexion( a0, a1 ) ) ),
		       evalState );
      return;
    }
  if( ! sort_ & Computation::SPECIALU_DIST )
    {
      throw Exceptions::InternalError( strrefdup( "The special unit is neither based on inflexsion or distance" ) );
    }

  double res = 1;

  if( sort_ & Computation::SPECIALU_CIRC )
    {
      res *= Computation::specialUnitCircleHandle( a0 );
    }

  if( sort_ & Computation::SPECIALU_CORR )
    { 
      res *= Computation::specialUnitCorrection( a0, a1 );
    }

  if( sort_ & Computation::SPECIALU_NOINFLEX )
    {
      res = min( res, Computation::specialUnitNoInflexion( a0, a1 ) );
    }
  Kernel::ContRef cont = evalState->cont_;
  cont->takeValue( Kernel::ValueRef( new Lang::Length( val_ * d * res ) ),
		   evalState );
}


Ast::DynamicVariable::DynamicVariable( const Ast::SourceLocation & loc, const char * id, bool warm )
  : Ast::Expression( loc ), id_( id ), warm_( warm ), idKey_( new Kernel::Environment::LexicalKey * ( 0 ) )
{
  immediate_ = true;
}

Ast::DynamicVariable::~DynamicVariable( )
{
  delete id_;
  if( *idKey_ != 0 )
    {
      delete *idKey_;
    }
  delete idKey_;     //  This can be done only as long as this is not shared!
}

void
Ast::DynamicVariable::eval( Kernel::EvalState * evalState ) const
{
  if( id_ != 0 )
    {
      /* This is not the default destination
       */

      if( *idKey_ == 0 )
	{
	  *idKey_ = new Kernel::Environment::LexicalKey( evalState->env_->findLexicalDynamicKey( loc_, id_ ) );
	}
      
      const Kernel::DynamicVariableProperties & dynProps = evalState->env_->lookupDynamicVariable( **idKey_ );
      
      Kernel::HandleType res = dynProps.fetch( evalState->dyn_ );

      if( res->isWarm( ) != warm_ )
	{
	  if( warm_ )
	    {
	      throw Exceptions::TackingOnCold( );
	    }
	  throw Exceptions::WarmAccess( );
	}

      Kernel::ContRef cont = evalState->cont_;
      cont->takeHandle( res,
			evalState );
    }
  else
    {
      /* This is the default destination
       */

      if( ! warm_ )
	{
	  throw Exceptions::WarmAccess( );
	}

      Kernel::ContRef cont = evalState->cont_;
      cont->takeHandle( evalState->dyn_->getDefaultDestination( ),
			evalState );
    }
}


Kernel::DynamicBindingContinuation::DynamicBindingContinuation( const Ast::SourceLocation & traceLoc, const Kernel::PassedEnv & env, const Kernel::Environment::LexicalKey & key, const Ast::SourceLocation & idLoc, const Kernel::ContRef & cont )
  : Kernel::Continuation( traceLoc ), env_( env ), key_( key ), idLoc_( idLoc ), cont_( cont )
{ }

Kernel::DynamicBindingContinuation::~DynamicBindingContinuation( )
{ }

void
Kernel::DynamicBindingContinuation::takeHandle( Kernel::HandleType val, Kernel::EvalState * evalState, bool dummy ) const
{
  if( val->isThunk( ) )
    {
      val->force( val, evalState );
      return;
    }
  evalState->cont_ = cont_;
  env_->lookupDynamicVariable( key_ ).makeBinding( val, idLoc_, evalState );
}

void
Kernel::DynamicBindingContinuation::backTrace( std::list< Kernel::Continuation::BackTraceElem > * trace ) const
{
  trace->push_front( Kernel::Continuation::BackTraceElem( this, "dynamic binding" ) );
  cont_->backTrace( trace );
}

void
Kernel::DynamicBindingContinuation::gcMark( Kernel::GCMarkedSet & marked )
{
  env_->gcMark( marked );
  cont_->gcMark( marked );
}

Ast::DynamicBindingExpression::DynamicBindingExpression( const Ast::SourceLocation & idLoc, const char * id, Ast::Expression * expr, Kernel::Environment::LexicalKey ** idKey )
  : Ast::Expression( Ast::SourceLocation( idLoc, expr->loc( ) ) ), idLoc_( idLoc ), id_( id ), expr_( expr ), idKey_( new Kernel::Environment::LexicalKey * ( 0 ) )
{ }

Ast::DynamicBindingExpression::~DynamicBindingExpression( )
{
  delete id_;
  delete expr_;
  if( *idKey_ != 0 )
    {
      delete *idKey_;
      *idKey_ = 0;
    }
  // Don't delete idKey as it's shared!
}

void
Ast::DynamicBindingExpression::eval( Kernel::EvalState * evalState ) const
{
  if( *idKey_ == 0 )
    {
      *idKey_ = new Kernel::Environment::LexicalKey( evalState->env_->findLexicalDynamicKey( idLoc_, id_ ) );
    }

  const Kernel::DynamicVariableProperties & dynProps = evalState->env_->lookupDynamicVariable( **idKey_ );
  
  if( dynProps.forceValue( ) || expr_->immediate_ )
    {
      evalState->expr_ = expr_;
      evalState->cont_ = Kernel::ContRef( new Kernel::DynamicBindingContinuation( expr_->loc( ), evalState->env_, **idKey_, idLoc_, evalState->cont_ ) );
    }
  else
    {
      dynProps.makeBinding( Kernel::HandleType( new Kernel::Variable( new Kernel::Thunk( evalState->env_, evalState->dyn_, expr_ ) ) ),
			    idLoc_,
			    evalState );
    }
}

Ast::DynamicBindWarmLexiographicExpr::DynamicBindWarmLexiographicExpr( const Ast::SourceLocation & loc, const Ast::SourceLocation & dstLoc, const char * dstId, const Ast::SourceLocation & srcLoc, const char * srcId )
  : Ast::Expression( loc ), dstLoc_( dstLoc ), dstId_( dstId ), dstIdKey_( new Kernel::Environment::LexicalKey * ( 0 ) ), srcLoc_( srcLoc ), srcId_( srcId ), srcIdKey_( new Kernel::Environment::LexicalKey * ( 0 ) )
{ }

Ast::DynamicBindWarmLexiographicExpr::~DynamicBindWarmLexiographicExpr( )
{
  delete srcId_;
  delete srcId_;
  if( *dstIdKey_ != 0 )
    {
      delete *dstIdKey_;
    }
  delete dstIdKey_;     //  This can be done only as long as this is not shared!
  if( *srcIdKey_ != 0 )
    {
      delete *srcIdKey_;
    }
  delete srcIdKey_;     //  This can be done only as long as this is not shared!

}

void
Ast::DynamicBindWarmLexiographicExpr::eval( Kernel::EvalState * evalState ) const
{
  if( *dstIdKey_ == 0 )
    {
      *dstIdKey_ = new Kernel::Environment::LexicalKey( evalState->env_->findLexicalDynamicKey( dstLoc_, dstId_ ) );
      *srcIdKey_ = new Kernel::Environment::LexicalKey( evalState->env_->findLexicalKey( srcLoc_, srcId_ ) );
    }
  
  const Kernel::DynamicVariableProperties & dstDynProps = evalState->env_->lookupDynamicVariable( **dstIdKey_ );
  
  dstDynProps.makeBinding( evalState->env_->getVarHandle( **srcIdKey_ ), dstLoc_, evalState );
}


Ast::DynamicBindWarmDynamicExpr::DynamicBindWarmDynamicExpr( const Ast::SourceLocation & loc, const Ast::SourceLocation & dstLoc, const char * dstId, const Ast::SourceLocation & srcLoc, const char * srcId )
  : Ast::Expression( loc ), dstLoc_( dstLoc ), dstId_( dstId ), dstIdKey_( new Kernel::Environment::LexicalKey * ( 0 ) ), srcLoc_( srcLoc ), srcId_( srcId ), srcIdKey_( new Kernel::Environment::LexicalKey * ( 0 ) )
{ }

Ast::DynamicBindWarmDynamicExpr::~DynamicBindWarmDynamicExpr( )
{
  delete srcId_;
  delete srcId_;
  if( *dstIdKey_ != 0 )
    {
      delete *dstIdKey_;
    }
  delete dstIdKey_;     //  This can be done only as long as this is not shared!
  if( *srcIdKey_ != 0 )
    {
      delete *srcIdKey_;
    }
  delete srcIdKey_;     //  This can be done only as long as this is not shared!

}

void
Ast::DynamicBindWarmDynamicExpr::eval( Kernel::EvalState * evalState ) const
{
  if( *dstIdKey_ == 0 )
    {
      *dstIdKey_ = new Kernel::Environment::LexicalKey( evalState->env_->findLexicalDynamicKey( dstLoc_, dstId_ ) );
      *srcIdKey_ = new Kernel::Environment::LexicalKey( evalState->env_->findLexicalDynamicKey( srcLoc_, srcId_ ) );
    }

  const Kernel::DynamicVariableProperties & dstDynProps = evalState->env_->lookupDynamicVariable( **dstIdKey_ );
  const Kernel::DynamicVariableProperties & srcDynProps = evalState->env_->lookupDynamicVariable( **srcIdKey_ );

  dstDynProps.makeBinding( srcDynProps.fetch( evalState->dyn_ ), dstLoc_, evalState );
}


Kernel::WithDynamicContinuation::WithDynamicContinuation( const Ast::SourceLocation & traceLoc, Ast::Expression * expr, const Kernel::EvalState & evalState )
  : Kernel::Continuation( traceLoc ), expr_( expr ), env_( evalState.env_ ), dyn_( evalState.dyn_ ), cont_( evalState.cont_ )
{ }

Kernel::WithDynamicContinuation::~WithDynamicContinuation( )
{ }

void
Kernel::WithDynamicContinuation::takeValue( const RefCountPtr< const Lang::Value > & val, Kernel::EvalState * evalState, bool dummy ) const
{
  evalState->dyn_ = Kernel::PassedDyn( new Kernel::DynamicEnvironment( dyn_, *Helpers::down_cast< const Lang::DynamicBindings >( val, traceLoc_ ) ) );
  evalState->env_ = env_;
  evalState->expr_ = expr_;
  evalState->cont_ = cont_;
}

void
Kernel::WithDynamicContinuation::backTrace( std::list< Kernel::Continuation::BackTraceElem > * trace ) const
{
  trace->push_front( Kernel::Continuation::BackTraceElem( this, "with dynamic bindings" ) );
  cont_->backTrace( trace );
}

void
Kernel::WithDynamicContinuation::gcMark( Kernel::GCMarkedSet & marked )
{
  env_->gcMark( marked );
  dyn_->gcMark( marked );
  cont_->gcMark( marked );
}


Ast::WithDynamicExpr::WithDynamicExpr( const Ast::SourceLocation & loc, Ast::Expression * bindings, Ast::Expression * expr )
  : Ast::Expression( loc ), bindings_( bindings ), expr_( expr )
{ }

Ast::WithDynamicExpr::~WithDynamicExpr( )
{ }

void
Ast::WithDynamicExpr::eval( Kernel::EvalState * evalState ) const
{
  evalState->expr_ = bindings_;
  evalState->cont_ = Kernel::ContRef( new Kernel::WithDynamicContinuation( bindings_->loc( ), expr_, *evalState ) );
}


Ast::DynamicVariableDecl::DynamicVariableDecl( const Ast::SourceLocation & loc, const Ast::SourceLocation & idLoc, const char * id, Ast::Expression * filterExpr, Ast::Expression * defaultExpr, size_t ** idPos )
  : Ast::BindNode( loc, idLoc, id ), filterExpr_( filterExpr ), defaultExpr_( defaultExpr ), idPos_( idPos )
{ }

Ast::DynamicVariableDecl::~DynamicVariableDecl( )
{
  delete filterExpr_;
  delete defaultExpr_;
}

void
Ast::DynamicVariableDecl::eval( Kernel::EvalState * evalState ) const
{
  if( *idPos_ == 0 )
    {
      *idPos_ = new size_t( evalState->env_->findLocalDynamicPosition( idLoc_, id_ ) );
    }

  evalState->expr_ = filterExpr_;
  evalState->cont_ = Kernel::ContRef( new Kernel::DynamicVariableDeclContinuation( filterExpr_->loc( ),
										   this,
										   *evalState ) );
}

void
Ast::DynamicVariableDecl::callBack( const RefCountPtr< const Lang::Function > & filter, Kernel::EvalState * evalState ) const
{
  evalState->env_->defineDynamic( id_,
				  **idPos_,
				  filter,
				  Kernel::HandleType( new Kernel::Variable( new Kernel::Thunk( evalState->env_,
											       evalState->dyn_,
											       defaultExpr_ ) ) ) );
  
  Kernel::ContRef cont = evalState->cont_;
  cont->takeHandle( Kernel::THE_SLOT_VARIABLE,
		    evalState );
}


Kernel::DynamicVariableDeclContinuation::DynamicVariableDeclContinuation( const Ast::SourceLocation & traceLoc, const Ast::DynamicVariableDecl * declExpr, Kernel::EvalState & evalState )
  : Kernel::Continuation( traceLoc ), declExpr_( declExpr ), env_( evalState.env_ ), dyn_( evalState.dyn_ ), cont_( evalState.cont_ )
{ }

Kernel::DynamicVariableDeclContinuation::~DynamicVariableDeclContinuation( )
{ }

void
  Kernel::DynamicVariableDeclContinuation::takeValue( const RefCountPtr< const Lang::Value > & val, Kernel::EvalState * evalState, bool dummy ) const
{
  evalState->env_ = env_;
  evalState->dyn_ = dyn_;
  evalState->cont_ = cont_;
  declExpr_->callBack( Helpers::down_cast< const Lang::Function >( val, traceLoc_ ),
		       evalState );
}

void
Kernel::DynamicVariableDeclContinuation::backTrace( std::list< Kernel::Continuation::BackTraceElem > * trace ) const
{
  trace->push_front( Kernel::Continuation::BackTraceElem( this, "dynamic variable declaration" ) );
  cont_->backTrace( trace );
}

void
Kernel::DynamicVariableDeclContinuation::gcMark( Kernel::GCMarkedSet & marked )
{
  env_->gcMark( marked );
  dyn_->gcMark( marked );
  cont_->gcMark( marked );
}


Ast::EvalSymbolFunction::EvalSymbolFunction( const Ast::SourceLocation & loc, Ast::Expression * expr )
  : Lang::Function( new Kernel::EvaluatedFormals( "< symbol evaluation >", true ) ), loc_( loc ), expr_( expr )
{ }

Ast::EvalSymbolFunction::~EvalSymbolFunction( )
{ 
  delete expr_;
}

void
Ast::EvalSymbolFunction::push_exprs( Ast::ArgListExprs * args ) const
{
  args->orderedExprs_->push_back( expr_ );
}

void
Ast::EvalSymbolFunction::call( Kernel::EvalState * evalState, Kernel::Arguments & args, const Ast::SourceLocation & callLoc ) const
{
  RefCountPtr< const Lang::Value > untypedVal = args.getValue( 0 );
  typedef const Lang::Symbol ArgType;
  ArgType * val = dynamic_cast< ArgType * >( untypedVal.getPtr( ) );
  if( val == 0 )
    {
      throw Exceptions::TypeMismatch( expr_->loc( ), untypedVal->getTypeName( ), ArgType::staticTypeName( ) );
    }
  if( val->isUnique( ) )
    {
      throw Exceptions::OutOfRange( expr_->loc( ), strrefdup( "Unique symbols can't denote variables." ) );
    }


  Kernel::Environment::LexicalKey key = evalState->env_->findLexicalKey( loc_, val->name( ).getPtr( ) );

  Kernel::PassedEnv env = evalState->env_;
  env->lookup( key, false, evalState );  // false means not warm
}


Ast::IntroduceCold::IntroduceCold( const Ast::SourceLocation & idLoc, const char * id, Ast::Expression * expr, size_t ** idPos )
  : Ast::BindNode( Ast::SourceLocation( idLoc, expr->loc( ) ), idLoc, id ), expr_( expr ), idPos_( idPos )
{ }

Ast::IntroduceCold::~IntroduceCold( )
{
  delete expr_;

  /* idPos_ is shared and will be a memory leak which must not be deleted.
   * It would be easy to fix the leak using RefCountPtr< size_t >, but the leakage is constant space, so silly efficiency is prioritized.
   */
}

void
Ast::IntroduceCold::eval( Kernel::EvalState * evalState ) const
{
  if( *idPos_ == 0 )
    {
      *idPos_ = new size_t( evalState->env_->findLocalPosition( idLoc_, id_ ) );
    }
  
  if( expr_->immediate_ )
    {
      evalState->cont_ = Kernel::ContRef( new Kernel::IntroduceColdContinuation( evalState->env_,
										  *idPos_,
										  evalState->cont_,
										  expr_->loc( ) ) );
      evalState->expr_ = expr_;
    }
  else
    {
      evalState->env_->define( **idPos_,
			       Kernel::HandleType( new Kernel::Variable( new Kernel::Thunk( evalState->env_,
											    evalState->dyn_,
											    expr_ ) ) ) );
      Kernel::ContRef cont = evalState->cont_;
      cont->takeHandle( Kernel::THE_SLOT_VARIABLE, evalState );
    }
}

Ast::LexiographicState::LexiographicState( const Ast::SourceLocation & loc, const char * id, Kernel::Environment::LexicalKey ** idKey, bool warm )
  : Ast::Expression( loc ), id_( id ), idKey_( idKey ), warm_( warm )
{
  immediate_ = true;
}

Ast::LexiographicState::~LexiographicState( )
{
  delete id_;
  if( *idKey_ != 0 )
    {
      delete *idKey_;
    }
  delete idKey_;     //  This can be done only as long as this is not shared!
}

void
Ast::LexiographicState::eval( Kernel::EvalState * evalState ) const
{
  throw Exceptions::InternalError( "A lexiographic state was evaluated." );
}

Kernel::StateHandle
Ast::LexiographicState::getHandle( Kernel::PassedEnv env ) const
{
  if( *idKey_ == 0 )
    {
      *idKey_ = new Kernel::Environment::LexicalKey( evalState->env_->findLexicalKey( loc_, id_ ) );
    }

  evalState->env_->lookup( **idKey_, warm_, evalState );
}


Ast::IntroduceWarm::IntroduceWarm( const Ast::SourceLocation & idLoc, const char * id, Ast::Expression * expr, size_t ** idPos )
  : Ast::BindNode( Ast::SourceLocation( idLoc, expr->loc( ) ), idLoc, id ), expr_( expr ), idPos_( idPos )
{ }

Ast::IntroduceWarm::~IntroduceWarm( )
{
  delete expr_;

  /* idPos_ shared and will be a memory leak which must not be deleted.
   * It would be easy to fix the leak using RefCountPtr< size_t >, but the leakage is constant space, so silly efficiency is prioritized.
   */
}

void
Ast::IntroduceWarm::eval( Kernel::EvalState * evalState ) const
{
  if( *idPos_ == 0 )
    {
      *idPos_ = new size_t( evalState->env_->findLocalPosition( idLoc_, id_ ) );
    }
  
  evalState->cont_ = Kernel::ContRef( new Kernel::IntroduceWarmContinuation( evalState->env_,
									     *idPos_,
									     evalState->cont_,
									     expr_->loc( ) ) );
  evalState->expr_ = expr_;
}


Ast::LexiographicInsertion::LexiographicInsertion( const Ast::SourceLocation & idLoc, const char * id, Ast::Expression * expr, Kernel::Environment::LexicalKey ** idKey )
  : Ast::SequencingNode( Ast::SourceLocation( idLoc, expr->loc( ) ), idLoc, id ), expr_( expr ), idKey_( idKey )
{ }

Ast::LexiographicInsertion::~LexiographicInsertion( )
{ }

void
Ast::LexiographicInsertion::eval( Kernel::EvalState * evalState ) const
{
  if( *idKey_ == 0 )
    {
      *idKey_ = new Kernel::Environment::LexicalKey( evalState->env_->findLexicalKey( idLoc_, id_ ) );
    }

  evalState->cont_ = Kernel::ContRef( new Kernel::InsertionContinuation( evalState->env_->getVarHandle( **idKey_ ),
									 evalState->cont_,
									 evalState->dyn_,
									 expr_->loc( ) ) );
  evalState->expr_ = expr_;
}


Ast::DynamicInsertion::DynamicInsertion( const Ast::SourceLocation & idLoc, const char * id, Ast::Expression * expr, Kernel::Environment::LexicalKey ** idKey )
  : Ast::SequencingNode( Ast::SourceLocation( idLoc, expr->loc( ) ), idLoc, id ), expr_( expr ), idKey_( idKey )
{ }

Ast::DynamicInsertion::~DynamicInsertion( )
{ }

void
Ast::DynamicInsertion::eval( Kernel::EvalState * evalState ) const
{
  if( id_ != 0 )
    {
      if( *idKey_ == 0 )
	{
	  *idKey_ = new Kernel::Environment::LexicalKey( evalState->env_->findLexicalDynamicKey( idLoc_, id_ ) );
	}
      
      const Kernel::DynamicVariableProperties & dynProps = evalState->env_->lookupDynamicVariable( **idKey_ );
      
      evalState->cont_ = Kernel::ContRef( new Kernel::InsertionContinuation( dynProps.fetch( evalState->dyn_ ),
									      evalState->cont_,
									      evalState->dyn_,
									      expr_->loc( ) ) );
    }
  else
    {
      evalState->cont_ = Kernel::ContRef( new Kernel::InsertionContinuation( evalState->dyn_->getDefaultDestination( ),
									      evalState->cont_,
									      evalState->dyn_,
									      expr_->loc( ) ) );
    }
  evalState->expr_ = expr_;
}


Ast::Freeze::Freeze( const Ast::SourceLocation & idLoc, const char * id, size_t ** idPos )
  : Ast::SequencingNode( idLoc, idLoc, id ), idPos_( idPos )
{
  immediate_ = true;
}

Ast::Freeze::~Freeze( )
{
  /* idPos shared and will be a memory leak which must not be deleted.
   * It would be easy to fix the leak using RefCountPtr< size_t >, but the leakage is constant space, so silly efficiency is prioritized.
   */
}

void
Ast::Freeze::eval( Kernel::EvalState * evalState ) const
{
  if( *idPos_ == 0 )
    {
      *idPos_ = new size_t( evalState->env_->findLocalPosition( idLoc_, id_ ) );
    }

  Kernel::ContRef cont = evalState->cont_;
  cont->takeValue( evalState->env_->freeze( **idPos_, evalState, idLoc_ ),
		   evalState );
}
