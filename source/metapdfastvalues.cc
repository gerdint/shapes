#include "metapdfastvalues.h"
#include "metapdfexceptions.h"
#include "metapdfcore.h"
#include "metapdfastfun.h"
#include "consts.h"
#include "globals.h"
#include "continuations.h"

using namespace MetaPDF;


Ast::Constant::Constant( const Ast::SourceLocation & loc, const Kernel::VariableHandle & val )
  : Ast::Expression( loc ), val_( val )
{ }

Ast::Constant::Constant( const Ast::SourceLocation & loc, RefCountPtr< const Lang::Value > val )
  : Ast::Expression( loc ), val_( Kernel::VariableHandle( new Kernel::Variable( val ) ) )
{ }

Ast::Constant::Constant( const Ast::SourceLocation & loc, const Lang::Value * val )
  : Ast::Expression( loc ), val_( Helpers::newValHandle( val ) )
{ }

Ast::Constant::~Constant( )
{ }

void
Ast::Constant::analyze( Ast::Node * parent, const Ast::AnalysisEnvironment * env )
{
  parent_ = parent;

  imperative_ = false;
}

void
Ast::Constant::eval( Kernel::EvalState * evalState ) const
{
  Kernel::ContRef cont = evalState->cont_;
  cont->takeHandle( val_,
		    evalState );
}


Ast::PolarHandle2DExpr::PolarHandle2DExpr( const Ast::SourceLocation & loc, Ast::Expression * rExpr, Ast::Expression * aExpr )
  : Ast::Expression( loc ), rExpr_( rExpr ), aExpr_( aExpr )
{ }

Ast::PolarHandle2DExpr::~PolarHandle2DExpr( )
{
  delete rExpr_;
  delete aExpr_;
}

void
Ast::PolarHandle2DExpr::analyze( Ast::Node * parent, const Ast::AnalysisEnvironment * env )
{
  parent_ = parent;

  rExpr_->analyze( this, env );
  aExpr_->analyze( this, env );

  imperative_ = rExpr_->imperative_ || aExpr_->imperative_;
}

void
Ast::PolarHandle2DExpr::eval( Kernel::EvalState * evalState ) const
{
  Kernel::ContRef cont = evalState->cont_;
  evalState->cont_ = Kernel::ContRef
    ( new Kernel::PolarHandle2DCont( aExpr_->loc( ),
				     RefCountPtr< Kernel::PolarHandlePromise >( new Kernel::PolarHandleTruePromise( new Kernel::Thunk( evalState->env_, evalState->dyn_, rExpr_ ) ) ),
				     cont ) );
  evalState->expr_ = aExpr_;
}

Kernel::PolarHandle2DCont::PolarHandle2DCont( const Ast::SourceLocation & traceLoc, const RefCountPtr< Kernel::PolarHandlePromise > & rPromise, const Kernel::ContRef & cont )
  : Kernel::Continuation( traceLoc ), rPromise_( rPromise ), cont_( cont )
{ }

Kernel::PolarHandle2DCont::~PolarHandle2DCont( )
{ }

void
Kernel::PolarHandle2DCont::takeValue( const RefCountPtr< const Lang::Value > & val, Kernel::EvalState * evalState, bool dummy ) const
{
  cont_->takeValue( Kernel::ValueRef( new Lang::PolarHandle2D( rPromise_,
							       Helpers::down_cast_ContinuationArgument< const Lang::Float >( val, this )->val_ ) ),
		    evalState );
}

void
Kernel::PolarHandle2DCont::backTrace( std::list< Kernel::Continuation::BackTraceElem > * trace ) const
{
  trace->push_front( Kernel::Continuation::BackTraceElem( this, "( <> ^ <> ) angle" ) );
  cont_->backTrace( trace );
}

void
Kernel::PolarHandle2DCont::gcMark( Kernel::GCMarkedSet & marked )
{
  cont_->gcMark( marked );
}


Ast::PolarHandle2DExprFree_a::PolarHandle2DExprFree_a( const Ast::SourceLocation & loc, Ast::Expression * rExpr )
  : Ast::Expression( loc ), rExpr_( rExpr )
{ }

Ast::PolarHandle2DExprFree_a::~PolarHandle2DExprFree_a( )
{
  delete rExpr_;
}

void
Ast::PolarHandle2DExprFree_a::analyze( Ast::Node * parent, const Ast::AnalysisEnvironment * env )
{
  parent_ = parent;

  rExpr_->analyze( this, env );

  imperative_ = rExpr_->imperative_;
}

void
Ast::PolarHandle2DExprFree_a::eval( Kernel::EvalState * evalState ) const
{
  Kernel::ContRef cont = evalState->cont_;
  cont->takeValue( Kernel::ValueRef( new Lang::PolarHandle2DFree_a( RefCountPtr< Kernel::PolarHandlePromise >( new Kernel::PolarHandleTruePromise( new Kernel::Thunk( evalState->env_, evalState->dyn_, rExpr_ ) ) ) ) ),
		   evalState );
}


Ast::EmptyExpression::EmptyExpression( const Ast::SourceLocation & loc )
  : Ast::Expression( loc )
{
  immediate_ = true;
}

Ast::EmptyExpression::~EmptyExpression( )
{ }

void
Ast::EmptyExpression::analyze( Ast::Node * parent, const Ast::AnalysisEnvironment * env )
{
  parent_ = parent;

  imperative_ = false;
}

void
Ast::EmptyExpression::eval( Kernel::EvalState * evalState ) const
{
  Kernel::ContRef cont = evalState->cont_;
  cont->takeHandle( Kernel::THE_SLOT_VARIABLE,
		    evalState );
}
