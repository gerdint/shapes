#include <cmath>

#include "metapdfastexprs.h"
#include "metapdfexceptions.h"
#include "lighttypes.h"

using namespace MetaPDF;
using namespace std;


Ast::UnaryExpr::UnaryExpr( const Ast::SourceLocation & _loc )
  : Ast::Expression( _loc )
{ }
Ast::UnaryExpr::~UnaryExpr( )
{ }

DEFAULTUNARYOPIMPL( UnaryExpr );


Kernel::UnaryCont_1::UnaryCont_1( const Ast::UnaryExpr * op, const Kernel::PassedDyn & dyn, const Kernel::ContRef & cont, const Ast::SourceLocation & traceLoc )
  : Kernel::Continuation( traceLoc ), op_( op ), dyn_( dyn ), cont_( cont )
{ }

Kernel::UnaryCont_1::~UnaryCont_1( )
{ }

void
Kernel::UnaryCont_1::takeValue( const RefCountPtr< const Lang::Value > & val, Kernel::EvalState * evalState, bool dummy ) const
{
  cont_->takeValue( val->unaryDispatch( val, dyn_, op_ ), evalState );
}

void
Kernel::UnaryCont_1::backTrace( std::list< Kernel::Continuation::BackTraceElem > * trace ) const
{
  trace->push_front( Kernel::Continuation::BackTraceElem( this, "unary's only" ) );
  cont_->backTrace( trace );
}

void
Kernel::UnaryCont_1::gcMark( Kernel::GCMarkedSet & marked )
{
  cont_->gcMark( marked );
}


Ast::UnaryPrefixExpr::UnaryPrefixExpr( const Ast::SourceLocation & opLoc, Ast::Expression * expr )
  : Ast::UnaryExpr( Ast::SourceLocation( opLoc, expr->loc( ) ) ), opLoc_( opLoc ), expr_( expr )
{ }

Ast::UnaryPrefixExpr::~UnaryPrefixExpr( )
{
  delete expr_;
}

void
Ast::UnaryPrefixExpr::analyze( Ast::Node * parent )
{
  parent_ = parent;

  expr_->analyze( this );

  imperative_ = expr_->imperative_;
}

void
Ast::UnaryPrefixExpr::eval( Kernel::EvalState * evalState ) const
{
  evalState->expr_ = expr_;
  evalState->cont_ = Kernel::ContRef( new Kernel::UnaryCont_1( this, evalState->dyn_, evalState->cont_, expr_->loc( ) ) );
}

RefCountPtr< const Lang::Value >
Ast::UnaryPrefixExpr::throwNotApplicable( const Lang::Value * arg ) const
{
  throw Exceptions::UnaryPrefixNotApplicable( opLoc_, expr_, arg->getTypeName( ) );
}




Ast::UnaryPostfixExpr::UnaryPostfixExpr( const Ast::SourceLocation & opLoc, Ast::Expression * expr )
  : Ast::UnaryExpr( Ast::SourceLocation( expr->loc( ), opLoc ) ), opLoc_( opLoc ), expr_( expr )
{ }

Ast::UnaryPostfixExpr::~UnaryPostfixExpr( )
{
  delete expr_;
}

void
Ast::UnaryPostfixExpr::analyze( Ast::Node * parent )
{
  parent_ = parent;

  expr_->analyze( this );

  imperative_ = expr_->imperative_;
}

void
Ast::UnaryPostfixExpr::eval( Kernel::EvalState * evalState ) const
{
  evalState->expr_ = expr_;
  evalState->cont_ = Kernel::ContRef( new Kernel::UnaryCont_1( this, evalState->dyn_, evalState->cont_, expr_->loc( ) ) );
}

RefCountPtr< const Lang::Value >
Ast::UnaryPostfixExpr::throwNotApplicable( const Lang::Value * arg ) const
{
  throw Exceptions::UnaryPostfixNotApplicable( opLoc_, expr_, arg->getTypeName( ) );
}


Kernel::BinaryInfixCont_2::BinaryInfixCont_2( const Ast::BinaryInfixExpr * op, const Kernel::ValueRef & val1, const Kernel::PassedDyn & dyn, const Kernel::ContRef & cont, const Ast::SourceLocation & traceLoc )
  : Kernel::Continuation( traceLoc ), op_( op ), val1_( val1 ), dyn_( dyn ), cont_( cont )
{ }

Kernel::BinaryInfixCont_2::~BinaryInfixCont_2( )
{ }

void
Kernel::BinaryInfixCont_2::takeValue( const RefCountPtr< const Lang::Value > & val, Kernel::EvalState * evalState, bool dummy ) const
{
  cont_->takeValue( val1_->binaryDispatch1( val1_, val, dyn_, op_ ), evalState );
}

void
Kernel::BinaryInfixCont_2::backTrace( std::list< Kernel::Continuation::BackTraceElem > * trace ) const
{
  trace->push_front( Kernel::Continuation::BackTraceElem( this, "binary infix's second" ) );
  cont_->backTrace( trace );
}

void
Kernel::BinaryInfixCont_2::gcMark( Kernel::GCMarkedSet & marked )
{
  const_cast< Lang::Value * >( val1_.getPtr( ) )->gcMark( marked );
  cont_->gcMark( marked );
}


Kernel::BinaryInfixCont_1::BinaryInfixCont_1( const Ast::BinaryInfixExpr * op, Kernel::EvalState & evalState, const Ast::SourceLocation & traceLoc )
  : Kernel::Continuation( traceLoc ), op_( op ), env_( evalState.env_ ), dyn_( evalState.dyn_ ), cont_( evalState.cont_ )
{ }

Kernel::BinaryInfixCont_1::~BinaryInfixCont_1( )
{ }

void
Kernel::BinaryInfixCont_1::takeValue( const RefCountPtr< const Lang::Value > & val, Kernel::EvalState * evalState, bool dummy ) const
{
  evalState->expr_ = const_cast< Ast::Expression * >( op_->get_expr2( ) );
  evalState->env_ = env_;
  evalState->dyn_ = dyn_;
  evalState->cont_ = Kernel::ContRef( new Kernel::BinaryInfixCont_2( op_, val, dyn_, cont_, evalState->expr_->loc( ) ) );
}

void
Kernel::BinaryInfixCont_1::backTrace( std::list< Kernel::Continuation::BackTraceElem > * trace ) const
{
  trace->push_front( Kernel::Continuation::BackTraceElem( this, "binary infix's first" ) );
  cont_->backTrace( trace );
}

void
Kernel::BinaryInfixCont_1::gcMark( Kernel::GCMarkedSet & marked )
{
  env_->gcMark( marked );
  cont_->gcMark( marked );
}


Ast::BinaryInfixExpr::BinaryInfixExpr( const Ast::SourceLocation & opLoc, Ast::Expression * expr1, Ast::Expression * expr2 )
  : Ast::Expression( Ast::SourceLocation( expr1->loc( ), expr2->loc( ) ) ), opLoc_( opLoc ), expr1_( expr1 ), expr2_( expr2 )
{ }

Ast::BinaryInfixExpr::~BinaryInfixExpr( )
{
  delete expr1_;
  delete expr2_;
}

void
Ast::BinaryInfixExpr::analyze( Ast::Node * parent )
{
  parent_ = parent;
  
  expr1_->analyze( this );
  expr2_->analyze( this );

  imperative_ = expr1_->imperative_ || expr2_->imperative_;
}

void
Ast::BinaryInfixExpr::eval( Kernel::EvalState * evalState ) const
{
  evalState->expr_ = expr1_;
  evalState->cont_ = Kernel::ContRef( new Kernel::BinaryInfixCont_1( this, *evalState, expr1_->loc( ) ) );
}

RefCountPtr< const Lang::Value >
Ast::BinaryInfixExpr::throwNotApplicable( const Lang::Value * arg1, const Lang::Value * arg2 ) const
{
  throw Exceptions::BinaryInfixNotApplicable( opLoc_, expr1_, arg1->getTypeName( ), expr2_, arg2->getTypeName( ) );
}

DEFAULTBINARYOPIMPL;
