#include "metapdfastflow.h"
#include "globals.h"
#include "metapdfexceptions.h"
#include "metapdfastfun.h"

using namespace MetaPDF;
using namespace std;


// MetaPDF::IfExpr::IfExpr( const Ast::SourceLocation & _loc, Ast::Expression * _predicate, Ast::Expression * _consequence, Ast::Expression * _alternative )
//   : Ast::Expression( _loc ), predicate( _predicate ), consequence( _consequence ), alternative( _alternative )
// {
//   predicate->setParent( this );
//   consequence->setParent( this );
//   alternative->setParent( this );
// }

// MetaPDF::IfExpr::IfExpr( const Ast::SourceLocation & _loc, Ast::Expression * _predicate, Ast::Expression * _consequence )
//   : loc( _loc ), predicate( _predicate ), consequence( _consequence ), alternative( 0 )
// {
//   predicate->setParent( this );
//   consequence->setParent( this );
// }

// MetaPDF::IfExpr::~IfExpr( )
// {
//   delete predicate;
//   delete consequence;
//   if( alternative != 0 )
//     {
//       delete alternative;
//     }
// }

// RefCountPtr< const Lang::Value >
// MetaPDF::IfExpr::value( Kernel::Environment::HandleType dstgroup, SimplePDF::PDF_out * pdfo, Kernel::GraphicsState * metaState, Kernel::PassedEnv env ) const
// {
//   RefCountPtr< const Lang::Value > predUntyped( predicate->value( dstgroup, pdfo, metaState, env ) );
//   typedef const Lang::Boolean PredType;
//   PredType * predVal = dynamic_cast< PredType * >( predUntyped.getPtr( ) );
//   if( predVal == 0 )
//     {
//       throw Exceptions::TypeMismatch( predicate, predVal->getTypeName( ), PredType::staticTypeName( ) );
//     }
//   if( predVal->val )
//     {
//       return consequence->value( dstgroup, pdfo, metaState, env );
//     }
//   else if( alternative != 0 )
//     {
//       return alternative->value( dstgroup, pdfo, metaState, env );
//     }
//   else
//     {
//       return MetaPDF::THE_VOID;
//     }
// }

Ast::LetDynamicECExpr::LetDynamicECExpr( const Ast::SourceLocation & loc, const Ast::SourceLocation & idLoc, const char * id, Ast::Expression * expr )
  : Ast::Expression( loc ), idLoc_( idLoc ), id_( id ), expr_( expr )
{ }

Ast::LetDynamicECExpr::~LetDynamicECExpr( )
{
  delete id_;
  delete expr_;
}

void
Ast::LetDynamicECExpr::eval( Kernel::EvalState * evalState ) const
{
  evalState->dyn_ = Kernel::PassedDyn( new Kernel::DynamicEnvironment( evalState->dyn_, id_, evalState->cont_ ) );
  evalState->expr_ = expr_;
}

Ast::ContinueDynamicECFunction::ContinueDynamicECFunction( const Ast::SourceLocation & idLoc, const char * id, Ast::Expression * expr )
  : Lang::Function( new Kernel::EvaluatedFormals( "< dynamic escape continuation >", true ) ), idLoc_( idLoc ), id_( id ), expr_( expr )
{ }

Ast::ContinueDynamicECFunction::~ContinueDynamicECFunction( )
{
  delete expr_;
  delete id_;
}

void
Ast::ContinueDynamicECFunction::push_exprs( Ast::ArgListExprs * args ) const
{
  args->orderedExprs_->push_back( expr_ );
}

void
Ast::ContinueDynamicECFunction::call( Kernel::EvalState * evalState, Kernel::Arguments & args, const Ast::SourceLocation & callLoc ) const
{
  Kernel::ContRef cont = evalState->dyn_->getEscapeContinuation( id_, idLoc_ );
  evalState->cont_ = cont;
  cont->takeHandle( args.getHandle( 0 ),
		    evalState );
}
