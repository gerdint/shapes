#ifndef astflow_h
#define astflow_h

#include "ast.h"

namespace Shapes
{

  namespace Ast
  {
/*   class IfExpr : public Expression */
/*   { */
/*     Ast::SourceLocation loc_; */
/*     Expression * predicate_; */
/*     Expression * consequence_; */
/*     Expression * alternative_; */
/*   public: */
/*     IfExpr( const Ast::SourceLocation & loc, Expression * predicate, Expression * consequence, Expression * alternative ); */
/*     IfExpr( const Ast::SourceLocation & loc, Expression * predicate, Expression * consequence ); */
/*     virtual ~IfExpr( ); */
/*     virtual void eval( Kernel::EvalState * evalState ) const; */
/*     virtual const Ast::SourceLocation & firstLoc( ) const; */
/*     virtual const Ast::SourceLocation & lastLoc( ) const; */
/*   }; */

  class LetDynamicECExpr : public Expression
  {
    Ast::SourceLocation loc_;
    Ast::SourceLocation idLoc_;
    const char * id_;
    Expression * expr_;
  public:
    LetDynamicECExpr( const Ast::SourceLocation & loc, const Ast::SourceLocation & idLoc, const char * id, Expression * expr );
    virtual ~LetDynamicECExpr( );
    virtual void analyze( Ast::Node * parent, const Ast::AnalysisEnvironment * env );
    virtual void eval( Kernel::EvalState * evalState ) const;
  };

  /* This isn't a function in the sense that it returns.
   */
  class ContinueDynamicECFunction : public Lang::Function
  {
    Ast::SourceLocation idLoc_;
    const char * id_;
    Expression * expr_;
  public:
    ContinueDynamicECFunction( const Ast::SourceLocation & idLoc, const char * id, Expression * expr );
    virtual ~ContinueDynamicECFunction( );
    void push_exprs( Ast::ArgListExprs * args ) const;
    virtual void analyze( Ast::Node * parent, const Ast::AnalysisEnvironment * env );
    virtual void call( Kernel::EvalState * evalState, Kernel::Arguments & args, const Ast::SourceLocation & callLoc ) const;
    virtual void gcMark( Kernel::GCMarkedSet & marked ){ };
    virtual bool isTransforming( ) const { return false; }
  };

  /*
  class IfChain : public Expression
  {
    Ast::SourceLocation loc_;
    std::list< IfExpr * > * clauses_;
    Expression * elseexpr_;
  public:
    IfChain( const Ast::SourceLocation & loc, std::list< IfExpr * > * clauses, Expression * elseexpr );
    IfChain( const Ast::SourceLocation & loc, std::list< IfExpr * > * clauses );
    virtual ~IfChain( );
    virtual void eval( Kernel::EvalState * evalState ) const;
    virtual const Ast::SourceLocation & firstLoc( ) const;
    virtual const Ast::SourceLocation & lastLoc( ) const;
  };
  */

  }
}


#endif
