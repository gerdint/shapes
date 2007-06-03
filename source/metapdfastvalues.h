#ifndef metapdfastvalues_h
#define metapdfastvalues_h

#include "metapdfast.h"

namespace MetaPDF
{
  namespace Ast
  {

    class Constant : public Expression
    {
      Kernel::VariableHandle val_;
    public:
      Constant( const Ast::SourceLocation & loc, const Kernel::VariableHandle & val );
      Constant( const Ast::SourceLocation & loc, RefCountPtr< const Lang::Value > val );
      Constant( const Ast::SourceLocation & loc, const Lang::Value * val );
      virtual ~Constant( );
      virtual void analyze( Ast::Node * parent, const Ast::AnalysisEnvironment * env );
      virtual void eval( Kernel::EvalState * evalState ) const;
    };
    
    class PolarHandle2DExpr : public Expression
    {
      Ast::Expression * rExpr_;
      Ast::Expression * aExpr_;
    public:
      PolarHandle2DExpr( const Ast::SourceLocation & loc, Ast::Expression * rExpr, Ast::Expression * aExpr );
      virtual ~PolarHandle2DExpr( );
      virtual void analyze( Ast::Node * parent, const Ast::AnalysisEnvironment * env );
      virtual void eval( Kernel::EvalState * evalState ) const;
    };
    
    class PolarHandle2DExprFree_a : public Expression
    {
      Ast::Expression * rExpr_;
    public:
      PolarHandle2DExprFree_a( const Ast::SourceLocation & loc, Ast::Expression * rExpr );
      virtual ~PolarHandle2DExprFree_a( );
      virtual void analyze( Ast::Node * parent, const Ast::AnalysisEnvironment * env );
      virtual void eval( Kernel::EvalState * evalState ) const;
    };
    
    /* This class really doesn't fit in any file, but here it is!
     */
    class EmptyExpression : public Expression
    {
    public:
      EmptyExpression( const Ast::SourceLocation & loc );
      virtual ~EmptyExpression( );
      virtual void analyze( Ast::Node * parent, const Ast::AnalysisEnvironment * env );
      virtual void eval( Kernel::EvalState * evalState ) const;
    };
  
  }

  namespace Kernel
  {

    class PolarHandle2DCont : public Kernel::Continuation
    {
      RefCountPtr< Kernel::PolarHandlePromise > rPromise_;
      Kernel::ContRef cont_;
    public:
      PolarHandle2DCont( const Ast::SourceLocation & _traceLoc, const RefCountPtr< Kernel::PolarHandlePromise > & rPromise, const Kernel::ContRef & cont );
      virtual ~PolarHandle2DCont( );
      virtual void takeValue( const RefCountPtr< const Lang::Value > & val, Kernel::EvalState * evalState, bool dummy ) const;
      virtual void backTrace( std::list< Kernel::Continuation::BackTraceElem > * trace ) const;
      virtual void gcMark( Kernel::GCMarkedSet & marked );
    };

  }
  
}

#endif
