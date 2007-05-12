#ifndef metapdfastexpr_h
#define metapdfastexpr_h

#include "metapdfast.h"

namespace MetaPDF
{
  namespace Ast
  {

    class UnaryExpr : public Expression
    {
    public:
      UnaryExpr( const Ast::SourceLocation & loc );
      virtual ~UnaryExpr( );
      virtual RefCountPtr< const Lang::Value > throwNotApplicable( const Lang::Value * arg ) const = 0;
      DEFAULTUNARYOPDECL;
    };
    
    class UnaryPrefixExpr : public UnaryExpr
    {
    protected:
      Ast::SourceLocation opLoc_;
      Ast::Expression * expr_;
    public:
      UnaryPrefixExpr( const Ast::SourceLocation & opLoc, Ast::Expression * expr );
      virtual ~UnaryPrefixExpr( );
      virtual void eval( Kernel::EvalState * evalState ) const;
      virtual RefCountPtr< const Lang::Value > throwNotApplicable( const Lang::Value * arg ) const;
    };
    
    class UnaryPostfixExpr : public UnaryExpr
    {
    protected:
      Ast::SourceLocation opLoc_;
      Ast::Expression * expr_;
    public:
      UnaryPostfixExpr( const Ast::SourceLocation & opLoc, Ast::Expression * expr );
      virtual ~UnaryPostfixExpr( );
      virtual void eval( Kernel::EvalState * evalState ) const;
      virtual RefCountPtr< const Lang::Value > throwNotApplicable( const Lang::Value * arg ) const;
    };
    
    class BinaryInfixExpr : public Expression
    {
    protected:
      Ast::SourceLocation opLoc_;
      Ast::Expression * expr1_;
      Ast::Expression * expr2_;
    public:
      BinaryInfixExpr( const Ast::SourceLocation & opLoc, Ast::Expression * expr1,  Ast::Expression * expr2 );
      virtual ~BinaryInfixExpr( );
      virtual void eval( Kernel::EvalState * evalState ) const;
      virtual RefCountPtr< const Lang::Value > throwNotApplicable( const Lang::Value * arg1, const Lang::Value * arg2 ) const;
      const Ast::Expression * get_expr2( ) const { return expr2_; } /* to be used by the continuation */
      DEFAULTBINARYOPDECL;
    };

  }

  namespace Kernel
  {

    class UnaryCont_1 : public Kernel::Continuation
    {
      const Ast::UnaryExpr * op_;
      Kernel::PassedDyn dyn_;
      Kernel::ContRef cont_;
    public:
      UnaryCont_1( const Ast::UnaryExpr * op, const Kernel::PassedDyn & dyn, const Kernel::ContRef & cont, const Ast::SourceLocation & traceLoc );
      virtual ~UnaryCont_1( );
      virtual void takeValue( const RefCountPtr< const Lang::Value > & val, Kernel::EvalState * evalState, bool dummy ) const;
      virtual void backTrace( std::list< Kernel::Continuation::BackTraceElem > * trace ) const;
      virtual void gcMark( Kernel::GCMarkedSet & marked );
    };
    
    class BinaryInfixCont_2 : public Kernel::Continuation
    {
      const Ast::BinaryInfixExpr * op_;
      Kernel::ValueRef val1_;
      Kernel::PassedDyn dyn_;
      Kernel::ContRef cont_;
    public:
      BinaryInfixCont_2( const Ast::BinaryInfixExpr * op, const Kernel::ValueRef & val1, const Kernel::PassedDyn & dyn, const Kernel::ContRef & cont, const Ast::SourceLocation & traceLoc );
      virtual ~BinaryInfixCont_2( );
      virtual void takeValue( const RefCountPtr< const Lang::Value > & val, Kernel::EvalState * evalState, bool dummy ) const;
      virtual void backTrace( std::list< Kernel::Continuation::BackTraceElem > * trace ) const;
      virtual void gcMark( Kernel::GCMarkedSet & marked );
    };
    
    class BinaryInfixCont_1 : public Kernel::Continuation
    {
      const Ast::BinaryInfixExpr * op_;
      Kernel::PassedEnv env_;
      Kernel::PassedDyn dyn_;
      Kernel::ContRef cont_;
    public:
      BinaryInfixCont_1( const Ast::BinaryInfixExpr * op, Kernel::EvalState & evalState, const Ast::SourceLocation & traceLoc );
      virtual ~BinaryInfixCont_1( );
      virtual void takeValue( const RefCountPtr< const Lang::Value > & val, Kernel::EvalState * evalState, bool dummy ) const;
      virtual void backTrace( std::list< Kernel::Continuation::BackTraceElem > * trace ) const;
      virtual void gcMark( Kernel::GCMarkedSet & marked );
    };
    
  }
}


#endif
