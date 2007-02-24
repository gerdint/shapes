#ifndef metapdfastfun_h
#define metapdfastfun_h

#include "MetaPDF_Ast_decls.h"
#include "MetaPDF_Kernel_decls.h"

#include "metapdfast.h"


namespace MetaPDF
{
  namespace Kernel
  {
    class Formals
    {
      Ast::SourceLocation loc_;
    public:
      bool seenDefault_;
      std::map< const char *, size_t, charPtrLess > * argumentOrder_;
      std::vector< Ast::Expression * > defaultExprs_;
      std::vector< bool > forcePos_;
      std::map< const char *, size_t, charPtrLess > * stateOrder_;
      Formals( );
      ~Formals( );
      void setLoc( Ast::SourceLocation loc );
      
      void push_exprs( Ast::ArgListExprs * args ) const;
      Kernel::EvaluatedFormals * newEvaluatedFormals( Kernel::Arguments & args ) const;
      Kernel::EvaluatedFormals * newEvaluatedFormals( Kernel::Arguments & args, size_t * pos ) const;  // values are taken at *pos, and *pos is incremented accordingly
      
      std::vector< bool > * newArgListForcePos( const Ast::ArgListExprs * argList ) const;
      std::vector< bool > * newArgListForcePos( const Ast::ArgListExprs * argList, const Kernel::Arguments & curryArgs ) const;
      
      const Ast::SourceLocation & loc( ) const;
    };
    
    class CallContInfo
    {
      std::vector< bool > * forcePos_;
      bool forceAll_;                     /* if forcePos == 0, then either all or none are forced according to this variable */
    public:
      const Ast::ArgListExprs * argList_;
      Kernel::PassedEnv env_;
      Kernel::PassedDyn dyn_;
      Kernel::ContRef cont_;
      
      CallContInfo( const Ast::ArgListExprs * argList, const Kernel::EvalState & evalState, std::vector< bool > * forcePos );
      CallContInfo( const Ast::ArgListExprs * argList, const Kernel::EvalState & evalState, bool forceAll );
      ~CallContInfo( );
      
      bool force( const size_t & pos ) const;
      bool isSelective( ) const;
      void gcMark( Kernel::GCMarkedSet & marked );
    };
    
  }

  namespace Ast
  {

    class ArgListExprs
    {
      bool exprOwner_;
    public:
      std::list< Ast::Expression * > * orderedExprs_;
      std::map< const char *, Ast::Expression *, charPtrLess > * namedExprs_;
      std::list< Ast::LexicalState * > * orderedStates_;
      std::map< const char *, Ast::LexicalState *, charPtrLess > * namedStates_;
      
      class ConstIterator
      {
      public:
	std::list< Ast::Expression * >::const_reverse_iterator i1_;
	std::map< const char *, Ast::Expression *, charPtrLess >::const_iterator i2_;
	size_t index_;
	
	ConstIterator( const ConstIterator & orig );
	ConstIterator( std::list< Ast::Expression * >::const_reverse_iterator i1, std::map< const char *, Ast::Expression *, charPtrLess >::const_iterator i2, const size_t & index  );
      };
      
      ArgListExprs( bool exprOwner );
      ArgListExprs( std::list< Ast::Expression * > * orderedExprs, std::map< const char *, Ast::Expression *, charPtrLess > * namedExprs, std::list< Ast::LexicalState * > * orderedStates, std::map< const char *, Ast::LexicalState *, charPtrLess > * namedStates );
      ArgListExprs( size_t numberOfOrderedDummyExprs );
      ~ArgListExprs( );
      
      ConstIterator begin( ) const;
      
      void evaluate( const RefCountPtr< const Kernel::CallContInfo > & info, const ArgListExprs::ConstIterator & pos, const RefCountPtr< const Lang::SingleList > & vals, Kernel::EvalState * evalState ) const;
      void bind( Kernel::Arguments * dst, RefCountPtr< const Lang::SingleList > vals, Kernel::PassedEnv env ) const;

      Kernel::VariableHandle findNamed( RefCountPtr< const Lang::SingleList > vals, const char * name ) const;
    };
    
    class FunctionFunction : public Lang::Function
    {
      Ast::SourceLocation loc_;
      const Kernel::Formals * formals_;
      Ast::Expression * body_;
      Ast::FunctionMode functionMode_;
    public:
      FunctionFunction( const Ast::SourceLocation & loc, const Kernel::Formals * formals, Ast::Expression * body, const Ast::FunctionMode & functionMode );
      virtual ~FunctionFunction( );
      
      void push_exprs( Ast::ArgListExprs * args ) const;
      
      virtual void call( Kernel::EvalState * evalState, Kernel::Arguments & args, const Ast::SourceLocation & callLoc ) const;
      
      virtual void gcMark( Kernel::GCMarkedSet & marked ){ };
      virtual bool isTransforming( ) const { return false; }
    };

  }

  namespace Kernel
  {

    class CallCont_1 : public Kernel::Continuation
    {
      const Ast::ArgListExprs * argList_;
      bool curry_;
      bool procedural_;
      Kernel::PassedEnv env_;
      Kernel::PassedDyn dyn_;
      Kernel::ContRef cont_;
      const Ast::SourceLocation & callLoc_;
    public:
      CallCont_1( const Ast::SourceLocation & traceLoc, const Ast::ArgListExprs * argList, bool curry, bool procedural, const Kernel::EvalState & evalState, const Ast::SourceLocation & callLoc );
      virtual ~CallCont_1( );
      virtual void takeValue( const RefCountPtr< const Lang::Value > & val, Kernel::EvalState * evalState, bool dummy ) const;
      virtual void backTrace( std::list< Kernel::Continuation::BackTraceElem > * trace ) const;
      virtual void gcMark( Kernel::GCMarkedSet & marked );
    };
    
    class CallCont_last : public Kernel::Continuation
    {
      RefCountPtr< const Lang::Function > fun_;
      const Ast::ArgListExprs * argList_;
      bool curry_;
      Kernel::PassedEnv env_;
      Kernel::PassedDyn dyn_;
      Kernel::ContRef cont_;
    public:
      CallCont_last( const RefCountPtr< const Lang::Function > & fun, const Ast::ArgListExprs * argList, bool curry, const Kernel::PassedEnv & env, const Kernel::PassedDyn & dyn, const Kernel::ContRef & cont, const Ast::SourceLocation & callLoc );
      virtual ~CallCont_last( );
      virtual void takeValue( const RefCountPtr< const Lang::Value > & valsUntyped, Kernel::EvalState * evalState, bool dummy ) const;
      virtual void backTrace( std::list< Kernel::Continuation::BackTraceElem > * trace ) const;
      virtual void gcMark( Kernel::GCMarkedSet & marked );
    };
    
    class CallCont_n : public Kernel::Continuation
    {
      RefCountPtr< const Kernel::CallContInfo > info_;
      Ast::ArgListExprs::ConstIterator pos_;
      RefCountPtr< const Lang::SingleList > vals_;
    public:
      CallCont_n( const Ast::SourceLocation & traceLoc, const RefCountPtr< const Kernel::CallContInfo > & info, const Ast::ArgListExprs::ConstIterator & pos, RefCountPtr< const Lang::SingleList > vals );
      virtual ~CallCont_n( );
      virtual void takeHandle( Kernel::VariableHandle val, Kernel::EvalState * evalState, bool dummy ) const;
      virtual void backTrace( std::list< Kernel::Continuation::BackTraceElem > * trace ) const;
      virtual void gcMark( Kernel::GCMarkedSet & marked );
    };

    class UnionCont_last : public Kernel::Continuation
    {
      const Ast::ArgListExprs * argList_;
      Kernel::ContRef cont_;
    public:
      UnionCont_last( const Ast::ArgListExprs * argList, const Kernel::ContRef & cont, const Ast::SourceLocation & callLoc );
      virtual ~UnionCont_last( );
      virtual void takeValue( const RefCountPtr< const Lang::Value > & valsUntyped, Kernel::EvalState * evalState, bool dummy ) const;
      virtual void backTrace( std::list< Kernel::Continuation::BackTraceElem > * trace ) const;
      virtual void gcMark( Kernel::GCMarkedSet & marked );
    };

    class SplitCont_1 : public Kernel::Continuation
    {
      Ast::Expression * argList_;
      bool curry_;
      Kernel::PassedEnv env_;
      Kernel::PassedDyn dyn_;
      Kernel::ContRef cont_;
      const Ast::SourceLocation & callLoc_;
    public:
      SplitCont_1( const Ast::SourceLocation & traceLoc, Ast::Expression * argList, bool curry, const Kernel::EvalState & evalState, const Ast::SourceLocation & callLoc );
      virtual ~SplitCont_1( );
      virtual void takeValue( const RefCountPtr< const Lang::Value > & val, Kernel::EvalState * evalState, bool dummy ) const;
      virtual void backTrace( std::list< Kernel::Continuation::BackTraceElem > * trace ) const;
      virtual void gcMark( Kernel::GCMarkedSet & marked );
    };
    
    class SplitCont_2 : public Kernel::Continuation
    {
      RefCountPtr< const Lang::Function > fun_;
      bool curry_;
      Kernel::PassedEnv env_;
      Kernel::PassedDyn dyn_;
      Kernel::ContRef cont_;
    public:
      SplitCont_2( const RefCountPtr< const Lang::Function > & fun, bool curry, const Kernel::PassedEnv & env, const Kernel::PassedDyn & dyn, const Kernel::ContRef & cont, const Ast::SourceLocation & callLoc );
      virtual ~SplitCont_2( );
      virtual void takeValue( const RefCountPtr< const Lang::Value > & valsUntyped, Kernel::EvalState * evalState, bool dummy ) const;
      virtual void backTrace( std::list< Kernel::Continuation::BackTraceElem > * trace ) const;
      virtual void gcMark( Kernel::GCMarkedSet & marked );
    };
    
  }

  namespace Ast
  {

    class CallExpr : public Expression
    {
      bool curry_;
      bool procedural_;
      RefCountPtr< const Lang::Function > constFun_;
    public:
      Ast::Expression * funExpr_;
      const Ast::ArgListExprs * argList_;
      
      CallExpr( const Ast::SourceLocation & loc, Ast::Expression * funExpr, const Ast::ArgListExprs * argList, bool curry = false, bool procedural = false );
      CallExpr( const Ast::SourceLocation & loc, const RefCountPtr< const Lang::Function > & constFun, const Ast::ArgListExprs * argList, bool curry = false, bool procedural = false );
      virtual ~CallExpr( );
      virtual void eval( Kernel::EvalState * evalState ) const;
    };
    
    class UnionExpr : public Expression
    {
    public:
      const Ast::ArgListExprs * argList_;
      
      UnionExpr( const Ast::SourceLocation & loc, const Ast::ArgListExprs * argList );
      virtual ~UnionExpr( );
      virtual void eval( Kernel::EvalState * evalState ) const;
    };
    
    class CallSplitExpr : public Expression
    {
      bool curry_;
      Ast::Expression * funExpr_;
      Ast::Expression * argList_;

    public:      
      CallSplitExpr( const Ast::SourceLocation & loc, Ast::Expression * funExpr, Ast::Expression * argList, bool curry = false );
      virtual ~CallSplitExpr( );
      virtual void eval( Kernel::EvalState * evalState ) const;
    };
    
    class DummyExpression : public Expression
    {
    public:
      DummyExpression( );
      DummyExpression( const Ast::SourceLocation & loc );
      virtual ~DummyExpression( );
      virtual void eval( Kernel::EvalState * evalState ) const;
    };
  }

}

#endif
