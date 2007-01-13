#ifndef metapdfastvar_h
#define metapdfastvar_h

#include "MetaPDF_Kernel_decls.h"

#include "metapdfast.h"

namespace MetaPDF
{
  namespace Ast
  {

    class CodeBracket : public Expression
    {
      std::list< Ast::Node * > * nodes_;
      std::map< const char *, size_t, charPtrLess > * argumentOrder_;
      std::map< const char *, size_t, charPtrLess > * dynamicMap_;
      std::map< const char *, size_t, charPtrLess > * stateOrder_;
    public:
      CodeBracket( const Ast::SourceLocation & loc, std::list< Ast::Node * > * nodes );
      virtual ~CodeBracket( );
      
      virtual void eval( Kernel::EvalState * evalState ) const;
      
      void evalAt( const RefCountPtr< const Kernel::CodeBracketContInfo > & info, const std::list< Ast::Node * >::const_iterator & i, Kernel::EvalState * evalState ) const;
    };
    
  }

  namespace Kernel
  {
    class CodeBracketContInfo
    {
    public:
      const Ast::CodeBracket * bracketExpr_;
      Kernel::PassedEnv env_;
      Kernel::PassedDyn dyn_;
      Kernel::ContRef cont_;
      
      CodeBracketContInfo( const Ast::CodeBracket * bracketExpr, const Kernel::EvalState & evalState );
      ~CodeBracketContInfo( );
      
      void gcMark( Kernel::GCMarkedSet & marked );
    };

    class CodeBracketContinuation : public Kernel::Continuation
    {
      RefCountPtr< const Kernel::CodeBracketContInfo > info_;
      std::list< Ast::Node * >::const_iterator pos_;
    public:
      CodeBracketContinuation( const Ast::SourceLocation & _traceLoc, const RefCountPtr< const Kernel::CodeBracketContInfo > & _info, const std::list< Ast::Node * >::const_iterator & _pos );
      virtual ~CodeBracketContinuation( );
      virtual void takeValue( const RefCountPtr< const Lang::Value > & val, Kernel::EvalState * evalState, bool dummy ) const;
      virtual void backTrace( std::list< Kernel::Continuation::BackTraceElem > * trace ) const;
      virtual void gcMark( Kernel::GCMarkedSet & marked );
    };

    
    class DynamicBindingContinuation : public Kernel::Continuation
    {
      Kernel::PassedEnv env_;
      Kernel::Environment::LexicalKey key_;
      Ast::SourceLocation idLoc_;
      Kernel::ContRef cont_;
    public:
      DynamicBindingContinuation( const Ast::SourceLocation & traceLoc, const Kernel::PassedEnv & env, const Kernel::Environment::LexicalKey & key, const Ast::SourceLocation & idLoc, const Kernel::ContRef & cont );
      virtual ~DynamicBindingContinuation( );
      virtual void takeHandle( Kernel::VariableHandle val, Kernel::EvalState * evalState, bool dummy ) const;
      virtual void backTrace( std::list< Kernel::Continuation::BackTraceElem > * trace ) const;
      virtual void gcMark( Kernel::GCMarkedSet & marked );
    };
    
    class WithDynamicContinuation : public Kernel::Continuation
    {
      Ast::Expression * expr_;
      Kernel::PassedEnv env_;
      Kernel::PassedDyn dyn_;
      Kernel::ContRef cont_;
    public:
      WithDynamicContinuation( const Ast::SourceLocation & traceLoc, Ast::Expression * expr, const Kernel::EvalState & evalState );
      virtual ~WithDynamicContinuation( );
      virtual void takeValue( const RefCountPtr< const Lang::Value > & val, Kernel::EvalState * evalState, bool dummy ) const;
      virtual void backTrace( std::list< Kernel::Continuation::BackTraceElem > * trace ) const;
      virtual void gcMark( Kernel::GCMarkedSet & marked );
    };
    
    class DynamicVariableDeclContinuation : public Kernel::Continuation
    {
      const Ast::DynamicVariableDecl * declExpr_;
      Kernel::PassedEnv env_;
      Kernel::PassedDyn dyn_;
      Kernel::ContRef cont_;
    public:
      DynamicVariableDeclContinuation( const Ast::SourceLocation & traceLoc, const Ast::DynamicVariableDecl * declExpr, Kernel::EvalState & evalState );
      virtual ~DynamicVariableDeclContinuation( );
      virtual void takeValue( const RefCountPtr< const Lang::Value > & val, Kernel::EvalState * evalState, bool dummy ) const;
      virtual void backTrace( std::list< Kernel::Continuation::BackTraceElem > * trace ) const;
      virtual void gcMark( Kernel::GCMarkedSet & marked );
    };

  }

  namespace Ast
  {
    class LexiographicVariable : public Expression 
    {
      const char * id_;
      mutable Kernel::Environment::LexicalKey ** idKey_;
      bool warm_;  // this is used when passing a warm variable in a funciton call
    public:
      LexiographicVariable( const Ast::SourceLocation & loc, const char * id, Kernel::Environment::LexicalKey ** idKey, bool warm = false );
      virtual ~LexiographicVariable( );
      virtual void eval( Kernel::EvalState * evalState ) const;
    };
    
    class EvalOutsideExpr : public Expression 
    {
      Ast::Expression * expr_;
    public:
      EvalOutsideExpr( const Ast::SourceLocation & loc, Ast::Expression * expr );
      virtual ~EvalOutsideExpr( );
      virtual void eval( Kernel::EvalState * evalState ) const;
    };
    
    class DefineVariable : public BindNode
    {
      Ast::Expression * expr_;
      mutable size_t ** idPos_;
    public:
      DefineVariable( const Ast::SourceLocation & idLoc, const char * id, Ast::Expression * expr, size_t ** idPos );
      virtual ~DefineVariable( );
      virtual void eval( Kernel::EvalState * evalState ) const;
    };
    
    class LexiographicState : public Node
    {
      const char * id_;
      mutable Kernel::Environment::LexicalKey ** idKey_;
    public:
      LexiographicState( const Ast::SourceLocation & loc, const char * id, Kernel::Environment::LexicalKey ** idKey );
      virtual ~LexiographicState( );
      virtual void eval( Kernel::EvalState * evalState ) const;  // illegal to call
      Kernel::StateHandle getHandle( Kernel::PassedEnv env ) const;
    };
    
    class IntroduceState : public BindNode
    {
      Ast::Expression * expr_;
      mutable size_t ** idPos_;
    public:
      IntroduceState( const Ast::SourceLocation & idLoc, const char * id, Ast::Expression * expr, size_t ** idPos );
      virtual ~IntroduceState( );
      virtual void eval( Kernel::EvalState * evalState ) const;
    };
    
    class LexiographicInsertion : public SequencingNode
    {
      Ast::Expression * expr_;
      mutable Kernel::Environment::LexicalKey ** idKey_;
    public:
      LexiographicInsertion( const Ast::SourceLocation & idLoc, const char * id, Ast::Expression * expr, Kernel::Environment::LexicalKey ** idKey );
      virtual ~LexiographicInsertion( );
      virtual void eval( Kernel::EvalState * evalState ) const;
    };
    
    class DynamicInsertion : public SequencingNode
    {
      Ast::Expression * expr_;
      mutable Kernel::Environment::LexicalKey ** idKey_;
    public:
      DynamicInsertion( const Ast::SourceLocation & idLoc, const char * id, Ast::Expression * expr, Kernel::Environment::LexicalKey ** idKey );
      virtual ~DynamicInsertion( );
      virtual void eval( Kernel::EvalState * evalState ) const;
    };
    
    class Freeze : public Expression
    {
      mutable size_t ** idPos_;
    public:
      Freeze( const Ast::SourceLocation & idLoc, const char * id, size_t ** idPos );
      virtual ~Freeze( );
      virtual void eval( Kernel::EvalState * evalState ) const;
    };
    
    class Peek : public Expression
    {
      mutable size_t ** idPos_;
    public:
      Peek( const Ast::SourceLocation & idLoc, const char * id, size_t ** idPos );
      virtual ~Peek( );
      virtual void eval( Kernel::EvalState * evalState ) const;
    };
    
    class EvalSymbolFunction : public Lang::Function
    {
      Ast::SourceLocation loc_;
      Ast::Expression * expr_;
    public:
      EvalSymbolFunction( const Ast::SourceLocation & loc, Ast::Expression * expr );
      virtual ~EvalSymbolFunction( );
      void push_exprs( Ast::ArgListExprs * args ) const;
      virtual void call( Kernel::EvalState * evalState, Kernel::Arguments & args, const Ast::SourceLocation & callLoc ) const;
      virtual void gcMark( Kernel::GCMarkedSet & marked ){ };
      virtual bool isTransforming( ) const { return false; }
    };
    
    class MemberReferenceFunction : public Lang::Function
    {
      Ast::SourceLocation loc_;
      Ast::SourceLocation memberLoc_;
      Ast::Expression * variable_;
      const char * fieldID_;
    public:
      MemberReferenceFunction( const Ast::SourceLocation & loc, Ast::Expression * variable, const char * fieldID );
      virtual ~MemberReferenceFunction( );
      void push_exprs( Ast::ArgListExprs * args ) const;
      virtual void call( Kernel::EvalState * evalState, Kernel::Arguments & args, const Ast::SourceLocation & callLoc ) const;
      virtual void gcMark( Kernel::GCMarkedSet & marked ){ };
      virtual bool isTransforming( ) const { return false; }
    };
    
    class SpecialLength : public Expression
    {
      Ast::SourceLocation loc_;
      double val_;
      int sort_;
    public:
      SpecialLength( const Ast::SourceLocation & loc, double val, int sort );
      virtual ~SpecialLength( );
      virtual void eval( Kernel::EvalState * evalState ) const;
    };
    
    class DynamicVariable : public Ast::Expression
    {
      Ast::SourceLocation loc_;
      const char * id_;
      bool warm_;
      mutable Kernel::Environment::LexicalKey ** idKey_;
    public:
      DynamicVariable( const Ast::SourceLocation & loc, const char * id, bool warm = false );
      virtual ~DynamicVariable( );
      virtual void eval( Kernel::EvalState * evalState ) const;
    };
    
    class DynamicBindingExpression : public Ast::Expression
    {
      Ast::SourceLocation idLoc_;
      const char * id_;
      Ast::Expression * expr_;
      mutable Kernel::Environment::LexicalKey ** idKey_;
    public:
      DynamicBindingExpression( const Ast::SourceLocation & idLoc, const char * id, Ast::Expression * expr, Kernel::Environment::LexicalKey ** idKey );
      virtual ~DynamicBindingExpression( );
      virtual void eval( Kernel::EvalState * evalState ) const;
    };
    
    /* Note that it makes little sense to bind a dynamic variable to a fresh warm variable, since there is no way the value
     * of that variable can be retrieved.  Neither would it be possible to freeze it.
     */
    
    class DynamicBindWarmLexiographicExpr : public Ast::Expression
    {
      Ast::SourceLocation loc_;
      Ast::SourceLocation dstLoc_;
      const char * dstId_;
      mutable Kernel::Environment::LexicalKey ** dstIdKey_;
      Ast::SourceLocation srcLoc_;
      const char * srcId_;
      mutable Kernel::Environment::LexicalKey ** srcIdKey_;
    public:
      DynamicBindWarmLexiographicExpr( const Ast::SourceLocation & loc, const Ast::SourceLocation & dstLoc, const char * dstId, const Ast::SourceLocation & srcLoc, const char * srcId );
      virtual ~DynamicBindWarmLexiographicExpr( );
      virtual void eval( Kernel::EvalState * evalState ) const;
    };
    
    class DynamicBindWarmDynamicExpr : public Ast::Expression
    {
      Ast::SourceLocation loc_;
      Ast::SourceLocation dstLoc_;
      const char * dstId_;
      mutable Kernel::Environment::LexicalKey ** dstIdKey_;
      Ast::SourceLocation srcLoc_;
      const char * srcId_;
      mutable Kernel::Environment::LexicalKey ** srcIdKey_;
    public:
      DynamicBindWarmDynamicExpr( const Ast::SourceLocation & loc, const Ast::SourceLocation & dstLoc, const char * dstId, const Ast::SourceLocation & srcLoc, const char * srcId );
      virtual ~DynamicBindWarmDynamicExpr( );
      virtual void eval( Kernel::EvalState * evalState ) const;
    };
    
    class WithDynamicExpr : public Ast::Expression
    {
      Ast::Expression * bindings_;
      Ast::Expression * expr_;
    public:
      WithDynamicExpr( const Ast::SourceLocation & loc, Ast::Expression * bindings, Ast::Expression * expr );
      virtual ~WithDynamicExpr( );
      virtual void eval( Kernel::EvalState * evalState ) const;
    };
    
    class DynamicVariableDecl : public Ast::BindNode
    {
      Ast::Expression * filterExpr_;
      Ast::Expression * defaultExpr_;
      mutable size_t ** idPos_;
    public:
      DynamicVariableDecl( const Ast::SourceLocation & loc, const Ast::SourceLocation & idLoc, const char * id, Ast::Expression * filterExpr, Ast::Expression * defaultExpr, size_t ** idPos );
      virtual ~DynamicVariableDecl( );
      virtual void eval( Kernel::EvalState * evalState ) const;
      void callBack( const RefCountPtr< const Lang::Function > & filter, Kernel::EvalState * evalState ) const;
    };

  }
}


#endif
