#ifndef metapdfast_h
#define metapdfast_h

#include "MetaPDF_Ast_decls.h"
#include "MetaPDF_Kernel_decls.h"

#include "refcount.h"
#include "metapdftypes.h"
#include "dynamicenvironment.h"
#include "sourcelocation.h"

#include <list>
#include <iostream>
#include <string>

namespace MetaPDF
{

  namespace Kernel
  {
 
    class EvalState
    {
    public:
      Ast::Expression * expr_;
      Kernel::PassedEnv env_;
      Kernel::PassedDyn dyn_;
      Kernel::ContRef cont_;
      
      EvalState( Ast::Expression * expr, const Kernel::PassedEnv & env, const Kernel::PassedDyn & dyn, const Kernel::ContRef & cont )
	: expr_( expr ), env_( env ), dyn_( dyn ), cont_( cont )
      { }
    };

    class Continuation
    {
    protected:
      const Ast::SourceLocation & traceLoc_;
    public:
      Continuation( const Ast::SourceLocation & traceLoc )
	: traceLoc_( traceLoc )
      { }
      virtual ~Continuation( )
      { }
      virtual void takeHandle( Kernel::HandleType val, Kernel::EvalState * evalState, bool callingMyself = false ) const;
      virtual void takeValue( const RefCountPtr< const Lang::Value > & val, Kernel::EvalState * evalState, bool callingMyself = false ) const;
      virtual void gcMark( Kernel::GCMarkedSet & marked ) = 0;
      const Ast::SourceLocation & traceLoc( ) const;
      class BackTraceElem
      {
	RefCountPtr< const char > mem_;
      public:
	const Kernel::Continuation * cont_;
	const char * msg_;
	BackTraceElem( const Kernel::Continuation * cont, const char * msg );
	BackTraceElem( const Kernel::Continuation * cont, const RefCountPtr< const char > msg );
	~BackTraceElem( );
      };
      virtual void backTrace( std::list< Kernel::Continuation::BackTraceElem > * trace ) const = 0; // outer continuation before ourselves in the list, that is first push_front ourselves, then recurse.
      void backTrace( std::ostream & os );
    };
    
    std::ostream & operator << ( std::ostream & os, const Kernel::Continuation::BackTraceElem & elem );
    
  }

  namespace Ast
  {

    class Node
    {
    protected:
      Node * parent_;
      const Ast::SourceLocation loc_;
    public:
      Node( const Ast::SourceLocation & loc );
      virtual ~Node( );
      void setParent( Node * parent );
      virtual void eval( Kernel::EvalState * evalState ) const = 0;
      const Ast::SourceLocation & loc( ) const;
    };
    
    class Expression : public Node
    {
    public:
      bool immediate_;
      Expression( const Ast::SourceLocation & loc );
      virtual ~Expression( );
    };
    
    class BindNode : public Node
    {
    protected:
      Ast::SourceLocation idLoc_;
      const char * id_;
    public:
      BindNode( const Ast::SourceLocation & loc, const Ast::SourceLocation & idLoc, const char * id );
      virtual ~BindNode( );
      const char * id( ) const;
      const Ast::SourceLocation & idLoc( ) const;
    };
    
    class SequencingNode : public Node
    {
    protected:
      Ast::SourceLocation idLoc_;
      const char * id_;
    public:
      SequencingNode( const Ast::SourceLocation & loc, const Ast::SourceLocation & idLoc, const char * id );
      virtual ~SequencingNode( );
      const char * id( ) const;
      const Ast::SourceLocation & idLoc( ) const;
    };
    
    class IdentifierNode
    {
    public:
      IdentifierNode( );
      virtual ~IdentifierNode( );
      virtual RefCountPtr< const char > identifier( Kernel::EvalState * evalState ) const = 0;
    };

  }

}

#endif
