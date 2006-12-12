#ifndef metapdfastidentifier_h
#define metapdfastidentifier_h

#include "metapdfast.h"


namespace MetaPDF
{
  namespace Ast
  {

    class LiteralIdentifier : public Ast::IdentifierNode
    {
      Ast::SourceLocation loc_;
      RefCountPtr< const char > id_;
    public:
      LiteralIdentifier( const Ast::SourceLocation & loc, const char * id );
      virtual ~LiteralIdentifier( );
      virtual RefCountPtr< const char > identifier( Kernel::GraphicsState * metaState, Kernel::PassedEnv env ) const;
    };
    
    class SameIdentifier : public Ast::IdentifierNode
    {
      const Ast::IdentifierNode * orig_;
    public:
      SameIdentifier( const Ast::IdentifierNode * orig );
      virtual ~SameIdentifier( );
      virtual RefCountPtr< const char > identifier( Kernel::GraphicsState * metaState, Kernel::PassedEnv env ) const;
    };

  }
}
#endif
