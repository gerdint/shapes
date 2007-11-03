#include "astidentifier.h"
#include "shapesexceptions.h"

Shapes::LiteralIdentifier::LiteralIdentifier( const Ast::SourceLocation & _loc, const char * _id )
  : loc( _loc ), id( strdup( _id ) )
{ }

Shapes::LiteralIdentifier::~LiteralIdentifier( )
{ }

RefCountPtr< const char >
Shapes::LiteralIdentifier::identifier( Kernel::VariableHandle dstgroup, SimplePDF::PDF_out * pdfo, Kernel::GraphicsState * metaState, Kernel::PassedEnv env ) const
{
  return id;
}


Shapes::SameIdentifier::SameIdentifier( const Shapes::IdentifierNode * _orig )
  : orig( _orig )
{ }

Shapes::SameIdentifier::~SameIdentifier( )
{
  /* The point is that we don't delete the original identifier here.
     We must hope that the original is not deleted before us...
  */
}

RefCountPtr< const char >
Shapes::SameIdentifier::identifier( Kernel::VariableHandle dstgroup, SimplePDF::PDF_out * pdfo, Kernel::GraphicsState * metaState, Kernel::PassedEnv env ) const
{
  return orig->identifier( dstgroup, pdfo, metaState, env );
}
