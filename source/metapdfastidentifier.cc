#include "metapdfastidentifier.h"
#include "metapdfexceptions.h"

MetaPDF::LiteralIdentifier::LiteralIdentifier( const Ast::SourceLocation & _loc, const char * _id )
  : loc( _loc ), id( strdup( _id ) )
{ }

MetaPDF::LiteralIdentifier::~LiteralIdentifier( )
{ }

RefCountPtr< const char >
MetaPDF::LiteralIdentifier::identifier( Kernel::HandleType dstgroup, SimplePDF::PDF_out * pdfo, Kernel::GraphicsState * metaState, Kernel::PassedEnv env ) const
{
  return id;
}


MetaPDF::SameIdentifier::SameIdentifier( const MetaPDF::IdentifierNode * _orig )
  : orig( _orig )
{ }

MetaPDF::SameIdentifier::~SameIdentifier( )
{
  /* The point is that we don't delete the original identifier here.
     We must hope that the original is not deleted before us...
  */
}

RefCountPtr< const char >
MetaPDF::SameIdentifier::identifier( Kernel::HandleType dstgroup, SimplePDF::PDF_out * pdfo, Kernel::GraphicsState * metaState, Kernel::PassedEnv env ) const
{
  return orig->identifier( dstgroup, pdfo, metaState, env );
}
