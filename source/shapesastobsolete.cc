MetaPDF::PathStatement::PathStatement( Ast::Expression * _expr )
  : expr( _expr )
{ }

MetaPDF::PathStatement::~PathStatement( )
{ }

const Lang::Path2D *
MetaPDF::PathStatement::path( )
{
  const Lang::Path2D * res = dynamic_cast< const Lang::Path2D * >( expr->value( ).getPtr( ) );
  if( res == 0 )
    {
      throw( "Path-statement expected value of type path." );
    }
  return res;
}

MetaPDF::Stroke::Stroke( Ast::Expression * _expr )
  : PathStatement( _expr )
{ }

MetaPDF::Stroke::~Stroke( )
{ }

void
MetaPDF::Stroke::writeTo( ostream & os, GraphicsState * metaState, GraphicsState * pdfState )
{
  path( )->stroke( os );
}


MetaPDF::Fill::Fill( Ast::Expression * _expr )
  : PathStatement( _expr )
{ }

MetaPDF::Fill::~Fill( )
{ }

void
MetaPDF::Fill::writeTo( ostream & os, GraphicsState * metaState, GraphicsState * pdfState )
{
  path( )->fill( os );
}

