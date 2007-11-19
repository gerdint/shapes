Shapes::PathStatement::PathStatement( Ast::Expression * _expr )
	: expr( _expr )
{ }

Shapes::PathStatement::~PathStatement( )
{ }

const Lang::MultiPath2D *
Shapes::PathStatement::path( )
{
	const Lang::MultiPath2D * res = dynamic_cast< const Lang::MultiPath2D * >( expr->value( ).getPtr( ) );
	if( res == 0 )
		{
			throw( "Path-statement expected value of type path." );
		}
	return res;
}

Shapes::Stroke::Stroke( Ast::Expression * _expr )
	: PathStatement( _expr )
{ }

Shapes::Stroke::~Stroke( )
{ }

void
Shapes::Stroke::writeTo( ostream & os, GraphicsState * metaState, GraphicsState * pdfState )
{
	path( )->stroke( os );
}


Shapes::Fill::Fill( Ast::Expression * _expr )
	: PathStatement( _expr )
{ }

Shapes::Fill::~Fill( )
{ }

void
Shapes::Fill::writeTo( ostream & os, GraphicsState * metaState, GraphicsState * pdfState )
{
	path( )->fill( os );
}

