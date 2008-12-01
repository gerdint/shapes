/* This file is part of Shapes.
 *
 * Shapes is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * any later version.
 *
 * Shapes is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with Shapes.  If not, see <http://www.gnu.org/licenses/>.
 *
 * Copyright 2008 Henrik Tidefelt
 */

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

