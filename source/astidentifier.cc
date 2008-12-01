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
