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

#ifndef astidentifier_h
#define astidentifier_h

#include "ast.h"


namespace Shapes
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
			virtual void analyze( );
			virtual RefCountPtr< const char > identifier( Kernel::GraphicsState * metaState, Kernel::PassedEnv env ) const;
		};

		class SameIdentifier : public Ast::IdentifierNode
		{
			const Ast::IdentifierNode * orig_;
		public:
			SameIdentifier( const Ast::IdentifierNode * orig );
			virtual ~SameIdentifier( );
			virtual void analyze( );
			virtual RefCountPtr< const char > identifier( Kernel::GraphicsState * metaState, Kernel::PassedEnv env ) const;
		};

	}
}
#endif
