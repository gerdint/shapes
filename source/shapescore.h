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

#ifndef shapescore_h
#define shapescore_h

#include <list>
#include <iostream>
#include <string>

#include "refcount.h"
#include "shapestypes.h"


namespace Shapes
{
	namespace Lang
	{

		class CoreFunction : public Lang::Function
		{
		protected:
			const char * title_;
		public:
			CoreFunction( const char * title );
			CoreFunction( const char * title, Kernel::EvaluatedFormals * formals );
			virtual void analyze( Ast::Node * parent, const Ast::AnalysisEnvironment * env );
			virtual bool isTransforming( ) const;
			const char * getTitle( ) const;
			virtual void gcMark( Kernel::GCMarkedSet & marked ){ };
		};

	}

}

#endif

