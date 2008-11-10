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

#ifndef basicsimplex_h
#define basicsimplex_h

#include "Shapes_Computation_decls.h"

#include <cstddef>

namespace Shapes
{
	namespace Computation
	{
		class BasicSimplex
		{
			size_t nVars_;
			size_t nEqns_;
			size_t nExt_;

			mutable double * a_;
			mutable double * b_;
			mutable double * c_;

			mutable size_t * varSet_;
			mutable size_t * nonBasicBegin_;
			mutable size_t * nonBasicEnd_;
			mutable size_t * basicBegin_;
			mutable size_t * basicEnd_;

		public:
			BasicSimplex( size_t nVars, size_t nEqns );
			~BasicSimplex( );

			bool minimize( double * xdst,
										 double * objdst, double objGoal,
										 const double * c, const double * a, const double * b,
										 bool changeSign = false ) const;
			bool maximize( double * xdst,
										 double * objdst, double objGoal,
										 const double * c, const double * a, const double * b ) const;

		protected:
			bool phaseOne( ) const;
			double phaseTwo( double * xdst, double objGoal ) const;
		};
	}
}

#endif
