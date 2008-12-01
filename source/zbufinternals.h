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

#ifndef zbufinternals_h
#define zbufinternals_h

#include "elementarycoords.h"

#include <vector>

namespace Shapes
{
	namespace Computation
	{

		class SplicingLine
		{
		public:
			bool isTriangleSide_;
			// The line is defined in several ways.	First, by p0_ -- (p0_+lengt_*d_), where d_ is unit:
			Concrete::Coords2D p0_;
			Concrete::UnitFloatPair d_;
			Concrete::Length length_;

			// The line through p0 -- p1 is given by x: <x,n_> == r_, where n_ is unit.
			Concrete::UnitFloatPair n_;
			Concrete::Length r_;

			std::vector< const Concrete::Coords2D * > intersections_;

			SplicingLine( const Concrete::Coords2D & p0, const Concrete::Coords2D & p1_sub_p0, bool isTriangleSide );
			SplicingLine( const SplicingLine & orig );
			explicit SplicingLine( ); // Only to be used to create a destination object.

			Concrete::Coords2D intersection( const Computation::SplicingLine & other ) const;
			size_t nextLine( const Concrete::Coords2D * p, const Concrete::UnitFloatPair & n, const std::vector< Computation::SplicingLine > & lines ) const;
			Concrete::Length distanceTo( const Concrete::Coords2D p ) const;
		};

	}
}


#endif
