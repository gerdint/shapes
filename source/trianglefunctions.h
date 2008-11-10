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

#ifndef trianglefunctions_h
#define trianglefunctions_h


#include "elementarycoords.h"


namespace Shapes
{
	namespace Computation
	{

		Concrete::Coords2D triangleIncenter( const Concrete::Coords2D & p1, const Concrete::Coords2D & p2, const Concrete::Coords2D & p3 );
		Concrete::Area triangleArea( const Concrete::Coords2D & p1, const Concrete::Coords2D & p2, const Concrete::Coords2D & p3 );
		Concrete::Length triangleSemiPerimeter( const Concrete::Coords2D & p1, const Concrete::Coords2D & p2, const Concrete::Coords2D & p3 );
		Concrete::Coords3D triangleIncenter( const Concrete::Coords3D & p1, const Concrete::Coords3D & p2, const Concrete::Coords3D & p3 );
		Concrete::Area triangleArea( const Concrete::Coords3D & p1, const Concrete::Coords3D & p2, const Concrete::Coords3D & p3 );

	}
}


#endif
