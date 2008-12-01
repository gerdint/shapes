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

#ifndef Shapes_Computation_decls_h
#define Shapes_Computation_decls_h

#include <cstddef> // For size_t

namespace Shapes
{
	namespace Computation
	{

		class IntersectionSegmentSections2D;

		class PaintedPolygon3D;
		class SplicingLine;
		class StrokedLine3D;

		typedef char FacetShadeOrder;

		class FacetInterpolatorGray;
		class FacetInterpolatorRGB;

		class BasicSimplex;

		class Upsampler2D;
		class Upsampler3D;
	}
}

#endif
