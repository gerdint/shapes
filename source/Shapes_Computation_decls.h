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
