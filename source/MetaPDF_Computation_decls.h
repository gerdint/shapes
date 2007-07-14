#ifndef MetaPDF_Computation_decls_h
#define MetaPDF_Computation_decls_h

#include <cstddef> // For size_t

namespace MetaPDF
{
  namespace Computation
  {

    class IntersectionSegmentSections2D;

    class PaintedPolygon3D;
    class SplicingLine;
    class StrokedLine3D;
  
    typedef char FacetShadeOrder;

    class FacetInterpolatorGray;

    class BasicSimplex;

    class Upsampler2D;
    class Upsampler3D;
  }
}

#endif
