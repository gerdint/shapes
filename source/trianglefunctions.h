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
