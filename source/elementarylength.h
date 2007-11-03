#ifndef elementarylength_h
#define elementarylength_h

#include "physical.h"

namespace Shapes
{
  namespace Concrete
  {

    typedef Physical< 0, 0 > Scalar;
    typedef Physical< 1, 0 > Length;
    typedef Physical< 2, 0 > Area;
    
    typedef Physical< -1, 0 > ReciprocalLength;
    
    typedef Physical< 0, 1 > Time;
    typedef Physical< 1, -3 > Bezier;
    
    typedef Physical< 1, -1 > Speed;
    typedef Physical< 1, -2 > Acceleration;
    typedef Physical< 1, -3 > Jerk;

  }
}

#endif
