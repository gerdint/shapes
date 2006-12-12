#include <cmath>

#include "angleselect.h"

double
angleSelectNorm2( double a1, double a2, double w1, double w2 )
{
  a1 = fmod( a1, 2 * M_PI );
  if( a1 < 0 )
    {
      a1 += 2 * M_PI;
    }
  a2 = fmod( a2, 2 * M_PI );
  while( a2 < a1 - M_PI )
    {
      a2 += 2 * M_PI;
    }
  while( a2 > a1 + M_PI )
    {
      a2 -= 2 * M_PI;
    }
  return ( w2 * a1 + w1 * a2 ) / ( w1 + w2 );
}

