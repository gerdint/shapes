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
      // The line is defined in several ways.  First, by p0_ -- (p0_+lengt_*d_), where d_ is unit:
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
