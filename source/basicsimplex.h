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
