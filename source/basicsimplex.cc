#include <cmath>

#include "MetaPDF_Computation_decls.h"

#include "autoonoff.h"

#include <vector>

using namespace MetaPDF;

// Call this function with preallocated memory for the result!
//
// It solves the problem of minimizing < c, *xdst > under the constraints
//   *xdst >= 0
//   a * *xdst <= b
// The constraints are stored in row major order, so consecutive memory
// belongs to the same constraint, not the same variable.
double
Computation::basicsimplex( size_t nVars, size_t nEqns,
			   double * xdst,
			   const double * _c, const double * _a, const double * _b )
{
  // Setup extended formulation with equality constraints and slack variables:
  
  double * a = new double[ nEqns * ( nVars + nEqns ) ];
  DeleteOnExit< double > aCleaner( a );

  double * b = new double[ nEqns ];
  DeleteOnExit< double > bCleaner( b );

  double * c = new double[ nVars + nEqns ];
  DeleteOnExit< double > cCleaner( c );
  
  {
    // Initialize c;
    double * end = c + nVars;
    double * dst = c;
    const double * src = _c;
    for( ; dst != end; ++dst )
      {
	*dst = *_c;
      }
    end += nEqns;
    for( ; dst != end; ++dst )
      {
	*dst = 0;
      }
  }

  {
    // Initialize a and b
    {
      // First all of a is zeroed.
      double * end = a + nEqns * ( nVars + nEqns );
      double * dst = a;
      for( ; dst != end; ++dst )
	{
	  *dst = 0;
	}
    }    

    const double * srcb = _b;
    double * dstb = b;
    for( size_t eq = 0; eq < nEqns; ++eq, ++srcb, ++dstb )
      {
	*dstb = *b;

	double * dst = a + eq * ( nVars + nEqns );
	const double * src = _a + eq * nVars;
	const double * end + nVars;
	for( ; src != end; ++src, ++dst )
	  {
	    *dst = *src;
	  }
	
      }
  }

  const double * cEnd = c + nVars + nEqns;

  size_t * basic = new size_t[ nVars ];
  DeleteOnExit< size_t > basicCleaner( basic );
  {
    size_t i = 0;
    for( size_t * dst = basic; dst != basic + nVars; ++dst, ++i )
      {
	*dst = i;
      }
  }

  while( true )
    {
      bool finished = true;
      for( const double * src = c; src != cEnd; ++src )
	{
	  if( *src < 0 )
	    {
	      finished = false;
	      break;
	    }
	}
      if( finished )
	{
	  double res = 0;
	  for( const double * src = c; src != cEnd; ++src, ++dstx )
	    {
	      *dstx = FIX ME!!!;
	      res += *src * *dstx;
	    }
	  
	  return res;
	}

      // Pick a pivot.

      // Make this variable basic.

    }
}
