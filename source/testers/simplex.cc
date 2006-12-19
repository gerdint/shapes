#include <cmath>

#include "autoonoff.h"

#include <iostream>
#include <string>

#include "../basicsimplex.h"

    
int
main( int argc, char ** argv )
{
  size_t nVars;
  size_t nEqns;
  std::cin >> nVars >> nEqns ;

  double * a = new double[ nEqns * ( nVars ) ];
  DeleteOnExit< double > aCleaner( a );

  double * b = new double[ nEqns ];
  DeleteOnExit< double > bCleaner( b );

  double * c = new double[ nVars ];
  DeleteOnExit< double > cCleaner( c );
  
  for( size_t col = 0; col < nVars; ++col )
    {
      std::cin >> *( c + col ) ;
    }

  for( size_t row = 0; row < nEqns; ++row )
    {
      for( size_t col = 0; col < nVars; ++col )
	{
	  std::cin >> *( a + nVars * row + col ) ;
	}
      std::cin >> *( b + row ) ;
    }

  double * x = new double[ nVars ];
  DeleteOnExit< double > xCleaner( x );

  MetaPDF::Computation::BasicSimplex solver( nVars, nEqns );

  double res;
  solver.minimize( x,
		   & res, -HUGE_VAL,
		   c, a, b );
  
  std::cout << "Optimal value: " << res << std::endl ;
  std::cout << "Optimum: " ;
  {
    const double * end = x + nVars;
    for( const double * src = x; src != end; ++src )
      {
	std::cout << *src << "  " ;
      }
  }
  std::cout << std::endl ;

  return 0;
}
