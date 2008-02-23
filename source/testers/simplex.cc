#include <cmath>

#include "autoonoff.h"
#include "dtforth.h"
#include "dtforthtypes.h"

#include <iostream>
#include <sstream>
#include <string>

#include "../basicsimplex.h"


int
main( int argc, char ** argv )
{
  {
    // The file begins with a comment, that ends after the first line beginning with an equal sign.

    char dummy;
    std::cin.get( dummy );
    for( ; dummy == '\n'; std::cin.get( dummy ) )
      ;
    while( dummy != '=' )
      {
	for( std::cin.get( dummy ); dummy != '\n'; std::cin.get( dummy ) )
	  ;
	for( ; dummy == '\n'; std::cin.get( dummy ) )
	  ;
      }

    // Skip the final comment row.
    for( std::cin.get( dummy ); dummy != '\n'; std::cin.get( dummy ) )
      ;
  }

  size_t nVars;
  size_t nEqns;
  std::cin >> nVars >> nEqns ;

  double * a = new double[ nEqns * ( nVars ) ];
  DeleteOnExit< double > aCleaner( a );

  double * b = new double[ nEqns ];
  DeleteOnExit< double > bCleaner( b );

  double * c = new double[ nVars ];
  DeleteOnExit< double > cCleaner( c );

  DTForth myForth( 10000 );

  try
    {
      std::string line;
      std::getline( std::cin, line );
      if( line.empty( ) )
	{
	  // After reading nEqns, the trailing newline was not eaten up.
	  std::getline( std::cin, line );
	}
      myForth << line ;
      for( size_t col = nVars; col > 0; --col )
	{
	  std::ostringstream tmp;
	  tmp << "Objective gradient, variable " << col ;
	  *( c + col - 1 ) = myForth.pop< ForthFloat >( tmp.str( ).c_str( ) );
	}
      if( myForth.stack.depth( ) > 0 )
	{
	  std::cerr << "Too many elements in objective gradient." << std::endl ;
	  exit( 1 );
	}
      
      for( size_t row = 0; row < nEqns; ++row )
	{
	  std::getline( std::cin, line );
	  myForth << line ;
	  {
	    std::ostringstream tmp;
	    tmp << "Constraint " << row + 1 << ", RHS" ;
	    *( b + row ) = myForth.pop< ForthFloat >( tmp.str( ).c_str( ) );
	  }
	  for( size_t col = nVars; col > 0; --col )
	    {
	      std::ostringstream tmp;
	      tmp << "Constraint " << row + 1 << ", variable " << col ;
	      *( a + nVars * row + col - 1 ) = myForth.pop< ForthFloat >( tmp.str( ).c_str( ) );
	    }
	  if( myForth.stack.depth( ) > 0 )
	    {
	      std::cerr << "Too many elements on constraint row " << row + 1 << "." << std::endl ;
	      exit( 1 );
	    }
	}
    }
  catch( ForthErr& msg )
    {
      std::cerr << "Oups!" << std::endl
		<< "\t" << msg.msg << std::endl ;
      exit( 1 );
    }
  catch( char * msg )
    {
      std::cerr << "Caught char * : " << msg << std::endl ;
      exit( 1 );
    }
  catch( std::string & msg )
    {
      std::cerr << "Caught string: " << msg << std::endl ;
      exit( 1 );
    }
  catch( std::exception & msg )
    {
      std::cerr << "Exception: " << msg.what() << std::endl ;
      exit( 1 );
    }
  catch(...)
    {
      std::cerr << "Unknown exception caugt in main." << std::endl ;
      exit( 1 );
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
