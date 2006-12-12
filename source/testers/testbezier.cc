#include "bezier.h"

#include <iostream>

int
main( int argc, char ** argv )
{
  Bezier::PolyCoeffs< double > poly1( 1, 3, 2, 2 );
  std::cout << poly1 << std::endl ;
  Bezier::ControlPoints< double > ctrls( poly1 );
  std::cout << ctrls << std::endl ;
  Bezier::PolyCoeffs< double > poly2( ctrls );
  std::cout << poly2 << std::endl ;
  std::cout << "Mid section: " ;
  Bezier::PolyCoeffs< double > poly3( poly1.subSection( 0.3, 0.7 ) );
  std::cout << poly3 << std::endl ;
  return 0;
}

