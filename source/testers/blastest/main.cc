#include <iostream>

#define DGESVD dgesvd
#include "clapack.h"

// From clapack.h:

// /* Subroutine */
// int dgesvd_( char *jobu, char *jobvt, __CLPK_integer *m, __CLPK_integer *n, 
// 	     __CLPK_doublereal *a, __CLPK_integer *lda,
// 	     __CLPK_doublereal *s,
// 	     __CLPK_doublereal *u, __CLPK_integer *ldu,
// 	     __CLPK_doublereal *vt, __CLPK_integer *ldvt,
// 	     __CLPK_doublereal *work, __CLPK_integer *lwork, 
// 	     __CLPK_integer *info);

// For the particular input used in this test, reference implementation values are obtained from Matlab:

// >> [ U, S, V ] = svd( reshape( 1:9, 3, [] ) );
// >> U                                          

// U =

//    -0.4797    0.7767    0.4082
//    -0.5724    0.0757   -0.8165
//    -0.6651   -0.6253    0.4082

// >> diag( S )                                  

// ans =

//    16.8481
//     1.0684
//     0.0000

// >> V                                          

// V =

//    -0.2148   -0.8872   -0.4082
//    -0.5206   -0.2496    0.8165
//    -0.8263    0.3879   -0.4082

// >> 



void displayArray( std::ostream & os, const double * pr, size_t m, size_t n )
{
  size_t r;
  size_t c;
  char buf[20];
  for( r = 0; r < m; ++r )
    {
      for( c = 0; c < n; ++c )
        {
          sprintf( buf, "%14.5e", *( pr + ( r + c * m ) ) );
	  os << buf ;
        }
      os << std::endl ;
    }
}


int
main( int argc, char ** argv )
{
  const size_t N = 3;
  __CLPK_integer mn = N;
  
  char jobuvt = 'A';

  double a[ N * N ] = { 1, 2, 3, 4, 5, 6, 7, 8, 9 };
  double u[ N * N ] = { 1, 2, 3, 4, 5, 6, 7, 8, 9 };
  double vt[ N * N ] = { 1, 2, 3, 4, 5, 6, 7, 8, 9 };
  __CLPK_integer ldauvt = N;

  double s[ N ];
  
  __CLPK_integer lwork;
  __CLPK_integer info;

  __CLPK_doublereal * work = 0;
  {  
    double tmpwork;
    lwork = -1;
    DGESVD( & jobuvt, & jobuvt,
	    & mn, & mn,
	    reinterpret_cast< __CLPK_doublereal * >( & a ), & ldauvt,
	    reinterpret_cast< __CLPK_doublereal * >( & s ),
	    reinterpret_cast< __CLPK_doublereal * >( & u ), & ldauvt,
	    reinterpret_cast< __CLPK_doublereal * >( & vt ), & ldauvt,
	    reinterpret_cast< __CLPK_doublereal * >( & tmpwork ), & lwork,
	    & info );
    lwork = static_cast< __CLPK_integer >( tmpwork );
    std::cout << "Optimal lwork: " << lwork << std::endl ;
    work = new __CLPK_doublereal[ lwork ];
  }

  std::cout << "a before:" << std::endl ;
  displayArray( std::cout, a, mn, mn );

  DGESVD( & jobuvt, & jobuvt,
	  & mn, & mn,
	  reinterpret_cast< __CLPK_doublereal * >( & a ), & ldauvt,
	  reinterpret_cast< __CLPK_doublereal * >( & s ),
	  reinterpret_cast< __CLPK_doublereal * >( & u ), & ldauvt,
	  reinterpret_cast< __CLPK_doublereal * >( & vt ), & ldauvt,
	  reinterpret_cast< __CLPK_doublereal * >( work ), & lwork,
	  & info );
  
  if( info != 0 )
    {
      std::cerr << "LAPACK routine DGESVD failed." << std::endl ;
      exit( 1 );
    }


  std::cout << "a after:" << std::endl ;
  displayArray( std::cout, a, mn, mn );

  std::cout << "s:" << std::endl ;
  displayArray( std::cout, s, mn, 1 );

  std::cout << "u:" << std::endl ;
  displayArray( std::cout, u, mn, mn );

  std::cout << "vt:" << std::endl ;
  displayArray( std::cout, vt, mn, mn );

  if( work != 0 )
    {
      delete work;
      work = 0;
    }

  return 0;
}
