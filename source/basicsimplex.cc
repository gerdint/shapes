#include <cmath>

#include "basicsimplex.h"

#include "autoonoff.h"

#include <vector>
#include <iostream> // For debugging

using namespace Shapes;

Computation::BasicSimplex::BasicSimplex( size_t nVars, size_t nEqns )
	: nVars_( nVars ), nEqns_( nEqns ),
		nExt_( nVars + nEqns ),
		a_( new double[ nEqns_ * ( nExt_ ) ] ),
		b_( new double[ nEqns_ ] ),
		c_( new double[ nExt_ ] ),
		varSet_( new size_t[ nExt_ ] ),
		nonBasicBegin_( varSet_ ),
		nonBasicEnd_( nonBasicBegin_ + nVars_ ),
		basicBegin_( nonBasicEnd_ ),
		basicEnd_( basicBegin_ + nEqns_ )
{ }

Computation::BasicSimplex::~BasicSimplex( )
{
	delete a_;
	delete b_;
	delete c_;
	delete varSet_;
}


// Call this function with preallocated memory for the result!
//
// It solves the problem of minimizing < c, *xdst > under the constraints
//	 *xdst >= 0
//	 a * *xdst <= b
// The constraints are stored in row major order, so consecutive memory
// belongs to the same constraint, not the same variable.
// The optimum is returned in *objdst, and the return value
// is true if optimization finished with *objdst < objGoal.
// If the return value is true, then the optimum is generally not located,
// but only a sufficiently good point.
bool
Computation::BasicSimplex::minimize( double * xdst,
																		 double * objdst, double objGoal,
																		 const double * c, const double * a, const double * b,
																		 bool changeSign ) const
{
	// Setup extended formulation with equality constraints and slack variables:
	
	bool infeasibleStart = false;

	{
		// Initialize c;
		double * end = c_ + nVars_;
		double * dst = c_;
		const double * src = c;
		if( changeSign )
			{
				for( ; dst != end; ++dst, ++src )
					{
						*dst = *src;
					}
			}
		else
			{
				for( ; dst != end; ++dst, ++src )
					{
						*dst = - *src;
					}
			}
		end += nEqns_;
		for( ; dst != end; ++dst )
			{
				*dst = 0;
			}
	}

	{
		// Initialize a and b
		{
			// First all of a is zeroed.
			double * end = a_ + nEqns_ * ( nExt_ );
			double * dst = a_;
			for( ; dst != end; ++dst )
				{
					*dst = 0;
				}
		}		

		const double * srcb = b;
		double * dstb = b_;
		for( size_t eq = 0; eq < nEqns_; ++eq, ++srcb, ++dstb )
			{
				*dstb = *srcb;
				if( *dstb < 0 )
					{
						infeasibleStart = true;
					}

				double * dst = a_ + eq * ( nExt_ );
				const double * src = a + eq * nVars_;
				const double * end = src + nVars_;
				for( ; src != end; ++src, ++dst )
					{
						*dst = *src;
					}
				dst += eq;
				*dst = 1;
			}
	}

	// The basic and non-basic sets contain indices to variables.
	// The basic set is organized so that the basic variable in equaiton i is located at ( basic + i ).

	{
		size_t i = 0;
		for( size_t * dst = nonBasicBegin_; dst != basicEnd_; ++dst, ++i )
			{
				*dst = i;
			}
	}

	double objective;
	if( infeasibleStart )
		{
			if( phaseOne( ) )
				{
					objective = phaseTwo( xdst, objGoal );
				}
			else
				{
					objective = HUGE_VAL;
				}
		}
	else
		{
			objective = phaseTwo( xdst, objGoal );
		}

	if( changeSign )
		{
			*objdst = - objective;
		}
	else
		{
			*objdst = objective;
		}
	return objective < objGoal;
}


bool
Computation::BasicSimplex::maximize( double * xdst,
																		 double * objdst, double objGoal,
																		 const double * c, const double * a, const double * b ) const
{
	return minimize( xdst,
									 objdst, -objGoal,
									 c, a, b,
									 true );
}


// Function to compute a feasible tableau.
bool
Computation::BasicSimplex::phaseOne( ) const
{
	std::cerr << "Simple phase one is not implemented!" << std::endl ;
	exit( 1 );
}

// Function for minimizing given a feasible initial tableau.
double
Computation::BasicSimplex::phaseTwo( double * xdst, double objGoal ) const
{
	double objective = 0;

	while( true )
		{

			if( false )
			{
				// Debug print.
				std::cerr << "Simplex tableau:" << std::endl ;
				for( size_t i = 0; i < 3 + nExt_ * 8 + 5; ++i )
					{
						std::cerr << "=" ;
					}
				std::cerr << std::endl ;
				std::cerr << "	|" ;
				for( size_t col = 0; col < nExt_; ++col )
					{
						std::cerr << "		" ;
						{
							bool found = false;
							for( const size_t * src = basicBegin_; src != basicEnd_; ++src )
								{
									if( *src == col )
										{
											found = true;
											break;
										}
								}
							if( found )
								{
									std::cerr << "+" ;
								}
							else
								{
									std::cerr << " " ;
								}
						}
						{
							bool found = false;
							for( const size_t * src = nonBasicBegin_; src != nonBasicEnd_; ++src )
								{
									if( *src == col )
										{
											found = true;
											break;
										}
								}
							if( found )
								{
									std::cerr << "-" ;
								}
							else
								{
									std::cerr << " " ;
								}
							std::cerr << "	" ;
						}
					}
				std::cerr << std::endl ;
				std::cerr << "	|" ;
				for( const double * src = c_; src != c_ + nExt_; ++src )
					{
						static char buf[10];
						sprintf( buf, "%7.3f", *src );
						std::cerr << buf << " " ;
					}
				std::cerr << "	| obj: " << objective << std::endl ;
				for( size_t i = 0; i < 3 + nExt_ * 8 + 5; ++i )
					{
						std::cerr << "-" ;
					}
				std::cerr << std::endl ;
				
				const double * srca = a_;
				for( size_t row = 0; row < nEqns_; ++row )
					{
						std::cerr << *( basicBegin_ + row ) << " |" ;
						const double * end = srca + nExt_;
						for( ; srca != end; ++srca )
							{
								static char buf[10];
								sprintf( buf, "%7.3f", *srca );
								std::cerr << buf << " " ;
							}
						std::cerr << "	| " << *( b_ + row ) << std::endl ;
					}
				for( size_t i = 0; i < 3 + nExt_ * 8 + 5; ++i )
					{
						std::cerr << "=" ;
					}
				std::cerr << std::endl ;				
				std::cerr << std::endl ;
			}

			// End of debug prints


			if( objective < objGoal )
				{
					goto finished;
				}

			// Pick a pivot column.	That is, the non-basic variable with the most positive coefficient
			size_t * nonBasicCol = 0;
			{
				double best = 0;
				for( size_t * src = nonBasicBegin_; src != nonBasicEnd_; ++src )
					{
						if( *( c_ + *src ) > best)
							{
								best = *( c_ + *src );
								nonBasicCol = src;
							}
					}
			}
			if( nonBasicCol == 0 )
				{
					goto finished;
				}

			size_t col = *nonBasicCol;

			// Find the outgoing row
			size_t row = nEqns_;
			{
				double thMin = HUGE_VAL;
				const double * srca = a_ + col;
				const double * srcb = b_;
				for( size_t i = 0; i < nEqns_; ++i, srca += nExt_, ++srcb	)
					{
						if( *srca > 0 )
							{
								double tmp = *srcb / *srca;
								if( tmp < thMin )
									{
										thMin = tmp;
										row = i;
									}
							}
					}
				if( row == nEqns_ )
					{
						// There is no constraining equation.

						// This return value is special, and does not correspond to the returned *dstx.
						objective = -HUGE_VAL;
						goto finished;
					}
			}

			{
				// Normalize the pivot
				double * dst = a_ + nExt_ * row;
				double tmp = 1. / *( dst + col );
				double * end = dst + nExt_;
				for( ; dst != end; ++dst )
					{
						*dst *= tmp;
					}
				*( b_ + row ) *= tmp;
			}

			{
				// Reduce other rows

				// Note that structural zeroes are kept track of using the basic/nonBasic distinction, and need not be enforced in a.

				double * dst = a_;
				double * enda = a_ + ( nExt_ * nEqns_ );
				const double * pivota = a_ + nExt_ * row;
				double * dstb = b_;
				double pivotb = *( b_ + row );
				for( ; dst != enda; ++dstb )
					{
						if( dst == pivota )
							{
								dst += nExt_;
								continue;
							}
						double * end = dst + nExt_;
						const double * src = pivota;
						double tmp = *( dst + col );
						for( ; dst != end; ++dst, ++src )
							{
								*dst -= tmp * *src;
							}
						*dstb -= tmp * pivotb;
					}

				{
					// "Reduce objective"
					const double * src = pivota;
					double * dst = c_;
					double * end = dst + nExt_;
					double tmp = *( dst + col );
					for( ; dst != end; ++dst, ++src )
						{
							*dst -= tmp * *src;
						}
					objective -= tmp * pivotb;
				}
			}

				
			{
				// Update the sets
				*nonBasicCol = *( basicBegin_ + row );
				*( basicBegin_ + row ) = col;
			}

		}

 finished:
	{
		double * dst = xdst;
		double * end = dst + nVars_;
		for( ; dst != end; ++dst )
			{
				*dst = 0;
			}
		const double * srcb = b_;
		for( const size_t * src = basicBegin_; src != basicEnd_; ++src, ++srcb )
			{
				if( *src < nVars_ )
					{
						*( xdst + *src ) = *srcb;
					}
			}
	}
	return objective;
}
