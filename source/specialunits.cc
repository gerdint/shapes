#include <cmath>

#include "specialunits.h"
#include "consts.h"
#include "isnan.h"

#include <iostream> /* I get size_t from here... */
#include <limits>

using namespace Shapes;

double
Computation::specialUnitCircleHandle( double a0 )
{
	/* a0 is half the angle of the circular arc segment.
	 */
	double t = a0 / Computation::RREL_TH_STEP;
	size_t i = static_cast< size_t >( t );
	t -= static_cast< double >( i );
	return ( 1 - t ) * Computation::RREL_TABLE[ i ] + t * Computation::RREL_TABLE[ i + 1 ];
}

double
Computation::specialUnitCorrection( double a0, double a1 )
{
	const double k2 = -2.0729490168751576;
	const double k3 = -1.381966011250105;
	const double wInv = 0.7821424388422419;
	
	if( a0 == a1 )
		{
			return 1;
		}

	double aRel = wInv * ( a1 - a0 ) / a0 ;
	double x = 2 * exp( aRel ) / ( 1 + exp( aRel ) ) - 1;
	if( IS_NAN( x ) )
		{
			if( a1 > 0 )
				{
					x = 1;
				}
			else
				{
					x = -1;
				}
		}
			
	double xSquare = x * x;
	if( aRel < 0 )
		{
			return 1 + k2 * xSquare + k3 * x * xSquare;
		}
	return 1 + 3 * xSquare - 2 * x * xSquare;
}

double
Computation::specialUnitNoInflexion( double a0, double a1 )
{
	{
		/* tmp is the opposite handles angle.	It will be in the range [ -\pi, 2\pi ], so the
		 * handle rays will intersect iff tmp \in [ 0, \pi - a0 ]
		 */
		double tmp = a0 + a1;
		if( tmp < 0 ||
				tmp > M_PI - a0 )
			{
				return HUGE_VAL;				
			}
	}
	return sin( a0 + a1 ) / sin( a0 + a0 + a1 );
}

