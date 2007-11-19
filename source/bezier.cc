#include "bezier.h"

#include <complex>

void
Bezier::bezierRootsOfPolynomial( double t[2], double k0, double k1 )
{
	double * dst = & t[0];
		
	if( k1 != 0 )
		{
			double tmp = - k0 / k1;
			if( 0 <= tmp && tmp <= 1 )
				{
					*dst = tmp;
					++dst;
				}
		}
	*dst = HUGE_VAL;
}

void
Bezier::bezierRootsOfPolynomial( double t[3], double k0, double k1, double k2 )
{
	double * dst = & t[0];
		
	if( k2 == 0 )
		{
			if( k1 != 0 )
				{
					double tmp = - k0 / k1;
					if( 0 <= tmp && tmp <= 1 )
						{
							*dst = tmp;
							++dst;
						}
				}
		}
	else
		{
			k0 /= k2;
			k1 /= k2;
			double r2 = k1 * k1 * 0.25 - k0;
			if( r2 >= 0 )
				{
					double r = sqrt( r2 );
					{
						double tmp = - 0.5 * k1 - r;
						if( 0 <= tmp && tmp <= 1 )
							{
								*dst = tmp;
								++dst;
							}
					}
					{
						double tmp = - 0.5 * k1 + r;
						if( 0 <= tmp && tmp <= 1 )
							{
								*dst = tmp;
								++dst;
							}
					}
				}
		}
	*dst = HUGE_VAL;
}

void
Bezier::bezierRootsOfPolynomial( double t[4], double k0, double k1, double k2, double k3 )
{
	double * dst = & t[0];
	if( k3 == 0 )
		{
			if( k2 == 0 )
				{
					if( k1 == 0 )
						{
							// If we reach here, the equation is identically solved for all t.
							if( k0 == 0 )
								{
									*dst = 0;
									++dst;
								}
						}
					else
						{
							// Here k1 != 0
							double tmp = - k0 / k1;
							if( 0 <= tmp && tmp <= 1 )
								{
									*dst = tmp;
									++dst;
								}
						}
				}
			else
				{
					// Here k2 != 0
					k1 /= k2;
					k0 /= k2;
					double tmp_c = - 0.5 * k1;
					double r2 = tmp_c * tmp_c - k0;
					if( r2 >= 0 )
						{
							double r = sqrt( r2 );
							{
								double tmp = tmp_c - r;
								if( 0 <= tmp && tmp <= 1 )
									{
										*dst = tmp;
										++dst;
									}
							}
							{
								double tmp = tmp_c + r;
								if( 0 <= tmp && tmp <= 1 )
									{
										*dst = tmp;
										++dst;
									}
							}
						}
				}
			*dst = HUGE_VAL;
			return;
		}
			
	k2 /= k3;
	k1 /= k3;
	k0 /= k3;

	/* The solution that now follows is an implementation of the first cubic formula
	 * presented on MathWorld.
	 */

	typedef std::complex< double > Complex;

	Complex Q( ( 3 * k1 - k2 * k2 ) / 9, 0 );
	Complex R( 0.5 * ( ( 9 * k1 * k2 - 2 * k2 * k2 * k2 ) / 27 - k0 ), 0 );
	Complex r = sqrt( R * R + Q * Q * Q );

	Complex wCube;
	wCube = R + r;	// ( R - r ) is also an option
	Complex w1 = pow( wCube, 1./3 );
	Complex w2 = w1 * exp( Complex( 0, 2 * M_PI / 3 ) );
	Complex w3 = w1 * exp( Complex( 0, -2 * M_PI / 3 ) );
			
	Complex t1 = w1 - Q / w1 - k2 / 3;
	Complex t2 = w2 - Q / w2 - k2 / 3;
	Complex t3 = w3 - Q / w3 - k2 / 3;

	const double IMAG_TOL = 1e-10; // This is "relative" to the interesting t-range being 0..1
	if( fabs( t1.imag( ) ) < IMAG_TOL )
		{
			double tmp = t1.real( );
			if( 0 <= tmp && tmp <= 1 )
				{
					*dst = tmp;
					++dst;
				}
		}
	if( fabs( t2.imag( ) ) < IMAG_TOL )
		{
			double tmp = t2.real( );
			if( 0 <= tmp && tmp <= 1 )
				{
					*dst = tmp;
					++dst;
				}
		}
	if( fabs( t3.imag( ) ) < IMAG_TOL )
		{
			double tmp = t3.real( );
			if( 0 <= tmp && tmp <= 1 )
				{
					*dst = tmp;
					++dst;
				}
		}
	*dst = HUGE_VAL;
}
