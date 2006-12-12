#ifndef angleselect_h
#define angleselect_h

/* angleSelectNorm2 minimizes a linear combination of the quadratic angle differences.
 * It does so by first bringing a1 to the interval [ 0, 2pi ], and then bringing the
 * second angle to the range a1 + [ -pi, pi ], and then performing the minimization
 * without the modulo operation involved.
 */
double
angleSelectNorm2( double a1, double a2, double w1, double w2 );


#endif
