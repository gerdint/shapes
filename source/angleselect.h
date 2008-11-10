/* This file is part of Shapes.
 *
 * Shapes is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * any later version.
 *
 * Shapes is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with Shapes.  If not, see <http://www.gnu.org/licenses/>.
 *
 * Copyright 2008 Henrik Tidefelt
 */

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
