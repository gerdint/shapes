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

#include <cmath>

#include "angleselect.h"

double
angleSelectNorm2( double a1, double a2, double w1, double w2 )
{
	a1 = fmod( a1, 2 * M_PI );
	if( a1 < 0 )
		{
			a1 += 2 * M_PI;
		}
	a2 = fmod( a2, 2 * M_PI );
	while( a2 < a1 - M_PI )
		{
			a2 += 2 * M_PI;
		}
	while( a2 > a1 + M_PI )
		{
			a2 -= 2 * M_PI;
		}
	return ( w2 * a1 + w1 * a2 ) / ( w1 + w2 );
}

