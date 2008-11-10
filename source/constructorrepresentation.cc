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

#include "constructorrepresentation.h"

#include <sstream>

using namespace Shapes;


std::string 
Helpers::shapesFormat( double scalar )
{
	std::ostringstream oss;

	if( scalar < 0 )
		{
			oss << "~" ;
		}
	static char buf[25];
	sprintf( buf, "%.5f", fabs( scalar ) );
	oss << buf ;
	return oss.str( );
}

std::string 
Helpers::shapesFormat( Concrete::Length length )
{
	std::ostringstream oss;

	double val = Concrete::Length::offtype( length );

	if( val < 0 )
		{
			oss << "~" ;
		}
	static char buf[25];
	sprintf( buf, "%.5fbp", fabs( val ) );
	oss << buf ;
	return oss.str( );
}

std::string 
Helpers::shapesFormat( Concrete::Coords2D coords )
{
	std::ostringstream oss;

	oss << "(" << Helpers::shapesFormat( coords.x_ ) << "," << Helpers::shapesFormat( coords.y_ ) << ")" ;

	return oss.str( );
}

std::string 
Helpers::shapesFormat( Concrete::Coords3D coords )
{
	std::ostringstream oss;

	oss << "(" << Helpers::shapesFormat( coords.x_ ) << "," << Helpers::shapesFormat( coords.y_ ) << "," << Helpers::shapesFormat( coords.z_ ) << ")" ;

	return oss.str( );
}

std::string 
Helpers::shapesFormat( Concrete::UnitFloatPair coords )
{
	std::ostringstream oss;

	oss << "(" << Helpers::shapesFormat( coords.x_ ) << "," << Helpers::shapesFormat( coords.y_ ) << ")" ;

	return oss.str( );
}

std::string 
Helpers::shapesFormat( Concrete::UnitFloatTriple coords )
{
	std::ostringstream oss;

	oss << "(" << Helpers::shapesFormat( coords.x_ ) << "," << Helpers::shapesFormat( coords.y_ ) << "," << Helpers::shapesFormat( coords.z_ ) << ")" ;

	return oss.str( );
}
