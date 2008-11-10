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

#include "strrefdup.h"

RefCountPtr< const char >
strrefdup( const char * str )
{
	return RefCountPtr< const char >( strdup( str ) );
}

RefCountPtr< const char >
strrefdup( const std::string & str )
{
	return RefCountPtr< const char >( strdup( str.c_str( ) ) );
}

RefCountPtr< const char >
strrefdup( const std::ostringstream & str )
{
	return RefCountPtr< const char >( strdup( str.str( ).c_str( ) ) );
}

