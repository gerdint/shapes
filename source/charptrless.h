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

#ifndef charptrless_h
#define charptrless_h

#include "refcount.h"

#include <string.h>

class charPtrLess
{
 public:
	bool operator () ( const char * s1, const char * s2 ) const
	{
		return strcmp( s1, s2 ) < 0;
	}
};

class charRefPtrLess
{
 public:
	bool operator () ( const RefCountPtr< const char > & s1, const RefCountPtr< const char > & s2 ) const
	{
		return strcmp( s1.getPtr( ), s2.getPtr( ) ) < 0;
	}
};



#endif
