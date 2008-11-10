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

#ifndef utf8tools_h
#define utf8tools_h


namespace Shapes
{
	namespace Helpers
	{
		inline bool utf8leadByte( char c )
		{
			return ! ( 0x80 <= *reinterpret_cast< unsigned char * >( & c ) && *reinterpret_cast< unsigned char * >( & c ) <= 0xBF );
		}
		inline bool utf8leadByte( unsigned char c )
		{
			return ! ( 0x80 <= c && c <= 0xBF );
		}
	}
}


#endif
