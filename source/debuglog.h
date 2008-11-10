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

#ifndef debuglog_h
#define debuglog_h

#include <iostream>
#include <fstream>
#include <string>


namespace Shapes
{
	namespace Kernel
	{
		class DebugLog
		{
			std::ostream * os_;
			std::string filename_;
			std::ofstream myFile_;
		public:
			DebugLog( );
			~DebugLog( );
			bool initialized( ) const;
			void setStream( std::ostream * os );
			void setFilename( const std::string & filename );
			std::ostream & os( );
		};
	}
}


#endif
