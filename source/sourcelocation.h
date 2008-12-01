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

#ifndef sourcelocation_h
#define sourcelocation_h

#include <stddef.h>
#include <iostream>

namespace Shapes
{
	namespace Ast
	{
		class SourceLocation
		{
			static const char * UNKNOWN_FILENAME;
		public:
			const char * filename;
			size_t firstLine;
			size_t firstColumn; // In bytes, shall be converted to utf-8 position when displaying.
			size_t lastLine;
			size_t lastColumn; // In bytes, shall be converted to utf-8 position when displaying.

			SourceLocation( );
			SourceLocation( const char * _filename );
			SourceLocation( const SourceLocation & orig );
			SourceLocation( const SourceLocation & firstLoc, const Ast::SourceLocation & lastLoc );
			bool isUnknown( ) const;
			bool contains( const SourceLocation & loc2 ) const;
			friend std::ostream & operator << ( std::ostream & os, const SourceLocation & self );

			void copy( std::ostream * os ) const;

			static size_t byteColumnToUTF8Column( const char * filename, size_t line, size_t byteCol );
			static size_t byteColumnToUTF8Column( const std::string & line, size_t byteCol );
		};

		extern SourceLocation THE_UNKNOWN_LOCATION;
	}
}

#endif
