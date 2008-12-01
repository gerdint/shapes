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

#include "texscanner.h"
#include "strrefdup.h"

#include <sstream>


TeXScanner::TeXScanner( )
	: yyFlexLexer( 0, 0 )
{ }

void
TeXScanner::check( const std::string & str )
{
	delimStack.clear( );
	std::istringstream is( str );
	switch_streams( & is, 0 );
	yylex( );
}

const char *
TeXScanner::opening( TeXScanner::Delimiter delim ) const
{
	switch( delim )
		{
		case BRACE:
			return "{";
		case BRACKET:
			return "[";
		case BEGIN_END:
			return "\begin{...}";
		default:
			throw "TeXScanner::opening: Delimiter out of range.";
		}
}

const char *
TeXScanner::closing( TeXScanner::Delimiter delim ) const
{
	switch( delim )
		{
		case BRACE:
			return "}";
		case BRACKET:
			return "]";
		case BEGIN_END:
			return "\end{...}";
		default:
			throw "TeXScanner::closing: Delimiter out of range.";
		}
}
