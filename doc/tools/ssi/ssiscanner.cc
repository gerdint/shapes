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

#include "ssiscanner.h"

#include <iostream>
#include <limits>

using namespace std;


SSIScanner::SSIScanner( bool onlyDependencies, const string & initDir, const vector< string > & includePath, istream * yyin, ostream * yyout )
	: yyFlexLexer( yyin, yyout ),
		onlyDependencies_( onlyDependencies ), includePath_( includePath )
{
	depthLimitStack_.push( std::numeric_limits< size_t >::max( ) );
	metaInclusionStack_.push( true );
	dirStack_.push( initDir );
}

SSIScanner::~SSIScanner( )
{ }

void
SSIScanner::more( )
{
	yy_more_flag = 1; // This one is for flex, and will be reset before we reach doBeforeEachAction
}
