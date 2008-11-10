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

#include "debuglog.h"
#include "shapesexceptions.h"

using namespace Shapes;

Kernel::DebugLog::DebugLog( )
	: os_( 0 )
{ }

Kernel::DebugLog::~DebugLog( )
{ }

bool
Kernel::DebugLog::initialized( ) const
{
	return os_ != 0 || ! filename_.empty( );
}

void
Kernel::DebugLog::setStream( std::ostream * os )
{
	if( initialized( ) )
		{
			throw "Multiply specified debug log";
		}
	os_ = os;
}

void
Kernel::DebugLog::setFilename( const std::string & filename )
{
	if( initialized( ) )
		{
			throw "Multiply specified debug log";
		}
	filename_ = filename;
}

std::ostream &
Kernel::DebugLog::os( )
{
	if( os_ == 0 )
		{
			if( filename_.empty( ) )
				{
					throw Exceptions::InternalError( "The debug log filename was not initialized before it was needed." );
				}
			myFile_.open( filename_.c_str( ) );
			if( ! myFile_.is_open( ) )
				{
					throw Exceptions::FileWriteOpenError( Ast::THE_UNKNOWN_LOCATION, strrefdup( filename_ ), "(debug log)" );
				}
			os_ = & myFile_;
		}
	return *os_;
}
