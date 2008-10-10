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
