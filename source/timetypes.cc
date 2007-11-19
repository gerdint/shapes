#include "timetypes.h"
#include "elementarytypes.h"
#include "environment.h"
#include "classtypes.h"

using namespace Shapes;


Lang::ChronologicalTime::ChronologicalTime( time_t t )
	: t_( t )
{ }

Lang::ChronologicalTime::~ChronologicalTime( )
{ }

RefCountPtr< const Lang::Class > Lang::ChronologicalTime::TypeID( new Lang::SystemFinalClass( strrefdup( "Time" ) ) );
TYPEINFOIMPL( ChronologicalTime );

const struct tm *
Lang::ChronologicalTime::temporary_localtime( ) const
{
	return localtime( & t_ );
}

Kernel::VariableHandle
Lang::ChronologicalTime::getField( const char * fieldID, const RefCountPtr< const Lang::Value > & selfRef ) const
{
	const struct tm * tmp = localtime( & t_ );
	if( strcmp( fieldID, "year" ) == 0 )
		{
			return Helpers::newValHandle( new Lang::Integer( 1900 + tmp->tm_year ) );
		}
	if( strcmp( fieldID, "month" ) == 0 )
		{
			return Helpers::newValHandle( new Lang::Integer( 1 + tmp->tm_mon ) );
		}
	if( strcmp( fieldID, "dayOfMonth" ) == 0 )
		{
			return Helpers::newValHandle( new Lang::Integer( 1 + tmp->tm_mday ) );
		}
	if( strcmp( fieldID, "hour" ) == 0 )
		{
			return Helpers::newValHandle( new Lang::Integer( tmp->tm_hour ) );
		}
	if( strcmp( fieldID, "minute" ) == 0 )
		{
			return Helpers::newValHandle( new Lang::Integer( tmp->tm_min ) );
		}
	if( strcmp( fieldID, "second" ) == 0 )
		{
			return Helpers::newValHandle( new Lang::Integer( tmp->tm_sec ) );
		}
	if( strcmp( fieldID, "dayOfWeek" ) == 0 )
		{
			return Helpers::newValHandle( new Lang::Integer( tmp->tm_wday ) );
		}
	throw Exceptions::NonExistentMember( getTypeName( ), fieldID );	
}

void
Lang::ChronologicalTime::show( std::ostream & os ) const
{
	const struct tm * tmp = localtime( & t_ );
	const size_t BUF_SIZE = 30;
	char buf[ BUF_SIZE ];
	strftime( reinterpret_cast< char * >( & buf ), BUF_SIZE, "%Y-%m-%d %H:%M:%S", tmp );
	os << buf ;
}
