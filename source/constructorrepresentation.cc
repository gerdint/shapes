#include "constructorrepresentation.h"

#include <sstream>

using namespace Shapes;


std::string 
Helpers::shapesFormat( double scalar )
{
	std::ostringstream oss;

	if( scalar < 0 )
		{
			oss << "~" ;
		}
	static char buf[25];
	sprintf( buf, "%.5f", fabs( scalar ) );
	oss << buf ;
	return oss.str( );
}

std::string 
Helpers::shapesFormat( Concrete::Length length )
{
	std::ostringstream oss;

	double val = Concrete::Length::offtype( length );
	
	if( val < 0 )
		{
			oss << "~" ;
		}
	static char buf[25];
	sprintf( buf, "%.5fbp", fabs( val ) );
	oss << buf ;
	return oss.str( );
}

std::string 
Helpers::shapesFormat( Concrete::Coords2D coords )
{
	std::ostringstream oss;

	oss << "(" << Helpers::shapesFormat( coords.x_ ) << "," << Helpers::shapesFormat( coords.y_ ) << ")" ;

	return oss.str( );
}

std::string 
Helpers::shapesFormat( Concrete::Coords3D coords )
{
	std::ostringstream oss;

	oss << "(" << Helpers::shapesFormat( coords.x_ ) << "," << Helpers::shapesFormat( coords.y_ ) << "," << Helpers::shapesFormat( coords.z_ ) << ")" ;

	return oss.str( );
}

std::string 
Helpers::shapesFormat( Concrete::UnitFloatPair coords )
{
	std::ostringstream oss;

	oss << "(" << Helpers::shapesFormat( coords.x_ ) << "," << Helpers::shapesFormat( coords.y_ ) << ")" ;

	return oss.str( );
}

std::string 
Helpers::shapesFormat( Concrete::UnitFloatTriple coords )
{
	std::ostringstream oss;

	oss << "(" << Helpers::shapesFormat( coords.x_ ) << "," << Helpers::shapesFormat( coords.y_ ) << "," << Helpers::shapesFormat( coords.z_ ) << ")" ;

	return oss.str( );
}
