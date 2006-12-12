#include "constructorrepresentation.h"

#include <sstream>

using namespace MetaPDF;


std::string 
Helpers::droolFormat( double scalar )
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
Helpers::droolFormat( Concrete::Length length )
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
Helpers::droolFormat( Concrete::Coords2D coords )
{
  std::ostringstream oss;

  oss << "(" << Helpers::droolFormat( coords.x_ ) << "," << Helpers::droolFormat( coords.y_ ) << ")" ;

  return oss.str( );
}

std::string 
Helpers::droolFormat( Concrete::Coords3D coords )
{
  std::ostringstream oss;

  oss << "(" << Helpers::droolFormat( coords.x_ ) << "," << Helpers::droolFormat( coords.y_ ) << "," << Helpers::droolFormat( coords.z_ ) << ")" ;

  return oss.str( );
}

std::string 
Helpers::droolFormat( Concrete::UnitFloatPair coords )
{
  std::ostringstream oss;

  oss << "(" << Helpers::droolFormat( coords.x_ ) << "," << Helpers::droolFormat( coords.y_ ) << ")" ;

  return oss.str( );
}

std::string 
Helpers::droolFormat( Concrete::UnitFloatTriple coords )
{
  std::ostringstream oss;

  oss << "(" << Helpers::droolFormat( coords.x_ ) << "," << Helpers::droolFormat( coords.y_ ) << "," << Helpers::droolFormat( coords.z_ ) << ")" ;

  return oss.str( );
}
