#include "physical.h"

std::ostream &
  operator << ( std::ostream & os, const Physical< 0, 0 > & p )
{
  os << p.value_ ;
  return os;
}
