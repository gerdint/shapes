#ifndef constructorrepresentation_h
#define constructorrepresentation_h

#include "elementarylength.h"
#include "elementarycoords.h"

#include <string>

namespace MetaPDF
{
  namespace Helpers
  {
    
    std::string droolFormat( double scalar );
    std::string droolFormat( Concrete::Length length );
    std::string droolFormat( Concrete::Coords2D coords );
    std::string droolFormat( Concrete::Coords3D coords );
    std::string droolFormat( Concrete::UnitFloatPair coords );
    std::string droolFormat( Concrete::UnitFloatTriple coords );
    
  }
}

#endif
