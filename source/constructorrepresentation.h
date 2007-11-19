#ifndef constructorrepresentation_h
#define constructorrepresentation_h

#include "elementarylength.h"
#include "elementarycoords.h"

#include <string>

namespace Shapes
{
	namespace Helpers
	{
		
		std::string shapesFormat( double scalar );
		std::string shapesFormat( Concrete::Length length );
		std::string shapesFormat( Concrete::Coords2D coords );
		std::string shapesFormat( Concrete::Coords3D coords );
		std::string shapesFormat( Concrete::UnitFloatPair coords );
		std::string shapesFormat( Concrete::UnitFloatTriple coords );
		
	}
}

#endif
