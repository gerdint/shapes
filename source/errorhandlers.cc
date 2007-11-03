#include "errorhandlers.h"

using namespace Shapes;


Lang::ErrorHandler::ErrorHandler( )
  : Lang::Function( new Kernel::EvaluatedFormals( "<error handler>", false ) )
{ }

Lang::ErrorHandler::~ErrorHandler( )
{ }

bool
Lang::ErrorHandler::isTransforming( ) const
{
  return false;
}

