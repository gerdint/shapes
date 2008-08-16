#include "ssiscanner.h"

#include <iostream>
#include <limits>

using namespace std;


SSIScanner::SSIScanner( bool onlyDependencies, istream * yyin, ostream * yyout, const char* includebase )
	: yyFlexLexer( yyin, yyout ), onlyDependencies_( onlyDependencies ), includebase_( new char[ strlen( includebase ) + 1 ] )
{
	depthLimitStack_.push( std::numeric_limits< size_t >::max( ) );
	metaInclusionStack_.push( true );
	strcpy( includebase_, includebase );
}

SSIScanner::~SSIScanner( )
{ }

void
SSIScanner::more( )
{
	yy_more_flag = 1; // This one is for flex, and will be reset before we reach doBeforeEachAction
}
