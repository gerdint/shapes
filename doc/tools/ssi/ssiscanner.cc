#include "ssiscanner.h"

#include <iostream>
#include <limits>

using namespace std;


SSIScanner::SSIScanner( bool onlyDependencies, const string & initDir, const vector< string > & includePath, istream * yyin, ostream * yyout )
	: yyFlexLexer( yyin, yyout ),
		onlyDependencies_( onlyDependencies ), includePath_( includePath )
{
	depthLimitStack_.push( std::numeric_limits< size_t >::max( ) );
	metaInclusionStack_.push( true );
	dirStack_.push( initDir );
}

SSIScanner::~SSIScanner( )
{ }

void
SSIScanner::more( )
{
	yy_more_flag = 1; // This one is for flex, and will be reset before we reach doBeforeEachAction
}
