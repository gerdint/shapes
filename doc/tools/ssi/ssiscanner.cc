#include "ssiscanner.h"

#include <iostream>

using namespace std;


SSIScanner::SSIScanner( bool onlyDependencies, istream * yyin, ostream * yyout )
	: yyFlexLexer( yyin, yyout ), onlyDependencies_( onlyDependencies )
{ }

SSIScanner::~SSIScanner( )
{ }

void
SSIScanner::more( )
{
	yy_more_flag = 1; // This one is for flex, and will be reset before we reach doBeforeEachAction
}
