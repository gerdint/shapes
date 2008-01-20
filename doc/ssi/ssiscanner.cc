#include "ssiscanner.h"

#include <iostream>

using namespace std;


SSIScanner::SSIScanner( const char * filename, bool onlyDependencies, istream * yyin, ostream * yyout )
	: yyFlexLexer( yyin, yyout ), onlyDependencies_( onlyDependencies )
{
	filenameStack_.push( filename );
	depStack_.push( std::list< const char * >( ) );
}

SSIScanner::~SSIScanner( )
{ }

void
SSIScanner::more( )
{
	yy_more_flag = 1; // This one is for flex, and will be reset before we reach doBeforeEachAction
}
