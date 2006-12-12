#include "pagescanner.h"

using namespace std;

PageScanner::PageScanner( istream * yyin, ostream * yyout )
  : yyFlexLexer( yyin, yyout ), state( 0 )
{ }
