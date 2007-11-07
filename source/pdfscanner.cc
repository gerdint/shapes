#include "pdfscanner.h"

using namespace std;

PdfScanner::PdfScanner( istream * yyin, ostream * yyout )
  : yyFlexLexer( yyin, yyout )
{ }
