#ifndef pdfscanner_h
#define pdfscanner_h

#include "pdfstructure.h"

#ifndef FLEXINT_H		// Else xmlFlexLexer will be defined twice
#undef yyFlexLexer
/// FLEX defined lexer
#define yyFlexLexer pdfFlexLexer
#include <FlexLexer.h>
#endif

class PdfScanner : public pdfFlexLexer
{
 public:
  typedef union
  {
    SimplePDF::PDF_Object * pdfObj;
    SimplePDF::PDF_Indirect_in * pdfR;
    char * str;
    long intVal;
  } UnionType;
  UnionType yylval;
  PdfScanner( std::istream * yyin = 0, std::ostream * yyout = 0 );
  virtual int yylex( );
};

enum { T_Constant = 256, T_obj, T_R, T_endobj, T_stream, T_endstream, T_OpenDic, T_CloseDic, T_String, T_Name };

#endif
