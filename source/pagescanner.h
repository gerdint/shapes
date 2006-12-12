#ifndef pagescanner_h
#define pagescanner_h

#include <FlexLexer.h>

class PageScanner : public yyFlexLexer
{
  double ytmp;
  int state;
 public:
  double x0;
  double y0;
  double height;
  double depth;
  double width;
  PageScanner( std::istream * yyin = 0, std::ostream * yyout = 0 );
  virtual int yylex( );
};

enum { T_cm = 256, T_m, T_l };



#endif
