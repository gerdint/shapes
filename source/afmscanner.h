#ifndef afmscanner_h
#define afmscanner_h

#include "fontmetrics.h"
#include "characterencoding.h"

#ifndef FLEXINT_H		// Else xmlFlexLexer will be defined twice
#undef yyFlexLexer
/// FLEX defined lexer
#define yyFlexLexer afmFlexLexer
#include <FlexLexer.h>
#endif

class AfmScanner : public afmFlexLexer
{
  FontMetrics::BaseFont * fontMetricsDst_;
  FontMetrics::WritingDirectionMetrics * currentDirectionDst_;
  int currentDirectionID_; // This corresponds to currentDirectionDst_ and is initially -1, indicating not initialized
  int activateDirectionID_; // This is initially 0, and is changed by StartDirection and EndDirection
  unsigned char metricsSets_;
  FontMetrics::CharacterMetrics * currentCharacter_;
  FontMetrics::AFM::KernPairMap * currentKernPairMapX_;
  FontMetrics::AFM::KernPairMap * currentKernPairMapY_;
  const std::map< RefCountPtr< const char >, size_t, charRefPtrLess > * currentNameMap_;
  bool tellQue_; // write things we don't understand on stderr.

  void throwError( const char * msg );
  void synchWritingDirection( );

  const FontMetrics::CharacterEncoding * encoding_;

 public:
  typedef int UnionType;
  UnionType yylval;
  AfmScanner( FontMetrics::BaseFont * fontMetricsDst, std::istream * yyin = 0 );
  virtual int yylex( );
  void setTellQue( bool tellQue ) { tellQue_ = tellQue; }
};

#endif
