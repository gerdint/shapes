#ifndef yyltype_h
#define yyltype_h

#define YYLTYPE Ast::SourceLocation

#define YYLLOC_DEFAULT( Current, Rhs, N )     \
  (Current).filename    = (Rhs)[1].filename;   \
  (Current).firstLine   = (Rhs)[1].firstLine;  \
  (Current).firstColumn = (Rhs)[1].firstColumn; \
  (Current).lastLine    = (Rhs)[N].lastLine;    \
  (Current).lastColumn  = (Rhs)[N].lastColumn;

#endif
