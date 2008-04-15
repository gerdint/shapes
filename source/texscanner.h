#ifndef texscanner_h
#define texscanner_h

#include <string>
#include <vector>

#ifndef FLEXINT_H								// Else *FlexLexer will be defined twice
#	undef yyFlexLexer
#	define yyFlexLexer texFlexLexer
#	include <FlexLexer.h>
#endif

class TeXScanner : public texFlexLexer
{
public:
	typedef enum { BRACE = 0, BRACKET, BEGIN_END } Delimiter;
	typedef int UnionType;
	UnionType yylval;
protected:
	std::vector< Delimiter > delimStack;

public:
	TeXScanner( );
	virtual int yylex( );

	void check( const std::string & str );

protected:
	const char * opening( Delimiter delim ) const;
	const char * closing( Delimiter delim ) const;
};

#endif
