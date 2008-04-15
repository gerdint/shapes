#include "texscanner.h"
#include "strrefdup.h"

#include <sstream>


TeXScanner::TeXScanner( )
	: yyFlexLexer( 0, 0 )
{ }

void
TeXScanner::check( const std::string & str )
{
	delimStack.clear( );
	std::istringstream is( str );
	switch_streams( & is, 0 );
	yylex( );
}

const char *
TeXScanner::opening( TeXScanner::Delimiter delim ) const
{
	switch( delim )
		{
		case BRACE:
			return "{";
		case BRACKET:
			return "[";
		case BEGIN_END:
			return "\begin{...}";
		default:
			throw "TeXScanner::opening: Delimiter out of range.";
		}
}

const char *
TeXScanner::closing( TeXScanner::Delimiter delim ) const
{
	switch( delim )
		{
		case BRACE:
			return "}";
		case BRACKET:
			return "]";
		case BEGIN_END:
			return "\end{...}";
		default:
			throw "TeXScanner::closing: Delimiter out of range.";
		}
}
