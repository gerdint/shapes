/* This file is part of Shapes.
 *
 * Shapes is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * any later version.
 *
 * Shapes is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with Shapes.  If not, see <http://www.gnu.org/licenses/>.
 *
 * Copyright 2008 Henrik Tidefelt
 */

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
