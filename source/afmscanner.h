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

#ifndef afmscanner_h
#define afmscanner_h

#include "fontmetrics.h"
#include "characterencoding.h"

#ifndef FLEXINT_H								// Else *FlexLexer will be defined twice
#	undef yyFlexLexer
#	define yyFlexLexer afmFlexLexer
#	include <FlexLexer.h>
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
