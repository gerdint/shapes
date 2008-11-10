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

#ifndef pdfscanner_h
#define pdfscanner_h

#include "pdfstructure.h"

#ifndef FLEXINT_H								// Else *FlexLexer will be defined twice
#	undef yyFlexLexer
#	define yyFlexLexer pdfFlexLexer
#	include <FlexLexer.h>
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
