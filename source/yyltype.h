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

#ifndef yyltype_h
#define yyltype_h

#define YYLTYPE Ast::SourceLocation

#define YYLLOC_DEFAULT( Current, Rhs, N )		 \
	(Current).filename		= (Rhs)[1].filename;	 \
	(Current).firstLine	 = (Rhs)[1].firstLine;	\
	(Current).firstColumn = (Rhs)[1].firstColumn; \
	(Current).lastLine		= (Rhs)[N].lastLine;		\
	(Current).lastColumn	= (Rhs)[N].lastColumn;

#endif
