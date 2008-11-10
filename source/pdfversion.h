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

#ifndef pdfversion_h
#define pdfversion_h

#include "SimplePDF_decls.h"


namespace SimplePDF
{

	class PDF_Version
	{
	public:
		enum Version { PDF_X = 0, PDF_1_1, PDF_1_2, PDF_1_3, PDF_1_4, PDF_1_5, PDF_1_6, VERSION_UNDEFINED };
		enum Action { ERROR = 0, WARN, SILENT };
	private:
		Version version_;
		Version maxRequestVersion_;
		Action versionAction_;

	public:
		PDF_Version( );
		const char * maxRequestVersionString( ) const;

		void setVersion( Version version );
		void setAction( Action action );
		bool greaterOrEqual( Version required, bool justCurious = false );
		bool greaterOrEqualOrX( Version required, bool justCurious = false );
		void message( Version required, const char * message ) const;

		static const char * toString( Version version );
	};

}

#endif
