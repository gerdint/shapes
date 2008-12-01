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

#ifndef pdfimport_h
#define pdfimport_h

#include "SimplePDF_decls.h"
#include "Shapes_Lang_decls.h"

#include "simplepdfi.h"

namespace Shapes
{
	namespace Kernel
	{

		class Import
		{
			std::list< RefCountPtr< SimplePDF::PDF_in > > importSources_;
		public:
			Import( );

			RefCountPtr< const std::vector< RefCountPtr< const Shapes::Lang::XObject > > > addPagesAsXObjects( RefCountPtr< SimplePDF::PDF_in > pdfi );
			void importBtexEtexThings( RefCountPtr< SimplePDF::PDF_in > pdfi, std::map< std::string, RefCountPtr< const Shapes::Lang::XObject > > * dstMap, const std::string & setupCodeHash );

			void free( );

		private:
			static double pdfNameToDouble( RefCountPtr< SimplePDF::PDF_Object > nameObject );
		};

	}
}

#endif
