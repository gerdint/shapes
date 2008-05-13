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
