#ifndef simplepdfi_h
#define simplepdfi_h

#include <vector>

#include "SimplePDF_decls.h"

#include "refcount.h"
#include "pdfstructure.h"

namespace SimplePDF
{

	class PDF_in
	{
		std::streamoff xref;
		size_t xrefSize;

		RefCountPtr< std::istream > is;
		std::istream * isPtr;

		RefCountPtr< PDF_Dictionary > resources;
		RefCountPtr< PDF_Vector > pages;

		RefCountPtr< PDF_Object > readObjectAt( std::streamoff pos );
		std::streamoff xreflookup( size_t i, size_t v );
		RefCountPtr< PDF_Object > parse( );
	public:
		class PageIterator
		{
			int pageNo;
			SimplePDF::PDF_in & in;
		public:
			PageIterator( SimplePDF::PDF_in & _in, int _pageNo );
			PageIterator( const PageIterator & orig );
			PageIterator & operator = ( const PageIterator & orig );
			bool operator == ( const PageIterator & i2 ) const;
			bool operator != ( const PageIterator & i2 ) const;
			RefCountPtr< PDF_Dictionary > operator * ( );
			PageIterator operator ++ ();
			PageIterator operator -- ();
			PageIterator operator ++ ( int );
			PageIterator operator -- ( int );
			PageIterator & operator += ( int diff );
			PageIterator & operator -= ( int diff );
		};

		PDF_in( RefCountPtr< std::istream > _is );
		~PDF_in( );

		RefCountPtr< PDF_Object > readObjectNumbered( size_t i, size_t v );
		template< class S >
			RefCountPtr< S > follow( RefCountPtr< PDF_Object > maybeIndirect );
		RefCountPtr< PDF_Object > follow( RefCountPtr< PDF_Object > maybeIndirect );
		size_t getPageCount( );
		PageIterator beginPages( );
		PageIterator endPages( );
		RefCountPtr< PDF_Dictionary > getPage( size_t pageNo );
	};

	template< class S >
		RefCountPtr< S > PDF_in::follow( RefCountPtr< PDF_Object > maybeIndirect )
		{
			PDF_Indirect * tmp( dynamic_cast< PDF_Indirect * >( maybeIndirect.getPtr( ) ) );
			if( tmp == 0 )
				{
					RefCountPtr< S > res( maybeIndirect.down_cast< S >( ) );
					if( res == NullPtr< S >( ) )
						{
							throw( "Downcast in PDF_in::follow failed" );
						}
					return res;
				}
			return follow< S >( readObjectNumbered( tmp->i, tmp->v ) );
		}

}

#endif
