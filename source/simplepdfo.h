#ifndef simplepdfo_h
#define simplepdfo_h

#include "pdfstructure.h"

#include "SimplePDF_decls.h"
#include "Shapes_Lang_decls.h"
#include "concretecolors.h"

#include <list>
#include <set>

namespace SimplePDF
{

	class PDF_Resources : public PDF_Object
	{
	public:
		enum ProcSet { PROC_SET_PDF, PROC_SET_TEXT, PROC_SET_IMAGE_GRAY, PROC_SET_IMAGE_COLOR, PROC_SET_IMAGE_INDEXED };

	private:
		size_t counter;
		RefCountPtr< PDF_Dictionary > xobject;
		RefCountPtr< PDF_Dictionary > graphicsStates;
		RefCountPtr< PDF_Dictionary > colorSpaces;
		RefCountPtr< PDF_Dictionary > fonts;
		RefCountPtr< PDF_Dictionary > shadings;
		std::set< ProcSet > procSets;
		RefCountPtr< PDF_Vector > procSetsVector;

		typedef std::map< PDF_Object *, PDF_Name > ReverseMap;
		ReverseMap reverse_xobject;
		ReverseMap reverse_graphicsStates;
		ReverseMap reverse_colorSpaces;
		ReverseMap reverse_fonts;
		ReverseMap reverse_shadings;

	public:
		PDF_Resources( );
		virtual ~PDF_Resources( );
		const PDF_Name & nameofXObject( const RefCountPtr< PDF_Object > & obj );
		const PDF_Name & nameofGraphicsState( const RefCountPtr< PDF_Object > & obj );
		const PDF_Name & nameofColorSpace( const RefCountPtr< PDF_Object > & obj );
		const PDF_Name & nameofFont( const RefCountPtr< PDF_Object > & obj );
		const PDF_Name & nameofShading( const RefCountPtr< PDF_Object > & obj );
		void requireProcedureSet( ProcSet procSet );

		virtual void writeTo( std::ostream & os ) const;

	private:
		static const PDF_Name & nameof( const RefCountPtr< PDF_Object > & obj, ReverseMap * reverseMap, RefCountPtr< PDF_Dictionary > * dic, const char * prefix, size_t * counter );
	};

	class PDF_out
	{
	public:
		RefCountPtr< PDF_Dictionary > root_;
		RefCountPtr< PDF_Dictionary > info_;
		std::list< std::string > extensionAuthorStrings;
	private:
		RefCountPtr< PDF_Object > i_root;
		size_t objCount;
		typedef std::list< RefCountPtr< PDF_Indirect_out > > IndirectQueueType;
		IndirectQueueType indirectQueue;
		static double pdfNameToDouble( RefCountPtr< PDF_Object > nameObject );
		std::list< RefCountPtr< PDF_in > > importSources;
	public:
		PDF_out( );
		~PDF_out( );
		void writeData( std::ostream & os, SimplePDF::PDF_Version & pdfVersion );

		void abort( );

		RefCountPtr< PDF_Indirect_out > indirect( RefCountPtr< PDF_Object > obj, size_t v = 0 );

		RefCountPtr< const std::vector< RefCountPtr< const Shapes::Lang::XObject > > > addPagesAsXObjects( RefCountPtr< PDF_in > pdfi );
		void importBtexEtexThings( RefCountPtr< PDF_in > pdfi, std::map< std::string, RefCountPtr< const Shapes::Lang::XObject > > * dstMap, const std::string & setupCodeHash );

		static RefCountPtr<PDF_Object> newName( const char * str );
		static RefCountPtr<PDF_Object> newString( const char * str );
		static RefCountPtr<PDF_Object> newInt( PDF_Int::ValueType val );
		static RefCountPtr<PDF_Object> newBoolean( PDF_Boolean::ValueType val );
		static RefCountPtr<PDF_Object> newFloat( PDF_Float::ValueType val );
	};

	extern RefCountPtr<PDF_Object> theTrue;
	extern RefCountPtr<PDF_Object> theFalse;

	class OutlineItem
	{
		RefCountPtr< PDF_Object > destination_;
		RefCountPtr< const char > title_;
		std::list< RefCountPtr< OutlineItem > > kids_;
		bool isOpen_;

		bool fontBold_;
		bool fontItalic_;
		Shapes::Concrete::RGB color_;
	public:
		OutlineItem( const RefCountPtr< PDF_Object > & destination, const RefCountPtr< const char > & title, bool isOpen, bool fontBold, bool fontItalic, const Shapes::Concrete::RGB & color );
		~OutlineItem( );
		void addKid( const RefCountPtr< OutlineItem > & kid );

		bool hasKids( ) const;
		RefCountPtr< SimplePDF::PDF_Indirect_out > getTopIndirectDictionary( SimplePDF::PDF_out * doc, SimplePDF::PDF_Version & pdfVersion ) const;
		size_t fillInDictionary( RefCountPtr< SimplePDF::PDF_Dictionary > dstDic, const RefCountPtr< SimplePDF::PDF_Indirect_out > & i_dstDic, SimplePDF::PDF_out * doc, SimplePDF::PDF_Version & pdfVersion ) const;
	};

}

#endif
