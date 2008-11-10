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

		virtual void writeTo( std::ostream & os, SimplePDF::PDF_xref * xref, const RefCountPtr< const PDF_Object > & self ) const;

	private:
		static const PDF_Name & nameof( const RefCountPtr< PDF_Object > & obj, ReverseMap * reverseMap, RefCountPtr< PDF_Dictionary > * dic, const char * prefix, size_t * counter );
	};

	class DocumentInfo
	{
		bool finalized_;
		RefCountPtr< PDF_Dictionary > info_;
		std::list< std::string > extensionAuthorStrings;
		RefCountPtr< PDF_Indirect_out > i_info_;
	public:
		DocumentInfo( );
		void addExtensionAuthorString( const std::string & str );
		bool addInfo( const char * key, const RefCountPtr< PDF_Object > & datum );
		RefCountPtr< PDF_Indirect_out > getIndirect( size_t * indirectObjectCounter );
	};

	class PDF_out
	{
	private:
		RefCountPtr< SimplePDF::PDF_Indirect_out > i_root_;
		RefCountPtr< SimplePDF::PDF_Indirect_out > i_info_;
		RefCountPtr< const char > firstPageLabel_;

	public:
		PDF_out( const RefCountPtr< SimplePDF::PDF_Indirect_out > & i_root, const RefCountPtr< SimplePDF::PDF_Indirect_out > & i_info, const RefCountPtr< const char > & firstPageLabel );
		~PDF_out( );
		void writeFile( std::ostream & os, SimplePDF::PDF_Version & pdfVersion );
		RefCountPtr< const char > getFirstPageLabel( ) const;
	};

	extern RefCountPtr<PDF_Object> theTrue;
	extern RefCountPtr<PDF_Object> theFalse;
	RefCountPtr<PDF_Object> newName( const char * str );
	RefCountPtr<PDF_Object> newString( const char * str );
	RefCountPtr<PDF_Object> newInt( PDF_Int::ValueType val );
	RefCountPtr<PDF_Object> newBoolean( PDF_Boolean::ValueType val );
	RefCountPtr<PDF_Object> newFloat( PDF_Float::ValueType val );


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
		RefCountPtr< SimplePDF::PDF_Indirect_out > getTopIndirectDictionary( SimplePDF::PDF_Version & pdfVersion ) const;
		size_t fillInDictionary( RefCountPtr< SimplePDF::PDF_Dictionary > dstDic, const RefCountPtr< SimplePDF::PDF_Indirect_out > & i_dstDic, SimplePDF::PDF_Version & pdfVersion ) const;
	};

}

#endif
