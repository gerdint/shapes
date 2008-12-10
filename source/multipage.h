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

#ifndef multipage_h
#define multipage_h

#include "Shapes_Ast_decls.h"
#include "Shapes_Kernel_decls.h"
#include "Shapes_Lang_decls.h"
#include "FontMetrics_decls.h"
#include "SimplePDF_decls.h"

#include "hottypes.h"
#include "tagtypes.h"
#include "pdfstructure.h"

namespace Shapes
{
	namespace Lang
	{

		class DocumentDestination : public Lang::NoOperatorOverloadGeometric2D
		{
		public:
			enum Sides{ PAGE, TOPLEFT, TOP, LEFT, RECTANGLE };
		private:
			bool remote_;
			/* Strings are used instead of symbols for destination names, since they are often composd from parts,
			 * like [sprintf `chap.%d.sec.%d´ chapNo secNo].
			 */
			RefCountPtr< const char > name_; // Null if not a named destination.	Must be present if remote_.
			int outlineLevel_; // Negative if not to appear in the document outline.

			RefCountPtr< const char > outlineText_;
			bool outlineOpen_; // Only applies if to appear in the document outline.
			bool outlineFontBold_;
			bool outlineFontItalic_;
			Concrete::RGB outlineColor_;

			/* The following members shall not be used for remote_ destinations.
			 */
			Sides sidesMode_;
			RefCountPtr< const Lang::Drawable2D > target_; // Not used if sidesMode_ is <page>.
			bool fittobbox_; // If true, the content's bounding box is used instead of the mediabox for zooming purposes.
			double zoom_; // If not strictly positive, zoom is unspecified.	Can only be specified in mode <topleft>.
		public:
			DocumentDestination( bool remote, RefCountPtr< const char > name, int outlineLevel,
													 RefCountPtr< const char > outlineText, bool outlineOpen, bool outlineFontBold, bool outlineFontItalic, const Concrete::RGB & outlineColor ); // remote must be true!.
			DocumentDestination( RefCountPtr< const char > name, int outlineLevel,
													 RefCountPtr< const char > outlineText, bool outlineOpen, bool outlineFontBold, bool outlineFontItalic, const Concrete::RGB & outlineColor,
													 Sides sidesMode, RefCountPtr< const Lang::Drawable2D > target, bool fittobbox, double zoom );
			virtual ~DocumentDestination( );
			virtual RefCountPtr< const Lang::Geometric2D > transformed( const Lang::Transform2D & transform, const RefCountPtr< const Lang::Geometric2D > & self ) const;
			virtual RefCountPtr< const Lang::Geometric3D > to3D( const RefCountPtr< const Lang::Geometric2D > & self ) const;

			virtual void gcMark( Kernel::GCMarkedSet & marked );

			bool definesNamed( ) const;
			RefCountPtr< const char > name( ) const;
			bool isOutlineEntry( ) const;
			size_t outlineLevel( ) const;
			RefCountPtr< SimplePDF::PDF_Object > getDestination( const RefCountPtr< SimplePDF::PDF_Indirect_out > & i_page ) const;
			RefCountPtr< SimplePDF::PDF_Vector > getDirectDestination( const RefCountPtr< SimplePDF::PDF_Indirect_out > & i_page ) const;
			RefCountPtr< SimplePDF::OutlineItem > getOutlineItem( const RefCountPtr< SimplePDF::PDF_Indirect_out > & i_page, RefCountPtr< const char > otherText = RefCountPtr< const char >( NullPtr< const char >( ) ) ) const;

			TYPEINFODECL;
		};

	}

	namespace Kernel
	{

		class WarmCatalog : public Kernel::State
		{
		public:
			class BoundingRectangle
			{
				Concrete::Length xmin_;
				Concrete::Length ymin_;
				Concrete::Length xmax_;
				Concrete::Length ymax_;
				mutable bool modified_;
				mutable RefCountPtr< SimplePDF::PDF_Vector > pdfVec_;
			public:
				BoundingRectangle( );
				void growToContain( const Concrete::Coords2D & ll, const Concrete::Coords2D & ur );
				RefCountPtr< SimplePDF::PDF_Vector > pdfVector( ) const;
			};
			class Page
			{
			public:
				size_t index_;
				RefCountPtr< SimplePDF::PDF_Resources > resources_;
				RefCountPtr< SimplePDF::PDF_Stream_out > contents_;
				RefCountPtr< BoundingRectangle > mediabox_;
				std::vector< RefCountPtr< const Lang::DocumentDestination > > destinations_;
				std::vector< RefCountPtr< const Lang::AnnotationBase > > annotations_;

				Page( size_t index, const RefCountPtr< SimplePDF::PDF_Resources > & resources, const RefCountPtr< SimplePDF::PDF_Stream_out > & contents, const RefCountPtr< Kernel::WarmCatalog::BoundingRectangle > & mediabox );
				~Page( );
			};
			class PageLabelEntry
			{
			public:
				enum Style{ NONE, DECIMAL, ROMAN, rOMAN, ALPHABET, aLPHABET };
				size_t pageIndex_;
				RefCountPtr< const char > prefix_;
				Style style_;
				size_t startNumber_;

				PageLabelEntry( size_t pageIndex, const RefCountPtr< const char > & prefix, Style style, size_t startNumber );
				~PageLabelEntry( );
			};
			typedef std::vector< SimplePDF::PDF_out > ShipoutList;
		private:
			PtrOwner_back_Access< std::list< const Page * > > pages_;
			PtrOwner_back_Access< std::list< const PageLabelEntry * > > labelEntries_;
			bool pageLabelsActivated_;
			std::map< Lang::Symbol::KeyType, RefCountPtr< BoundingRectangle > > mediaBoxes_;
			RefCountPtr< const Lang::Symbol > bboxGroup_;
		public:
			WarmCatalog( );
			virtual ~WarmCatalog( );
			virtual void tackOnImpl( Kernel::EvalState * evalState, const RefCountPtr< const Lang::Value > & piece, const Ast::SourceLocation & callLoc );
			virtual void peekImpl( Kernel::EvalState * evalState, const Ast::SourceLocation & callLoc );
			virtual void freezeImpl( Kernel::EvalState * evalState, const Ast::SourceLocation & callLoc );
			virtual void gcMark( Kernel::GCMarkedSet & marked );

			void setLabel( RefCountPtr< const char > prefix, PageLabelEntry::Style style, size_t start );
			size_t getNextPageNumber( ) const;
			PageLabelEntry::Style getNextPageStyle( ) const;
			RefCountPtr< const char > getNextPagePrefix( ) const;
			RefCountPtr< const char > getNextPageLabel( ) const;
			RefCountPtr< const char > getPageLabel( size_t index ) const;

			void setBBoxGroup( const RefCountPtr< const Lang::Symbol > & group );

			bool isEmpty( ) const;
			void tackOnPage( const Kernel::PassedDyn & dyn, const RefCountPtr< const Lang::Drawable2D > & pageContents, const Ast::SourceLocation & callLoc );
			void shipout( bool split, ShipoutList * docs );

			TYPEINFODECL;

		private:
			RefCountPtr< const char > getPageLabel( const Kernel::WarmCatalog::PageLabelEntry * entry, size_t index ) const;
			SimplePDF::PDF_out shipoutOne( RefCountPtr< SimplePDF::PDF_Indirect_out > i_info, int pageNo = 0 ); /* Use pageNo == -1 for all pages. */
		};

	}
}

#endif
