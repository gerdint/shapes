#ifndef annotations_h
#define annotations_h

#include "Shapes_Ast_decls.h"
#include "Shapes_Kernel_decls.h"
#include "Shapes_Lang_decls.h"
#include "FontMetrics_decls.h"
#include "SimplePDF_decls.h"

#include "hottypes.h"
#include "tagtypes.h"


namespace Shapes
{
	namespace Lang
	{

		class AnnotationSite : public Lang::NoOperatorOverloadGeometric2D
		{
		public:
			enum Flags{ INVISIBLE = 1<<(1-1), HIDDEN = 1<<(2-1), PRINT = 1<<(3-1), NO_ZOOM = 1<<(4-1), NO_ROTATE = 1<<(5-1), NO_VIEW = 1<<(6-1), READ_ONLY = 1<<(7-1), LOCKED = 1<<(8-1), TOGGLE_NO_VIEW = 1<<(9-1)};
		protected:
			RefCountPtr< const Lang::Drawable2D > target_; // In the end, this just defines a rectangle.
			RefCountPtr< const char > contentText_;
			RefCountPtr< const char > identifier_; // Perhaps we don't have to use this, but it is included just in case...
			size_t flags_;

			char borderStyle_;	// Ensure that the value is any of the characters in "SDBIU"!
			Concrete::Length borderWidth_;	// Use 0 to get no border at all.
			RefCountPtr< const Lang::Dash > borderDash_; // May be null.
			double borderCloudy_; // Use negative value to avoid the border effect alltogether.	Suggested values are in the range 0--2.
			Concrete::RGB color_;
			
			/* At the moment, appearance states are not supported.
			 */
			RefCountPtr< const Lang::XObject > appearanceNormal_; // May be null.
			RefCountPtr< const Lang::XObject > appearanceRollover_; // May be null.
			RefCountPtr< const Lang::XObject > appearanceDown_; // May be null.
			
		public:
			AnnotationSite( const RefCountPtr< const Lang::Drawable2D > & target, const RefCountPtr< const char > & contentText, const RefCountPtr< const char > & identifier, size_t flags,
											char borderStyle, const Concrete::Length & borderWidth, const RefCountPtr< const Lang::Dash > & borderDash, double borderCloudy, const Concrete::RGB & color,
											const RefCountPtr< const Lang::XObject > & appearanceNormal, const RefCountPtr< const Lang::XObject > & appearanceRollover, const RefCountPtr< const Lang::XObject > & appearanceDown );
			virtual ~AnnotationSite( );

			RefCountPtr< const Lang::AnnotationSite > typed_transformed( const Lang::Transform2D & transform ) const;
			virtual RefCountPtr< const Lang::Geometric2D > transformed( const Lang::Transform2D & transform, const RefCountPtr< const Lang::Geometric2D > & self ) const;
			virtual RefCountPtr< const Lang::Geometric3D > to3D( const RefCountPtr< const Lang::Geometric2D > & self ) const;

			virtual void gcMark( Kernel::GCMarkedSet & marked );
			
			RefCountPtr< SimplePDF::PDF_Dictionary > getDictionary( const char * subtype, const RefCountPtr< SimplePDF::PDF_Indirect_out > & i_page, SimplePDF::PDF_out * doc ) const;

			TYPEINFODECL;
		};

		class AnnotationBase : public Lang::NoOperatorOverloadGeometric2D
		{
		protected:
			RefCountPtr< const Lang::AnnotationSite > site_;
		public:
			AnnotationBase( const RefCountPtr< const Lang::AnnotationSite > & site );
			virtual ~AnnotationBase( );

			RefCountPtr< SimplePDF::PDF_Dictionary > virtual getDictionary( const RefCountPtr< SimplePDF::PDF_Indirect_out > & i_page, SimplePDF::PDF_out * doc, const std::map< RefCountPtr< const char >, RefCountPtr< SimplePDF::PDF_Vector >, charRefPtrLess > & namedDestinations ) const = 0;

			virtual RefCountPtr< const Lang::Geometric3D > to3D( const RefCountPtr< const Lang::Geometric2D > & self ) const;

			virtual void gcMark( Kernel::GCMarkedSet & marked );
			virtual void subtypeGcMark( Kernel::GCMarkedSet & marked ) = 0;

			TYPEINFODECL;
		};

		class TextAnnotation : public Lang::AnnotationBase
		{
			RefCountPtr< const char > title_;
			bool open_;
			RefCountPtr< const char > icon_; // If present, this string should belong to the set listed in table 8.19 (page 586) in the PDF-1.6 reference.
		public:
			TextAnnotation( const RefCountPtr< const Lang::AnnotationSite > & site, const RefCountPtr< const char > & title, bool open, const RefCountPtr< const char > & icon );
			virtual ~TextAnnotation( );

			virtual RefCountPtr< const Lang::Geometric2D > transformed( const Lang::Transform2D & transform, const RefCountPtr< const Lang::Geometric2D > & self ) const;
			RefCountPtr< SimplePDF::PDF_Dictionary > virtual getDictionary( const RefCountPtr< SimplePDF::PDF_Indirect_out > & i_page, SimplePDF::PDF_out * doc, const std::map< RefCountPtr< const char >, RefCountPtr< SimplePDF::PDF_Vector >, charRefPtrLess > & namedDestinations ) const;
			virtual void subtypeGcMark( Kernel::GCMarkedSet & marked );
		};

		class LinkAnnotation : public Lang::AnnotationBase
		{
		public:
			enum Kind{ LAUNCH_FILE, LAUNCH_URI, DOC_LINK };
		private:
			Ast::SourceLocation loc_;
			char highlight_; // Make sure that this is a value from table 8.20 in the PDF-1.6 reference.
			RefCountPtr< const char > destination_;
			Kind kind_;
		public:
			LinkAnnotation( const RefCountPtr< const Lang::AnnotationSite > & site, const Ast::SourceLocation & loc, char highlight, const RefCountPtr< const char > & destination, Kind kind );
			virtual ~LinkAnnotation( );

			virtual RefCountPtr< const Lang::Geometric2D > transformed( const Lang::Transform2D & transform, const RefCountPtr< const Lang::Geometric2D > & self ) const;
			RefCountPtr< SimplePDF::PDF_Dictionary > virtual getDictionary( const RefCountPtr< SimplePDF::PDF_Indirect_out > & i_page, SimplePDF::PDF_out * doc, const std::map< RefCountPtr< const char >, RefCountPtr< SimplePDF::PDF_Vector >, charRefPtrLess > & namedDestinations ) const;
			virtual void subtypeGcMark( Kernel::GCMarkedSet & marked );
		};

	}

}


#endif
