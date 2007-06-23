#ifndef multipage_h
#define multipage_h

#include "MetaPDF_Ast_decls.h"
#include "MetaPDF_Kernel_decls.h"
#include "MetaPDF_Lang_decls.h"
#include "FontMetrics_decls.h"
#include "SimplePDF_decls.h"

#include "hottypes.h"

namespace MetaPDF
{
  namespace Kernel
  {

    class WarmCatalog : public Kernel::State
    {
    public:
      class Page
      {
      public:
	RefCountPtr< SimplePDF::PDF_Resources > resources_;
	RefCountPtr< SimplePDF::PDF_Stream_out > contents_;
	RefCountPtr< SimplePDF::PDF_Vector > mediabox_;

	Page( const RefCountPtr< SimplePDF::PDF_Resources > & resources, const RefCountPtr< SimplePDF::PDF_Stream_out > & contents, const RefCountPtr< SimplePDF::PDF_Vector > & mediabox );
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
    private:
      PtrOwner_back_Access< std::list< const Page * > > pages_;
      PtrOwner_back_Access< std::list< const PageLabelEntry * > > labelEntries_;
    public:
      WarmCatalog( );
      virtual ~WarmCatalog( );
      virtual void tackOnImpl( Kernel::EvalState * evalState, const RefCountPtr< const Lang::Value > & piece, const Kernel::PassedDyn & dyn, const Ast::SourceLocation & callLoc );
      virtual void peekImpl( Kernel::EvalState * evalState, const Ast::SourceLocation & callLoc );
      virtual void freezeImpl( Kernel::EvalState * evalState, const Ast::SourceLocation & callLoc );
      virtual void gcMark( Kernel::GCMarkedSet & marked );
      void setLabel( RefCountPtr< const char > prefix, PageLabelEntry::Style style, size_t start );
      size_t getNextPageNumber( ) const;
      RefCountPtr< const char > getNextPageLabel( ) const;

      bool isEmpty( ) const;
      void tackOnPage( const RefCountPtr< const Lang::Drawable2D > & pageContents, const Ast::SourceLocation & callLoc );
      void shipout( SimplePDF::PDF_out * doc );
      
      TYPEINFODECL;
    };
    
  }
}

#endif
