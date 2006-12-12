#ifndef shadingtypes_h
#define shadingtypes_h

#include "drawabletypes.h"


namespace MetaPDF
{
  namespace Lang
  {

    class Type4ShadingGray : public Lang::PaintedPolygon2D
    {
      RefCountPtr< SimplePDF::PDF_Object > resource_;
      RefCountPtr< const Lang::ElementaryPath2D > mybbox_;
    public:
      Type4ShadingGray( RefCountPtr< SimplePDF::PDF_Object > & resource, RefCountPtr< const Lang::ElementaryPath2D > mybbox );
      virtual ~Type4ShadingGray( );
      virtual void shipout( std::ostream & os, Kernel::PageContentStates * pdfState, const Lang::Transform2D & tf ) const;
      virtual RefCountPtr< const Lang::ElementaryPath2D > bbox( ) const;
      virtual void show( std::ostream & os ) const;
      virtual void gcMark( Kernel::GCMarkedSet & marked );
    };
    
  }
}

#endif
