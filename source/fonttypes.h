#ifndef fonttypes_h
#define fonttypes_h

#include "Shapes_Lang_decls.h"
#include "FontMetrics_decls.h"

#include "shapesvalue.h"
#include "elementarylength.h"


namespace Shapes
{
  namespace Lang
  {

    class Type3Glyph : public NoOperatorOverloadValue
    {
    public:
      typedef enum { BASIC, COLORED } Kind;
      typedef unsigned char CodeType;
    private:
      Kind kind_;
      CodeType code_; // Use 0 for undefined.
      RefCountPtr< const char > name_;
      RefCountPtr< const Lang::Drawable2D > glyph_;
      Concrete::Length widthX_;
      Concrete::Length xmin_;
      Concrete::Length ymin_;
      Concrete::Length xmax_;
      Concrete::Length ymax_;
      
    public:
      Type3Glyph( Kind kind, CodeType code, const RefCountPtr< const char > & name, const RefCountPtr< const Lang::Drawable2D > & glyph, Concrete::Length widthX, Concrete::Length xmin, Concrete::Length ymin, Concrete::Length xmax, Concrete::Length ymax );
      virtual ~Type3Glyph( );

      RefCountPtr< const char > name( ) const;
      CodeType code( ) const { return code_; }
      double widthX( ) const { return Concrete::Length::offtype( widthX_ ); }
      void enlargeBBox( double * dstXMin, double * dstYMin, double * dstXMax, double * dstYMax ) const;
      void setupMetric( FontMetrics::CharacterMetrics * metric, Concrete::ReciprocalLength invSize ) const;
      void shipout( std::ostream & os, RefCountPtr< SimplePDF::PDF_Resources > resources ) const;

      virtual void gcMark( Kernel::GCMarkedSet & marked );      
      TYPEINFODECL;
    };

  }
}

#endif
