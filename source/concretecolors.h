#ifndef concretecolors_h
#define concretecolors_h

#include "MetaPDF_Ast_decls.h"

#include "pdfstructure.h"
#include "refcount.h"

#include <iostream>


namespace MetaPDF
{
  namespace Concrete
  {

    class Gray
    {
    public:
      double gr_;
      Gray( double gr ) : gr_( gr ) { }

      void setStroking( std::ostream & os ) const;
      void setNonStroking( std::ostream & os ) const;
      RefCountPtr< SimplePDF::PDF_Vector > componentVector( ) const;

      Concrete::Gray add( const Concrete::Gray & col2, const Ast::SourceLocation & callLoc ) const;
      Concrete::Gray mul( double factor, const Ast::SourceLocation & factorLoc ) const;
      Concrete::Gray addNoCheck( const Concrete::Gray & col2 ) const;
      Concrete::Gray mulNoCheck( double factor ) const;
    };

    class RGB
    {
    public:
      double r_;
      double g_;
      double b_;
      RGB( double r, double g, double b ) : r_( r ), g_( g ), b_( b ) { }

      void setStroking( std::ostream & os ) const;
      void setNonStroking( std::ostream & os ) const;
      RefCountPtr< SimplePDF::PDF_Vector > componentVector( ) const;

      Concrete::RGB add( const Concrete::RGB & col2, const Ast::SourceLocation & callLoc ) const;
      Concrete::RGB mul( double factor, const Ast::SourceLocation & factorLoc ) const;

      double mean( ) const { return (1./3) * ( r_ + g_ + b_ ); }
      Concrete::RGB addNoCheck( const Concrete::RGB & col2 ) const;
    };

    class CMYK
    {
    public:
      double c_;
      double m_;
      double y_;
      double k_;
      CMYK( double c, double m, double y, double k ) : c_( c ), m_( m ), y_( y ), k_( k ) { }

      void setStroking( std::ostream & os ) const;
      void setNonStroking( std::ostream & os ) const;
      RefCountPtr< SimplePDF::PDF_Vector > componentVector( ) const;
    };

  }
}


#endif
