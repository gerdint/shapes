#ifndef concretecolors_h
#define concretecolors_h

#include "Shapes_Ast_decls.h"

#include "pdfstructure.h"
#include "refcount.h"

#include <iostream>


namespace Shapes
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
			Concrete::Gray addNoCheck( const Concrete::Gray & col2 ) const
			{
				return Concrete::Gray( gr_ + col2.gr_ );
			}
			Concrete::Gray mulNoCheck( double factor ) const
			{
				return Concrete::Gray( factor * gr_ );
			}
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
			Concrete::RGB addNoCheck( const Concrete::RGB & col2 ) const
			{
				return Concrete::RGB( r_ + col2.r_, g_ + col2.g_, b_ + col2.b_ );
			}
			Concrete::RGB mulNoCheck( double factor ) const
			{
				return Concrete::RGB( factor * r_, factor * g_, factor * b_ );
			}

			double mean( ) const { return (1./3) * ( r_ + g_ + b_ ); }
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

			CMYK add( const CMYK & col2, const Ast::SourceLocation & callLoc ) const;
			CMYK mul( double factor, const Ast::SourceLocation & factorLoc ) const;
			CMYK addNoCheck( const CMYK & col2 ) const
			{
				return CMYK( c_ + col2.c_, m_ + col2.m_, y_ + col2.y_, k_ + col2.k_ );
			}
			CMYK mulNoCheck( double factor ) const
			{
				return CMYK( factor * c_, factor * m_, factor * y_, factor * k_ );
			}
		};

	}
}


#endif
