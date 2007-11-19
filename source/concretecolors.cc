#include "concretecolors.h"
#include "shapesexceptions.h"
#include "sourcelocation.h"

using namespace Shapes;


void
Concrete::Gray::setStroking( std::ostream & os ) const
{
	if( gr_ < 0 )
		{
			return;
		}
	os << gr_ << " G " ;
}

void
Concrete::Gray::setNonStroking( std::ostream & os ) const
{
	if( gr_ < 0 )
		{
			return;
		}
	os << gr_ << " g " ;
}

RefCountPtr< SimplePDF::PDF_Vector >
Concrete::Gray::componentVector( ) const
{
	return RefCountPtr< SimplePDF::PDF_Vector >( new SimplePDF::PDF_Vector( gr_ ) );
}

Concrete::Gray
Concrete::Gray::add( const Concrete::Gray & col2, const Ast::SourceLocation & callLoc ) const
{
	double res_gr = gr_ + col2.gr_;
	if( res_gr > 1 )
		{
			throw Exceptions::OutOfRange( callLoc, strrefdup( "The sum is greater than 1 in the gray component." ) );
		}
	return Concrete::Gray( res_gr );
}

Concrete::Gray
Concrete::Gray::addNoCheck( const Concrete::Gray & col2 ) const
{
	return Concrete::Gray( gr_ + col2.gr_ );
}

Concrete::Gray
Concrete::Gray::mul( double factor, const Ast::SourceLocation & factorLoc ) const
{
	if( factor < 0 )
		{
			throw Exceptions::OutOfRange( factorLoc, strrefdup( "The scalar is less than 0." ) );
		}
	if( factor > 1 )
		{
			throw Exceptions::OutOfRange( factorLoc, strrefdup( "The scalar is greater than 1." ) );
		}
	return Concrete::Gray( factor * gr_ );
}

Concrete::Gray
Concrete::Gray::mulNoCheck( double factor ) const
{
	return Concrete::Gray( factor * gr_ );
}



void
Concrete::RGB::setStroking( std::ostream & os ) const
{
	os << r_ << " " << g_ << " " << b_ << " RG " ;
}

void
Concrete::RGB::setNonStroking( std::ostream & os ) const
{
	os << r_ << " " << g_ << " " << b_ << " rg " ;
}

RefCountPtr< SimplePDF::PDF_Vector >
Concrete::RGB::componentVector( ) const
{
	return RefCountPtr< SimplePDF::PDF_Vector >( new SimplePDF::PDF_Vector( r_, g_, b_ ) );
}

Concrete::RGB
Concrete::RGB::add( const Concrete::RGB & col2, const Ast::SourceLocation & callLoc ) const
{
	double res_r = r_ + col2.r_;
	double res_g = g_ + col2.g_;
	double res_b = b_ + col2.b_;
	if( res_r > 1 )
		{
			throw Exceptions::OutOfRange( callLoc, strrefdup( "The sum is greater than 1 in the red component." ) );
		}
	if( res_g > 1 )
		{
			throw Exceptions::OutOfRange( callLoc, strrefdup( "The sum is greater than 1 in the green component." ) );
		}
	if( res_b > 1 )
		{
			throw Exceptions::OutOfRange( callLoc, strrefdup( "The sum is greater than 1 in the blue component." ) );
		}
	return Concrete::RGB( res_r, res_g, res_b );
}

Concrete::RGB
Concrete::RGB::addNoCheck( const Concrete::RGB & col2 ) const
{
	return Concrete::RGB( r_ + col2.r_, g_ + col2.g_, b_ + col2.b_ );
}

Concrete::RGB
Concrete::RGB::mul( double factor, const Ast::SourceLocation & factorLoc ) const
{
	if( factor < 0 )
		{
			throw Exceptions::OutOfRange( factorLoc, strrefdup( "The scalar is less than 0." ) );
		}
	if( factor > 1 )
		{
			throw Exceptions::OutOfRange( factorLoc, strrefdup( "The scalar is greater than 1." ) );
		}
	return Concrete::RGB( factor * r_, factor * g_, factor * b_ );
}



void
Concrete::CMYK::setStroking( std::ostream & os ) const
{
	os << c_ << " " << m_ << " " << y_ << " " << k_ << " K " ;
}

void
Concrete::CMYK::setNonStroking( std::ostream & os ) const
{
	os << c_ << " " << m_ << " " << y_ << " " << k_ << " k " ;
}

RefCountPtr< SimplePDF::PDF_Vector >
Concrete::CMYK::componentVector( ) const
{
	return RefCountPtr< SimplePDF::PDF_Vector >( new SimplePDF::PDF_Vector( c_, m_, y_, k_ ) );
}
