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

#include "concretecolors.h"
#include "shapesexceptions.h"
#include "sourcelocation.h"

using namespace Shapes;

const double theFloatTol = 0.001;

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
			if( res_gr - 1 > theFloatTol )
				{
					throw Exceptions::OutOfRange( callLoc, strrefdup( "The sum is greater than 1 in the gray component." ) );
				}
			else
				{
					res_gr = 1;
				}
		}
	return Concrete::Gray( res_gr );
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
			if( res_r - 1 > theFloatTol )
				{
					throw Exceptions::OutOfRange( callLoc, strrefdup( "The sum is greater than 1 in the red component." ) );
				}
			else
				{
					res_r = 1;
				}
		}
	if( res_g > 1 )
		{
			if( res_g - 1 > theFloatTol )
				{
					throw Exceptions::OutOfRange( callLoc, strrefdup( "The sum is greater than 1 in the green component." ) );
				}
			else
				{
					res_g = 1;
				}
		}
	if( res_b > 1 )
		{
			if( res_b - 1 > theFloatTol )
				{
					throw Exceptions::OutOfRange( callLoc, strrefdup( "The sum is greater than 1 in the blue component." ) );
				}
			else
				{
					res_b = 1;
				}
		}
	return Concrete::RGB( res_r, res_g, res_b );
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


Concrete::CMYK
Concrete::CMYK::add( const CMYK & col2, const Ast::SourceLocation & callLoc ) const
{
	double res_c = c_ + col2.c_;
	double res_m = m_ + col2.m_;
	double res_y = y_ + col2.y_;
	double res_k = k_ + col2.k_;
	if( res_k > 1 )
		{
			if( res_k - 1 > theFloatTol )
				{
					throw Exceptions::OutOfRange( callLoc, strrefdup( "The sum is greater than 1 in the black component." ) );
				}
			else
				{
					res_c = res_m = res_y = 0;
					res_k = 1;
				}
		}
	else
		{
			if( double tmp = res_c + res_k > 1 )
				{
					if( tmp - 1 > theFloatTol )
						{
							throw Exceptions::OutOfRange( callLoc, strrefdup( "The sum is greater than 1 in the cyan component." ) );
						}
					else
						{
							res_c = 1 - res_k;
						}
				}
			if( double tmp = res_m + res_k > 1 )
				{
					if( tmp - 1 > theFloatTol )
						{
							throw Exceptions::OutOfRange( callLoc, strrefdup( "The sum is greater than 1 in the magenta component." ) );
						}
					else
						{
							res_m = 1 - res_k;
						}
				}
			if( double tmp = res_y + res_k > 1 )
				{
					if( tmp - 1 > theFloatTol )
						{
							throw Exceptions::OutOfRange( callLoc, strrefdup( "The sum is greater than 1 in the yellow component." ) );
						}
					else
						{
							res_y = 1 - res_k;
						}
				}
		}
	return Concrete::CMYK( res_c, res_m, res_y, res_k );
}


Concrete::CMYK
Concrete::CMYK::mul( double factor, const Ast::SourceLocation & factorLoc ) const
{
	if( factor < 0 )
		{
			throw Exceptions::OutOfRange( factorLoc, strrefdup( "The scalar is less than 0." ) );
		}
	if( factor > 1 )
		{
			throw Exceptions::OutOfRange( factorLoc, strrefdup( "The scalar is greater than 1." ) );
		}
	return Concrete::CMYK( factor * c_, factor * m_, factor * y_, factor * k_ );
}
