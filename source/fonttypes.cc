#include "fonttypes.h"
#include "classtypes.h"
#include "strrefdup.h"
#include "fontmetrics.h"
#include "pagecontentstates.h"
#include "globals.h"

using namespace Shapes;


Lang::Type3Glyph::Type3Glyph( Kind kind, CodeType code, const RefCountPtr< const char > & name, const RefCountPtr< const Lang::Drawable2D > & glyph, Concrete::Length widthX, Concrete::Length xmin, Concrete::Length ymin, Concrete::Length xmax, Concrete::Length ymax )
	: kind_( kind ), code_( code ), name_( name ), glyph_( glyph ),
		widthX_( widthX ), xmin_( xmin ), ymin_( ymin ), xmax_( xmax ), ymax_( ymax )
{ }

Lang::Type3Glyph::~Type3Glyph( )
{ }

RefCountPtr< const Lang::Class > Lang::Type3Glyph::TypeID( new Lang::SystemFinalClass( strrefdup( "Glyph" ) ) );
TYPEINFOIMPL( Type3Glyph );

RefCountPtr< const char >
Lang::Type3Glyph::name( ) const
{
	return name_;
}

void
Lang::Type3Glyph::enlargeBBox( double * dstXMin, double * dstYMin, double * dstXMax, double * dstYMax ) const
{
	*dstXMin = std::min( *dstXMin, static_cast< double >( Concrete::Length::offtype( xmin_ ) ) );
	*dstYMin = std::min( *dstYMin, static_cast< double >( Concrete::Length::offtype( ymin_ ) ) );
	*dstXMax = std::max( *dstXMax, static_cast< double >( Concrete::Length::offtype( xmax_ ) ) );
	*dstYMax = std::max( *dstYMax, static_cast< double >( Concrete::Length::offtype( ymax_ ) ) );
}

void
Lang::Type3Glyph::setupMetric( FontMetrics::CharacterMetrics * metric, Physical< -1, 0 > invSize ) const
{
	{
		// I need to do a trick here to avoid a compiler tautology warning in case CodeType is unsigned char.
		size_t dummy = code_;
		if( dummy < 256 )
			{
				metric->characterCode_ = code_;
			}
	}
	metric->horizontalCharWidthX_ = widthX_ * invSize;
	metric->xmin_ = xmin_ * invSize;
	metric->ymin_ = ymin_ * invSize;
	metric->xmax_ = xmax_ * invSize;
	metric->ymax_ = ymax_ * invSize;
}

void
Lang::Type3Glyph::shipout( std::ostream & os, RefCountPtr< SimplePDF::PDF_Resources > resources ) const
{
	if( kind_ == BASIC )
		{
			os << Concrete::Length::offtype( widthX_ ) << " 0 d0" << std::endl ;
		}
	else
		{
			os << Concrete::Length::offtype( widthX_ ) << " 0 " 
				 << Concrete::Length::offtype( xmin_ ) << " " << Concrete::Length::offtype( ymin_ )
				 << " " << Concrete::Length::offtype( xmax_ ) << " " << Concrete::Length::offtype( ymax_ ) << " d1" << std::endl ;
		}

	Concrete::Length the1bp( 1 );

	Kernel::PageContentStates pdfState( resources );
	glyph_->shipout( os, & pdfState, Lang::THE_2D_IDENTITY );
}

void
Lang::Type3Glyph::gcMark( Kernel::GCMarkedSet & marked )
{
	const_cast< Lang::Drawable2D * >( glyph_.getPtr( ) )->gcMark( marked );
}
