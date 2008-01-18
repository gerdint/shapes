#include "shadingtypes.h"
#include "statetypes.h"
#include "globals.h"
#include "pathtypes.h"
#include "pagecontentstates.h"

using namespace Shapes;


Lang::Type4Shading::Type4Shading( RefCountPtr< SimplePDF::PDF_Object > & resource, RefCountPtr< const Lang::ElementaryPath2D > mybbox )
	: Lang::PaintedPolygon2D( Kernel::THE_NO_STATE, mybbox ), resource_( resource ), mybbox_( mybbox )
{ }

Lang::Type4Shading::~Type4Shading( )
{ }

void
Lang::Type4Shading::shipout( std::ostream & os, Kernel::PageContentStates * pdfState, const Lang::Transform2D & tf ) const
{
	Kernel::Auto_qQ auto_qQ( & pdfState->graphics_, os, false );
	if( ! tf.isIdentity( ) )
		{
			auto_qQ.activate( );
			tf.shipout( os );
			os << " cm" << std::endl ;
		}
	os << pdfState->resources_->nameofShading( resource_ ) << " sh" << std::endl ;
}

RefCountPtr< const Lang::ElementaryPath2D >
Lang::Type4Shading::bbox( ) const
{
	return mybbox_;
}

void
Lang::Type4Shading::show( std::ostream & os ) const
{
	os << "Type4Shading" ;
}

void
Lang::Type4Shading::gcMark( Kernel::GCMarkedSet & marked )
{
	// At the time of writing, there is nothing to propagate to.
}
