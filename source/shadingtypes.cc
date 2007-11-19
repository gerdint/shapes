#include "shadingtypes.h"
#include "statetypes.h"
#include "globals.h"
#include "pathtypes.h"
#include "pagecontentstates.h"

using namespace Shapes;


Lang::Type4ShadingGray::Type4ShadingGray( RefCountPtr< SimplePDF::PDF_Object > & resource, RefCountPtr< const Lang::ElementaryPath2D > mybbox )
	: Lang::PaintedPolygon2D( Kernel::THE_NO_STATE, mybbox ), resource_( resource ), mybbox_( mybbox )
{ }

Lang::Type4ShadingGray::~Type4ShadingGray( )
{ }

void
Lang::Type4ShadingGray::shipout( std::ostream & os, Kernel::PageContentStates * pdfState, const Lang::Transform2D & tf ) const
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
Lang::Type4ShadingGray::bbox( ) const
{
	return mybbox_;
}

void
Lang::Type4ShadingGray::show( std::ostream & os ) const
{
	os << "Type4ShadingGray" ;
}

void
Lang::Type4ShadingGray::gcMark( Kernel::GCMarkedSet & marked )
{
	// At the time of writing, there is nothing to propagate to.
}
