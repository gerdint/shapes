#include "pagecontentstates.h"

using namespace Shapes;


Kernel::PageContentStates::PageContentStates( RefCountPtr< SimplePDF::PDF_Resources > & resources, bool setDefaults )
  : graphics_( true ), text_( true ), resources_( resources )
{
  if( ! setDefaults )
    {
      throw Exceptions::InternalError( strrefdup( "setDefaults must be true in PageContentStates::PageContentStates." ) );
    }

  text_.font_ = NullPtr< const Lang::Font >( );
  text_.size_ = Concrete::Length( std::numeric_limits< double >::signaling_NaN( ) );
  text_.setLeading( Concrete::Length( 0 ) );
}

// Use default constructors for the members.
Kernel::PageContentStates::PageContentStates( RefCountPtr< SimplePDF::PDF_Resources > & resources )
  :  resources_( resources )
{ }

Kernel::PageContentStates::~PageContentStates( )
{ }

