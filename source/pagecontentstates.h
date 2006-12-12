#ifndef pagecontentstates_h
#define pagecontentstates_h


#include "statetypes.h"
#include "texttypes.h"
#include "simplepdfo.h"

namespace MetaPDF
{
  namespace Kernel
  {

    class PageContentStates
    {
    public:
      Kernel::GraphicsState graphics_;
      Kernel::TextState text_;
      RefCountPtr< SimplePDF::PDF_Resources > resources_;

      PageContentStates( RefCountPtr< SimplePDF::PDF_Resources > & resources );
      PageContentStates( RefCountPtr< SimplePDF::PDF_Resources > & resources, bool setDefaults );
      ~PageContentStates( );
    };
 
 }
}


#endif


