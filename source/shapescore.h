#ifndef metapdfcore_h
#define metapdfcore_h

#include <list>
#include <iostream>
#include <string>

#include "refcount.h"
#include "metapdftypes.h"


namespace MetaPDF
{
  namespace Lang
  {

    class CoreFunction : public Lang::Function
    {
    protected:
      const char * title_;
    public:
      CoreFunction( const char * title );
      CoreFunction( const char * title, Kernel::EvaluatedFormals * formals );
      virtual void analyze( Ast::Node * parent, const Ast::AnalysisEnvironment * env );
      virtual bool isTransforming( ) const;
      const char * getTitle( ) const;
      virtual void gcMark( Kernel::GCMarkedSet & marked ){ };
    };
    
  }
  
}

#endif

