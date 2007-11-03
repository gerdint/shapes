#ifndef errorhandlers_h
#define errorhandlers_h

#include "functiontypes.h"
#include "shapesexceptions.h"

namespace Shapes
{  
  namespace Lang
  {

  class ErrorHandler : public Lang::Function
  {
  public:
    ErrorHandler( );
    virtual ~ErrorHandler( );
    virtual bool isTransforming( ) const;
    virtual void gcMark( Kernel::GCMarkedSet & marked ){ };
  };

  template< class T >
    class ExceptionWrapper : public Lang::ErrorHandler
  {
  protected:
    const char * title;
    const char * msg;
  public:
    ExceptionWrapper( const char * _title, const char * _msg );
    virtual ~ExceptionWrapper( );
    virtual void call( Kernel::EvalState * evalState, Kernel::Arguments & args, const Ast::SourceLocation & callLoc ) const;
  };

  }
}


template< class T >
Shapes::Lang::ExceptionWrapper< T >::ExceptionWrapper( const char * _title, const char * _msg )
  : title( _title ), msg( _msg )
{ }

template< class T >
Shapes::Lang::ExceptionWrapper< T >::~ExceptionWrapper( )
{ }

template< class T >
void
Shapes::Lang::ExceptionWrapper< T >::call( Kernel::EvalState * evalState, Kernel::Arguments & args, const Ast::SourceLocation & callLoc ) const
{
  throw T( msg );
}

#endif
