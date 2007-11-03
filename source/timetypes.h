#ifndef timetypes_h
#define timetypes_h

#include "refcount.h"
#include "shapesvalue.h"

#include <ctime>

namespace Shapes
{
  
  namespace Lang
  {

    class ChronologicalTime : public Lang::NoOperatorOverloadValue
    {
      time_t t_;
    public:
      ChronologicalTime( time_t t );
      virtual ~ChronologicalTime( );
      const struct tm * temporary_localtime( ) const;
      const time_t & val( ) const { return t_; }
      virtual Kernel::VariableHandle getField( const char * fieldID, const RefCountPtr< const Lang::Value > & selfRef ) const;
      virtual void show( std::ostream & os ) const;
      virtual void gcMark( Kernel::GCMarkedSet & marked ){ };
      TYPEINFODECL;
    };

    /*
    class ChronologicalDuration : public Lang::Value
    {
      time_t diff_;
    public:
      ChronologicalDuration( time_t diff );
      virtual Kernel::VariableHandle getField( const char * fieldID, const RefCountPtr< const Lang::Value > & selfRef ) const;
      virtual void show( std::ostream & os ) const;
      virtual void gcMark( Kernel::GCMarkedSet & marked ){ };
      TYPEINFODECL;
      virtual Kernel::QuickTypeID getTypeID( ) const { return Kernel::TYPEID_NoOperatorOverloadValue; };
    };
    */
  }
}

#endif
