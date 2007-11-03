#ifndef debuglog_h
#define debuglog_h

#include <iostream>
#include <fstream>
#include <string>


namespace Shapes
{
  namespace Kernel
  {
    class DebugLog
    {
      std::ostream * os_;
      std::string filename_;
      std::ofstream myFile_;
    public:
      DebugLog( );
      ~DebugLog( );
      bool initialized( ) const;
      void setStream( std::ostream * os );
      void setFilename( const std::string & filename );
      std::ostream & os( );
    };
  }
}


#endif
