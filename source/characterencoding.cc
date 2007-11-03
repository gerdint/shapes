#include "characterencoding.h"
#include "shapesexceptions.h"


using namespace FontMetrics;

//   class CharacterEncoding
//   {
//   public:
//     // A character encoding is a primitive thing which can only handle 256 code points.
//     typedef unsigned char PositionType;
//   private:
//     RefCountPtr< const char > encodingName_;

//     // This is only a memory, with no particular order of its items.
//     PtrOwner_back_Access< std::vector< const char * > > nameMem_;

//     // This is idexed by size_t and maps to a name or a null pointer.
//     static const size_t TABLE_SIZE = 256;
//     std::vector< const char * > namePtrs_;

//     // This is the reverse map.
//     std::map< const char *, PositionType, charPtrLess > nameMap_;

//   public:

CharacterEncoding::CharacterEncoding( std::istream & iFile )
  : encodingName_( NullPtr< const char >( ) )
{
  namePtrs_.resize( TABLE_SIZE, 0 );
  readfile( iFile );
}

CharacterEncoding::~CharacterEncoding( )
{ }
    
// Returns true on success.
bool
CharacterEncoding::position_to_name( PositionType code, const char ** dst ) const
{
  *dst = nameMem_[ code ];
  return *dst != 0;
}

// Returns true on success.
bool
CharacterEncoding::name_to_position( const char * name, PositionType * dst ) const
{
  typedef typeof nameMap_ MapType;
  MapType::const_iterator i = nameMap_.find( name );
  if( i == nameMap_.end( ) )
    {
      return false;
    }
  *dst = i->second;
  return true;
}

void
CharacterEncoding::readfile( std::istream & iFile )
{
  std::string token;
  getToken( iFile, & token );
  if( token[0] != '/' )
    {
      throw "Expected a leading / in the name of the character encoding.";
    }
  encodingName_ = strrefdup( token.c_str( ) + 1 );

  getToken( iFile, & token );
  // This is a bracket.  Skip it.

  getToken( iFile, & token );
  
  for( unsigned char pos = 0; token[ 0 ] != ']'; ++pos, getToken( iFile, & token ) )
    {
      if( token[ 1 ] != '.' )
	{
	  nameMem_.push_back( strdup( token.c_str( ) + 1 ) );
	  namePtrs_[ pos ] = nameMem_.back( );
	  nameMap_[ nameMem_.back( ) ] = pos;
	  //	  std::cerr << nameMem_.back( ) << " --> " << static_cast< int >( pos ) << std::endl ;
	}
    }
}

void
CharacterEncoding::getToken( std::istream & iFile, std::string * dst )
{
  iFile >> *dst ;
  while( (*dst)[ 0 ] == '%' )
    {
      std::getline( iFile, *dst );
      iFile >> *dst ;
    }
}
