#ifndef glyphlist_h
#define glyphlist_h


#include "ptrowner.h"
#include "charptrless.h"

#include <vector>
#include <map>
#include <iostream>

// This may not be the appropriate namespace...
namespace FontMetrics
{

	class GlyphList
	{
	public:
		// This shall be encoded as iconv's Helpers::theUCS4EncodingName.
		// Please refer to charconverters.h for the exact meaning of this.
		typedef uint32_t UnicodeType;
	private:
		// This is only a memory, with no particular order of its items.
		PtrOwner_back_Access< std::vector< const char * > > nameMem_;

		// This is idexed by size_t and maps to a name or a null pointer.
		static const size_t TABLE_SIZE = 65536;
		std::vector< const char * > namePtrs_;
		std::map< UnicodeType, const char * > namePtrsWide_;

		// This is the reverse map.
		std::map< const char *, UnicodeType, charPtrLess > nameMap_;

	public:
		GlyphList( std::istream & iFile );
		~GlyphList( );

		// Returns true on success.
		bool UCS4_to_name( UnicodeType code, const char ** dst ) const;
		bool UTF8_to_name( const char * code, const char ** dst ) const;

		// Returns true on success.
		bool name_to_UCS4( const char * name, UnicodeType * dst ) const;

		size_t size( ) const;

	private:
		void readfile( std::istream & iFile );
	};

}

#endif
