#ifndef fontmetrics_h
#define fontmetrics_h

#include "FontMetrics_decls.h"

#include "charptrless.h"
#include "ptrowner.h"

#include <vector>
#include <map>
#include <list>

namespace FontMetrics
{
	class CharacterMetrics
	{
	public:
		size_t internalPosition_; // this is the position of this character in the containing charData_ vector of a WritingDirectionMetrics object.
		int characterCode_;
		double horizontalCharWidthX_;
		double horizontalCharWidthY_;
		double verticalCharWidthX_;
		double verticalCharWidthY_;
		double xmin_;
		double ymin_;
		double xmax_;
		double ymax_;
		double vX_;
		double vY_;
		
	private:
		// The following fields are mutable since they cannot be synchronized at an early stage, yet the object is not really
		// ready for access until synchronized.	Hence, it should be safe to store const pointers to objects of this type
		// in WritingDirectionMetrics, which are then synchronized once all characters are known by name.
		mutable std::map< size_t, size_t > ligatures_;
		mutable std::map< RefCountPtr< const char >, RefCountPtr< const char >, charRefPtrLess > * ligatureSetupMap_;
		
	public:
		CharacterMetrics( size_t internalPosition )
			: internalPosition_( internalPosition ),
				characterCode_( -1 ),
				horizontalCharWidthX_( 0 ),
				horizontalCharWidthY_( 0 ),
				verticalCharWidthX_( 0 ),
				verticalCharWidthY_( 0 ),
				xmin_( 0 ),
				ymin_( 0 ),
				xmax_( 0 ),
				ymax_( 0 ),
				vX_( 0 ),
				vY_( 0 ),
				ligatureSetupMap_( 0 )
		{ }
		~CharacterMetrics( );
		bool isEmpty( ) const;
		bool hasLigature( size_t otherInternalPosition, size_t * ligatureInternalPosition ) const;
		void addLigature( RefCountPtr< const char > otherName, RefCountPtr< const char > ligatureName );
		void setupLigatures( const std::map< RefCountPtr< const char >, size_t, charRefPtrLess > & nameMap ) const;

		void display( std::ostream & os ) const;
	};
	
	class WritingDirectionMetrics
	{
	public:
		double underlinePosition_;
		double underlineThickness_;
		double italicAngleRadians_;
		double charWidthX_;
		double charWidthY_;
		bool isFixedPitch_;
		
		PtrOwner_back_Access< std::vector< const CharacterMetrics * > > charData_;
		
		// The size_t is an index into charData_;
		std::map< RefCountPtr< const char >, size_t, charRefPtrLess > nameMap_;
		std::vector< size_t > codeMap_;
		
		WritingDirectionMetrics( );
		~WritingDirectionMetrics( );
		
		void setupLigatures( );
		const CharacterMetrics * charByName( const char * name ) const;
		const CharacterMetrics * charByCode( unsigned char code ) const;
		const CharacterMetrics * charByCode( char code ) const;	// shall not be used; generates an error
		
		void display( std::ostream & os ) const;
	};
	
	class TrackKerning
	{
		double sizeLow_;
		double trackLow_;
		double sizeHigh_;
		double trackHigh_;
	public:
		TrackKerning( double sizeLow, double trackLow, double sizeHigh, double trackHigh );
		double operator () ( double sz ) const;
	};

	class AFM
	{
	public:
		RefCountPtr< const char > fontName_;
		RefCountPtr< const char > fullName_;
		RefCountPtr< const char > familyName_;
		RefCountPtr< const char > weight_;
		size_t weightNumber_;
		double fontBBoxXMin_;
		double fontBBoxYMin_;
		double fontBBoxXMax_;
		double fontBBoxYMax_;
		RefCountPtr< const char > version_;
		RefCountPtr< const char > notice_;
		RefCountPtr< const char > encodingScheme_;
		RefCountPtr< const char > characterSet_;
		size_t charCount_;
		bool isCIDFont_;
		double capHeight_;
		double xHeight_;
		double ascender_;
		double descender_;
		double leading_;
		double stdHW_;
		double stdVW_;
		
		double vVectorX_;
		double vVectorY_;
		bool isFixedV_;
		// Either of these may be null.
		RefCountPtr< WritingDirectionMetrics > horizontalMetrics_;
		RefCountPtr< WritingDirectionMetrics > verticalMetrics_;

		std::map< int, RefCountPtr< TrackKerning > > trackKernings_;
		typedef std::map< std::pair< size_t, size_t >, double > KernPairMap;
		KernPairMap horizontalKernPairsX_;
		KernPairMap horizontalKernPairsY_;
		KernPairMap verticalKernPairsX_;
		KernPairMap verticalKernPairsY_;

		typedef std::pair< RefCountPtr< const char >, RefCountPtr< const char > > AssortedInfo;
		std::list< AssortedInfo > assortedGlobalInfo_;
		std::list< AssortedInfo > comments_;

		AFM( )
			: fontName_( NullPtr< const char >( ) ),
				fullName_( NullPtr< const char >( ) ),
				familyName_( NullPtr< const char >( ) ),
				weight_( NullPtr< const char >( ) ),
				weightNumber_( 0 ),
				version_( NullPtr< const char >( ) ),
				notice_( NullPtr< const char >( ) ),
				encodingScheme_( NullPtr< const char >( ) ),
				characterSet_( NullPtr< const char >( ) ),
				leading_( 0 ),
				isFixedV_( false ),
				horizontalMetrics_( NullPtr< WritingDirectionMetrics >( ) ),
				verticalMetrics_( NullPtr< WritingDirectionMetrics >( ) )
		{ }
		virtual ~AFM( );

		virtual bool isBaseFont( ) const = 0;

		double getHorizontalKernPairXByCode( unsigned char code1, unsigned char code2 ) const;
		double getHorizontalKernPairXByCode( char code1, char code2 ) const;	// generates error
	};
	
	class BaseFont : public AFM
	{
	public:
		BaseFont( )
		{ }
		~BaseFont( );
		
		virtual bool isBaseFont( ) const { return true; }
	};

}

#endif
