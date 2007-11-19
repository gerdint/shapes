#include "fontmetrics.h"
#include "strrefdup.h"


FontMetrics::CharacterMetrics::~CharacterMetrics( )
{
	if( ligatureSetupMap_ != 0 )
		{
			delete ligatureSetupMap_;
		}
}

bool
FontMetrics::CharacterMetrics::isEmpty( ) const
{
	return xmax_ == 0 && ymax_ == 0&& xmin_ == 0 && ymin_ == 0;
}

bool
FontMetrics::CharacterMetrics::hasLigature( size_t otherInternalPosition, size_t * ligatureInternalPosition ) const
{
	typedef typeof( ligatures_ ) MapType;
	MapType::const_iterator i = ligatures_.find( otherInternalPosition );
	if( i == ligatures_.end( ) )
		{
			return false;
		}
	*ligatureInternalPosition = i->second;
	return true;
}

void
FontMetrics::CharacterMetrics::addLigature( RefCountPtr< const char > otherName, RefCountPtr< const char > ligatureName )
{
	typedef typeof( *ligatureSetupMap_ ) MapType;
	if( ligatureSetupMap_ == 0 )
		{
			ligatureSetupMap_ = new MapType;;
		}
	ligatureSetupMap_->insert( MapType::value_type( otherName, ligatureName ) );
}

void
FontMetrics::CharacterMetrics::setupLigatures( const std::map< RefCountPtr< const char >, size_t, charRefPtrLess > & nameMap ) const
{
	if( ligatureSetupMap_ == 0 )
		{
			return;
		}
	
	typedef typeof( *ligatureSetupMap_ ) MapType;
	typedef typeof( nameMap ) NameMapType;
	for( MapType::const_iterator i = ligatureSetupMap_->begin( ); i != ligatureSetupMap_->end( ); ++i )
		{
			NameMapType::const_iterator iFirst = nameMap.find( i->first );
			NameMapType::const_iterator iSecond = nameMap.find( i->second );
			if( iFirst == nameMap.end( ) || iSecond == nameMap.end( ) )
				{
					throw strrefdup( "Font metrics ligature uses undefined character name." );
				}
			ligatures_[ iFirst->second ] = iSecond->second;
		}
	
	delete ligatureSetupMap_;
	ligatureSetupMap_ = 0;
}

void
FontMetrics::CharacterMetrics::display( std::ostream & os ) const
{
		os << "[" << internalPosition_ << "] "
			 << " C " << characterCode_ << ";"
			 << " W0 " << horizontalCharWidthX_ << " "	<< horizontalCharWidthY_ << ";"
			 << " B " << xmin_ << " "	<< ymin_ << " " << xmax_ << " "	<< ymax_ << ";" ;
}

FontMetrics::WritingDirectionMetrics::WritingDirectionMetrics( )
{
	// charData_ begins with a default entry.	This makes it possible to use 0 for "undefined" in codeMap_.
	FontMetrics::CharacterMetrics * defaultChar = new FontMetrics::CharacterMetrics( charData_.size( ) );
	charData_.push_back( defaultChar );
	// The values of defaultChar are not set here, since we need both horizontal and vertical information for that.

	codeMap_.resize( 256, 0 );
}

FontMetrics::WritingDirectionMetrics::~WritingDirectionMetrics( )
{ }

void
FontMetrics::WritingDirectionMetrics::setupLigatures( )
{
	typedef typeof( charData_ ) ListType;
	for( ListType::iterator i = charData_.begin( ); i != charData_.end( ); ++i )
		{
			(*i)->setupLigatures( nameMap_ );
		}
}

const FontMetrics::CharacterMetrics *
FontMetrics::WritingDirectionMetrics::charByName( const char * name ) const
{
	typedef typeof( nameMap_ ) NameMapType;
	NameMapType::const_iterator i = nameMap_.find( strrefdup( name ) );
	if( i == nameMap_.end( ) )
		{
			throw "No character by that name in font metrics.";
		}
	return charData_[ i->second ];
}

const FontMetrics::CharacterMetrics *
FontMetrics::WritingDirectionMetrics::charByCode( unsigned char code ) const
{
	return charData_[ codeMap_[ code ] ];
}

const FontMetrics::CharacterMetrics *
FontMetrics::WritingDirectionMetrics::charByCode( char code ) const
{
	throw "WritingDirectionMetrics::charByCode called with signed argument type.";
}

void
FontMetrics::WritingDirectionMetrics::display( std::ostream & os ) const
{
	typedef typeof( nameMap_ ) NameMapType;
	for( NameMapType::const_iterator i = nameMap_.begin( ); i != nameMap_.end( ); ++i )
		{
			os << "\"" << i->first << "\" " ;
			charData_[ i->second ]->display( os );
			os << std::endl ;
		}
}

FontMetrics::TrackKerning::TrackKerning( double sizeLow, double trackLow, double sizeHigh, double trackHigh )
	: sizeLow_( sizeLow ), trackLow_( trackLow ), sizeHigh_( sizeHigh ), trackHigh_( trackHigh )
{ }

double
FontMetrics::TrackKerning::operator () ( double sz ) const
{
	if( sz < sizeLow_ )
		{
			return trackLow_;
		}
	if( sz > sizeHigh_ )
		{
			return trackHigh_;
		}
	return trackLow_ + ( ( sz - sizeLow_ ) / ( sizeHigh_ - sizeLow_ ) ) * ( trackHigh_ - trackLow_ );
}


FontMetrics::AFM::~AFM( )
{ }

double
FontMetrics::AFM::getHorizontalKernPairXByCode( unsigned char code1, unsigned char code2 ) const
{
	KernPairMap::const_iterator i = horizontalKernPairsX_.find( KernPairMap::key_type( horizontalMetrics_->codeMap_[ code1 ], horizontalMetrics_->codeMap_[ code2 ] ) );
	if( i == horizontalKernPairsX_.end( ) )
		{
			return 0;
		}
	return i->second;
}

double
FontMetrics::AFM::getHorizontalKernPairXByCode( char code1, char code2 ) const
{
	throw "AFM::getHorizontalKernPairXByCode called with signed argument type.";
}

FontMetrics::BaseFont::~BaseFont( )
{ }
