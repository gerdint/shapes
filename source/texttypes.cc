#include "Shapes_Helpers_decls.h"

#include "texttypes.h"
#include "dynamicenvironment.h"
#include "lighttypes.h"
#include "ast.h"
#include "isnan.h"
#include "globals.h"
#include "autoonoff.h"
#include "afmscanner.h"
#include "charconverters.h"
#include "constructorrepresentation.h"
#include "pagecontentstates.h"
#include "config.h"

#include <fstream>
#include <sys/types.h>
#include <sys/stat.h>

using namespace Shapes;

std::map< RefCountPtr< const char >, RefCountPtr< SimplePDF::PDF_Object >, charRefPtrLess > Lang::Font::theFontResourceMap_;
std::map< RefCountPtr< const char >, RefCountPtr< const FontMetrics::BaseFont >, charRefPtrLess > Lang::Font::theFontMetricsMap_;
std::list< std::string > Lang::Font::theFontMetricsSearchPath_;

void
Lang::Font::push_backFontMetricsPath( const char * path )
{
	theFontMetricsSearchPath_.push_back( path );
}

std::string
Lang::Font::searchGlyphList( )
{
	std::string res;

	if( theFontMetricsSearchPath_.size( ) == 0 )
		{
			throw Exceptions::ExternalError( strrefdup( "The font metrics path was not set up (needed for the glyph list).	Consider defining the environment variable SHAPESFONTMETRICS." ) );
		}

	typedef typeof theFontMetricsSearchPath_ ListType;
	for( ListType::const_iterator i = theFontMetricsSearchPath_.begin( ); i != theFontMetricsSearchPath_.end( ); ++i )
		{
			res = *i + "/" + "glyphlist.txt";
			struct stat theStatDummy;
			if( stat( res.c_str( ), & theStatDummy ) == 0 )
				{
					return res;
				}
		}
	throw Exceptions::MiscellaneousRequirement( "A font operation required the glyph list, but it was not found.	It should be named \"glyphlist.txt\" and reside in a directory on the font metrics path.	Please refer to your environment variable SHAPESFONTMETRICS." );
}

std::string
Lang::Font::searchFontMetrics( RefCountPtr< const char > fontName )
{
	std::string res;

	if( strlen( fontName.getPtr( ) ) == 0 )
		{
			throw Exceptions::InternalError( strrefdup( "Lang::Font::searchFontMetrics called with empty argument." ) );
		}

	if( *fontName.getPtr( ) == '/' )
		{
			throw Exceptions::InternalError( "The font name cannot begin with \"/\", as if is was an absolute path to something." );
		}
	
	if( theFontMetricsSearchPath_.size( ) == 0 )
		{
			throw Exceptions::ExternalError( strrefdup( "The font metrics path was not set up.	Consider defining the environment variable SHAPESFONTMETRICS." ) );
		}

	typedef typeof theFontMetricsSearchPath_ ListType;
	for( ListType::const_iterator i = theFontMetricsSearchPath_.begin( ); i != theFontMetricsSearchPath_.end( ); ++i )
		{
			res = *i + "/" + fontName.getPtr( ) + ".afm";
			struct stat theStatDummy;
			if( stat( res.c_str( ), & theStatDummy ) == 0 )
				{
					return res;
				}
		}
	throw Exceptions::MissingFontMetrics( fontName, & theFontMetricsSearchPath_ );
}

std::string
Lang::Font::searchCharacterEncoding( const char * encodingName )
{
	std::string res;

	if( theFontMetricsSearchPath_.size( ) == 0 )
		{
			throw Exceptions::ExternalError( strrefdup( "The font metrics path was not set up.	Consider defining the environment variable SHAPESFONTMETRICS." ) );
		}

	typedef typeof theFontMetricsSearchPath_ ListType;
	for( ListType::const_iterator i = theFontMetricsSearchPath_.begin( ); i != theFontMetricsSearchPath_.end( ); ++i )
		{
			res = *i + "/" + encodingName + ".enc";
			struct stat theStatDummy;
			if( stat( res.c_str( ), & theStatDummy ) == 0 )
				{
					return res;
				}
		}
	std::ostringstream msg;
	msg << "A font operation required a character encoding file for " << encodingName << ", but it was not found.	It should be named \"" << encodingName << ".enc\" and reside in a directory on the font metrics path.	Please refer to your environment variable SHAPESFONTMETRICS." ;
	throw Exceptions::MiscellaneousRequirement( strrefdup( msg ) );
}


Lang::Font::Font( const RefCountPtr< const char > fontName, RefCountPtr< SimplePDF::PDF_Object > & resource, RefCountPtr< const FontMetrics::BaseFont > metrics )
	: fontName_( fontName ), resource_( resource ), metrics_( metrics )
{
	// If this font has been instantiated before, it is reused.
	{
		typedef typeof theFontResourceMap_ MapType;
		MapType::const_iterator i = theFontResourceMap_.find( fontName_ );
		if( i != theFontResourceMap_.end( ) )
			{
				resource_ = i->second;
			}
		else
			{
				theFontResourceMap_.insert( MapType::value_type( fontName_, resource_ ) );
			}
	}
	{
		typedef typeof theFontMetricsMap_ MapType;
		MapType::const_iterator i = theFontMetricsMap_.find( fontName_ );
		if( i != theFontMetricsMap_.end( ) )
			{
				metrics_ = i->second;
			}
		else if( metrics_ != NullPtr< const FontMetrics::BaseFont >( ) )
			{
				theFontMetricsMap_.insert( MapType::value_type( fontName_, metrics_ ) );
			}
	}
}

// Note that we must not initialize name_ with builtinFontMap_[ fontConst ] here, since builtinFontMap_ may not be
// initialized when this constructor is invoked from globals.cc.
Lang::Font::Font( const RefCountPtr< const char > fontName )
	: fontName_( fontName ), resource_( NullPtr< SimplePDF::PDF_Object >( ) ), metrics_( NullPtr< const FontMetrics::BaseFont >( ) )
{ }

Lang::Font::~Font( )
{ }

const RefCountPtr< SimplePDF::PDF_Object > &
Lang::Font::resource( ) const
{
	if( resource_ != NullPtr< SimplePDF::PDF_Object >( ) )
		{
			return resource_;
		}

	typedef typeof theFontResourceMap_ MapType;

	{
		MapType::const_iterator i = theFontResourceMap_.find( fontName_ );
		if( i != theFontResourceMap_.end( ) )
			{
				resource_ = i->second;
				return resource_;
			}
	}

	{
		RefCountPtr< SimplePDF::PDF_Dictionary > dic;
		resource_ = Kernel::the_pdfo->indirect( dic );
		(*dic)[ "Type" ] = SimplePDF::PDF_out::newName( "Font" );
		(*dic)[ "Subtype" ] = SimplePDF::PDF_out::newName( "Type1" );
		(*dic)[ "Encoding" ] = SimplePDF::PDF_out::newName( "MacRomanEncoding" );
		(*dic)[ "BaseFont" ] = SimplePDF::PDF_out::newName( fontName_.getPtr( ) );	// Here, it is crucial that this is really a build-in font!
	}
	return resource_;
}

RefCountPtr< const char >
Lang::Font::fontName( ) const
{
	return fontName_;
}

RefCountPtr< const FontMetrics::BaseFont >
Lang::Font::metrics( ) const
{
	if( metrics_ != NullPtr< const FontMetrics::BaseFont >( ) )
		{
			return metrics_;
		}

	// Otherwise, we hope that we can find an Adobe Font Metrics file.
	std::string filename = searchFontMetrics( fontName_ );
	std::ifstream afmFile( filename.c_str( ) );
	if( ! afmFile.is_open( ) )
		{
			std::ostringstream oss;
			oss << "File has been located but couldn't be opened: " << filename ;
			throw Exceptions::ExternalError( strrefdup( oss ) );
		}
	
	// I fiddle a little with the consts here...
	FontMetrics::BaseFont * newMetrics = new FontMetrics::BaseFont;
	metrics_ = RefCountPtr< const FontMetrics::BaseFont >( newMetrics );

	AfmScanner scanner( newMetrics, & afmFile );
	scanner.setTellQue( Interaction::fontMetricMessages ); // We want to know about things that are not recognized and this ignored.
	if( Interaction::fontMetricDebug )
		{
			scanner.set_debug( 1 );
		}
	try
		{
			int status = scanner.yylex( );
			if( status != 0 )
				{
					std::ostringstream oss;
					oss << "Font metrics parser returned with non-zero status: " << status ;
					throw Exceptions::InternalError( strrefdup( oss ) );
				}
		}
	catch( const char * ball )
		{
			std::ostringstream oss;
			oss << "Font metrics parser failed with message: " << ball ;
			throw Exceptions::InternalError( strrefdup( oss ) );
		}
	catch( const RefCountPtr< const char > ball )
		{
			std::ostringstream oss;
			oss << "Font metrics parser failed with message: " << ball ;
			throw Exceptions::InternalError( strrefdup( oss ) );
		}

	return metrics_;
}

void
Lang::Font::gcMark( Kernel::GCMarkedSet & marked )
{ }

RefCountPtr< const Lang::Class > Lang::Font::TypeID( new Lang::SystemFinalClass( strrefdup( "Font" ) ) );
TYPEINFOIMPL( Font );

Lang::TextOperation::TextOperation( )
{ }

Lang::TextOperation::~TextOperation( )
{ }

RefCountPtr< const Lang::Class > Lang::TextOperation::TypeID( new Lang::SystemFinalClass( strrefdup( "TextOperation" ) ) );
TYPEINFOIMPL( TextOperation );


Lang::KernedText::KernedText( const RefCountPtr< const Kernel::TextState > & textState, const RefCountPtr< const Kernel::GraphicsState > & metaState )
	: textState_( textState ), metaState_( metaState ), maxLength_( 0 )
{ }

Lang::KernedText::KernedText( const RefCountPtr< const Kernel::TextState > & textState, const RefCountPtr< const Kernel::GraphicsState > & metaState, const RefCountPtr< const Lang::String > & str)
	: textState_( textState ), metaState_( metaState ), maxLength_( 0 )
{
	pushString( str );
}

Lang::KernedText::~KernedText( )
{ }

Kernel::VariableHandle
Lang::KernedText::getField( const char * fieldID, const RefCountPtr< const Lang::Value > & selfRef ) const
{
	if( strcmp( fieldID, "list" ) == 0 )
		{
			return Kernel::VariableHandle( new Kernel::Variable( makeList( ) ) );
		}
	throw Exceptions::NonExistentMember( getTypeName( ), fieldID );
}

RefCountPtr< const Lang::SingleList >
Lang::KernedText::makeList( ) const
{
	/* The list is first computed in a bidirectional list, and then we construct the stateless cons list.
	 */
	
	/* I'm lazy today, so i use a cons pair to group the horizontal step with the glyph.	The better solution
	 * would be to use a structure with nicely named fields...
	 */
	
	std::list< RefCountPtr< const Lang::Value > > revlist;

	iconv_t converter = Helpers::requireUTF8ToMacRomanConverter( );

	RefCountPtr< FontMetrics::WritingDirectionMetrics > horizontalMetrics = textState_->font_->metrics( )->horizontalMetrics_;
	if( horizontalMetrics == NullPtr< FontMetrics::WritingDirectionMetrics >( ) )
		{
			throw Exceptions::FontMetricsError( textState_->font_->fontName( ), strrefdup( "No horizontal metrics defined." ) );
		}
	const FontMetrics::CharacterMetrics * defaultCharMetrics = horizontalMetrics->charData_[ 0 ];

	Concrete::Length ySize = textState_->size_;
	Concrete::Length xSize = ySize * textState_->horizontalScaling_;
	Concrete::Length characterTrackKern = textState_->horizontalScaling_ * textState_->characterSpacing_;
	Concrete::Length wordTrackKern = textState_->horizontalScaling_ * textState_->wordSpacing_;

	Concrete::Length xpos = 0;

	size_t bufSize = maxLength_ + 1;				 // This will be enough if MacRoman coding is used, since this encoding uses only one byte per character.
																					 // The extra one byte will be used to terminate the string.
	char * buf = new char[ bufSize ];
	DeleteOnExit< char > bufDeleter( buf );

	typedef typeof strings_ ListType;
	std::list< double >::const_iterator ki = kernings_.begin( );
	for( ListType::const_iterator i = strings_.begin( ); i != strings_.end( ); ++i )
		{
			if( *i != NullPtr< const Lang::String >( ) )
				{
					const char * inbuf = (*i)->val_.getPtr( );
					char * outbuf = buf;
					size_t inbytesleft = strlen( inbuf );
					size_t outbytesleft = bufSize - 1;
					// The ICONV_CAST macro is defined in config.h.
					size_t count = iconv( converter,
																ICONV_CAST( & inbuf ), & inbytesleft,
																& outbuf, & outbytesleft );
					if( count == (size_t)(-1) )
						{
							if( errno == EILSEQ )
								{
									throw Exceptions::MiscellaneousRequirement( "It is suspected that one of the UFT-8 characters used in showed text has no MacRoman representation." );
								}
							else if( errno == EINVAL )
								{
									throw Exceptions::MiscellaneousRequirement( "It is suspected that showed text ended with an incomplete multibyte character." );
								}
							else if( errno == E2BIG )
								{
									throw Exceptions::InternalError( "The buffer allocated for UTF-8 to MacRoman conversion was too small." );
								}
							else
								{
									std::ostringstream msg;
									msg << "iconv failed with an unrecognized error code: " << errno ;
									throw Exceptions::InternalError( strrefdup( msg ) );
								}
						}
					*outbuf = '\0';
					for( const char * src = buf; *src != '\0'; ++src )
						{
							switch( *src )
								{
								case ' ':
									{
										// Observe textState_->wordSpacing_
										const FontMetrics::CharacterMetrics * charMetrics = horizontalMetrics->charByCode( (unsigned char)( 32 ) );
										xpos += xSize * charMetrics->horizontalCharWidthX_;
										xpos += characterTrackKern;
										xpos += wordTrackKern;
									}
									break;
								case '\n':
									{
										throw Exceptions::MiscellaneousRequirement( "Newlines cannot be represented in the pos-character list." );
									}
									break;
								default:
									{
										// Observe textState_->characterSpacing_
										unsigned char currentChar = *reinterpret_cast< const unsigned char * >( src );
										const FontMetrics::CharacterMetrics * charMetrics = horizontalMetrics->charByCode( currentChar );
										if( Computation::fontMetricGuessIsError && charMetrics == defaultCharMetrics )
											{
												std::ostringstream msg;
												msg << "Character at offset " << src - buf << " in \"" << buf << "\" was not found in font metrics (and according to your options, guessing is not OK)." ;
												throw Exceptions::FontMetricsError( textState_->font_->fontName( ), strrefdup( msg ) );
											}
										revlist.push_back( RefCountPtr< const Lang::Value >
																			 ( new Lang::ConsPair
																				 ( Helpers::newValHandle( new Lang::Length( xpos ) ),
																					 Helpers::newValHandle( new Lang::KernedText( textState_, metaState_, oneMacRomanToUTF8( *src ) ) ) ) ) );
										/* Although not as efficient, it becomes easier to use the list if the distance is
										 * measured from the start of the text rather than from the previous character.
										 */
										//										xpos = 0;
										xpos += xSize * charMetrics->horizontalCharWidthX_;
										xpos += characterTrackKern;
									}
								}
						}
				}
			else
				{
					if( ki == kernings_.end( ) )
						{
							throw Exceptions::InternalError( "Short of kerning values in KernedText::measure." );
						}
					xpos -= xSize * *ki;
					++ki;
				}
		}
	if( ki != kernings_.end( ) )
		{
			throw Exceptions::InternalError( "Too many kerning values in KernedText::writePDFVectorTo." );
		}
	
	RefCountPtr< const Lang::SingleList > res = Lang::THE_CONS_NULL;
	while( revlist.size( ) > 0 )
		{
			res = RefCountPtr< const Lang::SingleList >( new Lang::SingleListPair( Kernel::VariableHandle( new Kernel::Variable( revlist.back( ) ) ),
																																						 res ) );
			revlist.pop_back( );
		}
	return res;
}

RefCountPtr< const Lang::String >
Lang::KernedText::oneMacRomanToUTF8( const char c )
{
	iconv_t converter = Helpers::requireMacRomanToUTF8Converter( );

	const size_t BUF_SIZE = 9;
	char buf[ BUF_SIZE ];

	static char * charbuf = new char[ 2 ]; // This is a one time leak.
	charbuf[0] = c;
	charbuf[1] = '\0';

	char * outbuf = buf;
	const char * inbuf = charbuf;
	size_t inbytesleft = 1;
	size_t outbytesleft = BUF_SIZE - 1;
	// The ICONV_CAST macro is defined in config.h.
	size_t count = iconv( converter,
												ICONV_CAST( & inbuf ), & inbytesleft,
												& outbuf, & outbytesleft );
	if( count == (size_t)(-1) )
		{
			throw Exceptions::ExternalError( "Conversion of one MacRoman character to UTF-8 failed." );
		}
	*outbuf = '\0';
	return RefCountPtr< const Lang::String >( new Lang::String( strrefdup( buf ) ) );
}

void
Lang::KernedText::pushString( const RefCountPtr< const Lang::String > & str )
{
	strings_.push_back( str );
	maxLength_ = std::max( maxLength_, strlen( str->val_.getPtr( ) ) );
}

void
Lang::KernedText::pushKerning( double kerning )
{
	strings_.push_back( NullPtr< const Lang::String >( ) );
	kernings_.push_back( kerning );
}

void
Lang::KernedText::show( std::ostream & os ) const
{
	typedef typeof strings_ ListType;
	std::list< double >::const_iterator ki = kernings_.begin( );
	for( ListType::const_iterator i = strings_.begin( ); i != strings_.end( ); ++i )
		{
			if( *i != NullPtr< const Lang::String >( ) )
				{
					os << "(" << (*i)->val_.getPtr( ) << ")" ;
				}
			else
				{
					if( ki == kernings_.end( ) )
						{
							throw Exceptions::InternalError( "Short of kerning values in KernedText::show." );
						}
					os << *ki ;
					++ki;
				}
		}
	if( ki != kernings_.end( ) )
		{
			throw Exceptions::InternalError( "Too many kerning values in KernedText::show." );
		}
}


void
Lang::KernedText::shipout( std::ostream & os, Kernel::PageContentStates * pdfState, const Lang::Transform2D & tf ) const
{
	iconv_t converter = Helpers::requireUTF8ToMacRomanConverter( );

	pdfState->text_.synchAssertKnockout( os, textState_.getPtr( ), pdfState->resources_.getPtr( ) );
	switch( textState_->mode_ )
		{
		case Lang::TextRenderingMode::FILL:
			pdfState->graphics_.synchForNonStroke( os, metaState_.getPtr( ), pdfState->resources_.getPtr( ) );
			break;
		case Lang::TextRenderingMode::STROKE:
			pdfState->graphics_.synchForStroke( os, metaState_.getPtr( ), pdfState->resources_.getPtr( ) );
			break;
		case Lang::TextRenderingMode::FILLSTROKE:
			pdfState->graphics_.synchForStroke( os, metaState_.getPtr( ), pdfState->resources_.getPtr( ) );
			pdfState->graphics_.synchForNonStroke( os, metaState_.getPtr( ), pdfState->resources_.getPtr( ) );
			break;
		case Lang::TextRenderingMode::INVISIBLE:
			break;
		case Lang::TextRenderingMode::FILLCLIP:
			pdfState->graphics_.synchForNonStroke( os, metaState_.getPtr( ), pdfState->resources_.getPtr( ) );
			break;
		case Lang::TextRenderingMode::STROKECLIP:
			pdfState->graphics_.synchForStroke( os, metaState_.getPtr( ), pdfState->resources_.getPtr( ) );
			break;
		case Lang::TextRenderingMode::FILLSTROKECLIP:
			pdfState->graphics_.synchForStroke( os, metaState_.getPtr( ), pdfState->resources_.getPtr( ) );
			pdfState->graphics_.synchForNonStroke( os, metaState_.getPtr( ), pdfState->resources_.getPtr( ) );
			break;
		case Lang::TextRenderingMode::CLIP:
			break;
		case Lang::TextRenderingMode::UNDEFINED:
			pdfState->graphics_.synchForStroke( os, metaState_.getPtr( ), pdfState->resources_.getPtr( ) );
			pdfState->graphics_.synchForNonStroke( os, metaState_.getPtr( ), pdfState->resources_.getPtr( ) );
		default:
			throw Exceptions::InternalError( "KernedText::writePDFVectorTo:	Text rendering mode out of range." );
		}


	size_t bufSize = maxLength_ + 1;				 // This will be enough if MacRoman coding is used, since this encoding uses only one byte per character.
																					 // The extra one byte will be used to terminate the string.
	char * buf = new char[ bufSize ];
	DeleteOnExit< char > bufDeleter( buf );

	typedef typeof strings_ ListType;
	std::list< double >::const_iterator ki = kernings_.begin( );
	os << "[ " ;
	for( ListType::const_iterator i = strings_.begin( ); i != strings_.end( ); ++i )
		{
			if( *i != NullPtr< const Lang::String >( ) )
				{
					const char * inbuf = (*i)->val_.getPtr( );
					char * outbuf = buf;
					size_t inbytesleft = strlen( inbuf );
					size_t outbytesleft = bufSize - 1;
					// The ICONV_CAST macro is defined in config.h.
					size_t count = iconv( converter,
																ICONV_CAST( & inbuf ), & inbytesleft,
																& outbuf, & outbytesleft );
					if( count == (size_t)(-1) )
						{
							if( errno == EILSEQ )
								{
									throw Exceptions::MiscellaneousRequirement( "It is suspected that one of the UFT-8 characters used in showed text has no MacRoman representation." );
								}
							else if( errno == EINVAL )
								{
									throw Exceptions::MiscellaneousRequirement( "It is suspected that showed text ended with an incomplete multibyte character." );
								}
							else if( errno == E2BIG )
								{
									throw Exceptions::InternalError( "The buffer allocated for UTF-8 to MacRoman conversion was too small." );
								}
							else
								{
									std::ostringstream msg;
									msg << "iconv failed with an unrecognized error code: " << errno ;
									throw Exceptions::InternalError( strrefdup( msg ) );
								}
						}
					*outbuf = '\0';
					os << "(" ;
					for( const char * src = buf; *src != '\0'; ++src )
						{
							switch( *src )
								{
								case '(':
									os << "\\(" ;
									break;
								case ')':
									os << "\\)" ;
									break;
								case '\n':
									os << ")] TJ T* [(" ;
									break;
								default:
									os << *src ;
								}
						}
					os << ") " ;
				}
			else
				{
					if( ki == kernings_.end( ) )
						{
							throw Exceptions::InternalError( "Short of kerning values in KernedText::writePDFVectorTo." );
						}
					os << 1000 * *ki << " " ;
					++ki;
				}
		}
	if( ki != kernings_.end( ) )
		{
			throw Exceptions::InternalError( "Too many kerning values in KernedText::writePDFVectorTo." );
		}
	
	os << "] TJ " << std::endl ;
}

void
Lang::KernedText::measure( Lang::Transform2D * textMatrix, Lang::Transform2D * textLineMatrix, Concrete::Length * xmin, Concrete::Length * ymin, Concrete::Length * xmax, Concrete::Length * ymax ) const
{
	iconv_t converter = Helpers::requireUTF8ToMacRomanConverter( );

	RefCountPtr< FontMetrics::WritingDirectionMetrics > horizontalMetrics = textState_->font_->metrics( )->horizontalMetrics_;
	if( horizontalMetrics == NullPtr< FontMetrics::WritingDirectionMetrics >( ) )
		{
			throw Exceptions::FontMetricsError( textState_->font_->fontName( ), strrefdup( "No horizontal metrics defined." ) );
		}
	const FontMetrics::CharacterMetrics * defaultCharMetrics = horizontalMetrics->charData_[ 0 ];

	Concrete::Length ySize = textState_->size_;
	Concrete::Length xSize = ySize * textState_->horizontalScaling_;
	Concrete::Length characterTrackKern = textState_->horizontalScaling_ * textState_->characterSpacing_;
	Concrete::Length wordTrackKern = textState_->horizontalScaling_ * textState_->wordSpacing_;

	size_t bufSize = maxLength_ + 1;				 // This will be enough if MacRoman coding is used, since this encoding uses only one byte per character.
																					 // The extra one byte will be used to terminate the string.
	char * buf = new char[ bufSize ];
	DeleteOnExit< char > bufDeleter( buf );

	if( textLineMatrix->isTranslation( ) )
		{
			Concrete::Length x0 = textLineMatrix->xt_;
			Concrete::Length y0 = textLineMatrix->yt_ + textState_->riseConcrete( );
			Concrete::Length xpos = 0;

			typedef typeof strings_ ListType;
			std::list< double >::const_iterator ki = kernings_.begin( );
			for( ListType::const_iterator i = strings_.begin( ); i != strings_.end( ); ++i )
				{
					if( *i != NullPtr< const Lang::String >( ) )
						{
							const char * inbuf = (*i)->val_.getPtr( );
							char * outbuf = buf;
							size_t inbytesleft = strlen( inbuf );
							size_t outbytesleft = bufSize - 1;
							// The ICONV_CAST macro is defined in config.h.
							size_t count = iconv( converter,
																		ICONV_CAST( & inbuf ), & inbytesleft,
																		& outbuf, & outbytesleft );
							if( count == (size_t)(-1) )
								{
									if( errno == EILSEQ )
										{
											throw Exceptions::MiscellaneousRequirement( "It is suspected that one of the UFT-8 characters used in showed text has no MacRoman representation." );
										}
									else if( errno == EINVAL )
										{
											throw Exceptions::MiscellaneousRequirement( "It is suspected that showed text ended with an incomplete multibyte character." );
										}
									else if( errno == E2BIG )
										{
											throw Exceptions::InternalError( "The buffer allocated for UTF-8 to MacRoman conversion was too small." );
										}
									else
										{
											std::ostringstream msg;
											msg << "iconv failed with an unrecognized error code: " << errno ;
											throw Exceptions::InternalError( strrefdup( msg ) );
										}
								}
							*outbuf = '\0';
							for( const char * src = buf; *src != '\0'; ++src )
								{
									switch( *src )
										{
										case ' ':
											{
												// Observe textState_->wordSpacing_
												// In addition, it seems reasonable to not let the space character affect the bounding box.
												const FontMetrics::CharacterMetrics * charMetrics = horizontalMetrics->charByCode( (unsigned char)( 32 ) );
												xpos += xSize * charMetrics->horizontalCharWidthX_;
												xpos += characterTrackKern;
												xpos += wordTrackKern;
											}
											break;
										case '\n':
											{
												textMatrix->prependShift( Concrete::Coords2D( 0, - textState_->leadingConcrete( ) ) );
												textLineMatrix->replaceBy( *textMatrix );
												x0 = textLineMatrix->xt_;
												y0 = textLineMatrix->yt_ + textState_->riseConcrete( );
												xpos = 0;
											}
											break;
										default:
											{
												// Observe textState_->characterSpacing_
												unsigned char currentChar = *reinterpret_cast< const unsigned char * >( src );
												const FontMetrics::CharacterMetrics * charMetrics = horizontalMetrics->charByCode( currentChar );
												if( Computation::fontMetricGuessIsError && charMetrics == defaultCharMetrics )
													{
														std::ostringstream msg;
														msg << "Character at offset " << src - buf << " in \"" << buf << "\" was not found in font metrics (and according to your options, guessing is not OK)." ;
														throw Exceptions::FontMetricsError( textState_->font_->fontName( ), strrefdup( msg ) );
													}
												*xmin = std::min( *xmin, x0 + xpos + xSize * charMetrics->xmin_ );
												*ymin = std::min( *ymin, y0 + ySize * charMetrics->ymin_ );
												*xmax = std::max( *xmax, x0 + xpos + xSize * charMetrics->xmax_ );
												*ymax = std::max( *ymax, y0 + ySize * charMetrics->ymax_ );
												xpos += xSize * charMetrics->horizontalCharWidthX_;
												xpos += characterTrackKern;
											}
										}
								}
						}
					else
						{
							if( ki == kernings_.end( ) )
								{
									throw Exceptions::InternalError( "Short of kerning values in KernedText::measure." );
								}
							xpos -= xSize * *ki;
							++ki;
						}
				}
			if( ki != kernings_.end( ) )
				{
					throw Exceptions::InternalError( "Too many kerning values in KernedText::writePDFVectorTo." );
				}
			
			textLineMatrix->prependXShift( xpos );
		}
	else
		{
			Concrete::Length rise = textState_->riseConcrete( );
			//			std::cerr << "@<< @stroking:RGB_RED | [stroke [shift " << Helpers::shapesFormat( Concrete::Coords2D( 0, 0 ).transformed( *textLineMatrix ) ) << "] [] [circle 2bp]]" << std::endl ;

			typedef typeof strings_ ListType;
			std::list< double >::const_iterator ki = kernings_.begin( );
			for( ListType::const_iterator i = strings_.begin( ); i != strings_.end( ); ++i )
				{
					if( *i != NullPtr< const Lang::String >( ) )
						{
							const char * inbuf = (*i)->val_.getPtr( );
							char * outbuf = buf;
							size_t inbytesleft = strlen( inbuf );
							size_t outbytesleft = bufSize - 1;
							// The ICONV_CAST macro is defined in config.h.
							size_t count = iconv( converter,
																		ICONV_CAST( & inbuf ), & inbytesleft,
																		& outbuf, & outbytesleft );
							if( count == (size_t)(-1) )
								{
									if( errno == EILSEQ )
										{
											throw Exceptions::MiscellaneousRequirement( "It is suspected that one of the UFT-8 characters used in showed text has no MacRoman representation." );
										}
									else if( errno == EINVAL )
										{
											throw Exceptions::MiscellaneousRequirement( "It is suspected that showed text ended with an incomplete multibyte character." );
										}
									else if( errno == E2BIG )
										{
											throw Exceptions::InternalError( "The buffer allocated for UTF-8 to MacRoman conversion was too small." );
										}
									else
										{
											std::ostringstream msg;
											msg << "iconv failed with an unrecognized error code: " << errno ;
											throw Exceptions::InternalError( strrefdup( msg ) );
										}
								}
							*outbuf = '\0';
							for( const char * src = buf; *src != '\0'; ++src )
								{
									switch( *src )
										{
										case ' ':
											{
												// Observe textState_->wordSpacing_
												// In addition, it seems reasonable to not let the space character affect the bounding box.
												const FontMetrics::CharacterMetrics * charMetrics = horizontalMetrics->charByCode( (unsigned char)( 32 ) );
												textLineMatrix->prependXShift( xSize * charMetrics->horizontalCharWidthX_ + characterTrackKern + wordTrackKern );
												//												std::cerr << "@<< @stroking:RGB_BLUE | [stroke [shift " << Helpers::shapesFormat( Concrete::Coords2D( 0, 0 ).transformed( *textLineMatrix ) ) << "] [] [circle 2bp]]" << std::endl ;
											}
											break;
										case '\n':
											{
												textMatrix->prependShift( Concrete::Coords2D( 0, - textState_->leadingConcrete( ) ) );
												textLineMatrix->replaceBy( *textMatrix );
												//												std::cerr << "@<< @stroking:RGB_RED | [stroke [shift " << Helpers::shapesFormat( Concrete::Coords2D( 0, 0 ).transformed( *textLineMatrix ) ) << "] [] [circle 2bp]]" << std::endl ;
											}
											break;
										default:
											{
												// Observe textState_->characterSpacing_
												unsigned char currentChar = *reinterpret_cast< const unsigned char * >( src );
												const FontMetrics::CharacterMetrics * charMetrics = horizontalMetrics->charByCode( currentChar );
												if( Computation::fontMetricGuessIsError && charMetrics == defaultCharMetrics )
													{
														std::ostringstream msg;
														msg << "Character at offset " << src - buf << " in \"" << buf << "\" was not found in the font metrics (and according to your options, guessing is not OK)." ;
														throw Exceptions::FontMetricsError( textState_->font_->fontName( ), strrefdup( msg ) );
													}

												Concrete::Coords2D x0y0 = Concrete::Coords2D( xSize * charMetrics->xmin_, ySize * charMetrics->ymin_ + rise ).transformed( *textLineMatrix );
												Concrete::Coords2D x0y1 = Concrete::Coords2D( xSize * charMetrics->xmin_, ySize * charMetrics->ymax_ + rise ).transformed( *textLineMatrix );
												Concrete::Coords2D x1y0 = Concrete::Coords2D( xSize * charMetrics->xmax_, ySize * charMetrics->ymin_ + rise ).transformed( *textLineMatrix );
												Concrete::Coords2D x1y1 = Concrete::Coords2D( xSize * charMetrics->xmax_, ySize * charMetrics->ymax_ + rise ).transformed( *textLineMatrix );
												//												std::cerr << "@<< @width:0.3bp | [stroke "<< Helpers::shapesFormat( x0y0 ) << "--" << Helpers::shapesFormat( x0y1 ) << "--" << Helpers::shapesFormat( x1y1 ) << "--" << Helpers::shapesFormat( x1y0 ) << "--cycle]" << std::endl ;

												*xmin = std::min( *xmin, x0y0.x_ );
												*ymin = std::min( *ymin, x0y0.y_ );
												*xmax = std::max( *xmax, x0y0.x_ );
												*ymax = std::max( *ymax, x0y0.y_ );

												*xmin = std::min( *xmin, x0y1.x_ );
												*ymin = std::min( *ymin, x0y1.y_ );
												*xmax = std::max( *xmax, x0y1.x_ );
												*ymax = std::max( *ymax, x0y1.y_ );

												*xmin = std::min( *xmin, x1y0.x_ );
												*ymin = std::min( *ymin, x1y0.y_ );
												*xmax = std::max( *xmax, x1y0.x_ );
												*ymax = std::max( *ymax, x1y0.y_ );

												*xmin = std::min( *xmin, x1y1.x_ );
												*ymin = std::min( *ymin, x1y1.y_ );
												*xmax = std::max( *xmax, x1y1.x_ );
												*ymax = std::max( *ymax, x1y1.y_ );

												textLineMatrix->prependXShift( xSize * charMetrics->horizontalCharWidthX_ + characterTrackKern );
												//												std::cerr << "@<< [stroke [shift " << Helpers::shapesFormat( Concrete::Coords2D( 0, 0 ).transformed( *textLineMatrix ) ) << "] [] [circle 2bp]]" << std::endl ;
											}
										}
								}
						}
					else
						{
							if( ki == kernings_.end( ) )
								{
									throw Exceptions::InternalError( "Short of kerning values in KernedText::measure." );
								}
							textLineMatrix->prependXShift( - xSize * *ki );
							++ki;
						}
				}
			if( ki != kernings_.end( ) )
				{
					throw Exceptions::InternalError( "Too many kerning values in KernedText::writePDFVectorTo." );
				}
		}
}

void
Lang::KernedText::push( Lang::KernedText * dst ) const
{
	typedef typeof strings_ ListType;
	std::list< double >::const_iterator ki = kernings_.begin( );
	for( ListType::const_iterator i = strings_.begin( ); i != strings_.end( ); ++i )
		{
			if( *i != NullPtr< const Lang::String >( ) )
				{
					dst->pushString( *i );
				}
			else
				{
					dst->pushKerning( *ki );
					++ki;
				}
		}
}
	
			
void
Lang::KernedText::gcMark( Kernel::GCMarkedSet & marked )
{
	typedef typeof strings_ ListType;
	for( ListType::const_iterator i = strings_.begin( ); i != strings_.end( ); ++i )
		{
			if( *i != NullPtr< const Lang::String >( ) )
				{
					const_cast< Lang::String * >( i->getPtr( ) )->gcMark( marked );
				}
		}
}

Lang::TextNewline::TextNewline( const Concrete::Length tx, const Concrete::Length ty )
	: t_( tx, ty )
{ }

Lang::TextNewline::~TextNewline( )
{ }

void
Lang::TextNewline::show( std::ostream & os ) const
{
	os << "Newline with offset in bp: " << t_ ;
}

void
Lang::TextNewline::shipout( std::ostream & os, Kernel::PageContentStates * pdfState, const Lang::Transform2D & tf ) const
{
	os << t_ << " Td " ;
}

void
Lang::TextNewline::measure( Lang::Transform2D * textMatrix, Lang::Transform2D * textLineMatrix, Concrete::Length * xmin, Concrete::Length * ymin, Concrete::Length * xmax, Concrete::Length * ymax ) const
{
	textMatrix->prependShift( t_ );
	textLineMatrix->replaceBy( *textMatrix );
}

void
Lang::TextNewline::gcMark( Kernel::GCMarkedSet & marked )
{ }

Lang::TextMoveto::TextMoveto( const RefCountPtr< const Lang::Transform2D > & tf )
	: tf_( tf )
{ }

Lang::TextMoveto::~TextMoveto( )
{ }

void
Lang::TextMoveto::show( std::ostream & os ) const
{
	os << "Moveto command" ;
}

void
Lang::TextMoveto::shipout( std::ostream & os, Kernel::PageContentStates * pdfState, const Lang::Transform2D & tf ) const
{
	Lang::Transform2D( tf, *tf_ ).shipout( os );
	os << " Tm" << std::endl ;
}

void
Lang::TextMoveto::measure( Lang::Transform2D * textMatrix, Lang::Transform2D * textLineMatrix, Concrete::Length * xmin, Concrete::Length * ymin, Concrete::Length * xmax, Concrete::Length * ymax ) const
{
	textMatrix->replaceBy( *tf_ );
	textLineMatrix->replaceBy( *textMatrix );
}

void
Lang::TextMoveto::gcMark( Kernel::GCMarkedSet & marked )
{
	const_cast< Lang::Transform2D * >( tf_.getPtr( ) )->gcMark( marked );
}



typedef enum { FILL = 0, STROKE, FILLSTROKE, INVISIBLE, FILLCLIP, STROKECLIP, FILLSTROKECLIP, CLIP, UNDEFINED } ValueType;
			ValueType mode_;

Lang::TextRenderingMode::TextRenderingMode( const ValueType & mode )
	: mode_( mode )
{ }

Lang::TextRenderingMode::TextRenderingMode( bool fill, bool stroke, bool clip )
{
	switch( ( fill	 ? 0x01 : 0x00 ) +
					( stroke ? 0x02 : 0x00 ) +
					( clip	 ? 0x04 : 0x00 ) )
		{
		case 0:
			mode_ = INVISIBLE;
			break;
		case 1:
			mode_ = FILL;
			break;
		case 2:
			mode_ = STROKE;
			break;
		case 3:
			mode_ = FILLSTROKE;
			break;
		case 4:
			mode_ = CLIP;
			break;
		case 5:
			mode_ = FILLCLIP;
			break;
		case 6:
			mode_ = STROKECLIP;
			break;
		case 7:
			mode_ = FILLSTROKECLIP;
			break;
		default:
			throw Exceptions::InternalError( "Semi-static switch out of range in TextRenderingMode::TextRenderingMode." );
		}
}

Lang::TextRenderingMode::~TextRenderingMode( )
{ }

RefCountPtr< const Lang::Class > Lang::TextRenderingMode::TypeID( new Lang::SystemFinalClass( strrefdup( "TextRenderingMode" ) ) );
TYPEINFOIMPL( TextRenderingMode );


Lang::CharacterSpacingBinding::CharacterSpacingBinding( const Ast::SourceLocation & loc, const Concrete::Length spacing )
	: loc_( loc ), spacing_( spacing )
{ }

Lang::CharacterSpacingBinding::~CharacterSpacingBinding( )
{ }

void
Lang::CharacterSpacingBinding::bind( MapType & bindings, Kernel::SystemDynamicVariables ** sysBindings ) const
{
	if( *sysBindings == 0 )
		{
			*sysBindings = new Kernel::SystemDynamicVariables( );
			Kernel::TextState * newState = new Kernel::TextState( );
			newState->characterSpacing_ = spacing_;
			(*sysBindings)->textState_ = RefCountPtr< const Kernel::TextState >( newState );
			return;
		}
	
	if( (*sysBindings)->textState_ == NullPtr< const Kernel::TextState >( ) )
		{
			Kernel::TextState * newState = new Kernel::TextState( );
			newState->characterSpacing_ = spacing_;
			(*sysBindings)->textState_ = RefCountPtr< const Kernel::TextState >( newState );			
			return;
		}
	
	Kernel::TextState * newState = new Kernel::TextState( *((*sysBindings)->textState_) );

	if( ! IS_NAN( newState->characterSpacing_ ) )
		{
			throw Exceptions::MultipleDynamicBind( "< text state character spacing >", loc_, Ast::THE_UNKNOWN_LOCATION );
		}

	newState->characterSpacing_ = spacing_;
	(*sysBindings)->textState_ = RefCountPtr< const Kernel::TextState >( newState );			
}

void
Lang::CharacterSpacingBinding::gcMark( Kernel::GCMarkedSet & marked )
{ }
		

Lang::WordSpacingBinding::WordSpacingBinding( const Ast::SourceLocation & loc, const Concrete::Length spacing )
	: loc_( loc ), spacing_( spacing )
{ }

Lang::WordSpacingBinding::~WordSpacingBinding( )
{ }

void
Lang::WordSpacingBinding::bind( MapType & bindings, Kernel::SystemDynamicVariables ** sysBindings ) const
{
	if( *sysBindings == 0 )
		{
			*sysBindings = new Kernel::SystemDynamicVariables( );
			Kernel::TextState * newState = new Kernel::TextState( );
			newState->wordSpacing_ = spacing_;
			(*sysBindings)->textState_ = RefCountPtr< const Kernel::TextState >( newState );
			return;
		}
	
	if( (*sysBindings)->textState_ == NullPtr< const Kernel::TextState >( ) )
		{
			Kernel::TextState * newState = new Kernel::TextState( );
			newState->wordSpacing_ = spacing_;
			(*sysBindings)->textState_ = RefCountPtr< const Kernel::TextState >( newState );			
			return;
		}
	
	Kernel::TextState * newState = new Kernel::TextState( *((*sysBindings)->textState_) );

	if( ! IS_NAN( newState->wordSpacing_ ) )
		{
			throw Exceptions::MultipleDynamicBind( "< text state word spacing >", loc_, Ast::THE_UNKNOWN_LOCATION );
		}

	newState->wordSpacing_ = spacing_;
	(*sysBindings)->textState_ = RefCountPtr< const Kernel::TextState >( newState );			
}

void
Lang::WordSpacingBinding::gcMark( Kernel::GCMarkedSet & marked )
{ }


Lang::HorizontalScalingBinding::HorizontalScalingBinding( const Ast::SourceLocation & loc, double scaling )
	: loc_( loc ), scaling_( scaling )
{ }

Lang::HorizontalScalingBinding::~HorizontalScalingBinding( )
{ }

void
Lang::HorizontalScalingBinding::bind( MapType & bindings, Kernel::SystemDynamicVariables ** sysBindings ) const
{
	if( *sysBindings == 0 )
		{
			*sysBindings = new Kernel::SystemDynamicVariables( );
			Kernel::TextState * newState = new Kernel::TextState( );
			newState->horizontalScaling_ = scaling_;
			(*sysBindings)->textState_ = RefCountPtr< const Kernel::TextState >( newState );
			return;
		}
	
	if( (*sysBindings)->textState_ == NullPtr< const Kernel::TextState >( ) )
		{
			Kernel::TextState * newState = new Kernel::TextState( );
			newState->horizontalScaling_ = scaling_;
			(*sysBindings)->textState_ = RefCountPtr< const Kernel::TextState >( newState );			
			return;
		}
	
	Kernel::TextState * newState = new Kernel::TextState( *((*sysBindings)->textState_) );

	if( ! IS_NAN( newState->horizontalScaling_ ) )
		{
			throw Exceptions::MultipleDynamicBind( "< text state horizontal scaling >", loc_, Ast::THE_UNKNOWN_LOCATION );
		}

	newState->horizontalScaling_ = scaling_;
	(*sysBindings)->textState_ = RefCountPtr< const Kernel::TextState >( newState );			
}

void
Lang::HorizontalScalingBinding::gcMark( Kernel::GCMarkedSet & marked )
{ }


Lang::LeadingBinding::LeadingBinding( const Ast::SourceLocation & loc, const Concrete::Length ty )
	: loc_( loc ), ty_( ty ), isRelative_( false )
{ }

Lang::LeadingBinding::LeadingBinding( const Ast::SourceLocation & loc, const double r )
	: loc_( loc ), ty_( r ), isRelative_( true )
{ }

Lang::LeadingBinding::~LeadingBinding( )
{ }

void
Lang::LeadingBinding::bind( MapType & bindings, Kernel::SystemDynamicVariables ** sysBindings ) const
{
	if( *sysBindings == 0 )
		{
			*sysBindings = new Kernel::SystemDynamicVariables( );
			Kernel::TextState * newState = new Kernel::TextState( );
			if( isRelative_ )
				{
					newState->setLeading( Concrete::Length::offtype( ty_ ) );
				}
			else
				{
					newState->setLeading( ty_ );
				}
			(*sysBindings)->textState_ = RefCountPtr< const Kernel::TextState >( newState );
			return;
		}
	
	if( (*sysBindings)->textState_ == NullPtr< const Kernel::TextState >( ) )
		{
			Kernel::TextState * newState = new Kernel::TextState( );
			if( isRelative_ )
				{
					newState->setLeading( Concrete::Length::offtype( ty_ ) );
				}
			else
				{
					newState->setLeading( ty_ );
				}
			(*sysBindings)->textState_ = RefCountPtr< const Kernel::TextState >( newState );			
			return;
		}
	
	Kernel::TextState * newState = new Kernel::TextState( *((*sysBindings)->textState_) );

	if( newState->hasLeading( ) )
		{
			throw Exceptions::MultipleDynamicBind( "< text state leading >", loc_, Ast::THE_UNKNOWN_LOCATION );
		}
	
	if( isRelative_ )
		{
			newState->setLeading( Concrete::Length::offtype( ty_ ) );
		}
	else
		{
			newState->setLeading( ty_ );
		}
	(*sysBindings)->textState_ = RefCountPtr< const Kernel::TextState >( newState );			
}

void
Lang::LeadingBinding::gcMark( Kernel::GCMarkedSet & marked )
{ }


Lang::FontBinding::FontBinding( const Ast::SourceLocation & loc, const RefCountPtr< const Lang::Font > & font )
	: loc_( loc ), font_( font )
{ }

Lang::FontBinding::~FontBinding( )
{ }

void
Lang::FontBinding::bind( MapType & bindings, Kernel::SystemDynamicVariables ** sysBindings ) const
{
	if( *sysBindings == 0 )
		{
			*sysBindings = new Kernel::SystemDynamicVariables( );
			Kernel::TextState * newState = new Kernel::TextState( );
			newState->font_ = font_;
			(*sysBindings)->textState_ = RefCountPtr< const Kernel::TextState >( newState );
			return;
		}
	
	if( (*sysBindings)->textState_ == NullPtr< const Kernel::TextState >( ) )
		{
			Kernel::TextState * newState = new Kernel::TextState( );
			newState->font_ = font_;
			(*sysBindings)->textState_ = RefCountPtr< const Kernel::TextState >( newState );			
			return;
		}
	
	Kernel::TextState * newState = new Kernel::TextState( *((*sysBindings)->textState_) );

	if( newState->font_ != NullPtr< const Lang::Font >( ) )
		{
			throw Exceptions::MultipleDynamicBind( "< text state font >", loc_, Ast::THE_UNKNOWN_LOCATION );
		}

	newState->font_ = font_;
	(*sysBindings)->textState_ = RefCountPtr< const Kernel::TextState >( newState );			
}

void
Lang::FontBinding::gcMark( Kernel::GCMarkedSet & marked )
{
	const_cast< Lang::Font * >( font_.getPtr( ) )->gcMark( marked );
}


Lang::TextSizeBinding::TextSizeBinding( const Ast::SourceLocation & loc, const Concrete::Length size )
	: loc_( loc ), size_( size )
{ }

Lang::TextSizeBinding::~TextSizeBinding( )
{ }

void
Lang::TextSizeBinding::bind( MapType & bindings, Kernel::SystemDynamicVariables ** sysBindings ) const
{
	if( *sysBindings == 0 )
		{
			*sysBindings = new Kernel::SystemDynamicVariables( );
			Kernel::TextState * newState = new Kernel::TextState( );
			newState->size_ = size_;
			(*sysBindings)->textState_ = RefCountPtr< const Kernel::TextState >( newState );
			return;
		}
	
	if( (*sysBindings)->textState_ == NullPtr< const Kernel::TextState >( ) )
		{
			Kernel::TextState * newState = new Kernel::TextState( );
			newState->size_ = size_;
			(*sysBindings)->textState_ = RefCountPtr< const Kernel::TextState >( newState );			
			return;
		}
	
	Kernel::TextState * newState = new Kernel::TextState( *((*sysBindings)->textState_) );

	if( ! IS_NAN( newState->size_ ) )
		{
			throw Exceptions::MultipleDynamicBind( "< text state font size >", loc_, Ast::THE_UNKNOWN_LOCATION );
		}

	newState->size_ = size_;
	(*sysBindings)->textState_ = RefCountPtr< const Kernel::TextState >( newState );			
}

void
Lang::TextSizeBinding::gcMark( Kernel::GCMarkedSet & marked )
{ }


Lang::TextRenderingModeBinding::TextRenderingModeBinding( const Ast::SourceLocation & loc, const Lang::TextRenderingMode::ValueType mode )
	: loc_( loc ), mode_( mode )
{ }

Lang::TextRenderingModeBinding::~TextRenderingModeBinding( )
{ }

void
Lang::TextRenderingModeBinding::bind( MapType & bindings, Kernel::SystemDynamicVariables ** sysBindings ) const
{
	if( *sysBindings == 0 )
		{
			*sysBindings = new Kernel::SystemDynamicVariables( );
			Kernel::TextState * newState = new Kernel::TextState( );
			newState->mode_ = mode_;
			(*sysBindings)->textState_ = RefCountPtr< const Kernel::TextState >( newState );
			return;
		}
	
	if( (*sysBindings)->textState_ == NullPtr< const Kernel::TextState >( ) )
		{
			Kernel::TextState * newState = new Kernel::TextState( );
			newState->mode_ = mode_;
			(*sysBindings)->textState_ = RefCountPtr< const Kernel::TextState >( newState );			
			return;
		}
	
	Kernel::TextState * newState = new Kernel::TextState( *((*sysBindings)->textState_) );

	if( newState->mode_ != Lang::TextRenderingMode::UNDEFINED )
		{
			throw Exceptions::MultipleDynamicBind( "< text state rendering mode >", loc_, Ast::THE_UNKNOWN_LOCATION );
		}

	newState->mode_ = mode_;
	(*sysBindings)->textState_ = RefCountPtr< const Kernel::TextState >( newState );			
}

void
Lang::TextRenderingModeBinding::gcMark( Kernel::GCMarkedSet & marked )
{ }


Lang::TextRiseBinding::TextRiseBinding( const Ast::SourceLocation & loc, const Concrete::Length ty )
	: loc_( loc ), ty_( ty ), isRelative_( false )
{ }

Lang::TextRiseBinding::TextRiseBinding( const Ast::SourceLocation & loc, const double r )
	: loc_( loc ), ty_( r ), isRelative_( true )
{ }

Lang::TextRiseBinding::~TextRiseBinding( )
{ }

void
Lang::TextRiseBinding::bind( MapType & bindings, Kernel::SystemDynamicVariables ** sysBindings ) const
{
	if( *sysBindings == 0 )
		{
			*sysBindings = new Kernel::SystemDynamicVariables( );
			Kernel::TextState * newState = new Kernel::TextState( );
			if( isRelative_ )
				{
					newState->setRise( Concrete::Length::offtype( ty_ ) );
				}
			else
				{
					newState->setRise( ty_ );
				}
			(*sysBindings)->textState_ = RefCountPtr< const Kernel::TextState >( newState );
			return;
		}
	
	if( (*sysBindings)->textState_ == NullPtr< const Kernel::TextState >( ) )
		{
			Kernel::TextState * newState = new Kernel::TextState( );
			if( isRelative_ )
				{
					newState->setRise( Concrete::Length::offtype( ty_ ) );
				}
			else
				{
					newState->setRise( ty_ );
				}
			(*sysBindings)->textState_ = RefCountPtr< const Kernel::TextState >( newState );			
			return;
		}
	
	Kernel::TextState * newState = new Kernel::TextState( *((*sysBindings)->textState_) );

	if( newState->hasRise( ) )
		{
			throw Exceptions::MultipleDynamicBind( "< text state rise >", loc_, Ast::THE_UNKNOWN_LOCATION );
		}
	
	if( isRelative_ )
		{
			newState->setRise( Concrete::Length::offtype( ty_ ) );
		}
	else
		{
			newState->setRise( ty_ );
		}
	(*sysBindings)->textState_ = RefCountPtr< const Kernel::TextState >( newState );			
}

void
Lang::TextRiseBinding::gcMark( Kernel::GCMarkedSet & marked )
{ }


Lang::TextKnockoutBinding::TextKnockoutBinding( const Ast::SourceLocation & loc, bool knockout )
	: loc_( loc ), knockout_( knockout )
{ }

Lang::TextKnockoutBinding::~TextKnockoutBinding( )
{ }

void
Lang::TextKnockoutBinding::bind( MapType & bindings, Kernel::SystemDynamicVariables ** sysBindings ) const
{
	if( *sysBindings == 0 )
		{
			*sysBindings = new Kernel::SystemDynamicVariables( );
			Kernel::TextState * newState = new Kernel::TextState( );
			newState->knockout_ = knockout_;
			(*sysBindings)->textState_ = RefCountPtr< const Kernel::TextState >( newState );
			return;
		}
	
	if( (*sysBindings)->textState_ == NullPtr< const Kernel::TextState >( ) )
		{
			Kernel::TextState * newState = new Kernel::TextState( );
			newState->knockout_ = knockout_;
			(*sysBindings)->textState_ = RefCountPtr< const Kernel::TextState >( newState );			
			return;
		}
	
	Kernel::TextState * newState = new Kernel::TextState( *((*sysBindings)->textState_) );

	if( ( newState->knockout_ & Kernel::TextState::KNOCKOUT_UNDEFINED_BIT ) != 0 )
		{
			throw Exceptions::MultipleDynamicBind( "< text state knockout >", loc_, Ast::THE_UNKNOWN_LOCATION );
		}

	newState->knockout_ = ( knockout_ ? Kernel::TextState::KNOCKOUT_FLAG_BIT : 0 );
	(*sysBindings)->textState_ = RefCountPtr< const Kernel::TextState >( newState );			
}

void
Lang::TextKnockoutBinding::gcMark( Kernel::GCMarkedSet & marked )
{ }


Kernel::CharacterSpacingDynamicVariableProperties::CharacterSpacingDynamicVariableProperties( const char * name )
	: Kernel::DynamicVariableProperties( name )
{ }

Kernel::CharacterSpacingDynamicVariableProperties::~CharacterSpacingDynamicVariableProperties( )
{ }

Kernel::VariableHandle
Kernel::CharacterSpacingDynamicVariableProperties::fetch( const Kernel::PassedDyn & dyn ) const
{
	RefCountPtr< const Kernel::TextState > textState = dyn->getTextState( );
	return Kernel::VariableHandle( Shapes::Helpers::newValHandle( new Lang::Length( textState->characterSpacing_ ) ) );
}

void
Kernel::CharacterSpacingDynamicVariableProperties::makeBinding( Kernel::VariableHandle val, Ast::SourceLocation loc, Kernel::EvalState * evalState ) const
{
	RefCountPtr< const Lang::Length > spacing = val->getVal< const Lang::Length >( loc );
	Kernel::ContRef cont = evalState->cont_;
	cont->takeValue( Kernel::ValueRef( new Lang::CharacterSpacingBinding( loc, spacing->get( ) ) ),
									 evalState );
}


Kernel::WordSpacingDynamicVariableProperties::WordSpacingDynamicVariableProperties( const char * name )
	: Kernel::DynamicVariableProperties( name )
{ }

Kernel::WordSpacingDynamicVariableProperties::~WordSpacingDynamicVariableProperties( )
{ }

Kernel::VariableHandle
Kernel::WordSpacingDynamicVariableProperties::fetch( const Kernel::PassedDyn & dyn ) const
{
	RefCountPtr< const Kernel::TextState > textState = dyn->getTextState( );
	return Kernel::VariableHandle( Shapes::Helpers::newValHandle( new Lang::Length( textState->wordSpacing_ ) ) );
}

void
Kernel::WordSpacingDynamicVariableProperties::makeBinding( Kernel::VariableHandle val, Ast::SourceLocation loc, Kernel::EvalState * evalState ) const
{
	RefCountPtr< const Lang::Length > spacing = val->getVal< const Lang::Length >( loc );
	Kernel::ContRef cont = evalState->cont_;
	cont->takeValue( Kernel::ValueRef( new Lang::WordSpacingBinding( loc, spacing->get( ) ) ),
									 evalState );
}


Kernel::HorizontalScalingDynamicVariableProperties::HorizontalScalingDynamicVariableProperties( const char * name )
	: Kernel::DynamicVariableProperties( name )
{ }

Kernel::HorizontalScalingDynamicVariableProperties::~HorizontalScalingDynamicVariableProperties( )
{ }

Kernel::VariableHandle
Kernel::HorizontalScalingDynamicVariableProperties::fetch( const Kernel::PassedDyn & dyn ) const
{
	RefCountPtr< const Kernel::TextState > textState = dyn->getTextState( );
	return Kernel::VariableHandle( Shapes::Helpers::newValHandle( new Lang::Float( textState->horizontalScaling_ ) ) );
}

void
Kernel::HorizontalScalingDynamicVariableProperties::makeBinding( Kernel::VariableHandle val, Ast::SourceLocation loc, Kernel::EvalState * evalState ) const
{
	RefCountPtr< const Lang::Float > scaling = val->getVal< const Lang::Float >( loc );
	Kernel::ContRef cont = evalState->cont_;
	cont->takeValue( Kernel::ValueRef( new Lang::HorizontalScalingBinding( loc, scaling->val_ ) ),
									 evalState );
}


Kernel::LeadingDynamicVariableProperties::LeadingDynamicVariableProperties( const char * name )
	: Kernel::DynamicVariableProperties( name )
{ }

Kernel::LeadingDynamicVariableProperties::~LeadingDynamicVariableProperties( )
{ }

Kernel::VariableHandle
Kernel::LeadingDynamicVariableProperties::fetch( const Kernel::PassedDyn & dyn ) const
{
	RefCountPtr< const Kernel::TextState > textState = dyn->getTextState( );
	return Kernel::VariableHandle( new Kernel::Variable( textState->leading( ) ) );
}

void
Kernel::LeadingDynamicVariableProperties::makeBinding( Kernel::VariableHandle val, Ast::SourceLocation loc, Kernel::EvalState * evalState ) const
{
	RefCountPtr< const Lang::Value > valUntyped = val->getUntyped( );

	Kernel::ContRef cont = evalState->cont_;

	try
		{
			typedef const Lang::Length ArgType;
			RefCountPtr< ArgType > ty = Helpers::try_cast_CoreArgument< ArgType >( valUntyped );
			cont->takeValue( Kernel::ValueRef( new Lang::LeadingBinding( loc, ty->get( ) ) ),
											 evalState );
			return;
		}
	catch( const NonLocalExit::NotThisType & ball )
		{
			/* Wrong type; never mind!.. but see below!
			 */
		}

	try
		{
			typedef const Lang::Float ArgType;
			RefCountPtr< ArgType > ty = Helpers::try_cast_CoreArgument< ArgType >( valUntyped );
			cont->takeValue( Kernel::ValueRef( new Lang::LeadingBinding( loc, ty->val_ ) ),
											 evalState );
			return;
		}
	catch( const NonLocalExit::NotThisType & ball )
		{
			/* Wrong type; never mind!.. but see below!
			 */
		}

	throw Exceptions::TypeMismatch( loc, valUntyped->getTypeName( ), Helpers::typeSetString( Lang::Length::staticTypeName( ), Lang::Float::staticTypeName( ) ) );
}


Kernel::FontDynamicVariableProperties::FontDynamicVariableProperties( const char * name )
	: Kernel::DynamicVariableProperties( name )
{ }

Kernel::FontDynamicVariableProperties::~FontDynamicVariableProperties( )
{ }

Kernel::VariableHandle
Kernel::FontDynamicVariableProperties::fetch( const Kernel::PassedDyn & dyn ) const
{
	RefCountPtr< const Kernel::TextState > textState = dyn->getTextState( );
	return Kernel::VariableHandle( new Kernel::Variable( textState->font_ ) );
}

void
Kernel::FontDynamicVariableProperties::makeBinding( Kernel::VariableHandle val, Ast::SourceLocation loc, Kernel::EvalState * evalState ) const
{
	Kernel::ContRef cont = evalState->cont_;
	cont->takeValue( Kernel::ValueRef( new Lang::FontBinding( loc, val->getVal< const Lang::Font >( loc ) ) ),
									 evalState );
}


Kernel::TextSizeDynamicVariableProperties::TextSizeDynamicVariableProperties( const char * name )
	: Kernel::DynamicVariableProperties( name )
{ }

Kernel::TextSizeDynamicVariableProperties::~TextSizeDynamicVariableProperties( )
{ }

Kernel::VariableHandle
Kernel::TextSizeDynamicVariableProperties::fetch( const Kernel::PassedDyn & dyn ) const
{
	RefCountPtr< const Kernel::TextState > textState = dyn->getTextState( );
	return Kernel::VariableHandle( Shapes::Helpers::newValHandle( new Lang::Length( textState->size_ ) ) );
}

void
Kernel::TextSizeDynamicVariableProperties::makeBinding( Kernel::VariableHandle val, Ast::SourceLocation loc, Kernel::EvalState * evalState ) const
{
	RefCountPtr< const Lang::Length > size = val->getVal< const Lang::Length >( loc );
	Kernel::ContRef cont = evalState->cont_;
	cont->takeValue( Kernel::ValueRef( new Lang::TextSizeBinding( loc, size->get( ) ) ),
									 evalState );
}


Kernel::TextRenderingModeDynamicVariableProperties::TextRenderingModeDynamicVariableProperties( const char * name )
	: Kernel::DynamicVariableProperties( name )
{ }

Kernel::TextRenderingModeDynamicVariableProperties::~TextRenderingModeDynamicVariableProperties( )
{ }

Kernel::VariableHandle
Kernel::TextRenderingModeDynamicVariableProperties::fetch( const Kernel::PassedDyn & dyn ) const
{
	RefCountPtr< const Kernel::TextState > textState = dyn->getTextState( );
	return Kernel::VariableHandle( Shapes::Helpers::newValHandle( new Lang::TextRenderingMode( textState->mode_ ) ) );
}

void
Kernel::TextRenderingModeDynamicVariableProperties::makeBinding( Kernel::VariableHandle val, Ast::SourceLocation loc, Kernel::EvalState * evalState ) const
{
	RefCountPtr< const Lang::TextRenderingMode > mode = val->getVal< const Lang::TextRenderingMode >( loc );
	Kernel::ContRef cont = evalState->cont_;
	cont->takeValue( Kernel::ValueRef( new Lang::TextRenderingModeBinding( loc, mode->mode_ ) ),
									 evalState );
}


Kernel::TextRiseDynamicVariableProperties::TextRiseDynamicVariableProperties( const char * name )
	: Kernel::DynamicVariableProperties( name )
{ }

Kernel::TextRiseDynamicVariableProperties::~TextRiseDynamicVariableProperties( )
{ }

Kernel::VariableHandle
Kernel::TextRiseDynamicVariableProperties::fetch( const Kernel::PassedDyn & dyn ) const
{
	RefCountPtr< const Kernel::TextState > textState = dyn->getTextState( );
	return Kernel::VariableHandle( new Kernel::Variable( textState->rise( ) ) );
}

void
Kernel::TextRiseDynamicVariableProperties::makeBinding( Kernel::VariableHandle val, Ast::SourceLocation loc, Kernel::EvalState * evalState ) const
{
	RefCountPtr< const Lang::Value > valUntyped = val->getUntyped( );

	Kernel::ContRef cont = evalState->cont_;

	try
		{
			typedef const Lang::Length ArgType;
			RefCountPtr< ArgType > ty = Helpers::try_cast_CoreArgument< ArgType >( valUntyped );
			cont->takeValue( Kernel::ValueRef( new Lang::TextRiseBinding( loc, ty->get( ) ) ),
											 evalState );
			return;
		}
	catch( const NonLocalExit::NotThisType & ball )
		{
			/* Wrong type; never mind!.. but see below!
			 */
		}

	try
		{
			typedef const Lang::Float ArgType;
			RefCountPtr< ArgType > ty = Helpers::try_cast_CoreArgument< ArgType >( valUntyped );
			cont->takeValue( Kernel::ValueRef( new Lang::TextRiseBinding( loc, ty->val_ ) ),
											 evalState );
			return;
		}
	catch( const NonLocalExit::NotThisType & ball )
		{
			/* Wrong type; never mind!.. but see below!
			 */
		}

	throw Exceptions::TypeMismatch( loc, valUntyped->getTypeName( ), Helpers::typeSetString( Lang::Length::staticTypeName( ), Lang::Float::staticTypeName( ) ) );
}


Kernel::TextKnockoutDynamicVariableProperties::TextKnockoutDynamicVariableProperties( const char * name )
	: Kernel::DynamicVariableProperties( name )
{ }

Kernel::TextKnockoutDynamicVariableProperties::~TextKnockoutDynamicVariableProperties( )
{ }

Kernel::VariableHandle
Kernel::TextKnockoutDynamicVariableProperties::fetch( const Kernel::PassedDyn & dyn ) const
{
	RefCountPtr< const Kernel::TextState > textState = dyn->getTextState( );
	return Kernel::VariableHandle( Shapes::Helpers::newValHandle( new Lang::Boolean( ( textState->knockout_ & Kernel::TextState::KNOCKOUT_FLAG_BIT ) != 0 ) ) );
}

void
Kernel::TextKnockoutDynamicVariableProperties::makeBinding( Kernel::VariableHandle val, Ast::SourceLocation loc, Kernel::EvalState * evalState ) const
{
	RefCountPtr< const Lang::Boolean > mode = val->getVal< const Lang::Boolean >( loc );
	Kernel::ContRef cont = evalState->cont_;
	cont->takeValue( Kernel::ValueRef( new Lang::TextKnockoutBinding( loc, mode->val_ ) ),
									 evalState );
}



Kernel::TextState::TextState( )
	: characterSpacing_( Concrete::Length( std::numeric_limits< double >::signaling_NaN( ) ) ),
		wordSpacing_( Concrete::Length( std::numeric_limits< double >::signaling_NaN( ) ) ),
		horizontalScaling_( std::numeric_limits< double >::signaling_NaN( ) ),
		leading_( Concrete::Length( std::numeric_limits< double >::signaling_NaN( ) ) ),
		leadingIsRelative_( false ),
		rise_( Concrete::Length( std::numeric_limits< double >::signaling_NaN( ) ) ),
		riseIsRelative_( false ),
		font_( NullPtr< const Lang::Font >( ) ),
		size_( Concrete::Length( std::numeric_limits< double >::signaling_NaN( ) ) ),
		mode_( Lang::TextRenderingMode::UNDEFINED ),
		knockout_( KNOCKOUT_UNDEFINED_BIT )
{ }

Kernel::TextState::TextState( const Kernel::TextState & orig )
	: characterSpacing_( orig.characterSpacing_ ),
		wordSpacing_( orig.wordSpacing_ ),
		horizontalScaling_( orig.horizontalScaling_ ),
		leading_( orig.leading_ ),
		leadingIsRelative_( orig.leadingIsRelative_ ),
		rise_( orig.rise_ ),
		riseIsRelative_( orig.riseIsRelative_ ),
		font_( orig.font_ ),
		size_( orig.size_ ),
		mode_( orig.mode_ ),
		knockout_( orig.knockout_ )
{ }

Kernel::TextState::TextState( const Kernel::TextState & newValues, const Kernel::TextState & oldValues )
	: characterSpacing_( oldValues.characterSpacing_ ),
		wordSpacing_( oldValues.wordSpacing_ ),
		horizontalScaling_( oldValues.horizontalScaling_ ),
		leading_( oldValues.leading_ ),
		leadingIsRelative_( oldValues.leadingIsRelative_ ),
		rise_( oldValues.rise_ ),
		riseIsRelative_( oldValues.riseIsRelative_ ),
		font_( oldValues.font_ ),
		size_( oldValues.size_ ),
		mode_( oldValues.mode_ ),
		knockout_( oldValues.knockout_ )
{
	if( ! IS_NAN( newValues.characterSpacing_ ) )
		{
			characterSpacing_ = newValues.characterSpacing_;
		}
	if( ! IS_NAN( newValues.wordSpacing_ ) )
		{
			wordSpacing_ = newValues.wordSpacing_;
		}
	if( ! IS_NAN( newValues.horizontalScaling_ ) )
		{
			horizontalScaling_ = newValues.horizontalScaling_;
		}
	if( ! IS_NAN( newValues.leading_ ) )
		{
			leading_ = newValues.leading_;
			leadingIsRelative_ = newValues.leadingIsRelative_;
		}
	if( ! IS_NAN( newValues.rise_ ) )
		{
			rise_ = newValues.rise_;
			riseIsRelative_ = newValues.riseIsRelative_;
		}
	if( newValues.font_ != NullPtr< const Lang::Font >( ) )
		{
			font_ = newValues.font_;
		}
	if( ! IS_NAN( newValues.size_ ) )
		{
			size_ = newValues.size_;
		}
	if( newValues.mode_ != Lang::TextRenderingMode::UNDEFINED )
		{
			mode_ = newValues.mode_;
		}
	if( ! ( newValues.knockout_ & KNOCKOUT_UNDEFINED_BIT ) )
		{
			knockout_ = newValues.knockout_;
		}
}

std::map< bool, RefCountPtr< SimplePDF::PDF_Object > > Kernel::TextState::knockoutNameMap_;

Kernel::TextState::TextState( bool setDefaults )
	: characterSpacing_( 0 ),
		wordSpacing_( 0 ),
		horizontalScaling_( 1 ),
		leading_( 1 ),
		leadingIsRelative_( true ),
		rise_( 0 ),
		riseIsRelative_( false ),
		font_( Lang::THE_FONT_HELVETICA ),
		size_( Concrete::Length( 10 ) ),
		mode_( Lang::TextRenderingMode::FILL ),
		knockout_( 0 ) // this means false
{
	if( ! setDefaults )
		{
			throw Exceptions::InternalError( strrefdup( "setDefaults must be true in TextState::TextState." ) );
		}
}

Kernel::TextState::~TextState( )
{ }


void
Kernel::TextState::setLeading( const Concrete::Length leading )
{
	leadingIsRelative_ = false;
	leading_ = leading;
}

void
Kernel::TextState::setLeading( const double relativeLeading )
{
	leadingIsRelative_ = true;
	leading_ = Concrete::Length( relativeLeading );	// We must keep track of this type trick by always looking at leadingIsRelative_.
}

RefCountPtr< const Lang::Value >
Kernel::TextState::leading( ) const
{
	if( leadingIsRelative_ )
		{
			return RefCountPtr< const Lang::Value >( new Lang::Float( Concrete::Length::offtype( leading_ ) ) );
		}
	return RefCountPtr< const Lang::Value >( new Lang::Length( leading_ ) );
}

Concrete::Length
Kernel::TextState::leadingConcrete( ) const
{
	if( leadingIsRelative_ )
		{
			return Concrete::Length::offtype( leading_ ) * size_;
		}
	return leading_;
}

bool
Kernel::TextState::hasLeading( ) const
{
	return ! IS_NAN( leading_ );
}


void
Kernel::TextState::setRise( const Concrete::Length rise )
{
	riseIsRelative_ = false;
	rise_ = rise;
}

void
Kernel::TextState::setRise( const double relativeRise )
{
	riseIsRelative_ = true;
	rise_ = Concrete::Length( relativeRise );	// We must keep track of this type trick by always looking at riseIsRelative_.
}

RefCountPtr< const Lang::Value >
Kernel::TextState::rise( ) const
{
	if( riseIsRelative_ )
		{
			return RefCountPtr< const Lang::Value >( new Lang::Float( Concrete::Length::offtype( rise_ ) ) );
		}
	return RefCountPtr< const Lang::Value >( new Lang::Length( rise_ ) );
}

Concrete::Length
Kernel::TextState::riseConcrete( ) const
{
	if( riseIsRelative_ )
		{
			return Concrete::Length::offtype( rise_ ) * size_;
		}
	return rise_;
}

bool
Kernel::TextState::hasRise( ) const
{
	return ! IS_NAN( rise_ );
}


bool
Kernel::TextState::synchAssertKnockout( std::ostream & os, const Kernel::TextState * ref, SimplePDF::PDF_Resources * resources, bool force )
{
	assertKnockout( ref );
	return synchButKnockout( os, ref, resources, force );
}

bool
Kernel::TextState::synchCharacterSpacing( std::ostream & os, const Kernel::TextState * ref, SimplePDF::PDF_Resources * resources, bool force )
{
	if( force || characterSpacing_ != ref->characterSpacing_ )
		{
			if( IS_NAN( ref->characterSpacing_ ) )
				{
					return false;
				}
			characterSpacing_ = ref->characterSpacing_;
			os << Concrete::Length::offtype( characterSpacing_ ) << " Tc " ;
			return true;
		}
	return false;	
}

bool
Kernel::TextState::synchWordSpacing( std::ostream & os, const Kernel::TextState * ref, SimplePDF::PDF_Resources * resources, bool force )
{
	if( force || wordSpacing_ != ref->wordSpacing_ )
		{
			if( IS_NAN( ref->wordSpacing_ ) )
				{
					return false;
				}
			wordSpacing_ = ref->wordSpacing_;
			os << Concrete::Length::offtype( wordSpacing_ ) << " Tw " ;
			return true;
		}
	return false;	
}

bool
Kernel::TextState::synchHorizontalScaling( std::ostream & os, const Kernel::TextState * ref, SimplePDF::PDF_Resources * resources, bool force )
{
	if( force || horizontalScaling_ != ref->horizontalScaling_ )
		{
			if( IS_NAN( ref->horizontalScaling_ ) )
				{
					return false;
				}
			horizontalScaling_ = ref->horizontalScaling_;
			os << 100 * horizontalScaling_ << " Tz " ;
			return true;
		}
	return false;	
}

bool
Kernel::TextState::synchLeading( std::ostream & os, const Kernel::TextState * ref, SimplePDF::PDF_Resources * resources, bool force )
{
	if( force ||
			leading_ != ref->leading_ ||
			leadingIsRelative_ != ref->leadingIsRelative_ )
		{
			if( ! ref->hasLeading( ) )
				{
					return false;
				}
			leadingIsRelative_ = ref->leadingIsRelative_;
			leading_ = ref->leading_;
			if( leadingIsRelative_ )
				{
					os << Concrete::Length::offtype( leading_ ) * Concrete::Length::offtype( ref->size_ ) << " TL " ;
				}
			else
				{
					os << Concrete::Length::offtype( leading_ ) << " TL " ;
				}
			return true;
		}
	return false;	
}

bool
Kernel::TextState::synchFontAndSize( std::ostream & os, const Kernel::TextState * ref, SimplePDF::PDF_Resources * resources, bool force )
{
	if( force ||
			font_ != ref->font_ ||
			size_ != ref->size_ )
		{
			if( ref->font_ == NullPtr< const Lang::Font >( ) &&
					IS_NAN( ref->size_ ) )
				{
					return false;
				}
			if( ref->font_ == NullPtr< const Lang::Font >( ) ||
					IS_NAN( ref->size_ ) )
				{
					throw Exceptions::MiscellaneousRequirement( "It is impossible to leave unspecified only one of font and size." );
				}
			font_ = ref->font_;
			size_ = ref->size_;
			os << resources->nameofFont( font_->resource( ) ) << " " << Concrete::Length::offtype( size_ ) << " Tf " ;
			return true;
		}
	return false;	
}

bool
Kernel::TextState::synchMode( std::ostream & os, const Kernel::TextState * ref, SimplePDF::PDF_Resources * resources, bool force )
{
	if( force || mode_ != ref->mode_ )
		{
			if( ref->mode_ == Lang::TextRenderingMode::UNDEFINED )
				{
					return false;
				}
			mode_ = ref->mode_;
			os << mode_ << " Tr " ;
			return true;
		}
	return false;	
}

bool
Kernel::TextState::synchRise( std::ostream & os, const Kernel::TextState * ref, SimplePDF::PDF_Resources * resources, bool force )
{
	if( force ||
			rise_ != ref->rise_ ||
			riseIsRelative_ != ref->riseIsRelative_ )
		{
			if( ! ref->hasRise( ) )
				{
					return false;
				}
			riseIsRelative_ = ref->riseIsRelative_;
			rise_ = ref->rise_;
			if( riseIsRelative_ )
				{
					os << Concrete::Length::offtype( rise_ ) * Concrete::Length::offtype( ref->size_ ) << " Ts " ;
				}
			else
				{
					os << Concrete::Length::offtype( rise_ ) << " Ts " ;
				}
			return true;
		}
	return false;	
}

bool
Kernel::TextState::synchKnockout( std::ostream & os, const Kernel::TextState * ref, SimplePDF::PDF_Resources * resources, bool force )
{
	if( force || knockout_ != ref->knockout_ )
		{
			if( ( ref->knockout_ & Kernel::TextState::KNOCKOUT_UNDEFINED_BIT ) != 0 )
				{
					return false;
				}
			const SimplePDF::PDF_out::Version KNOCKOUT_VERSION = SimplePDF::PDF_out::PDF_1_4;
			if( ! Kernel::the_pdfo->versionGreaterOrEqual( KNOCKOUT_VERSION ) )
				{
					Kernel::the_pdfo->versionMessage( KNOCKOUT_VERSION, "The text state knockout mode setting was ignored." );
				}
			knockout_ = ref->knockout_;
			typedef typeof knockoutNameMap_ MapType;
			MapType::const_iterator i = knockoutNameMap_.find( knockout_ );
			if( i != knockoutNameMap_.end( ) )
				{
					os << resources->nameofGraphicsState( i->second ) << " gs " ;
				}
			else
				{
					RefCountPtr< SimplePDF::PDF_Dictionary > dic;
					(*dic)[ "Type" ] = SimplePDF::PDF_out::newName( "ExtGState" );
					(*dic)[ "TK" ] = SimplePDF::PDF_out::newBoolean( knockout_ );

					RefCountPtr< SimplePDF::PDF_Object > indirection = Kernel::the_pdfo->indirect( dic );
					knockoutNameMap_.insert( MapType::value_type( knockout_, indirection ) );

					os << resources->nameofGraphicsState( indirection ) << " gs " ;
				}
			return true;
		}
	return false;	
}

void
Kernel::TextState::assertKnockout( const Kernel::TextState * ref )
{
	if( knockout_ != ref->knockout_ )
		{
			throw Exceptions::MiscellaneousRequirement( "PDF does not allow the text knockout mode to change within a text object." );
		}
}

bool
Kernel::TextState::synchButKnockout( std::ostream & os, const Kernel::TextState * ref, SimplePDF::PDF_Resources * resources, bool force )
{
	bool anyChange = false;
	anyChange = synchCharacterSpacing( os, ref, resources, force ) || anyChange;
	anyChange = synchWordSpacing( os, ref, resources, force ) || anyChange;
	anyChange = synchHorizontalScaling( os, ref, resources, force ) || anyChange;
	anyChange = synchFontAndSize( os, ref, resources, force ) || anyChange;
	anyChange = synchLeading( os, ref, resources, force ) || anyChange;					// It is important that this is done after synching the size!
	anyChange = synchMode( os, ref, resources, force ) || anyChange;
	anyChange = synchRise( os, ref, resources, force ) || anyChange;
	if( anyChange )
		{
			os << std::endl ;
		}
	return anyChange;
}
