#include "Shapes_Helpers_decls.h"

#include "multipage.h"
#include "annotations.h"
#include "pdfstructure.h"
#include "classtypes.h"
#include "pagecontentstates.h"
#include "continuations.h"
#include "globals.h"
#include "shapesexceptions.h"

#include <algorithm>


using namespace Shapes;

Lang::DocumentDestination::DocumentDestination( bool remote, RefCountPtr< const char > name, int outlineLevel,
																								RefCountPtr< const char > outlineText, bool outlineOpen, bool outlineFontBold, bool outlineFontItalic, const Concrete::RGB & outlineColor )
	: remote_( remote ), name_( name ), outlineLevel_( outlineLevel ),
		outlineText_( outlineText ), outlineOpen_( outlineOpen ), outlineFontBold_ ( outlineFontBold ), outlineFontItalic_( outlineFontItalic ), outlineColor_( outlineColor ),
		target_( NullPtr< const Lang::Drawable2D >( ) )
{
	if( ! remote_ )
		{
			throw Exceptions::InternalError( "The constructor of DocumentDestination requires <remote> to be true." );
		}
	if( name_ == NullPtr< const char >( ) )
		{
			throw Exceptions::InternalError( "The constructor remote DocumentDestination requires <name> to be non-null." );
		}
}

Lang::DocumentDestination::DocumentDestination( RefCountPtr< const char > name, int outlineLevel,
																								RefCountPtr< const char > outlineText, bool outlineOpen, bool outlineFontBold, bool outlineFontItalic, const Concrete::RGB & outlineColor,
																								Sides sidesMode, RefCountPtr< const Lang::Drawable2D > target, bool fittobbox, double zoom )
	: remote_( false ), name_( name ), outlineLevel_( outlineLevel ),
		outlineText_( outlineText ), outlineOpen_( outlineOpen ), outlineFontBold_ ( outlineFontBold ), outlineFontItalic_( outlineFontItalic ), outlineColor_( outlineColor ),
		sidesMode_( sidesMode ), target_( target ), fittobbox_( fittobbox ), zoom_( zoom )
{ }

Lang::DocumentDestination::~DocumentDestination( )
{ }


RefCountPtr< const Lang::Class > Lang::DocumentDestination::TypeID( new Lang::SystemFinalClass( strrefdup( "Destination" ) ) );
TYPEINFOIMPL( DocumentDestination );

RefCountPtr< const Lang::Geometric2D >
Lang::DocumentDestination::transformed( const Lang::Transform2D & transform, const RefCountPtr< const Lang::Geometric2D > & self ) const
{
	if( remote_ )
		{
			return self;
		}

	if( target_ == NullPtr< const Lang::Drawable2D >( ) )
		{
			return self;
		}

	return
		RefCountPtr< const Lang::Geometric2D >
		( new Lang::DocumentDestination( name_, outlineLevel_,
																		 outlineText_, outlineOpen_, outlineFontBold_, outlineFontItalic_, outlineColor_,
																		 sidesMode_, target_->typed_transformed( transform, target_ ), fittobbox_, zoom_ ) );
}

RefCountPtr< const Lang::Geometric3D >
Lang::DocumentDestination::to3D( const RefCountPtr< const Lang::Geometric2D > & self ) const
{
	throw Exceptions::NotImplemented( "Destinations in 3D." );
}


void
Lang::DocumentDestination::gcMark( Kernel::GCMarkedSet & marked )
{
	if( target_ != NullPtr< const Lang::Drawable2D >( ) )
		{
			const_cast< Lang::Drawable2D * >( target_.getPtr( ) )->gcMark( marked );
		}
}

bool
Lang::DocumentDestination::definesNamed( ) const
{
	return ! remote_ && name_ != NullPtr< const char >( );
}

RefCountPtr< const char >
Lang::DocumentDestination::name( ) const
{
	return name_;
}

bool
Lang::DocumentDestination::isOutlineEntry( ) const
{
	return outlineLevel_ >= 0;
}

size_t
Lang::DocumentDestination::outlineLevel( ) const
{
	return static_cast< size_t >( outlineLevel_ );
}

RefCountPtr< SimplePDF::PDF_Vector >
Lang::DocumentDestination::getDirectDestination( const RefCountPtr< SimplePDF::PDF_Indirect_out > & i_page ) const
{
	RefCountPtr< SimplePDF::PDF_Vector > res( new SimplePDF::PDF_Vector );
	res->vec.push_back( i_page );

	Concrete::Coords2D llcorner( 0, 0 );
	Concrete::Coords2D urcorner( 0, 0 );
	if( target_ != NullPtr< const Lang::Drawable2D >( ) )
		{
			RefCountPtr< const Lang::ElementaryPath2D > theBBox = target_->bbox( );
			if( theBBox->empty( ) )
				{
					throw Exceptions::MiscellaneousRequirement( "The destination target produced an empty bounding box." );
				}
			theBBox->boundingRectangle( & llcorner, & urcorner );
		}
	switch( sidesMode_ )
		{
		case PAGE:
			{
				if( fittobbox_ )
					{
						res->vec.push_back( SimplePDF::PDF_out::newName( "FitB" ) );
					}
				else
					{
						res->vec.push_back( SimplePDF::PDF_out::newName( "Fit" ) );
					}
			}
			break;
		case TOPLEFT:
			{
				res->vec.push_back( SimplePDF::PDF_out::newName( "XYZ" ) );
				res->vec.push_back( SimplePDF::PDF_out::newFloat( llcorner.x_.offtype< 1, 0 >( ) ) );
				res->vec.push_back( SimplePDF::PDF_out::newFloat( urcorner.y_.offtype< 1, 0 >( ) ) );
				res->vec.push_back( SimplePDF::PDF_out::newFloat( zoom_ ) );
			}
			break;
		case TOP:
			{
				if( fittobbox_ )
					{
						res->vec.push_back( SimplePDF::PDF_out::newName( "FitH" ) );
					}
				else
					{
						res->vec.push_back( SimplePDF::PDF_out::newName( "FitBH" ) );
					}
				res->vec.push_back( SimplePDF::PDF_out::newFloat( urcorner.y_.offtype< 1, 0 >( ) ) );
			}
			break;
		case LEFT:
			{
				if( fittobbox_ )
					{
						res->vec.push_back( SimplePDF::PDF_out::newName( "FitV" ) );
					}
				else
					{
						res->vec.push_back( SimplePDF::PDF_out::newName( "FitBV" ) );
					}
				res->vec.push_back( SimplePDF::PDF_out::newFloat( llcorner.x_.offtype< 1, 0 >( ) ) );
			}
			break;
		case RECTANGLE:
			{
				res->vec.push_back( SimplePDF::PDF_out::newName( "FitR" ) );
				res->vec.push_back( SimplePDF::PDF_out::newFloat( llcorner.x_.offtype< 1, 0 >( ) ) );
				res->vec.push_back( SimplePDF::PDF_out::newFloat( llcorner.y_.offtype< 1, 0 >( ) ) );
				res->vec.push_back( SimplePDF::PDF_out::newFloat( urcorner.x_.offtype< 1, 0 >( ) ) );
				res->vec.push_back( SimplePDF::PDF_out::newFloat( urcorner.y_.offtype< 1, 0 >( ) ) );
			}
			break;
		default:
			throw Exceptions::InternalError( "Destination's sidesMode_ out of range in getDestination." );
		}

	return res;
}

RefCountPtr< SimplePDF::PDF_Object >
Lang::DocumentDestination::getDestination( const RefCountPtr< SimplePDF::PDF_Indirect_out > & i_page ) const
{
	if( remote_ )
		{
			return SimplePDF::PDF_out::newString( name_.getPtr( ) );
		}
	return getDirectDestination( i_page );
}

RefCountPtr< SimplePDF::OutlineItem >
Lang::DocumentDestination::getOutlineItem( const RefCountPtr< SimplePDF::PDF_Indirect_out > & i_page, RefCountPtr< const char > otherText ) const
{
	RefCountPtr< const char > theText = otherText;
	if( theText == NullPtr< const char >( ) )
		{
			theText = outlineText_;
		}

	if( name_ != NullPtr< const char >( ) )
		{
			return RefCountPtr< SimplePDF::OutlineItem >
				( new SimplePDF::OutlineItem( SimplePDF::PDF_out::newString( name_.getPtr( ) ), theText,
																			outlineOpen_, outlineFontBold_, outlineFontItalic_, outlineColor_ ) );
		}
	return RefCountPtr< SimplePDF::OutlineItem >
		( new SimplePDF::OutlineItem( getDestination( i_page ), theText,
																	outlineOpen_, outlineFontBold_, outlineFontItalic_, outlineColor_ ) );
}

Kernel::WarmCatalog::BoundingRectangle::BoundingRectangle( )
	: xmin_( Concrete::HUGE_LENGTH ), ymin_( Concrete::HUGE_LENGTH ),
		xmax_( -Concrete::HUGE_LENGTH ), ymax_( -Concrete::HUGE_LENGTH ),
		modified_( true ), pdfVec_( RefCountPtr< SimplePDF::PDF_Vector >( NullPtr< SimplePDF::PDF_Vector >( ) ) )
{ }

void
Kernel::WarmCatalog::BoundingRectangle::growToContain( const Concrete::Coords2D & ll, const Concrete::Coords2D & ur )
{
	xmin_ = std::min( xmin_, ll.x_ );
	ymin_ = std::min( ymin_, ll.y_ );
	xmax_ = std::max( xmax_, ur.x_ );
	ymax_ = std::max( ymax_, ur.y_ );
	modified_ = true;
}

RefCountPtr< SimplePDF::PDF_Vector >
Kernel::WarmCatalog::BoundingRectangle::pdfVector( ) const
{
	if( modified_ )
		{
			modified_ = false;
		}
	pdfVec_ = RefCountPtr< SimplePDF::PDF_Vector >( new SimplePDF::PDF_Vector( xmin_.offtype< 1, 0 >( ),
																																						 ymin_.offtype< 1, 0 >( ),
																																						 xmax_.offtype< 1, 0 >( ),
																																						 ymax_.offtype< 1, 0 >( ) ) );
	return pdfVec_;
}


Kernel::WarmCatalog::Page::Page( size_t index, const RefCountPtr< SimplePDF::PDF_Resources > & resources, const RefCountPtr< SimplePDF::PDF_Stream_out > & contents, const RefCountPtr< Kernel::WarmCatalog::BoundingRectangle > & mediabox )
	: index_( index ), resources_( resources ), contents_( contents ), mediabox_( mediabox )
{ }

Kernel::WarmCatalog::Page::~Page( )
{ }


Kernel::WarmCatalog::PageLabelEntry::PageLabelEntry( size_t pageIndex, const RefCountPtr< const char > & prefix, Style style, size_t startNumber )
	: pageIndex_( pageIndex ), prefix_( prefix ), style_( style ), startNumber_( startNumber )
{ }

Kernel::WarmCatalog::PageLabelEntry::~PageLabelEntry( )
{ }


Kernel::WarmCatalog::WarmCatalog( )
	: pageLabelsActivated_( false ),
		bboxGroup_( RefCountPtr< const Lang::Symbol >( NullPtr< const Lang::Symbol >( ) ) )
{
	labelEntries_.push_back( new Kernel::WarmCatalog::PageLabelEntry( 0, strrefdup( "" ), PageLabelEntry::DECIMAL, 1 ) );
}

Kernel::WarmCatalog::~WarmCatalog( )
{ }

RefCountPtr< const Lang::Class > Kernel::WarmCatalog::TypeID( new Lang::SystemFinalClass( strrefdup( "#Catalog" ) ) );
TYPEINFOIMPL_STATE( WarmCatalog );

void
Kernel::WarmCatalog::tackOnImpl( Kernel::EvalState * evalState, const RefCountPtr< const Lang::Value > & piece, const Kernel::PassedDyn & dyn, const Ast::SourceLocation & callLoc )
{
	typedef const Lang::Drawable2D ArgType;
	RefCountPtr< const ArgType > pageContents( Helpers::down_cast< ArgType >( piece, callLoc ) );

	tackOnPage( evalState->dyn_, pageContents, callLoc );

	Kernel::ContRef cont = evalState->cont_;
	cont->takeHandle( Kernel::THE_SLOT_VARIABLE,
										evalState );
}

void
Kernel::WarmCatalog::freezeImpl( Kernel::EvalState * evalState, const Ast::SourceLocation & callLoc )
{
	throw Exceptions::MiscellaneousRequirement( strrefdup( "The catalog state cannot be frozen." ) );
}

void
Kernel::WarmCatalog::peekImpl( Kernel::EvalState * evalState, const Ast::SourceLocation & callLoc )
{
	throw Exceptions::MiscellaneousRequirement( strrefdup( "The catalog state cannot be peeked." ) );
}

void
Kernel::WarmCatalog::gcMark( Kernel::GCMarkedSet & marked )
{ }

void
Kernel::WarmCatalog::setLabel( RefCountPtr< const char > prefix, PageLabelEntry::Style style, size_t start )
{
	const SimplePDF::PDF_Version::Version PAGELABEL_VERSION = SimplePDF::PDF_Version::PDF_1_3;
	if( ! Kernel::the_PDF_version.greaterOrEqual( PAGELABEL_VERSION ) )
		{
			Kernel::the_PDF_version.message( PAGELABEL_VERSION, "The page label setting was ignored." );
			return;
		}

	pageLabelsActivated_ = true;

	if( labelEntries_.back( )->pageIndex_ == pages_.size( ) )
		{
			delete labelEntries_.back( );
			labelEntries_.pop_back( );
		}
	labelEntries_.push_back( new Kernel::WarmCatalog::PageLabelEntry( pages_.size( ), prefix, style, start ) );
}

size_t
Kernel::WarmCatalog::getNextPageNumber( ) const
{
	const Kernel::WarmCatalog::PageLabelEntry * lastEntry = labelEntries_.back( );
	return lastEntry->startNumber_ + pages_.size( ) - lastEntry->pageIndex_;
}

Kernel::WarmCatalog::PageLabelEntry::Style
Kernel::WarmCatalog::getNextPageStyle( ) const
{
	const Kernel::WarmCatalog::PageLabelEntry * lastEntry = labelEntries_.back( );
	return lastEntry->style_;
}

RefCountPtr< const char >
Kernel::WarmCatalog::getNextPagePrefix( ) const
{
	const Kernel::WarmCatalog::PageLabelEntry * lastEntry = labelEntries_.back( );
	return lastEntry->prefix_;
}

RefCountPtr< const char >
Kernel::WarmCatalog::getNextPageLabel( ) const
{
	return getPageLabel( labelEntries_.back( ), pages_.size( ) );
}

RefCountPtr< const char >
Kernel::WarmCatalog::getPageLabel( size_t index ) const
{
	typedef typeof labelEntries_ ListType;
	ListType::const_iterator i = labelEntries_.end( );
	--i;
	// Note that labelEntries_.begin( )->pageIndex is 0.
	while( (*i)->pageIndex_ > index )
		{
			--i;
		}
	return getPageLabel( *i, index );
}

RefCountPtr< const char >
Kernel::WarmCatalog::getPageLabel( const Kernel::WarmCatalog::PageLabelEntry * entry, size_t index ) const
{
	size_t current = entry->startNumber_ + index - entry->pageIndex_;
	std::ostringstream oss;
	oss << entry->prefix_.getPtr( ) ;
	switch( entry->style_ )
		{
		case PageLabelEntry::NONE:
			{ }
			break;
		case PageLabelEntry::DECIMAL:
			{
				oss << current ;
			}
			break;
		case PageLabelEntry::ROMAN:
			{
				if( current >= 5000 )
					{
						throw Exceptions::NotImplemented( "Conversion to roman numerals of numbers greater or equal 5000." );
					}
				if( current == 0 )
					{
						throw Exceptions::OutOfRange( "Conversion to roman numeral of the page number 0." );
					}
				while( current >= 1000 )
					{
						oss << "M" ;
						current -= 1000;
					}
				if( current >= 900 )
					{
						oss << "CM" ;
						current -= 900;
					}
				if( current >= 500 )
					{
						oss << "D" ;
						current -= 500;
					}
				if( current >= 400 )
					{
						oss << "CD" ;
						current -= 400;
					}
				while( current >= 100 )
					{
						oss << "C" ;
						current -= 100;
					}
				if( current >= 90 )
					{
						oss << "XC" ;
						current -= 90;
					}
				if( current >= 50 )
					{
						oss << "L" ;
						current -= 50;
					}
				if( current >= 40 )
					{
						oss << "XL" ;
						current -= 40;
					}
				while( current >= 10 )
					{
						oss << "X" ;
						current -= 10;
					}
				if( current >= 9 )
					{
						oss << "IX" ;
						current -= 9;
					}
				if( current >= 5 )
					{
						oss << "V" ;
						current -= 5;
					}
				if( current >= 4 )
					{
						oss << "IV" ;
						current -= 4;
					}
				while( current >= 1 )
					{
						oss << "I" ;
						current -= 1;
					}
			}
			break;
		case PageLabelEntry::rOMAN:
			{
				if( current >= 5000 )
					{
						throw Exceptions::NotImplemented( "Conversion to roman numerals of numbers greater or equal 5000." );
					}
				if( current == 0 )
					{
						throw Exceptions::OutOfRange( "Conversion to roman numeral of the page number 0." );
					}
				while( current >= 1000 )
					{
						oss << "m" ;
						current -= 1000;
					}
				if( current >= 900 )
					{
						oss << "cm" ;
						current -= 900;
					}
				if( current >= 500 )
					{
						oss << "d" ;
						current -= 500;
					}
				if( current >= 400 )
					{
						oss << "cd" ;
						current -= 400;
					}
				while( current >= 100 )
					{
						oss << "c" ;
						current -= 100;
					}
				if( current >= 90 )
					{
						oss << "xc" ;
						current -= 90;
					}
				if( current >= 50 )
					{
						oss << "l" ;
						current -= 50;
					}
				if( current >= 40 )
					{
						oss << "xl" ;
						current -= 40;
					}
				while( current >= 10 )
					{
						oss << "x" ;
						current -= 10;
					}
				if( current >= 9 )
					{
						oss << "ix" ;
						current -= 9;
					}
				if( current >= 5 )
					{
						oss << "v" ;
						current -= 5;
					}
				if( current >= 4 )
					{
						oss << "iv" ;
						current -= 4;
					}
				while( current >= 1 )
					{
						oss << "i" ;
						current -= 1;
					}
			}
			break;
		case PageLabelEntry::ALPHABET:
			{
				size_t base = static_cast< size_t >( 'Z' ) - static_cast< size_t >( 'A' ) + 1;
				size_t baseFactor = ( current - 1 ) / base ;
				char rest = 'A' + ( current - baseFactor * base ) - 1;
				for( size_t i = 0; i < baseFactor + 1; ++i )
					{
						oss << rest ;
					}
			}
			break;
		case PageLabelEntry::aLPHABET:
			{
				size_t base = static_cast< size_t >( 'Z' ) - static_cast< size_t >( 'A' ) + 1;
				size_t baseFactor = ( current - 1 ) / base ;
				char rest = 'a' + ( current - baseFactor * base ) - 1;
				for( size_t i = 0; i < baseFactor + 1; ++i )
					{
						oss << rest ;
					}
			}
			break;
		default:
			throw Exceptions::InternalError( "Page label style out of range." );
		}
	return strrefdup( oss );
}

void
Kernel::WarmCatalog::setBBoxGroup( const RefCountPtr< const Lang::Symbol > & group )
{
	bboxGroup_ = group;
}

bool
Kernel::WarmCatalog::isEmpty( ) const
{
	return pages_.empty( );
}

void
Kernel::WarmCatalog::tackOnPage( const Kernel::PassedDyn & dyn, const RefCountPtr< const Lang::Drawable2D > & pageContents, const Ast::SourceLocation & callLoc )
{
	RefCountPtr< SimplePDF::PDF_Resources > resources( new SimplePDF::PDF_Resources );
	RefCountPtr< SimplePDF::PDF_Stream_out > contents( new SimplePDF::PDF_Stream_out );

	resources->requireProcedureSet( SimplePDF::PDF_Resources::PROC_SET_PDF );

	// Forcing to synch is a bad thing, due to PDF version differences.	Instead, refer to the PDF documentation
	// on the graphics state dictionary (page 180 in the PDF-1.6 reference) to find out the correct default values,
	// and make sure that these are the initial values of the pdfState.
	Kernel::PageContentStates pdfState( resources, true );

	pageContents->shipout( contents->data, & pdfState, Lang::Transform2D( 1, 0, 0, 1, 0, 0 ) );

	RefCountPtr< const Lang::ElementaryPath2D > theBBox = pageContents->bbox( );
	if( theBBox->empty( ) )
		{
			throw Exceptions::InsertingEmptyPage( callLoc );
		}
	Concrete::Coords2D llcorner( 0, 0 );
	Concrete::Coords2D urcorner( 0, 0 );
	theBBox->boundingRectangle( & llcorner, & urcorner );
	RefCountPtr< BoundingRectangle > mediabox = RefCountPtr< BoundingRectangle >( NullPtr< BoundingRectangle >( ) );
	if( bboxGroup_ == NullPtr< const Lang::Symbol >( ) )
		{
			mediabox = RefCountPtr< BoundingRectangle >( );
		}
	else
		{
			mediabox = mediaBoxes_[ bboxGroup_->getKey( ) ];
		}
	mediabox->growToContain( llcorner, urcorner );
	Page * newPage( new Page( pages_.size( ), resources, contents, mediabox ) );
	pages_.push_back( newPage );

	{
		std::vector< Kernel::ValueRef > destinations;
		pageContents->findTags( & destinations, dyn, Kernel::THE_NAVIGATION_SYMBOL->getKey( ), Lang::THE_2D_IDENTITY );
		newPage->destinations_.reserve( destinations.size( ) );
		typedef typeof destinations ListType;
		for( ListType::const_iterator i = destinations.begin( ); i != destinations.end( ); ++i )
			{
				typedef const Lang::DocumentDestination ValType;
				RefCountPtr< ValType > dest = i->down_cast< ValType >( );
				if( dest == NullPtr< ValType >( ) )
					{
						throw Exceptions::TypeMismatch( callLoc, "The values tagged for navigations must have a certain type.", (*i)->getTypeName( ), ValType::staticTypeName( ) );
					}
				newPage->destinations_.push_back( dest );
			}
	}

	{
		std::vector< Kernel::ValueRef > annotations;
		pageContents->findTags( & annotations, dyn, Kernel::THE_ANNOTATION_SYMBOL->getKey( ), Lang::THE_2D_IDENTITY );
		newPage->annotations_.reserve( annotations.size( ) );
		typedef typeof annotations ListType;
		for( ListType::const_iterator i = annotations.begin( ); i != annotations.end( ); ++i )
			{
				typedef const Lang::AnnotationBase ValType;
				RefCountPtr< ValType > dest = i->down_cast< ValType >( );
				if( dest == NullPtr< ValType >( ) )
					{
						throw Exceptions::TypeMismatch( callLoc, "The values tagged for annotation must have a certain type.", (*i)->getTypeName( ), ValType::staticTypeName( ) );
					}
				newPage->annotations_.push_back( dest );
			}
	}
}

void
Kernel::WarmCatalog::shipout( SimplePDF::PDF_out * doc )
{
	std::map< RefCountPtr< const char >, RefCountPtr< SimplePDF::PDF_Vector >, charRefPtrLess > namedDestinations;
	std::vector< RefCountPtr< SimplePDF::OutlineItem > > outlineStack;
	outlineStack.reserve( 10 );
	outlineStack.push_back
		( RefCountPtr< SimplePDF::OutlineItem >
			( new SimplePDF::OutlineItem( RefCountPtr< SimplePDF::PDF_Object >( NullPtr< SimplePDF::PDF_Object >( ) ), strrefdup( "Top" ),
																		true, false, false, Concrete::RGB( 0, 0, 0 ) ) ) );

	RefCountPtr< SimplePDF::PDF_Dictionary > pages( new SimplePDF::PDF_Dictionary );
	RefCountPtr< SimplePDF::PDF_Dictionary > names( new SimplePDF::PDF_Dictionary );

	RefCountPtr< SimplePDF::PDF_Object > i_pages( doc->indirect( pages ) );
	doc->root_->dic[ "Pages" ] = i_pages;

	std::list< std::pair< const Page *, std::pair< RefCountPtr< SimplePDF::PDF_Dictionary >, RefCountPtr< SimplePDF::PDF_Indirect_out > > > > annotations;

	pages->dic[ "Type"	] = SimplePDF::PDF_out::newName( "Pages" );
	RefCountPtr< SimplePDF::PDF_Vector > pagesKids( new SimplePDF::PDF_Vector );
	{
		typedef typeof pages_ ListType;
		for( ListType::const_iterator i = pages_.begin( ); i != pages_.end( ); ++i )
			{
				RefCountPtr< SimplePDF::PDF_Dictionary > newPage( new SimplePDF::PDF_Dictionary );
				RefCountPtr< SimplePDF::PDF_Indirect_out > i_newPage = doc->indirect( newPage );
				pagesKids->vec.push_back( i_newPage );
				newPage->dic[ "Type" ] = SimplePDF::PDF_out::newName( "Page" );
				newPage->dic[ "Parent" ] = i_pages;
				newPage->dic[ "MediaBox" ] = (*i)->mediabox_->pdfVector( );
				newPage->dic[ "Contents" ] = doc->indirect( (*i)->contents_ );
				newPage->dic[ "Resources" ] = doc->indirect( (*i)->resources_ );
				/* The UserUnit entry appears in PDF 1.6, and cannot be relyed on */
				//	newPage->dic[ "UserUnit" ] = RefCountPtr< PDF_Object >( new PDF_Float( 72 / 2.52 ) );

				typedef typeof (*i)->destinations_ DestListType;
				for( DestListType::const_iterator j = (*i)->destinations_.begin( ); j != (*i)->destinations_.end( ); ++j )
					{
						RefCountPtr< const Lang::DocumentDestination > dest = *j;
						if( dest->definesNamed( ) )
							{
								typedef typeof namedDestinations MapType;
								RefCountPtr< const char > name = dest->name( );
								if( namedDestinations.find( name ) != namedDestinations.end( ) )
									{
										std::ostringstream oss;
										oss << "The named destination \"" << name.getPtr( ) << "\" appeared a second time (and possibly also the first time) on the page labeled "
												<< getPageLabel( (*i)->index_ ).getPtr( ) << ", with zero-based physical index " << (*i)->index_ << "." ;
										throw Exceptions::MiscellaneousRequirement( strrefdup( oss ) );
									}
								namedDestinations.insert( MapType::value_type( name, dest->getDirectDestination( i_newPage ) ) );
							}

						if( dest->isOutlineEntry( ) )
							{
								/* This is the index in the stack at which the item belongs.
									 In other words, this shall be the size of outlineStack just before the new item is pushed.
								 */
								const size_t stackLevel = dest->outlineLevel( ) + 1;

								if( outlineStack.size( ) < stackLevel )
									{
										RefCountPtr< const char > missingText = strrefdup( " " );
										while( outlineStack.size( ) < stackLevel )
											{
												RefCountPtr< SimplePDF::OutlineItem > missingItem = dest->getOutlineItem( i_newPage, missingText );
												outlineStack.back( )->addKid( missingItem );
												outlineStack.push_back( missingItem );
											}
									}
								while( outlineStack.size( ) > stackLevel )
									{
										outlineStack.pop_back( );
									}
								RefCountPtr< SimplePDF::OutlineItem > item = dest->getOutlineItem( i_newPage );
								outlineStack.back( )->addKid( item );
								outlineStack.push_back( item );
							}
					}

				if( ! (*i)->annotations_.empty( ) )
					{
						typedef typeof annotations ListType;
						annotations.push_back( ListType::value_type( *i, ListType::value_type::second_type( newPage, i_newPage ) ) );
					}

			}
	}
	pages->dic[ "Kids" ] = pagesKids;
	pages->dic[ "Count" ] = SimplePDF::PDF_out::newInt( pagesKids->vec.size( ) );

	{
		typedef std::list< std::pair< const Page *, std::pair< RefCountPtr< SimplePDF::PDF_Dictionary >, RefCountPtr< SimplePDF::PDF_Indirect_out > > > > ListType;
		for( ListType::const_iterator h = annotations.begin( ); h != annotations.end( ); ++h )
			{
				const Page * i = h->first;
				RefCountPtr< SimplePDF::PDF_Dictionary > newPage = h->second.first;
				RefCountPtr< SimplePDF::PDF_Indirect_out > i_newPage = h->second.second;
				RefCountPtr< SimplePDF::PDF_Vector > annots( new SimplePDF::PDF_Vector );
				newPage->dic[ "Annots" ] = doc->indirect( annots );
				annots->vec.reserve( i->annotations_.size( ) );

				typedef typeof i->annotations_ AnnotListType;
				for( AnnotListType::const_iterator j = i->annotations_.begin( ); j != i->annotations_.end( ); ++j )
					{
						RefCountPtr< const Lang::AnnotationBase > annot = *j;
						annots->vec.push_back( doc->indirect( annot->getDictionary( i_newPage, namedDestinations ) ) );
					}
			}

	}

	const SimplePDF::PDF_Version::Version PAGELABELS_VERSION = SimplePDF::PDF_Version::PDF_1_3;
	if( pageLabelsActivated_ &&
			Kernel::the_PDF_version.greaterOrEqual( PAGELABELS_VERSION ) )
		{
			RefCountPtr< SimplePDF::PDF_Dictionary > pageLabels( new SimplePDF::PDF_Dictionary );
			doc->root_->dic[ "PageLabels" ] = doc->indirect( pageLabels );
			pageLabels->dic[ "Type"	] = SimplePDF::PDF_out::newName( "PageLabels" );
			RefCountPtr< SimplePDF::PDF_Vector > nums( new SimplePDF::PDF_Vector );
			pageLabels->dic[ "Nums"	] = nums;
			typedef typeof labelEntries_ ListType;
			for( ListType::const_iterator i = labelEntries_.begin( ); i != labelEntries_.end( ); ++i )
				{
					RefCountPtr< SimplePDF::PDF_Dictionary > newEntry( new SimplePDF::PDF_Dictionary );
					nums->vec.push_back( SimplePDF::PDF_out::newInt( (*i)->pageIndex_ ) );
					nums->vec.push_back( newEntry );

					switch( (*i)->style_ )
						{
						case Kernel::WarmCatalog::PageLabelEntry::NONE:
							{ }
							break;
						case Kernel::WarmCatalog::PageLabelEntry::DECIMAL:
							{
								newEntry->dic[ "S" ] = SimplePDF::PDF_out::newName( "D" );
							}
							break;
						case Kernel::WarmCatalog::PageLabelEntry::ROMAN:
							{
								newEntry->dic[ "S" ] = SimplePDF::PDF_out::newName( "R" );
							}
							break;
						case Kernel::WarmCatalog::PageLabelEntry::rOMAN:
							{
								newEntry->dic[ "S" ] = SimplePDF::PDF_out::newName( "r" );
							}
							break;
						case Kernel::WarmCatalog::PageLabelEntry::ALPHABET:
							{
								newEntry->dic[ "S" ] = SimplePDF::PDF_out::newName( "A" );
							}
							break;
						case Kernel::WarmCatalog::PageLabelEntry::aLPHABET:
							{
								newEntry->dic[ "S" ] = SimplePDF::PDF_out::newName( "a" );
							}
							break;
						default:
							throw Exceptions::InternalError( "Page label style out of range during shipout." );
						}

					if( strlen( (*i)->prefix_.getPtr( ) ) > 0 )
						{
							newEntry->dic[ "P" ] = SimplePDF::PDF_out::newString( (*i)->prefix_.getPtr( ) );
						}

					if( (*i)->startNumber_ != 1 )
						{
							newEntry->dic[ "St" ] = SimplePDF::PDF_out::newInt( (*i)->startNumber_ );
						}
				}
		}

	if( outlineStack.front( )->hasKids( ) )
		{
			doc->root_->dic[ "Outlines" ] = outlineStack.front( )->getTopIndirectDictionary( doc, Kernel::the_PDF_version );
		}
	if( ! namedDestinations.empty( ) )
		{
			// If there are named destinations, the PDF version is already checked to be high enough.
			RefCountPtr< SimplePDF::PDF_Dictionary > dests( new SimplePDF::PDF_Dictionary );
			names->dic[ "Dests" ] = doc->indirect( dests );
//			 RefCountPtr< SimplePDF::PDF_Vector > kids( new SimplePDF::PDF_Vector );
//			 dests->dic[ "Kids" ] = kids;
//			 RefCountPtr< SimplePDF::PDF_Dictionary > kid( new SimplePDF::PDF_Dictionary );
//			 kids->vec.push_back( kid );
//			 {
//							 RefCountPtr< SimplePDF::PDF_Vector > limits( new SimplePDF::PDF_Vector );
//							 limits->vec.push_back( SimplePDF::PDF_out::newString( namedDestinations.begin( )->first.getPtr( ) ) );
//							 limits->vec.push_back( SimplePDF::PDF_out::newString( namedDestinations.rbegin( )->first.getPtr( ) ) );
//							 kid->dic[ "Limits"	] = limits;
//			 }
			{
				RefCountPtr< SimplePDF::PDF_Vector > names( new SimplePDF::PDF_Vector );
				typedef typeof namedDestinations Maptype;
				for( Maptype::const_iterator i = namedDestinations.begin( ); i != namedDestinations.end( ); ++i )
					{
						RefCountPtr< SimplePDF::PDF_Dictionary > newEntry( new SimplePDF::PDF_Dictionary );
						names->vec.push_back( SimplePDF::PDF_out::newString( i->first.getPtr( ) ) );
						names->vec.push_back( i->second );
					}
				dests->dic[ "Names"	] = names;
			}
		}

	if( ! names->dic.empty( ) )
		{
			doc->root_->dic[ "Names" ] = doc->indirect( names );
		}
}
