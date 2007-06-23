#include "MetaPDF_Helpers_decls.h"

#include "multipage.h"
#include "pdfstructure.h"
#include "classtypes.h"
#include "pagecontentstates.h"
#include "continuations.h"
#include "globals.h"
#include "metapdfexceptions.h"


using namespace MetaPDF;


Kernel::WarmCatalog::Page::Page( const RefCountPtr< SimplePDF::PDF_Resources > & resources, const RefCountPtr< SimplePDF::PDF_Stream_out > & contents, const RefCountPtr< SimplePDF::PDF_Vector > & mediabox )
  : resources_( resources ), contents_( contents ), mediabox_( mediabox )
{ }

Kernel::WarmCatalog::Page::~Page( )
{ }


Kernel::WarmCatalog::PageLabelEntry::PageLabelEntry( size_t pageIndex, const RefCountPtr< const char > & prefix, Style style, size_t startNumber )
  : pageIndex_( pageIndex ), prefix_( prefix ), style_( style ), startNumber_( startNumber )
{ }

Kernel::WarmCatalog::PageLabelEntry::~PageLabelEntry( )
{ }


Kernel::WarmCatalog::WarmCatalog( )
{
  labelEntries_.push_back( new Kernel::WarmCatalog::PageLabelEntry( 0, NullPtr< const char >( ), PageLabelEntry::DECIMAL, 1 ) );  
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

  tackOnPage( pageContents, callLoc );

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
  const SimplePDF::PDF_out::Version VERSION = SimplePDF::PDF_out::PDF_1_3;
  if( ! Kernel::the_pdfo->versionGreaterOrEqual( VERSION ) )
    {
      Kernel::the_pdfo->versionMessage( VERSION, "The page label setting was ignored." );
      return;
    }

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
  const Kernel::WarmCatalog::PageLabelEntry * lastEntry = labelEntries_.back( );
  size_t current = lastEntry->startNumber_ + pages_.size( ) - lastEntry->pageIndex_;
  std::ostringstream oss;
  oss << lastEntry->prefix_.getPtr( ) ;
  switch( lastEntry->style_ )
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

bool
Kernel::WarmCatalog::isEmpty( ) const
{
  return pages_.size( ) == 0;
}

void
Kernel::WarmCatalog::tackOnPage( const RefCountPtr< const Lang::Drawable2D > & pageContents, const Ast::SourceLocation & callLoc )
{
  RefCountPtr< SimplePDF::PDF_Resources > resources( new SimplePDF::PDF_Resources );
  RefCountPtr< SimplePDF::PDF_Stream_out > contents( new SimplePDF::PDF_Stream_out );

  resources->requireProcedureSet( SimplePDF::PDF_Resources::PROC_SET_PDF );
  
  // Forcing to synch is a bad thing, due to PDF version differences.  Instead, refer to the PDF documentation
  // on the graphics state dictionary (page 180 in the PDF-1.6 reference) to find out the correct default values,
  // and make sure that these are the initial values of the pdfState.
  Kernel::PageContentStates pdfState( resources, true );
  
  pageContents->shipout( contents->data, & pdfState, Lang::Transform2D( 1, 0, 0, 1, 0, 0 ) );

  RefCountPtr< const Lang::ElementaryPath2D > theBBox = pageContents->bbox( );
  if( theBBox->size( ) == 0 )
    {
      throw Exceptions::InsertingEmptyPage( callLoc );
    }
  Concrete::Coords2D llcorner( 0, 0 );
  Concrete::Coords2D urcorner( 0, 0 );
  theBBox->boundingRectangle( & llcorner, & urcorner );
  RefCountPtr< SimplePDF::PDF_Vector > mediabox( new SimplePDF::PDF_Vector( llcorner.x_.offtype< 1, 0 >( ),
									    llcorner.y_.offtype< 1, 0 >( ),
									    urcorner.x_.offtype< 1, 0 >( ),
									    urcorner.y_.offtype< 1, 0 >( ) ) );
  
  pages_.push_back( new Page( resources, contents, mediabox ) );  
}

void
Kernel::WarmCatalog::shipout( SimplePDF::PDF_out * doc )
{
  RefCountPtr< SimplePDF::PDF_Dictionary > pages( new SimplePDF::PDF_Dictionary );

  RefCountPtr< SimplePDF::PDF_Object > i_pages( doc->indirect( pages ) );
  doc->root_->dic[ "Pages" ] = i_pages;
  
  pages->dic[ "Type"  ] = doc->newName( "Pages" );
  RefCountPtr< SimplePDF::PDF_Vector > pagesKids( new SimplePDF::PDF_Vector );
  {
    typedef typeof pages_ ListType;
    for( ListType::const_iterator i = pages_.begin( ); i != pages_.end( ); ++i )
      {
	RefCountPtr< SimplePDF::PDF_Dictionary > newPage( new SimplePDF::PDF_Dictionary );
	pagesKids->vec.push_back( doc->indirect( newPage ) );
	newPage->dic[ "Type" ] = doc->newName( "Page" );
	newPage->dic[ "Parent" ] = i_pages;
	newPage->dic[ "MediaBox" ] = (*i)->mediabox_;
	newPage->dic[ "Contents" ] = doc->indirect( (*i)->contents_ );
	newPage->dic[ "Resources" ] = doc->indirect( (*i)->resources_ );
	/* The UserUnit entry appears in PDF 1.6, and cannot be relyed on */
	//  newPage->dic[ "UserUnit" ] = RefCountPtr< PDF_Object >( new PDF_Float( 72 / 2.52 ) );
      }
  }
  pages->dic[ "Kids" ] = pagesKids;
  pages->dic[ "Count" ] = doc->newInt( pagesKids->vec.size( ) );

  const SimplePDF::PDF_out::Version PAGELABELS_VERSION = SimplePDF::PDF_out::PDF_1_3;
  if( Kernel::the_pdfo->versionGreaterOrEqual( PAGELABELS_VERSION ) )
    {
      RefCountPtr< SimplePDF::PDF_Dictionary > pageLabels( new SimplePDF::PDF_Dictionary );
      doc->root_->dic[ "PageLabels" ] = doc->indirect( pageLabels );
      pageLabels->dic[ "Type"  ] = doc->newName( "PageLabels" );
      RefCountPtr< SimplePDF::PDF_Vector > nums( new SimplePDF::PDF_Vector );
      pageLabels->dic[ "Nums"  ] = nums;
      typedef typeof labelEntries_ ListType;
      for( ListType::const_iterator i = labelEntries_.begin( ); i != labelEntries_.end( ); ++i )
	{
	  RefCountPtr< SimplePDF::PDF_Dictionary > newEntry( new SimplePDF::PDF_Dictionary );
	  nums->vec.push_back( doc->newInt( (*i)->pageIndex_ ) );
	  nums->vec.push_back( newEntry );
	  
	  switch( (*i)->style_ )
	    {
	    case Kernel::WarmCatalog::PageLabelEntry::NONE:
	      { }
	      break;
	    case Kernel::WarmCatalog::PageLabelEntry::DECIMAL:
	      {
		newEntry->dic[ "S" ] = doc->newName( "D" );
	      }
	      break;
	    case Kernel::WarmCatalog::PageLabelEntry::ROMAN:
	      {
		newEntry->dic[ "S" ] = doc->newName( "R" );
	      }
	      break;
	    case Kernel::WarmCatalog::PageLabelEntry::rOMAN:
	      {
		newEntry->dic[ "S" ] = doc->newName( "r" );
	      }
	      break;
	    case Kernel::WarmCatalog::PageLabelEntry::ALPHABET:
	      {
		newEntry->dic[ "S" ] = doc->newName( "A" );
	      }
	      break;
	    case Kernel::WarmCatalog::PageLabelEntry::aLPHABET:
	      {
		newEntry->dic[ "S" ] = doc->newName( "a" );
	      }
	      break;
	    default:
	      throw Exceptions::InternalError( "Page label style out of range during shipout." );
	    }
	  
	  if( strlen( (*i)->prefix_.getPtr( ) ) > 0 )
	    {
	      newEntry->dic[ "P" ] = doc->newString( (*i)->prefix_.getPtr( ) );
	    }
	  
	  if( (*i)->startNumber_ != 1 )
	    {
	      newEntry->dic[ "St" ] = doc->newInt( (*i)->startNumber_ );
	    }
	}
    }
}
