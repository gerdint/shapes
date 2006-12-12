#include <iomanip>

#include "simplepdfo.h"
#include "simplepdfi.h"
#include "metapdfexceptions.h"
#include "texlabelmanager.h"
#include "metapdftypes.h"
#include "globals.h"

using namespace std;

using namespace SimplePDF;

PDF_Resources::PDF_Resources( )
  : counter( 0 ),
    xobject( new PDF_Dictionary ),
    graphicsStates( new PDF_Dictionary ),
    colorSpaces( new PDF_Dictionary ),
    fonts( new PDF_Dictionary ),
    shadings( new PDF_Dictionary ),
    procSetsVector( new PDF_Vector( ) )
{ }

PDF_Resources::~PDF_Resources( )
{ }

void
PDF_Resources::writeTo( std::ostream & os ) const
{
  PDF_Dictionary dic;
  if( xobject->dic.size( ) > 0 )
    {
      dic[ "XObject" ] = MetaPDF::Kernel::the_pdfo->indirect( xobject );
    }
  if( graphicsStates->dic.size( ) > 0 )
    {
      dic[ "ExtGState" ] = MetaPDF::Kernel::the_pdfo->indirect( graphicsStates );
    }
  if( colorSpaces->dic.size( ) > 0 )
    {
      dic[ "ColorSpaces" ] = MetaPDF::Kernel::the_pdfo->indirect( colorSpaces );
    }
  if( fonts->dic.size( ) > 0 )
    {
      dic[ "Font" ] = MetaPDF::Kernel::the_pdfo->indirect( fonts );
    }
  if( procSetsVector->vec.size( ) > 0 )
    {
      dic[ "ProcSet" ] = MetaPDF::Kernel::the_pdfo->indirect( procSetsVector );
    }
  if( shadings->dic.size( ) > 0 )
    {
      dic[ "Shading" ] = MetaPDF::Kernel::the_pdfo->indirect( shadings );
    }
  dic.writeTo( os );
}

const SimplePDF::PDF_Name &
PDF_Resources::nameof( const RefCountPtr< PDF_Object > & obj, ReverseMap * reverseMap, RefCountPtr< PDF_Dictionary > * dic, const char * prefix, size_t * counter )
{
  ReverseMap::iterator i = reverseMap->find( obj.getPtr( ) );
  if( i == reverseMap->end( ) )
    {
      ostringstream reference;
      reference << prefix << *counter ;
      ++*counter;
      
      (*dic)->dic[ reference.str( ) ] = obj;
      reverseMap->insert( ReverseMap::value_type( obj.getPtr( ), PDF_Name( reference.str( ) ) ) );
      i = reverseMap->find( obj.getPtr( ) );
    }
  return i->second;
}

const PDF_Name &
SimplePDF::PDF_Resources::nameofXObject( const RefCountPtr< PDF_Object > & obj )
{
  return nameof( obj, & reverse_xobject, & xobject, "x", & counter );
}

const PDF_Name &
SimplePDF::PDF_Resources::nameofGraphicsState( const RefCountPtr< PDF_Object > & obj )
{
  return nameof( obj, & reverse_graphicsStates, & graphicsStates, "g", & counter );
}

const PDF_Name &
SimplePDF::PDF_Resources::nameofColorSpace( const RefCountPtr< PDF_Object > & obj )
{
  return nameof( obj, & reverse_colorSpaces, & colorSpaces, "c", & counter );
}

const PDF_Name &
SimplePDF::PDF_Resources::nameofFont( const RefCountPtr< PDF_Object > & obj )
{
  return nameof( obj, & reverse_fonts, & fonts, "f", & counter );
}

const PDF_Name &
SimplePDF::PDF_Resources::nameofShading( const RefCountPtr< PDF_Object > & obj )
{
  return nameof( obj, & reverse_shadings, & shadings, "s", & counter );
}


void
SimplePDF::PDF_Resources::requireProcedureSet( ProcSet procSet )
{
  if( procSets.find( procSet ) == procSets.end( ) )
    {
      switch( procSet )
	{
	case PROC_SET_PDF:
	  procSetsVector->vec.push_back( SimplePDF::PDF_out::newName( "PDF" ) );
	  break;
	case PROC_SET_TEXT:
	  procSetsVector->vec.push_back( SimplePDF::PDF_out::newName( "Text" ) );
	  break;
	case PROC_SET_IMAGE_GRAY:
	  procSetsVector->vec.push_back( SimplePDF::PDF_out::newName( "ImageB" ) );
	  break;
	case PROC_SET_IMAGE_COLOR:
	  procSetsVector->vec.push_back( SimplePDF::PDF_out::newName( "ImageC" ) );
	  break;
	case PROC_SET_IMAGE_INDEXED:
	  procSetsVector->vec.push_back( SimplePDF::PDF_out::newName( "ImageI" ) );
	  break;
	default:
	  std::cerr << "Internal error: An unexpected procedure set was requested." << std::endl ;
	  exit( 1 );
	}
      procSets.insert( procSets.begin( ), procSet );
    }
}
    


SimplePDF::PDF_out::PDF_out( ostream * _os )
  : version_( SimplePDF::PDF_out::PDF_1_4 ),
    versionAction_( SimplePDF::PDF_out::WARN ),
    os( _os ), os_start( static_cast< streamoff >( os->tellp( ) ) ),
    i_root( NullPtr<PDF_Object>( ) ),
    pageResources( new PDF_Resources ),
    contents( new PDF_Stream_out ),
    mediabox( new PDF_Vector( 0, 0, 150, 120 ) ),
    info_( new PDF_Dictionary ),
    objCount( 0 )
{
  indirect( RefCountPtr< PDF_Int >( new PDF_Int( 0 ) ), 65535 );
  indirectQueue.back( )->inUse = false;

  (*info_)[ "Producer" ] = newString( "Drool" );
}

SimplePDF::PDF_out::~PDF_out( )
{ }

void
SimplePDF::PDF_out::writeData( )
{
  RefCountPtr< PDF_Object > i_info;
  try
    {
  (*os) << std::fixed ;
  (*os) << "%" << toString( version_ ) << endl
     << "%"
     << static_cast< char >( 129 ) << static_cast< char >( 130 )
     << static_cast< char >( 131 ) << static_cast< char >( 132 )
     << " Treat as binary" << endl ;

  RefCountPtr<PDF_Dictionary> pages( new PDF_Dictionary );
  RefCountPtr<PDF_Dictionary> thePage( new PDF_Dictionary );
  RefCountPtr<PDF_Dictionary> root( new PDF_Dictionary );
  i_root = indirect( root );
  i_info = indirect( info_ );

  if( extensionAuthorStrings.size( ) > 0 )
    {
      std::ostringstream oss;
      typedef typeof extensionAuthorStrings ListType;
      ListType::const_iterator i = extensionAuthorStrings.begin( );
      oss << *i ;
      ++i;
      for( ; i != extensionAuthorStrings.end( ); ++i )
	{
	  oss << "; " << *i ;
	}
      (*info_)[ "ExtensionAuthors" ] = newString( oss.str( ).c_str( ) );
    }

  root->dic[ "Type" ] = newName( "Catalog" );

  /*
  {
    RefCountPtr<PDF_Dictionary> outlines( new PDF_Dictionary );
    outlines->dic[ "Type"  ] = newName( "Outlines" );
    outlines->dic[ "Count" ] = newInt( 0 );
    root->dic[ "Outlines" ] = indirect( outlines );
  }
  */

  RefCountPtr<PDF_Object> i_pages( indirect( pages ) );
  root->dic[ "Pages" ] = i_pages;
  
  pages->dic[ "Type"  ] = newName( "Pages" );
  {
    RefCountPtr<PDF_Vector> pagesKids( new PDF_Vector );
    pagesKids->vec.push_back( indirect( thePage ) );
    pages->dic[ "Kids" ] = pagesKids;
    pages->dic[ "Count" ] = newInt( pagesKids->vec.size( ) );
  }

  thePage->dic[ "Type" ] = newName( "Page" );
  thePage->dic[ "Parent" ] = i_pages;
  thePage->dic[ "MediaBox" ] = mediabox;
  thePage->dic[ "Contents" ] = indirect( contents );
  thePage->dic[ "Resources" ] = indirect( pageResources );
  /* The UserUnit entry appears in PDF 1.6, and cannot be relyed on */
  //  thePage->dic[ "UserUnit" ] = RefCountPtr< PDF_Object >( new PDF_Float( 72 / 2.52 ) );

  {
    typedef IndirectQueueType::iterator I;
    for( I i( indirectQueue.begin( ) ); i != indirectQueue.end( ); ++i )
      {
	(*i)->byteOffset = static_cast< streamoff >( os->tellp( ) ) - os_start;
	(*i)->writeObject( *os );
      }
  }
  streamoff xref( static_cast< streamoff >( os->tellp( ) ) );
  {
    (*os) << setfill( '0' ) ;
    (*os) << "xref" << endl
       << 0 << " " << indirectQueue.size( ) << endl ;
    typedef IndirectQueueType::iterator I;
    for( I i( indirectQueue.begin( ) ); i != indirectQueue.end( ); ++i )
      {
	(*os) << setw( 10 ) << (*i)->byteOffset << ' ' << setw( 5 ) << (*i)->v << ' ' << 'n' << ' ' << static_cast< char >( 10 ) ;
      }
  }
  (*os) << "trailer" << endl ;
  {
    PDF_Dictionary trailer;
    trailer.dic[ "Size" ] = newInt( indirectQueue.size( ) );
    trailer.dic[ "Root" ] = i_root;
    trailer.dic[ "Info" ] = i_info;
    trailer.writeTo( *os );
  }
  (*os) << endl << "startxref" << endl ;
  (*os) << xref - os_start << endl ;
  ostringstream tmps;
  tmps << endl ;
  (*os) << "%%EOF" << endl ;
    }
  catch( const char * ball )
    {
      cerr << "Caught (char*) ball at top level:" << endl 
	   << "  " << ball << endl ;
      exit( 1 );
    }
  catch( const string & ball )
    {
      cerr << "Caught (string) ball at top level:" << endl 
	   << "  " << ball << endl ;
      exit( 1 );
    }
  catch( ... )
    {
      cerr << "Caught (...) ball at top level." << endl ;
      exit( 1 );
    }
  
  while( importSources.size( ) > 0 )
    {
      importSources.pop_back( );
    }
}

void
SimplePDF::PDF_out::setOutputStream( ostream * _os )
{
  if( static_cast< streamoff >( os->tellp( ) ) != os_start )
    {
      cerr << "Switching output file when contents has already been written in another file." << endl ;
      exit( 1 );
    }
  os = _os;
  os_start = static_cast< streamoff >( os->tellp( ) );
}


RefCountPtr< PDF_Indirect_out >
SimplePDF::PDF_out::indirect( RefCountPtr<PDF_Object> obj, size_t v )
{
  indirectQueue.push_back( RefCountPtr< PDF_Indirect_out >( new PDF_Indirect_out( obj, objCount, v ) ) );
  ++objCount;
  return indirectQueue.back( );
}

RefCountPtr< const std::vector< RefCountPtr< const MetaPDF::Lang::XObject > > >
SimplePDF::PDF_out::addPagesAsXObjects( RefCountPtr< PDF_in > pdfi )
{
  using namespace MetaPDF;
  
  importSources.push_back( pdfi ); // Keep the source alive so that it can be used when finally producing output
  IndirectRemapType indirectRemap;
  vector< RefCountPtr< const Lang::XObject > > * res = new vector< RefCountPtr< const Lang::XObject > >;
  for( size_t pageNumber( 0 ); pageNumber < pdfi->getPageCount( ); ++pageNumber )
    {
      RefCountPtr< PDF_Dictionary > pageDic( pdfi->getPage( pageNumber ) );
      RefCountPtr< PDF_Vector > cropBox = SimplePDF::down_cast_follow< SimplePDF::PDF_Vector >( pageDic->getInheritable( "CropBox" ) );
      if( cropBox == NullPtr< PDF_Vector >( ) )
	{
	  cropBox = SimplePDF::down_cast_follow< SimplePDF::PDF_Vector >( pageDic->getInheritable( "MediaBox" ) );
	}
      if( cropBox == NullPtr< PDF_Vector >( ) )
	{
	  throw Exceptions::InternalError( strrefdup( "Failed to find crop box of imported page.  (Searched to the page tree root.)" ) );
	}
      RefCountPtr< PDF_Stream_in > original( pdfi->follow< PDF_Stream_in >( (*pageDic)[ "Contents" ] ) );

//       RefCountPtr< PDF_Dictionary > xobjectDic( pdfi->follow< PDF_Dictionary >( (*pdfi->follow< PDF_Dictionary >( (*pageDic)[ "Resources" ] ))[ "XObject" ] ) );
//       if( xobjectDic->dic.size( ) != 1 )
// 	{
// 	  throw Exceptions::InternalError( strrefdup( "Expected exactly 1 XObject on a TeX label input page" ) );
// 	}
//       RefCountPtr< PDF_Stream_in > original( pdfi->follow< PDF_Stream_in >( xobjectDic->dic.begin( )->second ) );
      RefCountPtr< PDF_Stream_in > newObj( new PDF_Stream_in( original->is, original->dataStart ) );
      (*newObj)[ "Subtype" ] = newName( "Form" );
      (*newObj)[ "FormType" ] = newInt( 1 );
      (*newObj)[ "BBox" ] = cropBox;               // ->rectangleIntersection( SimplePDF::down_cast_follow< SimplePDF::PDF_Vector >( (*original)[ "BBox" ] ) );
      (*newObj)[ "Matrix" ] = RefCountPtr< PDF_Object >( new PDF_Vector( 1, 0, 0, 1, 0, 0 ) );
      (*newObj)[ "Filter" ] = (*original)[ "Filter" ];
      (*newObj)[ "Length" ] = RefCountPtr< PDF_Object >( new PDF_Int( pdfi->follow< PDF_Int >( (*original)[ "Length" ] )->value( ) ) );
      (*newObj)[ "Resources" ] = deepCopy( (*pageDic)[ "Resources" ], this, & indirectRemap );

      Concrete::Length xmin;
      Concrete::Length xmax;
      Concrete::Length ymin;
      Concrete::Length ymax;
      try
	{
	  RefCountPtr< PDF_Vector > bboxTyped = pdfi->follow< PDF_Vector >( (*newObj)[ "BBox" ] );
	  if( bboxTyped->vec.size( ) != 4 )
	    {
	      throw Exceptions::InternalError( strrefdup( "The bbox of the imported page was not of size 4." ) );
	    }
	  xmin = Concrete::Length( pdfi->follow< PDF_Float >( bboxTyped->vec[ 0 ] )->value( ) );
	  xmax = Concrete::Length( pdfi->follow< PDF_Float >( bboxTyped->vec[ 2 ] )->value( ) );
	  ymin = Concrete::Length( pdfi->follow< PDF_Float >( bboxTyped->vec[ 1 ] )->value( ) );
	  ymax = Concrete::Length( pdfi->follow< PDF_Float >( bboxTyped->vec[ 3 ] )->value( ) );
	}
      catch( Exceptions::Exception & ball )
	{
	  throw;
	}
      catch( const char * ball )
	{
	  std::ostringstream oss;
	  oss << "An error occurred while evaluating the bbox of the imported page: " << ball ;
	  throw Exceptions::InternalError( strrefdup( oss.str( ).c_str( ) ) );
	}
      catch( ... )
	{
	  throw Exceptions::InternalError( strrefdup( "An error occurred while evaluating the bbox of the imported page." ) );
	}
      Lang::ElementaryPath2D * bboxpath = new Lang::ElementaryPath2D;
      bboxpath->push_back( new Concrete::PathPoint2D( xmin, ymin ) );
      bboxpath->push_back( new Concrete::PathPoint2D( xmin, ymax ) );
      bboxpath->push_back( new Concrete::PathPoint2D( xmax, ymax ) );
      bboxpath->push_back( new Concrete::PathPoint2D( xmax, ymin ) );
      bboxpath->close( );

      RefCountPtr< PDF_Object > indirection = indirect( newObj );
      res->push_back( RefCountPtr< Lang::XObject >( new Lang::XObject( indirection, RefCountPtr< const Lang::ElementaryPath2D >( bboxpath ) ) ) );
    }
  return RefCountPtr< const std::vector< RefCountPtr< const Lang::XObject > > >( res );
}

void
SimplePDF::PDF_out::importBtexEtexThings( RefCountPtr< PDF_in > pdfi, MetaPDF::Kernel::TeXLabelManager::MapType * dstMap, const std::string & setupCodeHash )
{
  using namespace MetaPDF;

  importSources.push_back( pdfi ); // Keep the source alive so that it can be used when finally producing output
  IndirectRemapType indirectRemap;

  SimplePDF::PDF_in::PageIterator theEnd = pdfi->endPages( );
  for( SimplePDF::PDF_in::PageIterator i = pdfi->beginPages( ); i != theEnd; ++i )
    {
      RefCountPtr< PDF_Dictionary > xobjectDic( pdfi->follow< PDF_Dictionary >( (*pdfi->follow< PDF_Dictionary >( (**i)[ "Resources" ] ))[ "XObject" ] ) );
      if( xobjectDic->dic.size( ) != 1 )
	{
	  throw Exceptions::InternalError( strrefdup( "Expected exactly 1 XObject on a TeX label input page" ) );
	}
      RefCountPtr< PDF_Stream_in > original( pdfi->follow< PDF_Stream_in >( xobjectDic->dic.begin( )->second ) );

      string texStr;
      {
	RefCountPtr< PDF_Stream_in > texStream( pdfi->follow< PDF_Stream_in >( (*original)[ "TeXsrc" ] ) );
	ostringstream tmp;
	texStream->writeDataDefilteredTo( tmp );
	texStr = tmp.str( );
      }
      if( i == pdfi->beginPages( ) )
	{
	  /* This page only contains information about the TeX context */
	  if( texStr != setupCodeHash )
	    {
	      throw Exceptions::TeXSetupHasChanged( );
	    }
	  continue;
	}
      Concrete::Length height( pdfNameToDouble( (*original)[ "TeXht" ] ) );
      Concrete::Length depth( pdfNameToDouble( (*original)[ "TeXdp" ] ) );
      Concrete::Length width( pdfNameToDouble( (*original)[ "TeXwd" ] ) );

      const Concrete::Length bboxAddY = 0.04 * ( depth + height );
      const Concrete::Length xmin = Concrete::ZERO_LENGTH;
      const Concrete::Length xmax = width;
      const Concrete::Length ymin = -depth - bboxAddY;
      const Concrete::Length ymax = height + bboxAddY;

      /* The bbox we get from pdfLaTeX is too small!
	 It has to be grown a little.
	 (*newObj)[ "BBox" ] = (*original)[ "BBox" ];
      */
      /*
      cerr << "minmax: " << xmin << " " << ymin << " " << xmax << " " << ymax << endl ;

      cerr << "texbbox: " ;
      RefCountPtr< PDF_Vector > texbboxref = pdfi->follow< PDF_Vector >( (*original)[ "BBox" ] );
      double texbbox[ 4 ];
      for( size_t i = 0; i < 4; ++i )
        {
          texbbox[ i ] = pdfi->follow< PDF_Float >( texbboxref->vec[ i ] )->value( );
          cerr << texbbox[i] << " " ;
        }
      cerr << endl ;
      */
      RefCountPtr< PDF_Stream_in > newObj( new PDF_Stream_in( original->is, original->dataStart ) );
      (*newObj)[ "Subtype" ] = newName( "Form" );
      (*newObj)[ "FormType" ] = newInt( 1 );
      (*newObj)[ "BBox" ] = RefCountPtr< PDF_Object >( new PDF_Vector( 0.0,
								       - bboxAddY.offtype< 1, 0 >( ),
								       ( xmax - xmin ).offtype< 1, 0 >( ),
								       ( ymax - ymin ).offtype< 1, 0 >( ) ) );
      //      (*newObj)[ "BBox" ] = (*original)[ "BBox" ];
      (*newObj)[ "Matrix" ] = RefCountPtr< PDF_Object >( new PDF_Vector( 1, 0, 0, 1, 0, - depth.offtype< 1, 0 >( ) ) );
      (*newObj)[ "Filter" ] = (*original)[ "Filter" ];
      (*newObj)[ "Length" ] = RefCountPtr< PDF_Object >( new PDF_Int( pdfi->follow< PDF_Int >( (*original)[ "Length" ] )->value( ) ) );
      (*newObj)[ "Resources" ] = deepCopy( (*original)[ "Resources" ], this, & indirectRemap );

      Lang::ElementaryPath2D * bboxpath = new Lang::ElementaryPath2D;
      bboxpath->push_back( new Concrete::PathPoint2D( xmin, ymin ) );
      bboxpath->push_back( new Concrete::PathPoint2D( xmin, ymax ) );
      bboxpath->push_back( new Concrete::PathPoint2D( xmax, ymax ) );
      bboxpath->push_back( new Concrete::PathPoint2D( xmax, ymin ) );
      bboxpath->close( );

      if( dstMap->find( texStr ) != dstMap->end( ) )
	{
	  throw Exceptions::InternalError( strrefdup( "Multiply generated TeX label: " + texStr ) );
	}

      RefCountPtr< PDF_Object > indirection = indirect( newObj );
      dstMap->insert( MetaPDF::Kernel::TeXLabelManager::MapType::value_type( texStr, RefCountPtr< const Lang::XObject >( new Lang::XObject( indirection, RefCountPtr< const Lang::ElementaryPath2D >( bboxpath ) ) ) ) );
    }
}

double
SimplePDF::PDF_out::pdfNameToDouble( RefCountPtr< PDF_Object > nameObject )
{
  RefCountPtr< PDF_Name > name( nameObject.down_cast< PDF_Name >( ) );
  if( name == NullPtr< PDF_Name >( ) )
    {
      throw( "PDF_out::pdfNameToDouble: The object was not a name" );
    }
  const double pointToBigPointFactor = 72 / 72.27;
  char * end;
  double res = pointToBigPointFactor * strtod( name->name( ).c_str( ), & end );
  if( strcmp( end, "pt" ) != 0 )
    {
      throw( "PDF_out::pdfNameToDouble: Expected \"pt\" to follow length number" );
    }
  return res;
}


RefCountPtr<PDF_Object>
SimplePDF::PDF_out::newName( const char * str )
{
  return RefCountPtr<PDF_Name>( new PDF_Name( str ) );
}

RefCountPtr<PDF_Object>
SimplePDF::PDF_out::newString( const char * str )
{
  return RefCountPtr< PDF_String >( new PDF_LiteralString( str ) );
}

RefCountPtr<PDF_Object>
SimplePDF::PDF_out::newInt( PDF_Int::ValueType val )
{
  return RefCountPtr<PDF_Int>( new PDF_Int( val ) );
}

RefCountPtr<PDF_Object>
SimplePDF::PDF_out::newBoolean( PDF_Boolean::ValueType val )
{
  if( val )
    {
      return theTrue;
    }
  return theFalse;
}

RefCountPtr<PDF_Object>
SimplePDF::PDF_out::newFloat( PDF_Float::ValueType val )
{
  return RefCountPtr<PDF_Float>( new PDF_Float( val ) );
}

void
SimplePDF::PDF_out::versionMessage( Version required, const char * message )
{
  using namespace MetaPDF;
  
  switch( versionAction_ )
    {
    case ERROR:
      throw Exceptions::PDFVersionError( version_, required, message );
      break;
    case WARN:
      std::cerr << toString( version_ ) << " warning: " << message << std::endl ;
      break;
    case SILENT:
      // Just be quiet.
      break;
    default:
      throw Exceptions::InternalError( "PDF_out versionAction_ out of range." );
    }
}

const char *
SimplePDF::PDF_out::toString( SimplePDF::PDF_out::Version version )
{
  switch( version )
    {
    case PDF_X:
      return "PDF-X" ;
    case PDF_1_1:
      return "PDF-1.1" ;
    case PDF_1_2:
      return "PDF-1.2" ;
    case PDF_1_3:
      return "PDF-1.3" ;
    case PDF_1_4:
      return "PDF-1.4" ;
    case PDF_1_5:
      return "PDF-1.5" ;
    case PDF_1_6:
      return "PDF-1.6" ;
    default:
      throw MetaPDF::Exceptions::InternalError( "PDF version out of range." );
    }
}

RefCountPtr<PDF_Object> SimplePDF::theTrue( new PDF_Boolean( true ) );
RefCountPtr<PDF_Object> SimplePDF::theFalse( new PDF_Boolean( false ) );

