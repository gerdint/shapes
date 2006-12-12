#include "strrefdup.h"
#include "charconverters.h"
#include "metapdfexceptions.h"
#include "glyphlist.h"
#include "texttypes.h"
#include "iconvselect.h"


#include <string>
#include <fstream>
#include <sstream>



using namespace MetaPDF;

const char * Helpers::theUCS4EncodingName = "UCS-4-INTERNAL";

iconv_t
Helpers::requireUTF8ToMacRomanConverter( bool cleanup )
{
  static iconv_t converter = (iconv_t)( - 1 );
  if( cleanup )
    {
      if( converter != (iconv_t)( -1 ) )
	{
	  iconv_close( converter );
	  converter = (iconv_t)( -1 );
	}
    }
  else
    {
      if( converter == (iconv_t)( -1 ) )
	{
	  const char * iconv_Encoding_name = "Macintosh";  // This is meant to be what is called MacRoman in PDF.
	  converter = iconv_open( iconv_Encoding_name, "UTF-8" );
	  if( converter == (iconv_t)( -1 ) )
	    {
	      std::ostringstream msg;
	      msg << "iconv_open failed to create converter from UTF-8 to " << iconv_Encoding_name << "." ;
	      throw Exceptions::ExternalError( strrefdup( msg ) );
	    }
	}
    }
  return converter;
}

iconv_t
Helpers::requireMacRomanToUTF8Converter( bool cleanup )
{
  static iconv_t converter = (iconv_t)( - 1 );
  if( cleanup )
    {
      if( converter != (iconv_t)( -1 ) )
	{
	  iconv_close( converter );
	  converter = (iconv_t)( -1 );
	}
    }
  else
    {
      if( converter == (iconv_t)( -1 ) )
	{
	  const char * iconv_Encoding_name = "Macintosh";  // This is meant to be what is called MacRoman in PDF.
	  converter = iconv_open( "UTF-8", iconv_Encoding_name );
	  if( converter == (iconv_t)( -1 ) )
	    {
	      std::ostringstream msg;
	      msg << "iconv_open failed to create converter to UTF-8 from " << iconv_Encoding_name << "." ;
	      throw Exceptions::ExternalError( strrefdup( msg ) );
	    }
	}
    }
  return converter;
}

iconv_t
Helpers::requireUTF8ToASCIIConverter( bool cleanup )
{
  static iconv_t converter = (iconv_t)( - 1 );
  if( cleanup )
    {
      if( converter != (iconv_t)( -1 ) )
	{
	  iconv_close( converter );
	  converter = (iconv_t)( -1 );
	}
    }
  else
    {
      if( converter == (iconv_t)( -1 ) )
	{
	  const char * iconv_Encoding_name = "ASCII";  // This is used for the names of glyphs in a font
	  converter = iconv_open( iconv_Encoding_name, "UTF-8" );
	  if( converter == (iconv_t)( -1 ) )
	    {
	      std::ostringstream msg;
	      msg << "iconv_open failed to create converter from UTF-8 to " << iconv_Encoding_name << "." ;
	      throw Exceptions::ExternalError( strrefdup( msg ) );
	    }
	}
    }
  return converter;
}

iconv_t
Helpers::requireUTF8ToUCS4Converter( bool cleanup )
{
  static iconv_t converter = (iconv_t)( - 1 );
  if( cleanup )
    {
      if( converter != (iconv_t)( -1 ) )
	{
	  iconv_close( converter );
	  converter = (iconv_t)( -1 );
	}
    }
  else
    {
      if( converter == (iconv_t)( -1 ) )
	{
	  const char * iconv_Encoding_name = Helpers::theUCS4EncodingName;  // This is used for the glyph list.
	  converter = iconv_open( iconv_Encoding_name, "UTF-8" );
	  if( converter == (iconv_t)( -1 ) )
	    {
	      std::ostringstream msg;
	      msg << "iconv_open failed to create converter from UTF-8 to " << iconv_Encoding_name << "." ;
	      throw Exceptions::ExternalError( strrefdup( msg ) );
	    }
	}
    }
  return converter;
}

iconv_t
Helpers::requireUTF16BEToUCS4Converter( bool cleanup )
{
  static iconv_t converter = (iconv_t)( - 1 );
  if( cleanup )
    {
      if( converter != (iconv_t)( -1 ) )
	{
	  iconv_close( converter );
	  converter = (iconv_t)( -1 );
	}
    }
  else
    {
      if( converter == (iconv_t)( -1 ) )
	{
	  const char * iconv_Encoding_name = Helpers::theUCS4EncodingName;  // This is used for the glyph list.
	  converter = iconv_open( iconv_Encoding_name, "UTF-16BE" );
	  if( converter == (iconv_t)( -1 ) )
	    {
	      std::ostringstream msg;
	      msg << "iconv_open failed to create converter from UTF-8 to " << iconv_Encoding_name << "." ;
	      throw Exceptions::ExternalError( strrefdup( msg ) );
	    }
	}
    }
  return converter;
}

iconv_t
Helpers::requireUTF8ToWinANSIConverter( bool cleanup )
{
  static iconv_t converter = (iconv_t)( - 1 );
  if( cleanup )
    {
      if( converter != (iconv_t)( -1 ) )
	{
	  iconv_close( converter );
	  converter = (iconv_t)( -1 );
	}
    }
  else
    {
      if( converter == (iconv_t)( -1 ) )
	{
	  const char * iconv_Encoding_name = "LATIN1";  // This is meant to be what is called WinANSI in PDF.
	  converter = iconv_open( iconv_Encoding_name, "UTF-8" );
	  if( converter == (iconv_t)( -1 ) )
	    {
	      std::ostringstream msg;
	      msg << "iconv_open failed to create converter from UTF-8 to " << iconv_Encoding_name << "." ;
	      throw Exceptions::ExternalError( strrefdup( msg ) );
	    }
	}
    }
  return converter;
}

const FontMetrics::GlyphList &
Helpers::requireGlyphList( bool cleanup )
{
  static const FontMetrics::GlyphList * converter = 0;
  if( cleanup )
    {
      if( converter != 0 )
	{
	  delete converter;
	  converter = 0;
	}
    }
  else
    {
      if( converter == 0 )
	{
	  std::string filename = Lang::Font::searchGlyphList( );
	  std::ifstream iFile( filename.c_str( ) );
	  if( ! iFile.is_open( ) )
	    {
	      std::ostringstream oss;
	      oss << "Could locate, but not open the glyph list " << filename ;
	      throw Exceptions::ExternalError( strrefdup( oss ) );
	    }
	  try
	    {
	      converter = new FontMetrics::GlyphList( iFile );
	    }
	  catch( const char * ball )
	    {
	      std::ostringstream oss;
	      oss << "Parsing the glyph list " << filename << " resulted in the error: " << ball ;
	      throw Exceptions::ExternalError( strrefdup( oss ) );
	    }
	  catch( const std::string ball )
	    {
	      std::ostringstream oss;
	      oss << "Parsing the glyph list " << filename << " resulted in the error: " << ball ;
	      throw Exceptions::ExternalError( strrefdup( oss ) );
	    }
	  catch( const MetaPDF::Exceptions::Exception & ball )
	    {
	      std::cerr << "Parsing the glyph list " << filename << " resulted an error.  Rethrowing." << std::endl ;
	      throw;
	    }
	  catch( ... )
	    {
	      throw Exceptions::InternalError( "An unrecognized exception was caught from glyph list parsing." );
	    }
	}
    }
  return *converter;
}
