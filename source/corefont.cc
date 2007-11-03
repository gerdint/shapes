#include "metapdfcore.h"
#include "globals.h"
#include "charconverters.h"
#include "autoonoff.h"
#include "ast.h"
#include "glyphlist.h"

using namespace MetaPDF;


namespace MetaPDF
{
  namespace Lang
  {
    class Core_makeglyph : public Lang::CoreFunction
    {
      Lang::Type3Glyph::Kind kind_;
    public:
      Core_makeglyph( const char * title, Lang::Type3Glyph::Kind kind )
	: CoreFunction( title, new Kernel::EvaluatedFormals( title, true ) ), kind_( kind )
      {
	formals_->appendEvaluatedCoreFormal( "width", Kernel::THE_SLOT_VARIABLE );
	formals_->appendEvaluatedCoreFormal( "glyph", Kernel::THE_SLOT_VARIABLE );
	formals_->appendEvaluatedCoreFormal( "char", Kernel::THE_VOID_VARIABLE );
	formals_->appendEvaluatedCoreFormal( "name", Kernel::THE_VOID_VARIABLE );
      }
      virtual void
      call( Kernel::EvalState * evalState, Kernel::Arguments & args, const Ast::SourceLocation & callLoc ) const
      {
	args.applyDefaults( );

	size_t argsi = 0;
	typedef const Lang::Length WidthType;
	RefCountPtr< WidthType > widthX = Helpers::down_cast_CoreArgument< WidthType >( title_, args, argsi, callLoc );

	++argsi;
	typedef const Lang::Drawable2D GlyphType;
	RefCountPtr< GlyphType > glyph = Helpers::down_cast_CoreArgument< GlyphType >( title_, args, argsi, callLoc );

	++argsi;
	typedef const Lang::String CodeType;
	RefCountPtr< CodeType > codeStringUTF8 = Helpers::down_cast_CoreArgument< CodeType >( title_, args, argsi, callLoc, true );

	++argsi;
	typedef const Lang::Symbol NameType;
	RefCountPtr< NameType > nameUTF8 = RefCountPtr< NameType >( NullPtr< NameType >( ) );
	RefCountPtr< CodeType > nameStringUTF8 = codeStringUTF8;
	{
	  try
	    {
	      // If the argument is void, this goes through with nameUTF8 being null.  We will then refer to nameStringUTF8 as set above.
	      nameUTF8 = Helpers::try_cast_CoreArgument< NameType >( args.getValue( argsi ), true );
	      goto foundNameType;
	    }
	  catch( const NonLocalExit::NotThisType & ball )
	    {
	      // Never mind, but see below!
	    }

	  try
	    {
	      // If we reach here, the value is present, but not a Symbol.  Hence we require that it be a string value for the name that replaces the default value being that of codeStringUTF8.
	      nameStringUTF8 = Helpers::try_cast_CoreArgument< CodeType >( args.getValue( argsi ) );
	      goto foundNameType;
	    }
	  catch( const NonLocalExit::NotThisType & ball )
	    {
	      // Never mind, but see below!
	    }

	  throw Exceptions::CoreTypeMismatch( callLoc, title_, args, argsi, Helpers::typeSetString( Lang::Symbol::staticTypeName( ), Lang::String::staticTypeName( ) ) );
	}
      foundNameType:

	// Convert the character code string to a number:
	size_t code = 0;
	if( codeStringUTF8 != NullPtr< CodeType >( ) )
	  {
	    iconv_t converter = Helpers::requireUTF8ToMacRomanConverter( );
      
	    const char * inbuf = codeStringUTF8->val_.getPtr( );
      
	    size_t bufSize = strlen( inbuf );
	    char * buf = new char[ bufSize + 1 ];
	    DeleteOnExit< char > bufDeleter( buf );
      
	    char * outbuf = buf;
	    size_t inbytesleft = bufSize;
	    size_t outbytesleft = bufSize;
	    // For some reason, my iconv header seems unaware of the const modifier...
	    size_t count = iconv( converter,
				  & inbuf, & inbytesleft,
				  & outbuf, & outbytesleft );
	    if( count == (size_t)(-1) )
	      {
		if( errno == EILSEQ )
		  {
		    throw Exceptions::MiscellaneousRequirement( "It is suspected that one of the UFT-8 character cannot be used as a character code in a font with MacRoman encoding." );
		  }
		else if( errno == EINVAL )
		  {
		    throw Exceptions::MiscellaneousRequirement( "It is suspected that glyph's character code ended with an incomplete multibyte character." );
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
	    if( strlen( buf ) != 1 )
	      {
		throw Exceptions::CoreRequirement( "The glyph's character code must be a string with exactly one character, or not be specified at all.", title_, callLoc );
	      }
	    code = *reinterpret_cast< const unsigned char * >( buf );
	  }

	RefCountPtr< const char > name = RefCountPtr< const char >( NullPtr< const char >( ) );
	if( nameUTF8 == NullPtr< NameType >( ) )
	  {
	    if( nameStringUTF8 == NullPtr< CodeType >( ) )
	      {
		throw Exceptions::CoreRequirement( "At least one of <char> and <name> must be specified.", title_, callLoc );
	      }
	    const FontMetrics::GlyphList & glyphList = Helpers::requireGlyphList( );
	    const char * dst;
	    if( ! glyphList.UTF8_to_name( nameStringUTF8->val_.getPtr( ), & dst ) )
	      {
		throw Exceptions::CoreOutOfRange( title_, args, 2, "When no name is given, characters without default names are not allowed.  Please refer to the glyph list." );
	      }
	    name = strrefdup( dst );
	  }
	else
	  {
	    // Ensure that the glyph name is legal
	    iconv_t converter = Helpers::requireUTF8ToASCIIConverter( );
      
	    const char * inbuf = nameUTF8->name( ).getPtr( );
      
	    size_t bufSize = strlen( inbuf );
	    char * buf = new char[ bufSize + 1 ];
	    name = RefCountPtr< const char >( buf );  // this will delete the buffer if it becomes unused.
      
	    char * outbuf = buf;
	    size_t inbytesleft = bufSize;
	    size_t outbytesleft = bufSize;
	    // For some reason, my iconv header seems unaware of the const modifier...
	    size_t count = iconv( converter,
				  & inbuf, & inbytesleft,
				  & outbuf, & outbytesleft );
	    if( count == (size_t)(-1) )
	      {
		if( errno == EILSEQ )
		  {
		    throw Exceptions::CoreOutOfRange( title_, args, 3, "A non-ASCII character was found in a glyph name." );
		  }
		else if( errno == EINVAL )
		  {
		    throw Exceptions::MiscellaneousRequirement( "It is suspected that glyph's name ended with an incomplete multibyte character." );
		  }
		else if( errno == E2BIG )
		  {
		    throw Exceptions::InternalError( "The buffer allocated for UTF-8 to ASCII conversion was too small." );
		  }
		else
		  {
		    std::ostringstream msg;
		    msg << "iconv failed with an unrecognized error code: " << errno ;
		    throw Exceptions::InternalError( strrefdup( msg ) );
		  }
	      }
	    *outbuf = '\0';
	  }

	// Find boudning box:
	RefCountPtr< const Lang::ElementaryPath2D > theBBox = glyph->bbox( );
	Concrete::Coords2D llcorner( 0, 0 );
	Concrete::Coords2D urcorner( 0, 0 );
	if( ! theBBox->boundingRectangle( & llcorner, & urcorner ) )
	  {
	    std::string strTitle( title_ );
	    throw Exceptions::InternalError( strrefdup( strTitle + ": The glyph has no bounding box!" ) );
	  }

	Kernel::ContRef cont = evalState->cont_;
	cont->takeValue( Kernel::ValueRef( new Lang::Type3Glyph( kind_,
								 code,
								 name,
								 glyph,
								 widthX->get( ),
								 llcorner.x_,
								 llcorner.y_,
								 urcorner.x_,
								 urcorner.y_ ) ),
			 evalState );
      }
    };

  }
}


void
Kernel::registerCore_font( Kernel::Environment * env )
{
  env->initDefineCoreFunction( new Lang::Core_makeglyph( "basicglyph", Lang::Type3Glyph::BASIC ) );
  env->initDefineCoreFunction( new Lang::Core_makeglyph( "coloredglyph", Lang::Type3Glyph::COLORED ) );

  env->initDefine( "font_TIMES_ROMAN", RefCountPtr< const Lang::Font >( new Lang::Font( BuiltInFonts::TIMES_ROMAN ) ) );
  env->initDefine( "font_TIMES_BOLD", RefCountPtr< const Lang::Font >( new Lang::Font( BuiltInFonts::TIMES_BOLD ) ) );
  env->initDefine( "font_TIMES_ITALIC", RefCountPtr< const Lang::Font >( new Lang::Font( BuiltInFonts::TIMES_ITALIC ) ) );
  env->initDefine( "font_TIMES_BOLDITALIC", RefCountPtr< const Lang::Font >( new Lang::Font( BuiltInFonts::TIMES_BOLDITALIC ) ) );
  env->initDefine( "font_HELVETICA", Lang::THE_FONT_HELVETICA );
  env->initDefine( "font_HELVETICA_BOLD", RefCountPtr< const Lang::Font >( new Lang::Font( BuiltInFonts::HELVETICA_BOLD ) ) );
  env->initDefine( "font_HELVETICA_OBLIQUE", RefCountPtr< const Lang::Font >( new Lang::Font( BuiltInFonts::HELVETICA_OBLIQUE ) ) );
  env->initDefine( "font_HELVETICA_BOLDOBLIQUE", RefCountPtr< const Lang::Font >( new Lang::Font( BuiltInFonts::HELVETICA_BOLDOBLIQUE ) ) );
  env->initDefine( "font_COURIER", RefCountPtr< const Lang::Font >( new Lang::Font( BuiltInFonts::COURIER ) ) );
  env->initDefine( "font_COURIER_BOLD", RefCountPtr< const Lang::Font >( new Lang::Font( BuiltInFonts::COURIER_BOLD ) ) );
  env->initDefine( "font_COURIER_OBLIQUE", RefCountPtr< const Lang::Font >( new Lang::Font( BuiltInFonts::COURIER_OBLIQUE ) ) );
  env->initDefine( "font_COURIER_BOLDOBLIQUE", RefCountPtr< const Lang::Font >( new Lang::Font( BuiltInFonts::COURIER_BOLDOBLIQUE ) ) );
  env->initDefine( "font_SYMBOL", RefCountPtr< const Lang::Font >( new Lang::Font( BuiltInFonts::SYMBOL ) ) );
  env->initDefine( "font_ZAPFDINGBATS", RefCountPtr< const Lang::Font >( new Lang::Font( BuiltInFonts::ZAPFDINGBATS ) ) );
}
