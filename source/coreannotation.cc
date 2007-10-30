#include "MetaPDF_Helpers_decls.h"

#include "metapdfcore.h"
#include "annotations.h"
#include "globals.h"
#include "metapdfexceptions.h"
#include "consts.h"
#include "simplepdfi.h"
#include "simplepdfo.h"
#include "tagtypes.h"
#include "continuations.h"
#include "pagecontentstates.h"

#include <iostream>
#include <sstream>

using namespace MetaPDF;

namespace MetaPDF
{
  namespace Helpers
  {
    char
    takeHighlightArgument( const char * coreTitle, Kernel::Arguments & args, size_t i, const Ast::SourceLocation & callLoc )
    {
      typedef const Lang::Symbol T;
      RefCountPtr< const Lang::Value > untyped = args.getValue( i );
      RefCountPtr< T > val = untyped.down_cast< T >( );
      
      char highlight = 'I'; // This is the default.
      if( val == NullPtr< T >( ) )
	{
	  if( dynamic_cast< const Lang::Void * >( untyped.getPtr( ) ) == 0 )
	    {
	      throw Exceptions::CoreTypeMismatch( callLoc, coreTitle, args.getLoc( i ), untyped->getTypeName( ), T::staticTypeName( ) );
	    }
	}
      else
	{
	  static Lang::Symbol HIGHLIGHT_None( "none" );
	  static Lang::Symbol HIGHLIGHT_Invert( "invert" );
	  static Lang::Symbol HIGHLIGHT_Outline( "outline" );
	  static Lang::Symbol HIGHLIGHT_Push( "push" );
	  if( *val == HIGHLIGHT_None )
	    {
	      highlight = 'N';
	    }
	  else if( *val == HIGHLIGHT_Invert )
	    {
	      highlight = 'I';
	    }
	  else if( *val == HIGHLIGHT_Outline )
	    {
	      highlight = 'O';
	    }
	  else if( *val == HIGHLIGHT_Push )
	    {
	      highlight = 'P';
	    }
	  else
	    {
	      std::ostringstream oss;
	      oss << "Valid highlight styles are the symbols { "
		  << HIGHLIGHT_None.name( ).getPtr( ) << ", "
		  << HIGHLIGHT_Invert.name( ).getPtr( ) << ", "
		  << HIGHLIGHT_Outline.name( ).getPtr( ) << ", "
		  << HIGHLIGHT_Push.name( ).getPtr( )
		  << " }." ;
	      throw Exceptions::CoreOutOfRange( coreTitle, args, i, strrefdup( oss ) );
	    }
	}
      return highlight;
    }
  }
}


namespace MetaPDF
{
  namespace Lang
  {
    class Core_annotationsite : public Lang::CoreFunction
    {
    public:
      Core_annotationsite( const char * title )
	: CoreFunction( title, new Kernel::EvaluatedFormals( title, true ) )
      {
	/* General arguments.  Only <target> is required.
	 */
	formals_->appendEvaluatedCoreFormal( "target", Kernel::THE_SLOT_VARIABLE );
	formals_->appendEvaluatedCoreFormal( "text", Kernel::THE_VOID_VARIABLE );
	formals_->appendEvaluatedCoreFormal( "color", Helpers::newValHandle( new Lang::RGB( Concrete::RGB( 0, 0, 0 ) ) ) );
	formals_->appendEvaluatedCoreFormal( "name", Kernel::THE_VOID_VARIABLE );

	/* The following define the border.
	 */
	formals_->appendEvaluatedCoreFormal( "style", Kernel::THE_VOID_VARIABLE );
	formals_->appendEvaluatedCoreFormal( "width", Helpers::newValHandle( new Lang::Length( Concrete::Length( 1 ) ) ) );
	formals_->appendEvaluatedCoreFormal( "dash", Kernel::THE_VOID_VARIABLE );
	formals_->appendEvaluatedCoreFormal( "cloudy", Kernel::THE_VOID_VARIABLE );

	/* The following are appearances.
	 */
	formals_->appendEvaluatedCoreFormal( "normal", Kernel::THE_VOID_VARIABLE );
	formals_->appendEvaluatedCoreFormal( "rollover", Kernel::THE_VOID_VARIABLE );
	formals_->appendEvaluatedCoreFormal( "down", Kernel::THE_VOID_VARIABLE );

	/* The following boolean arguments correspond to flags.
	 */
	formals_->appendEvaluatedCoreFormal( "invisible", Kernel::THE_FALSE_VARIABLE );
	formals_->appendEvaluatedCoreFormal( "hidden", Kernel::THE_FALSE_VARIABLE );
	formals_->appendEvaluatedCoreFormal( "print", Kernel::THE_FALSE_VARIABLE );
	formals_->appendEvaluatedCoreFormal( "zoom", Kernel::THE_TRUE_VARIABLE );
	formals_->appendEvaluatedCoreFormal( "rotate", Kernel::THE_TRUE_VARIABLE );
	formals_->appendEvaluatedCoreFormal( "view", Kernel::THE_TRUE_VARIABLE );
	formals_->appendEvaluatedCoreFormal( "writable", Kernel::THE_TRUE_VARIABLE );
	formals_->appendEvaluatedCoreFormal( "locked", Kernel::THE_FALSE_VARIABLE );
	formals_->appendEvaluatedCoreFormal( "toggle", Kernel::THE_FALSE_VARIABLE );
      }
      virtual void
      call( Kernel::EvalState * evalState, Kernel::Arguments & args, const Ast::SourceLocation & callLoc ) const
      {
	args.applyDefaults( );

	size_t argsi = 0;
	typedef const Lang::Drawable2D TargetType;
	RefCountPtr< TargetType > target = Helpers::down_cast_CoreArgument< TargetType >( title_, args, argsi, callLoc );

	++argsi;
	typedef const Lang::String ContentTextType;
	RefCountPtr< ContentTextType > contentTextVal = Helpers::down_cast_CoreArgument< ContentTextType >( title_, args, argsi, callLoc, true );
	RefCountPtr< const char > contentText = RefCountPtr< const char >( NullPtr< const char >( ) );
	if( contentTextVal != NullPtr< ContentTextType >( ) )
	  {
	    contentText = contentTextVal->val_;
	  }
  
	++argsi;
	typedef const Lang::RGB ColorType;
	Concrete::RGB color = Helpers::down_cast_CoreArgument< ColorType >( title_, args, argsi, callLoc )->components( );

	++argsi;
	typedef const Lang::String IdentifierType;
	RefCountPtr< IdentifierType > identifierVal = Helpers::down_cast_CoreArgument< IdentifierType >( title_, args, argsi, callLoc, true );
	RefCountPtr< const char > identifier = RefCountPtr< const char >( NullPtr< const char >( ) );
	if( identifierVal != NullPtr< IdentifierType >( ) )
	  {
	    identifier = identifierVal->val_;
	  }

	++argsi;
	typedef const Lang::Symbol StyleType;
	RefCountPtr< StyleType > styleVal = Helpers::down_cast_CoreArgument< StyleType >( title_, args, argsi, callLoc, true );
	char borderStyle = 'S';
	static Lang::Symbol STYLE_Solid( "solid" );
	static Lang::Symbol STYLE_Dashed( "dashed" );
	static Lang::Symbol STYLE_Beveled( "beveled" );
	static Lang::Symbol STYLE_Inset( "inset" );
	static Lang::Symbol STYLE_Underline( "underline" );
	if( styleVal != NullPtr< StyleType >( ) )
	  {
	    if( *styleVal == STYLE_Solid )
	      {
		borderStyle = 'S';
	      }
	    else if( *styleVal == STYLE_Dashed )
	      {
		borderStyle = 'D';
	      }
	    else if( *styleVal == STYLE_Beveled )
	      {
		borderStyle = 'B';
	      }
	    else if( *styleVal == STYLE_Inset )
	      {
		borderStyle = 'I';
	      }
	    else if( *styleVal == STYLE_Underline )
	      {
		borderStyle = 'U';
	      }
	    else
	      {
		std::ostringstream oss;
		oss << "Valid border styles are the symbols { "
		    << STYLE_Solid.name( ).getPtr( ) << ", "
		    << STYLE_Dashed.name( ).getPtr( ) << ", "
		    << STYLE_Beveled.name( ).getPtr( ) << ", "
		    << STYLE_Inset.name( ).getPtr( ) << ", "
		    << STYLE_Underline.name( ).getPtr( )
		    << " }." ;
		throw Exceptions::CoreOutOfRange( title_, args, argsi, strrefdup( oss ) );
	      }
	  }

	++argsi;
	typedef const Lang::Length WidthType;
	Concrete::Length width = Helpers::down_cast_CoreArgument< WidthType >( title_, args, argsi, callLoc )->get( );
	if( width < Concrete::ZERO_LENGTH )
	  {
	    throw Exceptions::CoreOutOfRange( title_, args, argsi, "The width must be non-negative." );
	  }

	++argsi;
	typedef const Lang::Dash DashType;
	RefCountPtr< DashType > dash = Helpers::down_cast_CoreArgument< DashType >( title_, args, argsi, callLoc, true );
  
	++argsi;
	typedef const Lang::Float CloudyType;
	RefCountPtr< CloudyType > cloudyVal = Helpers::down_cast_CoreArgument< CloudyType >( title_, args, argsi, callLoc, true );
	double cloudy = -1;
	if( cloudyVal != NullPtr< CloudyType >( ) )
	  {
	    cloudy = cloudyVal->val_;
	    if( cloudy < 0 || cloudy > 2 )
	      {
		throw Exceptions::CoreOutOfRange( title_, args, argsi, "The cloudyness parameter is recommended to lie in the range [ 0, 2 ]." );
	      }
	  }

	typedef const Lang::XObject AppearanceType;
	++argsi;
	RefCountPtr< AppearanceType > appearanceNormal = Helpers::down_cast_CoreArgument< AppearanceType >( title_, args, argsi, callLoc, true );
	++argsi;
	RefCountPtr< AppearanceType > appearanceRollover = Helpers::down_cast_CoreArgument< AppearanceType >( title_, args, argsi, callLoc, true );
	++argsi;
	RefCountPtr< AppearanceType > appearanceDown = Helpers::down_cast_CoreArgument< AppearanceType >( title_, args, argsi, callLoc, true );

	typedef const Lang::Boolean FlagType;
	size_t flags = 0;
	++argsi;
	if( Helpers::down_cast_CoreArgument< FlagType >( title_, args, argsi, callLoc )->val_ )
	  {
	    flags = flags | Lang::AnnotationSite::INVISIBLE;
	  }
	++argsi;
	if( Helpers::down_cast_CoreArgument< FlagType >( title_, args, argsi, callLoc )->val_ )
	  {
	    flags = flags | Lang::AnnotationSite::HIDDEN;
	  }
	++argsi;
	if( Helpers::down_cast_CoreArgument< FlagType >( title_, args, argsi, callLoc )->val_ )
	  {
	    flags = flags | Lang::AnnotationSite::PRINT;
	  }
	++argsi;
	if( ! Helpers::down_cast_CoreArgument< FlagType >( title_, args, argsi, callLoc )->val_ )
	  {
	    flags = flags | Lang::AnnotationSite::NO_ZOOM;
	  }
	++argsi;
	if( ! Helpers::down_cast_CoreArgument< FlagType >( title_, args, argsi, callLoc )->val_ )
	  {
	    flags = flags | Lang::AnnotationSite::NO_ROTATE;
	  }
	++argsi;
	if( ! Helpers::down_cast_CoreArgument< FlagType >( title_, args, argsi, callLoc )->val_ )
	  {
	    flags = flags | Lang::AnnotationSite::NO_VIEW;
	  }
	++argsi;
	if( ! Helpers::down_cast_CoreArgument< FlagType >( title_, args, argsi, callLoc )->val_ )
	  {
	    flags = flags | Lang::AnnotationSite::READ_ONLY;
	  }
	++argsi;
	if( Helpers::down_cast_CoreArgument< FlagType >( title_, args, argsi, callLoc )->val_ )
	  {
	    flags = flags | Lang::AnnotationSite::LOCKED;
	  }
	++argsi;
	if( Helpers::down_cast_CoreArgument< FlagType >( title_, args, argsi, callLoc )->val_ )
	  {
	    flags = flags | Lang::AnnotationSite::TOGGLE_NO_VIEW;
	  }

	Kernel::ContRef cont = evalState->cont_;
	cont->takeValue( RefCountPtr< const Lang::Value >
			 ( new Lang::AnnotationSite( target, contentText, identifier, flags,
						     borderStyle, width, dash, cloudy, color,
						     appearanceNormal, appearanceRollover, appearanceDown ) ),
			 evalState );
      }
    };

    class Core_annotation_text : public Lang::CoreFunction
    {
    public:
      Core_annotation_text( const char * title )
	: CoreFunction( title, new Kernel::EvaluatedFormals( title, true ) )
      {
	/* General arguments.  Only <target> is required.
	 */
	formals_->appendEvaluatedCoreFormal( "site", Kernel::THE_SLOT_VARIABLE );
	formals_->appendEvaluatedCoreFormal( "title", Kernel::THE_VOID_VARIABLE );
	formals_->appendEvaluatedCoreFormal( "open", Kernel::THE_FALSE_VARIABLE );
	formals_->appendEvaluatedCoreFormal( "verifyiconname", Kernel::THE_TRUE_VARIABLE );
	formals_->appendEvaluatedCoreFormal( "icon", Kernel::THE_VOID_VARIABLE );
      }
      virtual void
      call( Kernel::EvalState * evalState, Kernel::Arguments & args, const Ast::SourceLocation & callLoc ) const
      {
	args.applyDefaults( );

	size_t argsi = 0;
	typedef const Lang::AnnotationSite SiteType;
	RefCountPtr< SiteType > site = Helpers::down_cast_CoreArgument< SiteType >( title_, args, argsi, callLoc );

	++argsi;
	typedef const Lang::String TitleType;
	RefCountPtr< TitleType > titleVal = Helpers::down_cast_CoreArgument< TitleType >( title_, args, argsi, callLoc, true );
	RefCountPtr< const char > title = RefCountPtr< const char >( NullPtr< const char >( ) );
	if( titleVal != NullPtr< TitleType >( ) )
	  {
	    title = titleVal->val_;
	  }

	++argsi;
	typedef const Lang::Boolean InitiallyOpenType;
	bool open = Helpers::down_cast_CoreArgument< InitiallyOpenType >( title_, args, argsi, callLoc )->val_;

	++argsi;
	typedef const Lang::Boolean VerifyType;
	bool verifyIconName = Helpers::down_cast_CoreArgument< VerifyType >( title_, args, argsi, callLoc )->val_;

	++argsi;
	typedef const Lang::String IconType;
	RefCountPtr< IconType > iconVal = Helpers::down_cast_CoreArgument< IconType >( title_, args, argsi, callLoc, true );
	RefCountPtr< const char > icon = RefCountPtr< const char >( NullPtr< const char >( ) );
	if( iconVal != NullPtr< IconType >( ) )
	  {
	    icon = iconVal->val_;
	    if( verifyIconName )
	      {
		const char * ptr = icon.getPtr( );
		if( strcmp( ptr, "Comment" ) == 0 )
		  {
		    // OK.
		  }
		else if( strcmp( ptr, "Help" ) == 0 )
		  {
		    // OK.
		  }
		else if( strcmp( ptr, "Note" ) == 0 )
		  {
		    // OK.
		  }
		else if( strcmp( ptr, "Key" ) == 0 )
		  {
		    // OK.
		  }
		else if( strcmp( ptr, "Insert" ) == 0 )
		  {
		    // OK.
		  }
		else if( strcmp( ptr, "Paragraph" ) == 0 )
		  {
		    // OK.
		  }
		else if( strcmp( ptr, "NewParagraph" ) == 0 )
		  {
		    // OK.
		  }
		else
		  {
		    throw Exceptions::CoreOutOfRange( title_, args, argsi, "The icon name is not one of the standard ones.  Consider passing false for the <verifyiconname> argument." );
		  }
	      }
	  }

	Kernel::ContRef cont = evalState->cont_;
	RefCountPtr< const Lang::AnnotationBase >
	  taggedObj( new Lang::TextAnnotation( site, title, open, icon ) );
	cont->takeValue( RefCountPtr< const Lang::Value >
			 ( new Lang::TaggedValue2D( Kernel::THE_ANNOTATION_SYMBOL, taggedObj ) ),
			 evalState );
      }
    };

    class Core_annotation_launch : public Lang::CoreFunction
    {
    public:
      Core_annotation_launch( const char * title )
	: CoreFunction( title, new Kernel::EvaluatedFormals( title, true ) )
      {
	/* General arguments.  Only <target> is required.
	 */
	formals_->appendEvaluatedCoreFormal( "site", Kernel::THE_SLOT_VARIABLE );
	formals_->appendEvaluatedCoreFormal( "file", Kernel::THE_SLOT_VARIABLE );
	formals_->appendEvaluatedCoreFormal( "highlight", Kernel::THE_VOID_VARIABLE );
	formals_->appendEvaluatedCoreFormal( "uri", Kernel::THE_VOID_VARIABLE );
      }
      virtual void
      call( Kernel::EvalState * evalState, Kernel::Arguments & args, const Ast::SourceLocation & callLoc ) const
      {
	args.applyDefaults( );

	size_t argsi = 0;
	typedef const Lang::AnnotationSite SiteType;
	RefCountPtr< SiteType > site = Helpers::down_cast_CoreArgument< SiteType >( title_, args, argsi, callLoc );

	++argsi;
	typedef const Lang::String FilenameType;
	RefCountPtr< const char > filename = Helpers::down_cast_CoreArgument< FilenameType >( title_, args, argsi, callLoc )->val_;

	++argsi;
	char highlight = Helpers::takeHighlightArgument( title_, args, argsi, callLoc );

	++argsi;
	typedef const Lang::Boolean IsURIType;
	RefCountPtr< IsURIType > isURIVal = Helpers::down_cast_CoreArgument< IsURIType >( title_, args, argsi, callLoc, true );
	Lang::LinkAnnotation::Kind kind = Lang::LinkAnnotation::LAUNCH_FILE;
	if( isURIVal != NullPtr< IsURIType >( ) )
	  {
	    if( isURIVal->val_ )
	      {
		kind = Lang::LinkAnnotation::LAUNCH_URI;
	      }
	    else
	      {
		kind = Lang::LinkAnnotation::LAUNCH_FILE; // Just in case the default above is changed in the future.
	      }
	  }
	else
	  {
	    const char * str = filename.getPtr( );
	    const char alt1[] = "http://";
	    const char alt2[] = "https://";
	    const char alt3[] = "ftp://";
	    const char alt4[] = "mailto://";
	    if( strncmp( str, alt1, strlen( alt1 ) ) == 0
		|| strncmp( str, alt2, strlen( alt2 ) ) == 0
		|| strncmp( str, alt3, strlen( alt3 ) ) == 0
		|| strncmp( str, alt4, strlen( alt4 ) ) == 0
		)
	      {
		kind = Lang::LinkAnnotation::LAUNCH_URI;
	      }
	    else
	      {
		kind = Lang::LinkAnnotation::LAUNCH_FILE; // Just in case the default above is changed in the future.
	      }
	  }

	Kernel::ContRef cont = evalState->cont_;
	RefCountPtr< const Lang::AnnotationBase >
	  taggedObj( new Lang::LinkAnnotation( site, callLoc, highlight, filename, kind ) );
	cont->takeValue( RefCountPtr< const Lang::Value >
			 ( new Lang::TaggedValue2D( Kernel::THE_ANNOTATION_SYMBOL, taggedObj ) ),
			 evalState );
      }
    };

    class Core_annotation_link : public Lang::CoreFunction
    {
    public:
      Core_annotation_link( const char * title )
	: CoreFunction( title, new Kernel::EvaluatedFormals( title, true ) )
      {
	/* General arguments.  Only <target> is required.
	 */
	formals_->appendEvaluatedCoreFormal( "site", Kernel::THE_SLOT_VARIABLE );
	formals_->appendEvaluatedCoreFormal( "name", Kernel::THE_SLOT_VARIABLE );
	formals_->appendEvaluatedCoreFormal( "highlight", Kernel::THE_VOID_VARIABLE );
      }
      virtual void
      call( Kernel::EvalState * evalState, Kernel::Arguments & args, const Ast::SourceLocation & callLoc ) const
      {
	args.applyDefaults( );

	size_t argsi = 0;
	typedef const Lang::AnnotationSite SiteType;
	RefCountPtr< SiteType > site = Helpers::down_cast_CoreArgument< SiteType >( title_, args, argsi, callLoc );

	++argsi;
	typedef const Lang::String IdentifierType;
	RefCountPtr< const char > identifier = Helpers::down_cast_CoreArgument< IdentifierType >( title_, args, argsi, callLoc )->val_;

	++argsi;
	char highlight = Helpers::takeHighlightArgument( title_, args, argsi, callLoc );

	Kernel::ContRef cont = evalState->cont_;
	RefCountPtr< const Lang::AnnotationBase >
	  taggedObj( new Lang::LinkAnnotation( site, callLoc, highlight, identifier, Lang::LinkAnnotation::DOC_LINK ) );
	cont->takeValue( RefCountPtr< const Lang::Value >
			 ( new Lang::TaggedValue2D( Kernel::THE_ANNOTATION_SYMBOL, taggedObj ) ),
			 evalState );
      }
    };
  }
}


void
Kernel::registerCore_annotation( Kernel::Environment * env )
{
  env->initDefineCoreFunction( new Lang::Core_annotationsite( "site" ) );
  env->initDefineCoreFunction( new Lang::Core_annotation_text( "annotationText" ) );
  env->initDefineCoreFunction( new Lang::Core_annotation_launch( "annotationLaunch" ) );
  env->initDefineCoreFunction( new Lang::Core_annotation_link( "annotationLink" ) );
}

