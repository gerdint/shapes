/*
 * File:  afmyylex.l
 * ----------------
 * Lex inupt file to generate the yylex method for the Adobe Font Metrics file parser.
 */

%{

/* The text within this first region delimited by %{ and %} is assumed to
 * be C/C++ code and will be copied verbatim to the lex.afm.c file ahead
 * of the definitions of the yylex() function. Add other header file inclusions
 * or C++ variable declarations/prototypes that are needed by your code here.
 */

#include <cmath>

#include "afmscanner.h"
#include "strrefdup.h"

#include <string.h>
#include <iostream>

// These functions must note be called with invalid arguments!
const char * strtoname( char * begin, char ** endp, const char * delim = " \t" );
const char * strtoname( char * begin, char ** endp, const char * delim, size_t delimSize );

%}

 /*
  * The section before the first %% is the Definitions section of the lex
  * input file. Here is where you set options for the scanner, define lex
  * states, and can set up definitions to give names to regular expressions
  * as a simple substitution mechanism that allows for more readable
  * entries in the Rules section later. 
  */

/* The following is not in agreement with the specification, as far as I can see,
 * but it's the smallest change I can think of which should make this program work
 * with DOS files as well as ordiary files.
 */
HorizontalWhiteSpace [ \t\r]*

Integer [+-]?[0-9]+
HexInteger [0-9A-Fa-f]+

Boolean "true"|"false"

Number [+-]?(([0-9]*[.][0-9]+)|([0-9]+))

Name [^ \t\n\r]+
String [^\r\n]*

%option c++
%option noyywrap

%option prefix="afm"
%option yyclass="AfmScanner"

%x Global
%x FinishGlobalLine
%x ExpectEOF
%x Que
%x MetricsSets
%x FontName
%x FullName
%x FamilyName
%x Weight
%x FontBBox
%x Version
%x Notice
%x EncodingScheme
%x CharacterSet
%x Characters
%x IsBaseFont
%x VVector
%x IsFixedV
%x IsCIDFont
%x CapHeight
%x XHeight
%x Ascender
%x Descender
%x StdHW
%x StdVW
%x StartDirection
%x UnderlinePosition
%x UnderlineThickness
%x ItalicAngle
%x CharWidth
%x IsFixedPitch
%x StartCharMetrics
%x CharMetrics
%x C
%x CH
%x W0X
%x W1X
%x W0Y
%x W1Y
%x W0
%x W1
%x VV
%x N
%x B
%x L
%x KernData
%x StartKernPairs
%x KernPairs
%x StartTrackKern
%x TrackKern

%%

<INITIAL>"StartFontMetrics ".*[\n] {
  BEGIN( Global );
}

<Global>"EndFontMetrics"{HorizontalWhiteSpace}[\n] {
  if( fontMetricsDst_->horizontalMetrics_ != NullPtr< FontMetrics::WritingDirectionMetrics >( ) )
  {
    // I const cast because I'm too tired to think of something better.
    FontMetrics::CharacterMetrics * defaultChar = const_cast< FontMetrics::CharacterMetrics * >( fontMetricsDst_->horizontalMetrics_->charData_[ 0 ] );
    defaultChar->xmin_ = fontMetricsDst_->fontBBoxXMin_;
    defaultChar->ymin_ = fontMetricsDst_->fontBBoxYMin_;
    defaultChar->xmax_ = fontMetricsDst_->fontBBoxXMax_;
    defaultChar->ymax_ = fontMetricsDst_->fontBBoxYMax_;
    if( fontMetricsDst_->horizontalMetrics_->isFixedPitch_ )
    {
      defaultChar->horizontalCharWidthX_ = fontMetricsDst_->horizontalMetrics_->charWidthX_;
      defaultChar->horizontalCharWidthY_ = fontMetricsDst_->horizontalMetrics_->charWidthY_;
    }
    else
    {
      defaultChar->horizontalCharWidthX_ = defaultChar->xmax_;
      defaultChar->horizontalCharWidthY_ = defaultChar->ymax_;
    }
  }
  if( fontMetricsDst_->verticalMetrics_ != NullPtr< FontMetrics::WritingDirectionMetrics >( ) )
  {
    // I const cast because I'm too tired to thing of something better.
    FontMetrics::CharacterMetrics * defaultChar = const_cast< FontMetrics::CharacterMetrics * >( fontMetricsDst_->verticalMetrics_->charData_[ 0 ] );
    defaultChar->xmin_ = fontMetricsDst_->fontBBoxXMin_;
    defaultChar->ymin_ = fontMetricsDst_->fontBBoxYMin_;
    defaultChar->xmax_ = fontMetricsDst_->fontBBoxXMax_;
    defaultChar->ymax_ = fontMetricsDst_->fontBBoxYMax_;
    if( fontMetricsDst_->verticalMetrics_->isFixedPitch_ )
    {
      defaultChar->verticalCharWidthX_ = fontMetricsDst_->verticalMetrics_->charWidthX_;
      defaultChar->verticalCharWidthY_ = fontMetricsDst_->verticalMetrics_->charWidthY_;
    }
    else
    {
      defaultChar->verticalCharWidthX_ = defaultChar->xmax_;
      defaultChar->verticalCharWidthY_ = defaultChar->ymax_;
    }
  }

  BEGIN( ExpectEOF );
}
<FinishGlobalLine>{HorizontalWhiteSpace}[\n] {
  // Skip to next line.
  BEGIN( Global );
}
<Global>^{HorizontalWhiteSpace} {
  // Ignore leading whitespace.
}

<ExpectEOF>{HorizontalWhiteSpace}[\n] {
  // ignore
}
<ExpectEOF><<EOF>> {
  return 0;
}

<ExpectEOF>.|[\n] {
  throwError( "Expected EOF." );
}

<Global>"MetricsSets"{HorizontalWhiteSpace} {
  BEGIN( MetricsSets );
}
<MetricsSets>{Integer} {
  char * endp;
  long int tmp = strtol( yytext, & endp, 10 );
  if( tmp < 0 || tmp > 2 )
  {
    throwError( "MetricsSets out of range." );
  }
  metricsSets_ = tmp;
  BEGIN( FinishGlobalLine );
}

<Global>"FontName"{HorizontalWhiteSpace} {
  BEGIN( FontName );
}
<FontName>{String} {
  fontMetricsDst_->fontName_ = strrefdup( yytext );
  BEGIN( FinishGlobalLine );
}

<Global>"FullName"{HorizontalWhiteSpace} {
  BEGIN( FullName );
}
<FullName>{String} {
  fontMetricsDst_->fullName_ = strrefdup( yytext );
  BEGIN( FinishGlobalLine );
}

<Global>"FamilyName"{HorizontalWhiteSpace} {
  BEGIN( FamilyName );
}
<FamilyName>{String} {
  fontMetricsDst_->familyName_ = strrefdup( yytext );
  BEGIN( FinishGlobalLine );
}

<Global>"Weight"{HorizontalWhiteSpace} {
  BEGIN( Weight );
}
<Weight>{String} {
  fontMetricsDst_->weight_ = strrefdup( yytext );
  BEGIN( FinishGlobalLine );
}

<Global>"FontBBox"{HorizontalWhiteSpace} {
  BEGIN( FontBBox );
}
<FontBBox>{Number}{HorizontalWhiteSpace}{Number}{HorizontalWhiteSpace}{Number}{HorizontalWhiteSpace}{Number} {
  char * src = yytext;
  char * endp;
  fontMetricsDst_->fontBBoxXMin_ = 0.001 * strtod( src, & endp );
  src = endp;
  fontMetricsDst_->fontBBoxYMin_ = 0.001 * strtod( src, & endp );
  src = endp;
  fontMetricsDst_->fontBBoxXMax_ = 0.001 * strtod( src, & endp );
  src = endp;
  fontMetricsDst_->fontBBoxYMax_ = 0.001 * strtod( src, & endp );
  BEGIN( FinishGlobalLine );
}

<Global>"Version"{HorizontalWhiteSpace} {
  BEGIN( Version );
}
<Version>{String} {
  fontMetricsDst_->version_ = strrefdup( yytext );
  BEGIN( FinishGlobalLine );
}

<Global>"Notice"{HorizontalWhiteSpace} {
  BEGIN( Notice );
}
<Notice>{String} {
  fontMetricsDst_->notice_ = strrefdup( yytext );
  BEGIN( FinishGlobalLine );
}

<Global>"EncodingScheme"{HorizontalWhiteSpace} {
  BEGIN( EncodingScheme );
}
<EncodingScheme>{String} {
  fontMetricsDst_->encodingScheme_ = strrefdup( yytext );
  BEGIN( FinishGlobalLine );
}

<Global>"CharacterSet"{HorizontalWhiteSpace} {
  BEGIN( CharacterSet );
}
<CharacterSet>{String} {
  fontMetricsDst_->characterSet_ = strrefdup( yytext );
  BEGIN( FinishGlobalLine );
}

<Global>"Characters"{HorizontalWhiteSpace} {
  BEGIN( Characters );
}
<Characters>{Integer} {
  char * endp;
  fontMetricsDst_->charCount_ = strtol( yytext, & endp, 10 );
  BEGIN( FinishGlobalLine );
}

<Global>"IsBaseFont"{HorizontalWhiteSpace} {
  BEGIN( IsBaseFont );
}
<IsBaseFont>{Boolean} {
  if( yytext[ 0 ] != 't' )
  {
    throwError( "Expected \"IsBaseFont true\"." );
  }
  BEGIN( FinishGlobalLine );
}

<Global>"VVector"{HorizontalWhiteSpace} {
  BEGIN( VVector );
}
<FontBBox>{Number}{HorizontalWhiteSpace}{Number} {
  char * src = yytext;
  char * endp;
  fontMetricsDst_->vVectorX_ = 0.001 * strtod( src, & endp );
  src = endp;
  fontMetricsDst_->vVectorY_ = 0.001 * strtod( src, & endp );
  fontMetricsDst_->isFixedV_ = true;
  BEGIN( FinishGlobalLine );
}

<Global>"IsFixedV"{HorizontalWhiteSpace} {
  BEGIN( IsFixedV );
}
<IsFixedV>{Boolean} {
  fontMetricsDst_->isFixedV_ = yytext[0] == 't';
  BEGIN( FinishGlobalLine );
}

<Global>"IsCIDFont"{HorizontalWhiteSpace} {
  BEGIN( IsCIDFont );
}
<IsCIDFont>{Boolean} {
  fontMetricsDst_->isCIDFont_ = yytext[0] == 't';
  BEGIN( FinishGlobalLine );
}

<Global>"CapHeight"{HorizontalWhiteSpace} {
  BEGIN( CapHeight );
}
<CapHeight>{Number} {
  char * endp;
  fontMetricsDst_->capHeight_ = 0.001 * strtod( yytext, & endp );
  BEGIN( FinishGlobalLine );
}

<Global>"XHeight"{HorizontalWhiteSpace} {
  BEGIN( XHeight );
}
<XHeight>{Number} {
  char * endp;
  fontMetricsDst_->xHeight_ = 0.001 * strtod( yytext, & endp );
  BEGIN( FinishGlobalLine );
}

<Global>"Ascender"{HorizontalWhiteSpace} {
  BEGIN( Ascender );
}
<Ascender>{Number} {
  char * endp;
  fontMetricsDst_->ascender_ = 0.001 * strtod( yytext, & endp );
  BEGIN( FinishGlobalLine );
}

<Global>"Descender"{HorizontalWhiteSpace} {
  BEGIN( Descender );
}
<Descender>{Number} {
  char * endp;
  fontMetricsDst_->descender_ = 0.001 * strtod( yytext, & endp );
  BEGIN( FinishGlobalLine );
}

<Global>"StdHW"{HorizontalWhiteSpace} {
  BEGIN( StdHW );
}
<StdHW>{Number} {
  char * endp;
  fontMetricsDst_->stdHW_ = 0.001 * strtod( yytext, & endp );
  BEGIN( FinishGlobalLine );
}

<Global>"StdVW"{HorizontalWhiteSpace} {
  BEGIN( StdVW );
}
<StdVW>{Number} {
  char * endp;
  fontMetricsDst_->stdVW_ = 0.001 * strtod( yytext, & endp );
  BEGIN( FinishGlobalLine );
}


<Global>"StartDirection"{HorizontalWhiteSpace} {
  BEGIN( StartDirection );
}
<StartDirection>{Integer} {
  char * endp;
  long int tmp = strtol( yytext, & endp, 10 );
  if( tmp < 0 || tmp > 2 )
  {
    throwError( "StartDirection out of range." );
  }
  activateDirectionID_ = tmp;
  BEGIN( FinishGlobalLine );
}

<Global>"EndDirection"{HorizontalWhiteSpace} {
  activateDirectionID_ = 0; // This is probably not necessary.
  BEGIN( FinishGlobalLine );
}

<Global>"UnderlinePosition"{HorizontalWhiteSpace} {
  BEGIN( UnderlinePosition );
}
<UnderlinePosition>{Number} {
  synchWritingDirection( );
  char * endp;
  currentDirectionDst_->underlinePosition_ = 0.001 * strtod( yytext, & endp );
  BEGIN( FinishGlobalLine );
}

<Global>"UnderlineThickness"{HorizontalWhiteSpace} {
  BEGIN( UnderlineThickness );
}
<UnderlineThickness>{Number} {
  synchWritingDirection( );
  char * endp;
  currentDirectionDst_->underlineThickness_ = 0.001 * strtod( yytext, & endp );
  BEGIN( FinishGlobalLine );
}

<Global>"ItalicAngle"{HorizontalWhiteSpace} {
  BEGIN( ItalicAngle );
}
<ItalicAngle>{Number} {
  synchWritingDirection( );
  char * endp;
  currentDirectionDst_->italicAngleRadians_ = strtod( yytext, & endp ) * ( M_PI / 180 );  // Note: Don't scale by 0.001!
  BEGIN( FinishGlobalLine );
}

<Global>"CharWidth"{HorizontalWhiteSpace} {
  BEGIN( CharWidth );
}
<CharWidth>{Number}{HorizontalWhiteSpace}{Number} {
  synchWritingDirection( );
  char * src = yytext;
  char * endp;
  currentDirectionDst_->charWidthX_ = 0.001 * strtod( yytext, & endp );
  src = endp;
  currentDirectionDst_->charWidthY_ = 0.001 * strtod( yytext, & endp );
  BEGIN( FinishGlobalLine );
}

<Global>"IsFixedPitch"{HorizontalWhiteSpace} {
  BEGIN( IsFixedPitch );
}
<IsFixedPitch>{Boolean} {
  synchWritingDirection( );
  currentDirectionDst_->isFixedPitch_ = yytext[0] == 't';
  BEGIN( FinishGlobalLine );
}

<Global>"StartCharMetrics"{HorizontalWhiteSpace} {
  BEGIN( StartCharMetrics );
}
<StartCharMetrics>{Integer}{HorizontalWhiteSpace}[\n] {
  synchWritingDirection( );
  if( currentDirectionDst_->charData_.size( ) > 1 )  // the first entry is the default values
  {
    throwError( "Multiply specified character metrics in this writing direction." );
  }
  char * endp;
  long int tmp = strtol( yytext, & endp, 10 );
  currentDirectionDst_->charData_.reserve( tmp + 1 ); // the first entry is the default values
  currentCharacter_ = new FontMetrics::CharacterMetrics( currentDirectionDst_->charData_.size( ) );
  BEGIN( CharMetrics );
}

<CharMetrics>"EndCharMetrics" {
  delete currentCharacter_;
  BEGIN( FinishGlobalLine );
}

<CharMetrics>[\r] {
  // Ignore DOS crap.
}
<CharMetrics>[\n] {
  currentDirectionDst_->charData_.push_back( currentCharacter_ );
  currentCharacter_ = new FontMetrics::CharacterMetrics( currentDirectionDst_->charData_.size( ) );
}

<CharMetrics>^{HorizontalWhiteSpace}[\n] {
  // A newline at the end of an empty line does not insert the current character.
}
<CharMetrics>{HorizontalWhiteSpace} {
  // Ingore
}

<CharMetrics>"C"{HorizontalWhiteSpace} {
  BEGIN( C );
}
<C>{Integer}{HorizontalWhiteSpace}[;] {
  char * endp;
  currentCharacter_->characterCode_ = strtol( yytext, & endp, 10 );
  if( currentCharacter_->characterCode_ >= 0 )
  {
    currentDirectionDst_->codeMap_[ currentCharacter_->characterCode_ ] = currentCharacter_->internalPosition_;
  }
  BEGIN( CharMetrics );
}

<CharMetrics>"CH"{HorizontalWhiteSpace} {
  BEGIN( CH );
}
<CH>{HexInteger}{HorizontalWhiteSpace}[;] {
  char * endp;
  currentCharacter_->characterCode_ = strtol( yytext, & endp, 16 );
  if( currentCharacter_->characterCode_ >= 0 )
  {
    currentDirectionDst_->codeMap_[ currentCharacter_->characterCode_ ] = currentCharacter_->internalPosition_;
  }
  BEGIN( CharMetrics );
}

<CharMetrics>("WX"|"W0X"){HorizontalWhiteSpace} {
  BEGIN( W0X );
}
<W0X>{Number}{HorizontalWhiteSpace}[;] {
  char * endp;
  currentCharacter_->horizontalCharWidthX_ = 0.001 * strtod( yytext, & endp );
  BEGIN( CharMetrics );
}

<CharMetrics>("WY"|"W0Y"){HorizontalWhiteSpace} {
  BEGIN( W0Y );
}
<W0Y>{Number}{HorizontalWhiteSpace}[;] {
  char * endp;
  currentCharacter_->horizontalCharWidthY_ = 0.001 * strtod( yytext, & endp );
  BEGIN( CharMetrics );
}

<CharMetrics>"W1X"{HorizontalWhiteSpace} {
  BEGIN( W1X );
}
<W1X>{Number}{HorizontalWhiteSpace}[;] {
  char * endp;
  currentCharacter_->verticalCharWidthX_ = 0.001 * strtod( yytext, & endp );
  BEGIN( CharMetrics );
}

<CharMetrics>"W1Y"{HorizontalWhiteSpace} {
  BEGIN( W1Y );
}
<W1Y>{Number}{HorizontalWhiteSpace}[;] {
  char * endp;
  currentCharacter_->verticalCharWidthY_ = 0.001 * strtod( yytext, & endp );
  BEGIN( CharMetrics );
}

<CharMetrics>"W0"{HorizontalWhiteSpace} {
  BEGIN( W0 );
}
<W0>{Number}{HorizontalWhiteSpace}{Number}{HorizontalWhiteSpace}[;] {
  char * src = yytext;
  char * endp;
  currentCharacter_->horizontalCharWidthX_ = 0.001 * strtod( src, & endp );
  src = endp;
  currentCharacter_->horizontalCharWidthY_ = 0.001 * strtod( src, & endp );
  BEGIN( CharMetrics );
}

<CharMetrics>"W1"{HorizontalWhiteSpace} {
  BEGIN( W1 );
}
<W1>{Number}{HorizontalWhiteSpace}{Number}{HorizontalWhiteSpace}[;] {
  char * src = yytext;
  char * endp;
  currentCharacter_->verticalCharWidthX_ = 0.001 * strtod( src, & endp );
  src = endp;
  currentCharacter_->verticalCharWidthY_ = 0.001 * strtod( src, & endp );
  BEGIN( CharMetrics );
}

<CharMetrics>"VV"{HorizontalWhiteSpace} {
  BEGIN( VV );
}
<VV>{Number}{HorizontalWhiteSpace}{Number}{HorizontalWhiteSpace}[;] {
  char * src = yytext;
  char * endp;
  currentCharacter_->vX_ = 0.001 * strtod( src, & endp );
  src = endp;
  currentCharacter_->vY_ = 0.001 * strtod( src, & endp );
  BEGIN( CharMetrics );
}

<CharMetrics>"N"{HorizontalWhiteSpace} {
  BEGIN( N );
}
<N>{Name}{HorizontalWhiteSpace}[;] {
  char * end = yytext;
  for( ; *end != ' ' && *end != '\t' && *end != ';'; ++end )
    ;
  *end = '\0';
  currentDirectionDst_->nameMap_[ strrefdup( yytext ) ] = currentCharacter_->internalPosition_;
  if( currentCharacter_->characterCode_ < 0 )
    {
      unsigned char pos;
      if( encoding_->name_to_position( yytext, & pos ) )
	{
	  currentCharacter_->characterCode_ = pos;
	  currentDirectionDst_->codeMap_[ currentCharacter_->characterCode_ ] = currentCharacter_->internalPosition_;
	}
    }

  BEGIN( CharMetrics );
}

<CharMetrics>"B"{HorizontalWhiteSpace} {
  BEGIN( B );
}
<B>{Number}{HorizontalWhiteSpace}{Number}{HorizontalWhiteSpace}{Number}{HorizontalWhiteSpace}{Number}{HorizontalWhiteSpace}[;] {
  char * src = yytext;
  char * endp;
  currentCharacter_->xmin_ = 0.001 * strtod( src, & endp );
  src = endp;
  currentCharacter_->ymin_ = 0.001 * strtod( src, & endp );
  src = endp;
  currentCharacter_->xmax_ = 0.001 * strtod( src, & endp );
  src = endp;
  currentCharacter_->ymax_ = 0.001 * strtod( src, & endp );
  BEGIN( CharMetrics );
}

<CharMetrics>"L"{HorizontalWhiteSpace} {
  BEGIN( L );
}
<L>{Name}{HorizontalWhiteSpace}{Name}{HorizontalWhiteSpace}[;] {
  char * end1;
  const char * begin1 = strtoname( yytext, & end1, " \t", 2 );
  char * end2;
  const char * begin2 = strtoname( end1, & end2, " ;\t", 3 );

  currentCharacter_->addLigature( strrefdup( begin1 ), strrefdup( begin2 ) );
  BEGIN( CharMetrics );
}

<Global>"StartKernData"{HorizontalWhiteSpace}[\n] {
  BEGIN( KernData );
}
<KernData>"EndKernData"{HorizontalWhiteSpace} {
  BEGIN( FinishGlobalLine );
}
<KernData>"StartKernPairs"([0]?){HorizontalWhiteSpace} {
  currentKernPairMapX_ = & fontMetricsDst_->horizontalKernPairsX_;
  currentKernPairMapY_ = & fontMetricsDst_->horizontalKernPairsY_;
  if( fontMetricsDst_->horizontalMetrics_ == NullPtr< FontMetrics::WritingDirectionMetrics >( ) )
    {
      throw "It would be nice if the character metrics appeared before the kern pairs.";
    }
  currentNameMap_ = & fontMetricsDst_->horizontalMetrics_->nameMap_;
  BEGIN( StartKernPairs );
}
<KernData>"StartKernPairs1"{HorizontalWhiteSpace} {
  currentKernPairMapX_ = & fontMetricsDst_->verticalKernPairsX_;
  currentKernPairMapY_ = & fontMetricsDst_->verticalKernPairsY_;
  if( fontMetricsDst_->verticalMetrics_ == NullPtr< FontMetrics::WritingDirectionMetrics >( ) )
    {
      throw "It would be nice if the character metrics appeared before the kern pairs.";
    }
  currentNameMap_ = & fontMetricsDst_->verticalMetrics_->nameMap_;
  BEGIN( StartKernPairs );
}
<StartKernPairs>{Integer}{HorizontalWhiteSpace}[\n] {
  // The number of pairs is not used.
  BEGIN( KernPairs );
}
<KernPairs>"EndKernPairs"{HorizontalWhiteSpace}[\n] {
  BEGIN( KernData );
}
<KernData>"StartTrackKern"{HorizontalWhiteSpace} {
  BEGIN( StartTrackKern );
}
<StartTrackKern>{Integer}{HorizontalWhiteSpace}[\n] {
  // The number of entries is not used.
  BEGIN( TrackKern );
}
<TrackKern>"EndTrackKern"{HorizontalWhiteSpace}[\n] {
  BEGIN( KernData );
}

<KernPairs>"KP"{HorizontalWhiteSpace}{Name}{HorizontalWhiteSpace}{Name}{HorizontalWhiteSpace}{Number}{HorizontalWhiteSpace}{Number}{HorizontalWhiteSpace}[\n] {
  char * src = yytext + 2;
  char * end;
  const char * name1 = strtoname( src, & end, " \t", 2 );
  src = end;
  const char * name2 = strtoname( src, & end, " \t", 2 );
  src = end;
  double x = 0.001 * strtod( src, & end );
  src = end;
  double y = 0.001 * strtod( src, & end );
  typedef typeof *currentNameMap_ NameMapType;
  NameMapType::const_iterator i1 = currentNameMap_->find( strrefdup( name1 ) );
  if( i1 == currentNameMap_->end( ) )
  {
    throw "Unable to resolve character name in kern pair.  Perhaps kern pairs appear before character metrics.";
  }
  NameMapType::const_iterator i2 = currentNameMap_->find( strrefdup( name2 ) );
  if( i2 == currentNameMap_->end( ) )
  {
    throw "Unable to resolve character name in kern pair.  Perhaps kern pairs appear before character metrics.";
  }
  (*currentKernPairMapX_)[ std::pair< size_t, size_t >( i1->second, i2->second ) ] = x;
  (*currentKernPairMapY_)[ std::pair< size_t, size_t >( i1->second, i2->second ) ] = y;
}

<KernPairs>"KPH"{HorizontalWhiteSpace}"<"{Name}">"{HorizontalWhiteSpace}"<"{Name}">"{HorizontalWhiteSpace}{Number}{HorizontalWhiteSpace}{Number}{HorizontalWhiteSpace}[\n] {
//  char * src = yytext + 3;
//  char * end;
  throw "Kern pairs with hexadecimal names are not understood.";
}

<KernPairs>"KPX"{HorizontalWhiteSpace}{Name}{HorizontalWhiteSpace}{Name}{HorizontalWhiteSpace}{Number}{HorizontalWhiteSpace}[\n] {
  char * src = yytext + 3;
  char * end;
  const char * name1 = strtoname( src, & end, " \t", 2 );
  src = end;
  const char * name2 = strtoname( src, & end, " \t", 2 );
  src = end;
  double x = 0.001 * strtod( src, & end );
  typedef typeof *currentNameMap_ NameMapType;
  NameMapType::const_iterator i1 = currentNameMap_->find( strrefdup( name1 ) );
  if( i1 == currentNameMap_->end( ) )
  {
    throw "Unable to resolve character name in kern pair.  Perhaps kern pairs appear before character metrics.";
  }
  NameMapType::const_iterator i2 = currentNameMap_->find( strrefdup( name2 ) );
  if( i2 == currentNameMap_->end( ) )
  {
    throw "Unable to resolve character name in kern pair.  Perhaps kern pairs appear before character metrics.";
  }
  (*currentKernPairMapX_)[ std::pair< size_t, size_t >( i1->second, i2->second ) ] = x;
}

<KernPairs>"KPY"{HorizontalWhiteSpace}{Name}{HorizontalWhiteSpace}{Name}{HorizontalWhiteSpace}{Number}{HorizontalWhiteSpace}[\n] {
  char * src = yytext + 3;
  char * end;
  const char * name1 = strtoname( src, & end, " \t", 2 );
  src = end;
  const char * name2 = strtoname( src, & end, " \t", 2 );
  src = end;
  double y = 0.001 * strtod( src, & end );
  typedef typeof *currentNameMap_ NameMapType;
  NameMapType::const_iterator i1 = currentNameMap_->find( strrefdup( name1 ) );
  if( i1 == currentNameMap_->end( ) )
  {
    throw "Unable to resolve character name in kern pair.  Perhaps kern pairs appear before character metrics.";
  }
  NameMapType::const_iterator i2 = currentNameMap_->find( strrefdup( name2 ) );
  if( i2 == currentNameMap_->end( ) )
  {
    throw "Unable to resolve character name in kern pair.  Perhaps kern pairs appear before character metrics.";
  }
  (*currentKernPairMapY_)[ std::pair< size_t, size_t >( i1->second, i2->second ) ] = y;
}


<TrackKern>{HorizontalWhiteSpace}{Integer}{HorizontalWhiteSpace}{Number}{HorizontalWhiteSpace}{Number}{HorizontalWhiteSpace}{Number}{HorizontalWhiteSpace}{Number}{HorizontalWhiteSpace}[\n] {
  char * src = yytext;
  char * endp;
  int degree = strtol( src, & endp, 10 );
  src = endp;
  double sizeLow = strtod( src, & endp );
  src = endp;
  double trackLow = 0.001 * strtod( src, & endp );
  src = endp;
  double sizeHigh = strtod( src, & endp );
  src = endp;
  double trackHigh = 0.001 * strtod( src, & endp );
  typedef typeof fontMetricsDst_->trackKernings_ MapType;
  fontMetricsDst_->trackKernings_.insert( MapType::value_type( degree, RefCountPtr< FontMetrics::TrackKerning >( new FontMetrics::TrackKerning( sizeLow, trackLow, sizeHigh, trackHigh ) ) ) );
}


<*>^"Comment"{HorizontalWhiteSpace}.*[\n] {
  // Ignore comments.
}

<Global>{Name} {
  // If we match the whole line here, it will be a _very_ good match for anything.  Hence we do things in two steps.
  if( tellQue_ )
  {
    std::cerr << "The afm parser was unable to understand the key \"" << yytext << "\", with data: " ;
  }
  BEGIN( Que ); 
}
<Que>{String} {
  // Ignore things we don't understand.
  if( tellQue_ )
  {
    std::cerr << yytext << std::endl ;
  }
  BEGIN( FinishGlobalLine );  
}

<*>. { throwError( "Unrecognized token." ); }

%%
/* The closing %% above marks the end of the Rules section and the beginning
 * of the User Subroutines section. All text from here to the end of the
 * file is copied verbatim to the end of the generated lex.pdf.c file.
 * This section is where you put definitions of helper functions.
 */

const char *
strtoname( char * begin, char ** endp, const char * delim )
{
  for( ; memchr( " \t", *begin, 2 ) != 0; ++begin )
    ;

  char * end = begin;
  for( ; strchr( delim, *end ) == 0; ++end )
    ;
  
  *end = '\0';
  *endp = end + 1;
  return begin;
}

const char *
strtoname( char * begin, char ** endp, const char * delim, size_t delimSize )
{
  for( ; memchr( " \t", *begin, 2 ) != 0; ++begin )
    ;

  char * end = begin;
  for( ; memchr( delim, *end, delimSize ) == 0; ++end )
    ;
  
  *end = '\0';
  *endp = end + 1;
  return begin;
}

