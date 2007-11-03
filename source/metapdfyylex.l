/*
 * File:  metapdfyylex.l
 * ----------------
 * Lex inupt file to generate the yylex method for the scanning of the MetaPDF language.
 */

%{

/* The text within this first region delimited by %{ and %} is assumed to
 * be C/C++ code and will be copied verbatim to the lex.pdf.c file ahead
 * of the definitions of the pdflex() function. Add other header file inclusions
 * or C++ variable declarations/prototypes that are needed by your code here.
 */

#include <cmath>

#include "metapdfscanner.h"
#include "metapdfvalue.h"
#include "metapdftypes.h"
#include "metapdfastflow.h"
#include "metapdfastvar.h"
#include "metapdfastclass.h"
#include "metapdfexceptions.h"
#include "texlabelmanager.h"
#include "globals.h"

using namespace MetaPDF;
#include "yyltype.h"
#include "metapdfparser.tab.h"

#include <string>
#include <cstdlib>
#include <iostream>
#include <fstream>
#include <iomanip>

#define YY_USER_ACTION doBeforeEachAction( );

double metaPDFstrtod( const char * str, char ** end );

%}

 /*
  * The section before the first %% is the Definitions section of the lex
  * input file. Here is where you set options for the scanner, define lex
  * states, and can set up definitions to give names to regular expressions
  * as a simple substitution mechanism that allows for more readable
  * entries in the Rules section later. 
  */

WhiteSpace [ \t]

Float [~]?[0-9]+([.][0-9]*)?
Greek "α"|"β"|"γ"|"Γ"|"δ"|"Δ"|"ε"|"ζ"|"η"|"Θ"|"ι"|"κ"|"λ"|"Λ"|"μ"|"ν"|"χ"|"Ξ"|"π"|"Π"|"ρ"|"σ"|"Σ"|"τ"|"ϕ"|"ω"|"Ω"
LowerCaseLetter [a-z_?]
UpperCaseLetter [A-Z]
Letter {LowerCaseLetter}|{UpperCaseLetter}|{Greek}
Identifier {Letter}({Letter}|[0-9])*
DynamicMark "@"
StateMark "#"|"•"
TypeMark "//"|"§"

/*
  At the moment, escape characters must occypy exactly 2 bytes.
*/
Escape "¢"|"¤"


%option c++
%option noyywrap
%option yylineno

%option yyclass="MetaPDFScanner"

%x DiscardRestOfLineAndBEGIN_INITIAL
%x Incl
%x Needs
%x InclFrom
%x InclPath
%x String
%x Comment
%x LaTeXOption
%x LaTeXClass
%x LaTeXPreamble
%x LaTeXDocumentTop
%x RandSeed
%x NewUnitName
%x NewUnitEqual
%x NewUnitValue
%x Echo
%x Author


%%

^"##classoption"[ \t]+ { BEGIN( LaTeXOption ); }
^"##documentclass"[ \t]+ { BEGIN( LaTeXClass ); }
^"##preamble"[ \t] { BEGIN( LaTeXPreamble ); }
^"##documenttop"[ \t] { BEGIN( LaTeXDocumentTop ); }
^"##no-lmodernT1"[\n] { Kernel::theTeXLabelManager.setlmodernT1( metapdflloc, false ); }
^"##no-utf8"[\n] { Kernel::theTeXLabelManager.setutf8( metapdflloc, false ); }
^"##seed"[ \t]+ { BEGIN( RandSeed ); }
^"##unit"[ \t]+ { BEGIN( NewUnitName ); }
^"##include"[ \t]+ { BEGIN( Incl ); }
^"##needs"[ \t]+ { BEGIN( Needs ); }
^"##echo"[ \t] { BEGIN( Echo ); }
^"##author"[ \t] { BEGIN( Author ); }
^"##" {
  Ast::theAnalysisErrorsList.push_back( new Exceptions::ScannerError( metapdflloc, strrefdup( "All lines beginning with ## must be scanner specials.  Please use a leading horizontal whitespace if this is not what is intended." ) ) );
}

<DiscardRestOfLineAndBEGIN_INITIAL>.* {
  BEGIN( INITIAL );
}

<LaTeXOption>[^,\n]+ { Kernel::theTeXLabelManager.addDocumentOption( metapdflloc, yytext  ); }
<LaTeXOption>[\n] {
  metapdflloc.firstLine = metapdflloc.lastLine + 1;
  metapdflloc.lastLine = metapdflloc.firstLine;
  metapdflloc.lastColumn = 0;
  BEGIN( INITIAL );
}
  
<LaTeXClass>.+ { Kernel::theTeXLabelManager.setDocumentClass( metapdflloc, yytext ); }
<LaTeXClass>[\n] {
  metapdflloc.firstLine = metapdflloc.lastLine + 1;
  metapdflloc.lastLine = metapdflloc.firstLine;
  metapdflloc.lastColumn = 0;
  BEGIN( INITIAL );
}
  
<LaTeXPreamble>.* { Kernel::theTeXLabelManager.addPreambleLine( metapdflloc, yytext ); }
<LaTeXPreamble>[\n] {
  metapdflloc.firstLine = metapdflloc.lastLine + 1;
  metapdflloc.lastLine = metapdflloc.firstLine;
  metapdflloc.lastColumn = 0;
  BEGIN( INITIAL );
}

<LaTeXDocumentTop>.* { Kernel::theTeXLabelManager.addDocumentTopLine( metapdflloc, yytext );  }
<LaTeXDocumentTop>[\n] {
  metapdflloc.firstLine = metapdflloc.lastLine + 1;
  metapdflloc.lastLine = metapdflloc.firstLine;
  metapdflloc.lastColumn = 0;
  BEGIN( INITIAL );
}

<RandSeed>.* {
  char * end;
  long int s = strtol( yytext, & end, 10 );
  if( *end != '\0' )
    {
      Ast::theAnalysisErrorsList.push_back( new Exceptions::ScannerError( metapdflloc, strrefdup( "Failed to scan integer." ) ) );
    }
  else
    {
      if( s < 0 )
	{
	  Ast::theAnalysisErrorsList.push_back( new Exceptions::ScannerError( metapdflloc, strrefdup( "Random nuber generator seed must not be negative." ) ) );
	}
      else
	{
	  srand( s );
	}
    }
}
<RandSeed>[\n] {
  metapdflloc.firstLine = metapdflloc.lastLine + 1;
  metapdflloc.lastLine = metapdflloc.firstLine;
  metapdflloc.lastColumn = 0;
  BEGIN( INITIAL );
}

<NewUnitName>{Identifier} {
  newUnitName = strdup( yytext );
  BEGIN( NewUnitEqual );
}
<NewUnitName>. {
  Ast::theAnalysisErrorsList.push_back( new Exceptions::ScannerError( metapdflloc, strrefdup( "Expected identifier." ) ) );
  BEGIN( DiscardRestOfLineAndBEGIN_INITIAL );
}
<NewUnitEqual>[ \t]*"="[ \t]* { BEGIN( NewUnitValue ); }
<NewUnitEqual>. {
  Ast::theAnalysisErrorsList.push_back( new Exceptions::ScannerError( metapdflloc, strrefdup( "Expected '='." ) ) );
  BEGIN( DiscardRestOfLineAndBEGIN_INITIAL );
 }
<NewUnitValue>{Float}"bp" {
  char * end;
  double newUnitValue = metaPDFstrtod( yytext, & end );
  typedef typeof unitTable MapType;
  MapType::const_iterator i = unitTable.find( newUnitName );
  if( i != unitTable.end( ) )
    {
      if( i->second != newUnitValue )
	{
	  Ast::theAnalysisErrorsList.push_back( new Exceptions::IntroducingExistingUnit( metapdflloc, strrefdup( newUnitName ) ) );
	}
    }
  else
    {
      unitTable[ newUnitName ] = newUnitValue;
    }
  BEGIN( INITIAL );
}
<NewUnitValue>{Float}"mm" {
  char * end;
  double newUnitValue = metaPDFstrtod( yytext, & end ) * ( 0.1 * 72 / 2.54 );
  typedef typeof unitTable MapType;
  MapType::const_iterator i = unitTable.find( newUnitName );
  if( i != unitTable.end( ) )
    {
      if( i->second != newUnitValue )
	{
	  Ast::theAnalysisErrorsList.push_back( new Exceptions::IntroducingExistingUnit( metapdflloc, strrefdup( newUnitName ) ) );
	}
    }
  else
    {
      unitTable[ newUnitName ] = newUnitValue;
    }
  BEGIN( INITIAL );
}
<NewUnitValue>{Float}"cm" {
  char * end;
  double newUnitValue = metaPDFstrtod( yytext, & end ) * ( 72 / 2.54 );
  typedef typeof unitTable MapType;
  MapType::const_iterator i = unitTable.find( newUnitName );
  if( i != unitTable.end( ) )
    {
      if( i->second != newUnitValue )
	{
	  Ast::theAnalysisErrorsList.push_back( new Exceptions::IntroducingExistingUnit( metapdflloc, strrefdup( newUnitName ) ) );
	}
    }
  else
    {
      unitTable[ newUnitName ] = newUnitValue;
    }
  BEGIN( INITIAL );
}
<NewUnitValue>{Float}"m" {
  char * end;
  double newUnitValue = metaPDFstrtod( yytext, & end ) * ( 100 * 72 / 2.54 );
  typedef typeof unitTable MapType;
  MapType::const_iterator i = unitTable.find( newUnitName );
  if( i != unitTable.end( ) )
    {
      if( i->second != newUnitValue )
	{
	  Ast::theAnalysisErrorsList.push_back( new Exceptions::IntroducingExistingUnit( metapdflloc, strrefdup( newUnitName ) ) );
	}
    }
  else
    {
      unitTable[ newUnitName ] = newUnitValue;
    }
  BEGIN( INITIAL );
}
<NewUnitValue>{Float}"in" {
  char * end;
  double newUnitValue = metaPDFstrtod( yytext, & end ) * 72;
  typedef typeof unitTable MapType;
  MapType::const_iterator i = unitTable.find( newUnitName );
  if( i != unitTable.end( ) )
    {
      if( i->second != newUnitValue )
	{
	  Ast::theAnalysisErrorsList.push_back( new Exceptions::IntroducingExistingUnit( metapdflloc, strrefdup( newUnitName ) ) );
	}
    }
  else
    {
      unitTable[ newUnitName ] = newUnitValue;
    }
  BEGIN( INITIAL );
}
<NewUnitValue>{Float}{Identifier} {
  char * end;
  double val = metaPDFstrtod( yytext, & end );

  typedef typeof unitTable MapType;
  MapType::const_iterator i = unitTable.find( end );
  if( i == unitTable.end( ) )
    {
      Ast::theAnalysisErrorsList.push_back( new Exceptions::LookupUnknownUnit( metapdflloc, strrefdup( end ) ) );
    }
  else
    {
      double newUnitValue = val * i->second;
      
      i = unitTable.find( newUnitName );
      if( i != unitTable.end( ) )
	{
	  if( i->second != newUnitValue )
	    {
	      Ast::theAnalysisErrorsList.push_back( new Exceptions::IntroducingExistingUnit( metapdflloc, strrefdup( newUnitName ) ) );
	    }
	}
      else
	{
	  unitTable[ newUnitName ] = newUnitValue;
	}
    }
  BEGIN( INITIAL );
}
<NewUnitValue>. {
  Ast::theAnalysisErrorsList.push_back( new Exceptions::ScannerError( metapdflloc, strrefdup( "Expected a length." ) ) );
  BEGIN( DiscardRestOfLineAndBEGIN_INITIAL );
 }



{Float}"°" {
  char * end;
  metapdflval.floatVal = M_PI / 180 * metaPDFstrtod( yytext, & end );
  return T_float;
}

{Float}{Escape}"degree" {
  char * end;
  metapdflval.floatVal = M_PI / 180 * metaPDFstrtod( yytext, & end );
  return T_float;
}

{Float}"bp" {
  char * end;
  metapdflval.floatVal = metaPDFstrtod( yytext, & end );
  return T_length;
}

{Float}"mm" {
  char * end;
  metapdflval.floatVal = metaPDFstrtod( yytext, & end ) * ( 0.1 * 72 / 2.54 );
  return T_length;
}

{Float}"cm" {
  char * end;
  metapdflval.floatVal = metaPDFstrtod( yytext, & end ) * ( 72 / 2.54 );
  return T_length;
}

{Float}"m" {
  char * end;
  metapdflval.floatVal = metaPDFstrtod( yytext, & end ) * ( 100 * 72 / 2.54 );
  return T_length;
}

{Float}"in" {
  char * end;
  metapdflval.floatVal = metaPDFstrtod( yytext, & end ) * 72;
  return T_length;
}

{Float}{Identifier} {
  char * end;
  double val = metaPDFstrtod( yytext, & end );

  typedef typeof unitTable MapType;
  MapType::const_iterator i = unitTable.find( end );
  if( i == unitTable.end( ) )
    {
      Ast::theAnalysisErrorsList.push_back( new Exceptions::LookupUnknownUnit( metapdflloc, strrefdup( end ) ) );
    }
  else
    {
      metapdflval.floatVal = val * i->second;
    }
  return T_length;
}

{Float}[\%][D0] {
  char * end;
  double val = metaPDFstrtod( yytext, & end );
  metapdflval.expr = new Ast::SpecialLength( metapdflloc, val, Computation::SPECIALU_DIST );
  return T_speciallength;
}

{Float}[\%][C1] {
  char * end;
  double val = metaPDFstrtod( yytext, & end );
  metapdflval.expr = new Ast::SpecialLength( metapdflloc, val, Computation::SPECIALU_DIST | Computation::SPECIALU_CIRC );
  return T_speciallength;
}

{Float}[\%][M2] {
  char * end;
  double val = metaPDFstrtod( yytext, & end );
  metapdflval.expr = new Ast::SpecialLength( metapdflloc, val, Computation::SPECIALU_DIST | Computation::SPECIALU_CORR );
  return T_speciallength;
}

{Float}[\%][F3] {
  char * end;
  double val = metaPDFstrtod( yytext, & end );
  metapdflval.expr = new Ast::SpecialLength( metapdflloc, val, Computation::SPECIALU_DIST | Computation::SPECIALU_CIRC | Computation::SPECIALU_CORR );
  return T_speciallength;
}

{Float}[\%][d4] {
  char * end;
  double val = metaPDFstrtod( yytext, & end );
  metapdflval.expr = new Ast::SpecialLength( metapdflloc, val, Computation::SPECIALU_DIST | Computation::SPECIALU_NOINFLEX );
  return T_speciallength;
}

{Float}[\%][c5] {
  char * end;
  double val = metaPDFstrtod( yytext, & end );
  metapdflval.expr = new Ast::SpecialLength( metapdflloc, val, Computation::SPECIALU_DIST | Computation::SPECIALU_CIRC | Computation::SPECIALU_NOINFLEX  );
  return T_speciallength;
}

{Float}[\%][m6] {
  char * end;
  double val = metaPDFstrtod( yytext, & end );
  metapdflval.expr = new Ast::SpecialLength( metapdflloc, val, Computation::SPECIALU_DIST | Computation::SPECIALU_CORR | Computation::SPECIALU_NOINFLEX  );
  return T_speciallength;
}

{Float}[\%][f7] {
  char * end;
  double val = metaPDFstrtod( yytext, & end );
  metapdflval.expr = new Ast::SpecialLength( metapdflloc, val, Computation::SPECIALU_DIST | Computation::SPECIALU_CIRC | Computation::SPECIALU_CORR | Computation::SPECIALU_NOINFLEX  );
  return T_speciallength;
}

{Float}[\%][i9] {
  char * end;
  double val = metaPDFstrtod( yytext, & end );
  metapdflval.expr = new Ast::SpecialLength( metapdflloc, val, Computation::SPECIALU_NOINFLEX );
  return T_speciallength;
}


{Float} {
  char * end;
  metapdflval.floatVal = metaPDFstrtod( yytext, & end );
  return T_float;
}

"\'"[~]?[0-9]+ {
  const char * src = yytext + 1;
  bool negative = false;
  if( *src == '~' )
    {
      negative = true;
      ++src;
    }
  char * end;
  int absval = strtol( src, & end, 10 );
  if( negative )
    {
      metapdflval.intVal = - absval;
    }
  else
    {
      metapdflval.intVal = absval;
    }
  return T_int;
}

"\'0x"[0-9A-F]+ {
  char * end;
  metapdflval.intVal = strtol( yytext + 3, & end, 16 );
  return T_int;
}

"\'0b"[0-1]+ {
  char * end;
  metapdflval.intVal = strtol( yytext + 3, & end, 2 );
  return T_int;
}

"∞" {
  metapdflval.floatVal = HUGE_VAL;
  return T_float;
}

"true" {
  metapdflval.boolVal = true;
  return T_bool;
}

"false" {
  metapdflval.boolVal = false;
  return T_bool;
}

"--" { return T_minusminus; }
"++" { return T_plusplus; }
".." { return T_ddot; }
"..." { return T_dddot; }

"::" { return T_declaretype; }
":=" { return T_assign; }
"{:=}" { return T_flassign; }
"==" { return T_eqeq; }
"=/="|"≠" { return T_eqneq; }

".>" { return T_mapsto; }
"../" { return T_surrounding; }
"->" { return T_bindto; }
"[]" { return T_emptybrackets; }
"[...]" { return T_dddotbrackets; }
"[!]" { return T_bangbrackets; }
"[!...]" { return T_bangdddotbrackets; }
"()" { return T_compose; }

"(>" { return T_unionLeft; }
"<)" { return T_unionRight; }
"<>" { return T_split; }
"(<" { return T_splitLeft; }
">)" { return T_splitRight; }

[\{\}\(\)\[\]\<\>] { return yytext[0]; }
[\.\,\;\:\_\@\!\#\%\&\|\^\-\+\'\"\\] { return yytext[0]; }
[*/~=] { return yytext[0]; }

"*/" { return T_projection; }
"/_" { return T_angle; }

"<=" { return T_lesseq; }
">=" { return T_greatereq; }

"<<" { return T_llthan; }
">>" { return T_ggthan; }

"!!" { return  T_bangbang; }

"@@" { return T_atat; }

"and" { return T_and; }
"or" { return T_or; }
"xor" { return T_xor; }
"not" { return T_not; }
"let" { return T_let; }
"let*" { return T_letstar; }
"letrec" { return T_letrec; }

"dynamic" { return T_dynamic; }

"cycle" { return T_cycle; }

"TeX" { return T_tex; }

"continuation" { return T_continuation; }
"continue" { return T_continue; }
"escape_continuation" { return T_esc_continuation; }
"escape_continue" { return T_esc_continue; }

"class" { return T_class; }
"__members__" { return T_members; }
"__prepare__" { return T_prepare; }
"__abstract__" { return T_abstract; }
"__overrides<" { return T_overrides; }
">__" { return T_gr__; }

{WhiteSpace}+ ;

[\n] {
  metapdflloc.firstLine = metapdflloc.lastLine + 1;
  metapdflloc.lastLine = metapdflloc.firstLine;
  metapdflloc.lastColumn = 0;
}

<INITIAL>"|**".*[\n] { ++metapdflloc.lastLine; metapdflloc.lastColumn = 0; }
<INITIAL>"/**" { quoteDepth = 1; BEGIN( Comment ); }
<INITIAL>"**/" { throw Exceptions::ScannerError( metapdflloc, strrefdup( "Found closing comment delimiter outside comment" ) ); }
<Comment>"/**" { ++quoteDepth; more( ); }
<Comment>"**/" {
  --quoteDepth;
  if( quoteDepth == 0 )
    {
      BEGIN( INITIAL );
    }
  else
    {
      more( );
    }
}
<Comment>. { }
<Comment>[\n] { ++metapdflloc.lastLine; metapdflloc.lastColumn = 0; }
<Comment><<EOF>> { throw Exceptions::ScannerError( metapdflloc, strrefdup( "Found EOF while scanning comment" ) ); }

<INITIAL>[`][\n]? {
  if( yyleng > 1 )
    {
      ++metapdflloc.lastLine;
      metapdflloc.lastColumn = 0;
    }
  quoteDepth = 1;
  BEGIN( String );
}
<INITIAL>"´" { throw Exceptions::ScannerError( metapdflloc, strrefdup( "Found closing quote outside string" ) ); }
<String>"`" { ++quoteDepth; more( ); }
<String>"´" {
  --quoteDepth;
  if( quoteDepth == 0 )
    {
      yytext[ yyleng - 2 ] = '\0';
      metapdflval.str = new char[ yyleng - 1 ];
      {
	char * dst = metapdflval.str;
	char * src = yytext;
	char * end = yytext + yyleng - 2;    // the "- 1" comes from empirical studies...
	for( ; src != end; ++src )
	  {
	    if( *src != 0 )
	      {
		*dst = *src;
		++dst;
	      }
	  }
	/* The following condition catches the optional trailing newline at the end of
	 * the literal.  That this condition is non-trivial indicates that this feature
	 * must be used with care.  For example, when the string creation is automated, either
	 * should all newlines be written `¢n´, or should the optional newlines always be there.
	 * The last of the three conditions ensures that the trailing newline was not generated
	 * as an escape sequence, in which case it shall be kept.
	 */
	if( dst > metapdflval.str && *( dst - 1 ) == '\n' && *( src - 1 ) != '\0' )
	  {
	    --dst;
	  }
	*dst = '\0';
      }
      --metapdflloc.firstColumn;
      BEGIN( INITIAL );
      return T_string;
    }
  else
    {
      more( );
    }
}
<String>[\n] { ++metapdflloc.lastLine; metapdflloc.lastColumn = 0; more( ); }
<String>{Escape}[`nt\n] {
  // The escaped characters each occupy 1 byte
  char * dst = yytext + yyleng - 3; // 3 = 2 + 1
  switch( yytext[ yyleng - 1 ] )
    {
    case 'n':
      *dst = '\n';
      break;
    case 't':
      *dst = '\t';
      break;
    case '\n':
      ++metapdflloc.lastLine;
      metapdflloc.lastColumn = 0;
      *dst = '\0';
      break;
    default:
      *dst = yytext[ yyleng - 1 ];
    }
  ++dst;
  *dst = '\0';
  ++dst;
  *dst = '\0';
  yymore( ); // The purpose of this line is only to let flex know that we use yy_more_flag
  more( );
}
<String>{Escape}({Escape}|"´") {
  char * dst = yytext;
  char * src = yytext + 2;
  for( ; *src != '\0'; ++dst, ++src )
    {
      *dst = *src;
    }
  *dst = '\0';
  ++dst;
  *dst = '\0';
  more( );
}
<String>{Escape} {
  Ast::theAnalysisErrorsList.push_back( new Exceptions::ScannerError( metapdflloc, strrefdup( "The only characters possible to protect using [¢¤] are [¢¤`´nt\n]." ) ) );
  more( );
}
<String>. { more( ); }
<String><<EOF>> { throw Exceptions::ScannerError( metapdflloc, strrefdup( "Found EOF while scanning string" ) ); }

<Incl>[^ \t\n]+ {
  currentNeedFile = yytext;
  currentNeedPushCount = 0;
  currentNeedIsNeed = false;
  BEGIN( InclFrom );
}

<Needs>[^ \t\n]+ {
  currentNeedFile = yytext;
  currentNeedPushCount = 0;
  currentNeedIsNeed = true;
  BEGIN( InclFrom );
}

<InclFrom>[ \t]+ { }
<InclFrom>[\n] { doInclusion( ); }
<InclFrom>":"[ \t]+ { BEGIN( InclPath ); }
<InclFrom>":". { throw Exceptions::ScannerError( metapdflloc, strrefdup( "The \":\" must be followed by whitespace." ) ); }
<InclFrom>. { throw Exceptions::ScannerError( metapdflloc, strrefdup( "Expected \":\"." ) ); }

<InclPath>[ \t]+ { }
<InclPath>[^ \t\n]+ {
  push_frontNeedPath( yytext );
  ++currentNeedPushCount;
}
<InclPath>[\n] {
  if( currentNeedPushCount == 0 )
    {
      throw Exceptions::ScannerError( metapdflloc, strrefdup( "Missing paths after \"from\"." ) );
    }
  doInclusion( );
}

<Echo>.* {
  std::cerr << yytext << std::endl ;
  BEGIN( INITIAL );
}

<Author>.* {
  if( stateStack.size( ) > 0 )
    {
      Kernel::the_pdfo->extensionAuthorStrings.push_back( metapdflloc.filename + std::string( " by " ) + yytext );
    }
  else
    {
      const char * key = "Author";
      if( Kernel::the_pdfo->info_->hasKey( key ) )
	{
	  Ast::theAnalysisErrorsList.push_back( new Exceptions::ScannerError( metapdflloc, strrefdup( "Multiply specified #author." ) ) );
	}
      (*Kernel::the_pdfo->info_)[ key ] = SimplePDF::PDF_out::newString( yytext );
    }
  BEGIN( INITIAL );
}


<<EOF>> {
  if( stateStack.size( ) == 0 )
  {
    if( appendStream_ != 0 )
      {
	yy_switch_to_buffer( yy_create_buffer( appendStream_, YY_BUF_SIZE ) );
	appendStream_ = 0;
      }
    else
      {
	return T_EOF;
      }
  }
  else
  {
    yy_delete_buffer( YY_CURRENT_BUFFER );
    yy_switch_to_buffer( stateStack.top( ) );
    stateStack.pop( );
    metapdflloc = locStack.top( );
    locStack.pop( );
    for( size_t tmp = pathCountStack.top( ); tmp > 0; --tmp )
      {
	pop_frontNeedPath( );
      }
    pathCountStack.pop( );
  }
}

{Identifier} {
  metapdflval.str = strdup( yytext );
  return T_identifier;
}
{TypeMark}{Identifier} {
  const char * id = yytext + 2; // The type mark is allways 2 bytes.
  metapdflval.str = strdup( id );
  return T_typename;
}

{DynamicMark}{Identifier} {
  metapdflval.str = strdup( yytext + 1 );
  return T_dynamic_identifier;
}
{StateMark}{Identifier} {
  const char * id = yytext;
  // Depending on which state mark is used, we must skip different number of bytes.
  if( *id == '#' )
    {
      id += 1;
    }
  else // The state mark is '•'
    {
      id += 3;
    }
  metapdflval.str = strdup( id );
  return T_state_identifier;
}
{DynamicMark}{StateMark}{Identifier} {
  const char * id = yytext + 1; // The dynamic mark is allways 1 byte.
  // Depending on which state mark is used, we must skip different number of bytes.
  if( *id == '#' )
    {
      id += 1;
    }
  else // The state mark is '•'
    {
      id += 3;
    }
  metapdflval.str = strdup( id );
  return T_dynamic_state_identifier;
}

. {
  Ast::theAnalysisErrorsList.push_back( new Exceptions::ScannerError( metapdflloc, strrefdup( ( std::string( "Scanner found unrecognized token: " ) + yytext ).c_str( ) ) ) );
  BEGIN( DiscardRestOfLineAndBEGIN_INITIAL );
}

%%
/* The closing %% above marks the end of the Rules section and the beginning
 * of the User Subroutines section. All text from here to the end of the
 * file is copied verbatim to the end of the generated lex.pdf.c file.
 * This section is where you put definitions of helper functions.
 */

double metaPDFstrtod( const char * str, char ** end )
{
  bool negate = false;
  if( *str == '~' )
    {
      negate = true;
      ++str;
    }
  double absval = strtod( str, end );
  if( negate )
    {
      return -absval;
    }
  return absval;
}

void
MetaPDFScanner::doInclusion( )
{
  std::string path;
  if( currentNeedIsNeed )
    {
      path = searchFile( currentNeedFile + ".shext" );
    }
  else
    {
      path = searchFile( currentNeedFile + ".shape" );
    }

  if( currentNeedIsNeed )
    {
      struct stat theStat;
      if( stat( path.c_str( ), & theStat ) != 0 )
	{
	  throw Exceptions::FileOpenError( metapdflloc, strrefdup( path.c_str( ) ), Exceptions::FileOpenError::STAT );
	}
      FileID fileID( theStat );
      if( neededFiles.find( fileID ) != neededFiles.end( ) )
	{
	  metapdflloc.firstLine = metapdflloc.lastLine + 1;
	  metapdflloc.lastLine = metapdflloc.firstLine;
	  metapdflloc.lastColumn = 0;
	  BEGIN( INITIAL );
	  return;
	}
      neededFiles.insert( fileID );
    }

  std::ifstream * iFile = new std::ifstream( path.c_str( ) );
  if( ! iFile->good( ) )
    {
      throw Exceptions::FileOpenError( metapdflloc, strrefdup( path.c_str( ) ) );
    }

  metapdflloc.firstLine = metapdflloc.lastLine + 1;
  metapdflloc.lastLine = metapdflloc.firstLine;
  metapdflloc.lastColumn = 0;

  locStack.push( metapdflloc );
  metapdflloc = Ast::SourceLocation( strdup( path.c_str( ) ) );
  stateStack.push( YY_CURRENT_BUFFER );
  yy_switch_to_buffer( yy_create_buffer( iFile, YY_BUF_SIZE ) );
  pathCountStack.push( currentNeedPushCount );

  BEGIN( INITIAL );
}

void
MetaPDFScanner::prependStream( std::istream * is )
{
  appendStream_ = yyin;
  yy_switch_to_buffer( yy_create_buffer( is, YY_BUF_SIZE ) );
}

