/*
 * File:  pagescanner.l
 * ----------------
 * Lex inupt file to generate the scanner for the scanning of length data stored in a page's content stream.
 */

%{

/* The text within this first region delimited by %{ and %} is assumed to
 * be C/C++ code and will be copied verbatim to the lex.pdf.c file ahead
 * of the definitions of the pdflex() function. Add other header file inclusions
 * or C++ variable declarations/prototypes that are needed by your code here.
 */

#include "pdfstructure.h"
#include "pdfscanner.h"
#include "pdfparser.tab.h"
#include <string.h>
#include <iostream>
#include <iomanip>

unsigned char hexToChar( char c1, char c2 );
unsigned char octalToChar( char c1, char c2, char c3 );

size_t stringParenDepth;

/*
 * Global variable: pdflval
 * -----------------------
 * This global variable is how we get attribute information about the token
 * just scanned to the client. The scanner sets the global variable
 * appropriately and since it's global the client can just read it.  In the
 * future, this variable will be declared for us in the y.tab.c file
 * produced by Yacc, but for now, we declare it manually.
 */
//YYSTYPE pdflval;  // manually declared for pp1, later Yacc provides

/*
 * Global variable: pdflloc
 * -----------------------
 * This global variable is how we get position information about the token
 * just scanned to the client. (Operates similarly to pdflval above)
 */
//struct pdfltype pdflloc; // manually dclared for pp1, later Yacc provides

%}

 /*
  * The section before the first %% is the Definitions section of the lex
  * input file. Here is where you set options for the scanner, define lex
  * states, and can set up definitions to give names to regular expressions
  * as a simple substitution mechanism that allows for more readable
  * entries in the Rules section later. 
  */

WhiteSpace     [ \t\n\r]
NonWhiteSpace [^ \t\n\r]

PlainInteger [0-9]+
DecInteger [+-]?[0-9]+

Float [+-]?[0-9]*[.][0-9]*

Name [/]{NonWhiteSpace}*

HexString [<]({WhiteSpace}|[0-9A-Fa-f])*[>]
ButParentheses ([^()]|(\\(.|\n)))*

%option c++
%option noyywrap

%x StringState

%%

{PlainInteger} {
  char * end;
  pdflval.plainInt = strtol( yytext, & end, 10 );
  return T_Int;
}

{Float} {
  char * end;
  pdflval.pdfObj = new PDF_Float( strtod( yytext, & end ) );
  return T_Constant;
}

{HexString} {
  /* Note that we don't parse the internal meaning of the bracketed contents, since this
   * would only require more work when writing back to a pdf file.
   */
  /*
  const char * src( yytext + 1 );
  char c1;
  char c2;
  string str;

  for( ; isblank( *src ); ++src )
    ;
  c1 = *src;
  ++src;
  while( c1 != '>' )
    {
      for( ; isblank( *src ); ++src )
	;
      c2 = *src;
      ++src;
      if( c2 == '>' )
	{
	  str += hexToChar( c1, 0 );
	  break;
	}
      str += hexToChar( c1, c2 );
      for( ; isblank( *src ); ++src )
	;
      c1 = *src;
      ++src;
    }

  pdflval.pdfObj = new PDF_String( str );
  */
  pdflval.pdfObj = new PDF_HexString( yytext );
  return T_Constant;
}

<INITIAL>[\(] {
  stringParenDepth = 1;
  BEGIN( StringState );
  return yytext[0];
}
<StringState>[\(] {
  ++stringParenDepth;
  pdflval.str = strdup( yytext );
  return T_String;
}
<StringState>[\)] {
  --stringParenDepth;
  if( stringParenDepth > 0 )
    {
      pdflval.str = strdup( yytext );
      return T_String;
    }
  else
    {
      BEGIN( INITIAL );
      return yytext[ 0 ];
    }
}

<StringState>{ButParentheses} {
  /* Note that we don't parse the internal meaning of escape sequences within the name, since this
   * would only require more work when writing back to a pdf file.
   */
  /*
  char * res( new char[ pdfleng + 1 ] );
  char * dst( res.getPtr( ) );
  const char * src( yytext );
  char c;
  while( true )
    {
      c = *(src++);
      if( c == '\\' )
	{
	  char c1( *(src++) );
	  switch( c1 )
	    {
	    case 'n':
	      *(dst++) = '\n';
	      break;
	    case 'r':
	      *(dst++) = '\r';
	      break;
	    case 't':
	      *(dst++) = '\t';
	      break;
	    case 'b':
	      *(dst++) = '\b';
	      break;
	    case 'f':
	      *(dst++) = '\f';
	      break;
	    case '(':
	      *(dst++) = '(';
	      break;
	    case ')':
	      *(dst++) = ')';
	      break;
	    case '\\':
	      *(dst++) = '\\';
	      break;
	    default:
	      if( isdigit( c1 ) )
		{
		  char c2;
		  isPtr->get( c2 );
		  if( isdigit( c2 ) )
		    {
		      char c3;
		      isPtr->get( c3 );
		      if( isdigit( c3 ) )
			{
			  *(dst++) = octalToChar( c1, c2, c3 );
			}
		      else
			{
			  *(dst++) = octalToChar( 0, c1, c2 );
			  *(dst++) = c3;
			}
		    }
		  else
		    {
		      *(dst++) = octalToChar( 0, 0, c1 );
		      *(dst++) = c2;
		    }
		}
	      else
		{
		  *(dst++) = c1;
		}
	      break;
	    }
	}
      else
	{
	  *(dst++) = c;
	}
    }

 done:
  *dst = '\0';
  pdflval.str = res;
  */
  pdflval.str = strdup( yytext );
  return T_String;
}
;;
true { pdflval.pdfObj = new PDF_Boolean( true ); return T_Constant; }
false { pdflval.pdfObj = new PDF_Boolean( false ); return T_Constant; }
null { pdflval.pdfObj = new PDF_Null( ); return T_Constant; }
obj { return T_obj; }
endobj { return T_endobj; }
stream("\r\n"|"\n") { return T_stream; }
endstream { return T_endstream; }
R { return T_R; }

"<<" { return T_OpenDic; }
">>" { return T_CloseDic; }

[\[\]] {
  return yytext[0];
}

{Name} {
  /* Note that we don't parse the internal meaning of #-sequences within the name, since this
   * would only require more work when writing back to a pdf file.
   */
  /*
  const char * src( yytext + 1 );
  RefCountPtr< char > dstMem( new char[ pdflength + 1 ] );
  char * dst( dstMem.getPtr( ) );
  char c;
  for( c = *(src++); ! isblank( c ); c = *(src++) )
    {
      if( c == '#' )
	{
	  char c1;
	  char c2;
	  c1 = *(src++);
	  c2 = *(src++);
	  (dst++) = hexToChar( c1, c2 );
	}
      else
	{
	  (dst++) = c;
	}
    }
  *dst = '\0';
  pdflval.pdfObj = new PDF_Name( dstMem.getPtr( ) );
  */
  pdflval.pdfObj = new PDF_Name( yytext + 1 );
  return T_Name;
}

<INITIAL>[ \t\n\r]+ ;

. { throw( "Page scanner found unrecognized token" ); }

%%
/* The closing %% above marks the end of the Rules section and the beginning
 * of the User Subroutines section. All text from here to the end of the
 * file is copied verbatim to the end of the generated lex.pdf.c file.
 * This section is where you put definitions of helper functions.
 */


unsigned char hexToChar( char c1, char c2 )
{
  unsigned char res( 0 );
  if( c1 < 'A' )
    {
      res += 16 * static_cast< unsigned char >( c1 - '0' );
    }
  else if( c1 < 'a' )
    {
      res += 16 * static_cast< unsigned char >( c1 - 'A' + 10 );
    }
  else
    {
      res += 16 * static_cast< unsigned char >( c1 - 'a' + 10 );
    }

  if( c2 < 'A' )
    {
      res += static_cast< unsigned char >( c2 - '0' );
    }
  else if( c2 < 'a' )
    {
      res += static_cast< unsigned char >( c2 - 'A' + 10 );
    }
  else
    {
      res += static_cast< unsigned char >( c2 - 'a' + 10 );
    }

    return res;
}

unsigned char octalToChar( char c1, char c2, char c3 )
{
  return 64 * static_cast< unsigned char >( c1 - '0' ) + 8 * static_cast< unsigned char >( c2 - '0' ) + static_cast< unsigned char >( c3 - '0' );
}
