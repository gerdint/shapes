%{

#include "pdfstructure.h"
#include "pdfscanner.h"
#include <string>
#include <iostream>
#include <iomanip>

unsigned char hexToChar( char c1, char c2 );
unsigned char octalToChar( char c1, char c2, char c3 );

size_t stringParenDepth;

using namespace SimplePDF;

%}

WhiteSpace [ \t\n\r]
Delimiter [\[\]\(\)\{\}\<\>\/\%]
Regular [^ \t\n\r\[\]\(\)\{\}\<\>\/\%]

PlainInteger [0-9]+
DecInteger [+-]?[0-9]+

Float [+-]?[0-9]*[.][0-9]*

Name [/]{Regular}*

HexString [<]({WhiteSpace}|[0-9A-Fa-f])*[>]
ButParentheses ([^()]|(\\(.|\n)))*

IndirectRef {PlainInteger}{WhiteSpace}+{PlainInteger}{WhiteSpace}*"R"
IndirectDef {PlainInteger}{WhiteSpace}+{PlainInteger}{WhiteSpace}*"obj"

%option c++
%option noyywrap

%option prefix="pdf"
%option yyclass="PdfScanner"

%x StringState

%%

{DecInteger} {
	char * end;
	yylval.pdfObj = new PDF_Int( strtol( yytext, & end, 10 ) );
	return T_Constant;
}

{IndirectDef} {
	char * end;
	long i;
	long v;
	i = strtol( yytext, & end, 10 );
	v = strtol( end, & end, 10 );
	yylval.pdfObj = new PDF_Indirect( i, v );
	return T_obj;
}

{IndirectRef} {
	char * end;
	long i;
	long v;
	i = strtol( yytext, & end, 10 );
	v = strtol( end, & end, 10 );
	yylval.pdfR = new PDF_Indirect_in( i, v );
	return T_R;
}

{Float} {
	char * end;
	yylval.pdfObj = new PDF_Float( strtod( yytext, & end ) );
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
	std::string str;

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

	yylval.pdfObj = new PDF_String( str );
	*/
	yytext[ yyleng - 1 ] = '\0';
	yylval.pdfObj = new PDF_HexString( yytext + 1 );
	return T_Constant;
}

<INITIAL>[\(] {
	stringParenDepth = 1;
	BEGIN( StringState );
	return yytext[0];
}
<StringState>[\(] {
	++stringParenDepth;
	yylval.str = strdup( yytext );
	return T_String;
}
<StringState>[\)] {
	--stringParenDepth;
	if( stringParenDepth > 0 )
		{
			yylval.str = strdup( yytext );
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
	yylval.str = res;
	*/
	yylval.str = strdup( yytext );
	return T_String;
}
;;
true { yylval.pdfObj = new PDF_Boolean( true ); return T_Constant; }
false { yylval.pdfObj = new PDF_Boolean( false ); return T_Constant; }
null { yylval.pdfObj = new PDF_Null( ); return T_Constant; }
endobj { return T_endobj; }
stream("\r\n"|"\n") { return T_stream; }
endstream { return T_endstream; }

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
	yylval.pdfObj = new PDF_Name( dstMem.getPtr( ) );
	*/
	yylval.str = strdup( yytext + 1 );
	return T_Name;
}

<INITIAL>[ \t\n\r]+ ;

. { throw( std::string( "PDF scanner found unrecognized token: " ) + yytext ); }

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
