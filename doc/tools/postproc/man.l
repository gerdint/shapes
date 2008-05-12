/* ÅÄÖåäö -- UTF-8 hint
 */

LineBreak ([\n]".ensure-line-break"([ \t]|" ")*)+
ParagraphBreak ([\n]".ensure-line-break"([ \t]|" ")*)*[\n]+{LineBreak}

%%

[\n]+".br" { printf( "\n.br" ); }
{ParagraphBreak}".SH" { printf( "\n.SH" ); }
{ParagraphBreak}".SS" { printf( "\n.SS" ); }
{ParagraphBreak} { printf( "\n\n" ); }
{LineBreak} { printf( "\n" ); }
"\\\n" { printf( "\\\\\n.br\n" ); }
[\n]{3,} { printf( "\n\n" ); }

"&lt;" { printf( "<" ); }
"&gt;" { printf( ">" ); }
"&amp;" { printf( "&" ); }
"&apos;" { printf( "'" ); }
"&quot;" { printf( "\"" ); }

"—" { printf( "\\-" ); }
"…" { printf( "..." ); }
"⋅" { printf( "*" ); }


%%

int
main( )
{
	yylex( );
	return 0;
}
