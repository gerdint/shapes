%%

([\n]".ensure-line-break"[ ]*)+ { printf( "\n" ); }

"&lt;" { printf( "<" ); }
"&gt;" { printf( ">" ); }
"&amp;" { printf( "&" ); }
"&apos;" { printf( "'" ); }
"&quot;" { printf( "\"" ); }

%%

int
main( )
{
	yylex( );
	return 0;
}
