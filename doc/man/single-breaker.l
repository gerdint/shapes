%%

([\n]".ensure-line-break"[ ]*)+ { printf( "\n" ); }
%%

int
main( )
{
	yylex( );
	return 0;
}
