%%

^".ensure-line-break"([\n]".ensure-line-break")*[\n]? { printf( "\n" ); }

%%

int
main( )
{
	yylex( );
	return 0;
}
