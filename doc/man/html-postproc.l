/*ÅÄÖåäö UTF-8 hint
 */
%%

"&bullet;" { printf( "•" ); }
"&acute-accent;" { printf( "´" ); }
"&grave-accent;" { printf( "`" ); }

%%

int
main( )
{
	yylex( );
	return 0;
}
