%option noyywrap

WS [ \t\n]*

%%

"void *yyalloc (yy_size_t  );" {
  printf( "static void *yyalloc (yy_size_t  );" );
}
"void *yyrealloc (void *,yy_size_t  );" {
  printf( "static void *yyrealloc (void *,yy_size_t  );" );
}
"void yyfree (void *  );" {
  printf( "static void yyfree (void *  );" );
}

"void *yyalloc (yy_size_t  size )"\n {
  printf( "static void *yyalloc (yy_size_t  size )\n" );
}
"void *yyrealloc  (void * ptr, yy_size_t  size )"\n {
  printf( "static void *yyrealloc  (void * ptr, yy_size_t  size )\n" );
}
"void yyfree (void * ptr )"\n {
  printf( "static void yyfree (void * ptr )\n" );
}


%%

int main( )
{
  yylex( );
  
  if( false )
    {
      yyunput( 0, 0 ); // Just to get rid of compiler warning about unused symbol.
    }

  return 0;
}
