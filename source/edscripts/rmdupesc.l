%%

\xc2\xc2 { printf( "%c", yytext[0] ); }

%%

int yywrap()
{
  return 1;
}

int main()
{
  yylex();
  return 0;
}
