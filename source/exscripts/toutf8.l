%%

[^\xc2]\xb0 { printf( "%c%c%c", yytext[0], -62, yytext[1] ); }
[^\xc2]\xb4 { printf( "%c%c%c", yytext[0], -62, yytext[1] ); }
[^\xc2]\xa2 { printf( "%c%c%c", yytext[0], -62, yytext[1] ); }
[^\xc3]\xe5 { printf( "%c%c%c", yytext[0], -61, -91 ); }
[^\xc3]\xe4 { printf( "%c%c%c", yytext[0], -61, -92 ); }
[^\xc3]\xf6 { printf( "%c%c%c", yytext[0], -61, -74 ); }
[^\xc3]\xc5 { printf( "%c%c%c", yytext[0], -61, -123 ); }
[^\xc3]\xc4 { printf( "%c%c%c", yytext[0], -61, -124 ); }
[^\xc3]\xd6 { printf( "%c%c%c", yytext[0], -61, -106 ); }

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
