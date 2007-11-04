%option noyywrap

WS [ \t\n]*

%%

"class istream;" {
  printf( "#include <iostream>\nusing namespace std;" );
}

"#include <FlexLexer.h>\n"{WS}"int yyFlexLexer::yylex()"{WS}"{"{WS}"LexerError( \"yyFlexLexer::yylex invoked but %option yyclass used\" );"{WS}"return 0;"{WS}"}" {
  printf( "/* The must-not-be-called yyFlexLexer::yylex definition was removed by lexerfixer */\n" );
}

%%

int main( )
{
  yylex( );
  
  printf( "int\n" );
  printf( "yyFlexLexer::yylex( )\n" );
  printf( "{\n" );
  printf( "  LexerError( \"yyFlexLexer::yylex invoked ..., and this message was added by lexerfixer\" );\n" );
  printf( "  return 0;\n" );
  printf( "}\n" );
  

  return 0;
}
