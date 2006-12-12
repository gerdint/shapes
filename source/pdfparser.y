/* File: pdfparser.y
 * --------------
 * Yacc input file to generate the parser for the input of pdf file structure.
 */

%{

/* Just like lex, the text within this first region delimited by %{ and %}
 * is assumed to be C/C++ code and will be copied verbatim to the y.tab.c
 * file ahead of the definitions of the yyparse() function. Add other header
 * file inclusions or C++ variable declarations/prototypes that are needed
 * by your code here.
 */
#include "pdfstructure.h"

#ifdef yylex
  /* This is ugly.
   * Warning! Warning! Warning!
   * We'll soon use that yylex was defined as
   *   #define yylex pdflex
   * in order to reset it after we're done with the inclusion.
   */
#undef yylex
#include "globals.h"
#define yylex pdflex
#endif

#include "refcount.h"

#include <list>

using namespace std;

int pdflex( );

void pdferror( char * msg )
{
  throw( string( "scanner error: " ) + msg );
}
 
%}

/*
 * The section before the first %% is the Definitions section of the yacc
 * input file. Here is where you declare tokens and types, add precedence
 * and associativity options, and so on.
 */
 
/*
 * yylval 
 * ------
 * Here we define the type of the yylval global variable that is used by
 * the scanner to store attibute information about the token just scanned
 * and thus communicate that information to the parser. You will need to
 * add new fields to this union as you add different attributes to your
 * non-terminal symbols.
 */

%union {
  PDF_Object * pdfObj;
  PDF_Vector * vec;
  PDF_Dictionary * dic;
  PDF_Name * name;
  char * str;
  std::list< RefCountPtr< char > > * strList;
  long plainInt;
  int tokenID;
}


/* Tokens
 * ------
 * Here we tell yacc about all the token types that we are using.
 * Yacc will assign unique numbers to these and export the #define
 * in the generated y.tab.h header file.
 */

%token <tokenID> T_obj T_endobj T_R T_stream T_endstream T_OpenDic T_CloseDic

%token <pdfObj> T_Constant
%token <name> T_Name
%token <plainInt> T_Int
%token <str> T_String


/* Non-terminal types
 * ------------------
 * In order for yacc to assign/access the correct field of $$, $1, we
 * must to declare which field is appropriate for the non-terminal.
 * As an example, this first type declaration establishes that the DeclList
 * non-terminal uses the field named "declList" in the yylval union. This
 * means that when we are setting $$ for a reduction for DeclList ore reading
 * $n which corresponds to a DeclList nonterminal we are accessing the field
 * of the union named "declList" which is of type List<Decl*>.
 * pp2: You'll need to add many of these of your own.
 */

%type <pdfObj> IndirectDef
%type <pdfObj> Expr String Vector Stream Integer Indirect
%type <vec> Exprs
%type <dic> Bindings Dic
%type <strList> StringContents

%start IndirectDefOrDictionary

%%
/*
 * All productions and actions should be placed between the start and stop
 * %% markers which delimit the Rules section.
 */

IndirectDefOrDictionary
: T_Int T_Int T_obj Expr T_endobj
{
  pdfparseReturn.n = $1;
  pdfparseReturn.v = $2;
  pdfparseReturn.obj = $4;
  YYACCEPT;
}
| Dic
{
  pdfparseReturn.n = 0;
  pdfparseReturn.v = 0;
  pdfparseReturn.obj = $1;
  YYACCEPT;
}
;
          
Expr  : T_Constant
      | Integer
      | T_Name { $$ = $1; }
      | String
      | Vector
      | Dic { $$ = $1; }
      | Stream
      | Indirect
      ;

String : '(' StringContents ')' { $$ = new PDF_LiteralString( *$2 ); }
       | '(' ')' { $$ = new PDF_LiteralString( "" ); }
       ;

StringContents : T_String { $$ = new list< RefCountPtr< char > >; $$->push_back( $1 ); }
               | StringContents T_String { $$ = $1; $$->push_back( $2 ); }
               ;

Vector : '[' Exprs ']' { $$ = $2; }
       | '[' ']' { $$ = new PDF_Vector( ); }
       ;

Exprs : Exprs Expr { $$ = $1; $$->vec.push_back( $2 ); }
      | Expr { $$ = new PDF_Vector( ); $$->vec.push_back( $1 ); }
      ;

Dic : T_OpenDic Bindings T_CloseDic { $$ = $2; }
    | T_OpenDic T_CloseDic { $$ = new PDF_Dictionary( ); }
    ;

Bindings : Bindings T_Name Expr { $$ = $1; $$->dic[ $2->name( ) ] = $3; }
         | T_Name Expr { $$ = new PDF_Dictionary( ); $$->dic[ $1->name( ) ] = $2; }
         ;

Stream : Dic T_stream
          {
	    $$ = new PDF_Stream_in( $1, pdfscannerInput, pdfscannerInput->tellg( ) );
	    pdfscannerInput->seekg( $1->getLength( ), ios::cur );
	    pdfscanner.yyrestart( pdfscannerInput );
	    delete $1;
	  } T_endstream { }
       ;

Integer : T_Int { $$ = new PDF_Int( $1 ); }
        ;

Indirect : T_Int T_Int T_R { $$ = new PDF_Indirect_in( $1, $2 ); }
         ;

%%

/* The closing %% above marks the end of the Rules section and the beginning
 * of the User Subroutines section. All text from here to the end of the
 * file is copied verbatim to the end of the generated y.tab.c file.
 * This section is where you put definitions of helper functions.
 */

/*
 * Function: InitParser
 * --------------------
 * This function will be called before any calls to yyparse().  It is designed
 * to give you an opportunity to do anything that must be done to initialize
 * the parser (set global variables, configure starting state, etc.). One
 * thing it already does for you is assign the value of the global variable
 * yydebug that controls whether yacc prints debugging information about
 * parser actions (shift/reduce) and contents of state stack during parser.
 * If set to false, no information is printed. Setting it to true will give
 * you a running trail that might be helpful when debugging your parser.
 * Please be sure the variable is set to false when submitting your final
 * version.
 */
void InitParser( )
{
  yydebug = false;
}

/* When used with the -p option, yylex is a macro that expands to something else.
 */
#undef yylex
int pdflex( )
{
  return pdfscanner.yylex( );
}

