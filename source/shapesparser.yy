/* This file is part of Shapes.
 *
 * Shapes is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * any later version.
 *
 * Shapes is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with Shapes.  If not, see <http://www.gnu.org/licenses/>.
 *
 * Copyright 2008 Henrik Tidefelt
 */

/* File: shapesparser.y
 * --------------
 * Yacc input file to generate the parser for the Shapes language
 */

%{

/* Just like lex, the text within this first region delimited by %{ and %}
 * is assumed to be C/C++ code and will be copied verbatim to the y.tab.c
 * file ahead of the definitions of the yyparse() function. Add other header
 * file inclusions or C++ variable declarations/prototypes that are needed
 * by your code here.
 */
#include "shapestypes.h"
#include "ast.h"
#include "astflow.h"
#include "astexprs.h"
#include "astfun.h"
#include "astvalues.h"
#include "astvar.h"
#include "astidentifier.h"
#include "astclass.h"
#include "shapesexceptions.h"
#include "consts.h"
#include "charptrless.h"
#include "autoonoff.h"
#include "shapescore.h"
#include "texlabelmanager.h"

using namespace Shapes;
#include "yyltype.h"
extern YYLTYPE shapeslloc;

#ifdef yylex
	/* This is ugly.
	 * Warning! Warning! Warning!
	 * We'll soon use that yylex was defined as
	 *	 #define yylex shapeslex
	 * in order to reset it after we're done with the inclusion.
	 */
#undef yylex
#include "globals.h"
#include "shapesscanner.h"
int shapeslex( )
{
	return Ast::theShapesScanner.yylex( );
}
#define yylex shapeslex
#endif

#include "refcount.h"

#include <list>
#include <map>
#include <sstream>

using namespace std;


int shapeslex( );


void shapeserror( RefCountPtr< const char > msg )
{
	throw Exceptions::ParserError( shapeslloc, msg );
}

void shapeserror( char * msg )
{
	shapeserror( strrefdup( msg ) );
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
	Ast::Expression * expr;
	Ast::Node * node;
	Ast::MethodIdExpr * methodId;
	std::list< Ast::Node * > * nodeList;
	std::list< Ast::Expression * > * exprList;
	std::list< RefCountPtr< const char > > * strList;
	std::map< const char *, Ast::Expression *, charPtrLess > * namedExprMap;
	Kernel::Formals * formals;
	std::list< Kernel::Formals * > * formalsList;
	Ast::ArgListExprs * argList;
	Ast::SplitDefineVariables * splitFormals;
	int intVal;
	double floatVal;
	bool boolVal;
	char * str;
	int tokenID;
	std::list< const Ast::CallExpr * > * callExprList;
	std::list< Ast::ClassSection * > * classSectionList;
	Ast::ClassSection * classSection;
	Ast::MemberSection * memberSection;
	Ast::MemberDeclaration * memberDeclaration;
	Ast::StateReference * stateReference;
	Ast::MemberMode memberMode;
	Ast::ClassMode classMode;
	Ast::FunctionMode functionMode;
}


/* Tokens
 * ------
 * Here we tell yacc about all the token types that we are using.
 * Yacc will assign unique numbers to these and export the #define
 * in the generated y.tab.h header file.
 */

%token <tokenID> T_EOF T_minusminus T_plusplus T_ddot T_dddot T_assign T_eqeq T_eqneq T_flassign T_atat T_projection T_angle T_ampersandMore
%token <tokenID> T_cycle T_and T_or T_xor T_not T_mapsto T_bindto T_emptybrackets T_dddotbrackets T_bangbrackets T_bangdddotbrackets T_compose T_surrounding T_lesseq T_greatereq T_llthan T_ggthan T_declaretype T_bangbang
%token <tokenID> T_let T_letstar T_letrec
%token <tokenID> T_tex T_dynamic T_continuation T_continue T_esc_continuation T_esc_continue
%token <tokenID> T_class T_members T_prepare T_abstract T_overrides T_gr__
%token <tokenID> T_split T_splitLeft T_splitRight T_unionLeft T_unionRight
 // %token <tokenID>	T_letdst T_plusassign T_minusassign T_starassign T_slashassign

%token <intVal> T_int
%token <floatVal> T_float T_length
%token <expr> T_speciallength
%token <boolVal> T_bool
%token <str> T_string T_identifier T_dynamic_identifier T_state_identifier T_dynamic_state_identifier T_typename

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

%type <expr> Program Expr ExprExceptConstStrings DynamicBinding CallExpr CurryCallExpr MutateExpr Function OperatorFunction Class ConstantExceptStrings Coords PolarHandle
%type <expr> NamedLetExpr								 //	 LetDestinations LetExpr LetStarExpr LetrecExpr
%type <expr> CodeBracket SuperCall SuperMemberReference
%type <exprList> InsertionSequence
%type <node> GroupElem
%type <nodeList> Group OneOrMoreGroupElems
%type <strList> OrderedFormals OneOrMoreOrderedFormals
%type <formals> Formals OneOrMoreFormalsItems
 //					%type <formalsList> NamedFormalsWithOrder OneOrMoreNamedFormalsWithOrder
%type <argList> ArgList OneOrMoreArgListItems
%type <methodId> MethodIdentifier
%type <callExprList> ListOfParentsWithInitargs;
%type <classSectionList> ClassSections OneOrMoreClassSections
%type <classSection> ClassSection
%type <memberSection> MemberDeclarations OneOrMoreMemberDeclarations
%type <memberSection> MethodDeclarations OneOrMoreMethodDeclarations
%type <memberDeclaration> MemberDeclaration MethodDeclaration
%type <memberMode> MemberAccessList MemberAccessSpecifier
%type <classMode> ClassModeList ClassModeSpecifier OneOrMoreClassModeSpecifiers
%type <functionMode> FunctionModeList OneOrMoreFunctionModeSpecifiers FunctionModeSpecifier
%type <expr> Split
%type <stateReference> StateReference
%type <splitFormals> SplitFormals OneOrMoreSplitFormals

%nonassoc T_assign ':'
%left ']' T_splitRight
%left T_llthan
%nonassoc T_bangbang
%right '|'
%right T_mapsto T_emptybrackets T_bangbrackets
%left T_dddotbrackets T_bangdddotbrackets
%left T_ampersandMore
%left '&'
%nonassoc T_dynamiccolon
%left T_or T_xor
%left T_and
%left T_not
%nonassoc T_eqeq T_eqneq
%left T_plusplus T_minusminus
%left '<' '>'
%nonassoc T_lesseq T_greatereq
%left '+' '-'
%nonassoc T_angle
%left '*' '/' T_projection
%left '~'
%left T_compose
%left '[' '.' T_splitLeft
%left '#'
%left T_split
%left T_atat T_surrounding

%nonassoc ')'
%left ','

%start Program

%name-prefix="shapes"

%%
/*
 * All productions and actions should be placed between the start and stop
 * %% markers which delimit the Rules section.
 */

Program
: Group T_EOF
{
	$$ = new Ast::CodeBracket( @1, $1 );
	Ast::theProgram = $$;
	YYACCEPT;
}
| Group error
{
	shapeserror( "Expecting end of file." );
}
;


Coords
: '(' Expr ',' Expr ')'
{
	Ast::ArgListExprs * args = new Ast::ArgListExprs( true );
	args->orderedExprs_->push_back( $2 );
	args->orderedExprs_->push_back( $4 );
	$$ = new Ast::CallExpr( @$,
													Ast::THE_FUNCTION_coords2D,
													args );
}
| '(' Expr ',' Expr '^' Expr ')'
{
	Ast::ArgListExprs * args = new Ast::ArgListExprs( true );
	args->orderedExprs_->push_back( $2 );
	args->orderedExprs_->push_back( $4 );
	args->orderedExprs_->push_back( $6 );
	$$ = new Ast::CallExpr( @$,
													Ast::THE_FUNCTION_cornercoords2D,
													args );
}
| '(' Expr ',' Expr '^' ')'
{
	Ast::ArgListExprs * args = new Ast::ArgListExprs( true );
	args->orderedExprs_->push_back( $2 );
	args->orderedExprs_->push_back( $4 );
	$$ = new Ast::CallExpr( @$,
													Ast::THE_FUNCTION_cornercoords2D,
													args );
}
| '(' Expr ',' Expr ',' Expr ')'
{
	Ast::ArgListExprs * args = new Ast::ArgListExprs( true );
	args->orderedExprs_->push_back( $2 );
	args->orderedExprs_->push_back( $4 );
	args->orderedExprs_->push_back( $6 );
	$$ = new Ast::CallExpr( @$,
													Ast::THE_FUNCTION_coords3D,
													args );
}
;


PolarHandle
: '(' Expr '^' Expr ')'
{
	$$ = new Ast::PolarHandle2DExpr( @$, $2, $4 );
}
| '(' Expr '^' ')'
{
	$$ = new Ast::PolarHandle2DExprFree_a( @$, $2 );
}
| '(' '^' Expr ')'
{
	Ast::ArgListExprs * args = new Ast::ArgListExprs( true );
	args->orderedExprs_->push_back( $3 );
	$$ = new Ast::CallExpr( @$,
													Ast::THE_FUNCTION_polarHandle2DFree_r,
													args );
}
| '(' '^' ')'
{
	Ast::ArgListExprs * args = new Ast::ArgListExprs( true );
	$$ = new Ast::CallExpr( @$,
													Ast::THE_FUNCTION_polarHandle2DFree_ra,
													args );
}
;

StateReference
: T_state_identifier
{
	$$ = new Ast::LexiographicState( @1, $1, new Kernel::Environment::LexicalKey * ( 0 ) );
}
| T_dynamic_state_identifier
{
	$$ = new Ast::DynamicState( @1, $1 );
}
;

ArgList
:
{
	$$ = new Ast::ArgListExprs( true );
}
| OneOrMoreArgListItems
;

OneOrMoreArgListItems
: Expr
{
	$$ = new Ast::ArgListExprs( true );
	$$->orderedExprs_->push_back( $1 );
}
| StateReference
{
	$$ = new Ast::ArgListExprs( true );
	$$->orderedStates_->push_back( $1 );
}
| T_identifier ':' Expr
{
	$$ = new Ast::ArgListExprs( true );
	(*$$->namedExprs_)[ $1 ] = $3;
}
| T_state_identifier ':' StateReference
{
	$$ = new Ast::ArgListExprs( true );
	(*$$->namedStates_)[ $1 ] = $3;
}
| OneOrMoreArgListItems Expr
{
	$$ = $1;
	if( ! $$->namedExprs_->empty( ) )
		{
			Ast::theAnalysisErrorsList.push_back( new Exceptions::ParserError( @2, strrefdup( "Unnamed expressions may not appear among named expressions." ) ) );
		}
	$$->orderedExprs_->push_back( $2 );
}
| OneOrMoreArgListItems StateReference
{
	$$ = $1;
	if( ! $$->namedStates_->empty( ) )
		{
			Ast::theAnalysisErrorsList.push_back( new Exceptions::ParserError( @2, strrefdup( "Unnamed states may not appear among named states." ) ) );
		}
	$$->orderedStates_->push_back( $2 );
}
| OneOrMoreArgListItems T_identifier ':' Expr
{
	$$ = $1;
	if( $$->namedExprs_->find( $2 ) != $$->namedExprs_->end( ) )
		{
			Ast::theAnalysisErrorsList.push_back( new Exceptions::RepeatedFormal( @2, $2 ) );
		}
	(*$$->namedExprs_)[ $2 ] = $4;
}
| OneOrMoreArgListItems T_state_identifier ':' StateReference
{
	$$ = $1;
	if( $$->namedStates_->find( $2 ) != $$->namedStates_->end( ) )
		{
			Ast::theAnalysisErrorsList.push_back( new Exceptions::RepeatedFormal( @2, $2 ) );
		}
	(*$$->namedStates_)[ $2 ] = $4;
}
;


CallExpr
: '[' Expr ArgList ']'
{
	$$ = new Ast::CallExpr( @$, $2, $3 );
}
| Expr T_emptybrackets Expr
{
	Ast::ArgListExprs * args = new Ast::ArgListExprs( true );
	args->orderedExprs_->push_back( $3 );
	$$ = new Ast::CallExpr( @$,
													$1,
													args );
}
| Expr T_emptybrackets Split
{
	$$ = new Ast::CallSplitExpr( @$,
															 $1,
															 $3 );
}
| Expr T_dddotbrackets Expr
{
	Ast::ArgListExprs * args = new Ast::ArgListExprs( true );
	args->orderedExprs_->push_back( $3 );
	$$ = new Ast::CallExpr( @$,
													$1,
													args,
													true );	/* true means Curry */
}
| Expr T_dddotbrackets T_identifier ':' Expr %prec T_dynamiccolon
{
	Ast::ArgListExprs * args = new Ast::ArgListExprs( true );
	(*args->namedExprs_)[ $3 ] = $5;
	$$ = new Ast::CallExpr( @$,
													$1,
													args,
													true );	/* true means Curry */
}
| Expr T_dddotbrackets Split
{
	$$ = new Ast::CallSplitExpr( @$,
															 $1,
															 $3,
															 true );	/* true means Curry */
}
| '[' '!' Expr ArgList ']'
{
	$$ = new Ast::CallExpr( @$, $3, $4, false, true );	/* false for no curry, true for procedural */
}
| Expr T_bangbrackets Expr
{
	Ast::ArgListExprs * args = new Ast::ArgListExprs( true );
	args->orderedExprs_->push_back( $3 );
	$$ = new Ast::CallExpr( @$,
													$1,
													args,
													false,	/* false means no curry */
													true ); /* true means procedural */
}
| Expr T_bangbrackets Split
{
	$$ = new Ast::CallSplitExpr( @$,
															 $1,
															 $3,
															 false, /* false means no curry */
															 true ); /* true means procedural */
}
| Expr T_bangdddotbrackets Expr
{
	Ast::ArgListExprs * args = new Ast::ArgListExprs( true );
	args->orderedExprs_->push_back( $3 );
	$$ = new Ast::CallExpr( @$,
													$1,
													args,
													true,	/* true means Curry */
													true ); /* true means procedural */
}
| Expr T_bangdddotbrackets Split
{
	$$ = new Ast::CallSplitExpr( @$,
															 $1,
															 $3,
															 true, /* false means no curry */
															 true ); /* true means procedural */
}
;


CurryCallExpr
: '[' Expr ArgList T_dddot ']'
{
	$$ = new Ast::CallExpr( @$, $2, $3, true ); /* true means Curry */
}
;

MutateExpr
: StateReference '.' '[' T_identifier ArgList ']'
{
	Ast::CallExpr * res =
		new Ast::CallExpr( @$,
											 new Ast::MutatorReference( @4, $1, $4 ),
											 $5 );
	res->setMutatorSelf( $1 );
	$$ = res;
}


Formals
:
{
	$$ = new Kernel::Formals( );
	$$->setLoc( @$ );
}
| T_split T_identifier
{
	$$ = new Kernel::Formals( );
	$$->argumentOrder_->insert( std::pair< const char *, size_t >( $2, $$->defaultExprs_.size( ) ) );
	/* Note that we do not push a default expression (not even a null pointer) for the sink.
	 * This way, the length of defaultExprs_ allways gives the number of non-sink arguments.
	 * The default value for the sink is taken care of in a non-standard way anyway.
	 */
	$$->setLoc( @$ );
	$$->sink_ = $2;
}
| OneOrMoreFormalsItems
{
	$$ = $1;
	$$->setLoc( @$ );
}
| OneOrMoreFormalsItems T_split T_identifier
{
	$$ = $1;
	$$->argumentOrder_->insert( std::pair< const char *, size_t >( $3, $$->defaultExprs_.size( ) ) );
	/* Note that we do not push a default expression (not even a null pointer) for the sink.
	 * This way, the length of defaultExprs_ allways gives the number of non-sink arguments.
	 * The default value for the sink is taken care of in a non-standard way anyway.
	 */
	$$->setLoc( @$ );
	$$->sink_ = $3;
}
;

OneOrMoreFormalsItems
: T_identifier
{
	$$ = new Kernel::Formals( );
	$$->argumentOrder_->insert( std::pair< const char *, size_t >( $1, $$->defaultExprs_.size( ) ) );
	$$->defaultExprs_.push_back( 0 );
}
| T_identifier ':' Expr
{
	$$ = new Kernel::Formals( );
	$$->argumentOrder_->insert( std::pair< const char *, size_t >( $1, $$->defaultExprs_.size( ) ) );
	$$->defaultExprs_.push_back( $3 );
}
| T_state_identifier
{
	$$ = new Kernel::Formals( );
	$$->stateOrder_->insert( std::pair< const char *, size_t >( $1, $$->stateOrder_->size( ) ) );
}
| OneOrMoreFormalsItems T_identifier
{
	$$ = $1;
	if( $$->seenDefault_ )
		{
			Ast::theAnalysisErrorsList.push_back( new Exceptions::ParserError( @2, strrefdup( "Order-based formals may not appear among named formals." ) ) );
		}
	if( $$->argumentOrder_->find( $2 ) != $$->argumentOrder_->end( ) )
		{
			Ast::theAnalysisErrorsList.push_back( new Exceptions::RepeatedFormal( @2, $2 ) );
		}
	$$->argumentOrder_->insert( std::pair< const char *, size_t >( $2, $$->defaultExprs_.size( ) ) );
	$$->defaultExprs_.push_back( 0 );
}
| OneOrMoreFormalsItems T_identifier ':' Expr
{
	$$ = $1;
	$$->seenDefault_ = true;
	if( $$->argumentOrder_->find( $2 ) != $$->argumentOrder_->end( ) )
		{
			Ast::theAnalysisErrorsList.push_back( new Exceptions::RepeatedFormal( @2, $2 ) );
		}
	$$->argumentOrder_->insert( std::pair< const char *, size_t >( $2, $$->defaultExprs_.size( ) ) );
	$$->defaultExprs_.push_back( $4 );
}
| OneOrMoreFormalsItems T_state_identifier
{
	$$ = $1;
	if( $$->stateOrder_->find( $2 ) != $$->stateOrder_->end( ) )
		{
			Ast::theAnalysisErrorsList.push_back( new Exceptions::RepeatedFormal( @2, $2 ) );
		}
	$$->stateOrder_->insert( std::pair< const char *, size_t >( $2, $$->stateOrder_->size( ) ) );
}
;


SplitFormals
:
{
	Ast::theAnalysisErrorsList.push_back( new Exceptions::ParserError( @$, strrefdup( "The list of split assignment variables must not be empty." ) ) );
	$$ = new Ast::SplitDefineVariables( );
}
| T_split T_identifier
{
	Ast::theAnalysisErrorsList.push_back( new Exceptions::ParserError( @$, strrefdup( "Just a sink in a split assignment formals list makes no sense." ) ) );
	$$ = new Ast::SplitDefineVariables( );
}
| OneOrMoreSplitFormals
{
	$$ = $1;
}
| OneOrMoreSplitFormals T_split T_identifier
{
	$$ = $1;
	Ast::StructSplitSink * expr = new Ast::StructSplitSink( );
	size_t ** pos = new size_t * ( 0 );
	$$->sinkDefine_ = new Ast::DefineVariable( @3, $3, expr, pos );
	$$->sinkExpr_ = expr;
}
;

OneOrMoreSplitFormals
: T_identifier
{
	$$ = new Ast::SplitDefineVariables( );
	typedef typeof $$->exprs_ ListType;
	size_t ** pos = new size_t * ( 0 );
	Ast::StructSplitReference * ref = new Ast::StructSplitReference( @1, static_cast< size_t >( 0 ), 0 );
	$$->exprs_.push_back( ListType::value_type( new Ast::DefineVariable( @1, $1, ref, pos ),
																							 ref ) );
}
| T_identifier ':' Expr
{
	$$ = new Ast::SplitDefineVariables( );
	typedef typeof $$->exprs_ ListType;
	size_t ** pos = new size_t * ( 0 );
	Ast::StructSplitReference * ref = new Ast::StructSplitReference( @1, static_cast< size_t >( 0 ), $3 );
	$$->exprs_.push_back( ListType::value_type( new Ast::DefineVariable( @1, $1, ref, pos ),
																							 ref ) );
}
| T_identifier ':' '.' T_identifier
{
	$$ = new Ast::SplitDefineVariables( );
	typedef typeof $$->exprs_ ListType;
	size_t ** pos = new size_t * ( 0 );
	Ast::StructSplitReference * ref = new Ast::StructSplitReference( @4, $4, 0 );
	$$->exprs_.push_back( ListType::value_type( new Ast::DefineVariable( @1, $1, ref, pos ),
																							 ref ) );
}
| T_identifier ':' '.' T_identifier ':' Expr
{
	$$ = new Ast::SplitDefineVariables( );
	typedef typeof $$->exprs_ ListType;
	size_t ** pos = new size_t * ( 0 );
	Ast::StructSplitReference * ref = new Ast::StructSplitReference( @4, $4, $6 );
	$$->exprs_.push_back( ListType::value_type( new Ast::DefineVariable( @1, $1, ref, pos ),
																							 ref ) );
}
| T_identifier ':' '.' '\"'
{
	$$ = new Ast::SplitDefineVariables( );
	typedef typeof $$->exprs_ ListType;
	size_t ** pos = new size_t * ( 0 );
	Ast::StructSplitReference * ref = new Ast::StructSplitReference( @4, strdup( $1 ), 0 );
	$$->exprs_.push_back( ListType::value_type( new Ast::DefineVariable( @1, $1, ref, pos ),
																							 ref ) );
}
| T_identifier ':' '.' '\"' ':' Expr
{
	$$ = new Ast::SplitDefineVariables( );
	typedef typeof $$->exprs_ ListType;
	size_t ** pos = new size_t * ( 0 );
	Ast::StructSplitReference * ref = new Ast::StructSplitReference( @4, strdup( $1 ), $6 );
	$$->exprs_.push_back( ListType::value_type( new Ast::DefineVariable( @1, $1, ref, pos ),
																							 ref ) );
}
| OneOrMoreSplitFormals T_identifier
{
	$$ = $1;
	if( $$->seenNamed_ )
		{
			Ast::theAnalysisErrorsList.push_back( new Exceptions::ParserError( @2, strrefdup( "Order-based formals may not appear among named formals." ) ) );
		}
	if( $$->seenDefault_ )
		{
			Ast::theAnalysisErrorsList.push_back( new Exceptions::ParserError( @2, strrefdup( "All order-based formals without default values must be placed before those with default values." ) ) );
		}
	typedef typeof $$->exprs_ ListType;
	size_t ** pos = new size_t * ( 0 );
	Ast::StructSplitReference * ref = new Ast::StructSplitReference( @2, $$->exprs_.size( ), 0 );
	$$->exprs_.push_back( ListType::value_type( new Ast::DefineVariable( @2, $2, ref, pos ),
																							 ref ) );
}
| OneOrMoreSplitFormals T_identifier ':' Expr
{
	$$ = $1;
	if( $$->seenNamed_ )
		{
			Ast::theAnalysisErrorsList.push_back( new Exceptions::ParserError( @2, strrefdup( "Order-based formals may not appear among named formals." ) ) );
		}
	$$->seenDefault_ = true;
	typedef typeof $$->exprs_ ListType;
	size_t ** pos = new size_t * ( 0 );
	Ast::StructSplitReference * ref = new Ast::StructSplitReference( @2, $$->exprs_.size( ), $4 );
	$$->exprs_.push_back( ListType::value_type( new Ast::DefineVariable( @2, $2, ref, pos ),
																							 ref ) );
}
| OneOrMoreSplitFormals T_identifier ':' '.' T_identifier
{
	$$ = $1;
	$$->seenNamed_ = true;
	typedef typeof $$->exprs_ ListType;
	size_t ** pos = new size_t * ( 0 );
	Ast::StructSplitReference * ref = new Ast::StructSplitReference( @5, $5, 0 );
	$$->exprs_.push_back( ListType::value_type( new Ast::DefineVariable( @2, $2, ref, pos ),
																							 ref ) );
}
| OneOrMoreSplitFormals T_identifier ':' '.' T_identifier ':' Expr
{
	$$ = $1;
	$$->seenNamed_ = true;
	typedef typeof $$->exprs_ ListType;
	size_t ** pos = new size_t * ( 0 );
	Ast::StructSplitReference * ref = new Ast::StructSplitReference( @5, $5, $7 );
	$$->exprs_.push_back( ListType::value_type( new Ast::DefineVariable( @2, $2, ref, pos ),
																							 ref ) );
}
| OneOrMoreSplitFormals T_identifier ':' '.' '\"'
{
	$$ = $1;
	$$->seenNamed_ = true;
	typedef typeof $$->exprs_ ListType;
	size_t ** pos = new size_t * ( 0 );
	Ast::StructSplitReference * ref = new Ast::StructSplitReference( @5, strdup( $2 ), 0 );
	$$->exprs_.push_back( ListType::value_type( new Ast::DefineVariable( @2, $2, ref, pos ),
																							 ref ) );
}
| OneOrMoreSplitFormals T_identifier ':' '.' '\"' ':' Expr
{
	$$ = $1;
	$$->seenNamed_ = true;
	typedef typeof $$->exprs_ ListType;
	size_t ** pos = new size_t * ( 0 );
	Ast::StructSplitReference * ref = new Ast::StructSplitReference( @5, strdup( $2 ), $7 );
	$$->exprs_.push_back( ListType::value_type( new Ast::DefineVariable( @2, $2, ref, pos ),
																							 ref ) );
}
;


Function
: '\\' Formals T_mapsto FunctionModeList Expr
{
	Ast::ArgListExprs * args = new Ast::ArgListExprs( false );
	Ast::FunctionFunction * res = new Ast::FunctionFunction( @$, $2, $5, $4 );
	res->push_exprs( args );
	$$ = new Ast::CallExpr( @$,
													RefCountPtr< const Lang::Function >( res ),
													args );
}
| '(' OperatorFunction ')'
{
	$$ = $2;
}
;

OrderedFormals
:
{
	$$ = new list< RefCountPtr< const char > >( );
}
| OneOrMoreOrderedFormals
;

OneOrMoreOrderedFormals
: T_identifier
{
	$$ = new list< RefCountPtr< const char > >( );
	$$->push_back( strrefdup( $1 ) );
}
| OneOrMoreOrderedFormals T_identifier
{
	$$ = $1;
	$$->push_back( strrefdup( $2 ) );
}
;

OperatorFunction
: T_minusminus
{
	$$ = new Ast::Constant( @$, static_cast< RefCountPtr< const Lang::Value > >( Lang::THE_OPERATOR_MINUSMINUS ) );
}
| T_plusplus
{
	$$ = new Ast::Constant( @$, static_cast< RefCountPtr< const Lang::Value > >( Lang::THE_OPERATOR_PLUSPLUS ) );
}
| '&'
{
	$$ = new Ast::Constant( @$, static_cast< RefCountPtr< const Lang::Value > >( Lang::THE_OPERATOR_AMPERSAND ) );
}
| '+'
{
	$$ = new Ast::Constant( @$, static_cast< RefCountPtr< const Lang::Value > >( Lang::THE_OPERATOR_PLUS ) );
}
| '-'
{
	$$ = new Ast::Constant( @$, static_cast< RefCountPtr< const Lang::Value > >( Lang::THE_OPERATOR_MINUS ) );
}
| '*'
{
	$$ = new Ast::Constant( @$, static_cast< RefCountPtr< const Lang::Value > >( Lang::THE_OPERATOR_STAR ) );
}
| '/'
{
	$$ = new Ast::Constant( @$, static_cast< RefCountPtr< const Lang::Value > >( Lang::THE_OPERATOR_SLASH ) );
}
| T_projection
{
	$$ = new Ast::Constant( @$, static_cast< RefCountPtr< const Lang::Value > >( Lang::THE_OPERATOR_PROJECTION ) );
}
| T_angle
{
	$$ = new Ast::Constant( @$, static_cast< RefCountPtr< const Lang::Value > >( Lang::THE_OPERATOR_ANGLE ) );
}
| T_ampersandMore
{
	$$ = new Ast::Constant( @$, static_cast< RefCountPtr< const Lang::Value > >( Lang::THE_OPERATOR_AMPERSAND_MORE ) );
}
| '~'
{
	$$ = new Ast::Constant( @$, static_cast< RefCountPtr< const Lang::Value > >( Lang::THE_OPERATOR_NEG ) );
}
| T_compose
{
	$$ = new Ast::Constant( @$, static_cast< RefCountPtr< const Lang::Value > >( Lang::THE_OPERATOR_COMPOSE ) );
}
| '<'
{
	$$ = new Ast::Constant( @$, static_cast< RefCountPtr< const Lang::Value > >( Lang::THE_OPERATOR_LESS ) );
}
| '>'
{
	$$ = new Ast::Constant( @$, static_cast< RefCountPtr< const Lang::Value > >( Lang::THE_OPERATOR_GREATER ) );
}
| T_eqeq
{
	$$ = new Ast::Constant( @$, static_cast< RefCountPtr< const Lang::Value > >( Lang::THE_OPERATOR_EQEQ ) );
}
| T_eqneq
{
	$$ = new Ast::Constant( @$, static_cast< RefCountPtr< const Lang::Value > >( Lang::THE_OPERATOR_EQNEQ ) );
}
| T_lesseq
{
	$$ = new Ast::Constant( @$, static_cast< RefCountPtr< const Lang::Value > >( Lang::THE_OPERATOR_LESSEQ ) );
}
| T_greatereq
{
	$$ = new Ast::Constant( @$, static_cast< RefCountPtr< const Lang::Value > >( Lang::THE_OPERATOR_GREATEREQ ) );
}
| T_not
{
	$$ = new Ast::Constant( @$, static_cast< RefCountPtr< const Lang::Value > >( Lang::THE_OPERATOR_NOT ) );
}
| T_and
{
	$$ = new Ast::Constant( @$, static_cast< RefCountPtr< const Lang::Value > >( Lang::THE_FUNCTION_AND ) );
}
| T_or
{
	$$ = new Ast::Constant( @$, static_cast< RefCountPtr< const Lang::Value > >( Lang::THE_FUNCTION_OR ) );
}
| T_xor
{
	$$ = new Ast::Constant( @$, static_cast< RefCountPtr< const Lang::Value > >( Lang::THE_OPERATOR_XOR ) );
}
;

Expr
: ExprExceptConstStrings
| T_string
{
	$$ = new Ast::Constant( @1, new Lang::String( $1, false ) );
}
;

ExprExceptConstStrings
: ConstantExceptStrings
| Coords
| PolarHandle
| '(' T_tex ExprExceptConstStrings ')'
{
	Ast::ArgListExprs * args = new Ast::ArgListExprs( true );
	args->orderedExprs_->push_back( $3 );
	$$ = new Ast::CallExpr( @$,
													Ast::THE_FUNCTION_TeX,
													args );
}
| '(' T_tex T_string ')'
{
	Kernel::theTeXLabelManager.announce( string( $3 ), @3 );
	Ast::ArgListExprs * args = new Ast::ArgListExprs( true );
	args->orderedExprs_->push_back( new Ast::Constant( @3, new Lang::String( $3, false ) ) );
	$$ = new Ast::CallExpr( @$,
													Ast::THE_FUNCTION_TeX,
													args );
}
| T_bangbang Expr
{
	$$ = $2;
	$$->immediate_ = true;
}
| CallExpr
| CurryCallExpr
| SuperCall
| SuperMemberReference
| T_speciallength
| '(' Expr ')'
{
	$$ = $2;
}
| '(' '-' Expr ')'
{
	$$ = new Ast::NegExpr( @$, @2, $3 );
}
| '(' '+' Expr ')'
{
	$$ = new Ast::RelativeExpr( @$, @2, $3 );
}
| '(' ')'
{
	$$ = new Ast::EmptyExpression( @$ );
}
| '(' Expr T_llthan InsertionSequence ')'
{
	std::list< Ast::Node * > * bracket = new std::list< Ast::Node * >( );

	size_t ** pos = new size_t * ( 0 );
	Ast::StateReference * dst = new Ast::LexiographicState( @2, strdup( Kernel::SEQUENTIAL_EXPR_VAR_ID ), new Kernel::Environment::LexicalKey * ( 0 ) );

	bracket->push_back( new Ast::IntroduceState( @3,
																							 strdup( Kernel::SEQUENTIAL_EXPR_VAR_ID ),
																							 $2,
																							 pos ) );
	for( std::list< Ast::Expression * >::const_iterator i = $4->begin( ); i != $4->end( ); ++i )
		{
			bracket->push_back( new Ast::Insertion( dst, *i ) );
		}
	bracket->push_back( new Ast::Freeze( @3, strdup( Kernel::SEQUENTIAL_EXPR_VAR_ID ), pos ) );
	$$ = new Ast::CodeBracket( @$, bracket );
}
| T_surrounding Expr
{
	$$ = new Ast::EvalOutsideExpr( @$, $2 );
}
| CodeBracket
| Function
| Class
| T_identifier
{
	Kernel::Environment::LexicalKey ** key = new Kernel::Environment::LexicalKey * ( 0 );
	$$ = new Ast::LexiographicVariable( @$, $1, key );
}
| T_atat Expr
{
	Ast::ArgListExprs * args = new Ast::ArgListExprs( false );
	Ast::EvalSymbolFunction * res = new Ast::EvalSymbolFunction( @$, $2 );
	res->push_exprs( args );
	$$ = new Ast::CallExpr( @$,
													RefCountPtr< const Lang::Function >( res ),
													args );
}
| T_dynamic_identifier
{
	$$ = new Ast::DynamicVariable( @$, $1 );
}
| '(' StateReference ')'
{
	$$ = new Ast::Peek( @$, $2 );
}
| MutateExpr
| Expr '.' T_identifier
{
	Ast::ArgListExprs * args = new Ast::ArgListExprs( false );
	Ast::MemberReferenceFunction * res = new Ast::MemberReferenceFunction( @$, $1, $3 );
	res->push_exprs( args );
	$$ = new Ast::CallExpr( @$,
													RefCountPtr< const Lang::Function >( res ),
													args );
}
| Expr '.' MethodIdentifier
{
	Ast::ArgListExprs * args = new Ast::ArgListExprs( false );
	Ast::PublicMethodReferenceFunction * res = new Ast::PublicMethodReferenceFunction( @$, $1, $3 );
	res->push_exprs( args );
	$$ = new Ast::CallExpr( @$,
													RefCountPtr< const Lang::Function >( res ),
													args );
}
| DynamicBinding
| '(' T_esc_continuation T_identifier Expr ')'
{
	$$ = new Ast::LetDynamicECExpr( @$, @3, $3, $4 );
}
| '(' T_esc_continue T_identifier Expr ')'
{
	Ast::ArgListExprs * args = new Ast::ArgListExprs( false );
	Ast::ContinueDynamicECFunction * res = new Ast::ContinueDynamicECFunction( @3, $3, $4 );
	res->push_exprs( args );
	$$ = new Ast::CallExpr( @$,
													RefCountPtr< const Lang::Function >( res ),
													args );
	/* This used to be immediate, but right now that seems utterly wrong!
	 * Imagine choosing between two continuations; then both continuations would require invokation before being "passed" to the <if> function.
	 * On the other hand, I can admit that it seems a bit uncanny to let the <if> function return the continuation invokations as thunks, not
	 * knowing when they will be forced...	But I don't think there's a choice here anyway; this expression can't be immediate.
	 */
}
| Expr '|' Expr
{
	$$ = new Ast::WithDynamicExpr( @$, $1, $3 );
}
| T_unionLeft ArgList T_unionRight
{
	$$ = new Ast::UnionExpr( @$, $2 );
}
| NamedLetExpr
| Expr T_minusminus T_cycle
{
	$$ = new Ast::CycleExpr( @3, $1 );
}
| Expr T_minusminus Expr
{
	$$ = new Ast::MinusMinusExpr( @2, $1, $3 );
}
| Expr T_plusplus Expr
{
	$$ = new Ast::PlusPlusExpr( @2, $1, $3 );
}
| Expr '&' Expr
{
	$$ = new Ast::AmpersandExpr( @2, $1, $3 );
}
| Expr '+' Expr
{
	$$ = new Ast::PlusExpr( @2, $1, $3 );
}
| Expr '-' Expr
{
	$$ = new Ast::MinusExpr( @2, $1, $3 );
}
| Expr T_angle Expr
{
	$$ = new Ast::AngleExpr( @2, $1, $3 );
}
| Expr T_ampersandMore Expr
{
	$$ = new Ast::AmpersandMoreExpr( @2, $1, $3 );
}
| Expr '*' Expr
{
	$$ = new Ast::StarExpr( @2, $1, $3 );
}
| Expr T_projection Expr
{
	$$ = new Ast::ProjectionExpr( @2, $1, $3 );
}
| Expr '/' Expr
{
	$$ = new Ast::SlashExpr( @2, $1, $3 );
}
| '~' Expr
{
	$$ = new Ast::NegExpr( @1, $2 );
}
| Expr T_compose Expr
{
	$$ = new Ast::ComposeExpr( @2, $1, $3 );
}
| Expr '<' Expr
{
	$$ = new Ast::LessExpr( @2, $1, $3 );
}
| Expr '>' Expr
{
	$$ = new Ast::GreaterExpr( @2, $1, $3 );
}
| Expr T_eqeq Expr
{
	$$ = new Ast::EqualExpr( @2, $1, $3 );
}
| Expr T_eqneq Expr
{
	$$ = new Ast::NotEqualExpr( @2, $1, $3 );
}
| Expr T_lesseq Expr
{
	$$ = new Ast::LessEqualExpr( @2, $1, $3 );
}
| Expr T_greatereq Expr
{
	$$ = new Ast::GreaterEqualExpr( @2, $1, $3 );
}
| T_not Expr
{
	$$ = new Ast::NotExpr( @1, $2 );
}
| Expr T_and Expr
{
	//	$$ = new Ast::AndExpr( @2, $1, $3 );

	Ast::ArgListExprs * args = new Ast::ArgListExprs( true );
	args->orderedExprs_->push_back( $1 );
	args->orderedExprs_->push_back( $3 );
	$$ = new Ast::CallExpr( @$,
													Lang::THE_FUNCTION_AND,
													args );
}
| Expr T_or Expr
{
	//	$$ = new Ast::OrExpr( @2, $1, $3 );

	Ast::ArgListExprs * args = new Ast::ArgListExprs( true );
	args->orderedExprs_->push_back( $1 );
	args->orderedExprs_->push_back( $3 );
	$$ = new Ast::CallExpr( @$,
													Lang::THE_FUNCTION_OR,
													args );
}
| Expr T_xor Expr
{
	$$ = new Ast::XorExpr( @2, $1, $3 );
}
| T_typename
{
	Kernel::Environment::LexicalKey ** key = new Kernel::Environment::LexicalKey * ( 0 );
	$$ = new Ast::LexiographicType( @$, $1, key );
}
;


DynamicBinding
: T_dynamic_identifier ':' Expr %prec T_dynamiccolon
{
	$$ = new Ast::DynamicBindingExpression( @$, $1, $3, new Kernel::Environment::LexicalKey * ( 0 ) );
}
| T_dynamic_identifier ':' T_dynamic Expr %prec T_dynamiccolon
{
	$$ = new Ast::DynamicBindingExpression( @$, $1,
																					new Ast::DynamicExpression( @4, $4 ),
																					new Kernel::Environment::LexicalKey * ( 0 ) );
}
| T_dynamic_state_identifier ':' StateReference	%prec T_dynamiccolon
{
	$$ = new Ast::DynamicStateBindingExpression( @$, @1, $1, $3 );
}
;

NamedLetExpr
: '[' T_let T_identifier '(' Formals ')' Expr ']'
{
	for( std::vector< Ast::Expression * >::const_iterator i = $5->defaultExprs_.begin( ); i != $5->defaultExprs_.end( ); ++i )
		{
			if( *i == 0 )
				{
					Ast::theAnalysisErrorsList.push_back( new Exceptions::ParserError( @5, strrefdup( "Formals without default value are not allowed in list of let bindings." ) ) );
				}
		}

	std::list< Ast::Node * > * bracket = new std::list< Ast::Node * >( );

	{
		Ast::ArgListExprs * args = new Ast::ArgListExprs( false );
		Ast::FunctionFunction * res = new Ast::FunctionFunction( @5, $5, $7, 0 );
		res->push_exprs( args );
		size_t ** pos = new size_t * ( 0 );
		bracket->push_back( new Ast::DefineVariable( @3,
																								 $3,
																								 new Ast::CallExpr( @$,
																																		RefCountPtr< const Lang::Function >( res ),
																																		args ),
																								 pos ) );
	}

	{
		Kernel::Environment::LexicalKey ** key = new Kernel::Environment::LexicalKey * ( 0 );
		bracket->push_back( new Ast::CallExpr( @$,
																					 new Ast::LexiographicVariable( @3, $3, key ),
																					 new Ast::ArgListExprs( true ) ) );
	}

	$$ = new Ast::CodeBracket( @$, bracket );
}
;


ConstantExceptStrings
: T_int
{
	$$ = new Ast::Constant( @1, new Lang::Integer( $1 ) );
}
| T_float
{
	$$ = new Ast::Constant( @1, new Lang::Float( $1 ) );
}
| T_length
{
	$$ = new Ast::Constant( @1, new Lang::Length( $1 ) );
}
| T_bool
{
	$$ = new Ast::Constant( @1, new Lang::Boolean( $1 ) );
}
| '\'' T_identifier
{
	$$ = new Ast::Constant( @1, new Lang::Symbol( $2 ) );
}
;

CodeBracket
: '{' Group '}'
{
	$$ = new Ast::CodeBracket( @$, $2 );
}
;

GroupElem
: Expr
{
	$$ = $1;	// Explicit upcast avoids bison warning.
}
| T_identifier ':' Expr
{
	size_t ** pos = new size_t * ( 0 );
	$$ = new Ast::DefineVariable( @1, $1, $3, pos );
}
| T_state_identifier ':' Expr
{
	size_t ** pos = new size_t * ( 0 );
	$$ = new Ast::IntroduceState( @1, $1, $3, pos );
}
| T_state_identifier ';'
{
	size_t ** pos = new size_t * ( 0 );
	$$ = new Ast::Freeze( @1, $1, pos );
}
| T_identifier ':' T_state_identifier ';'
{
	size_t ** posVar = new size_t * ( 0 );
	size_t ** posState = new size_t * ( 0 );
	$$ = new Ast::DefineVariable( @1, $1, new Ast::Freeze( @3, $3, posState ), posVar );
}
| T_dynamic T_dynamic_identifier Expr Expr
{
	$$ = new Ast::DynamicVariableDecl( @$, @2, $2, $3, $4 );
}
| T_dynamic T_dynamic_identifier Expr T_dynamic Expr
{
	$$ = new Ast::DynamicVariableDecl( @$, @2, $2, $3,
																		 new Ast::DynamicExpression( @5, $5 ) );
}
| T_dynamic T_dynamic_state_identifier StateReference
{
	$$ = new Ast::DynamicStateDecl( @$, @2, $2, $3, new size_t * ( 0 ) );
}
| Expr '.' T_identifier T_llthan InsertionSequence
{
	shapeserror( "MemberInsertionSequence not implemented" );
	//	$$ = new Ast::MemberInsertionSequence( @$, $1, $3, $5 );
}
| '(' '#' Expr ')' '.' T_identifier T_llthan InsertionSequence
{
	shapeserror( "ProtectedMemberInsertionSequence not implemented" );
	//	$$ = new Ast::ProtectedMemberInsertionSequence( @$, @2, $3, $6, $8 );
}
;

InsertionSequence
: Expr
{
	$$ = new std::list< Ast::Expression * >( );
	$$->push_back( $1 );
}
| InsertionSequence T_llthan Expr
{
	$$ = $1;
	$$->push_back( $3 );
}
;

OneOrMoreGroupElems
: GroupElem
{
	$$ = new list< Ast::Node * >( );
	$$->push_back( $1 );
}
| OneOrMoreGroupElems GroupElem
{
	$$ = $1;
	$$->push_back( $2 );
}
| StateReference T_llthan InsertionSequence
{
	$$ = new list< Ast::Node * >( );
	for( std::list< Ast::Expression * >::const_iterator i = $3->begin( ); i != $3->end( ); ++i )
		{
			$$->push_back( new Ast::Insertion( $1, *i ) );
		}
}
| OneOrMoreGroupElems StateReference T_llthan InsertionSequence
{
	$$ = $1;
	for( std::list< Ast::Expression * >::const_iterator i = $4->begin( ); i != $4->end( ); ++i )
		{
			$$->push_back( new Ast::Insertion( $2, *i ) );
		}
}
| T_splitLeft SplitFormals T_splitRight ':' Expr
{
	$$ = new list< Ast::Node * >( );
	size_t ** pos = new size_t * ( 0 );

	$5->immediate_ = true;
	$$->push_back( new Ast::DefineVariable( @5, $2->newSplitVarId( ), $5, pos ) );

	size_t orderedCount = 0;

	typedef typeof $2->exprs_ ListType;
	for( ListType::iterator i = $2->exprs_.begin( ); i != $2->exprs_.end( ); ++i )
		{
			i->second->setStruct( @5, pos );
			$$->push_back( i->first );
			if( i->second->isOrdered( ) )
				{
					++orderedCount;
				}
		}

	if( $2->sinkDefine_ != 0 )
		{
			$2->sinkExpr_->setStruct( @5, pos, orderedCount );
			$$->push_back( $2->sinkDefine_ );
		}
	else
		{
			$$->push_back( new Ast::AssertNoSinkNeeded( @2, orderedCount, @5, pos ) );
		}
}
| T_splitLeft SplitFormals T_splitRight Expr
{
	Ast::theAnalysisErrorsList.push_back( new Exceptions::ParserError( @4, strrefdup( "Expected ':'." ) ) );
	$$ = new list< Ast::Node * >( );
	$$->push_back( new Ast::ErrorExpression( @$ ) );
}
| OneOrMoreGroupElems T_splitLeft SplitFormals T_splitRight ':' Expr
{
	$$ = $1;
	size_t ** pos = new size_t * ( 0 );

	$6->immediate_ = true;
	$$->push_back( new Ast::DefineVariable( @6, $3->newSplitVarId( ), $6, pos ) );

	size_t orderedCount = 0;

	typedef typeof $3->exprs_ ListType;
	for( ListType::iterator i = $3->exprs_.begin( ); i != $3->exprs_.end( ); ++i )
		{
			i->second->setStruct( @6, pos );
			$$->push_back( i->first );
			if( i->second->isOrdered( ) )
				{
					++orderedCount;
				}
		}

	if( $3->sinkDefine_ != 0 )
		{
			$3->sinkExpr_->setStruct( @6, pos, orderedCount );
			$$->push_back( $3->sinkDefine_ );
		}
	else
		{
			$$->push_back( new Ast::AssertNoSinkNeeded( @3, orderedCount, @6, pos ) );
		}
}
| OneOrMoreGroupElems T_splitLeft SplitFormals T_splitRight Expr
{
	Ast::theAnalysisErrorsList.push_back( new Exceptions::ParserError( @4, strrefdup( "Expected ':'." ) ) );
	$$ = $1;
	$$->push_back( new Ast::ErrorExpression( @$ ) );
}
;

Group
:
{
	$$ = new list< Ast::Node * >( );
}
| OneOrMoreGroupElems
;


SuperMemberReference
: '(' '#' Expr ')' '.' T_identifier
{
	Ast::ArgListExprs * args = new Ast::ArgListExprs( false );
	Ast::ProtectedMemberReferenceFunction * res = new Ast::ProtectedMemberReferenceFunction( @$, @2, $3, @6, $6 );
	res->push_exprs( args );
	$$ = new Ast::CallExpr( @$,
													RefCountPtr< const Lang::Function >( res ),
													args );
}
;


MethodIdentifier
: T_identifier '#' T_identifier
{
	Kernel::Environment::LexicalKey ** key = new Kernel::Environment::LexicalKey * ( 0 );
	$$ = new Ast::MethodIdExpr( @$, new Ast::LexiographicVariable( @1, $1, key ), $3 );
}
;


SuperCall
: '[' '(' '#' Expr ')' '.' MethodIdentifier ArgList ']'
{
	Ast::ArgListExprs * args = new Ast::ArgListExprs( false );
	Ast::ProtectedMethodReferenceFunction * res = new Ast::ProtectedMethodReferenceFunction( @$, @3, $4, $7 );
	res->push_exprs( args );
	$$ = new Ast::CallExpr( @$,
													new Ast::CallExpr( @3,
																						 RefCountPtr< const Lang::Function >( res ),
																						 args ),
													$8 );
}
| '[' '(' '#' ')' '.' MethodIdentifier ArgList ']'
{
	Ast::ArgListExprs * args = new Ast::ArgListExprs( false );
	Ast::ProtectedMethodReferenceFunction * res = new Ast::ProtectedMethodReferenceFunction( @$, @3, 0, $6 );
	res->push_exprs( args );
	$$ = new Ast::CallExpr( @$,
													new Ast::CallExpr( @3,
																						 RefCountPtr< const Lang::Function >( res ),
																						 args ),
													$7 );
}
;


Class
: '[' T_class '(' Expr Formals ')' T_identifier '(' ListOfParentsWithInitargs ')'
	ClassModeList
	ClassSections
	']'
{
	DeleteOnExit< char > isaDeleter( $7 );
	if( strcmp( $7, "isa" ) != 0 )
		{
			Ast::theAnalysisErrorsList.push_back( new Exceptions::ParserError( @7, strrefdup( "Expected \"isa\"." ) ) );
		}
	if( ( $11 & Ast::CLASS_MODE_ABSTRACT ) != 0 && ( $11 & Ast::CLASS_MODE_FINAL ) != 0 )
		{
			Ast::theAnalysisErrorsList.push_back( new Exceptions::ParserError( @11, strrefdup( "Declaring a class both abstract and final is forbidden." ) ) );
		}

	Ast::ArgListExprs * args = new Ast::ArgListExprs( false );
	Ast::ClassFunction * res = new Ast::ClassFunction( @$, $4, $5, $9, $11, $12 );
	res->push_exprs( args );
	$$ = new Ast::CallExpr( @$,
													RefCountPtr< const Lang::Function >( res ),
													args );
}
;

ListOfParentsWithInitargs
: '(' Expr ArgList ')'
{
	$$ = new std::list< const Ast::CallExpr * >;
	$$->push_back( new Ast::CallExpr( @$, $2, $3 ) );
}
| ListOfParentsWithInitargs '(' Expr ArgList ')'
{
	$$ = $1;
	$$->push_back( new Ast::CallExpr( Ast::SourceLocation( @2, @5 ), $3, $4 ) );
}
;

ClassModeList
:
{
	$$ = 0;
}
| OneOrMoreClassModeSpecifiers
;

OneOrMoreClassModeSpecifiers
: ClassModeSpecifier
| OneOrMoreClassModeSpecifiers ClassModeSpecifier
{
	$$ = $1 | $2;
}
;

ClassModeSpecifier
: T_identifier
{
	DeleteOnExit< char > strDeleter( $1 );
	$$ = 0;
	if( strcmp( $1, "abstract" ) == 0 )
		{
			$$ = Ast::CLASS_MODE_ABSTRACT;
		}
	else if( strcmp( $1, "final" ) == 0 )
		{
			$$ = Ast::CLASS_MODE_FINAL;
		}
	else
		{
			Ast::theAnalysisErrorsList.push_back( new Exceptions::ParserError( @$, strrefdup( "This is not a valid class mode specifier" ) ) );
		}
}
;

ClassSections
:
{
	$$ = new std::list< Ast::ClassSection * >;
}
| OneOrMoreClassSections
;

OneOrMoreClassSections
: ClassSection
{
	$$ = new std::list< Ast::ClassSection * >;
	$$->push_back( $1 );
}
| OneOrMoreClassSections ClassSection
{
	$$ = $1;
	$$->push_back( $2 );
}
;

ClassSection
: '(' T_members MemberDeclarations ')'
{
	$$ = $3;
}
| '(' T_prepare Group ')'
{
	$$ = new Ast::PrepareSection( @$, $3 );
}
| '(' T_identifier MethodDeclarations ')'
{
	DeleteOnExit< char > accessSpecDeleter( $2 );
	unsigned int accessSpec = 0;
	if( strcmp( $2, "__methods__" ) == 0	)
		{
			accessSpec = Ast::MEMBER_ACCESS_PUBLIC_GET | Ast::MEMBER_ACCESS_PROTECTED_GET;
		}
	else if( strcmp( $2, "__abstract__" ) == 0	)
		{
			accessSpec = Ast::MEMBER_ACCESS_PUBLIC_GET | Ast::MEMBER_ACCESS_PROTECTED_GET | Ast::MEMBER_ABSTRACT;
		}
	else if( strcmp( $2, "__final__" ) == 0	)
		{
			accessSpec = Ast::MEMBER_ACCESS_PUBLIC_GET | Ast::MEMBER_ACCESS_PROTECTED_GET | Ast::MEMBER_FINAL;
		}
	else if( strcmp( $2, "__protected__" ) == 0 )
		{
			accessSpec = Ast::MEMBER_ACCESS_PROTECTED_GET;
		}
	else if( strcmp( $2, "__private__" ) == 0 )
		{
			/* OK, no change */
		}
	else
		{
			Ast::theAnalysisErrorsList.push_back( new Exceptions::ParserError( @2, strrefdup( "This is not a valid method access specifier." ) ) );
		}
	$3->addModeBits( accessSpec );
	$$ = $3;
}
| '(' T_abstract OrderedFormals ')'
{
	$$ = new Ast::AbstractSection( @$, $3 );
}
| '(' T_overrides Expr T_gr__ MethodDeclarations ')'
{
	$$ = new Ast::OverridesSection( $3, $5 );
}
;

MemberDeclarations
:
{
	$$ = new Ast::MemberSection;
}
| OneOrMoreMemberDeclarations
;

OneOrMoreMemberDeclarations
: MemberDeclaration
{
	$$ = new Ast::MemberSection;
	$$->push_back( $1 );
}
| OneOrMoreMemberDeclarations MemberDeclaration
{
	$$ = $1;
	$$->push_back( $2 );
}
;

MemberDeclaration
: '(' T_identifier Expr ')'
{
	$$ = new Ast::MemberDeclaration( @$, $2, $3, 0 );
}
| '(' T_identifier Expr MemberAccessList ')'
{
	$$ = new Ast::MemberDeclaration( @$, $2, $3, $4 );
}
;

MemberAccessList
: MemberAccessSpecifier
| MemberAccessList MemberAccessSpecifier
{
	$$ = $1 | $2;
}
;

MemberAccessSpecifier
: '.'
{
	$$ = Ast::MEMBER_ACCESS_PUBLIC_GET | Ast::MEMBER_ACCESS_PROTECTED_GET;
}
| T_llthan
{
	$$ = Ast::MEMBER_ACCESS_PUBLIC_GET | Ast::MEMBER_ACCESS_PUBLIC_INSERT | Ast::MEMBER_ACCESS_PROTECTED_GET | Ast::MEMBER_ACCESS_PROTECTED_INSERT;
}
| '(' '#' '.' ')'
{
	$$ = Ast::MEMBER_ACCESS_PROTECTED_GET;
}
| '(' '#' T_llthan ')'
{
	$$ = Ast::MEMBER_ACCESS_PROTECTED_GET | Ast::MEMBER_ACCESS_PROTECTED_INSERT;
}
| '^'
{
	$$ = Ast::MEMBER_TRANSFORMING;
}
;

MethodDeclarations
:
{
	$$ = new Ast::MemberSection;
}
| OneOrMoreMethodDeclarations
;

OneOrMoreMethodDeclarations
: MethodDeclaration
{
	$$ = new Ast::MemberSection;
	$$->push_back( $1 );
}
| OneOrMoreMethodDeclarations MethodDeclaration
{
	$$ = $1;
	$$->push_back( $2 );
}
;

MethodDeclaration
: '(' T_identifier Expr ')'
{
	$$ = new Ast::MemberDeclaration( @$, $2, $3, Ast::MEMBER_CONST | Ast::MEMBER_METHOD );
}
| '(' '[' T_identifier Formals ']' FunctionModeList GroupElem ')'
{
	Ast::Expression * body = dynamic_cast< Ast::Expression * >( $7 );
	if( body == 0 )
		{
			std::list< Ast::Node * > * bracket = new std::list< Ast::Node * >( );
			bracket->push_back( $7 );
			body = new Ast::CodeBracket( @7, bracket );
		}
	Ast::ArgListExprs * args = new Ast::ArgListExprs( false );
	Ast::FunctionFunction * res = new Ast::FunctionFunction( @$, $4, body, $6 );
	res->push_exprs( args );
	$$ = new Ast::MemberDeclaration( @$, $3, new Ast::CallExpr( @$,
																															RefCountPtr< const Lang::Function >( res ),
																															args ),
																	 Ast::MEMBER_CONST | Ast::MEMBER_METHOD | ( (($6 & Ast::FUNCTION_TRANSFORMING) != 0) ? Ast::MEMBER_TRANSFORMING : 0 ) );
}
;

FunctionModeList
:
{
	$$ = 0;
}
| OneOrMoreFunctionModeSpecifiers
;

OneOrMoreFunctionModeSpecifiers
: FunctionModeSpecifier
| OneOrMoreFunctionModeSpecifiers FunctionModeSpecifier
{
	$$ = $1 | $2;
}
;

FunctionModeSpecifier
: '^'
{
	$$ = Ast::FUNCTION_TRANSFORMING;
}
| '!'
{
	$$ = Ast::FUNCTION_PROCEDURAL;
}
;

Split
: T_split Expr
{
	$$ = $2;
}
;

%%

/* The closing %% above marks the end of the Rules section and the beginning
 * of the User Subroutines section. All text from here to the end of the
 * file is copied verbatim to the end of the generated y.tab.c file.
 * This section is where you put definitions of helper functions.
 */

