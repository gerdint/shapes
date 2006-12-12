#include "globals.h"
#include "metapdfcore.h"
#include "metapdfastfun.h"
#include "metapdfastexprs.h"

using namespace MetaPDF;

RefCountPtr< const Lang::BinaryOperatorFunction >
Lang::THE_OPERATOR_MINUSMINUS( new Lang::BinaryOperatorFunction( new Ast::MinusMinusExpr( Ast::SourceLocation( ), new Ast::DummyExpression, new Ast::DummyExpression ),
								       "(--)" ) );
RefCountPtr< const Lang::BinaryOperatorFunction >
Lang::THE_OPERATOR_PLUSPLUS( new Lang::BinaryOperatorFunction( new Ast::PlusPlusExpr( Ast::SourceLocation( ), new Ast::DummyExpression, new Ast::DummyExpression ),
								     "(++)" ) );
RefCountPtr< const Lang::BinaryOperatorFunction >
Lang::THE_OPERATOR_AMPERSAND( new Lang::BinaryOperatorFunction( new Ast::AmpersandExpr( Ast::SourceLocation( ), new Ast::DummyExpression, new Ast::DummyExpression ),
								      "(&)" ) );
RefCountPtr< const Lang::BinaryOperatorFunction >
Lang::THE_OPERATOR_PLUS( new Lang::BinaryOperatorFunction( new Ast::PlusExpr( Ast::SourceLocation( ), new Ast::DummyExpression, new Ast::DummyExpression ),
								 "(+)" ) );
RefCountPtr< const Lang::BinaryOperatorFunction >
Lang::THE_OPERATOR_MINUS( new Lang::BinaryOperatorFunction( new Ast::MinusExpr( Ast::SourceLocation( ), new Ast::DummyExpression, new Ast::DummyExpression ),
								  "(-)" ) );
RefCountPtr< const Lang::BinaryOperatorFunction >
Lang::THE_OPERATOR_ANGLE( new Lang::BinaryOperatorFunction( new Ast::AngleExpr( Ast::SourceLocation( ), new Ast::DummyExpression, new Ast::DummyExpression ),
								  "(/_)" ) );
RefCountPtr< const Lang::BinaryOperatorFunction >
Lang::THE_OPERATOR_STAR( new Lang::BinaryOperatorFunction( new Ast::StarExpr( Ast::SourceLocation( ), new Ast::DummyExpression, new Ast::DummyExpression ),
								 "(*)" ) );
RefCountPtr< const Lang::BinaryOperatorFunction >
Lang::THE_OPERATOR_SLASH( new Lang::BinaryOperatorFunction( new Ast::SlashExpr( Ast::SourceLocation( ), new Ast::DummyExpression, new Ast::DummyExpression ),
								  "(/)" ) );
RefCountPtr< const Lang::BinaryOperatorFunction >
Lang::THE_OPERATOR_PROJECTION( new Lang::BinaryOperatorFunction( new Ast::ProjectionExpr( Ast::SourceLocation( ), new Ast::DummyExpression, new Ast::DummyExpression ),
								       "(*/)" ) );
RefCountPtr< const Lang::BinaryOperatorFunction >
Lang::THE_OPERATOR_COMPOSE( new Lang::BinaryOperatorFunction( new Ast::ComposeExpr( Ast::SourceLocation( ), new Ast::DummyExpression, new Ast::DummyExpression ),
								    "(())" ) );
RefCountPtr< const Lang::UnaryOperatorFunction >
Lang::THE_OPERATOR_NEG( new Lang::UnaryOperatorFunction( new Ast::NegExpr( Ast::SourceLocation( ), new Ast::DummyExpression ),
							       "(~)" ) );
RefCountPtr< const Lang::BinaryOperatorFunction >
Lang::THE_OPERATOR_LESS( new Lang::BinaryOperatorFunction( new Ast::LessExpr( Ast::SourceLocation( ), new Ast::DummyExpression, new Ast::DummyExpression ),
								 "(<)" ) );
RefCountPtr< const Lang::BinaryOperatorFunction >
Lang::THE_OPERATOR_GREATER( new Lang::BinaryOperatorFunction( new Ast::GreaterExpr( Ast::SourceLocation( ), new Ast::DummyExpression, new Ast::DummyExpression ),
								    "(>)" ) );
RefCountPtr< const Lang::BinaryOperatorFunction >
Lang::THE_OPERATOR_EQEQ( new Lang::BinaryOperatorFunction( new Ast::EqualExpr( Ast::SourceLocation( ), new Ast::DummyExpression, new Ast::DummyExpression ),
								 "(==)" ) );
RefCountPtr< const Lang::BinaryOperatorFunction >
Lang::THE_OPERATOR_EQNEQ( new Lang::BinaryOperatorFunction( new Ast::NotEqualExpr( Ast::SourceLocation( ), new Ast::DummyExpression, new Ast::DummyExpression ),
								  "(<>)" ) );
RefCountPtr< const Lang::BinaryOperatorFunction >
Lang::THE_OPERATOR_LESSEQ( new Lang::BinaryOperatorFunction( new Ast::LessEqualExpr( Ast::SourceLocation( ), new Ast::DummyExpression, new Ast::DummyExpression ),
								   "(<=)" ) );
RefCountPtr< const Lang::BinaryOperatorFunction >
Lang::THE_OPERATOR_GREATEREQ( new Lang::BinaryOperatorFunction( new Ast::GreaterEqualExpr( Ast::SourceLocation( ), new Ast::DummyExpression, new Ast::DummyExpression ),
								      "(>=)" ) );
RefCountPtr< const Lang::UnaryOperatorFunction >
Lang::THE_OPERATOR_NOT( new Lang::UnaryOperatorFunction( new Ast::NotExpr( Ast::SourceLocation( ), new Ast::DummyExpression ),
							       "(not)" ) );
RefCountPtr< const Lang::CoreFunction > Lang::THE_FUNCTION_AND( new Lang::Core_and( "and" ) );
RefCountPtr< const Lang::CoreFunction > Lang::THE_FUNCTION_OR( new Lang::Core_or( "or" ) );
RefCountPtr< const Lang::BinaryOperatorFunction >
Lang::THE_OPERATOR_XOR( new Lang::BinaryOperatorFunction( new Ast::XorExpr( Ast::SourceLocation( ), new Ast::DummyExpression, new Ast::DummyExpression ),
								"(xor)" ) );
