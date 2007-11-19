#include "globals.h"
#include "shapescore.h"
#include "astfun.h"
#include "astexprs.h"

using namespace Shapes;


namespace Shapes
{
	namespace Kernel
	{

	class AndContinuation : public Kernel::Continuation
	{
		RefCountPtr< std::vector< Kernel::VariableHandle > > arguments_;
		std::vector< Kernel::VariableHandle >::const_iterator next_;
		Kernel::ContRef cont_;
	public:
		AndContinuation( const RefCountPtr< std::vector< Kernel::VariableHandle > > & arguments, const std::vector< Kernel::VariableHandle >::const_iterator & next, Kernel::ContRef cont, const Ast::SourceLocation & traceLoc )
			: Kernel::Continuation( traceLoc ), arguments_( arguments ), next_( next ), cont_( cont )
		{ }
		virtual ~AndContinuation( ) { }
		virtual void takeValue( const RefCountPtr< const Lang::Value > & val, Kernel::EvalState * evalState, bool dummy ) const
		{
			if( ! Helpers::down_cast< const Lang::Boolean >( val, traceLoc_ )->val_ )
				{
					cont_->takeValue( Lang::THE_FALSE,
													 evalState );
					return;
				}
			
			std::vector< Kernel::VariableHandle >::const_iterator nextNext = next_;
			++nextNext;
			if( nextNext == arguments_->end( ) )
				{
					cont_->takeHandle( *next_, evalState );
					return;					
				}

			Kernel::ContRef newCont = Kernel::ContRef( new Kernel::AndContinuation( arguments_, nextNext, cont_, traceLoc_ ) );
			evalState->cont_ = newCont;
			newCont->takeHandle( *next_, evalState );
		}
		virtual void backTrace( std::list< Kernel::Continuation::BackTraceElem > * trace ) const
		{
			trace->push_front( Kernel::Continuation::BackTraceElem( this, "and" ) );
			cont_->backTrace( trace );
		}
		virtual void gcMark( Kernel::GCMarkedSet & marked )
		{
			typedef typeof next_ I;
			for( I i = next_; i != arguments_->end( ); ++i )
				{
					const_cast< Kernel::Variable * >( i->getPtr( ) )->gcMark( marked );
				}
			cont_->gcMark( marked );
		}
	};

	class OrContinuation : public Kernel::Continuation
	{
		RefCountPtr< std::vector< Kernel::VariableHandle > > arguments_;
		std::vector< Kernel::VariableHandle >::const_iterator next_;
		Kernel::ContRef cont_;
	public:
		OrContinuation( const RefCountPtr< std::vector< Kernel::VariableHandle > > & arguments, const std::vector< Kernel::VariableHandle >::const_iterator & next, Kernel::ContRef cont, const Ast::SourceLocation & traceLoc )
			: Kernel::Continuation( traceLoc ), arguments_( arguments ), next_( next ), cont_( cont )
		{ }
		virtual ~OrContinuation( ) { }
		virtual void takeValue( const RefCountPtr< const Lang::Value > & val, Kernel::EvalState * evalState, bool dummy ) const
		{
			if( Helpers::down_cast< const Lang::Boolean >( val, traceLoc_ )->val_ )
				{
					cont_->takeValue( Lang::THE_TRUE,
													 evalState );
					return;
				}
			
			std::vector< Kernel::VariableHandle >::const_iterator nextNext = next_;
			++nextNext;
			if( nextNext == arguments_->end( ) )
				{
					cont_->takeHandle( *next_, evalState );
					return;					
				}

			Kernel::ContRef newCont = Kernel::ContRef( new Kernel::OrContinuation( arguments_, nextNext, cont_, traceLoc_ ) );
			evalState->cont_ = newCont;
			newCont->takeHandle( *next_, evalState );
		}
		virtual void backTrace( std::list< Kernel::Continuation::BackTraceElem > * trace ) const
		{
			trace->push_front( Kernel::Continuation::BackTraceElem( this, "or" ) );
			cont_->backTrace( trace );
		}
		virtual void gcMark( Kernel::GCMarkedSet & marked )
		{
			typedef typeof next_ I;
			for( I i = next_; i != arguments_->end( ); ++i )
				{
					const_cast< Kernel::Variable * >( i->getPtr( ) )->gcMark( marked );
				}
			cont_->gcMark( marked );
		}
	};

	}
}


namespace Shapes
{
	namespace Lang
	{

		class Core_and : public Lang::CoreFunction
		{
		public:
			Core_and( const char * title )
				: CoreFunction( title, new Kernel::EvaluatedFormals( title, false ) )
			{ }
			virtual void
			call( Kernel::EvalState * evalState, Kernel::Arguments & args, const Ast::SourceLocation & callLoc ) const
			{
				if( args.size( ) == 0 )
					{
						Kernel::ContRef cont = evalState->cont_;
						cont->takeValue( THE_TRUE, evalState );
						return;
					}
				
				if( args.size( ) == 1 )
					{
						Kernel::ContRef cont = evalState->cont_;
						cont->takeHandle( args.getHandle( 0 ), evalState );
						return;
					}

				typedef typeof *(args.getVariables( )) ListType;
				ListType::const_iterator next = args.getVariables( )->begin( );
				++next;
				evalState->cont_ = Kernel::ContRef( new Kernel::AndContinuation( args.getVariables( ), next, evalState->cont_, callLoc ) );
				Kernel::ContRef cont = evalState->cont_;
				cont->takeHandle( args.getHandle( 0 ), evalState );
			}
		};

		class Core_or : public Lang::CoreFunction
		{
		public:
			Core_or( const char * title )
				: CoreFunction( title, new Kernel::EvaluatedFormals( title, false ) )
			{ }
			virtual void
			call( Kernel::EvalState * evalState, Kernel::Arguments & args, const Ast::SourceLocation & callLoc ) const
			{
				if( args.size( ) == 0 )
					{
						Kernel::ContRef cont = evalState->cont_;
						cont->takeValue( Lang::THE_FALSE, evalState );
						return;
					}
				
				if( args.size( ) == 1 )
					{
						Kernel::ContRef cont = evalState->cont_;
						cont->takeHandle( args.getHandle( 0 ),
															evalState );
						return;
					}
				
				typedef typeof *(args.getVariables( )) ListType;
				ListType::const_iterator next = args.getVariables( )->begin( );
				++next;
				evalState->cont_ = Kernel::ContRef( new Kernel::OrContinuation( args.getVariables( ), next, evalState->cont_, callLoc ) );
				Kernel::ContRef cont = evalState->cont_;
				cont->takeHandle( args.getHandle( 0 ), evalState );
			}
		};

	}
}


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
RefCountPtr< const Lang::BinaryOperatorFunction >
Lang::THE_OPERATOR_XOR( new Lang::BinaryOperatorFunction( new Ast::XorExpr( Ast::SourceLocation( ), new Ast::DummyExpression, new Ast::DummyExpression ),
																													"(xor)" ) );

RefCountPtr< const Lang::CoreFunction > Lang::THE_FUNCTION_AND( new Lang::Core_and( "and" ) );
RefCountPtr< const Lang::CoreFunction > Lang::THE_FUNCTION_OR( new Lang::Core_or( "or" ) );
