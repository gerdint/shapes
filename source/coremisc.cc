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

#include <cmath>

#include "shapescore.h"
#include "globals.h"
#include "shapesexceptions.h"
#include "consts.h"
#include "simplepdfi.h"
#include "astfun.h"
#include "continuations.h"
#include "multipage.h"
#include "debuglog.h"

#include <iostream>
#include <sstream>
#include <fstream>
#include <vector>
#include <stdio.h>
#include <sys/time.h>

using namespace Shapes;


namespace Shapes
{
	namespace Lang
	{
		class NullFunction : public Lang::CoreFunction
		{
		public:
			NullFunction( const char * title ) : CoreFunction( title ) { }
			virtual void call( Kernel::EvalState * evalState, Kernel::Arguments & args, const Ast::SourceLocation & callLoc ) const
			{
				Kernel::ContRef cont = evalState->cont_;
				cont->takeValue( Lang::THE_VOID,
												 evalState );
			}
		};

		class Core_identity : public Lang::CoreFunction
		{
		public:
			Core_identity( const char * title ) : CoreFunction( title ) { }
			virtual void call( Kernel::EvalState * evalState, Kernel::Arguments & args, const Ast::SourceLocation & callLoc ) const
			{
				const size_t ARITY = 1;
				CHECK_ARITY( args, ARITY, title_ );

				Kernel::ContRef cont = evalState->cont_;
				cont->takeHandle( args.getHandle( 0 ),
													evalState );
			}
		};

		class Core_typeof : public Lang::CoreFunction
		{
		public:
			Core_typeof( const char * title ) : CoreFunction( title ) { }
			virtual void call( Kernel::EvalState * evalState, Kernel::Arguments & args, const Ast::SourceLocation & callLoc ) const
			{
				const size_t ARITY = 1;
				CHECK_ARITY( args, ARITY, title_ );

				Kernel::ContRef cont = evalState->cont_;
				cont->takeValue( args.getValue( 0 )->getClass( ),
												 evalState );
			}
		};

		class Core_error : public Lang::CoreFunction
		{
		public:
			Core_error( const char * title ) : CoreFunction( title ) { }
			virtual void call( Kernel::EvalState * evalState, Kernel::Arguments & args, const Ast::SourceLocation & callLoc ) const
			{
				const size_t ARITY = 1;
				CHECK_ARITY( args, ARITY, title_ );

				throw Exceptions::UserError( Helpers::down_cast_CoreArgument< const Lang::String >( title_, args, 0, callLoc )->val_ );
			}
		};

		class Core_show : public Lang::CoreFunction
		{
		public:
			Core_show( const char * title ) : CoreFunction( title ) { }
			virtual void
			call( Kernel::EvalState * evalState, Kernel::Arguments & args, const Ast::SourceLocation & callLoc ) const
			{
				std::ostringstream oss;
				for( size_t i = 0; i != args.size( ); ++i )
					{
						args.getValue( i )->show( oss );
					}
				Kernel::ContRef cont = evalState->cont_;
				cont->takeValue( RefCountPtr< const Lang::Value >( new Lang::String( strrefdup( oss ) ) ),
												 evalState );
			}
		};

		class Core_typename : public Lang::CoreFunction
		{
		public:
			Core_typename( const char * title ) : CoreFunction( title ) { }
			virtual void
			call( Kernel::EvalState * evalState, Kernel::Arguments & args, const Ast::SourceLocation & callLoc ) const
			{
				const size_t ARITY = 1;
				CHECK_ARITY( args, ARITY, title_ );

				Kernel::ContRef cont = evalState->cont_;
				cont->takeValue( RefCountPtr< const Lang::Value >( new Lang::String( args.getValue( 0 )->getTypeName( ) ) ),
												 evalState );
			}
		};

		class Core_debuglog_before : public Lang::CoreFunction
		{
		public:
			Core_debuglog_before( const char * title )
				: CoreFunction( title, new Kernel::EvaluatedFormals( "< core function debuglog_before >" ) )
			{
				formals_->appendEvaluatedCoreFormal( "msg", Kernel::THE_SLOT_VARIABLE, true );
				formals_->appendEvaluatedCoreFormal( "result", Kernel::THE_SLOT_VARIABLE, false );
			}
			virtual void
			call( Kernel::EvalState * evalState, Kernel::Arguments & args, const Ast::SourceLocation & callLoc ) const
			{
				args.applyDefaults( );

				args.getValue( 0 )->show( Kernel::theDebugLog.os( ) );
				Kernel::theDebugLog.os( ) << std::flush ;

				Kernel::ContRef cont = evalState->cont_;
				cont->takeHandle( args.getHandle( 1 ),
													evalState );
			}
		};

		class Core_debuglog_after : public Lang::CoreFunction
		{
		public:
			Core_debuglog_after( const char * title )
				: CoreFunction( title, new Kernel::EvaluatedFormals( "< core function debuglog_after >" ) )
			{
				formals_->appendEvaluatedCoreFormal( "msg", Kernel::THE_SLOT_VARIABLE, true );
				formals_->appendEvaluatedCoreFormal( "result", Kernel::THE_SLOT_VARIABLE, true );
			}
			virtual void
			call( Kernel::EvalState * evalState, Kernel::Arguments & args, const Ast::SourceLocation & callLoc ) const
			{
				args.applyDefaults( );

				args.getValue( 0 )->show( Kernel::theDebugLog.os( ) );
				Kernel::theDebugLog.os( ) << std::flush ;

				Kernel::ContRef cont = evalState->cont_;
				cont->takeValue( args.getValue( 1 ),
													evalState );
			}
		};

		class Core_if : public Lang::CoreFunction
		{
		public:
			Core_if( const char * title )
				: CoreFunction( title, new Kernel::EvaluatedFormals( "< core function if >" ) )
			{
				formals_->appendEvaluatedCoreFormal( "predicate", Kernel::THE_SLOT_VARIABLE, true );
				formals_->appendEvaluatedCoreFormal( "consequence", Kernel::THE_SLOT_VARIABLE, false );
				formals_->appendEvaluatedCoreFormal( "alternative", Kernel::VariableHandle( new Kernel::Variable( Lang::THE_VOID ) ), false );
			}

			virtual void
			call( Kernel::EvalState * evalState, Kernel::Arguments & args, const Ast::SourceLocation & callLoc ) const
			{
				args.applyDefaults( );

				evalState->cont_ = Kernel::ContRef( new Kernel::IfContinuation( args.getHandle( 1 ), args.getHandle( 2 ), evalState->cont_, callLoc ) );

				Kernel::ContRef cont = evalState->cont_;
				cont->takeHandle( args.getHandle( 0 ), evalState );
			}
		};

		class Core_memoryinfo : public Lang::CoreFunction
		{
		public:
			Core_memoryinfo( const char * title ) : CoreFunction( title ) { }
			virtual void
			call( Kernel::EvalState * evalState, Kernel::Arguments & args, const Ast::SourceLocation & callLoc ) const
			{
				const size_t ARITY = 0;
				CHECK_ARITY( args, ARITY, title_ );
				std::cerr << "Environments:	alive: " << Kernel::Environment::liveCount << "	of total: " << Kernel::Environment::createdCount
									<< "	(" << 100 * static_cast< double >( Kernel::Environment::liveCount ) / static_cast< double >( Kernel::Environment::createdCount ) << "%)" << std::endl ;
				Kernel::ContRef cont = evalState->cont_;
				cont->takeHandle( Kernel::THE_SLOT_VARIABLE,
													evalState );
			}
		};

		class Core_rectangle : public Lang::CoreFunction
		{
		public:
			Core_rectangle( const char * title ) : CoreFunction( title ) { }
			virtual void
			call( Kernel::EvalState * evalState, Kernel::Arguments & args, const Ast::SourceLocation & callLoc ) const
			{
				const size_t ARITY = 2;
				CHECK_ARITY( args, ARITY, title_ );

				typedef typeof args ListType;

				typedef const Lang::Coords2D ArgType;

				RefCountPtr< ArgType > arg1 = Helpers::down_cast_CoreArgument< ArgType >( title_, args, 0, callLoc );
				RefCountPtr< ArgType > arg2 = Helpers::down_cast_CoreArgument< ArgType >( title_, args, 1, callLoc );

				Lang::ElementaryPath2D * res = new Lang::ElementaryPath2D;

				res->push_back( new Concrete::PathPoint2D( arg1->x_.get( ), arg1->y_.get( ) ) );
				res->push_back( new Concrete::PathPoint2D( arg2->x_.get( ), arg1->y_.get( ) ) );
				res->push_back( new Concrete::PathPoint2D( arg2->x_.get( ), arg2->y_.get( ) ) );
				res->push_back( new Concrete::PathPoint2D( arg1->x_.get( ), arg2->y_.get( ) ) );
				res->close( );

				Kernel::ContRef cont = evalState->cont_;
				cont->takeValue( Kernel::ValueRef( res ),
												 evalState );
			}
		};

		class Core_hot : public Lang::CoreFunction
		{
		public:
			Core_hot( const char * title )
				: CoreFunction( title, new Kernel::EvaluatedFormals( "< core function hot >", true ) )
			{
				formals_->appendEvaluatedCoreFormal( "init", Kernel::THE_SLOT_VARIABLE );
				formals_->appendEvaluatedCoreFormal( "tackon", Kernel::THE_SLOT_VARIABLE );
				formals_->appendEvaluatedCoreFormal( "freeze", Kernel::VariableHandle( new Kernel::Variable( Lang::THE_IDENTITY ) ) );
			}

			virtual void
			call( Kernel::EvalState * evalState, Kernel::Arguments & args, const Ast::SourceLocation & callLoc ) const
			{
				args.applyDefaults( );

				Kernel::ContRef cont = evalState->cont_;
				cont->takeValue( Kernel::ValueRef( new Lang::HotTriple
																					 ( args.getValue( 0 ),
																						 Helpers::down_cast_CoreArgument< const Lang::Function >( title_, args, 1, callLoc ),
																						 Helpers::down_cast_CoreArgument< const Lang::Function >( title_, args, 2, callLoc ) ) ),
												 evalState );
			}
		};

		class Core_ampersand_dynamic : public Lang::CoreFunction
		{
		public:
			Core_ampersand_dynamic( const char * title )
				: CoreFunction( title, new Kernel::EvaluatedFormals( "< core function (&) for dynamic bindings >", true ) )
			{
			}

			virtual void
			call( Kernel::EvalState * evalState, Kernel::Arguments & args, const Ast::SourceLocation & callLoc ) const
			{
				//				args.applyDefaults( );
				/*
				 * Here, we should check that there are no named arguments, and that there are no states being passed...
				 */

				RefCountPtr< const Lang::DynamicBindings > res = RefCountPtr< const Lang::DynamicBindings >( new Lang::DynamicBindingsNull( ) );

				for( size_t i = 0; i != args.size( ); ++i )
					{
						res = RefCountPtr< const Lang::DynamicBindings >
							( new Lang::DynamicBindingsPair( Helpers::down_cast_CoreArgument< const Lang::DynamicBindings >( title_, args, args.size( ) - 1 - i, callLoc, true ),
																							 res ) );
					}

				Kernel::ContRef cont = evalState->cont_;
				cont->takeValue( res,
												 evalState );
			}
		};

		class Core_nextpagenumber : public Lang::CoreFunction
		{
		public:
			Core_nextpagenumber( const char * title )
				: CoreFunction( title, new Kernel::EvaluatedFormals( title, true ) )
			{
				formals_->appendCoreStateFormal( "catalog" );
			}

			virtual void
			call( Kernel::EvalState * evalState, Kernel::Arguments & args, const Ast::SourceLocation & callLoc ) const
			{
				args.applyDefaults( );

				typedef Kernel::WarmCatalog StateType;
				StateType * state = Helpers::down_cast_CoreState< StateType >( title_, args, 0, callLoc );

				Kernel::ContRef cont = evalState->cont_;
				cont->takeValue( Kernel::ValueRef( new Lang::Integer( state->getNextPageNumber( ) ) ),
												 evalState );
			}
		};

		class Core_nextpagelabel : public Lang::CoreFunction
		{
		public:
			Core_nextpagelabel( const char * title )
				: CoreFunction( title, new Kernel::EvaluatedFormals( title, true ) )
			{
				formals_->appendCoreStateFormal( "catalog" );
			}

			void
			call( Kernel::EvalState * evalState, Kernel::Arguments & args, const Ast::SourceLocation & callLoc ) const
			{
				args.applyDefaults( );

				typedef Kernel::WarmCatalog StateType;
				StateType * state = Helpers::down_cast_CoreState< StateType >( title_, args, 0, callLoc );

				Kernel::ContRef cont = evalState->cont_;
				cont->takeValue( Kernel::ValueRef( new Lang::String( state->getNextPageLabel( ) ) ),
												 evalState );
			}
		};

		class Core_setpagelabel : public Lang::CoreFunction
		{
		public:
			Core_setpagelabel( const char * title )
				: CoreFunction( title, new Kernel::EvaluatedFormals( title, true ) )
			{
				formals_->appendCoreStateFormal( "catalog" );

				formals_->appendEvaluatedCoreFormal( "prefix", Kernel::THE_VOID_VARIABLE );
				formals_->appendEvaluatedCoreFormal( "style", Kernel::THE_VOID_VARIABLE );
				formals_->appendEvaluatedCoreFormal( "number", Kernel::THE_VOID_VARIABLE );
			}

			virtual void
			call( Kernel::EvalState * evalState, Kernel::Arguments & args, const Ast::SourceLocation & callLoc ) const
			{
				args.applyDefaults( );

				typedef Kernel::WarmCatalog StateType;
				StateType * state = Helpers::down_cast_CoreState< StateType >( title_, args, 0, callLoc );

				size_t argsi = 0;
				typedef const Lang::String PrefixValType;
				RefCountPtr< PrefixValType > prefixVal = Helpers::down_cast_CoreArgument< PrefixValType >( title_, args, argsi, callLoc, true );
				RefCountPtr< const char > prefix = state->getNextPagePrefix( );
				if( prefixVal != NullPtr< PrefixValType >( ) )
					{
						prefix = prefixVal->val_;
					}

				++argsi;
				typedef const Lang::Symbol StyleType;
				RefCountPtr< StyleType > style = Helpers::down_cast_CoreArgument< StyleType >( title_, args, 1, callLoc, true );

				static Lang::Symbol STYLE_none( "none" );
				static Lang::Symbol STYLE_decimal( "decimal" );
				static Lang::Symbol STYLE_ROMAN( "ROMAN" );
				static Lang::Symbol STYLE_roman( "roman" );
				static Lang::Symbol STYLE_ALPHABET( "ALPHABET" );
				static Lang::Symbol STYLE_alphabet( "alphabet" );

				Kernel::WarmCatalog::PageLabelEntry::Style styleVal;
				if( style == NullPtr< StyleType >( ) )
					{
						styleVal = state->getNextPageStyle( );
					}
				else
					{
						if( *style == STYLE_none )
							{
								styleVal = Kernel::WarmCatalog::PageLabelEntry::NONE;
							}
						else if( *style == STYLE_decimal )
							{
								styleVal = Kernel::WarmCatalog::PageLabelEntry::DECIMAL;
							}
						else if( *style == STYLE_ROMAN )
							{
								styleVal = Kernel::WarmCatalog::PageLabelEntry::ROMAN;
							}
						else if( *style == STYLE_roman )
							{
								styleVal = Kernel::WarmCatalog::PageLabelEntry::rOMAN;
							}
						else if( *style == STYLE_ALPHABET )
							{
								styleVal = Kernel::WarmCatalog::PageLabelEntry::ALPHABET;
							}
						else if( *style == STYLE_alphabet )
							{
								styleVal = Kernel::WarmCatalog::PageLabelEntry::aLPHABET;
							}
						else
							{
								std::ostringstream oss;
								oss << "Valid page label styles are the symbols { "
										<< STYLE_none.name( ).getPtr( ) << ", "
										<< STYLE_decimal.name( ).getPtr( ) << ", "
										<< STYLE_ROMAN.name( ).getPtr( ) << ", "
										<< STYLE_roman.name( ).getPtr( ) << ", "
										<< STYLE_alphabet.name( ).getPtr( ) << ", "
										<< STYLE_ALPHABET.name( ).getPtr( )
										<< " }." ;
								throw Exceptions::CoreOutOfRange( title_, args, argsi, strrefdup( oss ) );
							}
					}

				++argsi;
				typedef const Lang::Integer NumberType;
				RefCountPtr< NumberType > numberVal = Helpers::down_cast_CoreArgument< NumberType >( title_, args, argsi, callLoc, true );
				size_t number;
				if( numberVal != NullPtr< NumberType >( ) )
					{
						if( numberVal->val_ < 1 )
							{
								throw Exceptions::CoreOutOfRange( title_, args, argsi, "PDF only allows strictly positive page numbers." );
							}
						number = static_cast< size_t >( numberVal->val_ );
					}
				else
					{
						number = state->getNextPageNumber( );
					}

				state->setLabel( prefix, styleVal, number );

				Kernel::ContRef cont = evalState->cont_;
				cont->takeHandle( Kernel::THE_SLOT_VARIABLE,
													evalState );
			}
		};

		class Core_setbboxgroup : public Lang::CoreFunction
		{
		public:
			Core_setbboxgroup( const char * title )
				: CoreFunction( title, new Kernel::EvaluatedFormals( title, true ) )
			{
				formals_->appendCoreStateFormal( "catalog" );

				formals_->appendEvaluatedCoreFormal( "key", Kernel::THE_VOID_VARIABLE );
			}

			virtual void
			call( Kernel::EvalState * evalState, Kernel::Arguments & args, const Ast::SourceLocation & callLoc ) const
			{
				args.applyDefaults( );

				typedef Kernel::WarmCatalog StateType;
				StateType * state = Helpers::down_cast_CoreState< StateType >( title_, args, 0, callLoc );

				state->setBBoxGroup( Helpers::down_cast_CoreArgument< const Lang::Symbol >( title_, args, 0, callLoc, true ) );

				Kernel::ContRef cont = evalState->cont_;
				cont->takeHandle( Kernel::THE_SLOT_VARIABLE,
													evalState );
			}
		};

		class Core_locate : public Lang::CoreFunction
		{
		public:
			Core_locate( const char * title ) : CoreFunction( title ) { }
			virtual void call( Kernel::EvalState * evalState, Kernel::Arguments & args, const Ast::SourceLocation & callLoc ) const
			{
				const size_t ARITY = 1;
				CHECK_ARITY( args, ARITY, title_ );

				RefCountPtr< const Lang::Value > res = args.getValue( 0 );
				res->set_node( args.getNode( 0 ) );

				Kernel::ContRef cont = evalState->cont_;
				cont->takeValue( res,
												 evalState );
			}
		};

		class Core_sourceof : public Lang::CoreFunction
		{
		public:
			Core_sourceof( const char * title ) : CoreFunction( title ) { }
			virtual void call( Kernel::EvalState * evalState, Kernel::Arguments & args, const Ast::SourceLocation & callLoc ) const
			{
				const size_t ARITY = 1;
				CHECK_ARITY( args, ARITY, title_ );

				const Ast::Node * node = args.getValue( 0 )->node( );
				if( node == 0 )
					{
						throw Exceptions::CoreOutOfRange( title_, args, 0, "The value has not been located." );
					}

				std::ostringstream oss;
				try
					{
						node->loc( ).copy( & oss );
					}
				catch( const std::string & ball )
					{
						std::ostringstream msg;
						msg << "Source of located value could not be copied.  Reason: " << ball ;
						throw Exceptions::CoreOutOfRange( title_, args, 0, strrefdup( msg ) );
					}

				Kernel::ContRef cont = evalState->cont_;
				cont->takeValue( RefCountPtr< const Lang::Value >( new Lang::String( strrefdup( oss.str( ) ) ) ),
													evalState );
			}
		};

	}
}


RefCountPtr< const Lang::CoreFunction > Lang::THE_IDENTITY( new Lang::Core_identity( "identity" ) );

void
Kernel::registerCore_misc( Kernel::Environment * env )
{
	env->initDefineCoreFunction( new Lang::Core_typeof( "typeof" ) );
	env->initDefineCoreFunction( new Lang::Core_error( "error" ) );
	env->initDefineCoreFunction( new Lang::Core_show( "show" ) );
	env->initDefineCoreFunction( new Lang::Core_typename( "typename" ) );
	env->initDefineCoreFunction( new Lang::Core_debuglog_before( "debuglog_before" ) );
	env->initDefineCoreFunction( new Lang::Core_debuglog_after( "debuglog_after" ) );
	env->initDefineCoreFunction( new Lang::Core_if( "if" ) );
	env->initDefineCoreFunction( new Lang::NullFunction( "ignore" ) );
	env->initDefineCoreFunction( new Lang::Core_rectangle( "rectangle" ) );
	env->initDefineCoreFunction( new Lang::Core_memoryinfo( "memoryinfo" ) );
	env->initDefineCoreFunction( new Lang::Core_hot( "hot" ) );
	env->initDefineCoreFunction( new Lang::Core_ampersand_dynamic( "bindings" ) );

	env->initDefineCoreFunction( new Lang::Core_nextpagenumber( "nextpagenumber" ) );
	env->initDefineCoreFunction( new Lang::Core_nextpagelabel( "nextpagelabel" ) );
	env->initDefineCoreFunction( new Lang::Core_setpagelabel( "setpagelabel" ) );
	env->initDefineCoreFunction( new Lang::Core_setbboxgroup( "setbboxgroup" ) );

	env->initDefineCoreFunction( new Lang::Core_locate( "locate" ) );
	env->initDefineCoreFunction( new Lang::Core_sourceof( "sourceof" ) );
}

