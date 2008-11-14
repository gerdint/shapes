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

#include "Shapes_Helpers_decls.h"

#include "shapescore.h"
#include "ast.h"
#include "globals.h"
#include "shapesexceptions.h"
#include "consts.h"
#include "simplepdfi.h"
#include "autoonoff.h"

#include <iostream>
#include <sstream>
#include <fstream>
#include <vector>
#include <stdio.h>
#include <stdlib.h>
#define min(a,b) (((a)<(b))?(a):(b))
#define max(a,b) (((a)>(b))?(a):(b))
#define RANDOM_MAX ((1<<31)-1)
#define TWO_DIV_RANDOM_MAX (2. / ( (1<<31)-1 ))

using namespace Shapes;

namespace Shapes
{
	namespace Lang
	{
		class Core_mod : public Lang::CoreFunction
		{
		public:
			Core_mod( const char * title ) : CoreFunction( title ) { }
			virtual void
			call( Kernel::EvalState * evalState, Kernel::Arguments & args, const Ast::SourceLocation & callLoc ) const
			{
				const size_t ARITY = 2;
				CHECK_ARITY( args, ARITY, title_ );

				try
					{
						typedef const Lang::Integer ArgType;
						RefCountPtr< ArgType > num = Helpers::try_cast_CoreArgument< ArgType >( args.getValue( 0 ) );
						RefCountPtr< ArgType > den = Helpers::down_cast_CoreArgument< ArgType >( title_, args, 1, callLoc );

						Kernel::ContRef cont = evalState->cont_;
						cont->takeValue( Kernel::ValueRef( new Lang::Integer( num->val_ % den->val_ ) ),
														 evalState );
						return;
					}
				catch( const NonLocalExit::NotThisType & ball )
					{
						/* Wrong type; never mind!.. but see below!
						 */
					}

				try
					{
						typedef const Lang::Float ArgType;
						RefCountPtr< ArgType > num = Helpers::try_cast_CoreArgument< ArgType >( args.getValue( 0 ) );
						RefCountPtr< ArgType > den = Helpers::down_cast_CoreArgument< ArgType >( title_, args, 1, callLoc );

						Kernel::ContRef cont = evalState->cont_;
						cont->takeValue( Kernel::ValueRef( new Lang::Float( fmod( num->val_, den->val_ ) ) ),
														 evalState );
						return;
					}
				catch( const NonLocalExit::NotThisType & ball )
					{
						/* Wrong type; never mind!.. but see below!
						 */
					}

				try
					{
						typedef const Lang::Length ArgType;
						RefCountPtr< ArgType > num = Helpers::try_cast_CoreArgument< ArgType >( args.getValue( 0 ) );
						RefCountPtr< ArgType > den = Helpers::down_cast_CoreArgument< ArgType >( title_, args, 1, callLoc );

						Kernel::ContRef cont = evalState->cont_;
						cont->takeValue( Kernel::ValueRef( new Lang::Length( fmod( num->getScalar( ), den->getScalar( ) ) ) ),
														 evalState );
						return;
					}
				catch( const NonLocalExit::NotThisType & ball )
					{
						/* Wrong type; never mind!.. but see below!
						 */
					}

				throw Exceptions::CoreTypeMismatch( callLoc, title_, args, 0, Helpers::typeSetString( Lang::Integer::staticTypeName( ), Lang::Float::staticTypeName( ), Lang::Length::staticTypeName( ) ) );
			}
		};

		class Core_floor : public Lang::CoreFunction
		{
		public:
			Core_floor( const char * title ) : CoreFunction( title ) { }
			virtual void
			call( Kernel::EvalState * evalState, Kernel::Arguments & args, const Ast::SourceLocation & callLoc ) const
			{
				const size_t ARITY = 1;
				CHECK_ARITY( args, ARITY, title_ );

				typedef const Lang::Float ArgType;
				RefCountPtr< ArgType > arg = Helpers::down_cast_CoreArgument< ArgType >( title_, args, 0, callLoc );

				Kernel::ContRef cont = evalState->cont_;
				cont->takeValue( Kernel::ValueRef( new Lang::Integer( static_cast< Lang::Integer::ValueType >( floor( arg->val_ ) ) ) ),
												 evalState );
			}
		};

		class Core_ceil : public Lang::CoreFunction
		{
		public:
			Core_ceil( const char * title ) : CoreFunction( title ) { }
			virtual void
			call( Kernel::EvalState * evalState, Kernel::Arguments & args, const Ast::SourceLocation & callLoc ) const
			{
				const size_t ARITY = 1;
				CHECK_ARITY( args, ARITY, title_ );

				typedef const Lang::Float ArgType;
				RefCountPtr< ArgType > arg = Helpers::down_cast_CoreArgument< ArgType >( title_, args, 0, callLoc );

				Kernel::ContRef cont = evalState->cont_;
				cont->takeValue( Kernel::ValueRef( new Lang::Integer( static_cast< Lang::Integer::ValueType >( ceil( arg->val_ ) ) ) ),
												 evalState );
			}
		};

		class Core_rint : public Lang::CoreFunction
		{
		public:
			Core_rint( const char * title ) : CoreFunction( title ) { }
			virtual void
			call( Kernel::EvalState * evalState, Kernel::Arguments & args, const Ast::SourceLocation & callLoc ) const
			{
				const size_t ARITY = 1;
				CHECK_ARITY( args, ARITY, title_ );

				typedef const Lang::Float ArgType;
				RefCountPtr< ArgType > arg = Helpers::down_cast_CoreArgument< ArgType >( title_, args, 0, callLoc );

				Kernel::ContRef cont = evalState->cont_;
				cont->takeValue( Kernel::ValueRef( new Lang::Integer( static_cast< Lang::Integer::ValueType >( rint( arg->val_ ) ) ) ),
												 evalState );
			}
		};

		class Core_cos : public Lang::CoreFunction
		{
		public:
			Core_cos( const char * title ) : CoreFunction( title ) { }
			virtual void
			call( Kernel::EvalState * evalState, Kernel::Arguments & args, const Ast::SourceLocation & callLoc ) const
			{
				const size_t ARITY = 1;
				CHECK_ARITY( args, ARITY, title_ );

				typedef const Lang::Float ArgType;
				RefCountPtr< ArgType > arg = Helpers::down_cast_CoreArgument< ArgType >( title_, args, 0, callLoc );

				Kernel::ContRef cont = evalState->cont_;
				cont->takeValue( Kernel::ValueRef( new Lang::Float( cos( arg->val_ ) ) ),
												 evalState );
			}
		};

		class Core_sin : public Lang::CoreFunction
		{
		public:
			Core_sin( const char * title ) : CoreFunction( title ) { }
			virtual void
			call( Kernel::EvalState * evalState, Kernel::Arguments & args, const Ast::SourceLocation & callLoc ) const
			{
				const size_t ARITY = 1;
				CHECK_ARITY( args, ARITY, title_ );

				typedef const Lang::Float ArgType;
				RefCountPtr< ArgType > arg = Helpers::down_cast_CoreArgument< ArgType >( title_, args, 0, callLoc );

				Kernel::ContRef cont = evalState->cont_;
				cont->takeValue( Kernel::ValueRef( new Lang::Float( sin( arg->val_ ) ) ),
												 evalState );
			}
		};

		class Core_tan : public Lang::CoreFunction
		{
		public:
			Core_tan( const char * title ) : CoreFunction( title ) { }
			virtual void
			call( Kernel::EvalState * evalState, Kernel::Arguments & args, const Ast::SourceLocation & callLoc ) const
			{
				const size_t ARITY = 1;
				CHECK_ARITY( args, ARITY, title_ );

				typedef const Lang::Float ArgType;
				RefCountPtr< ArgType > arg = Helpers::down_cast_CoreArgument< ArgType >( title_, args, 0, callLoc );

				Kernel::ContRef cont = evalState->cont_;
				cont->takeValue( Kernel::ValueRef( new Lang::Float( tan( arg->val_ ) ) ),
												 evalState );
			}
		};

		class Core_cot : public Lang::CoreFunction
		{
		public:
			Core_cot( const char * title ) : CoreFunction( title ) { }
			virtual void
			call( Kernel::EvalState * evalState, Kernel::Arguments & args, const Ast::SourceLocation & callLoc ) const
			{
				const size_t ARITY = 1;
				CHECK_ARITY( args, ARITY, title_ );

				typedef const Lang::Float ArgType;
				RefCountPtr< ArgType > arg = Helpers::down_cast_CoreArgument< ArgType >( title_, args, 0, callLoc );

				Kernel::ContRef cont = evalState->cont_;
				cont->takeValue( Kernel::ValueRef( new Lang::Float( 1 / tan( arg->val_ ) ) ),
												 evalState );
			}
		};

		class Core_arccos : public Lang::CoreFunction
		{
		public:
			Core_arccos( const char * title ) : CoreFunction( title ) { }
			virtual void
			call( Kernel::EvalState * evalState, Kernel::Arguments & args, const Ast::SourceLocation & callLoc ) const
			{
				const size_t ARITY = 1;
				CHECK_ARITY( args, ARITY, title_ );

				typedef const Lang::Float ArgType;
				RefCountPtr< ArgType > arg = Helpers::down_cast_CoreArgument< ArgType >( title_, args, 0, callLoc );

				Kernel::ContRef cont = evalState->cont_;
				cont->takeValue( Kernel::ValueRef( new Lang::Float( acos( arg->val_ ) ) ),
												 evalState );
			}
		};

		class Core_arcsin : public Lang::CoreFunction
		{
		public:
			Core_arcsin( const char * title ) : CoreFunction( title ) { }
			virtual void
			call( Kernel::EvalState * evalState, Kernel::Arguments & args, const Ast::SourceLocation & callLoc ) const
			{
				const size_t ARITY = 1;
				CHECK_ARITY( args, ARITY, title_ );

				typedef const Lang::Float ArgType;
				RefCountPtr< ArgType > arg = Helpers::down_cast_CoreArgument< ArgType >( title_, args, 0, callLoc );

				Kernel::ContRef cont = evalState->cont_;
				cont->takeValue( Kernel::ValueRef( new Lang::Float( asin( arg->val_ ) ) ),
												 evalState );
			}
		};

		class Core_arctan : public Lang::CoreFunction
		{
		public:
			Core_arctan( const char * title ) : CoreFunction( title ) { }
			virtual void
			call( Kernel::EvalState * evalState, Kernel::Arguments & args, const Ast::SourceLocation & callLoc ) const
			{
				const size_t ARITY = 1;
				CHECK_ARITY( args, ARITY, title_ );

				typedef const Lang::Float ArgType;
				RefCountPtr< ArgType > arg = Helpers::down_cast_CoreArgument< ArgType >( title_, args, 0, callLoc );

				Kernel::ContRef cont = evalState->cont_;
				cont->takeValue( Kernel::ValueRef( new Lang::Float( atan( arg->val_ ) ) ),
												 evalState );
			}
		};

		class Core_min : public Lang::CoreFunction
		{
		public:
			Core_min( const char * title ) : CoreFunction( title ) { }
			virtual void
			call( Kernel::EvalState * evalState, Kernel::Arguments & args, const Ast::SourceLocation & callLoc ) const
			{
				if( args.empty( ) )
					{
						Kernel::ContRef cont = evalState->cont_;
						cont->takeValue( Kernel::ValueRef( new Lang::Float( HUGE_VAL ) ),
														 evalState );
						return;
					}

				{
					typedef const Lang::Float ArgType;
					size_t i = 0;
					ArgType * arg = dynamic_cast< ArgType * >( args.getValue( i ).getPtr( ) );
					if( arg != 0 )
						{
							double res = arg->val_;
							typedef typeof args ListType;
							++i;
							const size_t & end = args.size( );
							for( ; i != end; ++i )
								{
									res = min( res, Helpers::down_cast_CoreArgument< ArgType >( title_, args, i, callLoc )->val_ );
								}
							Kernel::ContRef cont = evalState->cont_;
							cont->takeValue( Kernel::ValueRef( new ArgType( res ) ),
															 evalState );
							return;
						}
				}

				{
					typedef const Lang::Length ArgType;
					size_t i = 0;
					ArgType * arg = dynamic_cast< ArgType * >( args.getValue( i ).getPtr( ) );
					if( arg != 0 )
						{
							Concrete::Length res = arg->get( );
							typedef typeof args ListType;
							++i;
							const size_t & end = args.size( );
							for( ; i != end; ++i )
								{
									res = min( res, Helpers::down_cast_CoreArgument< ArgType >( title_, args, i, callLoc )->get( ) );
								}
							Kernel::ContRef cont = evalState->cont_;
							cont->takeValue( Kernel::ValueRef( new ArgType( res ) ),
															 evalState );
							return;
						}
				}

				{
					typedef const Lang::Integer ArgType;
					size_t i = 0;
					ArgType * arg = dynamic_cast< ArgType * >( args.getValue( i ).getPtr( ) );
					if( arg != 0 )
						{
							Lang::Integer::ValueType res = arg->val_;
							typedef typeof args ListType;
							++i;
							const size_t & end = args.size( );
							for( ; i != end; ++i )
								{
									res = min( res, Helpers::down_cast_CoreArgument< ArgType >( title_, args, i, callLoc )->val_ );
								}
							Kernel::ContRef cont = evalState->cont_;
							cont->takeValue( Kernel::ValueRef( new ArgType( res ) ),
															 evalState );
							return;
						}
				}

				throw Exceptions::CoreTypeMismatch( callLoc, title_, args, 0, Helpers::typeSetString( Lang::Float::staticTypeName( ), Lang::Integer::staticTypeName( ), Lang::Length::staticTypeName( ) ) );
			}
		};

		class Core_max : public Lang::CoreFunction
		{
		public:
			Core_max( const char * title ) : CoreFunction( title ) { }
			virtual void
			call( Kernel::EvalState * evalState, Kernel::Arguments & args, const Ast::SourceLocation & callLoc ) const
			{
				if( args.empty( ) )
					{
						Kernel::ContRef cont = evalState->cont_;
						cont->takeValue( Kernel::ValueRef( new Lang::Float( -HUGE_VAL ) ),
														 evalState );
						return;
					}

				{
					typedef const Lang::Float ArgType;
					size_t i = 0;
					ArgType * arg = dynamic_cast< ArgType * >( args.getValue( i ).getPtr( ) );
					if( arg != 0 )
						{
							double res = arg->val_;
							typedef typeof args ListType;
							++i;
							const size_t & end = args.size( );
							for( ; i != end; ++i )
								{
									res = max( res, Helpers::down_cast_CoreArgument< ArgType >( title_, args, i, callLoc )->val_ );
								}
							Kernel::ContRef cont = evalState->cont_;
							cont->takeValue( Kernel::ValueRef( new ArgType( res ) ),
															 evalState );
							return;
						}
				}

				{
					typedef const Lang::Length ArgType;
					size_t i = 0;
					ArgType * arg = dynamic_cast< ArgType * >( args.getValue( i ).getPtr( ) );
					if( arg != 0 )
						{
							Concrete::Length res = arg->get( );
							typedef typeof args ListType;
							++i;
							const size_t & end = args.size( );
							for( ; i != end; ++i )
								{
									res = max( res, Helpers::down_cast_CoreArgument< ArgType >( title_, args, i, callLoc )->get( ) );
								}
							Kernel::ContRef cont = evalState->cont_;
							cont->takeValue( Kernel::ValueRef( new ArgType( res ) ),
															 evalState );
							return;
						}
				}


				{
					typedef const Lang::Integer ArgType;
					size_t i = 0;
					ArgType * arg = dynamic_cast< ArgType * >( args.getValue( i ).getPtr( ) );
					if( arg != 0 )
						{
							Lang::Integer::ValueType res = arg->val_;
							typedef typeof args ListType;
							++i;
							const size_t & end = args.size( );
							for( ; i != end; ++i )
								{
									res = max( res, Helpers::down_cast_CoreArgument< ArgType >( title_, args, i, callLoc )->val_ );
								}
							Kernel::ContRef cont = evalState->cont_;

							cont->takeValue( Kernel::ValueRef( new ArgType( res ) ),
															 evalState );
							return;
						}
				}

				throw Exceptions::CoreTypeMismatch( callLoc, title_, args, 0, Helpers::typeSetString( Lang::Float::staticTypeName( ), Lang::Integer::staticTypeName( ), Lang::Length::staticTypeName( ) ) );
			}
		};

		class Core_sqrt : public Lang::CoreFunction
		{
		public:
			Core_sqrt( const char * title ) : CoreFunction( title ) { }
			virtual void
			call( Kernel::EvalState * evalState, Kernel::Arguments & args, const Ast::SourceLocation & callLoc ) const
			{
				const size_t ARITY = 1;
				CHECK_ARITY( args, ARITY, title_ );

				typedef const Lang::Float ArgType;
				RefCountPtr< ArgType > arg = Helpers::down_cast_CoreArgument< ArgType >( title_, args, 0, callLoc );

				Kernel::ContRef cont = evalState->cont_;
				cont->takeValue( Kernel::ValueRef( new Lang::Float( sqrt( arg->val_ ) ) ),
												 evalState );
			}
		};

		class Core_angle : public Lang::CoreFunction
		{
		public:
			Core_angle( const char * title ) : CoreFunction( title ) { }
			virtual void
			call( Kernel::EvalState * evalState, Kernel::Arguments & args, const Ast::SourceLocation & callLoc ) const
			{
				const size_t ARITY = 1;
				CHECK_ARITY( args, ARITY, title_ );

				{
					typedef const Lang::Coords2D ArgType;
					ArgType * arg = dynamic_cast< ArgType * >( args.getValue( 0 ).getPtr( ) );
					if( arg != 0 )
						{
							if( arg->x_.get( ) == 0 && arg->y_.get( ) == 0 )
								{
									throw Exceptions::CoreOutOfRange( title_, args, 0, "Can't take the angle of something of norm 0." );
								}
							Kernel::ContRef cont = evalState->cont_;
							cont->takeValue( Kernel::ValueRef( new Lang::Float( atan2( arg->y_.getScalar( ), arg->x_.getScalar( ) ) ) ),
															 evalState );
							return;
						}
				}

				{
					typedef const Lang::FloatPair ArgType;
					ArgType * arg = dynamic_cast< ArgType * >( args.getValue( 0 ).getPtr( ) );
					if( arg != 0 )
						{
							if( arg->x_ == 0 && arg->y_ == 0 )
								{
									throw Exceptions::CoreOutOfRange( title_, args, 0, "Can't take the angle of something of norm 0." );
								}
							Kernel::ContRef cont = evalState->cont_;
							cont->takeValue( Kernel::ValueRef( new Lang::Float( atan2( arg->y_, arg->x_ ) ) ),
															 evalState );
							return;
						}
				}

				throw Exceptions::CoreTypeMismatch( callLoc, title_, args, 0, Interaction::SEVERAL_TYPES );
			}
		};

		class Core_dir : public Lang::CoreFunction
		{
		public:
			Core_dir( const char * title ) : CoreFunction( title ) { }
			virtual void
			call( Kernel::EvalState * evalState, Kernel::Arguments & args, const Ast::SourceLocation & callLoc ) const
			{
				const size_t ARITY = 1;
				CHECK_ARITY( args, ARITY, title_ );

				typedef const Lang::Float ArgType;
				RefCountPtr< ArgType > arg = Helpers::down_cast_CoreArgument< ArgType >( title_, args, 0, callLoc );

				Kernel::ContRef cont = evalState->cont_;
				cont->takeValue( Kernel::ValueRef( new Lang::FloatPair( cos( arg->val_ ), sin( arg->val_ ) ) ),
												 evalState );
			}
		};

		class Core_abs : public Lang::CoreFunction
		{
		public:
			Core_abs( const char * title ) : CoreFunction( title ) { }
			virtual void
			call( Kernel::EvalState * evalState, Kernel::Arguments & args, const Ast::SourceLocation & callLoc ) const
			{
				const size_t ARITY = 1;
				CHECK_ARITY( args, ARITY, title_ );

				const Lang::Value * untypedArg = args.getValue( 0 ).getPtr( );

				{
					typedef const Lang::Float ArgType;
					ArgType * arg = dynamic_cast< ArgType * >( untypedArg );
					if( arg != 0 )
						{
							Kernel::ContRef cont = evalState->cont_;
							cont->takeValue( Kernel::ValueRef( new Lang::Float( fabs( arg->val_ ) ) ),
															 evalState );
							return;
						}
				}

				{
					typedef const Lang::Length ArgType;
					ArgType * arg = dynamic_cast< ArgType * >( untypedArg );
					if( arg != 0 )
						{
							Kernel::ContRef cont = evalState->cont_;
							cont->takeValue( Kernel::ValueRef( new Lang::Length( arg->get( ).abs( ) ) ),
															 evalState );
							return;
						}
				}

				{
					typedef const Lang::FloatPair ArgType;
					ArgType * arg = dynamic_cast< ArgType * >( untypedArg );
					if( arg != 0 )
						{
							Kernel::ContRef cont = evalState->cont_;
							cont->takeValue( Kernel::ValueRef( new Lang::Float( hypot( arg->x_, arg->y_ ) ) ),
															 evalState );
							return;
						}
				}

				{
					typedef const Lang::Coords2D ArgType;
					ArgType * arg = dynamic_cast< ArgType * >( untypedArg );
					if( arg != 0 )
						{
							Kernel::ContRef cont = evalState->cont_;
							cont->takeValue( Kernel::ValueRef( new Lang::Length( hypotPhysical( arg->x_.get( ), arg->y_.get( ) ) ) ),
															 evalState );
							return;
						}
				}

				{
					typedef const Lang::FloatTriple ArgType;
					ArgType * arg = dynamic_cast< ArgType * >( untypedArg );
					if( arg != 0 )
						{
							Kernel::ContRef cont = evalState->cont_;
							cont->takeValue( Kernel::ValueRef( new Lang::Float( Concrete::Scalar::hypot3( arg->x_, arg->y_, arg->z_ ) ) ),
															 evalState );
							return;
						}
				}

				{
					typedef const Lang::Coords3D ArgType;
					ArgType * arg = dynamic_cast< ArgType * >( untypedArg );
					if( arg != 0 )
						{
							Kernel::ContRef cont = evalState->cont_;
							cont->takeValue( Kernel::ValueRef( new Lang::Length( hypotPhysical( arg->x_.get( ), arg->y_.get( ), arg->z_.get( ) ) ) ),
															 evalState );
							return;
						}
				}

				{
					typedef const Lang::ElementaryPath2D ArgType;
					try
						{
							RefCountPtr< ArgType > p = Helpers::elementaryPathTry2D( args.getValue( 0 ) );
							Kernel::ContRef cont = evalState->cont_;
							cont->takeValue( RefCountPtr< const Lang::Length >( new Lang::Length( p->arcLength( ) ) ),
															 evalState );
							return;
						}
					catch( NonLocalExit::NotThisType & ball )
						{
							/* Never mind */
						}
				}

				{
					typedef const Lang::ElementaryPath3D ArgType;
					try
						{
							RefCountPtr< ArgType > p = Helpers::elementaryPathTry3D( args.getValue( 0 ) );
							Kernel::ContRef cont = evalState->cont_;
							cont->takeValue( RefCountPtr< const Lang::Length >( new Lang::Length( p->arcLength( ) ) ),
															 evalState );
							return;
						}
					catch( NonLocalExit::NotThisType & ball )
						{
							/* Never mind */
						}
				}

				{
					typedef const Lang::Integer ArgType;
					ArgType * arg = dynamic_cast< ArgType * >( untypedArg );
					if( arg != 0 )
						{
							Kernel::ContRef cont = evalState->cont_;
							cont->takeValue( Kernel::ValueRef( new Lang::Integer( std::abs( arg->val_ ) ) ),
															 evalState );
							return;
						}
				}

				{
					typedef const Lang::Dash ArgType;
					ArgType * arg = dynamic_cast< ArgType * >( untypedArg );
					if( arg != 0 )
						{
							Kernel::ContRef cont = evalState->cont_;
							cont->takeValue( Kernel::ValueRef( new Lang::Length( arg->length( ) ) ),
															 evalState );
							return;
						}
				}

				throw Exceptions::CoreTypeMismatch( callLoc, title_, args, 0, Interaction::SEVERAL_TYPES );
			}
		};

		class Core_normalized : public Lang::CoreFunction
		{
		public:
			Core_normalized( const char * title ) : CoreFunction( title ) { }
			virtual void
			call( Kernel::EvalState * evalState, Kernel::Arguments & args, const Ast::SourceLocation & callLoc ) const
			{
				const size_t ARITY = 1;
				CHECK_ARITY( args, ARITY, title_ );

				{
					typedef const Lang::FloatPair ArgType;
					ArgType * arg = dynamic_cast< ArgType * >( args.getValue( 0 ).getPtr( ) );
					if( arg != 0 )
						{
							double norm = hypot( arg->x_, arg->y_ );
							if( norm == 0 )
								{
									throw Exceptions::CoreOutOfRange( title_, args, 0, "Can't normalize something of norm 0" );
								}
							Kernel::ContRef cont = evalState->cont_;
							cont->takeValue( Kernel::ValueRef( new Lang::FloatPair( arg->x_ / norm, arg->y_ / norm ) ),
															 evalState );
							return;
						}
				}

				{
					typedef const Lang::Coords2D ArgType;
					ArgType * arg = dynamic_cast< ArgType * >( args.getValue( 0 ).getPtr( ) );
					if( arg != 0 )
						{
							Concrete::Length norm = hypotPhysical( arg->x_.get( ), arg->y_.get( ) );
							if( norm == 0 )
								{
									throw Exceptions::CoreOutOfRange( title_, args, 0, "Can't normalize something of norm 0" );
								}
							Kernel::ContRef cont = evalState->cont_;
							cont->takeValue( Kernel::ValueRef( new Lang::FloatPair( arg->x_.get( ) / norm, arg->y_.get( ) / norm ) ),
															 evalState );
							return;
						}
				}

				{
					typedef const Lang::FloatTriple ArgType;
					ArgType * arg = dynamic_cast< ArgType * >( args.getValue( 0 ).getPtr( ) );
					if( arg != 0 )
						{
							double norm = Concrete::Scalar::hypot3( arg->x_, arg->y_, arg->z_ );
							if( norm == 0 )
								{
									throw Exceptions::CoreOutOfRange( title_, args, 0, "Can't normalize something of norm 0" );
								}
							Kernel::ContRef cont = evalState->cont_;
							cont->takeValue( Kernel::ValueRef( new Lang::FloatTriple( arg->x_ / norm, arg->y_ / norm, arg->z_ / norm ) ),
															 evalState );
							return;
						}
				}

				{
					typedef const Lang::Coords3D ArgType;
					ArgType * arg = dynamic_cast< ArgType * >( args.getValue( 0 ).getPtr( ) );
					if( arg != 0 )
						{
							Concrete::Length norm = hypotPhysical( arg->x_.get( ), arg->y_.get( ), arg->z_.get( ) );
							if( norm == 0 )
								{
									throw Exceptions::CoreOutOfRange( title_, args, 0, "Can't normalize something of norm 0" );
								}
							Kernel::ContRef cont = evalState->cont_;
							cont->takeValue( Kernel::ValueRef( new Lang::FloatTriple( arg->x_.get( ) / norm, arg->y_.get( ) / norm, arg->z_.get( ) / norm ) ),
															 evalState );
							return;
						}
				}

				{
					typedef const Lang::Float ArgType;
					ArgType * arg = dynamic_cast< ArgType * >( args.getValue( 0 ).getPtr( ) );
					if( arg != 0 )
						{
							if( arg->val_ > 0 )
								{
									Kernel::ContRef cont = evalState->cont_;
									cont->takeValue( Kernel::ValueRef( new Lang::Float( 1 ) ),
																	 evalState );
									return;
								}
							if( arg->val_ < 0 )
								{
									Kernel::ContRef cont = evalState->cont_;
									cont->takeValue( Kernel::ValueRef( new Lang::Float( -1 ) ),
																	 evalState );
									return;
								}
							throw Exceptions::CoreOutOfRange( title_, args, 0, "Can't normalize something of norm 0" );
						}
				}

				{
					typedef const Lang::Length ArgType;
					ArgType * arg = dynamic_cast< ArgType * >( args.getValue( 0 ).getPtr( ) );
					if( arg != 0 )
						{
							if( arg->get( ) > 0 )
								{
									Kernel::ContRef cont = evalState->cont_;
									cont->takeValue( Kernel::ValueRef( new Lang::Float( 1 ) ),
																	 evalState );
									return;
								}
							if( arg->get( ) < 0 )
								{
									Kernel::ContRef cont = evalState->cont_;
									cont->takeValue( Kernel::ValueRef( new Lang::Float( -1 ) ),
																	 evalState );
									return;
								}
							throw Exceptions::CoreOutOfRange( title_, args, 0, "Can't normalize something of norm 0" );
						}
				}

				{
					typedef const Lang::Integer ArgType;
					ArgType * arg = dynamic_cast< ArgType * >( args.getValue( 0 ).getPtr( ) );
					if( arg != 0 )
						{
							if( arg->val_ > 0 )
								{
									Kernel::ContRef cont = evalState->cont_;
									cont->takeValue( Kernel::ValueRef( new Lang::Integer( 1 ) ),
																	 evalState );
									return;
								}
							if( arg->val_ < 0 )
								{
									Kernel::ContRef cont = evalState->cont_;
									cont->takeValue( Kernel::ValueRef( new Lang::Integer( -1 ) ),
																	 evalState );
									return;
								}
							throw Exceptions::CoreOutOfRange( title_, args, 0, "Can't normalize something of norm 0" );
						}
				}


				throw Exceptions::CoreTypeMismatch( callLoc, title_, args, 0, Interaction::SEVERAL_TYPES );
			}
		};

		class Core_cross : public Lang::CoreFunction
		{
		public:
			Core_cross( const char * title ) : CoreFunction( title ) { }
			virtual void
			call( Kernel::EvalState * evalState, Kernel::Arguments & args, const Ast::SourceLocation & callLoc ) const
			{
				const size_t ARITY = 2;
				CHECK_ARITY( args, ARITY, title_ );

				double x1;
				double y1;
				double z1;
				double x2;
				double y2;
				double z2;
				bool isLength = false;

				{
					typedef const Lang::FloatTriple ArgType;
					ArgType * arg = dynamic_cast< ArgType * >( args.getValue( 0 ).getPtr( ) );
					if( arg != 0 )
						{
							x1 = arg->x_;
							y1 = arg->y_;
							z1 = arg->z_;
							goto secondArgument;
						}
				}

				{
					typedef const Lang::Coords3D ArgType;
					ArgType * arg = dynamic_cast< ArgType * >( args.getValue( 0 ).getPtr( ) );
					if( arg != 0 )
						{
							isLength = true;
							x1 = arg->x_.get( ).offtype< 1, 0 >( );
							y1 = arg->y_.get( ).offtype< 1, 0 >( );
							z1 = arg->z_.get( ).offtype< 1, 0 >( );
							goto secondArgument;
						}
				}

				throw Exceptions::CoreTypeMismatch( callLoc, title_, args, 0, Helpers::typeSetString( Lang::FloatTriple::staticTypeName( ), Lang::Coords3D::staticTypeName( ) ) );

			secondArgument:
				{
					typedef const Lang::FloatTriple ArgType;
					ArgType * arg = dynamic_cast< ArgType * >( args.getValue( 1 ).getPtr( ) );
					if( arg != 0 )
						{
							x2 = arg->x_;
							y2 = arg->y_;
							z2 = arg->z_;
							goto multiplyArguments;
						}
				}

				{
					typedef const Lang::Coords3D ArgType;
					ArgType * arg = dynamic_cast< ArgType * >( args.getValue( 1 ).getPtr( ) );
					if( arg != 0 )
						{
							if( isLength )
								{
									throw Exceptions::CoreOutOfRange( title_, args, 1, "Can't multiply two coordinate vectors.	Try normalizing one of them!" );
								}
							isLength = true;
							x2 = arg->x_.get( ).offtype< 1, 0 >( );
							y2 = arg->y_.get( ).offtype< 1, 0 >( );
							z2 = arg->z_.get( ).offtype< 1, 0 >( );
							goto multiplyArguments;
						}
				}

				throw Exceptions::CoreTypeMismatch( callLoc, title_, args, 1, Helpers::typeSetString( Lang::FloatTriple::staticTypeName( ), Lang::Coords3D::staticTypeName( ) ) );

			multiplyArguments:
				Kernel::ContRef cont = evalState->cont_;
				if( isLength )
					{
						cont->takeValue( Kernel::ValueRef( new Lang::Coords3D( Concrete::Length( y1*z2-z1*y2 ), Concrete::Length( z1*x2-x1*z2 ), Concrete::Length( x1*y2-y1*x2 ) ) ),
														 evalState );
					}
				else
					{
						cont->takeValue( Kernel::ValueRef( new Lang::FloatTriple( y1*z2-z1*y2, z1*x2-x1*z2, x1*y2-y1*x2 ) ),
														 evalState );
					}
			}
		};

		class Core_orthogonal : public Lang::CoreFunction
		{
		public:
			Core_orthogonal( const char * title ) : CoreFunction( title ) { }
			virtual void
			call( Kernel::EvalState * evalState, Kernel::Arguments & args, const Ast::SourceLocation & callLoc ) const
			{
				const size_t ARITY = 1;
				CHECK_ARITY( args, ARITY, title_ );

				{
					typedef const Lang::FloatPair ArgType;
					ArgType * arg = dynamic_cast< ArgType * >( args.getValue( 0 ).getPtr( ) );
					if( arg != 0 )
						{
							Kernel::ContRef cont = evalState->cont_;
							cont->takeValue( Kernel::ValueRef( new Lang::FloatPair( -arg->y_, arg->x_ ) ),
															 evalState );
							return;
						}
				}

				{
					typedef const Lang::FloatTriple ArgType;
					ArgType * arg = dynamic_cast< ArgType * >( args.getValue( 0 ).getPtr( ) );
					if( arg != 0 )
						{
							double x;
							double y;
							double z;
							if( fabs( arg->x_ ) < fabs( arg->y_ ) )
								{
									// arg is more parallell to y; cross with x
									x = 0;
									y = - arg->z_;
									z = arg->y_;
								}
							else
								{
									// arg is more parallell to x; cross with y
									x = arg->z_;
									y = 0;
									z = - arg->x_;
								}
							double scale = sqrt( ( (arg->x_)*(arg->x_) + (arg->y_)*(arg->y_) + (arg->z_)*(arg->z_) ) / ( x*x + y*y + z*z ) );
							Kernel::ContRef cont = evalState->cont_;
							cont->takeValue( Kernel::ValueRef( new Lang::FloatTriple( x * scale, y * scale, z * scale ) ),
															 evalState );
							return;
						}
				}

				throw Exceptions::CoreTypeMismatch( callLoc, title_, args, 0, Helpers::typeSetString( Lang::FloatPair::staticTypeName( ), Lang::FloatTriple::staticTypeName( ) ) );
			}
		};

		class Core_randomNatural : public Lang::CoreFunction
		{
		public:
			Core_randomNatural( const char * title )
				: CoreFunction( title, new Kernel::EvaluatedFormals( "< core function randomNatural >", true ) )
			{
				formals_->appendCoreStateFormal( "state" );
			}
			virtual void
			call( Kernel::EvalState * evalState, Kernel::Arguments & args, const Ast::SourceLocation & callLoc ) const
			{
				args.applyDefaults( );

				typedef Kernel::WarmRandomState StateType;
				StateType * st = Helpers::down_cast_CoreState< StateType >( title_, args, 0, callLoc );

				st->setState( );

				Kernel::ContRef cont = evalState->cont_;
				cont->takeValue( Kernel::ValueRef( new Lang::Float( TWO_DIV_RANDOM_MAX * static_cast< double >( random( ) ) - 1 ) ),
												 evalState );
			}
		};

		class Core_randomBall1D : public Lang::CoreFunction
		{
		public:
			Core_randomBall1D( const char * title )
				: CoreFunction( title, new Kernel::EvaluatedFormals( "< core function randomBall1D >", true ) )
			{
				formals_->appendCoreStateFormal( "state" );
			}
			virtual void
			call( Kernel::EvalState * evalState, Kernel::Arguments & args, const Ast::SourceLocation & callLoc ) const
			{
				args.applyDefaults( );

				typedef Kernel::WarmRandomState StateType;
				StateType * st = Helpers::down_cast_CoreState< StateType >( title_, args, 0, callLoc );

				st->setState( );

				Kernel::ContRef cont = evalState->cont_;
				cont->takeValue( Kernel::ValueRef( new Lang::Float( TWO_DIV_RANDOM_MAX * static_cast< double >( random( ) ) - 1 ) ),
												 evalState );
			}
		};

		class Core_randomBall2D : public Lang::CoreFunction
		{
		public:
			Core_randomBall2D( const char * title )
				: CoreFunction( title, new Kernel::EvaluatedFormals( "< core function randomBall2D >", true ) )
			{
				formals_->appendCoreStateFormal( "state" );
			}
			virtual void
			call( Kernel::EvalState * evalState, Kernel::Arguments & args, const Ast::SourceLocation & callLoc ) const
			{
				args.applyDefaults( );

				typedef Kernel::WarmRandomState StateType;
				StateType * st = Helpers::down_cast_CoreState< StateType >( title_, args, 0, callLoc );

				st->setState( );

				double x1 = TWO_DIV_RANDOM_MAX * static_cast< double >( random( ) ) - 1;
				double x2 = TWO_DIV_RANDOM_MAX * static_cast< double >( random( ) ) - 1;
				while( x1 * x1 + x2 * x2 > 1 )
					{
						x1 = TWO_DIV_RANDOM_MAX * static_cast< double >( random( ) ) - 1;
						x2 = TWO_DIV_RANDOM_MAX * static_cast< double >( random( ) ) - 1;
					}

				Kernel::ContRef cont = evalState->cont_;
				cont->takeValue( Kernel::ValueRef( new Lang::FloatPair( x1, x2 ) ),
												 evalState );
			}
		};

		class Core_randomBall3D : public Lang::CoreFunction
		{
		public:
			Core_randomBall3D( const char * title )
				: CoreFunction( title, new Kernel::EvaluatedFormals( "< core function randomBall3D >", true ) )
			{
				formals_->appendCoreStateFormal( "state" );
			}
			virtual void
			call( Kernel::EvalState * evalState, Kernel::Arguments & args, const Ast::SourceLocation & callLoc ) const
			{
				args.applyDefaults( );

				typedef Kernel::WarmRandomState StateType;
				StateType * st = Helpers::down_cast_CoreState< StateType >( title_, args, 0, callLoc );

				st->setState( );

				double x1 = TWO_DIV_RANDOM_MAX * static_cast< double >( random( ) ) - 1;
				double x2 = TWO_DIV_RANDOM_MAX * static_cast< double >( random( ) ) - 1;
				double x3 = TWO_DIV_RANDOM_MAX * static_cast< double >( random( ) ) - 1;
				while( x1 * x1 + x2 * x2 + x3 * x3 > 1 )
					{
						x1 = TWO_DIV_RANDOM_MAX * static_cast< double >( random( ) ) - 1;
						x2 = TWO_DIV_RANDOM_MAX * static_cast< double >( random( ) ) - 1;
						x3 = TWO_DIV_RANDOM_MAX * static_cast< double >( random( ) ) - 1;
					}

				Kernel::ContRef cont = evalState->cont_;
				cont->takeValue( Kernel::ValueRef( new Lang::FloatTriple( x1, x2, x3 ) ),
												 evalState );
			}
		};

		class Core_gensym : public Lang::CoreFunction
		{
		public:
			Core_gensym( const char * title ) : CoreFunction( title ) { }
			virtual void call( Kernel::EvalState * evalState, Kernel::Arguments & args, const Ast::SourceLocation & callLoc ) const
			{
				const size_t ARITY = 0;
				CHECK_ARITY( args, ARITY, title_ );

				Kernel::ContRef cont = evalState->cont_;
				cont->takeValue( Kernel::ValueRef( new Lang::Symbol( ) ),
												 evalState );
			}
		};
	}
}


void
Kernel::registerCore_elem( Kernel::Environment * env )
{
	env->initDefineCoreFunction( new Lang::Core_mod( "mod" ) );
	env->initDefineCoreFunction( new Lang::Core_ceil( "ceil" ) );
	env->initDefineCoreFunction( new Lang::Core_floor( "floor" ) );
	env->initDefineCoreFunction( new Lang::Core_rint( "round" ) );
	env->initDefineCoreFunction( new Lang::Core_cos( "cos" ) );
	env->initDefineCoreFunction( new Lang::Core_sin( "sin" ) );
	env->initDefineCoreFunction( new Lang::Core_tan( "tan" ) );
	env->initDefineCoreFunction( new Lang::Core_cot( "cot" ) );
	env->initDefineCoreFunction( new Lang::Core_arccos( "arccos" ) );
	env->initDefineCoreFunction( new Lang::Core_arcsin( "arcsin" ) );
	env->initDefineCoreFunction( new Lang::Core_arctan( "arctan" ) );
	env->initDefineCoreFunction( new Lang::Core_min( "min" ) );
	env->initDefineCoreFunction( new Lang::Core_max( "max" ) );
	env->initDefineCoreFunction( new Lang::Core_sqrt( "sqrt" ) );
	env->initDefineCoreFunction( new Lang::Core_abs( "abs" ) );
	env->initDefineCoreFunction( new Lang::Core_angle( "angle" ) );
	env->initDefineCoreFunction( new Lang::Core_dir( "dir" ) );
	env->initDefineCoreFunction( new Lang::Core_normalized( "normalized" ) );
	env->initDefineCoreFunction( new Lang::Core_cross( "cross" ) );
	env->initDefineCoreFunction( new Lang::Core_orthogonal( "orthogonal" ) );

	env->initDefineCoreFunction( new Lang::Core_randomNatural( "randomN" ) );
	env->initDefineCoreFunction( new Lang::Core_randomBall1D( "random1D" ) );
	env->initDefineCoreFunction( new Lang::Core_randomBall2D( "random2D" ) );
	env->initDefineCoreFunction( new Lang::Core_randomBall3D( "random3D" ) );

	env->initDefineCoreFunction( new Lang::Core_gensym( "gensym" ) );
}

