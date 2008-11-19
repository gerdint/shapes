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
#include "astfun.h"
#include "continuations.h"

#include <gsl/gsl_matrix.h>
#include <gsl/gsl_linalg.h>
#include <gsl/gsl_blas.h>
#include <gsl/gsl_eigen.h>
#include <gsl/gsl_complex_math.h>
#include <iostream>
#include <sstream>

using namespace Shapes;


namespace Shapes
{
	namespace Helpers
	{
		void Schur_decomposition_helper_2D( Kernel::EvalState * evalState, const RefCountPtr< const Lang::Transform2D > & tf, int rank, bool canonical, const Ast::SourceLocation & callLoc );
		void Schur_decomposition_helper_3D( Kernel::EvalState * evalState, const RefCountPtr< const Lang::Transform3D > & tf, int rank, bool canonical, const Ast::SourceLocation & callLoc );
		Kernel::StructureFactory Schur_decomposition_resultFactory( "Q", "U" );

		void minRotationMatrix3D( const Concrete::UnitFloatTriple & from, const Concrete::UnitFloatTriple & to, gsl_matrix * dst_3_3 );
	}
}

namespace Shapes
{
	namespace Lang
	{
		class Core_Schur_decomposition : public Lang::CoreFunction
		{
		public:
			Core_Schur_decomposition( const char * title )
				: CoreFunction( title, new Kernel::EvaluatedFormals( title, true ) )
			{
				formals_->appendEvaluatedCoreFormal( "tf", Kernel::THE_SLOT_VARIABLE );
				formals_->appendEvaluatedCoreFormal( "rank", Kernel::THE_VOID_VARIABLE );
			}
			virtual void
			call( Kernel::EvalState * evalState, Kernel::Arguments & args, const Ast::SourceLocation & callLoc ) const
			{
				args.applyDefaults( );

				size_t argsi = 1;

				int rank = -1;
				typedef const Lang::Integer RankType;
				RefCountPtr< RankType > rankPtr = Helpers::down_cast_CoreArgument< RankType >( title_, args, argsi, callLoc, true );
				if( rankPtr != NullPtr< RankType >( ) )
					{
						rank = rankPtr->val_;
						if( rank < 0 )
							{
								throw Exceptions::CoreOutOfRange( title_, args, argsi, "Rank is negative." );
							}
					}

				argsi = 0;
				try
					{
						typedef const Lang::Transform2D ArgType;
						RefCountPtr< ArgType > tf = Helpers::try_cast_CoreArgument< ArgType >( args.getValue( argsi ) );
						if( rank > 2 )
							{
								throw Exceptions::CoreOutOfRange( title_, args, argsi, "Rank exceeds dimension." );
							}
						Helpers::Schur_decomposition_helper_2D( evalState, tf, rank, callLoc );
						return;
					}
				catch( const NonLocalExit::NotThisType & ball )
					{
						/* Wrong type; never mind!.. but see below!
						 */
					}

				try
					{
						typedef const Lang::Transform3D ArgType;
						RefCountPtr< ArgType > tf = Helpers::try_cast_CoreArgument< ArgType >( args.getValue( argsi ) );
						if( rank > 3 )
							{
								throw Exceptions::CoreOutOfRange( title_, args, argsi, "Rank exceeds dimension." );
							}
						Helpers::Schur_decomposition_helper_3D( evalState, tf, rank, callLoc );
						return;
					}
				catch( const NonLocalExit::NotThisType & ball )
					{
						/* Wrong type; never mind!.. but see below!
						 */
					}

				throw Exceptions::CoreTypeMismatch( callLoc, title_, args, 0, Helpers::typeSetString( Lang::Transform2D::staticTypeName( ), Lang::Transform3D::staticTypeName( ) ) );
			}
		};
	}
}

namespace Shapes
{
	namespace Helpers
	{
		void
		Schur_decomposition_helper_2D( Kernel::EvalState * evalState, const RefCountPtr< const Lang::Transform2D > & tf, int rank, const Ast::SourceLocation & callLoc )
		{
			throw Exceptions::NotImplemented( "Schur_decomposition_helper_2D" );
		}

		void
		Schur_decomposition_helper_3D( Kernel::EvalState * evalState, const RefCountPtr< const Lang::Transform3D > & tf, int rank, const Ast::SourceLocation & callLoc )
		{
			const size_t N = 3;
			static gsl_eigen_nonsymm_workspace * ws = gsl_eigen_nonsymm_alloc( N );
			gsl_eigen_nonsymm_params( 1, 0, ws );
			static gsl_matrix * L = gsl_matrix_alloc( N, N );
			static gsl_vector * p = gsl_vector_alloc( N );
			static gsl_matrix * Q = gsl_matrix_alloc( N, N );
			static gsl_vector * Qp = gsl_vector_alloc( N );
			static gsl_vector_complex * eval = gsl_vector_complex_alloc( N );
			tf->write_gsl_matrix( L );
			if( gsl_eigen_nonsymm_Z( L, eval, Q, ws ) != 0 )
				{
					throw Exceptions::ExternalError( "gsl_eigen_nonsymm_Z failed.  (This is one of those \"rare occations\"...)" );
				}
			/* Now, we ensure that the upper block triangular matrix is in what we consider canonical form,
			 * namely that any 2x2 block on the diagonal comes before the 1x1 blocks.
			 */
			if( fabs( gsl_matrix_get( L, 1, 0 ) ) < fabs( gsl_matrix_get( L, 2, 1 ) ) )
				{
					gsl_matrix_swap_columns( Q, 0, 2 );
					gsl_matrix_swap_rows( L, 0, 2 );
					gsl_matrix_swap_columns( L, 0, 2 );
					/* Perhaps we should also switch the order of the eigenvalues in eval here, but they seem
					 * unrelated anyway.
					 */
				}
			static gsl_vector * QTp = gsl_vector_alloc( N );
			tf->write_gsl_vector( p );
			gsl_blas_dgemv( CblasTrans, 1, Q, p, 0, QTp );
			if( rank == 0 )
				{
					gsl_vector_set_zero( Qp );
				}
			else
				{
					/* Compute the translational part of the change of coordinates.
					 * The goal is to make the transform as linear as possible in the new coordinates.
					 */
					static gsl_matrix * IsubL = gsl_matrix_alloc( N, N );
					gsl_matrix_set_identity( IsubL );
					gsl_matrix_sub( IsubL, L );
					static gsl_matrix * A = gsl_matrix_alloc( N, N );
					gsl_blas_dgemm( CblasNoTrans, CblasTrans, 1, IsubL, Q, 0, A );
					static gsl_vector * SVD_work = gsl_vector_alloc( N );
					static gsl_vector * sigma = gsl_vector_alloc( N );
					static gsl_matrix * V = gsl_matrix_alloc( N, N );
					gsl_linalg_SV_decomp( A, V, sigma, SVD_work );
					if( rank > 0 )
						{
							for( size_t i = rank; i < N; ++i )
								{
									gsl_vector_set( sigma, i, 0 );
								}
						}
					else
						{
							/* A negative value of rank means that the rank shall be determined automatically.
							 */
							const double lim = 1e-2 * gsl_vector_get( sigma, 0 );
							for( size_t i = 0; i < N; ++i )
								{
									if( fabs( gsl_vector_get( sigma, i ) ) < lim )
										{
											gsl_vector_set( sigma, i, 0 );
										}
								}
						}
					gsl_linalg_SV_solve( A, V, sigma, QTp, Qp );
				}
			/* Given Qp, we now compute the offset of the upper block triangular transform. */
			static gsl_vector * Up = gsl_vector_alloc( N );
			gsl_blas_dcopy( QTp, Up );
			static gsl_vector * QTQq = gsl_vector_alloc( N );
			gsl_blas_dgemv( CblasTrans, 1, Q, Qp, 0, QTQq );
			gsl_vector_sub( Up, QTQq );
			gsl_blas_dgemv( CblasNoTrans, 1, L, QTQq, 1, Up );

			Schur_decomposition_resultFactory.set( "Q", Helpers::newValHandle( new Lang::Transform3D( Q, Qp ) ) );
			Schur_decomposition_resultFactory.set( "U", Helpers::newValHandle( new Lang::Transform3D( L, Up ) ) );

			Kernel::ContRef cont = evalState->cont_;
			cont->takeValue( Schur_decomposition_resultFactory.build( ),
											 evalState );
		}
	}
}

void
Helpers::minRotationMatrix3D( const Concrete::UnitFloatTriple & from, const Concrete::UnitFloatTriple & to, gsl_matrix * dst_3_3 )
{
	/* The rotation maps:
	 *   from -> to
	 *   r    -> r
	 *   a3   -> b3
	 * as defined below.
	 */
	try
		{
			Concrete::UnitFloatTriple r = Concrete::crossDirection( from, to );
			Concrete::UnitFloatTriple a3 = Concrete::crossDirection( from, r );
			Concrete::UnitFloatTriple b3 = Concrete::crossDirection( to, r );
			static gsl_matrix * A = gsl_matrix_alloc( 3, 3 );
			static gsl_matrix * B = gsl_matrix_alloc( 3, 3 );
			gsl_matrix_set( A, 0, 0, from.x_ );
			gsl_matrix_set( A, 1, 0, from.y_ );
			gsl_matrix_set( A, 2, 0, from.z_ );
			gsl_matrix_set( A, 0, 1, r.x_ );
			gsl_matrix_set( A, 1, 1, r.y_ );
			gsl_matrix_set( A, 2, 1, r.z_ );
			gsl_matrix_set( A, 0, 2, a3.x_ );
			gsl_matrix_set( A, 1, 2, a3.y_ );
			gsl_matrix_set( A, 2, 2, a3.z_ );
			gsl_matrix_set( B, 0, 0, to.x_ );
			gsl_matrix_set( B, 1, 0, to.y_ );
			gsl_matrix_set( B, 2, 0, to.z_ );
			gsl_matrix_set( B, 0, 1, r.x_ );
			gsl_matrix_set( B, 1, 1, r.y_ );
			gsl_matrix_set( B, 2, 1, r.z_ );
			gsl_matrix_set( B, 0, 2, b3.x_ );
			gsl_matrix_set( B, 1, 2, b3.y_ );
			gsl_matrix_set( B, 2, 2, b3.z_ );
			gsl_blas_dgemm( CblasNoTrans, CblasTrans, 1, B, A, 0, dst_3_3 );
		}
	catch( const NonLocalExit::CrossDirectionOfParallel & ball )
		{
			gsl_matrix_set_identity( dst_3_3 );
		}
}

namespace Shapes
{
	namespace Lang
	{
		class Core_rotationMapping3D : public Lang::CoreFunction
		{
		public:
			Core_rotationMapping3D( const char * title )
				: CoreFunction( title, new Kernel::EvaluatedFormals( title, true ) )
			{
				formals_->appendEvaluatedCoreFormal( "from", Kernel::THE_SLOT_VARIABLE );
				formals_->appendEvaluatedCoreFormal( "to", Kernel::THE_SLOT_VARIABLE );
			}
			virtual void
			call( Kernel::EvalState * evalState, Kernel::Arguments & args, const Ast::SourceLocation & callLoc ) const
			{
				args.applyDefaults( );

				size_t i = 0;

				typedef const Lang::FloatTriple ArgType;
				RefCountPtr< ArgType > from = Helpers::down_cast_CoreArgument< ArgType >( title_, args, i, callLoc );
				if( from->x_ == 0 && from->y_ == 0 && from->z_ == 0 )
					{
						throw Exceptions::CoreOutOfRange( title_, args, i, "The <from> direction is degenerate, that is (0,0,0)." );
					}

				++i;
				RefCountPtr< ArgType > to = Helpers::down_cast_CoreArgument< ArgType >( title_, args, i, callLoc );
				if( to->x_ == 0 && to->y_ == 0 && to->z_ == 0 )
					{
						throw Exceptions::CoreOutOfRange( title_, args, i, "The <to> direction is degenerate, that is (0,0,0)." );
					}

				gsl_matrix * R = gsl_matrix_alloc( 3, 3 );
				gsl_vector * t = gsl_vector_alloc( 3 );
				Helpers::minRotationMatrix3D( Concrete::UnitFloatTriple( from->x_, from->y_, from->z_ ),
																			Concrete::UnitFloatTriple( to->x_, to->y_, to->z_ ),
																			R );
				gsl_vector_set_zero( t );

				Kernel::ContRef cont = evalState->cont_;
				cont->takeValue( Kernel::ValueRef( new Lang::Transform3D( R, t ) ),
												 evalState );

				gsl_vector_free( t );
				gsl_matrix_free( R );
			}
		};

	}
}

void
Kernel::registerCore_decomp( Kernel::Environment * env )
{
	env->initDefineCoreFunction( new Lang::Core_Schur_decomposition( "Schur_decomp" ) );
	env->initDefineCoreFunction( new Lang::Core_rotationMapping3D( "rotationMap3D" ) );
}
