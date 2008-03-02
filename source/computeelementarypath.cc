#include <cmath>

#include "shapestypes.h"
#include "shapesexceptions.h"
#include "consts.h"
#include "angleselect.h"

#include <stack>

using namespace Shapes;
using namespace std;


void
Lang::CompositePath2D::computeElementaryPath( ) const
{
	if( elementaryPath_ != NullPtr< const Lang::ElementaryPath2D >( ) )
		{
			return;
		}
	Lang::ElementaryPath2D * pth = new Lang::ElementaryPath2D;

	const double GAMMA_ANGLE = 0.5;

	Concrete::Coords2D basePoint( 0, 0 );

	std::stack< const Lang::Value * > nodeStack;
	nodeStack.push( this );
	while( ! nodeStack.empty( ) )
		{
			const Lang::Value * node = nodeStack.top( );
			nodeStack.pop( );
			switch( node->getTypeID( ) )
				{
				SINGLELOOP1( CLASSTREE1_Coords2D, QUICKTYPECASE )
					{
						const Lang::Coords2D * coords = dynamic_cast< const Lang::Coords2D * >( node );
						/* We don't care if the point has an angle since there are no handles.
						 * Perhaps it should be an error.
						 */
						/*
							const Lang::CornerCoords2D * tmp = dynamic_cast< const Lang::CornerCoords2D * >( coords );
							if( tmp != 0 )
							{
								newPoint->defaultAngle = tmp->a_;
							}
						*/
						Concrete::Coords2D * newMid = new Concrete::Coords2D( basePoint, *coords );
						basePoint = *newMid;
						pth->push_back( new Concrete::PathPoint2D( newMid ) );
					}
					break;
				SINGLELOOP1( CLASSTREE1_PathPoint2D, QUICKTYPECASE )
					{
						const Lang::PathPoint2D * pathPoint = reinterpret_cast< const Lang::PathPoint2D * >( node );
						Concrete::Coords2D * newMid = new Concrete::Coords2D( basePoint, *(pathPoint->mid_) );
						basePoint = *newMid;
						Concrete::PathPoint2D * newPoint = new Concrete::PathPoint2D( newMid );
						{
							const Lang::CornerCoords2D * tmp = dynamic_cast< const Lang::CornerCoords2D * >( pathPoint->mid_.getPtr( ) );
							if( tmp != 0 )
								{
									newPoint->defaultAngle_ = tmp->a_;
								}
						}

						if( pathPoint->rear_ != NullPtr< const Lang::Value >( ) )
							{
								newPoint->rear_ = new Concrete::Coords2D( 0, 0 );
								{
									typedef const Lang::Coords2D * VariableHandle;
									VariableHandle typedHandle = dynamic_cast< VariableHandle >( pathPoint->rear_.getPtr( ) );
									if( typedHandle != 0 )
										{
											*(newPoint->rear_) = Concrete::Coords2D( basePoint, *typedHandle );
											Concrete::Length dx = newPoint->rear_->x_ - newPoint->mid_->x_;
											Concrete::Length dy = newPoint->rear_->y_ - newPoint->mid_->y_;
											newPoint->rearAngle_ = atan2( dy.offtype< 1, 0 >( ), dx.offtype< 1, 0 >( ) );
											newPoint->rearModulus_ = hypotPhysical( dx, dy );
											newPoint->rearState_ = Concrete::PathPoint2D::COMPLETE;
											goto doneRear;
										}
								}
								newPoint->rearState_ = Concrete::PathPoint2D::UNFORCED_M;	// This is the normal case, the only exception being rectangular handles
								pth->allComplete_ = false;
								{
									typedef const Lang::PolarHandle2D * VariableHandle;
									VariableHandle typedHandle = dynamic_cast< VariableHandle >( pathPoint->rear_.getPtr( ) );
									if( typedHandle != 0 )
										{
											newPoint->rearModulusPromise_ = typedHandle->rPromise_;
											newPoint->rearAngle_ = typedHandle->a_;
											newPoint->rearState_ |= Concrete::PathPoint2D::COMPLETE;	// This is just a no-op way of saying "keep it UNFORCED_M"
											goto doneRear;
										}
								}
								{
									typedef const Lang::PolarHandle2DFree_a * VariableHandle;
									VariableHandle typedHandle = dynamic_cast< VariableHandle >( pathPoint->rear_.getPtr( ) );
									if( typedHandle != 0 )
										{
											newPoint->rearModulusPromise_ = typedHandle->rPromise_;
											newPoint->rearState_ |= Concrete::PathPoint2D::FREE_ANGLE;
											goto doneRear;
										}
								}
								{
									typedef const Lang::PolarHandle2DFree_r * VariableHandle;
									VariableHandle typedHandle = dynamic_cast< VariableHandle >( pathPoint->rear_.getPtr( ) );
									if( typedHandle != 0 )
										{
											newPoint->rearModulusPromise_ = typedHandle->defaultModulus_;
											newPoint->rearAngle_ = typedHandle->a_;
											newPoint->rearState_ |= Concrete::PathPoint2D::FREE_MODULUS;
											goto doneRear;
										}
								}
								{
									typedef const Lang::PolarHandle2DFree_ra * VariableHandle;
									VariableHandle typedHandle = dynamic_cast< VariableHandle >( pathPoint->rear_.getPtr( ) );
									if( typedHandle != 0 )
										{
											newPoint->rearModulusPromise_ = typedHandle->defaultModulus_;
											newPoint->rearState_ |= Concrete::PathPoint2D::FREE;
											goto doneRear;
										}
								}
								cerr << "Internal error:	Unexpected type found in handle point: " << pathPoint->rear_->getTypeName( ) << endl ;
								exit( 1 );
							}
					doneRear:

						if( pathPoint->front_ != NullPtr< const Lang::Value >( ) )
							{
								newPoint->front_ = new Concrete::Coords2D( 0, 0 );
								{
									typedef const Lang::Coords2D * VariableHandle;
									VariableHandle typedHandle = dynamic_cast< VariableHandle >( pathPoint->front_.getPtr( ) );
									if( typedHandle != 0 )
										{
											*(newPoint->front_) = Concrete::Coords2D( basePoint, *typedHandle );
											Concrete::Length dx = newPoint->front_->x_ - newPoint->mid_->x_;
											Concrete::Length dy = newPoint->front_->y_ - newPoint->mid_->y_;
											newPoint->frontAngle_ = atan2( dy.offtype< 1, 0 >( ), dx.offtype< 1, 0 >( ) );
											newPoint->frontModulus_ = hypotPhysical( dx, dy );
											newPoint->frontState_ = Concrete::PathPoint2D::COMPLETE;
											goto doneFront;
										}
								}
								newPoint->frontState_ = Concrete::PathPoint2D::UNFORCED_M;
								pth->allComplete_ = false;
								{
									typedef const Lang::PolarHandle2D * VariableHandle;
									VariableHandle typedHandle = dynamic_cast< VariableHandle >( pathPoint->front_.getPtr( ) );
									if( typedHandle != 0 )
										{
											newPoint->frontModulusPromise_ = typedHandle->rPromise_;
											newPoint->frontAngle_ = typedHandle->a_;
											newPoint->frontState_ |= Concrete::PathPoint2D::COMPLETE;	// This is just a no-op way of saying "keep it UNFORCED_M"
											goto doneFront;
										}
								}
								{
									typedef const Lang::PolarHandle2DFree_a * VariableHandle;
									VariableHandle typedHandle = dynamic_cast< VariableHandle >( pathPoint->front_.getPtr( ) );
									if( typedHandle != 0 )
										{
											newPoint->frontModulusPromise_ = typedHandle->rPromise_;
											newPoint->frontState_ |= Concrete::PathPoint2D::FREE_ANGLE;
											goto doneFront;
										}
								}
								{
									typedef const Lang::PolarHandle2DFree_r * VariableHandle;
									VariableHandle typedHandle = dynamic_cast< VariableHandle >( pathPoint->front_.getPtr( ) );
									if( typedHandle != 0 )
										{
											newPoint->frontModulusPromise_ = typedHandle->defaultModulus_;
											newPoint->frontAngle_ = typedHandle->a_;
											newPoint->frontState_ |= Concrete::PathPoint2D::FREE_MODULUS;
											goto doneFront;
										}
								}
								{
									typedef const Lang::PolarHandle2DFree_ra * VariableHandle;
									VariableHandle typedHandle = dynamic_cast< VariableHandle >( pathPoint->front_.getPtr( ) );
									if( typedHandle != 0 )
										{
											newPoint->frontModulusPromise_ = typedHandle->defaultModulus_;
											newPoint->frontState_ |= Concrete::PathPoint2D::FREE;
											goto doneFront;
										}
								}
								cerr << "Internal error:	Unexpected type found in handle point: " << pathPoint->front_->getTypeName( ) << endl ;
								exit( 1 );
							}
					doneFront:

						pth->push_back( newPoint );
					}
					break;
				SINGLELOOP1( CLASSTREE1_Path2D, QUICKTYPECASE )
					{
						const Lang::Path2D * subPath = reinterpret_cast< const Lang::Path2D * >( node );
						subPath->elementaryJob( & nodeStack, pth );
					}
					break;
				default:
					cerr << "Internal error:	Unexpected type found in subpath: " << node->getTypeName( ) << endl ;
					exit( 1 );
				}
		}

	if( pth->allComplete_ )
		{
			goto allComplete;
		}


	/* Now, fill in all the missing angles.
	 */

	{
		Lang::ElementaryPath2D::iterator the_rend = pth->begin( );
		--the_rend;
		Lang::ElementaryPath2D::iterator the_rbegin = pth->end( );
		--the_rbegin;


		pth->allComplete_ = true;
		for( Lang::ElementaryPath2D::iterator i = pth->begin( );
				 i != pth->end( );
				 ++i )
			{
				if( ( (*i)->rear_ == (*i)->mid_ &&
							(*i)->front_ == (*i)->mid_ ) ||
						isnan( (*i)->defaultAngle_ ) )
					{
						/* There are no handles at this point.
						 * Set angles in direction to neighboring coordinates.
						 */
						if( (*i)->rearState_ & Concrete::PathPoint2D::FREE_ANGLE )
							{
								Lang::ElementaryPath2D::iterator prev = i;
								--prev;
								if( prev == the_rend )
									{
										prev = the_rbegin;
									}
								Concrete::Length dxRear = (*prev)->mid_->x_ - (*i)->mid_->x_;
								Concrete::Length dyRear = (*prev)->mid_->y_ - (*i)->mid_->y_;
								(*i)->rearAngle_ = atan2( dyRear.offtype< 1, 0 >( ), dxRear.offtype< 1, 0 >( ) );
								(*i)->rearState_ &= ~Concrete::PathPoint2D::FREE_ANGLE;
							}
						if( (*i)->frontState_ & Concrete::PathPoint2D::FREE_ANGLE )
							{
								Lang::ElementaryPath2D::iterator next = i;
								++next;
								if( next == pth->end( ) )
									{
										next = pth->begin( );
									}
								Concrete::Length dxFront = (*next)->mid_->x_ - (*i)->mid_->x_;
								Concrete::Length dyFront = (*next)->mid_->y_ - (*i)->mid_->y_;
								(*i)->frontAngle_ = atan2( dyFront.offtype< 1, 0 >( ), dxFront.offtype< 1, 0 >( ) );
								(*i)->frontState_ &= ~Concrete::PathPoint2D::FREE_ANGLE;
							}
						continue;
					}

				if( (*i)->front_ == (*i)->mid_ )
					{
						/* There is only a rear handle at this point */
						if( (*i)->rearState_ & Concrete::PathPoint2D::UNFORCED_A )
							{
								/* Put code for forcing the angle here
								 * ...
								 */
							}

						if( (*i)->rearState_ & ( Concrete::PathPoint2D::FREE_MODULUS | Concrete::PathPoint2D::UNFORCED_M ) )
							{
								pth->allComplete_ = false;
							}

						if( ! ( (*i)->rearState_ & Concrete::PathPoint2D::FREE_ANGLE ) )
							{
								continue;
							}
						(*i)->rearState_ &= ~Concrete::PathPoint2D::FREE_ANGLE;

						Lang::ElementaryPath2D::iterator next = i;
						++next;
						if( next == pth->end( ) )
							{
								if( closed_ )
									{
										next = pth->begin( );
									}
								else
									{
										/* In this case we do something completely different.
										 */
										Lang::ElementaryPath2D::iterator prev = i;
										--prev;
										if( prev == the_rend )
											{
												throw Exceptions::MiscellaneousRequirement( "The angles of a singleton pathpoint cannot be determined." );
											}
										Concrete::Length dxRear = (*prev)->mid_->x_ - (*i)->mid_->x_;
										Concrete::Length dyRear = (*prev)->mid_->y_ - (*i)->mid_->y_;
										(*i)->rearAngle_ = atan2( dyRear.offtype< 1, 0 >( ), dxRear.offtype< 1, 0 >( ) );
										(*i)->frontAngle_ = (*i)->rearAngle_ + M_PI + (*i)->defaultAngle_;    /* Both angles must always be set, even where there is no handle. */
										if( ! ( (*i)->rearState_ & ( Concrete::PathPoint2D::FREE_MODULUS | Concrete::PathPoint2D::UNFORCED_M ) ) )
											{
												(*i)->rear_->x_ = (*i)->mid_->x_ + (*i)->rearModulus_ * cos( (*i)->rearAngle_ );
												(*i)->rear_->y_ = (*i)->mid_->y_ + (*i)->rearModulus_ * sin( (*i)->rearAngle_ );
												(*i)->rearState_ = Concrete::PathPoint2D::COMPLETE;
											}
										continue;
									}
							}
						Concrete::Length dxFront = (*next)->mid_->x_ - (*i)->mid_->x_;
						Concrete::Length dyFront = (*next)->mid_->y_ - (*i)->mid_->y_;
						(*i)->frontAngle_ = atan2( dyFront.offtype< 1, 0 >( ), dxFront.offtype< 1, 0 >( ) );  /* Both angles must always be set, even where there is no handle. */
						(*i)->rearAngle_ = (*i)->frontAngle_ + M_PI - (*i)->defaultAngle_;
						if( ! ( (*i)->rearState_ & ( Concrete::PathPoint2D::FREE_MODULUS | Concrete::PathPoint2D::UNFORCED_M ) ) )
							{
								(*i)->rear_->x_ = (*i)->mid_->x_ + (*i)->rearModulus_ * cos( (*i)->rearAngle_ );
								(*i)->rear_->y_ = (*i)->mid_->y_ + (*i)->rearModulus_ * sin( (*i)->rearAngle_ );
								(*i)->rearState_ = Concrete::PathPoint2D::COMPLETE;
							}
						continue;
					}
				if( (*i)->rear_ == (*i)->mid_ )
					{
						/* There is only a front handle at this point */

						if( (*i)->frontState_ & Concrete::PathPoint2D::UNFORCED_A )
							{
								/* Put code for forcing the angle here
								 * ...
								 */
							}

						if( (*i)->frontState_ & ( Concrete::PathPoint2D::FREE_MODULUS | Concrete::PathPoint2D::UNFORCED_M ) )
							{
								pth->allComplete_ = false;
							}

						if( ! ( (*i)->frontState_ & Concrete::PathPoint2D::FREE_ANGLE ) )
							{
								continue;
							}
						(*i)->frontState_ &= ~Concrete::PathPoint2D::FREE_ANGLE;

						Lang::ElementaryPath2D::iterator prev = i;
						--prev;
						if( prev == the_rend )
							{
								if( closed_ )
									{
										prev = the_rbegin;
									}
								else
									{
										/* In this case we do something completely different.
										 */
										Lang::ElementaryPath2D::iterator next = i;
										++next;
										if( next == pth->end( ) )
											{
												throw Exceptions::MiscellaneousRequirement( "The angles of a singleton pathpoint cannot be determined." );
											}
										Concrete::Length dxFront = (*next)->mid_->x_ - (*i)->mid_->x_;
										Concrete::Length dyFront = (*next)->mid_->y_ - (*i)->mid_->y_;
										(*i)->frontAngle_ = atan2( dyFront.offtype< 1, 0 >( ), dxFront.offtype< 1, 0 >( ) );
										(*i)->rearAngle_ = (*i)->frontAngle_ + M_PI - (*i)->defaultAngle_;  /* Both angles must always be set, even where there is no handle. */
										if( ! ( (*i)->frontState_ & ( Concrete::PathPoint2D::FREE_MODULUS | Concrete::PathPoint2D::UNFORCED_M ) ) )
											{
												(*i)->front_->x_ = (*i)->mid_->x_ + (*i)->frontModulus_ * cos( (*i)->frontAngle_ );
												(*i)->front_->y_ = (*i)->mid_->y_ + (*i)->frontModulus_ * sin( (*i)->frontAngle_ );
												(*i)->frontState_ = Concrete::PathPoint2D::COMPLETE;
											}
										continue;
									}
							}
						Concrete::Length dxRear = (*prev)->mid_->x_ - (*i)->mid_->x_;
						Concrete::Length dyRear = (*prev)->mid_->y_ - (*i)->mid_->y_;
						(*i)->rearAngle_ = atan2( dyRear.offtype< 1, 0 >( ), dxRear.offtype< 1, 0 >( ) );  /* Both angles must always be set, even where there is no handle. */
						(*i)->frontAngle_ = (*i)->rearAngle_ + M_PI + (*i)->defaultAngle_;
						if( ! ( (*i)->frontState_ & ( Concrete::PathPoint2D::FREE_MODULUS | Concrete::PathPoint2D::UNFORCED_M ) ) )
							{
								(*i)->front_->x_ = (*i)->mid_->x_ + (*i)->frontModulus_ * cos( (*i)->frontAngle_ );
								(*i)->front_->y_ = (*i)->mid_->y_ + (*i)->frontModulus_ * sin( (*i)->frontAngle_ );
								(*i)->frontState_ = Concrete::PathPoint2D::COMPLETE;
							}
						continue;
					}


				/* We reach here if there are two handles at the point */

				if( (*i)->rearState_ & Concrete::PathPoint2D::UNFORCED_A )
					{
						/* Put code for forcing the angle here
						 * ...
						 */
					}

				if( (*i)->frontState_ & Concrete::PathPoint2D::UNFORCED_A )
					{
						/* Put code for forcing the angle here
						 * ...
						 */
					}

				if( ( (*i)->rearState_ | (*i)->frontState_ ) & ( Concrete::PathPoint2D::FREE_MODULUS | Concrete::PathPoint2D::UNFORCED_M ) )
					{
						pth->allComplete_ = false;
					}

				if( ! ( (*i)->rearState_ & Concrete::PathPoint2D::FREE_ANGLE ) &&
						! ( (*i)->frontState_ & Concrete::PathPoint2D::FREE_ANGLE ) )
					{
						/* Both handles have specified angles */
						continue;
					}
				if( ! ( (*i)->rearState_ & Concrete::PathPoint2D::FREE_ANGLE ) )
					{
						/* The rear handle has a specified angle, that may propagate to the front handle */
						if( (*i)->frontState_ & Concrete::PathPoint2D::FREE_ANGLE )
							{
								(*i)->frontAngle_ = (*i)->rearAngle_ + M_PI + (*i)->defaultAngle_;
								(*i)->frontState_ &= ~Concrete::PathPoint2D::FREE_ANGLE;
							}
						continue;
					}
				if( ! ( (*i)->frontState_ & Concrete::PathPoint2D::FREE_ANGLE ) )
					{
						/* Analogous to above */
						if( (*i)->rearState_ & Concrete::PathPoint2D::FREE_ANGLE )
							{
								(*i)->rearAngle_ = (*i)->frontAngle_ + M_PI - (*i)->defaultAngle_;
								(*i)->rearState_ &= ~Concrete::PathPoint2D::FREE_ANGLE;
							}
						continue;
					}

				/* None of the handles have specified angles */

				Lang::ElementaryPath2D::iterator prev = i;
				--prev;
				if( prev == the_rend )
					{
						prev = the_rbegin;
					}
				Lang::ElementaryPath2D::iterator next = i;
				++next;
				if( next == pth->end( ) )
					{
						next = pth->begin( );
					}

				Concrete::Length dxRear = (*prev)->mid_->x_ - (*i)->mid_->x_;
				Concrete::Length dyRear = (*prev)->mid_->y_ - (*i)->mid_->y_;
				Concrete::Length dxFront = (*next)->mid_->x_ - (*i)->mid_->x_;
				Concrete::Length dyFront = (*next)->mid_->y_ - (*i)->mid_->y_;
				double angleRear = atan2( dyRear.offtype< 1, 0 >( ), dxRear.offtype< 1, 0 >( ) );
				double angleFront = atan2( dyFront.offtype< 1, 0 >( ), dxFront.offtype< 1, 0 >( ) );
				double weightRear = pow( hypot( dxFront.offtype< 1, 0 >( ), dyFront.offtype< 1, 0 >( ) ), GAMMA_ANGLE );
				double weightFront = pow( hypot( dxRear.offtype< 1, 0 >( ), dyRear.offtype< 1, 0 >( ) ), GAMMA_ANGLE );
				double a = angleSelectNorm2( angleRear, angleFront - ( M_PI + (*i)->defaultAngle_ ), weightRear, weightFront );
				(*i)->rearAngle_ = a;
				(*i)->frontAngle_ = a + M_PI + (*i)->defaultAngle_;

				(*i)->frontState_ &= ~Concrete::PathPoint2D::FREE_ANGLE;
				(*i)->rearState_ &= ~Concrete::PathPoint2D::FREE_ANGLE;

			}

	} // Done filling in angles.


	/* Now, complete the path by filling in all the handle radii.
	 */

	{
		Lang::ElementaryPath2D::iterator the_rend = pth->begin( );
		--the_rend;
		Lang::ElementaryPath2D::iterator the_rbegin = pth->end( );
		--the_rbegin;

		pth->allComplete_ = true;
		for( Lang::ElementaryPath2D::iterator i = pth->begin( );
				 i != pth->end( );
				 ++i )
			{
				if( (*i)->rear_ == (*i)->mid_ &&
						(*i)->front_ == (*i)->mid_ )
					{
						/* There are no handles at this point */

						continue;
					}

				Lang::ElementaryPath2D::iterator prev = i;
				--prev;
				if( prev == the_rend )
					{
						prev = the_rbegin;
					}
				Lang::ElementaryPath2D::iterator next = i;
				++next;
				if( next == pth->end( ) )
					{
						next = pth->begin( );
					}

				if( (*i)->front_ == (*i)->mid_ )
					{
						/* There is only a rear handle at this point */
						if( (*i)->rearState_ == Concrete::PathPoint2D::COMPLETE )
							{
								continue;
							}
						if( ( (*i)->rearState_ & Concrete::PathPoint2D::UNFORCED_M ) != 0)
							{
								(*i)->rearModulus_ = (*i)->rearModulusPromise_->force( *prev, *i, true );
							}
						(*i)->rear_->x_ = (*i)->mid_->x_ + (*i)->rearModulus_ * cos( (*i)->rearAngle_ );
						(*i)->rear_->y_ = (*i)->mid_->y_ + (*i)->rearModulus_ * sin( (*i)->rearAngle_ );
						continue;
					}
				if( (*i)->rear_ == (*i)->mid_ )
					{
						/* There is only a front handle at this point */
						if( (*i)->frontState_ == Concrete::PathPoint2D::COMPLETE )
							{
								continue;
							}
						if( ( (*i)->frontState_ & Concrete::PathPoint2D::UNFORCED_M ) != 0 )
							{
								(*i)->frontModulus_ = (*i)->frontModulusPromise_->force( *i, *next, false );
							}
						(*i)->front_->x_ = (*i)->mid_->x_ + (*i)->frontModulus_ * cos( (*i)->frontAngle_ );
						(*i)->front_->y_ = (*i)->mid_->y_ + (*i)->frontModulus_ * sin( (*i)->frontAngle_ );
						continue;
					}

				/* We reach here if there are two handles at the point */

				if( (*i)->rearState_ != Concrete::PathPoint2D::COMPLETE )
					{
						if( ! ( (*i)->rearState_ & Concrete::PathPoint2D::FREE_MODULUS ) )
							{
								if( ( (*i)->rearState_ & Concrete::PathPoint2D::UNFORCED_M ) != 0 )
									{
										(*i)->rearModulus_ = (*i)->rearModulusPromise_->force( *prev, *i, true );
									}
								(*i)->rear_->x_ = (*i)->mid_->x_ + (*i)->rearModulus_ * cos( (*i)->rearAngle_ );
								(*i)->rear_->y_ = (*i)->mid_->y_ + (*i)->rearModulus_ * sin( (*i)->rearAngle_ );
								(*i)->rearState_ = Concrete::PathPoint2D::COMPLETE;
							}
					}

				if( (*i)->frontState_ != Concrete::PathPoint2D::COMPLETE )
					{
						if( ! ( (*i)->frontState_ & Concrete::PathPoint2D::FREE_MODULUS ) )
							{
								if( ( (*i)->frontState_ & Concrete::PathPoint2D::UNFORCED_M ) != 0)
									{
										(*i)->frontModulus_ = (*i)->frontModulusPromise_->force( *i, *next, false );
									}
								(*i)->front_->x_ = (*i)->mid_->x_ + (*i)->frontModulus_ * cos( (*i)->frontAngle_ );
								(*i)->front_->y_ = (*i)->mid_->y_ + (*i)->frontModulus_ * sin( (*i)->frontAngle_ );
								(*i)->frontState_ = Concrete::PathPoint2D::COMPLETE;
							}
					}

				if( (*i)->rearState_ == Concrete::PathPoint2D::COMPLETE &&
						(*i)->frontState_ == Concrete::PathPoint2D::COMPLETE )
					{
						continue;
					}

				/* The only reason why a handle is not complete at this point is that it's modulus
				 * has not been determined.
				 */
				if( (*i)->rearState_ == Concrete::PathPoint2D::COMPLETE )
					{
						/* The rear modulus is known, and may propagate to the other side. */
						if( (*i)->frontState_ & Concrete::PathPoint2D::FREE_MODULUS )
							{
								(*i)->frontModulus_ = (*i)->rearModulus_;
								(*i)->front_->x_ = (*i)->mid_->x_ + (*i)->frontModulus_ * cos( (*i)->frontAngle_ );
								(*i)->front_->y_ = (*i)->mid_->y_ + (*i)->frontModulus_ * sin( (*i)->frontAngle_ );
							}
						continue;
					}
				if( (*i)->frontState_ == Concrete::PathPoint2D::COMPLETE )
					{
						/* Analogous to the above */
						if( (*i)->rearState_ & Concrete::PathPoint2D::FREE_MODULUS )
							{
								(*i)->rearModulus_ = (*i)->frontModulus_;
								(*i)->rear_->x_ = (*i)->mid_->x_ + (*i)->rearModulus_ * cos( (*i)->rearAngle_ );
								(*i)->rear_->y_ = (*i)->mid_->y_ + (*i)->rearModulus_ * sin( (*i)->rearAngle_ );
							}
						continue;
					}

				/* We get here if both modulus are free.
				 * We first compute madulus for each individually, and then merge the two values somehow to make both equal in the end.
				 */

				double rearModulus = (*i)->rearModulusPromise_->force( *prev, *i, true );
				double frontModulus = (*i)->frontModulusPromise_->force( *i, *next, false );
				double theModulus = min( rearModulus, frontModulus );
				(*i)->rearModulus_ = theModulus;
				(*i)->frontModulus_ = theModulus;
				(*i)->front_->x_ = (*i)->mid_->x_ + (*i)->frontModulus_ * cos( (*i)->frontAngle_ );
				(*i)->front_->y_ = (*i)->mid_->y_ + (*i)->frontModulus_ * sin( (*i)->frontAngle_ );
				(*i)->rear_->x_ = (*i)->mid_->x_ + (*i)->rearModulus_ * cos( (*i)->rearAngle_ );
				(*i)->rear_->y_ = (*i)->mid_->y_ + (*i)->rearModulus_ * sin( (*i)->rearAngle_ );

			}

	} // Closing scope of the_rend and the_rbegin


 allComplete:

	if( closed_ )
		{
			pth->close( );
		}

	elementaryPath_ = RefCountPtr< const Lang::ElementaryPath2D >( pth );

}


