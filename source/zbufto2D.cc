#include <cmath>

#include "shapestypes.h"
#include "shapesexceptions.h"
#include "astexpr.h"
#include "consts.h"
#include "angleselect.h"
#include "astvar.h"
#include "astclass.h"
#include "statetypes.h"
#include "lighttypes.h"
#include "shadingtypes.h"
#include "globals.h"

#include <ctype.h>
#include <list>
#include <algorithm>

using namespace Shapes;

#define SPLICEDEBUG( code ) // code


RefCountPtr< const Lang::Drawable2D >
Lang::ZBuf::typed_to2D( const Kernel::PassedDyn & dyn, const Lang::Transform3D & tf, const RefCountPtr< const Lang::Drawable3D > & self ) const
{
	Concrete::Length eyez = dyn->getEyeZ( );

	std::list< RefCountPtr< const Lang::LightSource > > commonLights;
	std::list< RefCountPtr< const Lang::LightSource > > allShadowLights;

	{
		typedef typeof *lightPile_ PileType;
		for( PileType::const_iterator src = lightPile_->begin( ); src != lightPile_->end( ); ++src )
			{
				if( (*src)->shadows( ) )
					{
						allShadowLights.push_back( (*src)->typed_transformed( tf ) );
					}
				else
					{
						commonLights.push_back( (*src)->typed_transformed( tf ) );
					}
			}
	}

	if( allShadowLights.size( ) > 0 )
		{
			throw Exceptions::NotImplemented( "Shadow lights in ZBuf::typed_to2D" );
		}

	size_t debugCounter = 0;

	// There is one queue per original object.	This structure will be where all the
	// triangles resides at the beginning of the treatment of each shadow light or the
	// view occlusion computation.
	//
	// We begin by initializing these queues.
	std::list< std::list< Computation::ZBufTriangle > * > triangleQueues;
	{
		typedef typeof *pile_ PileType;
		for( PileType::const_iterator src = pile_->begin( ); src != pile_->end( ); ++src )
			{
				std::list< Computation::ZBufTriangle > * triangleQueue = new std::list< Computation::ZBufTriangle >;
				(*src)->push_zBufTriangles( tf, eyez, triangleQueue );
				triangleQueues.push_back( triangleQueue );
			}
	}

	// The vectors triangleQueue and disjointTriangles share indices in the sense that same indices points to
	// triangles with the same origin.
	std::list< std::list< Computation::ZBufTriangle > * > disjointTriangles;
	std::list< std::list< Computation::ZBufTriangle > * > occludedTriangles;

	for( std::list< std::list< Computation::ZBufTriangle > * >::iterator src = triangleQueues.begin( );
			 src != triangleQueues.end( ); ++src )
		{
			std::list< Computation::ZBufTriangle > * triangleQueue = *src;

			std::list< Computation::ZBufTriangle > * currentDisjoint = new std::list< Computation::ZBufTriangle >;
			std::list< Computation::ZBufTriangle > * currentOccluded = new std::list< Computation::ZBufTriangle >;

			while( triangleQueue->size( ) > 0 )
				{
					SPLICEDEBUG( std::cerr << "====( Debug step number " << debugCounter << " )====" << std::endl );
					Computation::ZBufTriangle current = triangleQueue->front( );
					triangleQueue->pop_front( );

					typedef typeof disjointTriangles ListType;
					bool foundOverlap = false;
					ListType::iterator iOccluded = occludedTriangles.begin( ); 
					for( ListType::iterator iDisjoint = disjointTriangles.begin( ); iDisjoint != disjointTriangles.end( );
							 ++iDisjoint, ++iOccluded )
						{
							typedef typeof **iDisjoint SubListType;
							for( SubListType::iterator j = (*iDisjoint)->begin( ); j != (*iDisjoint)->end( ); ++j )
								{
									if( current.overlaps( *j ) )
										{
											Computation::ZBufTriangle tOld( *j );
											(*iDisjoint)->erase( j );
											Computation::ZBufTriangle::splice( tOld, current,
																												 *iDisjoint, *iOccluded,
																												 currentDisjoint, currentOccluded,
																												 triangleQueue );
											mergeTriangles( *iDisjoint );
											recombineTriangles( *iOccluded );
											foundOverlap = true;
											goto doublebreak;
										}
								}
						}
				doublebreak:
					if( ! foundOverlap )
						{
							currentDisjoint->push_back( current );
						}
				}

			mergeTriangles( currentDisjoint );
			recombineTriangles( currentDisjoint );
			disjointTriangles.push_back( currentDisjoint );

			mergeTriangles( currentOccluded );
			recombineTriangles( currentOccluded );
			occludedTriangles.push_back( currentOccluded );

			++debugCounter;
			if( debugCounter >= Interaction::debugStep )
				{
					//					break;
				}
		}

	{
		size_t sum = 0;
		typedef typeof disjointTriangles ListType;
		for( ListType::const_iterator i = disjointTriangles.begin( ); i != disjointTriangles.end( ); ++i )
			{
				sum += (*i)->size( );
			}
	}

	// It is now time to take care of the lines.
	// The first thing we shall do is to remove what is occluded by triangles.
	// Note that triangles will be drawn before the lines, and that is the only way lines
	// can occlude triangles.
	// Then, we make sure that the lines are drawn in back-to-front order, clipping occluded lines
	// behind the line occluding them to avoid cyclic locking.

	std::list< const Computation::ZBufLine * > lineQueue;
	{
		typedef typeof *linePile_ PileType;
		for( PileType::const_iterator src = linePile_->begin( ); src != linePile_->end( ); ++src )
			{
				(*src)->push_zBufLine( tf, eyez, & lineQueue );
			}
	}

	std::list< const Computation::ZBufLine * > disjointLines;
	while( lineQueue.size( ) > 0 )
		{

			std::list< const Computation::ZBufLine * > queue1;
			std::list< const Computation::ZBufLine * > queue2;

			std::list< const Computation::ZBufLine * > * currentQueue = & queue1;
			std::list< const Computation::ZBufLine * > * otherQueue = & queue2;

			currentQueue->push_back( lineQueue.front( ) );
			lineQueue.pop_front( );

			typedef typeof disjointTriangles ListType;
			for( ListType::const_iterator i = disjointTriangles.begin( ); i != disjointTriangles.end( ); ++i )
				{
					typedef typeof **i SubListType;
					for( SubListType::iterator j = (*i)->begin( ); j != (*i)->end( ); ++j )
						{
							while( currentQueue->size( ) > 0 )
								{
									const Computation::ZBufLine * currentLine = currentQueue->front( );
									currentQueue->pop_front( );
									if( currentLine->overlaps( *j ) )
										{
											currentLine->splice( *j, otherQueue );
											delete currentLine;
										}
									else
										{
											otherQueue->push_back( currentLine );
										}
								}

							{
								// swap the queues
								std::list< const Computation::ZBufLine * > * tmp = currentQueue;
								currentQueue = otherQueue;
								otherQueue = tmp;
							}
						}
				}

			// When we reach here the line segments are in currentQueue.
			// These segments are ready to be inserted in the line z buffer.

			// The algorithm below is inefficient, because it will generally make the overlap comparison
			// several times for any overlaping pair of line segments.
			while( currentQueue->size( ) > 0 )
				{
					const Computation::ZBufLine * current = currentQueue->front( );
					currentQueue->pop_front( );

					bool foundOverlap = false;
					typedef typeof disjointLines ListType;
					for( ListType::iterator i = disjointLines.begin( ); i != disjointLines.end( );
							 ++i )
						{
							if( current->overlaps( **i ) )
								{
									disjointLines.erase( i );
									// Note that splice is responsible for deleting that of *i and current which is not used.
									Computation::ZBufLine::splice( *i, current,
																								 & disjointLines,
																								 currentQueue );
									foundOverlap = true;
									break;
								}
						}
					if( ! foundOverlap )
						{
							disjointLines.push_back( current );
						}
				}
		}


	RefCountPtr< const Lang::Group2D > res = Lang::THE_NULL2D;

	const bool SHOW_TRIXELS = false;
	if( SHOW_TRIXELS )
		{
			// This is a debug mode

			typedef typeof disjointTriangles ListType;
			for( ListType::const_iterator i = disjointTriangles.begin( ); i != disjointTriangles.end( ); ++i )
				{
					typedef typeof **i SubListType;
					for( SubListType::iterator j = (*i)->begin( ); j != (*i)->end( ); ++j )
						{
							res = RefCountPtr< const Lang::Group2D >( new Lang::GroupPair2D( j->debugFrame( ),
																																							 res,
																																							 metaState_ ) );
						}
				}
		}
	else
		{
			// This is the standard mode

			size_t commonLightCount = commonLights.size( );

			typedef typeof disjointTriangles ListType;
			for( ListType::iterator i = disjointTriangles.begin( ); i != disjointTriangles.end( ); ++i )
				{
					if( (*i)->empty( ) )
						{
							continue;
						}
					typedef std::list< RefCountPtr< const Lang::LightSource > > ShadowLightListType;
					const ShadowLightListType & shadowLights = (*i)->front( ).shadowLights_;
					for( ShadowLightListType::const_iterator l = shadowLights.begin( ); l != shadowLights.end( ); ++l )
						{
							commonLights.push_back( *l );
						}
					RefCountPtr< const Lang::PaintedPolygon2D > tmp = (*i)->front( ).painter_->polygon_to2D( dyn, tf, commonLights );
					while( commonLights.size( ) > commonLightCount )
						{
							commonLights.pop_back( );
						}

					// The following statement will consume the objects in the list pointed to by *i
					res = RefCountPtr< const Lang::Group2D >( new Lang::GroupPair2D( tmp->clip( *i, tmp ),
																																					 res,
																																					 metaState_ ) );
				}
		}


	while( disjointTriangles.size( ) > 0 )
		{
			delete disjointTriangles.back( );
			disjointTriangles.pop_back( );

			delete occludedTriangles.back( );
			occludedTriangles.pop_back( );
		}

	while( triangleQueues.size( ) > 0 )
		{
			delete triangleQueues.back( );
			triangleQueues.pop_back( );
		}


	// Finally we draw the lines.
	while( disjointLines.size( ) > 0 )
		{
			const Computation::ZBufLine * current = disjointLines.front( );
			disjointLines.pop_front( );
			res = RefCountPtr< const Lang::Group2D >( new Lang::GroupPair2D( current->stroke2D( ),
																																			 res,
																																			 metaState_ ) );
			delete current;
		}


	return res;
}

