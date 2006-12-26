#include <cmath>

#include "metapdftypes.h"
#include "metapdfexceptions.h"
#include "metapdfastexpr.h"
#include "consts.h"
#include "angleselect.h"
#include "metapdfastvar.h"
#include "metapdfastclass.h"
#include "statetypes.h"
#include "lighttypes.h"
#include "shadingtypes.h"
#include "globals.h"

#include <ctype.h>
#include <list>
#include <algorithm>

using namespace MetaPDF;


RefCountPtr< const Lang::Drawable2D >
Lang::ZSorter::typed_to2D( const Kernel::PassedDyn & dyn, const Lang::Transform3D & tf, const RefCountPtr< const Lang::Drawable3D > & self ) const
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
      throw Exceptions::MiscellaneousRequirement( "Shadow lights cannot be used in a z-sorter." );
    }

  // There is one queue per original object.  This structure will be where all the
  // triangles resides at the beginning of the treatment of each shadow light or the
  // view occlusion computation.
  //
  // We begin by initializing these queues.
  typedef std::list< Computation::ZBufTriangle > CompoundObject;

  // This variable is only used teporarily; in the end, it will be empty and
  // there will be no pointers in it that needs to be freed.
  const size_t pileSize = pile_->size( );
  std::vector< CompoundObject * > triangleMem;
  triangleMem.reserve( pileSize );
  {
    typedef typeof *pile_ PileType;
    for( PileType::const_iterator src = pile_->begin( ); src != pile_->end( ); ++src )
      {
	std::list< Computation::ZBufTriangle > * triangleQueue = new std::list< Computation::ZBufTriangle >;
	(*src)->push_zBufTriangles( tf, eyez, triangleQueue );
	triangleMem.push_back( triangleQueue );
      }
  }

  // ==============================================
  // When sorting the objects, each list of ZBufTriable objects is treated as a single object.
  // I keep track of which objects an object has to be drawn after.  The objects that are not
  // waiting for other objects to be drawn are kept in a special list.
  // To do this efficiently, each object need to know how many objects it is waiting for to be 
  // drawn.  It must also know what objects to update when it has been drawn.  An object is identified
  // by the pointer to its list, so the additional information must be kept in a separate structure.
  // As usual, objects are identified by their position in a memory vector.
  // At this state, "to draw" an object means to place it in triangleLists.
  std::list< CompoundObject * > triangleLists;
  {
    std::vector< size_t > waitCounters;
    waitCounters.resize( pileSize, 0 );
    std::vector< std::list< size_t > > waitingObjects;
    waitingObjects.resize( pileSize );

    // We begin by setting up waitingObjects and waitCounters.
    // This involves testing each object against all other objects.
    for( size_t i0 = 0; i0 < pileSize - 1; ++i0 )
      {
	CompoundObject * obj0 = triangleMem[ i0 ];
	for( size_t i1 = i0 + 1; i1 < pileSize; ++i1 )
	  {
	    CompoundObject * obj1 = triangleMem[ i1 ];
	    bool overlaps = false;
	    {
	      for( CompoundObject::const_iterator t0 = obj0->begin( ); t0 != obj0->end( ); ++t0 )
		{
		  for( CompoundObject::const_iterator t1 = obj1->begin( ); t1 != obj1->end( ); ++t1 )
		    {
		      if( t0->overlaps( *t1 ) )
			{
			  Concrete::Coords2D commonPoint( 0, 0 );
			  if( ! t0->overlaps( *t1, & commonPoint, Computation::theTrixelizeOverlapTol ) )
			    {
			      // Too little overlap.
			      continue;
			    }
			  if( t0->isOnTopOfAt( *t1, commonPoint ) )
			    {
			      waitingObjects[ i1 ].push_back( i0 );
			      ++waitCounters[ i0 ];
			    }
			  else
			    {
			      waitingObjects[ i0 ].push_back( i1 );
			      ++waitCounters[ i1 ];
			    }
			  goto foundOverlap;
			}
		    }
		}
	    }
	  foundOverlap:
	    overlaps = overlaps; // A no-op to jump to.
	  }
      }

    // Then the queue of objects to be "drawn" is initialized.
    std::list< size_t > drawQueue;
    {
      std::vector< size_t >::iterator wi = waitCounters.begin( );
      for( size_t i = 0; i < pileSize; ++i, ++wi )
	{
	  if( *wi == 0 )
	    {
	      drawQueue.push_back( i );
	    }
	}
    }

    size_t drawnCount = 0;
    while( drawQueue.size( ) > 0 )
      {
	size_t i = drawQueue.front( );
	drawQueue.pop_front( );
	++drawnCount;

	if( triangleMem[ i ] == 0 )
	  {
	    throw Exceptions::InternalError( "Attempt to draw an object again!" );
	  }
	triangleLists.push_back( triangleMem[ i ] );
	triangleMem[ i ] = 0;

	const std::list< size_t > & waitList = waitingObjects[ i ];
	for( std::list< size_t >::const_iterator j = waitList.begin( ); j != waitList.end( ); ++j )
	  {
	    --waitCounters[ *j ];
	    if( waitCounters[ *j ] == 0 )
	      {
		drawQueue.push_back( *j );
	      }
	  }
      }
    if( drawnCount < pileSize )
      {
	throw Exceptions::MiscellaneousRequirement( "It is suspected that you placed objects with cyclic overlaps in a z-sorter.  That's forbidden.  The z-buffer is the solution if you really need cyclic overlaps." );
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

      typedef typeof triangleLists ListType;
      for( ListType::const_iterator i = triangleLists.begin( ); i != triangleLists.end( ); ++i )
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
      
      typedef typeof triangleLists ListType;
      for( ListType::const_iterator i = triangleLists.begin( ); i != triangleLists.end( ); ++i )
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
      
      typedef typeof triangleLists ListType;
      for( ListType::iterator i = triangleLists.begin( ); i != triangleLists.end( ); ++i )
	{
	  if( (*i)->size( ) == 0 )
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


  while( triangleLists.size( ) > 0 )
    {
      delete triangleLists.back( );
      triangleLists.pop_back( );
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

