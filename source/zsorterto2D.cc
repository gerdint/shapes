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
#include "zbufinternals.h"
#include "constructorrepresentation.h"

#include <ctype.h>
#include <list>
#include <algorithm>

using namespace MetaPDF;

namespace MetaPDF
{
  namespace Computation
  {
    
    struct SplitJob
    {
      // This is not the most natural place to define CompoundObject, but since we need it...
      typedef std::list< Computation::ZBufTriangle > CompoundObject;

      CompoundObject::iterator splitted_;
      std::vector< CompoundObject * >::iterator splittingObject_;
      CompoundObject::iterator splitting_;
      
      SplitJob( const CompoundObject::iterator & splitted, const std::vector< CompoundObject * >::iterator & splittingObject, const CompoundObject::iterator & splitting )
	: splitted_( splitted ), splittingObject_( splittingObject ), splitting_( splitting )
      { }
    };
  }
}


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
  typedef Computation::SplitJob::CompoundObject CompoundObject;  // It is a bit unnatural to define the type somewhere but here...

  const size_t pileSize = pile_->size( );
  size_t triangleCount = 0;

  // Now the main storage for triangles is set up
  typedef PtrOwner_back_Access< std::vector< CompoundObject * > > CompoundMemType;
  CompoundMemType compoundMem;
  compoundMem.reserve( pileSize );

  // Then we fill it with triangles that don't intersect.
  // Note that it is a responsibility of push_zBufTriangles to not push any tiny triangles.
  {
    Computation::SplicingLine spliceLine;
    std::list< Computation::SplitJob > job;
    typedef typeof *pile_ PileType;
    for( PileType::const_iterator src = pile_->begin( ); src != pile_->end( ); ++src )
      {
	std::list< Computation::ZBufTriangle > * tmpList = new std::list< Computation::ZBufTriangle >;
	// Since there cannot be shadow lights in a z sorter, it is OK not to include the backsides of single sided facets.
	(*src)->push_zBufTriangles( tf, eyez, tmpList, true ); // true means allow dropping back sides of single sided objects
	for( CompoundObject::iterator i = tmpList->begin( ); i != tmpList->end( ); ++i )
	  {
	    typedef std::vector< CompoundObject * >::iterator ObjectIterator;
	    for( ObjectIterator o = compoundMem.begin( ); o != compoundMem.end( ); ++o )
	      {
		for( CompoundObject::iterator j = (*o)->begin( ); j != (*o)->end( ); ++j )
		  {
		    if( j->intersection( *i, & spliceLine ) &&
			j->overlapsAlong( *i, spliceLine, Computation::theTrixelizeOverlapTol ) )
		      {
			job.push_back( Computation::SplitJob( i, o, j ) );
			goto splitFound1;
		      }
		  }
	      }
	  splitFound1:
	    continue; // this is just a no-op to make this a valid jump destination
	  }
	while( job.size( ) != 0 )
	  {
	    Computation::SplitJob & currentJob = job.front( );
	    if( ! currentJob.splitted_->intersection( *currentJob.splitting_, & spliceLine ) )
	      {
		throw Exceptions::InternalError( "These triangles were known to intsersect, and now they're parallel!" );
	      }
	    
	    // New triangles are added to the end of tmpList, and the iterator to the first new triangle is returned.
	    CompoundObject::iterator i = currentJob.splitted_->spliceAlong( spliceLine, tmpList );

	    // Now the procedure from above is repeated, but not comparing against triangles that have already been checked.
	    for( ; i != tmpList->end( ); ++i )
	      {
		CompoundObject::iterator j = currentJob.splitting_;
		++j;
		typedef std::vector< CompoundObject * >::iterator ObjectIterator;
		for( ObjectIterator o = currentJob.splittingObject_; o != compoundMem.end( ); ++o )
		  {
		    // For the splitting object object (the first in this loop), the first triangle to compare against
		    // was set above.  For the other object, all triangles shall be compared against (that is, j starts from
		    // the beginning).
		    if( o != currentJob.splittingObject_ )
		      {
			j = (*o)->begin( );
		      }
		    for( ; j != (*o)->end( ); ++j )
		      {
			if( j->intersection( *i, & spliceLine ) &&
			    j->overlapsAlong( *i, spliceLine, Computation::theTrixelizeOverlapTol ) )
			  {
			    job.push_back( Computation::SplitJob( i, o, j ) );
			    goto splitFound2;
			  }
		      }
		  }
	      splitFound2:
		continue; // this is just a no-op to make this a valid jump destination
	      }

	    tmpList->erase( currentJob.splitted_ );
	    job.pop_front( );
	  }
	compoundMem.push_back( tmpList );
	triangleCount += tmpList->size( );
      }
  }


  // In order to be able to deal with the triangles efficiently from here on, I put them all in a long vector.
  // This has one drawback, though, being that we will lose track of the fact that triangles belonging to the same
  // facet does not have to be checked for depth order.  However, I count on them not being ordered thanks to the
  // tolerance used in the overlap test.
  std::vector< const Computation::ZBufTriangle * > triangleMem;
  triangleMem.reserve( triangleCount );
  for( std::vector< CompoundObject * >::iterator o = compoundMem.begin( ); o != compoundMem.end( ); ++o )
    {
      for( CompoundObject::iterator j = (*o)->begin( ); j != (*o)->end( ); ++j )
	{
	  triangleMem.push_back( & *j );
	}
    }
  if( triangleMem.size( ) != triangleCount )
    {
      throw Exceptions::InternalError( "Some triangles are missing!" );
    }


  // I keep track of which objects an object has to be drawn after.  The objects that are not
  // waiting for other objects to be drawn are kept in a special list.
  // To do this efficiently, each object need to know how many objects it is waiting for to be 
  // drawn.  It must also know what objects to update when it has been drawn.  An object is identified
  // by the pointer to its list, so the additional information must be kept in a separate structure.
  // As usual, objects are identified by their position in a memory vector.
  // At this state, "to draw" an object means to place it in drawOrder.

  std::list< const Computation::ZBufTriangle * > drawOrder;
  if( triangleCount > 0 )  // This avoids a bug that would otherwise occur due to computing triangleCount - 1
    {
      std::vector< size_t > waitCounters;
      waitCounters.resize( triangleCount, 0 );
      std::vector< std::list< size_t > > waitingObjects;
      waitingObjects.resize( triangleCount );
      
      // We begin by setting up waitingObjects and waitCounters.
      // This involves testing each object against all other objects.
      for( size_t i0 = 0; i0 < triangleCount - 1; ++i0 )  // Ugly bug if triangleCount == 0
	{
	  const Computation::ZBufTriangle * t0 = triangleMem[ i0 ];
	  for( size_t i1 = i0 + 1; i1 < triangleCount; ++i1 )
	    {
	      const Computation::ZBufTriangle * t1 = triangleMem[ i1 ];
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
		}
	    }
	}
      
      // Then the queue of objects to be "drawn" is initialized.
      std::list< size_t > drawQueue;
      std::map< const Computation::PaintedPolygon3D *, size_t > trianglesInPolygon;
      {
	std::vector< size_t >::iterator wi = waitCounters.begin( );
	for( size_t i = 0; i < triangleCount; ++i, ++wi )
	  {
	    if( *wi == 0 )
	      {
		drawQueue.push_back( i );
		++trianglesInPolygon[ triangleMem[ i ]->painter_ ];
	      }
	  }
      }
      
      // Now, when the triangles are "drawn", that is, placed in drawOrder, they shall be placed so
      // in such a way that triangles that belong to the same polygon appear in sequence as often as possible.
      // While there is probably accurate ways to treat this problem, I resort to some simple heuristics here.
      // The question is basically how to pick a polygon among those than contain a triangle that may be drawn.

      size_t drawnCount = 0;
      while( drawnCount < triangleCount )
	{
	  while( drawQueue.size( ) > 0 )
	    {
	      // First, use find the polygon whith the most triangles in drawQueue.
	      const Computation::PaintedPolygon3D * painter = 0;

	      // Note that triangles that belong to the same polygon will never overlap, and hence drawing triangles that belong
	      // to painter will not make other triangles that belong to painter ready to be drawn.  This algorithm depends on
	      // this!

	      size_t bestCount = 0;
	      {
		typedef typeof trianglesInPolygon MapType;
		MapType::iterator best_j;
		for( MapType::iterator j = trianglesInPolygon.begin( ); j != trianglesInPolygon.end( ); ++j )
		  {
		    if( j->second > bestCount )
		      {
			bestCount = j->second;
			best_j = j;
		      }
		  }
		painter = best_j->first;
		trianglesInPolygon.erase( best_j ); // If we need this again it will be re-created.
	      }
	      
	      // Soon, bestCount triangles will have been drawn, but the value of bestCount will then be destroyed.
	      drawnCount += bestCount;

	      // There shall be bestCount triangles to draw, so by counting the number of triangles we find we may not
	      // have to search all of drawQueue.
	      for( std::list< size_t >::iterator ii = drawQueue.begin( ); bestCount > 0; )
		{
		  size_t i = *ii;
		  if( triangleMem[ i ]->painter_ != painter )
		    {
		      ++ii;
		      continue;
		    }

		  std::list< size_t >::iterator erase_i = ii;
		  ++ii;
		  drawQueue.erase( erase_i );
		  --bestCount;

		  drawOrder.push_back( triangleMem[ i ] );
		  triangleMem[ i ] = 0;
		  
		  const std::list< size_t > & waitList = waitingObjects[ i ];
		  for( std::list< size_t >::const_iterator j = waitList.begin( ); j != waitList.end( ); ++j )
		    {
		      --waitCounters[ *j ];
		      if( waitCounters[ *j ] == 0 )
			{
			  drawQueue.push_back( *j );
			  // Note that it is assumed that ( triangleMem[ *j ]->painter_ != painter ) !
// 			  if( triangleMem[ *j ]->painter_ == painter )
// 			    {
// 			      throw Exceptions::InternalError( "Triangle waiting for another triangle in the same polygon!" );
// 			    }
			  ++trianglesInPolygon[ triangleMem[ *j ]->painter_ ];
			}
		    }
		  
		}
	      
	    }
	  if( drawnCount < triangleCount )
	    {
	      // Resolve cyclic overlap the hard way...
	      std::cerr << "A cyclic overlap situation in a z sorter required a small triangle to be drawn too deep." << std::endl ;
	      Concrete::Area bestArea = HUGE_VAL;
	      size_t besti;
	      // This search is a somewhat expensive since we must search the whole list.
	      typedef typeof triangleMem ListType;
	      ListType::iterator ti = triangleMem.begin( );
	      for( size_t i = 0; i < triangleCount; ++i, ++ti )
		{
		  if( *ti == 0 )
		    {
		      continue;
		    }
		  Concrete::Area tmpArea = (*ti)->area( );
		  if( tmpArea < bestArea )
		    {
		      bestArea = tmpArea;
		      besti = i;
		    }
		}
	      drawQueue.push_back( besti );
	      ++trianglesInPolygon[ triangleMem[ besti ]->painter_ ];
	      waitCounters[ besti ] = 0;  // This will avoid that this is drawn again, since the counter will never _reach_ 0 now.
	    }
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

      typedef typeof drawOrder ListType;
      for( ListType::const_iterator i = drawOrder.begin( ); i != drawOrder.end( ); ++i )
	{
	  while( currentQueue->size( ) > 0 )
	    {
	      const Computation::ZBufLine * currentLine = currentQueue->front( );
	      currentQueue->pop_front( );
	      if( currentLine->overlaps( **i ) )
		{
		  currentLine->splice( **i, otherQueue );
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
      
      typedef typeof drawOrder ListType;
      for( ListType::const_iterator i = drawOrder.begin( ); i != drawOrder.end( ); ++i )
	{
	  res = RefCountPtr< const Lang::Group2D >( new Lang::GroupPair2D( (*i)->debugFrame( ),
									   res,
									   metaState_ ) );
	}
    }
  else
    {
      // This is the standard mode

      // Remember that the grouping of triangles into polygons can probably be improved!

      typedef typeof drawOrder ListType;
      for( ListType::iterator i = drawOrder.begin( ); i != drawOrder.end( ); ) // Note that i shall not be incremented here.
	{
	  std::list< Computation::ZBufTriangle > regions;
	  // This is a hideous copy!
	  regions.push_back( **i );
	  
	  // Now we'll simply join the triangle **i with the immediately following triangles sharing the same painter.
	  // The job of joining triangles thus consists in placing them in a good order in drawOrder.

	  const Computation::PaintedPolygon3D * painter = (*i)->painter_;
	  ++i;
	  for( ; i != drawOrder.end( ) && (*i)->painter_ == painter; ++i )
	    {
	      // Another hideous copy!
	      regions.push_back( **i );
	    }

	  // If this would have been a z-buffer, we should add the common lights to the shadow lights here,
	  // but in a z-sorter there are no shadow lights.
	  RefCountPtr< const Lang::PaintedPolygon2D > tmp = painter->polygon_to2D( dyn, tf, commonLights );

	  // The following statement will consume the objects in regions.
	  res = RefCountPtr< const Lang::Group2D >( new Lang::GroupPair2D( tmp->clip( & regions, tmp ),
									   res,
									   metaState_ ) );
	}
    }

  
  // Memory cleanup of the triangles should be taken care of by compoundMem.
  

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

