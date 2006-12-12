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

  size_t debugCounter = 0;

  // There is one queue per original object.  This structure will be where all the
  // triangles resides at the beginning of the treatment of each shadow light or the
  // view occlusion computation.
  //
  // We begin by initializing these queues.
  std::list< std::list< Computation::ZBufTriangle > * > triangleLists;
  {
    typedef typeof *pile_ PileType;
    for( PileType::const_iterator src = pile_->begin( ); src != pile_->end( ); ++src )
      {
	std::list< Computation::ZBufTriangle > * triangleQueue = new std::list< Computation::ZBufTriangle >;
	(*src)->push_zBufTriangles( tf, eyez, triangleQueue );
	triangleLists.push_back( triangleQueue );
      }
  }

  // ==============================================

  // The sort goes here.

  // ==============================================

  
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

