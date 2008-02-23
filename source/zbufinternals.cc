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
#include "trianglefunctions.h"
#include "constructorrepresentation.h"

#include <ctype.h>
#include <list>
#include <algorithm>

using namespace Shapes;

namespace Shapes
{
	namespace Lang
	{

	class ZBuf::IndexRepresentation
	{
		char end_;
		bool pushed_;
	public:
		size_t i0;
		size_t i1;
		size_t i2;

		IndexRepresentation( );
		void push_back( size_t i );
		Lang::ZBuf::PolyIndices toPolyIndices( const std::vector< Concrete::Coords2D > & cornerMem ) const;
		size_t getDifferent( size_t notThis1, size_t notThis2 ) const;
		void addEdges( Computation::UndirectedEdgeMatrix< std::pair< bool, std::list< Lang::ZBuf::IndexRepresentation * > > > * edgeMatrix, std::list< Computation::UndirectedEdge > * jobQueue, const std::vector< Concrete::Coords2D > & cornerMem );
		void removeEdges( Computation::UndirectedEdgeMatrix< std::pair< bool, std::list< Lang::ZBuf::IndexRepresentation * > > > * edgeMatrix );
		void pushTriangle( std::list< Computation::ZBufTriangle > * triangles, const std::vector< Concrete::Coords2D > & cornerMem, const Computation::PaintedPolygon3D * painter, const RefCountPtr< const Computation::ZBufTriangle::ZMap > zMap );
		void show( std::ostream & os ) const;
		void show( std::ostream & os, const std::vector< Concrete::Coords2D > & cornerMem ) const;
		Computation::UndirectedEdge getEdge( char pos ) const;
		size_t getDifferent( char pos ) const;
		bool isMember( const size_t i ) const;
	};
	}
}

Lang::ZBuf::IndexRepresentation::IndexRepresentation( )
	: end_( 0 ), pushed_( false )
{ }

void
Lang::ZBuf::IndexRepresentation::push_back( size_t i )
{
	switch( end_ )
		{
		case 0:
			i0 = i;
			++end_;
			break;
		case 1:
			i1 = i;
			++end_;
			break;
		case 2:
			i2 = i;
			++end_;
			break;
		default:
			throw Exceptions::InternalError( "IndexRepresentation overflow" );
		}
}

Lang::ZBuf::PolyIndices
Lang::ZBuf::IndexRepresentation::toPolyIndices( const std::vector< Concrete::Coords2D > & cornerMem ) const
{
	PolyIndices res;
	const Concrete::Coords2D d1 = cornerMem[ i1 ] - cornerMem[ i0 ];
	const Concrete::Coords2D d2 = cornerMem[ i2 ] - cornerMem[ i0 ];
	if( d1.x_ * d2.y_ - d1.y_ * d2.x_ > 0 )
		{
			// we have counter-clockwise ordering
			res.push_back( i0 );
			res.push_back( i1 );
			res.push_back( i2 );
		}
	else
		{
			res.push_back( i0 );
			res.push_back( i2 );
			res.push_back( i1 );
		}
	return res;
}

size_t
Lang::ZBuf::IndexRepresentation::getDifferent( size_t notThis1, size_t notThis2 ) const
{
	if( i0 != notThis1 && i0 != notThis2 )
		{
			return i0;
		}
	if( i1 != notThis1 && i1 != notThis2 )
		{
			return i1;
		}
	return i2;
}

bool
Lang::ZBuf::IndexRepresentation::isMember( const size_t i ) const
{
	switch( end_ )
		{
		case 0:
			return false;
		case 1:
			return i == i0;
		case 2:
			return i == i0 || i == i1;
		case 3:
			return i == i0 || i == i1 || i == i2;
		default:
			throw Exceptions::InternalError( "IndexRepresentation end out of range." );
		}
}

void
Lang::ZBuf::IndexRepresentation::addEdges( Computation::UndirectedEdgeMatrix< std::pair< bool, std::list< Lang::ZBuf::IndexRepresentation * > > > * edgeMatrix, std::list< Computation::UndirectedEdge > * jobQueue, const std::vector< Concrete::Coords2D > & cornerMem )
{
	// When adding edges, we must be prepared to accept that the triangle may be rejected at any edge because it
	// competes with a bigger triangle on the same side of the edge.

	// We should remember that the triangle may first knock out other triangles when being added to the first edges,
	// and then be knocked out itself at a later edge.	Then it seems that the triangles it knocked out first shouldn't
	// have been knocked out.	However, they wouldn't have been knocked out if they were not very small, so it's
	// probably not a big loss anyway.

	if( ! addEdge( edgeMatrix, Computation::UndirectedEdge( i0, i1 ), this, jobQueue, cornerMem ) )
		{
			return;
		}
	if( ! addEdge( edgeMatrix, Computation::UndirectedEdge( i1, i2 ), this, jobQueue, cornerMem ) )
		{
			// We know that this was push_back'ed, so we don't have to search using removeEdge.
			{
				std::pair< bool, std::list< Lang::ZBuf::IndexRepresentation * > > & p = (*edgeMatrix)[ Computation::UndirectedEdge( i0, i1 ) ];
				p.first = false;
				p.second.pop_back( );
			}
			return;
		}
	if( ! addEdge( edgeMatrix, Computation::UndirectedEdge( i2, i0 ), this, jobQueue, cornerMem ) )
		{
			// We know that this was push_back'ed, so we don't have to search using removeEdge.
			{
				std::pair< bool, std::list< Lang::ZBuf::IndexRepresentation * > > & p = (*edgeMatrix)[ Computation::UndirectedEdge( i0, i1 ) ];
				p.first = false;
				p.second.pop_back( );
			}
			{
				std::pair< bool, std::list< Lang::ZBuf::IndexRepresentation * > > & p = (*edgeMatrix)[ Computation::UndirectedEdge( i1, i2 ) ];
				p.first = false;
				p.second.pop_back( );
			}
			return;
		}
}

void
Lang::ZBuf::IndexRepresentation::removeEdges( Computation::UndirectedEdgeMatrix< std::pair< bool, std::list< Lang::ZBuf::IndexRepresentation * > > > * edgeMatrix )
{
	removeEdge( edgeMatrix, Computation::UndirectedEdge( i0, i1 ), this );
	removeEdge( edgeMatrix, Computation::UndirectedEdge( i1, i2 ), this );
	removeEdge( edgeMatrix, Computation::UndirectedEdge( i2, i0 ), this );
}

void
Lang::ZBuf::IndexRepresentation::pushTriangle( std::list< Computation::ZBufTriangle > * triangles, const std::vector< Concrete::Coords2D > & cornerMem, const Computation::PaintedPolygon3D * painter, const RefCountPtr< const Computation::ZBufTriangle::ZMap > zMap )
{
	if( pushed_ )
		{
			return;
		}
	pushed_ = true;

	triangles->push_back( Computation::ZBufTriangle( painter,
																							 zMap,
																							 cornerMem[ i0 ], cornerMem[ i1 ], cornerMem[ i2 ] ) );
}

void
Lang::ZBuf::IndexRepresentation::show( std::ostream & os ) const
{
	switch( end_ )
		{
		case 0:
			os << "No points" ;
			break;
		case 1:
			os << "1 point:	" << i0 ;
			break;
		case 2:
			os << "2 points: " << i0 << " -- " << i1 ;
			break;
		case 3:
			os << "3 points: " << i0 << " -- " << i1 << " -- " << i2 ;
			break;
		default:
			os << "end_ out of range!" ;
		}
}

void
Lang::ZBuf::IndexRepresentation::show( std::ostream & os, const std::vector< Concrete::Coords2D > & cornerMem ) const
{
	switch( end_ )
		{
		case 0:
			os << "|** No points" ;
			break;
		case 1:
			os << "|** 1 point:	" << cornerMem[ i0 ] ;
			break;
		case 2:
			os << "|** 2 points: " << cornerMem[ i0	] << " -- " << cornerMem[ i1 ] ;
			break;
		case 3:
			os << "@<< @width:0.3bp | stroke [] " << Helpers::shapesFormat( cornerMem[ i0 ] ) << "--" << Helpers::shapesFormat( cornerMem[ i1 ] ) << "--" << Helpers::shapesFormat( cornerMem[ i2 ] ) << "--cycle" << std::endl ;
			break;
		default:
			os << "end_ out of range!" ;
		}
}

Computation::UndirectedEdge
Lang::ZBuf::IndexRepresentation::getEdge( char pos ) const
{
	switch( pos )
		{
		case 0:
			return Computation::UndirectedEdge( i0, i1 );
		case 1:
			return Computation::UndirectedEdge( i1, i2 );
		case 2:
			return Computation::UndirectedEdge( i2, i0 );
		default:
			throw Exceptions::InternalError( "getEdge: pos out of range." );
		}
}

size_t
Lang::ZBuf::IndexRepresentation::getDifferent( char pos ) const
{
	switch( pos )
		{
		case 0:
			return i2;
		case 1:
			return i0;
		case 2:
			return i1;
		default:
			throw Exceptions::InternalError( "getEdge: pos out of range." );
		}
}


void
Lang::ZBuf::mergeTriangles( std::list< Computation::ZBufTriangle > * triangles )
{
	if( triangles->size( ) <= 1 )
		{
			return;
		}

	const Computation::PaintedPolygon3D * painter = triangles->front( ).painter_;
	const RefCountPtr< const Computation::ZBufTriangle::ZMap > zMap = triangles->front( ).zMap_;

	std::vector< Lang::ZBuf::IndexRepresentation > indexRepMem;
	indexRepMem.reserve( 2 * triangles->size( ) ); // This is crucial!

	// One needs to be careful here.	The reuse of points based on distances being less than the tolerance
	// may cause two points on the same triangle to be approximated by the very _same_ point.	This will
	// lead to a degenerate triangle, and must be avoided in order to prevent confusion later on
	// in the algorithm.

	// Even worse, the triangles being "disjoint" does not mean they are perfectly disjoint (perhaps due to some
	// point approximations somewhere), but only overlaping by at most some tolerance value.	Hence, we may
	// even find triangles included in other triangles and similar phenomena.
	// This would be interesting to design better with more powerful invariants so that one could guarantee
	// stronger properties.	However, as the current state of affairs when writing this desperate note is
	// that we may find ourselves with more than two triangles sharing the same edge!
	// The solution to this problem is to discard, on each side of the edge (a zero tolerance test is used to determine the side
	// of a triangle) discard all but the triangle with the biggest area.	Luckily, this can be done incrementaly by
	// always keeping track of the biggest triangle on each side.
	// Need I say it feels like this algorithm is running out of control?

	std::vector< Concrete::Coords2D > cornerMem;
	while( ! triangles->empty( ) )
		{
			const std::vector< Concrete::Coords2D > & points = triangles->front( ).points_;
			indexRepMem.push_back( IndexRepresentation( ) );
			Lang::ZBuf::IndexRepresentation & current = indexRepMem.back( );
			for( std::vector< Concrete::Coords2D >::const_iterator i = points.begin( ); i != points.end( ); ++i )
				{
					Concrete::Coords2D tmp = *i;
					bool reuse = false;
					size_t memIdx = 0;
					typedef typeof cornerMem MemType;
					for( MemType::const_iterator j = cornerMem.begin( ); j != cornerMem.end( ); ++j, ++memIdx )
						{
							if( ( tmp - *j ).norm( ) < Computation::theTrixelizeSplicingTol )
								{
									if( current.isMember( memIdx ) )
										{
											// This is the error situation when we are just about to reuse a point which is already part of this triangle.
											// The solution is to abort the creation of the current triangle.
											indexRepMem.pop_back( );
											goto abortCurrent;
										}
									reuse = true;
									current.push_back( memIdx );
									break;
								}
						}
					if( ! reuse )
						{
							current.push_back( cornerMem.size( ) );
							cornerMem.push_back( tmp );
						}
				}
		abortCurrent:
			triangles->pop_front( );
		}

	const size_t pointCount = cornerMem.size( );

	Computation::UndirectedEdgeMatrix< std::pair< bool, std::list< Lang::ZBuf::IndexRepresentation * > > >
		edgeMatrix( pointCount,
								std::pair< bool, std::list< Lang::ZBuf::IndexRepresentation * > >( false,
																																									 std::list< Lang::ZBuf::IndexRepresentation * >( ) ) );

	std::list< Computation::UndirectedEdge > jobQueue;

	{
		typedef typeof indexRepMem ListType;
		for( ListType::iterator i = indexRepMem.begin( ); i != indexRepMem.end( ); ++i )
			{
				i->addEdges( & edgeMatrix, & jobQueue, cornerMem );
			}
	}

	while( ! jobQueue.empty( ) )
		{
			Computation::UndirectedEdge edge = jobQueue.front( );
			jobQueue.pop_front( );

			// Elements can be removed from the jobQueue by unflagging them; only flagged elements that appear in the jobQueue
			// are really waiting to be processed.
			if( ! edgeMatrix[ edge ].first )
				{
					continue;
				}
			edgeMatrix[ edge ].first = false;	// Flag that this is no longer waiting to be processed.

			typedef std::list< Lang::ZBuf::IndexRepresentation * > ListType;
			ListType & meetingTriangles = edgeMatrix[ edge ].second;

			if( meetingTriangles.size( ) < 2 )
				{
					// One situation I can think of when this could happen is when three triangles have been 
					// found to be on the same side of the edge during addEdge.	Then two of them are removed,
					// but the edge is still in the jobQueue since it was queued when the second triange was added
					// to the edge.
					continue;
				}

			if( meetingTriangles.size( ) > 2 )
				{
					std::cerr << "|** Triangles in queue: " << std::endl ;
					for( ListType::const_iterator i = meetingTriangles.begin( ); i != meetingTriangles.end( ); ++i )
						{
							(*i)->show( std::cerr, cornerMem );
							std::cerr << std::endl ;
						}
					std::ostringstream oss;
					oss << "Only two (not " << meetingTriangles.size( ) << ") edges can share a pair of points!" ;
					throw Exceptions::InternalError( oss );
				}

			Concrete::Coords2D cornerLow = cornerMem[ edge.low( ) ];
			Concrete::Coords2D cornerHigh = cornerMem[ edge.high( ) ];

			ListType::iterator src = meetingTriangles.begin( );
			Lang::ZBuf::IndexRepresentation * triangle1 = *src;
			Concrete::Coords2D differentPoint1 = cornerMem[ triangle1->getDifferent( edge.low( ), edge.high( ) ) ];

			++src;
			Lang::ZBuf::IndexRepresentation * triangle2 = *src;
			Concrete::Coords2D differentPoint2 = cornerMem[ triangle2->getDifferent( edge.low( ), edge.high( ) ) ];
			bool aligned = false;
			size_t keepPoint = 0;	/* Initialized with dummy value only because it is hard for the compiler to see that it won't be used uninitialized. */
			if( areAligned( cornerLow, differentPoint1, differentPoint2 ) )
				{
					aligned = true;
					keepPoint = edge.high( );
				}
			else if( areAligned( cornerHigh, differentPoint1, differentPoint2 ) )
				{
					aligned = true;
					keepPoint = edge.low( );
				}

			if( aligned )
				{
					triangle1->removeEdges( & edgeMatrix );
					triangle2->removeEdges( & edgeMatrix );
					indexRepMem.push_back( IndexRepresentation( ) );
					Lang::ZBuf::IndexRepresentation & current = indexRepMem.back( );
					current.push_back( triangle1->getDifferent( edge.low( ), edge.high( ) ) );
					current.push_back( triangle2->getDifferent( edge.low( ), edge.high( ) ) );
					current.push_back( keepPoint );
					current.addEdges( & edgeMatrix, & jobQueue, cornerMem );
				}
		}

	for( Computation::UndirectedEdge edge = Computation::UndirectedEdge::begin( ); edge != Computation::UndirectedEdge::end( ); edge.increment( pointCount ) )
		{
			std::list< Lang::ZBuf::IndexRepresentation * > & lst = edgeMatrix[ edge ].second;
			for( std::list< Lang::ZBuf::IndexRepresentation * >::iterator i = lst.begin( ); i != lst.end( ); ++i )
				{
					// The IndexRepresentation keeps a state to ensure that is is really pushed only once.
					(*i)->pushTriangle( triangles, cornerMem, painter, zMap );
				}
		}
}

// recombineTriangles generalizes mergeTriangles, but is more expensive and should be called less often.
void
Lang::ZBuf::recombineTriangles( std::list< Computation::ZBufTriangle > * mergedTriangles )
{
	if( mergedTriangles->size( ) <= 1 )
		{
			return;
		}

	const Computation::PaintedPolygon3D * painter = mergedTriangles->front( ).painter_;
	const RefCountPtr< const Computation::ZBufTriangle::ZMap > zMap = mergedTriangles->front( ).zMap_;

	std::list< Lang::ZBuf::IndexRepresentation > indexRepMem;

	std::vector< Concrete::Coords2D > cornerMem;
	while( ! mergedTriangles->empty( ) )
		{
			const std::vector< Concrete::Coords2D > & points = mergedTriangles->front( ).points_;
			indexRepMem.push_back( IndexRepresentation( ) );
			Lang::ZBuf::IndexRepresentation & current = indexRepMem.back( );
			for( std::vector< Concrete::Coords2D >::const_iterator i = points.begin( ); i != points.end( ); ++i )
				{
					Concrete::Coords2D tmp = *i;
					bool reuse = false;
					size_t memIdx = 0;
					typedef typeof cornerMem MemType;
					for( MemType::const_iterator j = cornerMem.begin( ); j != cornerMem.end( ); ++j, ++memIdx )
						{
							if( ( tmp - *j ).norm( ) < Computation::theTrixelizeSplicingTol )
								{
									if( current.isMember( memIdx ) )
										{
											// This is the error situation when we are just about to reuse a point which is already part of this triangle.
											// The solution is to abort the creation of the current triangle.
											indexRepMem.pop_back( );
											goto abortCurrent;
										}
									reuse = true;
									current.push_back( memIdx );
									break;
								}
						}
					if( ! reuse )
						{
							current.push_back( cornerMem.size( ) );
							cornerMem.push_back( tmp );
						}
				}
		abortCurrent:
			mergedTriangles->pop_front( );
		}

	const size_t pointCount = cornerMem.size( );

	std::vector< PolyIndices > polyMem;
	polyMem.reserve( indexRepMem.size( ) );	// this is crucial to avoid reallocation!

	Computation::UndirectedEdgeMatrix< std::list< PolyIndices * > > edgeMatrix( pointCount );

	while( ! indexRepMem.empty( ) )
	{
		bool foundExtension = false;
		typedef typeof indexRepMem TrianglesType;
		for( TrianglesType::iterator i = indexRepMem.begin( ); i != indexRepMem.end( ) && ! foundExtension; ++i )
			{
				for( char e = 0; e < 3; ++e )
					{
						Computation::UndirectedEdge edge = i->getEdge( e );
						std::list< PolyIndices * > & polys = edgeMatrix[ edge ];
						if( polys.size( ) == 1 )	// By the way, it is an error if the size is greater than 1
							{
								if( ! extendPoly( polys.front( ), edge, i->getDifferent( e ), & edgeMatrix, cornerMem, true ) ) // true for convex
									{
										continue;
									}
								foundExtension = true;
								indexRepMem.erase( i );
								break;
							}
					}
			}
		if( ! foundExtension )
			{
				polyMem.push_back( indexRepMem.front( ).toPolyIndices( cornerMem ) );
				indexRepMem.pop_front( );
				PolyIndices * newPoly = & polyMem.back( );
				addEdges( newPoly, & edgeMatrix );
			}
	}

	while( ! polyMem.empty( ) )
		{
			PolyIndices & poly = polyMem.back( );
			// First we remove unnecessary points on straight segments
			if( poly.size( ) > 3 )
				{
					for( PolyIndices::iterator i0 = poly.begin( ); i0 != poly.end( ); ++i0 )
						{
							Concrete::Coords2D p0 = cornerMem[ *i0 ];
							PolyIndices::iterator i1 = i0;
							++i1;
							if( i1 == poly.end( ) )
								{
									i1 = poly.begin( );
								}
							Concrete::Coords2D p1 = cornerMem[ *i1 ];
							PolyIndices::iterator i2 = i1;
							++i2;
							if( i2 == poly.end( ) )
								{
									i2 = poly.begin( );
								}
							Concrete::Coords2D p2 = cornerMem[ *i2 ];

							// We do the usual inscribed circle test to test alignment
							while( Computation::triangleArea( p0, p1, p2 ) < Computation::theTrixelizeOverlapTol * Computation::triangleSemiPerimeter( p0, p1, p2 ) )
								{
									PolyIndices::iterator iRemove = i1;
									i1 = i2;
									p1 = p2;
									poly.erase( iRemove );
									if( poly.size( ) < 3 )
										{
											goto done;
										}
									++i2;
									if( i2 == poly.end( ) )
										{
											i2 = poly.begin( );
										}
									p2 = cornerMem[ *i2 ];
								}
						}
				done:
					if( poly.size( ) < 3 )
						{
							polyMem.pop_back( );
							continue;
						}
				}


			PolyIndices::const_iterator src = poly.begin( );
			size_t i0 = *src;
			++src;
			size_t i1 = *src;
			++src;
			for( ; src != poly.end( ); ++src )
				{
					size_t i2 = *src;
					mergedTriangles->push_back( Computation::ZBufTriangle( painter,
																														 zMap,
																														 cornerMem[ i0 ], cornerMem[ i1 ], cornerMem[ i2 ] ) );
					i1 = i2;
				}
			polyMem.pop_back( );
		}

}

void
Lang::ZBuf::trianglesToPolys( std::list< Computation::ZBufTriangle > * triangles, Lang::Clipped2D * dst )
{
	if( triangles->empty( ) )
		{
			return;
		}

	const bool DEBUGME = false;
	if( DEBUGME )
	{
		while( ! triangles->empty( ) )
			{
				const std::vector< Concrete::Coords2D > & points = triangles->front( ).points_;

				Lang::ElementaryPath2D * newPath = new Lang::ElementaryPath2D;

				for( std::vector< Concrete::Coords2D >::const_iterator src = points.begin( ); src != points.end( ); ++src )
				{
					newPath->push_back( new Concrete::PathPoint2D( new Concrete::Coords2D( *src ) ) );
				}
				newPath->close( );
				dst->addSubPath( RefCountPtr< const Lang::ElementaryPath2D >( newPath ) );

				triangles->pop_front( );
			}
		return;
	}

	std::list< Lang::ZBuf::IndexRepresentation > indexRepMem;
	std::vector< Concrete::Coords2D > cornerMem;

	{
		std::list< Lang::ZBuf::IndexRepresentation > indexRepQueue;
		while( ! triangles->empty( ) )
			{
				const std::vector< Concrete::Coords2D > & points = triangles->front( ).points_;
				indexRepQueue.push_back( IndexRepresentation( ) );
				Lang::ZBuf::IndexRepresentation & current = indexRepQueue.back( );
				for( std::vector< Concrete::Coords2D >::const_iterator i = points.begin( ); i != points.end( ); ++i )
					{
						Concrete::Coords2D tmp = *i;
						bool reuse = false;
						size_t memIdx = 0;
						typedef typeof cornerMem MemType;
						for( MemType::const_iterator j = cornerMem.begin( ); j != cornerMem.end( ); ++j, ++memIdx )
							{
								if( ( tmp - *j ).norm( ) < Computation::theTrixelizeSplicingTol )
									{
										if( current.isMember( memIdx ) )
											{
												// This is the error situation when we are just about to reuse a point which is already part of this triangle.
												// The solution is to abort the creation of the current triangle.
												indexRepMem.pop_back( );
												goto abortCurrent;
											}
										reuse = true;
										current.push_back( memIdx );
										break;
									}
							}
						if( ! reuse )
							{
								current.push_back( cornerMem.size( ) );
								cornerMem.push_back( tmp );
							}
					}
			abortCurrent:
				triangles->pop_front( );
			}

		while( ! indexRepQueue.empty( ) )
			{
				Lang::ZBuf::IndexRepresentation & current = indexRepQueue.front( );

				bool wasSplit = false;
				for( char e = 0; e < 3 && ! wasSplit; ++e )
					{
						Computation::UndirectedEdge edge = current.getEdge( e );
						size_t iLow = edge.low( );
						size_t iHigh = edge.high( );
						size_t iOther = current.getDifferent( e );
						Concrete::Coords2D pLow = cornerMem[ iLow ];
						Concrete::Coords2D pHigh = cornerMem[ iHigh ];
						for( size_t i = 0; i < cornerMem.size( ) && ! wasSplit; ++i )
							{
								if( i == iLow || i == iHigh || i == iOther )
									{
										continue;
									}
								if( areAlignedAndOrdered( pLow, cornerMem[ i ], pHigh ) )
									{
										// To avoid infinite loops, we make sure that the triangles get smaller each time
										// they are splitted.	Otherwise, four points in a row can cause an infinite loop.

										Concrete::Coords2D pOther = cornerMem[ iOther ];
										Concrete::Coords2D pi = cornerMem[ i ];

										Concrete::Area area0 = Computation::triangleArea( pLow, pHigh, pOther );

										const double AREA_TOL = 1e-3;

										{
											const double relArea = Computation::triangleArea( pOther, pLow, pi ) / area0;
											if( AREA_TOL < relArea && relArea < 1 - AREA_TOL )
												{
													indexRepQueue.push_back( IndexRepresentation( ) );
													Lang::ZBuf::IndexRepresentation & newTriangle = indexRepQueue.back( );
													newTriangle.push_back( iOther );
													newTriangle.push_back( iLow );
													newTriangle.push_back( i );
													wasSplit = true;
												}
										}
										{
											const double relArea = Computation::triangleArea( pOther, pi, pHigh ) / area0;
											if( AREA_TOL < relArea && relArea < 1 - AREA_TOL )
												{
													indexRepQueue.push_back( IndexRepresentation( ) );
													Lang::ZBuf::IndexRepresentation & newTriangle = indexRepQueue.back( );
													newTriangle.push_back( iOther );
													newTriangle.push_back( i );
													newTriangle.push_back( iHigh );
													wasSplit = true;
												}
										}
									}
							}
					}

				if( ! wasSplit )
					{
						indexRepMem.push_back( current );
					}

				indexRepQueue.pop_front( );
			}
	}

	const size_t pointCount = cornerMem.size( );

	std::vector< PolyIndices > polyMem;
	polyMem.reserve( indexRepMem.size( ) );	// this is crucial to avoid reallocation!

	Computation::UndirectedEdgeMatrix< std::list< PolyIndices * > > edgeMatrix( pointCount );

	while( ! indexRepMem.empty( ) )
	{
		bool foundExtension = false;
		typedef typeof indexRepMem TrianglesType;
		for( TrianglesType::iterator i = indexRepMem.begin( ); i != indexRepMem.end( ) && ! foundExtension; ++i )
			{
				for( char e = 0; e < 3; ++e )
					{
						Computation::UndirectedEdge edge = i->getEdge( e );
						std::list< PolyIndices * > & polys = edgeMatrix[ edge ];
						if( polys.size( ) == 1 )	// By the way, it is an error if the size is greater than 1
							{
								if( ! extendPoly( polys.front( ), edge, i->getDifferent( e ), & edgeMatrix, cornerMem, false ) ) // false to not require convex
									{
										continue;
									}
								foundExtension = true;
								indexRepMem.erase( i );
								break;
							}
					}
			}
		if( ! foundExtension )
			{
				polyMem.push_back( indexRepMem.front( ).toPolyIndices( cornerMem ) );
				indexRepMem.pop_front( );
				PolyIndices * newPoly = & polyMem.back( );
				addEdges( newPoly, & edgeMatrix );
			}
	}

	while( ! polyMem.empty( ) )
		{
			PolyIndices & poly = polyMem.back( );
			if( poly.size( ) > 3 )
				{
					// First we remove insections, that is sequences like 4-5-4 et c.
					{
						PolyIndices::iterator theEnd = poly.begin( );
						PolyIndices::iterator i0 = poly.begin( );
						while( poly.size( ) >= 5 )	// it would be pathological to have insection in polygons with four corners or less
							{
								PolyIndices::iterator i1 = i0;
								++i1;
								if( i1 == poly.end( ) )
									{
										i1 = poly.begin( );
									}
								PolyIndices::iterator i2 = i1;
								++i2;
								if( i2 == poly.end( ) )
									{
										i2 = poly.begin( );
									}

								if( *i0 == *i2 )
									{
										// We found an insection
										poly.erase( i1 );
										poly.erase( i2 );
										theEnd = i0;
									}

								++i0;
								if( i0 == poly.end( ) )
									{
										i0 = poly.begin( );
									}
								if( i0 == theEnd )
									{
										break;
									}
							}
					}
					// Then we remove unnecessary points on straight segments
					for( PolyIndices::iterator i0 = poly.begin( ); i0 != poly.end( ); ++i0 )
						{
							Concrete::Coords2D p0 = cornerMem[ *i0 ];
							PolyIndices::iterator i1 = i0;
							++i1;
							if( i1 == poly.end( ) )
								{
									i1 = poly.begin( );
								}
							Concrete::Coords2D p1 = cornerMem[ *i1 ];
							PolyIndices::iterator i2 = i1;
							++i2;
							if( i2 == poly.end( ) )
								{
									i2 = poly.begin( );
								}
							Concrete::Coords2D p2 = cornerMem[ *i2 ];

							// We do the usual inscribed circle test to test alignment
							while( Computation::triangleArea( p0, p1, p2 ) < Computation::theTrixelizeOverlapTol * Computation::triangleSemiPerimeter( p0, p1, p2 ) )
								{
									PolyIndices::iterator iRemove = i1;
									i1 = i2;
									p1 = p2;
									poly.erase( iRemove );
									if( poly.size( ) < 3 )
										{
											goto done;
										}
									++i2;
									if( i2 == poly.end( ) )
										{
											i2 = poly.begin( );
										}
									p2 = cornerMem[ *i2 ];
								}
						}
				done:
					if( poly.size( ) < 3 )
						{
							polyMem.pop_back( );
							continue;
						}
				}


			{
				Lang::ElementaryPath2D * newPath = new Lang::ElementaryPath2D;

				for( PolyIndices::const_iterator src = poly.begin( ); src != poly.end( ); ++src )
				{
					newPath->push_back( new Concrete::PathPoint2D( new Concrete::Coords2D( cornerMem[ *src ] ) ) );
				}
				newPath->close( );
				dst->addSubPath( RefCountPtr< const Lang::ElementaryPath2D >( newPath ) );
			}
			polyMem.pop_back( );
		}

}

bool
Lang::ZBuf::addEdge( Computation::UndirectedEdgeMatrix< std::pair< bool, std::list< Lang::ZBuf::IndexRepresentation * > > > * edgeMatrix, const Computation::UndirectedEdge & edge, Lang::ZBuf::IndexRepresentation * indRep, std::list< Computation::UndirectedEdge > * jobQueue, const std::vector< Concrete::Coords2D > & cornerMem )
{
	typedef typeof (*edgeMatrix)[ edge ].second ListType;
	ListType & trianglesAtEdge = (*edgeMatrix)[ edge ].second;
	trianglesAtEdge.push_back( indRep );

	bool alreadyInQueue = false;

	if( trianglesAtEdge.size( ) > 2 )
		{
			// This is a problematic case.	I've written about it elsewhere.	The decision I took was to discard
			// the smaller if the triangles being on the same side of the edge.
			// Since we only do this when there are already three triangles sharing the edge, there's a risk
			// that all three are on the same side.	Then we may even drop two of them.

			alreadyInQueue = true;

			Concrete::UnitFloatPair edgeNormalNotNormalized = cornerMem[ edge.low( ) ].unNormalizedOrthogonal( cornerMem[ edge.high( ) ] );
			double m = Concrete::innerScalar( cornerMem[ edge.low( ) ], edgeNormalNotNormalized );

			Lang::ZBuf::IndexRepresentation * posMax = 0;
			Lang::ZBuf::IndexRepresentation * negMax = 0;
			Concrete::Area posArea = -1;
			Concrete::Area negArea = -1;

			for( ListType::iterator i = trianglesAtEdge.begin( ); i != trianglesAtEdge.end( ); ++i )
				{
					Concrete::Area tmpArea = Computation::triangleArea( cornerMem[ (*i)->i0 ], cornerMem[ (*i)->i1 ], cornerMem[ (*i)->i2 ] );
					if( Concrete::innerScalar( cornerMem[ (*i)->getDifferent( edge.low( ), edge.high( ) ) ], edgeNormalNotNormalized ) > m )
						{
							if( tmpArea > posArea )
								{
									posMax = *i;
									posArea = tmpArea;
								}
						}
					else
						{
							if( tmpArea > negArea )
								{
									negMax = *i;
									negArea = tmpArea;
								}
						}
				}

			// *** It is required that the new triangle appears at the end of trianglesAtEdge in case we add it. ***

			// We temporarily remove the newly added triangle...
			trianglesAtEdge.pop_back( );

			// Now we remove those of the old triangles that were knocked out.
			// Note that removing triangles from edges clears the job flags at their edges, so we don't have to do that here.
			{
				// We must be careful since trianglesAtEdge might change when triangles are removed!
				ListType::iterator i = trianglesAtEdge.begin( );
				Lang::ZBuf::IndexRepresentation * t1 = *i;
				++i;
				Lang::ZBuf::IndexRepresentation * t2 = *i;

				if( t1 != posMax && t1 != negMax )
					{
						t1->removeEdges( edgeMatrix );
					}
				if( t2 != posMax && t2 != negMax )
					{
						t2->removeEdges( edgeMatrix );
					}
			}

			// Next, we consider the new triangle.
			if( indRep != posMax && indRep != negMax )
				{
					// The new triangle couldn't compete with the old ones, so we shall return false.
					return false;
				}

			// ... and now we put it back if it passed the test.
			trianglesAtEdge.push_back( indRep );
		}

	if( jobQueue != 0 )
		{
			if( trianglesAtEdge.size( ) == 2 && ! (*edgeMatrix)[ edge ].first )
				{
					if( ! alreadyInQueue )
						{
							jobQueue->push_back( edge );
						}
					(*edgeMatrix)[ edge ].first = true; // Flag that this pair is in the queue.
				}
		}
	return true;
}

void
Lang::ZBuf::removeEdge( Computation::UndirectedEdgeMatrix< std::pair< bool, std::list< Lang::ZBuf::IndexRepresentation * > > > * edgeMatrix, const Computation::UndirectedEdge & edge, Lang::ZBuf::IndexRepresentation * indRep )
{
	std::pair< bool, std::list< Lang::ZBuf::IndexRepresentation * > > & p = (*edgeMatrix)[ edge ];

	// An edge can have at most two triangles, so when one is removed, the edge cannot be queued for work.
	// If this was the only triangle at the edge, this will be a no-op.
	p.first = false;

	std::list< Lang::ZBuf::IndexRepresentation * > & lst = p.second;
	std::list< Lang::ZBuf::IndexRepresentation * >::iterator i = find( lst.begin( ), lst.end( ), indRep );
	if( i == lst.end( ) )
		{
			throw Exceptions::InternalError( "Attempt to remove nonexistent edge." );
		}
	lst.erase( i );
}

bool
Lang::ZBuf::areAligned( const Concrete::Coords2D p0, const Concrete::Coords2D p1, const Concrete::Coords2D p2 )
{
	// We test alignment by looking at the radius of the inscribed circle.
	return Computation::triangleArea( p0, p1, p2 ) < Computation::theTrixelizeSplicingTol * Computation::triangleSemiPerimeter( p0, p1, p2 );
}

bool
Lang::ZBuf::areAlignedAndOrdered( const Concrete::Coords2D p0, const Concrete::Coords2D p1, const Concrete::Coords2D p2 )
{
	// We test alignment by looking at the radius of the inscribed circle.
	if( Computation::triangleArea( p0, p1, p2 ) > Computation::theTrixelizeSplicingTol * Computation::triangleSemiPerimeter( p0, p1, p2 ) )
		{
			return false;
		}

	// We then test that p1 is between p0 and p2 by computing a projection onto an unnormalized vector...
	// With dHat = d.normalized( ) = d * ( 1 / d.norm( ) ), the condition
	//	 dHat.inner( p1 - p0 ) < d.norm( )
	// is equivalent to
	//	 d.inner( p1 - p0 ) < d.norm( ) * d.norm( )
	Concrete::UnitFloatPair d( ( p2.x_ - p0.x_ ).offtype< 1, 0 >( ), ( p2.y_ - p0.y_ ).offtype< 1, 0 >( ), bool( ) );
	Concrete::Length c = Concrete::inner( d, p1 - p0 );
	if( c < Concrete::ZERO_LENGTH )
		{
			return false;
		}
	if( c > d.normSquaredThatOughtToBeOne( ) )
		{
			return false;
		}
	return true;
}

void
Lang::ZBuf::addEdges( PolyIndices * poly, Computation::UndirectedEdgeMatrix< std::list< PolyIndices * > > * edgeMatrix )
{
	PolyIndices::const_iterator src1 = poly->begin( );
	PolyIndices::const_iterator src2 = src1;
	++src2;
	for( ; src2 != poly->end( ); ++src1, ++src2 )
		{
			addEdge( edgeMatrix, *src1, *src2, poly );
		}
	src2 = poly->begin( );
	addEdge( edgeMatrix, *src1, *src2, poly );
}

void
Lang::ZBuf::addEdge( Computation::UndirectedEdgeMatrix< std::list< PolyIndices * > > * edgeMatrix, size_t ia, size_t ib, PolyIndices * indRep )
{
	(*edgeMatrix)[ Computation::UndirectedEdge( ia, ib ) ].push_back( indRep );
}

// Return true when extension was really performed
bool
Lang::ZBuf::extendPoly( PolyIndices * poly, const Computation::UndirectedEdge & edge, size_t iNew, Computation::UndirectedEdgeMatrix< std::list< PolyIndices * > > * edgeMatrix, const std::vector< Concrete::Coords2D > & cornerMem, bool convex )
{
	const PolyIndices::iterator src0 = std::find( poly->begin( ), poly->end( ), edge.low( ) );
	if( src0 == poly->end( ) )
		{
			return false;
		}

	PolyIndices::iterator src1;
	PolyIndices::iterator src2;
	// First we try the previous point
	if( src0 == poly->begin( ) )
		{
			if( poly->back( ) == edge.high( ) )
				{
					src2 = src0;
					src1 = poly->end( );
					--src1;
					goto doEdges;
				}
		}
	else
		{
			src1 = src0;
			--src1;
			src2 = src0;
			if( *src1 == edge.high( ) )
				{
					goto doEdges;
				}
		}

	// Then we try the next point
	{
		src1 = src0;
		src2 = src0;
		++src2;
		if( src2 == poly->end( ) )
			{
				src2 = poly->begin( );
			}
		if( *src2 == edge.high( ) )
			{
				goto doEdges;
			}
	}
	return false;

 doEdges:
	// Before we replace the old edge by the new ones we check convexity, if that's what the caller wants
	if( convex )
		{
			PolyIndices::iterator srcBack = src1;
			if( srcBack == poly->begin( ) )
				{
					srcBack = poly->end( );
				}
			--srcBack;
			{
				Concrete::UnitFloatPair inHat = cornerMem[ *srcBack ].normalizedOrthogonal( cornerMem[ *src1 ] );
				if( Concrete::inner( inHat, cornerMem[ iNew ] - cornerMem[ *src1 ] ) < - Computation::theTrixelizeSplicingTol )
					{
						return false;
					}
			}

			PolyIndices::iterator srcForward = src2;
			++srcForward;
			if( srcForward == poly->end( ) )
				{
					srcForward = poly->begin( );
				}
			{
				Concrete::UnitFloatPair inHat = cornerMem[ *src2 ].normalizedOrthogonal( cornerMem[ *srcForward ] );
				if( Concrete::inner( inHat, cornerMem[ iNew ] - cornerMem[ *src2 ] ) < - Computation::theTrixelizeSplicingTol )
					{
						return false;
					}
			}
		}

	poly->insert( src2, iNew );
	(*edgeMatrix)[ edge ].remove( poly );
	(*edgeMatrix)[ Computation::UndirectedEdge( edge.low( ), iNew ) ].push_back( poly );
	(*edgeMatrix)[ Computation::UndirectedEdge( edge.high( ), iNew ) ].push_back( poly );
	return true;
}
