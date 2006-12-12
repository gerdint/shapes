#ifndef pathtypes_h
#define pathtypes_h

#include <cmath>

#include "MetaPDF_Lang_decls.h"
#include "MetaPDF_Computation_decls.h"
#include "MetaPDF_Kernel_decls.h"
#include "MetaPDF_Concrete_decls.h"
#include "MetaPDF_Ast_decls.h"

#include "ptrowner.h"
#include "refcount.h"
#include "pdfstructure.h"
#include "metapdfvalue.h"
#include "environment.h"
#include "charptrless.h"
#include "functiontypes.h"
#include "elementarycoords.h"
#include "statetypes.h"

#include <list>
#include <iostream>
#include <stack>
#include <set>



namespace MetaPDF
{

  Concrete::Time computeDt( Concrete::Length segLength ); // it is sufficient to provide an over-estimate of the segment's length
  Concrete::Time straightLineArcTime( double fraction );

  namespace Lang
  {
    class PolarHandleBase : public Lang::Value
    {
    public:
      PolarHandleBase( );
      virtual ~PolarHandleBase( );
      TYPEINFODECL;
      DISPATCHDECL;
    };

  class PolarHandle2D : public Lang::PolarHandleBase
  {
  public:
    RefCountPtr< const Kernel::PolarHandlePromise > rPromise_;
    double a_;
    PolarHandle2D( const RefCountPtr< const Kernel::PolarHandlePromise > & rPromise, double a );
    virtual ~PolarHandle2D( );
    virtual void show( std::ostream & os ) const;
    virtual void gcMark( Kernel::GCMarkedSet & marked );
  };

  class PolarHandle2DFree_a : public Lang::PolarHandleBase
  {
  public:
    RefCountPtr< const Kernel::PolarHandlePromise > rPromise_;
    PolarHandle2DFree_a( const RefCountPtr< const Kernel::PolarHandlePromise > & rPromise );
    virtual ~PolarHandle2DFree_a( );
    virtual void show( std::ostream & os ) const;
    virtual void gcMark( Kernel::GCMarkedSet & marked );
  };

  class PolarHandle2DFree_r : public Lang::PolarHandleBase
  {
  public:
    RefCountPtr< const Kernel::PolarHandlePromise > defaultModulus_;
    double a_;
    PolarHandle2DFree_r( const RefCountPtr< const Kernel::PolarHandlePromise > & defaultModulus, double a );
    virtual ~PolarHandle2DFree_r( );
    virtual void show( std::ostream & os ) const;
    virtual void gcMark( Kernel::GCMarkedSet & marked );
  };

  class PolarHandle2DFree_ra : public Lang::PolarHandleBase
  {
  public:
    RefCountPtr< const Kernel::PolarHandlePromise > defaultModulus_;
    PolarHandle2DFree_ra( const RefCountPtr< const Kernel::PolarHandlePromise > & defaultModulus );
    virtual ~PolarHandle2DFree_ra( );
    virtual void show( std::ostream & os ) const;
    virtual void gcMark( Kernel::GCMarkedSet & marked );
  };

  class PathPoint2D : public Lang::Geometric2D
  {
  public:
    Kernel::ValueRef rear_;
    RefCountPtr< const Lang::Coords2D > mid_;
    Kernel::ValueRef front_;
    PathPoint2D( const PathPoint2D & orig );
    PathPoint2D( RefCountPtr< const Lang::Coords2D > mid );
    virtual RefCountPtr< const Lang::Geometric2D > transformed( const Lang::Transform2D & tf, const RefCountPtr< const Lang::Geometric2D > & self ) const;
    virtual RefCountPtr< const Lang::Geometric3D > to3D( const RefCountPtr< const Lang::Geometric2D > & self ) const;
    virtual void gcMark( Kernel::GCMarkedSet & marked );
    TYPEINFODECL;
    DISPATCHDECL;
  };

  class SubPath2D : public Lang::Geometric2D
  {
  protected:
    bool closed_;
  public:
    SubPath2D( );
    virtual ~SubPath2D( );

    void close( );
    bool isClosed( ) const;
    virtual RefCountPtr< const Lang::Geometric2D > transformed( const Lang::Transform2D & tf, const RefCountPtr< const Lang::Geometric2D > & self ) const;
    virtual RefCountPtr< const Lang::Geometric3D > to3D( const RefCountPtr< const Lang::Geometric2D > & self ) const;
    virtual RefCountPtr< const Lang::SubPath2D > typed_transformed( const Lang::Transform2D & tf ) const = 0;
    virtual RefCountPtr< const Lang::SubPath3D > typed_to3D( const RefCountPtr< const Lang::SubPath2D > & self ) const = 0;
    virtual void writePath( std::ostream & os ) const = 0;
    virtual void elementaryJob( std::stack< const Lang::Value * > * nodeStack, Lang::ElementaryPath2D * pth ) const = 0;
    TYPEINFODECL;
    DISPATCHDECL;
  };

  class CompositePath2D : public Lang::SubPath2D
  {
  protected:
    mutable RefCountPtr< const ElementaryPath2D > elementaryPath_;
  public:
    CompositePath2D( );
    virtual ~CompositePath2D( );
    virtual Kernel::HandleType getField( const char * fieldID, const RefCountPtr< const Lang::Value > & selfRef ) const;

    virtual void writePath( std::ostream & os ) const;
    virtual RefCountPtr< const Lang::SubPath2D > typed_transformed( const Lang::Transform2D & tf ) const;
    virtual RefCountPtr< const Lang::SubPath3D > typed_to3D( const RefCountPtr< const Lang::SubPath2D > & self ) const;
    RefCountPtr< const Lang::ElementaryPath2D > getElementaryPath( ) const;
    virtual void show( std::ostream & os ) const;
  protected:
    void computeElementaryPath( ) const;
  };
  
  class ClosedPath2D : public Lang::CompositePath2D
  {
  public:
    RefCountPtr< const Lang::SubPath2D > openPath_;
    ClosedPath2D( RefCountPtr< const Lang::SubPath2D > openPath );
    virtual ~ClosedPath2D( );
    virtual void elementaryJob( std::stack< const Lang::Value * > * nodeStack, Lang::ElementaryPath2D * pth ) const;
    virtual void gcMark( Kernel::GCMarkedSet & marked );
  };

  class ElementaryPath2D : public PtrOwner_back_Access< std::list< Concrete::PathPoint2D * > >, public Lang::SubPath2D
  {
  public:
    bool allComplete_;
    ElementaryPath2D( );
    virtual ~ElementaryPath2D( );
    virtual Kernel::HandleType getField( const char * fieldID, const RefCountPtr< const Lang::Value > & selfRef ) const;
    virtual Kernel::HandleType getField( const char * fieldID, const RefCountPtr< const Lang::ElementaryPath2D > & selfRef ) const;

    virtual void writePath( std::ostream & os ) const;
    virtual void writeInputForm( std::ostream & os ) const;
    virtual RefCountPtr< const Lang::ElementaryPath2D > elementaryTransformed( const Lang::Transform2D & tf ) const;
    virtual RefCountPtr< const Lang::ElementaryPath3D > elementaryTransformed( const Lang::Transform3D & tf ) const;
    virtual RefCountPtr< const Lang::SubPath2D > typed_transformed( const Lang::Transform2D & tf ) const;
    virtual RefCountPtr< const Lang::SubPath3D > typed_to3D( const RefCountPtr< const Lang::SubPath2D > & self ) const;
    virtual RefCountPtr< const Lang::Geometric3D > to3D( const RefCountPtr< const Lang::Geometric2D > & self ) const;
    virtual void elementaryJob( std::stack< const Lang::Value * > * nodeStack, Lang::ElementaryPath2D * pth ) const;

    size_t duration( ) const;
    bool controllingMaximizer( const Lang::FloatPair & d, Lang::Coords2D * dst ) const;
    bool boundingRectangle( Concrete::Coords2D * dstll, Concrete::Coords2D * dstur ) const;
    bool lineSegmentIntersection( Concrete::Time * dst, const Concrete::Coords2D & l0, const Concrete::Coords2D & l1 ) const;

    RefCountPtr< const Lang::Coords2D > point( Concrete::Time t ) const;
    RefCountPtr< const Lang::Length > speed( Concrete::Time t ) const;
    RefCountPtr< const Lang::Length > reverse_speed( Concrete::Time t ) const;
    RefCountPtr< const Lang::FloatPair > direction( Concrete::Time t ) const;
    RefCountPtr< const Lang::FloatPair > reverse_direction( Concrete::Time t ) const;
    RefCountPtr< const Lang::Length > radiusOfCurvature( Concrete::Time t ) const;
    RefCountPtr< const Lang::Length > reverse_radiusOfCurvature( Concrete::Time t ) const;

    RefCountPtr< const Lang::Coords2D > discreteMean( ) const;
    RefCountPtr< const Lang::Coords2D > continuousMean( ) const;
    RefCountPtr< const Lang::Coords2D > controllingMaximizer( const Lang::FloatPair & d ) const;
    Concrete::Length arcLength( ) const;
    Concrete::Length arcLength( Concrete::Time tFinal ) const;

    Concrete::SplineTime arcTime( const Concrete::Length & t, Concrete::Time t0 = 0 ) const;
    Concrete::SplineTime discreteMaximizer( const Lang::FloatPair & d ) const;
    Concrete::SplineTime continuousMaximizer( const Lang::FloatPair & d ) const;
    Concrete::SplineTime discreteApproximator( const Lang::Coords2D & p ) const;
    Concrete::SplineTime continuousApproximator( const Lang::Coords2D & p ) const;
    Concrete::SplineTime intersection( const Lang::ElementaryPath2D & p2 ) const;

    RefCountPtr< const Lang::ElementaryPath2D > subpath( const Concrete::SplineTime t1, const Concrete::SplineTime t2 ) const;
    RefCountPtr< const Lang::ElementaryPath2D > reverse( ) const;
    RefCountPtr< const Lang::ElementaryPath2D > controlling_hull( ) const;

    bool isConvexPoly( Concrete::Length tol ) const;
    bool convexPolyContains( const Concrete::Coords2D & p, Concrete::Length tol ) const;

    virtual void show( std::ostream & os ) const;
    virtual void gcMark( Kernel::GCMarkedSet & marked );

  private:
    Concrete::Time timeCheck( Concrete::Time t ) const;
    void findSegment( Concrete::Time t, Concrete::Time * tRes, const Concrete::PathPoint2D ** p1, const Concrete::PathPoint2D ** p2 ) const;
    void findSegmentReverse( Concrete::Time t, Concrete::Time * tRes, const Concrete::PathPoint2D ** p1, const Concrete::PathPoint2D ** p2 ) const;
    static void pushOverlappingDeleteOther( std::list< Computation::IntersectionSegmentSections2D * > & work, Computation::IntersectionSegmentSections2D * item );
    Concrete::Length negative_arcLength( Concrete::Time tFinal ) const;
    Concrete::SplineTime negative_arcTime( const Concrete::Length dist, Concrete::Time t0 = 0 ) const;
  };

  class PathSlider2D : public Lang::Value
  {
    RefCountPtr< const Lang::ElementaryPath2D > path_;
    Concrete::SplineTime t_;
  public:
    PathSlider2D( const Lang::PathSlider2D & orig );
    PathSlider2D( const RefCountPtr< const Lang::ElementaryPath2D > & path );
    PathSlider2D( const RefCountPtr< const Lang::ElementaryPath2D > & path, Concrete::SplineTime t );
    virtual ~PathSlider2D( );
    virtual Kernel::HandleType getField( const char * fieldID, const RefCountPtr< const Lang::Value > & selfRef ) const;
    RefCountPtr< const Lang::ElementaryPath2D > getPath( ) const { return path_; }
    Concrete::SplineTime getTime( ) const { return t_; }

    RefCountPtr< Lang::PathSlider2D > move_time( const Concrete::Time delta ) const;
    RefCountPtr< Lang::PathSlider2D > move_length( const Concrete::Length delta ) const;
    RefCountPtr< Lang::PathSlider2D > seek_time( const Concrete::Time val ) const;
    RefCountPtr< Lang::PathSlider2D > seek_start( ) const;
    RefCountPtr< Lang::PathSlider2D > seek_end( ) const;
    RefCountPtr< Lang::PathSlider2D > seek_length( const Concrete::Length val ) const;

    RefCountPtr< const Lang::Coords2D > point( ) const;
    RefCountPtr< const Lang::Length > speed( ) const;
    RefCountPtr< const Lang::Length > reverse_speed( ) const;
    RefCountPtr< const Lang::FloatPair > direction( ) const;
    RefCountPtr< const Lang::FloatPair > reverse_direction( ) const;
    RefCountPtr< const Lang::FloatPair > normal( ) const;
    RefCountPtr< const Lang::FloatPair > reverse_normal( ) const;
    RefCountPtr< const Lang::Length > radiusOfCurvature( ) const;
    RefCountPtr< const Lang::Length > reverse_radiusOfCurvature( ) const;
    RefCountPtr< const Lang::Float > time( ) const;
    RefCountPtr< const Lang::Length > length( ) const;

    virtual RefCountPtr< const Lang::Value > getRear( ) const;
    virtual RefCountPtr< const Lang::Value > getFront( ) const;

    virtual void show( std::ostream & os ) const;
    virtual void gcMark( Kernel::GCMarkedSet & marked );
    TYPEINFODECL;
    DISPATCHDECL;
  };

  class PathSlider2D_rear : public Lang::PathSlider2D
  {
    RefCountPtr< const Lang::Value > rear_;
  public:
    PathSlider2D_rear( const Lang::PathSlider2D & orig, const RefCountPtr< const Lang::Value > & rear );
    virtual RefCountPtr< const Lang::Value > getRear( ) const;
  };

  class PathSlider2D_front : public Lang::PathSlider2D
  {
    RefCountPtr< const Lang::Value > front_;
  public:
    PathSlider2D_front( const Lang::PathSlider2D & orig, const RefCountPtr< const Lang::Value > & front );
    virtual RefCountPtr< const Lang::Value > getFront( ) const;
  };

  class Connection2D : public Lang::CompositePath2D
  {
  public:
    Kernel::ValueRef rear_;
    Kernel::ValueRef front_;
    Connection2D( Kernel::ValueRef rear, Kernel::ValueRef front );
    ~Connection2D( );
    virtual void elementaryJob( std::stack< const Lang::Value * > * nodeStack, Lang::ElementaryPath2D * pth ) const;
    virtual void gcMark( Kernel::GCMarkedSet & marked );
  };

  class SinglePointPath2D : public Lang::CompositePath2D
  {
  public:
    Kernel::ValueRef thePoint_;
    SinglePointPath2D( Kernel::ValueRef thePoint );
    ~SinglePointPath2D( );
    virtual void elementaryJob( std::stack< const Lang::Value * > * nodeStack, Lang::ElementaryPath2D * pth ) const;
    virtual void gcMark( Kernel::GCMarkedSet & marked );
  };

  class HeadedPath2D_helper : public Lang::CompositePath2D
  {
    RefCountPtr< const Lang::ElementaryPath2D > bodyPath_;
  public:
    HeadedPath2D_helper( const RefCountPtr< const Lang::ElementaryPath2D > & bodyPath );
    ~HeadedPath2D_helper( );
    virtual void elementaryJob( std::stack< const Lang::Value * > * nodeStack, Lang::ElementaryPath2D * pth ) const;
    virtual void gcMark( Kernel::GCMarkedSet & marked );
  };

  class HeadedPath2D : public Lang::CompositePath2D
  {
    RefCountPtr< const Lang::HeadedPath2D_helper > bodyPath_;
    Kernel::ValueRef rearPathPoint_;
    Kernel::ValueRef frontPathPoint_;
  public:
    HeadedPath2D( Kernel::ValueRef rear, const RefCountPtr< const Lang::ElementaryPath2D > & bodyPath, Kernel::ValueRef front );
    ~HeadedPath2D( );
    virtual void elementaryJob( std::stack< const Lang::Value * > * nodeStack, Lang::ElementaryPath2D * pth ) const;
    virtual void gcMark( Kernel::GCMarkedSet & marked );
  };

  class Path2D : public Lang::Geometric2D, public std::list< RefCountPtr< const SubPath2D > >
  {
  public:
    Path2D( );
    ~Path2D( );
    Path2D * clone( ) const;
    virtual RefCountPtr< const Lang::Geometric2D > transformed(const Lang::Transform2D & tf, const RefCountPtr< const Lang::Geometric2D > & self ) const;
    virtual RefCountPtr< const Lang::Geometric3D > to3D( const RefCountPtr< const Lang::Geometric2D > & self ) const;
    void writePath( std::ostream & os ) const;
    TYPEINFODECL;
    virtual void show( std::ostream & os ) const;
    virtual void gcMark( Kernel::GCMarkedSet & marked );
    DISPATCHDECL;
  };


  class PathPoint3D : public Lang::Geometric3D
  {
  public:
    RefCountPtr< const Lang::Coords3D > rear_;
    RefCountPtr< const Lang::Coords3D > mid_;
    RefCountPtr< const Lang::Coords3D > front_;
    PathPoint3D( const PathPoint3D & orig );
    PathPoint3D( const RefCountPtr< const Lang::Coords3D > & mid );
    void elementaryJob( Lang::ElementaryPath3D * pth, Concrete::Coords3D * basePoint ) const;
    virtual RefCountPtr< const Lang::Geometric3D > transformed( const Lang::Transform3D & tf, const RefCountPtr< const Lang::Geometric3D > & self ) const;
    virtual RefCountPtr< const Lang::Geometric2D > to2D( const Kernel::PassedDyn & dyn, const RefCountPtr< const Lang::Geometric3D > & self ) const;
    virtual void gcMark( Kernel::GCMarkedSet & marked );
    TYPEINFODECL;
    DISPATCHDECL;
  };


  class SubPath3D : public Lang::Geometric3D
  {
  protected:
    bool closed_;
  public:
    SubPath3D( );
    virtual ~SubPath3D( );

    void close( );
    bool isClosed( ) const; 
    virtual RefCountPtr< const Lang::Geometric3D > transformed( const Lang::Transform3D & tf, const RefCountPtr< const Lang::Geometric3D > & self ) const;
    virtual RefCountPtr< const Lang::SubPath3D > typed_transformed( const Lang::Transform3D & tf ) const = 0;
    virtual RefCountPtr< const Lang::Geometric2D > to2D( const Kernel::PassedDyn & dyn, const RefCountPtr< const Lang::Geometric3D > & self ) const;
    virtual RefCountPtr< const Lang::ElementaryPath2D > make2D( Concrete::Length eyez ) const = 0;
    virtual void elementaryJob( std::stack< const Lang::SubPath3D * > * nodeStack, Lang::ElementaryPath3D * pth, Concrete::Coords3D * basePoint ) const = 0;
    TYPEINFODECL;
    DISPATCHDECL;
  };

  class CompositePath3D : public Lang::SubPath3D
  {
  protected:
    mutable RefCountPtr< const ElementaryPath3D > elementaryPath_;
  public:
    CompositePath3D( );
    virtual ~CompositePath3D( );
    virtual Kernel::HandleType getField( const char * fieldID, const RefCountPtr< const Lang::Value > & selfRef ) const;

    virtual RefCountPtr< const Lang::ElementaryPath2D > make2D( Concrete::Length eyez ) const;
    virtual RefCountPtr< const Lang::SubPath3D > typed_transformed( const Lang::Transform3D & tf ) const;
    RefCountPtr< const Lang::ElementaryPath3D > getElementaryPath( ) const;
    virtual void show( std::ostream & os ) const;
  protected:
    void computeElementaryPath( ) const;
  };

  class SubPath2Din3D : public Lang::CompositePath3D
  {
    RefCountPtr< const Lang::ElementaryPath2D > elementaryPath2D_;
  public:
    SubPath2Din3D( const RefCountPtr< const Lang::ElementaryPath2D > & _elementaryPath2D );
    ~SubPath2Din3D( );
    virtual RefCountPtr< const Lang::ElementaryPath2D > make2D( Concrete::Length eyez ) const;
    virtual RefCountPtr< const Lang::SubPath3D > typed_transformed( const Lang::Transform3D & tf ) const;
    virtual void elementaryJob( std::stack< const Lang::SubPath3D * > * nodeStack, Lang::ElementaryPath3D * pth, Concrete::Coords3D * basePoint ) const;
    virtual void gcMark( Kernel::GCMarkedSet & marked );
  };
  
  class ClosedPath3D : public Lang::CompositePath3D
  {
  public:
    RefCountPtr< const Lang::SubPath3D > openPath_;
    ClosedPath3D( RefCountPtr< const Lang::SubPath3D > openPath );
    virtual ~ClosedPath3D( );
    virtual void elementaryJob( std::stack< const Lang::SubPath3D * > * nodeStack, Lang::ElementaryPath3D * pth, Concrete::Coords3D * basePoint ) const;
    virtual void gcMark( Kernel::GCMarkedSet & marked );
  };

  class ElementaryPath3D : public PtrOwner_back_Access< std::list< Concrete::PathPoint3D * > >, public Lang::SubPath3D
  {
  public:
    ElementaryPath3D( );
    virtual ~ElementaryPath3D( );
    virtual Kernel::HandleType getField( const char * fieldID, const RefCountPtr< const Lang::Value > & selfRef ) const;
    virtual Kernel::HandleType getField( const char * fieldID, const RefCountPtr< const Lang::ElementaryPath3D > & selfRef ) const;

    virtual RefCountPtr< const Lang::ElementaryPath2D > make2D( Concrete::Length eyez ) const;
    void dashifyIn2D( RefCountPtr< const Lang::Group2D > * res, Concrete::Length eyez, const RefCountPtr< const Kernel::GraphicsState > & metaState ) const;
    virtual RefCountPtr< const Lang::ElementaryPath3D > elementaryTransformed( const Lang::Transform3D & tf ) const;
    virtual RefCountPtr< const Lang::SubPath3D > typed_transformed( const Lang::Transform3D & tf ) const;
    virtual void elementaryJob( std::stack< const Lang::SubPath3D * > * nodeStack, Lang::ElementaryPath3D * pth, Concrete::Coords3D * basePoint ) const;

    size_t duration( ) const;
    bool controllingMaximizer( const Lang::FloatTriple & d, Lang::Coords3D * dst ) const;

    RefCountPtr< const Lang::Coords3D > point( Concrete::Time t ) const;
    RefCountPtr< const Lang::Length > speed( Concrete::Time t ) const;
    RefCountPtr< const Lang::Length > reverse_speed( Concrete::Time t ) const;
    RefCountPtr< const Lang::FloatTriple > direction( Concrete::Time t ) const;
    RefCountPtr< const Lang::FloatTriple > reverse_direction( Concrete::Time t ) const;
    RefCountPtr< const Lang::FloatTriple > normal( Concrete::Time t ) const;
    RefCountPtr< const Lang::FloatTriple > reverse_normal( Concrete::Time t ) const;
    RefCountPtr< const Lang::Length > radiusOfCurvature( Concrete::Time t ) const;
    RefCountPtr< const Lang::Length > reverse_radiusOfCurvature( Concrete::Time t ) const;
    RefCountPtr< const Lang::Length > radiusOfTorsion( Concrete::Time t ) const;
    RefCountPtr< const Lang::Length > reverse_radiusOfTorsion( Concrete::Time t ) const;

    RefCountPtr< const Lang::Coords3D > discreteMean( ) const;
    RefCountPtr< const Lang::Coords3D > continuousMean( ) const;
    RefCountPtr< const Lang::Coords3D > controllingMaximizer( const Lang::FloatTriple & d ) const;
    Concrete::Length arcLength( ) const;
    Concrete::Length arcLength( Concrete::Time tFinal ) const;

    Concrete::SplineTime arcTime( const Concrete::Length & t, Concrete::Time t0 = 0 ) const;
    Concrete::SplineTime discreteMaximizer( const Lang::FloatTriple & d ) const;
    Concrete::SplineTime continuousMaximizer( const Lang::FloatTriple & d ) const;
    Concrete::SplineTime discreteApproximator( const Lang::Coords3D & p ) const;
    Concrete::SplineTime continuousApproximator( const Lang::Coords3D & p ) const;

    RefCountPtr< const Lang::ElementaryPath3D > subpath( const Concrete::SplineTime t1, const Concrete::SplineTime t2 ) const;
    RefCountPtr< const Lang::ElementaryPath3D > reverse( ) const;

    virtual void show( std::ostream & os ) const;
    virtual void gcMark( Kernel::GCMarkedSet & marked );

    void getRepresentativePoints( const Lang::Transform3D & tf, Concrete::Coords3D * p0, Concrete::Coords3D * p1, Concrete::Coords3D * p2 ) const;

  private:
    Concrete::Time timeCheck( Concrete::Time t ) const;
    void findSegment( Concrete::Time t, Concrete::Time * tRes, const Concrete::PathPoint3D ** p1, const Concrete::PathPoint3D ** p2 ) const;
    void findSegmentReverse( Concrete::Time t, Concrete::Time * tRes, const Concrete::PathPoint3D ** p1, const Concrete::PathPoint3D ** p2 ) const;
    Concrete::Length negative_arcLength( Concrete::Time tFinal ) const;
    Concrete::SplineTime negative_arcTime( const Concrete::Length dist, Concrete::Time t0 = 0 ) const;
    static void makeSegment2D( Lang::ElementaryPath2D * dst, Concrete::Coords2D ** passedRear, const Concrete::Coords3D * p0, const Concrete::Coords3D * p1, const Concrete::Coords3D * p2, const Concrete::Coords3D * p3, Concrete::Length eyez );
    static double viewDistortionTooBig( const Concrete::Coords3D & p0, const Concrete::Coords3D & p1, const Concrete::Coords3D & p2, const Concrete::Coords3D & p3, Concrete::Length eyez, double threshold );
    static void dashifySegment( RefCountPtr< const Lang::Group2D > * res, Lang::ElementaryPath3D & newPath, Concrete::Length remainingLength, Concrete::Coords3D ** passedRear, const Concrete::Coords3D * p0, const Concrete::Coords3D * p1, const Concrete::Coords3D * p2, const Concrete::Coords3D * p3, Concrete::Length eyez, const RefCountPtr< const Kernel::GraphicsState > & solidState, Lang::Dash::Iterator & dashi );
    static void laterArcTime( const Concrete::Bezier & x0, const Concrete::Bezier & y0, const Concrete::Bezier & z0, const Concrete::Bezier & x1, const Concrete::Bezier & y1, const Concrete::Bezier & z1, const Concrete::Bezier & x2, const Concrete::Bezier & y2, const Concrete::Bezier & z2, const Concrete::Bezier & x3, const Concrete::Bezier & y3, const Concrete::Bezier & z3, Concrete::Time * _t, Concrete::Length * l, const Concrete::Time & dt );
  };

  class PathSlider3D : public Lang::Value
  {
    RefCountPtr< const Lang::ElementaryPath3D > path_;
    Concrete::SplineTime t_;
  public:
    PathSlider3D( const Lang::PathSlider3D & orig );
    PathSlider3D( const RefCountPtr< const Lang::ElementaryPath3D > & _path );
    PathSlider3D( const RefCountPtr< const Lang::ElementaryPath3D > & _path, Concrete::SplineTime _t );
    virtual ~PathSlider3D( );
    virtual Kernel::HandleType getField( const char * fieldID, const RefCountPtr< const Lang::Value > & selfRef ) const;
    RefCountPtr< const Lang::ElementaryPath3D > getPath( ) const { return path_; }
    Concrete::SplineTime getTime( ) const { return t_; }

    RefCountPtr< Lang::PathSlider3D > move_time( const Concrete::Time delta ) const;
    RefCountPtr< Lang::PathSlider3D > move_length( const Concrete::Length delta ) const;
    RefCountPtr< Lang::PathSlider3D > seek_time( const Concrete::Time val ) const;
    RefCountPtr< Lang::PathSlider3D > seek_start( ) const;
    RefCountPtr< Lang::PathSlider3D > seek_end( ) const;
    RefCountPtr< Lang::PathSlider3D > seek_length( const Concrete::Length val ) const;

    RefCountPtr< const Lang::Coords3D > point( ) const;
    RefCountPtr< const Lang::Length > speed( ) const;
    RefCountPtr< const Lang::Length > reverse_speed( ) const;
    RefCountPtr< const Lang::FloatTriple > direction( ) const;
    RefCountPtr< const Lang::FloatTriple > reverse_direction( ) const;
    RefCountPtr< const Lang::FloatTriple > normal( ) const;
    RefCountPtr< const Lang::FloatTriple > reverse_normal( ) const;
    RefCountPtr< const Lang::FloatTriple > binormal( ) const;
    RefCountPtr< const Lang::FloatTriple > reverse_binormal( ) const;
    RefCountPtr< const Lang::Length > radiusOfCurvature( ) const;
    RefCountPtr< const Lang::Length > reverse_radiusOfCurvature( ) const;
    RefCountPtr< const Lang::Length > radiusOfTorsion( ) const;
    RefCountPtr< const Lang::Length > reverse_radiusOfTorsion( ) const;
    RefCountPtr< const Lang::Float > time( ) const;
    RefCountPtr< const Lang::Length > length( ) const;

    virtual RefCountPtr< const Lang::Value > getRear( ) const;
    virtual RefCountPtr< const Lang::Value > getFront( ) const;

    virtual void show( std::ostream & os ) const;
    virtual void gcMark( Kernel::GCMarkedSet & marked );
    TYPEINFODECL;
    DISPATCHDECL;
  };

  class PathSlider3D_rear : public Lang::PathSlider3D
  {
    RefCountPtr< const Lang::Value > rear_;
  public:
    PathSlider3D_rear( const Lang::PathSlider3D & orig, const RefCountPtr< const Lang::Value > & rear );
    virtual RefCountPtr< const Lang::Value > getRear( ) const;
  };

  class PathSlider3D_front : public Lang::PathSlider3D
  {
    RefCountPtr< const Lang::Value > front_;
  public:
    PathSlider3D_front( const Lang::PathSlider3D & orig, const RefCountPtr< const Lang::Value > & front );
    virtual RefCountPtr< const Lang::Value > getFront( ) const;
  };

  class Connection3D : public Lang::CompositePath3D
  {
  public:
    RefCountPtr< const Lang::SubPath3D > rear_;
    RefCountPtr< const Lang::SubPath3D > front_;
    Connection3D( const RefCountPtr< const Lang::SubPath3D > & rear, const RefCountPtr< const Lang::SubPath3D > & front );
    ~Connection3D( );
    virtual void elementaryJob( std::stack< const Lang::SubPath3D * > * nodeStack, Lang::ElementaryPath3D * pth, Concrete::Coords3D * basePoint ) const;
    virtual void gcMark( Kernel::GCMarkedSet & marked );
  };

  class SinglePointPath3D : public Lang::CompositePath3D
  {
  public:
    RefCountPtr< const Lang::PathPoint3D > thePoint_;
    SinglePointPath3D( const RefCountPtr< const Lang::PathPoint3D > & thePoint );
    SinglePointPath3D( const RefCountPtr< const Lang::Coords3D > & mid );
    ~SinglePointPath3D( );
    virtual void elementaryJob( std::stack< const Lang::SubPath3D * > * nodeStack, Lang::ElementaryPath3D * pth, Concrete::Coords3D * basePoint ) const;
    virtual void gcMark( Kernel::GCMarkedSet & marked );
  };

  class HeadedPath3D_helper : public Lang::CompositePath3D
  {
    RefCountPtr< const Lang::ElementaryPath3D > bodyPath_;
  public:
    HeadedPath3D_helper( const RefCountPtr< const Lang::ElementaryPath3D > & bodyPath );
    ~HeadedPath3D_helper( );
    virtual void elementaryJob( std::stack< const Lang::SubPath3D * > * nodeStack, Lang::ElementaryPath3D * pth, Concrete::Coords3D * basePoint ) const;
    virtual void gcMark( Kernel::GCMarkedSet & marked );
  };

  class HeadedPath3D : public Lang::CompositePath3D
  {
    RefCountPtr< const Lang::HeadedPath3D_helper > bodyPath_;
    RefCountPtr< const Lang::SinglePointPath3D > rearPathPoint_;
    RefCountPtr< const Lang::SinglePointPath3D > frontPathPoint_;
  public:
    HeadedPath3D( Kernel::ValueRef rear, const RefCountPtr< const Lang::ElementaryPath3D > & bodyPath, Kernel::ValueRef front );
    ~HeadedPath3D( );
    virtual void elementaryJob( std::stack< const Lang::SubPath3D * > * nodeStack, Lang::ElementaryPath3D * pth, Concrete::Coords3D * basePoint ) const;
    virtual void gcMark( Kernel::GCMarkedSet & marked );
  };

  class Path3D : public Lang::Geometric3D, public std::list< RefCountPtr< const SubPath3D > >
  {
  public:
    Path3D( );
    ~Path3D( );
    Path3D * clone( ) const;
    virtual RefCountPtr< const Lang::Geometric3D > transformed( const Lang::Transform3D & tf, const RefCountPtr< const Lang::Geometric3D > & self ) const;
    virtual RefCountPtr< const Lang::Geometric2D > to2D( const Kernel::PassedDyn & dyn, const RefCountPtr< const Lang::Geometric3D > & self ) const;
    TYPEINFODECL;
    virtual void show( std::ostream & os ) const;
    virtual void gcMark( Kernel::GCMarkedSet & marked );
    DISPATCHDECL;
  };
  
  }


  namespace Kernel
  {
    class PolarHandlePromise
    {
    public:
      PolarHandlePromise( );
      virtual ~PolarHandlePromise( );
      virtual double force( const Concrete::PathPoint2D * specialUnitP0, const Concrete::PathPoint2D * specialUnitP1, bool reverse ) const = 0;
      virtual void gcMark( Kernel::GCMarkedSet & marked ) const = 0;
    };

    class PolarHandleEmptyPromise : public PolarHandlePromise
    {
    public:
      PolarHandleEmptyPromise( );
      virtual ~PolarHandleEmptyPromise( );
      virtual double force( const Concrete::PathPoint2D * specialUnitP0, const Concrete::PathPoint2D * specialUnitP1, bool reverse ) const;
      virtual void gcMark( Kernel::GCMarkedSet & marked ) const;
    };
    
    class PolarHandleTruePromise : public PolarHandlePromise
    {
      Kernel::Thunk * thunk_;  /* this thunk always waits to be forced, since it will be evaluated in different dynamic contexts each time */
    public:
      PolarHandleTruePromise( Kernel::Thunk * thunk );
      virtual ~PolarHandleTruePromise( );
      virtual double force( const Concrete::PathPoint2D * specialUnitP0, const Concrete::PathPoint2D * specialUnitP1, bool reverse ) const;
      virtual void gcMark( Kernel::GCMarkedSet & marked ) const;
    };
  }

  namespace Concrete
  {

    class PathPoint2D
    {
    public:
      enum HandleState { COMPLETE = 0x0000, FREE_ANGLE = 0x0001, FREE_MODULUS = 0x0002, FREE = 0x0003, UNFORCED_M = 0x0004, UNFORCED_A = 0x0008 };
      
      int rearState_;
      double rearAngle_;
      Concrete::Length rearModulus_;
      RefCountPtr< const Kernel::PolarHandlePromise > rearModulusPromise_;

      int frontState_;
      double frontAngle_;
      Concrete::Length frontModulus_;
      RefCountPtr< const Kernel::PolarHandlePromise > frontModulusPromise_;
      
      double defaultAngle_;
      
      Concrete::Coords2D * rear_;
      Concrete::Coords2D * mid_;
      Concrete::Coords2D * front_;
      
      PathPoint2D( const Concrete::PathPoint2D & orig );
      PathPoint2D( Concrete::Coords2D * _mid );
      PathPoint2D( Concrete::Length midx, Concrete::Length midy );
      ~PathPoint2D( );
      Concrete::PathPoint2D * transformed( const Lang::Transform2D & tf ) const;
      Concrete::PathPoint3D * transformed( const Lang::Transform3D & tf ) const; // treat as z = 0
      Concrete::PathPoint3D * typed_to3D( ) const; // treat as z = 0
    };
    
    class PathPoint3D
      {
      public:
	Concrete::Coords3D * rear_;
	Concrete::Coords3D * mid_;
	Concrete::Coords3D * front_;
	
	PathPoint3D( const Concrete::PathPoint3D & orig );
	PathPoint3D( Concrete::Coords3D * mid );
	PathPoint3D( const Concrete::Coords2D & mid );  // treat as z = 0
	PathPoint3D( Concrete::Length midx, Concrete::Length midy, Concrete::Length midz );
	~PathPoint3D( );
	Concrete::PathPoint3D * transformed( const Lang::Transform3D & tf ) const;
	Concrete::PathPoint2D * make2D( Concrete::Length eyez ) const;
      };

  }
  
}

#endif
