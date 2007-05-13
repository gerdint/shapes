#include <cmath>

#include "metapdftypes.h"
#include "metapdfexceptions.h"
#include "metapdfastexpr.h"
#include "consts.h"
#include "angleselect.h"
#include "metapdfastvar.h"
#include "metapdfastclass.h"
#include "globals.h"
#include "continuations.h"
#include "check.h"

//#include "clapack.h"
//#include "cblas.h"
#include <gsl/gsl_matrix.h>
#include <gsl/gsl_linalg.h>
#include <gsl/gsl_blas.h>
#include <ctype.h>
#include <stack>

using namespace MetaPDF;
using namespace std;

void displayArray( std::ostream & os, const double * pr, size_t m, size_t n )
{
  size_t r;
  size_t c;
  char buf[20];
  for( r = 0; r < m; ++r )
    {
      for( c = 0; c < n; ++c )
        {
          sprintf( buf, "%14.5e", *( pr + ( r + c * m ) ) );
	  os << buf ;
        }
      os << std::endl ;
    }
}

void displayArray( std::ostream & os, const gsl_matrix * m )
{
  char buf[20];
  for( size_t r = 0; r < m->size1; ++r )
    {
      for( size_t c = 0; c < m->size2; ++c )
        {
          sprintf( buf, "%14.5e", gsl_matrix_get( m, r, c ) );
	  os << buf ;
        }
      os << std::endl ;
    }
}


Lang::Transform2D::Transform2D( double xx, double yx, double xy, double yy, Concrete::Length xt, Concrete::Length yt )
  : xx_( xx ), yx_( yx ), xy_( xy ), yy_( yy ), xt_( xt ), yt_( yt )
{ }

Lang::Transform2D::Transform2D( const Lang::Transform2D & tf2, const Lang::Transform2D & tf1 )
  : xx_( tf2.xx_ * tf1.xx_ + tf2.xy_ * tf1.yx_ ),
    yx_( tf2.yx_ * tf1.xx_ + tf2.yy_ * tf1.yx_ ),
    xy_( tf2.xx_ * tf1.xy_ + tf2.xy_ * tf1.yy_ ),
    yy_( tf2.yx_ * tf1.xy_ + tf2.yy_ * tf1.yy_ ),
    xt_( tf2.xx_ * tf1.xt_ + tf2.xy_ * tf1.yt_ + tf2.xt_ ),
    yt_( tf2.yx_ * tf1.xt_ + tf2.yy_ * tf1.yt_ + tf2.yt_ )
{ }

DISPATCHIMPL( Transform2D );

bool
Lang::Transform2D::isIdentity( ) const
{
  return
    xt_ == Concrete::ZERO_LENGTH && yt_ == Concrete::ZERO_LENGTH &&
    xx_ == 1 && yy_ == 1 &&
    xy_ == 0 && yx_ == 0;
}

bool
Lang::Transform2D::isTranslation( ) const
{
  return
    xx_ == 1 && yy_ == 1 &&
    xy_ == 0 && yx_ == 0;
}

void
Lang::Transform2D::shipout( std::ostream & os ) const
{
  os << xx_ << " " << yx_ << " " << xy_ << " " << yy_ << " "
     << Concrete::Length::offtype( xt_ ) << " " << Concrete::Length::offtype( yt_ ) ;
}

// to be used by text moveto commands
void
Lang::Transform2D::replaceBy( const Lang::Transform2D & newtf )
{
  xx_ = newtf.xx_;
  yx_ = newtf.yx_;
  xy_ = newtf.xy_;
  yy_ = newtf.yy_;
  xt_ = newtf.xt_;
  yt_ = newtf.yt_;
}

// to be used by text newline commands
void
Lang::Transform2D::prependShift( const Concrete::Coords2D & d )
{
  // Think of d as tf1 when composing transforms.

  xt_ += xx_ * d.x_ + xy_ * d.y_;
  yt_ += yx_ * d.x_ + yy_ * d.y_;
}

// to be used by text painting commands
void
Lang::Transform2D::prependXShift( const Concrete::Length & dx )
{
  // Think of d as tf1 when composing transforms.

  xt_ += xx_ * dx;
  yt_ += yx_ * dx;
}

RefCountPtr< const Lang::Class > Lang::Transform2D::TypeID( new Lang::SystemFinalClass( strrefdup( "Transform2D" ) ) );
TYPEINFOIMPL( Transform2D );

void
Lang::Transform2D::show( std::ostream & os ) const
{
  os << "[ ("
     << xx_ << ", " << yx_ << ") ("
     << xy_ << ", " << yy_ << ") ("
     << Lang::Length( xt_ ) << ", " << Lang::Length( yt_ ) << ") ]" ;
}


Lang::Transform3D::Transform3D( double xx, double yx, double zx, double xy, double yy, double zy, double xz, double yz, double zz, Concrete::Length xt, Concrete::Length yt, Concrete::Length zt )
  : planeNormalTransformData_( 0 ),
    xx_( xx ), yx_( yx ), zx_( zx ), xy_( xy ), yy_( yy ), zy_( zy ), xz_( xz ), yz_( yz ), zz_( zz ), xt_( xt ), yt_( yt ), zt_( zt )
{ }

Lang::Transform3D::Transform3D( const Lang::Transform3D & tf2, const Lang::Transform3D & tf1 )
  : planeNormalTransformData_( 0 ),
    xx_( tf2.xx_ * tf1.xx_ + tf2.xy_ * tf1.yx_ + tf2.xz_ * tf1.zx_ ),
    yx_( tf2.yx_ * tf1.xx_ + tf2.yy_ * tf1.yx_ + tf2.yz_ * tf1.zx_ ),
    zx_( tf2.zx_ * tf1.xx_ + tf2.zy_ * tf1.yx_ + tf2.zz_ * tf1.zx_ ),
    xy_( tf2.xx_ * tf1.xy_ + tf2.xy_ * tf1.yy_ + tf2.xz_ * tf1.zy_ ),
    yy_( tf2.yx_ * tf1.xy_ + tf2.yy_ * tf1.yy_ + tf2.yz_ * tf1.zy_ ),
    zy_( tf2.zx_ * tf1.xy_ + tf2.zy_ * tf1.yy_ + tf2.zz_ * tf1.zy_ ),
    xz_( tf2.xx_ * tf1.xz_ + tf2.xy_ * tf1.yz_ + tf2.xz_ * tf1.zz_ ),
    yz_( tf2.yx_ * tf1.xz_ + tf2.yy_ * tf1.yz_ + tf2.yz_ * tf1.zz_ ),
    zz_( tf2.zx_ * tf1.xz_ + tf2.zy_ * tf1.yz_ + tf2.zz_ * tf1.zz_ ),
    xt_( tf2.xx_ * tf1.xt_ + tf2.xy_ * tf1.yt_ + tf2.xz_ * tf1.zt_ + tf2.xt_ ),
    yt_( tf2.yx_ * tf1.xt_ + tf2.yy_ * tf1.yt_ + tf2.yz_ * tf1.zt_ + tf2.yt_ ),
    zt_( tf2.zx_ * tf1.xt_ + tf2.zy_ * tf1.yt_ + tf2.zz_ * tf1.zt_ + tf2.zt_ )
{ }

DISPATCHIMPL( Transform3D );

Lang::Transform3D::~Transform3D( )
{
  if( planeNormalTransformData_ != 0 )
    {
      gsl_matrix_free( planeNormalTransformData_ );
      //      delete planeNormalTransformData_;
    }
}

bool
Lang::Transform3D::isIdentity( ) const
{
  return
    xt_ == Concrete::ZERO_LENGTH && yt_ == Concrete::ZERO_LENGTH && zt_ == Concrete::ZERO_LENGTH &&
    xx_ == 1 && yy_ == 1 && zz_ == 1 &&
    xy_ == 0 && xz_ == 0 && yx_ == 0 && yz_ == 0 && zx_ == 0 && zy_ == 0;
}

Concrete::UnitFloatTriple
Lang::Transform3D::transformPlaneUnitNormal( const Concrete::UnitFloatTriple & n ) const
{
  const int N = 3;
  // These statically allocated matrices will leak memory.  They could be removed by using a static automaitc
  // deallocator object.  However, since they shall not be deleted by delete (but by gsl_matrix_free), we cannot
  // us RefCountPtr< gsl_matrix >.
  static gsl_matrix * a = gsl_matrix_alloc( N, N );
  static gsl_matrix * v = gsl_matrix_alloc( N, N );
  static gsl_vector * s = gsl_vector_alloc( N );
  static gsl_vector * work = gsl_vector_alloc( N );


//   static __CLPK_integer mn = N;
//   static char jobuvt = 'A';

//   static double a[ N * N ];
//   static double u[ N * N ];
//   static double vt[ N * N ];
//   __CLPK_integer ldauvt = N;
//   static double s[ N ];
//   __CLPK_integer lwork;
//   __CLPK_integer info;

//   static __CLPK_doublereal * work = 0;
//   static RefCountPtr< __CLPK_doublereal > workCleaner;

//   if( work == 0 )
//     {
//       // Then we ask LAPACK how much workspace it wants, and then we assume that this number will not change
//       // as we call DGESVD with other arguments.
  
//       double tmpwork;
//       lwork = -1;
//       dgesvd_( & jobuvt, & jobuvt,
// 	       & mn, & mn,
// 	       reinterpret_cast< __CLPK_doublereal * >( & a ), & ldauvt,
// 	       reinterpret_cast< __CLPK_doublereal * >( & s ),
// 	       reinterpret_cast< __CLPK_doublereal * >( & u ), & ldauvt,
// 	       reinterpret_cast< __CLPK_doublereal * >( & vt ), & ldauvt,
// 	       reinterpret_cast< __CLPK_doublereal * >( & tmpwork ), & lwork,
// 	       & info );
//       lwork = static_cast< __CLPK_integer >( tmpwork );
//       work = new __CLPK_doublereal[ lwork ];
//       workCleaner = RefCountPtr< __CLPK_doublereal >( work );
//     }


  if( planeNormalTransformData_ == 0 )
    {
      // This is the first time this transform is used to transform a unit plane normal.
      // The linear part of this transform must then be computed, and we use the singular
      // value decomposition for this.

      planeNormalTransformData_ = gsl_matrix_alloc( N, N );

      gsl_matrix_set( a, 0, 0, xx_ );
      gsl_matrix_set( a, 1, 0, yx_ );
      gsl_matrix_set( a, 2, 0, zx_ );
      gsl_matrix_set( a, 0, 1, xy_ );
      gsl_matrix_set( a, 1, 1, yy_ );
      gsl_matrix_set( a, 2, 1, zy_ );
      gsl_matrix_set( a, 0, 2, xz_ );
      gsl_matrix_set( a, 1, 2, yz_ );
      gsl_matrix_set( a, 2, 2, zz_ );

//       std::cerr << "Here's a:" << std::endl ;
//       displayArray( std::cerr, a );

      {
	int status = gsl_linalg_SV_decomp( a, v, s, work );
	if( status != 0 )
	  {
	    std::cerr << "Gnu Scientific Library SVD routine failed." << std::endl ;
	    exit( 1 );
	  }
      }

//       a[ 0 ] = xx_;
//       a[ 1 ] = yx_;
//       a[ 2 ] = zx_;
//       a[ 3 ] = xy_;
//       a[ 4 ] = yy_;
//       a[ 5 ] = zy_;
//       a[ 6 ] = xz_;
//       a[ 7 ] = yz_;
//       a[ 8 ] = zz_;

//       dgesvd_( & jobuvt, & jobuvt,
// 	       & mn, & mn,
// 	       reinterpret_cast< __CLPK_doublereal * >( & a ), & ldauvt,
// 	       reinterpret_cast< __CLPK_doublereal * >( & s ),
// 	       reinterpret_cast< __CLPK_doublereal * >( & u ), & ldauvt,
// 	       reinterpret_cast< __CLPK_doublereal * >( & vt ), & ldauvt,
// 	       reinterpret_cast< __CLPK_doublereal * >( work ), & lwork,
// 	       & info );
  
//       if( info != 0 )
// 	{
// 	  std::cerr << "LAPACK routine DGESVD failed." << std::endl ;
// 	  exit( 1 );
// 	}

      if( gsl_vector_get( s, 1 ) < 1e-5 )
	{
	  throw Exceptions::AffineTransformKillsPlane( gsl_vector_get( s, 1 ) );
	}
      
//       std::cerr << "Here's s:" << std::endl ;
//       gsl_vector_fprintf( stderr, s, "%f" );

      gsl_vector_set( s, 0, gsl_vector_get( s, 2 ) / gsl_vector_get( s, 0 ) );
      gsl_vector_set( s, 1, gsl_vector_get( s, 2 ) / gsl_vector_get( s, 1 ) );
      //      s[ 2 ] = 1;

//       std::cerr << "Here's the modified s:" << std::endl ;
//       gsl_vector_fprintf( stderr, s, "%f" );


//       if( s[ 1 ] < 1e-5 )
// 	{
// 	  throw Exceptions::AffineTransformKillsPlane( s[ 1 ] );
// 	}

//       s[ 0 ] = s[ 2 ] / s[ 0 ];
//       s[ 1 ] = s[ 2 ] / s[ 1 ];
//       //      s[ 2 ] = 1;

      // We will now compute " u * diag( s ) * vt ".

      // Note that "u" is stored in a when gsl_linalg_SV_decomp is used.

//       std::cerr << "Here's u:" << std::endl ;
//       displayArray( std::cerr, a );

      gsl_matrix_set( a, 0, 0, gsl_matrix_get( a, 0, 0 ) * gsl_vector_get( s, 0 ) );
      gsl_matrix_set( a, 1, 0, gsl_matrix_get( a, 1, 0 ) * gsl_vector_get( s, 0 ) );
      gsl_matrix_set( a, 2, 0, gsl_matrix_get( a, 2, 0 ) * gsl_vector_get( s, 0 ) );
      gsl_matrix_set( a, 0, 1, gsl_matrix_get( a, 0, 1 ) * gsl_vector_get( s, 1 ) );
      gsl_matrix_set( a, 1, 1, gsl_matrix_get( a, 1, 1 ) * gsl_vector_get( s, 1 ) );
      gsl_matrix_set( a, 2, 1, gsl_matrix_get( a, 2, 1 ) * gsl_vector_get( s, 1 ) );
      //  Note that s[ 2 ] == 1

//       u[ 0 ] *= s[ 0 ];
//       u[ 1 ] *= s[ 0 ];
//       u[ 2 ] *= s[ 0 ];
//       u[ 3 ] *= s[ 1 ];
//       u[ 4 ] *= s[ 1 ];
//       u[ 5 ] *= s[ 1 ];
//       //  Note that s[ 2 ] == 1

//       std::cerr << "Here's the modified a:" << std::endl ;
//       displayArray( std::cerr, a );

//       std::cerr << "Here's v:" << std::endl ;
//       displayArray( std::cerr, v );

      gsl_blas_dgemm( CblasNoTrans,
		      CblasTrans,
		      1.,
		      a,
		      v,
		      0.,
		      planeNormalTransformData_ );

//       std::cerr << "Here's the matrix:" << std::endl ;
//       displayArray( std::cerr, planeNormalTransformData_ );

//       cblas_dgemm( CblasColMajor, CblasNoTrans, CblasNoTrans,
// 		   N, N, N,
// 		   1.,
// 		   u, N,
// 		   vt, N,
// 		   0., planeNormalTransformData_, N );
    }

  // Now that the linear, not too singular, transform has been computed, we just apply it and normalize the result.
  // The normalization is made by the UnitFloatTriple constructor.
  
  static gsl_vector * _n = gsl_vector_alloc( N );
  static gsl_vector * res = gsl_vector_alloc( N );

  //  static double _n[ N ];
  //  static double res[ N ];
  
  gsl_vector_set( _n, 0, n.x_ );
  gsl_vector_set( _n, 1, n.y_ );
  gsl_vector_set( _n, 2, n.z_ );

//   _n[ 0 ] = n.x_;
//   _n[ 1 ] = n.y_;
//   _n[ 2 ] = n.z_;
  
//   std::cerr << "Input vector: " << std::endl ;
//   gsl_vector_fprintf( stderr, _n, "%f" );
//   std::cerr << std::endl ;

  gsl_blas_dgemv( CblasNoTrans,
		  1.,
		  planeNormalTransformData_,
		  _n,
		  0.,
		  res );

//   std::cerr << "Result: " << std::endl ;
//   gsl_vector_fprintf( stderr, res, "%f" );
//   std::cerr << std::endl ;

//   cblas_dgemv( CblasColMajor, CblasNoTrans,
// 	       N, N,
// 	       1.,
// 	       planeNormalTransformData_, N,
// 	       reinterpret_cast< double * >( & _n ), 1,
// 	       0., reinterpret_cast< double * >( & res ), 1 );
  
  return Concrete::UnitFloatTriple( gsl_vector_get( res, 0 ),
				    gsl_vector_get( res, 1 ),
				    gsl_vector_get( res, 2 ) );
  //  return Concrete::UnitFloatTriple( res[0], res[1], res[2] );

  /*
      {
	// First we compute a unit vector in the plane
	static const Concrete::UnitFloatTriple xHat( 1, 0, 0 );	
	static const Concrete::UnitFloatTriple yHat( 0, 1, 0 );
	Concrete::UnitFloatTriple r1 = MetaPDF::cross( unitNormal_, xHat );
	Concrete::UnitFloatTriple r2 = MetaPDF::cross( unitNormal_, yHat );
	Concrete::UnitFloatTriple r( 0, 0, 0 );
	if( r1.norm( ) > r2.norm( ) )
	  {
	    r = r1.normalized( );
	  }
	else
	  {
	    r = r2.normalized( );
	  }
	
	// Then we find one point that is in the plane
	double ax = fabs( unitNormal_.x_ );
	double ay = fabs( unitNormal_.y_ );
	double az = fabs( unitNormal_.z_ );
	
	Concrete::Coords3D x0( 0, 0, 0 );
	if( ax >= ay && ax >= az )
	  {
	    x0 = Concrete::Coords3D( m_ / unitNormal_.x_, 0, 0 );
	  }
	else if( ay >= az )
	  {
	    x0 = Concrete::Coords3D( 0, m_ / unitNormal_.y_, 0 );
	  }
	else
	  {
	    x0 = Concrete::Coords3D( 0, 0, m_ / unitNormal_.z_ );
	  }

	// Now it is easy to find two more points that span the plane together with x0
	Concrete::Coords3D x1 = x0 + r;
	Concrete::Coords3D x2 = x0 + MetaPDF::cross( unitNormal_, r );

	// Now we see where these points go...
	Concrete::Coords3D Tx0 = x0.transformed( tf );
	Concrete::Coords3D Tx1 = x1.transformed( tf );
	Concrete::Coords3D Tx2 = x2.transformed( tf );

	// ... from which the new equation may be computed.
	Concrete::UnitFloatTriple Tnormal = MetaPDF::cross( Tx1 - Tx0, Tx2 - Tx0 );
	double TnormalNorm = Tnormal.norm( );
	if( TnormalNorm == 0 )
	  {
	    // A polygon of lower dimension is invisible, so we may just return without pushing any triangles.
	    return;
	  }
	Concrete::UnitFloatTriple TunitNormal = Tnormal * ( 1 / TnormalNorm );
  */
}

RefCountPtr< const Lang::Class > Lang::Transform3D::TypeID( new Lang::SystemFinalClass( strrefdup( "Transform3D" ) ) );
TYPEINFOIMPL( Transform3D );

void
Lang::Transform3D::show( std::ostream & os ) const
{
  os << "[ ("
     << xx_ << ", " << yx_ << ", " << zx_ << ") ("
     << xy_ << ", " << yy_ << ", " << zy_ << ") ("
     << xz_ << ", " << yz_ << ", " << zz_ << ") ("
     << Lang::Length( xt_ ) << ", " << Lang::Length( yt_ ) << ", " << Lang::Length( zt_ ) << ") ]" ;
}


Kernel::Arguments::Arguments( const Kernel::EvaluatedFormals * formals )
  : formals_( formals ), variables_( new Environment::ValueVector::ValueType ), dst_( 0 ),
    hasSink_( formals_->formals_->hasSink( ) ),
    dstEnd_( formals_->formals_->defaultExprs_.size( ) ),
    isSink_( formals_->isSink_ ),
    sinkArgList_( 0 ), sinkValues_( NullPtr< const Lang::SingleList >( ) ),
    states_( new Environment::StateVector::ValueType ), stateDst_( 0 )
{
  if( hasSink_ )
    {
      sinkArgList_ = new Ast::ArgListExprs( false ); // This is not an expression-owner.
      sinkValues_ = RefCountPtr< const Lang::SingleList >( new Lang::SingleListNull( ) );
      if( formals_->formals_->argumentOrder_->size( ) == 0 )
	{
	  // All arguments go in the sink.
	  dst_ = INT_MAX;
	}
    }
  // Thinking of all the evaluated cuts, it actually makes some sense not to reserve memory here.
  //  variables_.reserve( formals_->argumentOrder_->size( ) );
  //  states_.reserve( formals_->stateOrder_->size( ) );
}

Kernel::Arguments::~Arguments( )
{ }

Kernel::Arguments
Kernel::Arguments::clone( ) const
{
  CHECK(
	if( states_->size( ) != 0 )
	  {
	    throw Exceptions::InternalError( "Arguments with states may not be cloned." );
	  }
	);

  Kernel::Arguments res( formals_ );

  res.variables_->reserve( variables_->size( ) );
  {
    typedef typeof *variables_ ListType;
    for( ListType::const_iterator i = variables_->begin( ); i != variables_->end( ); ++i )
      {
	res.variables_->push_back( *i );
      }
  }
  res.locations_ = locations_;
  res.dst_ = dst_;
  if( sinkArgList_ != 0 )
    {
      throw Exceptions::NotImplemented( "Cloning of arguments with sink." );
      //res.sinkArgList_ = sinkArgList_->clone( );
      res.sinkValues_ = sinkValues_;
    }

  return res;
}


void
Kernel::Arguments::addOrderedArgument( const Kernel::VariableHandle & arg, Ast::Expression * loc )
{
  /* Recall that if there's a sink, this will be the last argument.
   */
  if( ! isSink_ &&
      dst_ == dstEnd_ )
    {
      // Put it in the sink.
      CHECK(
	    if( ! hasSink_ )
	      {
		throw Exceptions::InternalError( "Excess of arguments and no sink." );
	      }
	    );
      sinkArgList_->orderedExprs_->push_back( loc );
      sinkValues_ = RefCountPtr< Lang::SingleList >( new Lang::SingleListPair( arg, sinkValues_ ) );
    }
  else if( dst_ == variables_->size( ) )
    {
      variables_->push_back( arg );
      locations_.push_back( loc );
      ++dst_;
    }
  else
    {
      (*variables_)[ dst_ ] = arg;
      locations_[ dst_ ] = loc;
      while( dst_ < variables_->size( ) &&
	     (*variables_)[ dst_ ] != Kernel::THE_SLOT_VARIABLE )
	{
	  ++dst_;
	}
    }
}

void
Kernel::Arguments::addNamedArgument( const char * id, const Kernel::VariableHandle & arg, Ast::Expression * loc )
{
  if( formals_ == 0 )
    {
      throw Exceptions::CoreNoNamedFormals( "???" );
    }

  typedef typeof *(formals_->formals_->argumentOrder_) FormalsMapType;
  FormalsMapType & formalsMap = *(formals_->formals_->argumentOrder_);

  /* Note that the name of the sink is invisible, so referring to it is just another arguments which is
   * put in the sink.  This variable happens to have the same name as the sink.
   */
  FormalsMapType::const_iterator j = formalsMap.find( id );
  if( j == formalsMap.end( ) ||
      ( hasSink_ &&
	j->second == dstEnd_ ) )
    {
      if( hasSink_ )
	{
	  if( sinkArgList_->namedExprs_->find( id ) != sinkArgList_->namedExprs_->end( ) )
	    {
	      throw Exceptions::InternalError( "It is a surprise that the sink got a repeated formal." );
	    }
	  sinkArgList_->namedExprs_->insert( std::pair< const char *, Ast::Expression * >( id, loc ) );
	  sinkValues_ = RefCountPtr< Lang::SingleList >( new Lang::SingleListPair( arg, sinkValues_ ) );
	  return;
	}
      throw Exceptions::NamedFormalMismatch( formals_->formals_->loc( ), strrefdup( id ), Exceptions::NamedFormalMismatch::VARIABLE );
    }
  size_t pos = j->second;

  if( pos < dst_ )
    {
      throw Exceptions::NamedFormalAlreadySpecified( formals_->formals_->loc( ), strrefdup( id ), pos, Exceptions::NamedFormalAlreadySpecified::VARIABLE );
    }
  
  if( pos >= variables_->size( ) )
    {
      while( variables_->size( ) < pos )
	{
	  variables_->push_back( Kernel::THE_SLOT_VARIABLE );
	  locations_.push_back( 0 );
	}
      variables_->push_back( arg );
      locations_.push_back( loc );
    }
  else
    {
      if( (*variables_)[ pos ] != Kernel::THE_SLOT_VARIABLE )
	{
	  throw Exceptions::NamedFormalAlreadySpecified( formals_->formals_->loc( ), strrefdup( id ), pos, Exceptions::NamedFormalAlreadySpecified::VARIABLE );
	}
      (*variables_)[ pos ] = arg;
      locations_[ pos ] = loc;
    }
  if( pos == dst_ )
    {
      while( dst_ < variables_->size( ) &&
	     (*variables_)[ dst_ ] != Kernel::THE_SLOT_VARIABLE )
	{
	  ++dst_;
	}
    }
}

void
Kernel::Arguments::addOrderedState( const Kernel::StateHandle & state, Ast::Node * loc )
{
  if( stateDst_ == states_->size( ) )
    {
      states_->push_back( state );
      stateLocations_.push_back( loc );
      ++stateDst_;
    }
  else
    {
      (*states_)[ stateDst_ ] = state;
      stateLocations_[ stateDst_ ] = loc;
      while( stateDst_ < states_->size( ) &&
	     (*states_)[ stateDst_ ] != Kernel::THE_SLOT_STATE )
	{
	  ++stateDst_;
	}
    }
}

void
Kernel::Arguments::addNamedState( const char * id, const Kernel::StateHandle & state, Ast::Node * loc )
{
  if( formals_ == 0 )
    {
      throw Exceptions::CoreNoNamedFormals( "???" );
    }
  
  typedef typeof *(formals_->formals_->stateOrder_) FormalsMapType;
  FormalsMapType & formalsMap = *(formals_->formals_->stateOrder_);

  FormalsMapType::const_iterator j = formalsMap.find( id );
  if( j == formalsMap.end( ) )
    {
      throw Exceptions::NamedFormalMismatch( formals_->formals_->loc( ), strrefdup( id ), Exceptions::NamedFormalMismatch::STATE );
    }
  size_t pos = j->second;

  if( pos < stateDst_ )
    {
      throw Exceptions::NamedFormalAlreadySpecified( formals_->formals_->loc( ), strrefdup( id ), pos, Exceptions::NamedFormalAlreadySpecified::STATE );
    }
  
  if( pos >= states_->size( ) )
    {
      while( states_->size( ) < pos )
	{
	  states_->push_back( Kernel::THE_SLOT_STATE );
	  stateLocations_.push_back( 0 );
	}
      states_->push_back( state );
      stateLocations_.push_back( loc );
    }
  else
    {
      if( (*states_)[ pos ] != Kernel::THE_SLOT_STATE )
	{
	  throw Exceptions::NamedFormalAlreadySpecified( formals_->formals_->loc( ), strrefdup( id ), pos, Exceptions::NamedFormalAlreadySpecified::STATE );
	}
      (*states_)[ pos ] = state;
      stateLocations_[ pos ] = loc;
    }
  if( pos == stateDst_ )
    {
      while( stateDst_ < states_->size( ) &&
	     (*states_)[ stateDst_ ] != Kernel::THE_SLOT_STATE )
	{
	  ++stateDst_;
	}
    }
}

void
Kernel::Arguments::applyDefaults( )
{
  if( formals_ == 0 )
    {
      return;
    }

  size_t numberOfArguments = variables_->size( );
  size_t formalsSize = formals_->defaults_.size( );

  if( numberOfArguments > formalsSize &&
      ! hasSink_ )
    {
      /* The location of the ball must be set by the caller. */
      throw Exceptions::UserArityMismatch( formals_->formals_->loc( ), formalsSize, numberOfArguments, Exceptions::UserArityMismatch::VARIABLE );
    }

  size_t numberOfStates = states_->size( );
  size_t formalsStateSize = formals_->formals_->stateOrder_->size( );
  if( numberOfStates > formalsStateSize )
    {
      /* The location of the ball must be set by the caller. */
      throw Exceptions::UserArityMismatch( formals_->formals_->loc( ), formalsStateSize, numberOfStates, Exceptions::UserArityMismatch::STATE );
    }

  /* First the easy part:  All states must be specified.
   */
  std::map< size_t, RefCountPtr< const char > > * missingStates = 0;
  {
    size_t pos = 0;
    typedef typeof *states_ ListType;
    for( ListType::const_iterator i = states_->begin( ); i != states_->end( ); ++i, ++pos )
      {
	if( *i == Kernel::THE_SLOT_STATE )
	  {
	    if( missingStates == 0 )
	      {
		missingStates = new typeof *missingStates;
	      }
	    typedef typeof *(formals_->formals_->stateOrder_) FormalsMapType;
	    FormalsMapType & formalsMap = *(formals_->formals_->stateOrder_);
	    for( FormalsMapType::const_iterator i = formalsMap.begin( ); ; )
	      {
		if( i->second == pos )
		  {
		    missingStates->insert( missingStates->begin( ), std::pair< size_t, RefCountPtr< const char > >( pos, strrefdup( i->first ) ) );
		    break;
		  }
		++i;
		if( i == formalsMap.end( ) )
		  {
		    throw Exceptions::InternalError( "Failed to find position of missing state." );
		  }
	      }

	  }
      }
  }

  /* Allocate positions in the vector for all arguments.
   */
  variables_->reserve( hasSink_ ? formalsSize + 1 : formalsSize );
  while( variables_->size( ) < formalsSize )
    {
      variables_->push_back( Kernel::THE_SLOT_VARIABLE );
    }
  locations_.resize( formalsSize );

  typedef typeof *variables_ MyListType;
  typedef typeof formals_->defaults_ DefaultListType;
  typedef typeof formals_->locations_ LocationListType;
  DefaultListType::const_iterator src = formals_->defaults_.begin( );
  LocationListType::const_iterator srcLoc = formals_->locations_.begin( );
  std::map< size_t, RefCountPtr< const char > > * missingArgs = 0;
  size_t pos = 0;
  typedef typeof locations_ MyLocationsType;
  for( MyListType::iterator dst = variables_->begin( ); dst != variables_->end( ); ++dst, ++src, ++srcLoc, ++pos )
    {
      if( *dst == Kernel::THE_SLOT_VARIABLE )
	{
	  /* Handle error situation.
	   */
	  if( *src == Kernel::THE_SLOT_VARIABLE )
	    {
	      if( missingArgs == 0 )
		{
		  missingArgs = new typeof *missingArgs;
		}
	      typedef typeof *(formals_->formals_->argumentOrder_) FormalsMapType;
	      FormalsMapType & formalsMap = *(formals_->formals_->argumentOrder_);
	      for( FormalsMapType::const_iterator i = formalsMap.begin( ); ; )
		{
		  if( i->second == pos )
		    {
		      missingArgs->insert( missingArgs->begin( ), std::pair< size_t, RefCountPtr< const char > >( pos, strrefdup( i->first ) ) );
		      break;
		    }
		  ++i;
		  if( i == formalsMap.end( ) )
		    {
		      throw Exceptions::InternalError( "Failed to find position of missing argument." );
		    }
		}
	    }
	  
	  /* Normal case.
	   */
	  *dst = *src;
	  locations_[ pos ] = *srcLoc;
	}
    }
  
  if( missingArgs != 0 || missingStates != 0 )
    {
      throw Exceptions::MissingArguments( formals_->formals_->loc( ), missingArgs, missingStates );
    }

  if( hasSink_ )
    {
      variables_->push_back
	( Helpers::newValHandle
	  ( new Lang::Structure( sinkArgList_,
				 sinkValues_, 
				 true ) ) ); // true means that the sinkArgList_ gets owned by the Structure.
      sinkArgList_ = 0;
    }
}


Kernel::VariableHandle &
Kernel::Arguments::getHandle( size_t i )
{
  return (*variables_)[ i ];
}

RefCountPtr< const Lang::Value > &
Kernel::Arguments::getValue( size_t i )
{
  return (*variables_)[ i ]->getUntyped( );
}

const Ast::SourceLocation &
Kernel::Arguments::getLoc( size_t i ) const
{
  return locations_[ i ]->loc( );
}

const Ast::Node *
Kernel::Arguments::getNode( size_t i ) const
{
  return locations_[ i ];
}

Kernel::Thunk *
Kernel::Arguments::getThunk( size_t i )
{
  return (*variables_)[ i ]->copyThunk( );
}

bool
Kernel::Arguments::isSlot( size_t i ) const
{
  return (*variables_)[ i ] == Kernel::THE_SLOT_VARIABLE;
}

size_t
Kernel::Arguments::size( ) const
{
  return variables_->size( );
}

void
Kernel::Arguments::gcMark( Kernel::GCMarkedSet & marked )
{
  {
    typedef typeof *variables_ ListType;
    for( ListType::const_iterator i = variables_->begin( ); i != variables_->end( ); ++i )
      {
	const_cast< Kernel::Variable * >( i->getPtr( ) )->gcMark( marked );
      }
  }
}

Kernel::Environment::ValueVector
Kernel::Arguments::getVariables( )
{
  return variables_;
}

Kernel::Environment::StateVector
Kernel::Arguments::getStates( )
{
  return states_;
}


namespace MetaPDF
{
  namespace Kernel
  {

  class FunctionOneHandleCont : public Kernel::Continuation
  {
    RefCountPtr< const Lang::Function > fun_;
    Kernel::PassedDyn dyn_;
    Kernel::ContRef cont_;
  public:
    FunctionOneHandleCont( const RefCountPtr< const Lang::Function > & fun, const Kernel::PassedDyn & dyn, Kernel::ContRef cont, const Ast::SourceLocation & traceLoc )
      : Kernel::Continuation( traceLoc ), fun_( fun ), dyn_( dyn ), cont_( cont )
    { }
    virtual ~FunctionOneHandleCont( ) { }
    virtual void takeHandle( Kernel::VariableHandle val, Kernel::EvalState * evalState, bool dummy ) const
    {
      /* This continuation really seeks forced arguments, for otherwise a thunk would have been generated directly.
       * However, this continuation takes handles anyway, since handles is what goes into the argument list.
       */
      
      if( val->isThunk( ) )
	{
	  val->force( val, evalState );
	  return;
	}

      Kernel::Arguments args = fun_->newCurriedArguments( );
      args.addOrderedArgument( val, & Ast::THE_INTERNAL_VALUE_EXPRESSION );

      evalState->dyn_ = dyn_;
      evalState->cont_ = cont_;
      fun_->call( evalState, args, traceLoc_ );
    }
    virtual void backTrace( std::list< Kernel::Continuation::BackTraceElem > * trace ) const
    {
      trace->push_front( Kernel::Continuation::BackTraceElem( this, "internal function call with one handle" ) );
      cont_->backTrace( trace );
    }
    virtual void gcMark( Kernel::GCMarkedSet & marked )
    {
      const_cast< Lang::Function * >( fun_.getPtr( ) )->gcMark( marked );
      dyn_->gcMark( marked );
      cont_->gcMark( marked );
    }
  };

  class FunctionTwoHandlesCont_2 : public Kernel::Continuation
  {
    RefCountPtr< const Lang::Function > fun_;
    Kernel::VariableHandle arg1_;
    Kernel::PassedDyn dyn_;
    Kernel::ContRef cont_;
  public:
    FunctionTwoHandlesCont_2( const RefCountPtr< const Lang::Function > & fun, const Kernel::VariableHandle & arg1, const Kernel::PassedDyn & dyn, Kernel::ContRef cont, const Ast::SourceLocation & traceLoc )
      : Kernel::Continuation( traceLoc ), fun_( fun ), arg1_( arg1 ), dyn_( dyn ), cont_( cont )
    { }
    virtual ~FunctionTwoHandlesCont_2( ) { }
    virtual void takeHandle( Kernel::VariableHandle arg2, Kernel::EvalState * evalState, bool dummy ) const
    {
      /* This continuation really seeks forced arguments, for otherwise a thunk would have been generated directly.
       * However, this continuation takes handles anyway, since handles is what goes into the argument list.
       */
      
      if( arg2->isThunk( ) )
	{
	  arg2->force( arg2, evalState );
	  return;
	}

      Kernel::Arguments args = fun_->newCurriedArguments( );
      args.addOrderedArgument( arg1_, & Ast::THE_INTERNAL_VALUE_EXPRESSION );
      args.addOrderedArgument( arg2, & Ast::THE_INTERNAL_VALUE_EXPRESSION );
      evalState->dyn_ = dyn_;
      evalState->cont_ = cont_;
      fun_->call( evalState, args, traceLoc_ );
    }
    virtual void backTrace( std::list< Kernel::Continuation::BackTraceElem > * trace ) const
    {
      trace->push_front( Kernel::Continuation::BackTraceElem( this, "internal function call with two handles, second" ) );
      cont_->backTrace( trace );
    }
    virtual void gcMark( Kernel::GCMarkedSet & marked )
    {
      const_cast< Lang::Function * >( fun_.getPtr( ) )->gcMark( marked );
      dyn_->gcMark( marked );
      cont_->gcMark( marked );
    }
  };

  class FunctionTwoHandlesCont_1 : public Kernel::Continuation
  {
    RefCountPtr< const Lang::Function > fun_;
    Kernel::VariableHandle arg2_;
    bool forceArg2_;
    Kernel::PassedDyn dyn_;
    Kernel::ContRef cont_;
  public:
    FunctionTwoHandlesCont_1( const RefCountPtr< const Lang::Function > & fun, const Kernel::VariableHandle & arg2, bool forceArg2, const Kernel::PassedDyn & dyn, Kernel::ContRef cont, const Ast::SourceLocation & traceLoc )
      : Kernel::Continuation( traceLoc ), fun_( fun ), arg2_( arg2 ), forceArg2_( forceArg2 ), dyn_( dyn ), cont_( cont )
    { }
    virtual ~FunctionTwoHandlesCont_1( ) { }
    virtual void takeHandle( Kernel::VariableHandle arg1, Kernel::EvalState * evalState, bool dummy ) const
    {
      /* This continuation really seeks forced arguments, for otherwise a thunk would have been generated directly.
       * However, this continuation takes handles anyway, since handles is what goes into the argument list.
       */
      
      if( arg1->isThunk( ) )
	{
	  arg1->force( arg1, evalState );
	  return;
	}

      if( forceArg2_ )
	{
	  Kernel::ContRef newCont = Kernel::ContRef( new Kernel::FunctionTwoHandlesCont_2( fun_, arg1, dyn_, cont_, traceLoc_ ) );
	  evalState->cont_ = newCont;
	  newCont->takeHandle( arg2_, evalState );
	  return;
	}

      /* The second handle need not be forced
       */
      Kernel::Arguments args = fun_->newCurriedArguments( );
      args.addOrderedArgument( arg1, & Ast::THE_INTERNAL_VALUE_EXPRESSION );
      args.addOrderedArgument( arg2_, & Ast::THE_INTERNAL_VALUE_EXPRESSION );
      evalState->dyn_ = dyn_;
      evalState->cont_ = cont_;
      fun_->call( evalState, args, traceLoc_ );
    }
    virtual void backTrace( std::list< Kernel::Continuation::BackTraceElem > * trace ) const
    {
      trace->push_front( Kernel::Continuation::BackTraceElem( this, "internal function call with two handles, first" ) );
      cont_->backTrace( trace );
    }
    virtual void gcMark( Kernel::GCMarkedSet & marked )
    {
      const_cast< Lang::Function * >( fun_.getPtr( ) )->gcMark( marked );
      dyn_->gcMark( marked );
      cont_->gcMark( marked );
    }
  };
  }
}


Lang::Function::Function( Kernel::EvaluatedFormals * formals )
  : formals_( formals )
{ }

DISPATCHIMPL( Function );

Lang::Function::~Function( )
{
  if( formals_ != 0 )
    {
      delete formals_->formals_;
      delete formals_;
    }
}

Kernel::ValueRef
Lang::Function::transformed( const Lang::Transform2D & tf, Kernel::ValueRef self ) const
{
  return Kernel::ValueRef( new Lang::TransformedFunction2D( tf, self.down_cast< const Lang::Function >( ) ) );
}

bool
Lang::Function::isProcedural( ) const
{
  return false;
}

void
Lang::Function::call( Kernel::EvalState * evalState, const Kernel::ValueRef & arg1, const Ast::SourceLocation & callLoc ) const
{
  Kernel::Arguments args = this->newCurriedArguments( );
  
  args.addOrderedArgument( Kernel::VariableHandle( new Kernel::Variable( arg1 ) ), & Ast::THE_INTERNAL_VALUE_EXPRESSION );

  this->call( evalState, args, callLoc );
}

void
Lang::Function::call( Kernel::EvalState * evalState, const Kernel::ValueRef & arg1, const Kernel::ValueRef & arg2, const Ast::SourceLocation & callLoc ) const
{
  Kernel::Arguments args = this->newCurriedArguments( );
  
  args.addOrderedArgument( Kernel::VariableHandle( new Kernel::Variable( arg1 ) ), & Ast::THE_INTERNAL_VALUE_EXPRESSION );
  args.addOrderedArgument( Kernel::VariableHandle( new Kernel::Variable( arg2 ) ), & Ast::THE_INTERNAL_VALUE_EXPRESSION );

  this->call( evalState, args, callLoc );
}

Ast::ArgListExprs * Lang::Function::oneExprArgList = new Ast::ArgListExprs( static_cast< size_t >( 1 ) );
Ast::ArgListExprs * Lang::Function::twoExprsArgList = new Ast::ArgListExprs( static_cast< size_t >( 2 ) );

void
Lang::Function::call( const RefCountPtr< const Lang::Function > & selfRef, Kernel::EvalState * evalState, const Kernel::VariableHandle & arg1, const Ast::SourceLocation & callLoc ) const
{
  const RefCountPtr< const Kernel::CallContInfo > info = this->newCallContInfo( Lang::Function::oneExprArgList, *evalState );
  
  if( info->force( 0 ) )
    {
      Kernel::ContRef cont = Kernel::ContRef( new Kernel::FunctionOneHandleCont( selfRef, evalState->dyn_, evalState->cont_, callLoc ) );
      evalState->cont_ = cont;
      cont->takeHandle( arg1, evalState );
      return;
    }

  /* The handle need not be forced
   */
  Kernel::Arguments args = this->newCurriedArguments( );
  args.addOrderedArgument( arg1, & Ast::THE_INTERNAL_VALUE_EXPRESSION );
  this->call( evalState, args, callLoc );
}

void
Lang::Function::call( const RefCountPtr< const Lang::Function > & selfRef, Kernel::EvalState * evalState, const Kernel::VariableHandle & arg1, const Kernel::VariableHandle & arg2, const Ast::SourceLocation & callLoc ) const
{
  const RefCountPtr< const Kernel::CallContInfo > info = this->newCallContInfo( Lang::Function::oneExprArgList, *evalState );

  /* Remember that arguments are ordered backwards!
   */
  
  if( info->force( 1 ) )
    {
      Kernel::ContRef cont = Kernel::ContRef( new Kernel::FunctionTwoHandlesCont_1( selfRef, arg2, info->force( 0 ), evalState->dyn_, evalState->cont_, callLoc ) );
      evalState->cont_ = cont;
      cont->takeHandle( arg1, evalState );
      return;
    }

  if( info->force( 0 ) )
    {
      Kernel::ContRef cont = Kernel::ContRef( new Kernel::FunctionTwoHandlesCont_2( selfRef, arg1, evalState->dyn_, evalState->cont_, callLoc ) );
      evalState->cont_ = cont;
      cont->takeHandle( arg2, evalState );
      return;
    }

  /* None of the handles need to be forced
   */
  Kernel::Arguments args = this->newCurriedArguments( );
  args.addOrderedArgument( arg1, & Ast::THE_INTERNAL_VALUE_EXPRESSION );
  args.addOrderedArgument( arg2, & Ast::THE_INTERNAL_VALUE_EXPRESSION );
  this->call( evalState, args, callLoc );
}


RefCountPtr< Kernel::CallContInfo >
Lang::Function::newCallContInfo( const Ast::ArgListExprs * argList, const Kernel::EvalState & evalState ) const
{
  return formals_->newCallContInfo( argList, evalState );
}

RefCountPtr< Kernel::CallContInfo >
Lang::Function::newCallContInfo( const Ast::ArgListExprs * argList, const Kernel::EvalState & evalState, const Kernel::Arguments & curryArgs ) const
{
  return formals_->newCallContInfo( argList, evalState, curryArgs );
}

Kernel::Arguments
Lang::Function::newCurriedArguments( ) const
{
  return Kernel::Arguments( formals_ );
}


RefCountPtr< const Lang::Class > Lang::Function::TypeID( new Lang::SystemFinalClass( strrefdup( "Function" ) ) );
TYPEINFOIMPL( Function );

Kernel::EvaluatedFormals::EvaluatedFormals( Kernel::Formals * formals )
  : selectiveForcing_( false ), forceAll_( false ), formals_( formals ), isSink_( true )
{ }

Kernel::EvaluatedFormals::EvaluatedFormals( const char * locationString )
  : selectiveForcing_( true ), formals_( 0 ), isSink_( true )
{
  Kernel::Formals * formals( new Kernel::Formals( ) );
  formals->setLoc( Ast::SourceLocation( locationString ) );
  formals_ = formals;
}

Kernel::EvaluatedFormals::EvaluatedFormals( const char * locationString, bool forceAll)
  : selectiveForcing_( false ), forceAll_( forceAll ), formals_( 0 ), isSink_( true )
{
  Kernel::Formals * formals( new Kernel::Formals( ) );
  formals->setLoc( Ast::SourceLocation( locationString ) );
  formals_ = formals;
}

Kernel::EvaluatedFormals::~EvaluatedFormals( )
{
  /* Don't delete the orderedFormals, since we don't own it.  A case for reference counting? */
}

RefCountPtr< Kernel::CallContInfo >
Kernel::EvaluatedFormals::newCallContInfo( const Ast::ArgListExprs * argList, const Kernel::EvalState & evalState ) const
{
  if( selectiveForcing_ )
    {
      return RefCountPtr< Kernel::CallContInfo >( new Kernel::CallContInfo( argList, evalState, formals_->newArgListForcePos( argList ) ) );
    }
  else
    {
      return RefCountPtr< Kernel::CallContInfo >( new Kernel::CallContInfo( argList, evalState, forceAll_ ) );
    }
}

RefCountPtr< Kernel::CallContInfo >
Kernel::EvaluatedFormals::newCallContInfo( const Ast::ArgListExprs * argList, const Kernel::EvalState & evalState, const Kernel::Arguments & curryArgs ) const
{
  if( selectiveForcing_ )
    {
      return RefCountPtr< Kernel::CallContInfo >( new Kernel::CallContInfo( argList, evalState, formals_->newArgListForcePos( argList, curryArgs ) ) );
    }
  else
    {
      return RefCountPtr< Kernel::CallContInfo >( new Kernel::CallContInfo( argList, evalState, forceAll_ ) );
    }
}

void
Kernel::EvaluatedFormals::appendEvaluatedFormal( const char * id, const Kernel::VariableHandle & defaultVal, const Ast::Node * loc, bool force )
{
  if( ! selectiveForcing_ )
    {
      throw Exceptions::InternalError( "EvaluatedFormals::appendEvaluatedFormal: Function does not have selective forcing." );
    }
  (*(formals_->argumentOrder_))[ id ] = defaults_.size( );
  formals_->forcePos_.push_back( force );
  defaults_.push_back( defaultVal );
  locations_.push_back( loc );
}

void
Kernel::EvaluatedFormals::appendEvaluatedFormal( const char * id, const Kernel::VariableHandle & defaultVal, const Ast::Node * loc )
{
  if( selectiveForcing_ )
    {
      throw Exceptions::InternalError( "EvaluatedFormals::appendEvaluatedFormal: Function requires individual forcing specification." );
    }
  (*(formals_->argumentOrder_))[ id ] = defaults_.size( );
  formals_->forcePos_.push_back( forceAll_ );
  defaults_.push_back( defaultVal );
  locations_.push_back( loc );
}

void
Kernel::EvaluatedFormals::appendEvaluatedCoreFormal( const char * id, const Kernel::VariableHandle & defaultVal, bool force )
{
  appendEvaluatedFormal( id, defaultVal, & Ast::THE_CORE_DEFAULT_VALUE_EXPRESSION, force );
}

void
Kernel::EvaluatedFormals::appendEvaluatedCoreFormal( const char * id, const Kernel::VariableHandle & defaultVal )
{
  appendEvaluatedFormal( id, defaultVal, & Ast::THE_CORE_DEFAULT_VALUE_EXPRESSION );
}

void
Kernel::EvaluatedFormals::gcMark( Kernel::GCMarkedSet & marked )
{
  typedef typeof defaults_ ListType;
  for( ListType::const_iterator i = defaults_.begin( ); i != defaults_.end( ); ++i )
    {
      if( *i != NullPtr< Kernel::Variable >( ) )
	{
	  const_cast< Kernel::Variable * >( i->getPtr( ) )->gcMark( marked );
	}
    }
}


Lang::CuteFunction::CuteFunction( RefCountPtr< const Lang::Function > callee, const Kernel::Arguments & someArgs )
  : Lang::Function( 0 ), callee_( callee), someArgs_( someArgs.clone( ) )
{ }

Lang::CuteFunction::~CuteFunction( )
{ }

RefCountPtr< Kernel::CallContInfo >
Lang::CuteFunction::newCallContInfo( const Ast::ArgListExprs * argList, const Kernel::EvalState & evalState ) const
{
  return callee_->newCallContInfo( argList, evalState, someArgs_ );
}

RefCountPtr< Kernel::CallContInfo >
Lang::CuteFunction::newCallContInfo( const Ast::ArgListExprs * argList, const Kernel::EvalState & evalState, const Kernel::Arguments & curryArgs ) const
{
  /* when we are the callee of a CuteFunction, our someArgs are part of that CuteFunction's someArgs, and hence curryArgs 
     contains everything that is to be passed to our callee.
  */
  return callee_->newCallContInfo( argList, evalState, curryArgs );
}

Kernel::Arguments
Lang::CuteFunction::newCurriedArguments( ) const
{
  return someArgs_.clone( );  
}

void
Lang::CuteFunction::call( Kernel::EvalState * evalState, Kernel::Arguments & args, const Ast::SourceLocation & callLoc ) const
{
  /* Note that curruying "takes place" in newCurriedArguments.  Other than that, this is the same as the original function.
     Also note that the total set of arguments being passed to the original function is no more than someArgs and what was passed in the last call.
   */
  callee_->call( evalState, args, callLoc );
}

bool
Lang::CuteFunction::isTransforming( ) const
{
  return callee_->isTransforming( );
}

void
Lang::CuteFunction::gcMark( Kernel::GCMarkedSet & marked )
{
  const_cast< Lang::Function * >( callee_.getPtr( ) )->gcMark( marked );
  someArgs_.gcMark( marked );
}


Lang::ComposedFunction::ComposedFunction( const RefCountPtr< const Lang::Function > & second, const RefCountPtr< const Lang::Function > & first )
  : Lang::Function( 0 ), second_( second ), first_( first )
{ }

Lang::ComposedFunction::~ComposedFunction( )
{ }

void
Lang::ComposedFunction::call( Kernel::EvalState * evalState, Kernel::Arguments & args, const Ast::SourceLocation & callLoc ) const
{
  /* Note that curruying "takes place" in newCurriedArguments.  Other than that, this is the same as the original function.
     Also note that the total set of arguments being passed to the original function is no more than someArgs and what was passed in the last call.
   */
  evalState->cont_ = Kernel::ContRef( new Kernel::ComposedFunctionCall_cont( second_, evalState->dyn_, evalState->cont_, callLoc ) );
  first_->call( evalState, args, callLoc );
}

RefCountPtr< Kernel::CallContInfo >
Lang::ComposedFunction::newCallContInfo( const Ast::ArgListExprs * argList, const Kernel::EvalState & evalState ) const
{
  return first_->newCallContInfo( argList, evalState );
}

RefCountPtr< Kernel::CallContInfo >
Lang::ComposedFunction::newCallContInfo( const Ast::ArgListExprs * argList, const Kernel::EvalState & evalState, const Kernel::Arguments & curryArgs ) const
{
  return first_->newCallContInfo( argList, evalState, curryArgs );
}

Kernel::Arguments
Lang::ComposedFunction::newCurriedArguments( ) const
{
  return first_->newCurriedArguments( );
}

bool
Lang::ComposedFunction::isTransforming( ) const
{
  return second_->isTransforming( );
}

void
Lang::ComposedFunction::gcMark( Kernel::GCMarkedSet & marked )
{
  const_cast< Lang::Function * >( second_.getPtr( ) )->gcMark( marked );
  const_cast< Lang::Function * >( first_.getPtr( ) )->gcMark( marked );
}


Lang::UserFunction::UserFunction( Kernel::EvaluatedFormals * formals, Ast::Expression * body, Kernel::PassedEnv env, const Ast::FunctionMode & functionMode )
  : Lang::Function( formals ), body_( body ), env_( env ), functionMode_( functionMode )
{ }

// DISPATCHIMPL( UserFunction );

Lang::UserFunction::~UserFunction( )
{
  // Note that we don't delete the things that we most likely share with other objects
  // Reference counting could be used here, but there will never be more such things than there are function expressions in the source

  // This prevents formals->formals from being deleted by Lang::Function::~Function
  delete formals_;
  formals_ = 0;
}

void
Lang::UserFunction::call( Kernel::EvalState * evalState, Kernel::Arguments & args, const Ast::SourceLocation & callLoc ) const
{
  args.applyDefaults( );
  evalState->env_ = new Kernel::Environment( Kernel::theEnvironmentList, env_, formals_->formals_->argumentOrder_, args.getVariables( ), formals_->formals_->stateOrder_, args.getStates( ) );

  if( ! this->isProcedural( ) )
    {
      evalState->env_->activateFunctionBoundary( );
    }

  body_->eval( evalState );
}

bool
Lang::UserFunction::isTransforming( ) const
{
  return ( functionMode_ & Ast::FUNCTION_TRANSFORMING ) != 0;
}

bool
Lang::UserFunction::isProcedural( ) const
{
  return ( functionMode_ & Ast::FUNCTION_PROCEDURAL ) != 0;
}

void
Lang::UserFunction::gcMark( Kernel::GCMarkedSet & marked )
{
  env_->gcMark( marked );
}


Lang::TransformedFunction2D::TransformedFunction2D( const Lang::Transform2D & tf, const RefCountPtr< const Lang::Function > & fun )
  : Lang::Function( 0 ), tf_( tf ), fun_( fun )
{ }

Lang::TransformedFunction2D::~TransformedFunction2D( )
{ }

void
Lang::TransformedFunction2D::call( Kernel::EvalState * evalState, Kernel::Arguments & args, const Ast::SourceLocation & callLoc ) const
{
  evalState->cont_ = Kernel::ContRef( new Kernel::Transform2DCont( tf_, evalState->cont_, 0 ) );
  fun_->call( evalState, args, callLoc );
}

bool
Lang::TransformedFunction2D::isTransforming( ) const
{
  return fun_->isTransforming( );
}

void
Lang::TransformedFunction2D::gcMark( Kernel::GCMarkedSet & marked )
{
  const_cast< Lang::Function * >( fun_.getPtr( ) )->gcMark( marked );
}


Lang::VectorFunction::VectorFunction( const std::vector< Kernel::ValueRef > * mem )
  : Lang::Function( new Kernel::EvaluatedFormals( "<vector>", true ) ), mem_( mem ),
    memNumeric_( NullPtr< const std::vector< double > >( ) )
{
  formals_->appendEvaluatedCoreFormal( "index", Kernel::THE_SLOT_VARIABLE );
}

// DISPATCHIMPL( VectorFunction );

Lang::VectorFunction::~VectorFunction( )
{ }

Kernel::VariableHandle
Lang::VectorFunction::getField( const char * fieldID, const RefCountPtr< const Lang::Value > & selfRef ) const
{
  if( strcmp( fieldID, "size" ) == 0 )
    {
      return Helpers::newValHandle( new Lang::Integer( mem_->size( ) ) );
    }
  throw Exceptions::NonExistentMember( getTypeName( ), fieldID );
}

const char * Lang::VectorFunction::title_ = "<vector>";

void
Lang::VectorFunction::call( Kernel::EvalState * evalState, Kernel::Arguments & args, const Ast::SourceLocation & callLoc ) const
{
  const size_t ARITY = 1;
  if( args.size( ) != ARITY )
    {
      throw Exceptions::CoreArityMismatch( title_, ARITY, args.size( ) );
    }

  typedef const Lang::Integer ArgType;
  RefCountPtr< ArgType > arg = Helpers::down_cast_CoreArgument< ArgType >( title_, args, 0, callLoc );

  if( arg->val_ < 0 )
    {
      throw Exceptions::CoreOutOfRange( title_, args, 0, "Index is negative." );
    }

  if( arg->val_ >= static_cast< int >( mem_->size( ) ) )
    {
      throw Exceptions::CoreOutOfRange( title_, args, 0, "Index exceeds vector size." );
    }

  Kernel::ContRef cont = evalState->cont_;
  cont->takeValue( (*mem_)[ arg->val_ ],
		   evalState );
}

bool
Lang::VectorFunction::isTransforming( ) const
{
  return false;
}

void
Lang::VectorFunction::gcMark( Kernel::GCMarkedSet & marked )
{
  for( std::vector< Kernel::ValueRef >::const_iterator i = mem_->begin( ); i != mem_->end( ); ++i )
    {
      const_cast< Lang::Value * >( i->getPtr( ) )->gcMark( marked );
    }
}

RefCountPtr< const std::vector< double > > 
Lang::VectorFunction::getNumeric( const Ast::SourceLocation & callLoc ) const
{
  if( memNumeric_ == NullPtr< const std::vector< double > >( ) )
    {
      RefCountPtr< std::vector< double > > res( new std::vector< double > ); // Note that this is not const, so far...
      res->reserve( mem_->size( ) );
      typedef typeof *mem_ SrcType;
      for( SrcType::const_iterator src = mem_->begin( ); src != mem_->end( ); ++src )
	{
	  res->push_back( Helpers::down_cast< const Lang::Float >( *src, callLoc )->val_ );
	}

      memNumeric_ = res;
    }
  return memNumeric_;
}


Lang::BinaryOperatorFunction::BinaryOperatorFunction( Ast::BinaryInfixExpr * opExpr, const char * title )
  : Lang::Function( new Kernel::EvaluatedFormals( title, true ) ), opExpr_( opExpr ), title_( title )
{ 
  formals_->appendEvaluatedCoreFormal( "first", Kernel::THE_SLOT_VARIABLE );
  formals_->appendEvaluatedCoreFormal( "second", Kernel::THE_SLOT_VARIABLE );
}

Lang::BinaryOperatorFunction::~BinaryOperatorFunction( )
{
  delete opExpr_;
}

void
Lang::BinaryOperatorFunction::call( Kernel::EvalState * evalState, Kernel::Arguments & args, const Ast::SourceLocation & callLoc ) const
{
  const size_t ARITY = 2;
  CHECK_ARITY( args, ARITY, title_ );
  RefCountPtr< const Lang::Value > arg1 = args.getValue( 0 );
  RefCountPtr< const Lang::Value > arg2 = args.getValue( 1 );

  try
    {
      Kernel::ContRef cont = evalState->cont_;
      cont->takeValue( arg1->binaryDispatch1( arg1, arg2, evalState->dyn_, opExpr_ ),
		       evalState );
    }
  catch( Exceptions::BinaryInfixNotApplicable & ball )
    {
      ball.setOperatorSymbol( title_ );
      throw;
    }
}

bool
Lang::BinaryOperatorFunction::isTransforming( ) const
{
  return false;
}


Lang::UnaryOperatorFunction::UnaryOperatorFunction( Ast::UnaryExpr * opExpr, const char * title )
  : Lang::Function( new Kernel::EvaluatedFormals( title, true ) ), opExpr_( opExpr ), title_( title )
{
  formals_->appendEvaluatedCoreFormal( "only", Kernel::THE_SLOT_VARIABLE );
}

Lang::UnaryOperatorFunction::~UnaryOperatorFunction( )
{
  delete opExpr_;
}

void
Lang::UnaryOperatorFunction::call( Kernel::EvalState * evalState, Kernel::Arguments & args, const Ast::SourceLocation & callLoc ) const
{
  const size_t ARITY = 1;
  CHECK_ARITY( args, ARITY, title_ );
  
  RefCountPtr< const Lang::Value > arg = args.getValue( 0 );

  try
    {
      Kernel::ContRef cont = evalState->cont_;
      cont->takeValue( arg->unaryDispatch( arg, evalState->dyn_, opExpr_ ),
		       evalState );
    }
  catch( Exceptions::UnaryPrefixNotApplicable & ball )
    {
      ball.setOperatorSymbol( title_ );
      throw;
    }
  catch( Exceptions::UnaryPostfixNotApplicable & ball )
    {
      ball.setOperatorSymbol( title_ );
      throw;
    }
}

bool
Lang::UnaryOperatorFunction::isTransforming( ) const
{
  return false;
}

