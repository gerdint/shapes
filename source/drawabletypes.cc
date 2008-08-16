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
#include "pagecontentstates.h"
#include "continuations.h"

#include <ctype.h>
#include <list>
#include <algorithm>

using namespace Shapes;


Lang::Drawable2D::Drawable2D( )
{ }

Lang::Drawable2D::~Drawable2D( )
{ }

RefCountPtr< const Lang::Transformed2D >
Lang::Drawable2D::typed_transformed( const Lang::Transform2D & tf, const RefCountPtr< const Lang::Drawable2D > & self ) const
{
	return RefCountPtr< const Lang::Transformed2D >( new Lang::Transformed2D( self, tf ) );
}

RefCountPtr< const Lang::Geometric2D >
Lang::Drawable2D::transformed( const Lang::Transform2D & tf, const RefCountPtr< const Lang::Geometric2D > & self ) const
{
	return typed_transformed( tf, self.down_cast< const Lang::Drawable2D >( ) );
}

RefCountPtr< const Lang::Geometric3D >
Lang::Drawable2D::to3D( const RefCountPtr< const Lang::Geometric2D > & self ) const
{
	typedef const Lang::Drawable2D SelfType;
	RefCountPtr< SelfType > typedSelf = self.down_cast< SelfType >( );
	if( typedSelf == NullPtr< SelfType >( ) )
		{
			throw Exceptions::InternalError( "The self-value passed to Drawable2D::to3D was of bad type." );
		}
	return RefCountPtr< const Lang::Geometric3D >( new Lang::Drawable2Din3D( typedSelf ) );
}

void
Lang::Drawable2D::findTags( std::vector< Kernel::ValueRef > * dst, const Kernel::PassedDyn & dyn, Lang::Symbol::KeyType key, const Lang::Transform2D & tf ) const
{
	// Not overloading this methods means that there are no tagged objects within this.
}

bool
Lang::Drawable2D::findOneTag( Kernel::EvalState * evalState, Lang::Symbol::KeyType key, const Lang::Transform2D & tf ) const
{
	// Not overloading this methods means that there are no tagged objects within this.
	return false;
}

void
Lang::Drawable2D::gcMark( Kernel::GCMarkedSet & marked )
{ }

RefCountPtr< const Lang::Class > Lang::Drawable2D::TypeID = NullPtr< const Lang::Class >( );	/* The value is set in main */
TYPEINFOIMPL( Drawable2D );
DISPATCHIMPL( Drawable2D );


Lang::Group2D::Group2D( )
{ }

Lang::Group2D::~Group2D( )
{ }

RefCountPtr< const Lang::Class > Lang::Group2D::TypeID( new Lang::SystemFinalClass( strrefdup( "Group2D" ) ) );
TYPEINFOIMPL( Group2D );


RefCountPtr< const Lang::Group2D >
Helpers::newGroup2D( const RefCountPtr< const Kernel::GraphicsState > & metaState, const RefCountPtr< const Lang::Drawable2D > & obj2, const RefCountPtr< const Lang::Drawable2D > & obj1 )
{
	RefCountPtr< const Lang::Group2D > res = Lang::THE_NULL2D;
	res = RefCountPtr< const Lang::GroupPair2D >( new Lang::GroupPair2D( obj1,
																																						 res,
																																						 metaState ) );
	res = RefCountPtr< const Lang::GroupPair2D >( new Lang::GroupPair2D( obj2,
																																						 res,
																																						 metaState ) );
	return res;
}

RefCountPtr< const Lang::Group2D >
Helpers::newGroup2D( const RefCountPtr< const Kernel::GraphicsState > & metaState, const RefCountPtr< const Lang::Drawable2D > & obj3, const RefCountPtr< const Lang::Drawable2D > & obj2, const RefCountPtr< const Lang::Drawable2D > & obj1 )
{
	RefCountPtr< const Lang::Group2D > res = Lang::THE_NULL2D;
	res = RefCountPtr< const Lang::GroupPair2D >( new Lang::GroupPair2D( obj1,
																																						 res,
																																						 metaState ) );
	res = RefCountPtr< const Lang::GroupPair2D >( new Lang::GroupPair2D( obj2,
																																						 res,
																																						 metaState ) );
	res = RefCountPtr< const Lang::GroupPair2D >( new Lang::GroupPair2D( obj3,
																																						 res,
																																						 metaState ) );
	return res;
}


Lang::GroupPair2D::GroupPair2D( RefCountPtr< const Lang::Drawable2D > car, RefCountPtr< const Lang::Group2D > cdr, const RefCountPtr< const Kernel::GraphicsState > & metaState )
	: metaState_( metaState ), car_( car ), cdr_( cdr )
{ }

Lang::GroupPair2D::~GroupPair2D( )
{ }

bool
Lang::GroupPair2D::isNull( ) const
{
	return false;
}

void
Lang::GroupPair2D::shipout( std::ostream & os, Kernel::PageContentStates * pdfState, const Lang::Transform2D & tf ) const
{
	cdr_->shipout( os, pdfState, tf );
	if( pdfState->graphics_.synchBlend( os, metaState_.getPtr( ), pdfState->resources_.getPtr( ) ) )
		{
			os << std::endl ;
		}
	car_->shipout( os, pdfState, tf );
}

RefCountPtr< const Lang::ElementaryPath2D >
Lang::GroupPair2D::bbox( ) const
{
	RefCountPtr< const Lang::ElementaryPath2D > carbbox = car_->bbox( );
	RefCountPtr< const Lang::ElementaryPath2D > cdrbbox = cdr_->bbox( );

	if( cdrbbox->empty( ) )
		{
			return carbbox;
		}

	Concrete::Length xmin = Concrete::HUGE_LENGTH;
	Concrete::Length xmax = -Concrete::HUGE_LENGTH;
	Concrete::Length ymin = Concrete::HUGE_LENGTH;
	Concrete::Length ymax = -Concrete::HUGE_LENGTH;

	typedef typeof *carbbox PathType;
	for( PathType::const_iterator i = carbbox->begin( ); i != carbbox->end( ); ++i )
		{
			Concrete::Length x = (*i)->mid_->x_;
			xmin = std::min( xmin, x );
			xmax = std::max( xmax, x );

			Concrete::Length y = (*i)->mid_->y_;
			ymin = std::min( ymin, y );
			ymax = std::max( ymax, y );
		}

	for( PathType::const_iterator i = cdrbbox->begin( ); i != cdrbbox->end( ); ++i )
		{
			Concrete::Length x = (*i)->mid_->x_;
			xmin = std::min( xmin, x );
			xmax = std::max( xmax, x );

			Concrete::Length y = (*i)->mid_->y_;
			ymin = std::min( ymin, y );
			ymax = std::max( ymax, y );
		}

	Lang::ElementaryPath2D * res = new Lang::ElementaryPath2D;

	if( xmin < Concrete::HUGE_LENGTH )
		{
			res->push_back( new Concrete::PathPoint2D( xmin, ymin ) );
			res->push_back( new Concrete::PathPoint2D( xmin, ymax ) );
			res->push_back( new Concrete::PathPoint2D( xmax, ymax ) );
			res->push_back( new Concrete::PathPoint2D( xmax, ymin ) );
			res->close( );
		}

	return RefCountPtr< const Lang::ElementaryPath2D >( res );

}

void
Lang::GroupPair2D::findTags( std::vector< Kernel::ValueRef > * dst, const Kernel::PassedDyn & dyn, Lang::Symbol::KeyType key, const Lang::Transform2D & tf ) const
{
	/* Note the order!	Objects are added on the car side of a group being built, so the car side is the "latter".
	 */
	cdr_->findTags( dst, dyn, key, tf );
	car_->findTags( dst, dyn, key, tf );
}

bool
Lang::GroupPair2D::findOneTag( Kernel::EvalState * evalState, Lang::Symbol::KeyType key, const Lang::Transform2D & tf ) const
{
	return
		cdr_->findOneTag( evalState, key, tf ) ||
		car_->findOneTag( evalState, key, tf );
}

void
Lang::GroupPair2D::gcMark( Kernel::GCMarkedSet & marked )
{
	const_cast< Lang::Drawable2D * >( car_.getPtr( ) )->gcMark( marked );
	const_cast< Lang::Group2D * >( cdr_.getPtr( ) )->gcMark( marked );
}


Lang::GroupNull2D::GroupNull2D( )
{ }

Lang::GroupNull2D::~GroupNull2D( )
{ }

bool
Lang::GroupNull2D::isNull( ) const
{
	return true;
}

void
Lang::GroupNull2D::shipout( std::ostream & os, Kernel::PageContentStates * pdfState, const Lang::Transform2D & tf ) const
{ }

RefCountPtr< const Lang::ElementaryPath2D >
Lang::GroupNull2D::bbox( ) const
{
	return Lang::THE_EMPTYPATH2D;
}


Lang::XObject::XObject( const RefCountPtr< SimplePDF::PDF_Object > & resource, RefCountPtr< const Lang::ElementaryPath2D > mybbox )
	: metaState_( Kernel::THE_NO_STATE ), resource_( resource ), mybbox_( mybbox )
{ }

Lang::XObject::XObject( const RefCountPtr< SimplePDF::PDF_Object > & resource, RefCountPtr< const Lang::ElementaryPath2D > mybbox, const RefCountPtr< const Kernel::GraphicsState > & metaState )
	: metaState_( metaState ), resource_( resource ), mybbox_( mybbox )
{ }

Lang::XObject::~XObject( )
{ }

void
Lang::XObject::shipout( std::ostream & os, Kernel::PageContentStates * pdfState, const Lang::Transform2D & tf ) const
{
	Kernel::Auto_qQ auto_qQ( & pdfState->graphics_, os, false );
	if( ! tf.isIdentity( ) )
		{
			auto_qQ.activate( );
			tf.shipout( os );
			os << " cm" << std::endl ;
		}
	if( metaState_ != Kernel::THE_NO_STATE )
		{
			pdfState->graphics_.synchForNonStroke( os, metaState_.getPtr( ), pdfState->resources_.getPtr( ) );
		}
	os << pdfState->resources_->nameofXObject( resource_ ) << " Do" << std::endl ;
}

RefCountPtr< SimplePDF::PDF_Object >
Lang::XObject::getResource( ) const
{
	return resource_;
}

RefCountPtr< const Lang::ElementaryPath2D >
Lang::XObject::bbox( ) const
{
	return mybbox_;
}

RefCountPtr< const Lang::XObject >
Lang::XObject::cloneWithState( const RefCountPtr< const Kernel::GraphicsState > & metaState ) const
{
	return RefCountPtr< const Lang::XObject >( new Lang::XObject( resource_, mybbox_, metaState ) );
}

void
Lang::XObject::setDebugStr( const std::string & debugStr )
{
	debugStr_ = debugStr;
}

const std::string &
Lang::XObject::getDebugStr( ) const
{
	return debugStr_;
}

void
Lang::XObject::show( std::ostream & os ) const
{
	os << "XObject (" << getDebugStr( ) << ")" ;
}

void
Lang::XObject::gcMark( Kernel::GCMarkedSet & marked )
{
	// At the time of writing, there is nothing to propagate to.
}


Lang::TransparencyGroup::TransparencyGroup( const RefCountPtr< SimplePDF::PDF_Indirect_out > & indirection, RefCountPtr< const Lang::ElementaryPath2D > mybbox, const RefCountPtr< const Lang::ColorSpace > & colorSpace )
	: Lang::XObject( indirection, mybbox ), colorSpace_( colorSpace ), indirection_( indirection )
{ }

Lang::TransparencyGroup::~TransparencyGroup( )
{ }

RefCountPtr< const Lang::ColorSpace >
Lang::TransparencyGroup::colorSpace( ) const
{
	return colorSpace_;
}

RefCountPtr< SimplePDF::PDF_Indirect_out >
Lang::TransparencyGroup::getPDF_Object( ) const
{
	return indirection_.unconst_cast< SimplePDF::PDF_Indirect_out >( );
}

RefCountPtr< const Lang::TransparencyGroup >
Helpers::newTransparencyGroup( const RefCountPtr< const Lang::Group2D > & content, bool isolated, bool knockout, const RefCountPtr< const Lang::ColorSpace > & blendSpace )
{
	RefCountPtr< const Lang::ElementaryPath2D > theBBox = content->bbox( );
	Concrete::Coords2D llcorner( 0, 0 );
	Concrete::Coords2D urcorner( 0, 0 );
	if( ! theBBox->boundingRectangle( & llcorner, & urcorner ) )
		{
			throw Exceptions::InternalError( "newTransparencyGroup: The object has no bounding box!" );
		}

	using namespace Shapes;

	RefCountPtr< SimplePDF::PDF_Stream_out > form;

	(*form)[ "Subtype" ] = SimplePDF::newName( "Form" );
	(*form)[ "FormType" ] = SimplePDF::newInt( 1 );
	(*form)[ "BBox" ] = RefCountPtr< SimplePDF::PDF_Vector >( new SimplePDF::PDF_Vector( llcorner.x_.offtype< 1, 0 >( ), llcorner.y_.offtype< 1, 0 >( ),
																																											 urcorner.x_.offtype< 1, 0 >( ), urcorner.y_.offtype< 1, 0 >( ) ) );
	RefCountPtr< SimplePDF::PDF_Resources > resources;
	(*form)[ "Resources" ] = SimplePDF::indirect( resources, & Kernel::theIndirectObjectCount );

	if( ! Kernel::allowTransparency )
		{
			/* OK, fine. */
		}
	else if( ! Kernel::the_PDF_version.greaterOrEqual( SimplePDF::PDF_Version::PDF_1_4 ) )
		{
			Kernel::the_PDF_version.message( SimplePDF::PDF_Version::PDF_1_4, "A transparency group was replaced by a plain XObject." );
		}
	else
		{
			RefCountPtr< SimplePDF::PDF_Dictionary > groupDic;
			(*form)[ "Group" ] = groupDic;
			(*groupDic)[ "S" ] = SimplePDF::newName( "Transparency" );
			if( ! blendSpace->isInherent( ) )
				{
					(*groupDic)[ "CS" ] = blendSpace->name( );
				}
			if( isolated )
				{
					(*groupDic)[ "I" ] = SimplePDF::newBoolean( true );
				}
			if( knockout )
				{
					(*groupDic)[ "K" ] = SimplePDF::newBoolean( true );
				}
		}

	/* There's a possibility of adding a transformation matrix entry in the dictionary here, but it is not used, not even
	 * for transformed drawables.
	 */
	//	(*markForm)[ "Matrix" ] = RefCountPtr<PDF_Object>( new PDF_Vector( 1, 0, 0, 1, -30, -30 ) );

	Kernel::PageContentStates pdfState( resources );
	content->shipout( form->data, & pdfState, Lang::Transform2D( 1, 0, 0, 1, Concrete::ZERO_LENGTH, Concrete::ZERO_LENGTH ) );

	Lang::TransparencyGroup * res = new Lang::TransparencyGroup( SimplePDF::indirect( form, & Kernel::theIndirectObjectCount ),
																															 content->bbox( ),
																															 blendSpace );
	res->setDebugStr( "transparency group" );
	return RefCountPtr< const Lang::TransparencyGroup >( res );
}




Lang::PaintedPath2D::PaintedPath2D( const RefCountPtr< const Kernel::GraphicsState > & metaState,
																		const char * paintCmd )
	: metaState_( metaState ), paintCmd_( paintCmd )
{ }

Lang::PaintedPath2D::PaintedPath2D( const RefCountPtr< const Kernel::GraphicsState > & metaState,
																		RefCountPtr< const Lang::ElementaryPath2D > path,
																		const char * paintCmd )
	: metaState_( metaState ), paintCmd_( paintCmd )
{
	addSubPath( path );
}

Lang::PaintedPath2D::PaintedPath2D( const RefCountPtr< const Kernel::GraphicsState > & metaState,
																		RefCountPtr< const Lang::MultiPath2D > paths,
																		const char * paintCmd )
	: metaState_( metaState ), paintCmd_( paintCmd )
{
	for( Lang::MultiPath2D::const_iterator i = paths->begin( ); i != paths->end( ); ++i )
		{
			{
				typedef const Lang::ElementaryPath2D ArgType;
				RefCountPtr< ArgType > subpath = (*i).down_cast< ArgType >( );
				if( subpath != NullPtr< ArgType >( ) )
					{
						addSubPath( subpath );
						continue;
					}
			}

			{
				typedef const Lang::CompositePath2D ArgType;
				ArgType * subpath = dynamic_cast< ArgType * >( (*i).getPtr( ) );
				if( subpath != 0 )
					{
						addSubPath( subpath->getElementaryPath( ) );
						continue;
					}
			}
			throw Exceptions::InternalError( "Painting 2D path: Encountered a subpath of unexpected type" );
		}
}

Lang::PaintedPath2D::~PaintedPath2D( )
{ }

RefCountPtr< const Lang::Class > Lang::PaintedPath2D::TypeID( new Lang::SystemFinalClass( strrefdup( "PaintedPath2D" ) ) );
TYPEINFOIMPL( PaintedPath2D );

void
Lang::PaintedPath2D::addSubPath( RefCountPtr< const Lang::ElementaryPath2D > subpath )
{
	path_.push_back( subpath );
}

void
Lang::PaintedPath2D::shipout( std::ostream & os, Kernel::PageContentStates * pdfState, const Lang::Transform2D & tf ) const
{
	/* Transforming the path by tf is not good enough, since that does not transform softmasks etc.
	 */

	char pdfCmd[4];
	strcpy( pdfCmd, paintCmd_ );

	Kernel::Auto_qQ auto_qQ( & pdfState->graphics_, os, false );
	if( ! tf.isIdentity( ) )
		{
			auto_qQ.activate( );
			tf.shipout( os );
			os << " cm" << std::endl ;
		}
	if( strcmp( paintCmd_, "S" ) == 0 )
		{
			pdfState->graphics_.synchForStroke( os, metaState_.getPtr( ), pdfState->resources_.getPtr( ) );
		}
	else if( strcmp( paintCmd_, "f" ) == 0 ||
					 strcmp( paintCmd_, "f*" ) == 0 )
		{
			pdfState->graphics_.synchForNonStroke( os, metaState_.getPtr( ), pdfState->resources_.getPtr( ) );
		}
	else if( strcmp( paintCmd_, "B" ) == 0 ||
					 strcmp( paintCmd_, "B*" ) == 0 ||
					 strcmp( paintCmd_, "b" ) == 0 ||
					 strcmp( paintCmd_, "b*" ) == 0 )
		{
			pdfState->graphics_.synchForNonStroke( os, metaState_.getPtr( ), pdfState->resources_.getPtr( ) );
			pdfState->graphics_.synchForStroke( os, metaState_.getPtr( ), pdfState->resources_.getPtr( ) );
		}
	else if( strcmp( paintCmd_, "E" ) == 0 ||
					 strcmp( paintCmd_, "E*" ) == 0 ||
					 strcmp( paintCmd_, "e" ) == 0 ||
					 strcmp( paintCmd_, "e*" ) == 0 )
		{
			pdfCmd[0] += 'B' - 'E';
			// Note that this is my own interpretation; usually, the stroke is made with the stroking color,
			// but I use this to make filled regions just a little bit bigger, and then it is the nonstroking
			// color that shall be applied to the stroke as well.
			pdfState->graphics_.synchForNonStroke( os, metaState_.getPtr( ), pdfState->resources_.getPtr( ) );
			pdfState->graphics_.synchStrokingColorWithNonStrokingColor( os, pdfState->resources_.getPtr( ), Concrete::ZERO_LENGTH );
		}
	else
		{
			throw Exceptions::InternalError( "Unexpected paintCmd in PaintedPath2D::shipout" );
		}

	typedef typeof path_ ListType;
	for( ListType::const_iterator i = path_.begin( ); i != path_.end( ); ++i )
		{
			(*i)->writePath( os );
		}
	os << " " << pdfCmd << std::endl ;
}

RefCountPtr< const Lang::ElementaryPath2D >
Lang::PaintedPath2D::bbox( ) const
{
	if( path_.empty( ) )
		{
			return RefCountPtr< const Lang::ElementaryPath2D >( new Lang::ElementaryPath2D );
		}

	Concrete::Length xmin = Concrete::HUGE_LENGTH;
	Concrete::Length xmax = -Concrete::HUGE_LENGTH;
	Concrete::Length ymin = Concrete::HUGE_LENGTH;
	Concrete::Length ymax = -Concrete::HUGE_LENGTH;

	Concrete::Coords2D llcorner( 0, 0 ); /* Temporary variables to be used in the loop below. */
	Concrete::Coords2D urcorner( 0, 0 );

	typedef typeof path_ PathType;
	for( PathType::const_iterator j = path_.begin( ); j != path_.end( ); ++j )
		{
			if( (*j)->boundingRectangle( & llcorner, & urcorner ) )
				{
					/* non-empty path */
					xmin = std::min( xmin, llcorner.x_ );
					xmax = std::max( xmax, urcorner.x_ );
					ymin = std::min( ymin, llcorner.y_ );
					ymax = std::max( ymax, urcorner.y_ );
				}
		}

	Lang::ElementaryPath2D * res = new Lang::ElementaryPath2D;

	if( xmin < Concrete::HUGE_LENGTH )
		{
			if( toupper( *paintCmd_ ) == 'S' ||
					toupper( *paintCmd_ ) == 'B' )
				{
					xmin -= 0.5 * metaState_->width_;
					ymin -= 0.5 * metaState_->width_;
					xmax += 0.5 * metaState_->width_;
					ymax += 0.5 * metaState_->width_;
				}
			res->push_back( new Concrete::PathPoint2D( xmin, ymin ) );
			res->push_back( new Concrete::PathPoint2D( xmin, ymax ) );
			res->push_back( new Concrete::PathPoint2D( xmax, ymax ) );
			res->push_back( new Concrete::PathPoint2D( xmax, ymin ) );
			res->close( );
		}

	return RefCountPtr< const Lang::ElementaryPath2D >( res );
}

void
Lang::PaintedPath2D::gcMark( Kernel::GCMarkedSet & marked )
{ }


Lang::Transformed2D::Transformed2D( RefCountPtr< const Lang::Drawable2D > element, const Lang::Transform2D & mytf )
	: mytf_( mytf ), element_( element )
{ }

Lang::Transformed2D::~Transformed2D( )
{ }

void
Lang::Transformed2D::shipout( std::ostream & os, Kernel::PageContentStates * pdfState, const Lang::Transform2D & tf ) const
{
	element_->shipout( os, pdfState, Lang::Transform2D( tf, mytf_ ) );
}

RefCountPtr< const Lang::ElementaryPath2D >
Lang::Transformed2D::bbox( ) const
{
	return element_->bbox( )->elementaryTransformed( mytf_ );
}

void
Lang::Transformed2D::findTags( std::vector< Kernel::ValueRef > * dst, const Kernel::PassedDyn & dyn, Lang::Symbol::KeyType key, const Lang::Transform2D & tf ) const
{
	element_->findTags( dst, dyn, key, Lang::Transform2D( tf, mytf_ ) );
}

bool
Lang::Transformed2D::findOneTag( Kernel::EvalState * evalState, Lang::Symbol::KeyType key, const Lang::Transform2D & tf ) const
{
	return
		element_->findOneTag( evalState, key, Lang::Transform2D( tf, mytf_ ) );
}

void
Lang::Transformed2D::gcMark( Kernel::GCMarkedSet & marked )
{
	const_cast< Lang::Drawable2D * >( element_.getPtr( ) )->gcMark( marked );
}


Lang::BBoxed2D::BBoxed2D( RefCountPtr< const Lang::Drawable2D > element, RefCountPtr< const Lang::ElementaryPath2D > mybbox )
	: Lang::PaintedPolygon2D( Kernel::THE_NO_STATE, mybbox ), mybbox_( mybbox ), element_( element )
{ }

Lang::BBoxed2D::~BBoxed2D( )
{ }

void
Lang::BBoxed2D::shipout( std::ostream & os, Kernel::PageContentStates * pdfState, const Lang::Transform2D & tf ) const
{
	/* At the moment, we don't clip according to the bbox, we only lie about our size.
	 */
	element_->shipout( os, pdfState, tf );
}

RefCountPtr< const Lang::ElementaryPath2D >
Lang::BBoxed2D::bbox( ) const
{
	return mybbox_;
}

void
Lang::BBoxed2D::findTags( std::vector< Kernel::ValueRef > * dst, const Kernel::PassedDyn & dyn, Lang::Symbol::KeyType key, const Lang::Transform2D & tf ) const
{
	element_->findTags( dst, dyn, key, tf );
}

bool
Lang::BBoxed2D::findOneTag( Kernel::EvalState * evalState, Lang::Symbol::KeyType key, const Lang::Transform2D & tf ) const
{
	return
		element_->findOneTag( evalState, key, tf );
}

void
Lang::BBoxed2D::gcMark( Kernel::GCMarkedSet & marked )
{
	Lang::PaintedPolygon2D::gcMark( marked );
	const_cast< Lang::Drawable2D * >( element_.getPtr( ) )->gcMark( marked );
}


Lang::Clipped2D::Clipped2D( const RefCountPtr< const Lang::Drawable2D > & element, const char * clipCommand )
	: element_( element ), clipCommand_( clipCommand )
{ }

Lang::Clipped2D::~Clipped2D( )
{ }

void
Lang::Clipped2D::addSubPath( const RefCountPtr< const Lang::ElementaryPath2D > & subpath )
{
	clipList_.push_back( subpath );
}

void
Lang::Clipped2D::shipout( std::ostream & os, Kernel::PageContentStates * pdfState, const Lang::Transform2D & tf ) const
{
	Kernel::Auto_qQ auto_qQ( & pdfState->graphics_, os );
	if( ! tf.isIdentity( ) )
		{
			tf.shipout( os );
			os << " cm" << std::endl ;
		}
	typedef typeof clipList_ ListType;
	for( ListType::const_iterator i = clipList_.begin( ); i != clipList_.end( ); ++i )
		{
			(*i)->writePath( os );
		}
	os << " " << clipCommand_ << " n" << std::endl ;
	element_->shipout( os, pdfState, THE_2D_IDENTITY );
}

RefCountPtr< const Lang::ElementaryPath2D >
Lang::Clipped2D::bbox( ) const
{
	if( clipList_.empty( ) )
		{
			return RefCountPtr< const Lang::ElementaryPath2D >( new Lang::ElementaryPath2D );
		}

	RefCountPtr< const Lang::ElementaryPath2D > elem_bbox = element_->bbox( );
	Concrete::Coords2D llElemBbox( 0, 0 );
	Concrete::Coords2D urElemBbox( 0, 0 );
	elem_bbox->boundingRectangle( & llElemBbox, & urElemBbox );

	Concrete::Length xmin = Concrete::HUGE_LENGTH;
	Concrete::Length xmax = -Concrete::HUGE_LENGTH;
	Concrete::Length ymin = Concrete::HUGE_LENGTH;
	Concrete::Length ymax = -Concrete::HUGE_LENGTH;

	Concrete::Coords2D llcorner( 0, 0 ); /* Temporary variables to be used in the loop below. */
	Concrete::Coords2D urcorner( 0, 0 );

	typedef typeof clipList_ PathType;
	for( PathType::const_iterator j = clipList_.begin( ); j != clipList_.end( ); ++j )
		{
			if( (*j)->boundingRectangle( & llcorner, & urcorner ) )
				{
					/* non-empty path */
					xmin = std::min( xmin, llcorner.x_ );
					xmax = std::max( xmax, urcorner.x_ );
					ymin = std::min( ymin, llcorner.y_ );
					ymax = std::max( ymax, urcorner.y_ );
				}
		}

	xmin = std::max( xmin, llElemBbox.x_ );
	ymin = std::max( ymin, llElemBbox.y_ );
	xmax = std::min( xmax, urElemBbox.x_ );
	ymax = std::min( ymax, urElemBbox.y_ );

	Lang::ElementaryPath2D * res = new Lang::ElementaryPath2D;

	if( xmin < xmax && ymin < ymax )
		{
			res->push_back( new Concrete::PathPoint2D( xmin, ymin ) );
			res->push_back( new Concrete::PathPoint2D( xmin, ymax ) );
			res->push_back( new Concrete::PathPoint2D( xmax, ymax ) );
			res->push_back( new Concrete::PathPoint2D( xmax, ymin ) );
			res->close( );
		}

	return RefCountPtr< const Lang::ElementaryPath2D >( res );
}

void
Lang::Clipped2D::findTags( std::vector< Kernel::ValueRef > * dst, const Kernel::PassedDyn & dyn, Lang::Symbol::KeyType key, const Lang::Transform2D & tf ) const
{
	element_->findTags( dst, dyn, key, tf );
}

bool
Lang::Clipped2D::findOneTag( Kernel::EvalState * evalState, Lang::Symbol::KeyType key, const Lang::Transform2D & tf ) const
{
	return
		element_->findOneTag( evalState, key, tf );
}

void
Lang::Clipped2D::gcMark( Kernel::GCMarkedSet & marked )
{
	const_cast< Lang::Drawable2D * >( element_.getPtr( ) )->gcMark( marked );
}

RefCountPtr< const Lang::Drawable2D >
Lang::Clipped2D::debugPolys( ) const
{
	Kernel::GraphicsState * frameStatePtr = new Kernel::GraphicsState( true );
	frameStatePtr->width_ = Concrete::Length( 0.2 );
	RefCountPtr< const Kernel::GraphicsState > frameState( frameStatePtr );

	Lang::PaintedPath2D * res = new Lang::PaintedPath2D( frameState, "S" );
	typedef typeof clipList_ ListType;
	for( ListType::const_iterator i = clipList_.begin( ); i != clipList_.end( ); ++i )
		{
			res->addSubPath( *i );
		}

	return RefCountPtr< const Lang::Drawable2D >( res );
}

bool
Lang::Clipped2D::isSingleConvexPoly( Concrete::Length tol ) const
{
	if( clipList_.size( ) != 1 )
		{
			return false;
		}
	return clipList_.front( )->isConvexPoly( tol );
}

bool
Lang::Clipped2D::convexPolyContains( const Concrete::Coords2D & p, Concrete::Length tol ) const
{
	return clipList_.front( )->convexPolyContains( p, tol );
}

Lang::SoftMasked2D::SoftMasked2D( const RefCountPtr< const Lang::Drawable2D > & element, const RefCountPtr< const Lang::SoftMask > & mask )
	: element_( element ), mask_( mask )
{ }

Lang::SoftMasked2D::~SoftMasked2D( )
{ }

void
Lang::SoftMasked2D::shipout( std::ostream & os, Kernel::PageContentStates * pdfState, const Lang::Transform2D & tf ) const
{
	const SimplePDF::PDF_Version::Version SOFTMASK_VERSION = SimplePDF::PDF_Version::PDF_1_4;
	if( Kernel::the_PDF_version.greaterOrEqual( SOFTMASK_VERSION ) )
		{
			Kernel::Auto_qQ auto_qQ( & pdfState->graphics_, os );
			if( ! tf.isIdentity( ) )
				{
					tf.shipout( os );
					os << " cm" << std::endl ;
				}
			os << pdfState->resources_->nameofGraphicsState( mask_->graphicsStateResource_ ) << " gs " << std::endl ;
			element_->shipout( os, pdfState, THE_2D_IDENTITY );
		}
	else
		{
			Kernel::the_PDF_version.message( SOFTMASK_VERSION, "A soft mask was ignored." );
			element_->shipout( os, pdfState, tf );
		}
}

RefCountPtr< const Lang::ElementaryPath2D >
Lang::SoftMasked2D::bbox( ) const
{
	return element_->bbox( );
}

void
Lang::SoftMasked2D::findTags( std::vector< Kernel::ValueRef > * dst, const Kernel::PassedDyn & dyn, Lang::Symbol::KeyType key, const Lang::Transform2D & tf ) const
{
	element_->findTags( dst, dyn, key, tf );
}

bool
Lang::SoftMasked2D::findOneTag( Kernel::EvalState * evalState, Lang::Symbol::KeyType key, const Lang::Transform2D & tf ) const
{
	return
		element_->findOneTag( evalState, key, tf );
}

void
Lang::SoftMasked2D::gcMark( Kernel::GCMarkedSet & marked )
{
	const_cast< Lang::Drawable2D * >( element_.getPtr( ) )->gcMark( marked );
	const_cast< Lang::SoftMask * >( mask_.getPtr( ) )->gcMark( marked );
}


Lang::Drawable3D::Drawable3D( )
{ }

Lang::Drawable3D::~Drawable3D( )
{ }

RefCountPtr< const Lang::Transformed3D >
Lang::Drawable3D::typed_transformed( const Lang::Transform3D & tf, const RefCountPtr< const Lang::Drawable3D > & self ) const
{
	return RefCountPtr< const Lang::Transformed3D >( new Lang::Transformed3D( self, tf ) );
}

RefCountPtr< const Lang::Geometric3D >
Lang::Drawable3D::transformed( const Lang::Transform3D & tf, const RefCountPtr< const Lang::Geometric3D > & self ) const
{
	return typed_transformed( tf, self.down_cast< const Lang::Drawable3D >( ) );
}

RefCountPtr< const Lang::Geometric2D >
Lang::Drawable3D::to2D( const Kernel::PassedDyn & dyn, const RefCountPtr< const Lang::Geometric3D > & self ) const
{
	typedef const Lang::Drawable3D SelfType;
	RefCountPtr< SelfType > typedSelf = self.down_cast< SelfType >( );
	if( typedSelf == NullPtr< SelfType >( ) )
		{
			throw Exceptions::InternalError( "The self-value passed to Drawable3D::to2D was of bad type." );
		}
	return typed_to2D( dyn, Lang::THE_3D_IDENTITY, typedSelf );
}

void
Lang::Drawable3D::findTags( std::vector< Kernel::ValueRef > * dst, const Kernel::PassedDyn & dyn, Lang::Symbol::KeyType key, const Lang::Transform3D & tf ) const
{
	// Not overloading this methods means that there are no tagged objects within this.
}

bool
Lang::Drawable3D::findOneTag( Kernel::EvalState * evalState, Lang::Symbol::KeyType key, const Lang::Transform3D & tf ) const
{
	// Not overloading this methods means that there are no tagged objects within this.
	return false;
}

void
Lang::Drawable3D::gcMark( Kernel::GCMarkedSet & marked )
{ }

RefCountPtr< const Lang::Class > Lang::Drawable3D::TypeID = NullPtr< const Lang::Class >( );	/* The value is set in main */
TYPEINFOIMPL( Drawable3D );
DISPATCHIMPL( Drawable3D );


Lang::Group3D::Group3D( )
{ }

Lang::Group3D::~Group3D( )
{ }

RefCountPtr< const Lang::Drawable2D >
Lang::Group3D::typed_to2D( const Kernel::PassedDyn & dyn, const Lang::Transform3D & tf, const RefCountPtr< const Lang::Drawable3D > & self ) const
{
	return group_to2D( dyn, tf );
}


RefCountPtr< const Lang::Class > Lang::Group3D::TypeID( new Lang::SystemFinalClass( strrefdup( "Group3D" ) ) );
TYPEINFOIMPL( Group3D );

RefCountPtr< const Lang::Group3D >
Helpers::newGroup3D( const RefCountPtr< const Kernel::GraphicsState > & metaState, const RefCountPtr< const Lang::Drawable3D > & obj2, const RefCountPtr< const Lang::Drawable3D > & obj1 )
{
	RefCountPtr< const Lang::Group3D > res = Lang::THE_NULL3D;
	res = RefCountPtr< const Lang::GroupPair3D >( new Lang::GroupPair3D( obj1,
																																						 res,
																																						 metaState ) );
	res = RefCountPtr< const Lang::GroupPair3D >( new Lang::GroupPair3D( obj2,
																																						 res,
																																						 metaState ) );
	return res;
}

RefCountPtr< const Lang::Group3D >
Helpers::newGroup3D( const RefCountPtr< const Kernel::GraphicsState > & metaState, const RefCountPtr< const Lang::Drawable3D > & obj3, const RefCountPtr< const Lang::Drawable3D > & obj2, const RefCountPtr< const Lang::Drawable3D > & obj1 )
{
	RefCountPtr< const Lang::Group3D > res = Lang::THE_NULL3D;
	res = RefCountPtr< const Lang::GroupPair3D >( new Lang::GroupPair3D( obj1,
																																			 res,
																																			 metaState ) );
	res = RefCountPtr< const Lang::GroupPair3D >( new Lang::GroupPair3D( obj2,
																																			 res,
																																			 metaState ) );
	res = RefCountPtr< const Lang::GroupPair3D >( new Lang::GroupPair3D( obj3,
																																			 res,
																																			 metaState ) );
	return res;
}


Lang::GroupPair3D::GroupPair3D( const RefCountPtr< const Lang::Drawable3D > & car, const RefCountPtr< const Lang::Group3D > & cdr, const RefCountPtr< const Kernel::GraphicsState > & metaState )
	: metaState_( metaState ), car_( car ), cdr_( cdr )
{ }

Lang::GroupPair3D::~GroupPair3D( )
{ }

bool
Lang::GroupPair3D::isNull( ) const
{
	return false;
}

RefCountPtr< const Lang::Group2D >
Lang::GroupPair3D::group_to2D( const Kernel::PassedDyn & dyn, const Lang::Transform3D & tf ) const
{
	return RefCountPtr< const Lang::Group2D >( new Lang::GroupPair2D( car_->typed_to2D( dyn, tf, car_ ),
																																					cdr_->group_to2D( dyn, tf ),
																																					metaState_ ) );
}

void
Lang::GroupPair3D::polygonize( std::list< RefCountPtr< Computation::PaintedPolygon3D > > * zBufPile, std::list< RefCountPtr< Computation::StrokedLine3D > > * linePile, const Kernel::PassedDyn & dyn, const Lang::Transform3D & tf, const RefCountPtr< const Lang::Drawable3D > & self ) const
{
	car_->polygonize( zBufPile, linePile, dyn, tf, car_ );
	cdr_->polygonize( zBufPile, linePile, dyn, tf, cdr_ );
}

void
Lang::GroupPair3D::findTags( std::vector< Kernel::ValueRef > * dst, const Kernel::PassedDyn & dyn, Lang::Symbol::KeyType key, const Lang::Transform3D & tf ) const
{
	/* See note on order in GroupPair2D!
	 */
	cdr_->findTags( dst, dyn, key, tf );
	car_->findTags( dst, dyn, key, tf );
}

bool
Lang::GroupPair3D::findOneTag( Kernel::EvalState * evalState, Lang::Symbol::KeyType key, const Lang::Transform3D & tf ) const
{
	return
		cdr_->findOneTag( evalState, key, tf ) ||
		car_->findOneTag( evalState, key, tf );
}

void
Lang::GroupPair3D::gcMark( Kernel::GCMarkedSet & marked )
{
	const_cast< Lang::Drawable3D * >( car_.getPtr( ) )->gcMark( marked );
	const_cast< Lang::Group3D * >( cdr_.getPtr( ) )->gcMark( marked );
}


Lang::GroupNull3D::GroupNull3D( )
{ }

Lang::GroupNull3D::~GroupNull3D( )
{ }

bool
Lang::GroupNull3D::isNull( ) const
{
	return true;
}

RefCountPtr< const Lang::Group2D >
Lang::GroupNull3D::group_to2D( const Kernel::PassedDyn & dyn, const Lang::Transform3D & tf ) const
{
	return Lang::THE_NULL2D;
}

void
Lang::GroupNull3D::polygonize( std::list< RefCountPtr< Computation::PaintedPolygon3D > > * zBufPile, std::list< RefCountPtr< Computation::StrokedLine3D > > * linePile, const Kernel::PassedDyn & dyn, const Lang::Transform3D & tf, const RefCountPtr< const Lang::Drawable3D > & self ) const
{ }


Lang::Drawable2Din3D::Drawable2Din3D( RefCountPtr< const Lang::Drawable2D > element )
	: element_( element )
{ }

Lang::Drawable2Din3D::~Drawable2Din3D( )
{ }

RefCountPtr< const Lang::Drawable2D >
Lang::Drawable2Din3D::typed_to2D( const Kernel::PassedDyn & dyn, const Lang::Transform3D & tf, const RefCountPtr< const Lang::Drawable3D > & self ) const
{
	Concrete::Length eyez = dyn->getEyeZ( );
	if( eyez < Concrete::HUGE_LENGTH )
		{
			std::ostringstream msg;
			msg << "An object of type " << element_->getTypeName( ) << " in 3D cannot go back to 2D viewed from a finite distance.	Consider using facing rather than immerse." ;
			throw Exceptions::MiscellaneousRequirement( strrefdup( msg.str( ).c_str( ) ) );
		}

	// The transform in 2D is obtained by setting z = 0 in the source 3D coordinates, and omitting the z-coordinate in the new coordinates

	return RefCountPtr< const Lang::Drawable2D >( new Lang::Transformed2D( element_, Lang::Transform2D( tf.xx_, tf.yx_, tf.xy_, tf.yy_, tf.xt_, tf.yt_ ) ) );
}

void
Lang::Drawable2Din3D::polygonize( std::list< RefCountPtr< Computation::PaintedPolygon3D > > * zBufPile, std::list< RefCountPtr< Computation::StrokedLine3D > > * linePile, const Kernel::PassedDyn & dyn, const Lang::Transform3D & tf, const RefCountPtr< const Lang::Drawable3D > & self ) const
{
	throw Exceptions::NotImplemented( "Triangularization of immersed objects." );
}

void
Lang::Drawable2Din3D::findTags( std::vector< Kernel::ValueRef > * dst, const Kernel::PassedDyn & dyn, Lang::Symbol::KeyType key, const Lang::Transform3D & tf ) const
{
	Concrete::Length eyez = dyn->getEyeZ( );
	if( eyez < Concrete::HUGE_LENGTH )
		{
			std::ostringstream msg;
			msg << "Tags in an immersed object are not accessible from a finite viewing distance.	Consider using facing rather than immerse." ;
			throw Exceptions::MiscellaneousRequirement( strrefdup( msg.str( ).c_str( ) ) );
		}

	// The transform in 2D is obtained by setting z = 0 in the source 3D coordinates, and omitting the z-coordinate in the new coordinates

	element_->findTags( dst, dyn, key, Lang::Transform2D( tf.xx_, tf.yx_, tf.xy_, tf.yy_, tf.xt_, tf.yt_ ) );
}

bool
Lang::Drawable2Din3D::findOneTag( Kernel::EvalState * evalState, Lang::Symbol::KeyType key, const Lang::Transform3D & tf ) const
{
	Concrete::Length eyez = evalState->dyn_->getEyeZ( );
	if( eyez < Concrete::HUGE_LENGTH )
		{
			std::ostringstream msg;
			msg << "Tags in an immersed object are not accessible from a finite viewing distance.	Consider using facing rather than immerse." ;
			throw Exceptions::MiscellaneousRequirement( strrefdup( msg.str( ).c_str( ) ) );
		}

	// The transform in 2D is obtained by setting z = 0 in the source 3D coordinates, and omitting the z-coordinate in the new coordinates

	return
		element_->findOneTag( evalState, key, Lang::Transform2D( tf.xx_, tf.yx_, tf.xy_, tf.yy_, tf.xt_, tf.yt_ ) );
}

void
Lang::Drawable2Din3D::gcMark( Kernel::GCMarkedSet & marked )
{
	const_cast< Lang::Drawable2D * >( element_.getPtr( ) )->gcMark( marked );
}



Lang::Facing2Din3D::Facing2Din3D( RefCountPtr< const Lang::Drawable2D > element, bool scale, bool distort )
	: element_( element ), scale_( scale ), distort_( distort )
{ }

Lang::Facing2Din3D::~Facing2Din3D( )
{ }

RefCountPtr< const Lang::Drawable2D >
Lang::Facing2Din3D::typed_to2D( const Kernel::PassedDyn & dyn, const Lang::Transform3D & tf, const RefCountPtr< const Lang::Drawable3D > & self ) const
{
	Concrete::Length eyez = dyn->getEyeZ( );
	double s = 1;
	if( scale_ && eyez < Concrete::HUGE_LENGTH )
		{
			s = eyez / ( eyez - tf.zt_ );
		}

	Concrete::Length x;
	Concrete::Length y;
	if( eyez < Concrete::HUGE_LENGTH )
		{
			x = tf.xt_ * ( eyez / ( eyez - tf.zt_ ) );
			y = tf.yt_ * ( eyez / ( eyez - tf.zt_ ) );
		}
	else
		{
			x = tf.xt_;
			y = tf.yt_;
		}

	if( distort_ )
		{
			return RefCountPtr< const Lang::Drawable2D >( new Lang::Transformed2D( element_, Lang::Transform2D( s * tf.xx_, s * tf.yx_, s * tf.xy_, s * tf.yy_, x, y ) ) );
		}
	else
		{
			return RefCountPtr< const Lang::Drawable2D >( new Lang::Transformed2D( element_, Lang::Transform2D( s, 0, 0, s, x, y ) ) );
		}
}

void
Lang::Facing2Din3D::polygonize( std::list< RefCountPtr< Computation::PaintedPolygon3D > > * zBufPile, std::list< RefCountPtr< Computation::StrokedLine3D > > * linePile, const Kernel::PassedDyn & dyn, const Lang::Transform3D & tf, const RefCountPtr< const Lang::Drawable3D > & self ) const
{
	throw Exceptions::MiscellaneousRequirement( "Facing objects cannot be polygonized." );
}

void
Lang::Facing2Din3D::findTags( std::vector< Kernel::ValueRef > * dst, const Kernel::PassedDyn & dyn, Lang::Symbol::KeyType key, const Lang::Transform3D & tf ) const
{
	Concrete::Length eyez = dyn->getEyeZ( );
	double s = 1;
	if( scale_ && eyez < Concrete::HUGE_LENGTH )
		{
			s = eyez / ( eyez - tf.zt_ );
		}

	Concrete::Length x;
	Concrete::Length y;
	if( eyez < Concrete::HUGE_LENGTH )
		{
			x = tf.xt_ * ( eyez / ( eyez - tf.zt_ ) );
			y = tf.yt_ * ( eyez / ( eyez - tf.zt_ ) );
		}
	else
		{
			x = tf.xt_;
			y = tf.yt_;
		}

	if( distort_ )
		{
			element_->findTags( dst, dyn, key, Lang::Transform2D( s * tf.xx_, s * tf.yx_, s * tf.xy_, s * tf.yy_, x, y ) );
		}
	else
		{
			element_->findTags( dst, dyn, key, Lang::Transform2D( s, 0, 0, s, x, y ) );
		}
}

bool
Lang::Facing2Din3D::findOneTag( Kernel::EvalState * evalState, Lang::Symbol::KeyType key, const Lang::Transform3D & tf ) const
{
	Concrete::Length eyez = evalState->dyn_->getEyeZ( );
	double s = 1;
	if( scale_ && eyez < Concrete::HUGE_LENGTH )
		{
			s = eyez / ( eyez - tf.zt_ );
		}

	Concrete::Length x;
	Concrete::Length y;
	if( eyez < Concrete::HUGE_LENGTH )
		{
			x = tf.xt_ * ( eyez / ( eyez - tf.zt_ ) );
			y = tf.yt_ * ( eyez / ( eyez - tf.zt_ ) );
		}
	else
		{
			x = tf.xt_;
			y = tf.yt_;
		}

	if( distort_ )
		{
			return
				element_->findOneTag( evalState, key, Lang::Transform2D( s * tf.xx_, s * tf.yx_, s * tf.xy_, s * tf.yy_, x, y ) );
		}
	else
		{
			return
				element_->findOneTag( evalState, key, Lang::Transform2D( s, 0, 0, s, x, y ) );
		}
}

void
Lang::Facing2Din3D::gcMark( Kernel::GCMarkedSet & marked )
{
	const_cast< Lang::Drawable2D * >( element_.getPtr( ) )->gcMark( marked );
}


Lang::FacingFunction3D::FacingFunction3D( Kernel::PassedDyn dyn, RefCountPtr< const Lang::Function > generator )
	: dyn_( dyn ), generator_( generator )
{ }

Lang::FacingFunction3D::~FacingFunction3D( )
{ }

RefCountPtr< const Lang::Drawable2D >
Lang::FacingFunction3D::typed_to2D( const Kernel::PassedDyn & dyn, const Lang::Transform3D & tf, const RefCountPtr< const Lang::Drawable3D > & self ) const
{
	/* Too bad we can't call the function CPP here...
	 */
	Kernel::ValueRef valUntyped = NullPtr< const Lang::Value >( );

	Ast::SourceLocation loc = Ast::SourceLocation( "** Facing a function in 3D... **" );

	/* Note that the use of a StoreValueContinuation relies on valUntyped being alive at the time the continuation is invoked.
	 */
	bool done = false;
	Kernel::EvalState evalState( 0,
															 0,
															 dyn_,
															 Kernel::ContRef( new Kernel::StoreValueContinuation( & valUntyped,
																																										Kernel::ContRef( new Kernel::ExitContinuation( & done, loc ) ),
																																										loc ) ) );

	generator_->call( & evalState, RefCountPtr< const Lang::Value >( new Lang::Transform3D( tf ) ), loc );

	while( ! done )
		{
			evalState.expr_->eval( & evalState );
		}

	return Helpers::down_cast< const Lang::Drawable2D >( valUntyped, loc );
}

void
Lang::FacingFunction3D::polygonize( std::list< RefCountPtr< Computation::PaintedPolygon3D > > * zBufPile, std::list< RefCountPtr< Computation::StrokedLine3D > > * linePile, const Kernel::PassedDyn & dyn, const Lang::Transform3D & tf, const RefCountPtr< const Lang::Drawable3D > & self ) const
{
	throw Exceptions::MiscellaneousRequirement( "Facing functions cannot be polygonized." );
}

void
Lang::FacingFunction3D::findTags( std::vector< Kernel::ValueRef > * dst, const Kernel::PassedDyn & dyn, Lang::Symbol::KeyType key, const Lang::Transform3D & tf ) const
{
	/*
	 * At the moment, we don't search facing functions for tags!
	 */
}

bool
Lang::FacingFunction3D::findOneTag( Kernel::EvalState * evalState, Lang::Symbol::KeyType key, const Lang::Transform3D & tf ) const
{
	/*
	 * At the moment, we don't search facing functions for tags!
	 */
	return false;
}

void
Lang::FacingFunction3D::gcMark( Kernel::GCMarkedSet & marked )
{
	const_cast< Lang::Function * >( generator_.getPtr( ) )->gcMark( marked );
}


Computation::PaintedPolygon3D::PaintedPolygon3D( bool singleSided, const Concrete::UnitFloatTriple & normal, Concrete::Length m, Concrete::Length tiebreaker )
	: singleSided_( singleSided ), normal_( normal ), m_( m ), tiebreaker_( tiebreaker )
{ }

Computation::PaintedPolygon3D::~PaintedPolygon3D( )
{ }

void
Computation::PaintedPolygon3D::pushPoint( const Concrete::Coords3D & p )
{
	Concrete::Length tmp = Concrete::inner( normal_, p ) - m_;
	if( fabs( tmp > Computation::theTrixelizeSplicingTol ) )
		{
			std::cerr << "Greater than tol: " << tmp.offtype< 1, 0 >( ) << std::endl ;
			throw Exceptions::OutOfRange( "The 3D painted path was not flat enough to make a polygon." );
		}
	points_.push_back( p );
}

void
Computation::PaintedPolygon3D::push_zBufTriangles( const Lang::Transform3D & tf, const Concrete::Length eyez, std::list< Computation::ZBufTriangle > * triangleQueue, bool respectSingleSided ) const
{
	// This function shall not push tiny triangles!

	// Note that the ZBufTriangles pushed are not completely transformed, since *this is the painter, and not transformed by tf.
	// The ugly solution to this is that tf must also be specified when painting the area.

	if( points_.size( ) < 3 )
		{
			return;
		}

	// At the moment, convexity is assumed!

	typedef Computation::ZBufTriangle::ZMap ZMapType;
	if( tf.isIdentity( ) )
		{
			if( respectSingleSided && singleSided_ )
				{
					const Concrete::Coords3D & somePoint = points_.front( );
					if( normal_.z_ * eyez - Concrete::inner( normal_, somePoint ) <= 0 )
						{
							return;
						}
				}

			RefCountPtr< const ZMapType > zMap( new ZMapType( normal_, m_, tiebreaker_, eyez ) );
			typedef typeof points_ ListType;
			ListType::const_iterator i = points_.begin( );
			Concrete::Coords2D p0 = i->make2DAutomatic( eyez );
			++i;
			Concrete::Coords2D p1 = i->make2DAutomatic( eyez );
			++i;
			for( ; i != points_.end( ); ++i )
				{
					Concrete::Coords2D p2 = i->make2DAutomatic( eyez );
					// This tolerance test assures that we don't produce tiny-tiny triangles.	It is an inscribed circle test.
					if( Computation::triangleArea( p0, p1, p2 ) > Computation::theTrixelizeOverlapTol * Computation::triangleSemiPerimeter( p0, p1, p2 ) )
						{
							triangleQueue->push_back( Computation::ZBufTriangle( this,
																																	 zMap,
																																	 p0, p1, p2 ) );
						}
					p1 = p2;
				}
		}
	else
		{
			RefCountPtr< const ZMapType > zMap = RefCountPtr< const ZMapType >( NullPtr< const ZMapType >( ) );
			{
				Concrete::UnitFloatTriple Tnormal = tf.transformPlaneUnitNormal( normal_ );
				if( respectSingleSided && singleSided_ )
					{
						const Concrete::Coords3D & somePoint = points_.front( );
						if( Tnormal.z_ * eyez - Concrete::inner( Tnormal, somePoint.transformed( tf ) ) <= 0 )
							{
								return;
							}
					}

				double ax = fabs( normal_.x_ );
				double ay = fabs( normal_.y_ );
				double az = fabs( normal_.z_ );
				Concrete::Coords3D x0( 0, 0, 0 );
				if( ax >= ay && ax >= az )
					{
						x0 = Concrete::Coords3D( m_ / normal_.x_, 0, 0 );
					}
				else if( ay >= az )
					{
						x0 = Concrete::Coords3D( 0, m_ / normal_.y_, 0 );
					}
				else
					{
						x0 = Concrete::Coords3D( 0, 0, m_ / normal_.z_ );
					}
				Concrete::Length Tm = Concrete::inner( Tnormal, x0.transformed( tf ) );

				zMap = RefCountPtr< const ZMapType >( new ZMapType( Tnormal, Tm, tiebreaker_, eyez ) );
			}


			typedef typeof points_ ListType;
			ListType::const_iterator i = points_.begin( );
			Concrete::Coords2D p0 = i->transformed( tf ).make2DAutomatic( eyez );
			++i;
			Concrete::Coords2D p1 = i->transformed( tf ).make2DAutomatic( eyez );
			++i;
			for( ; i != points_.end( ); ++i )
				{
					Concrete::Coords2D p2 = i->transformed( tf ).make2DAutomatic( eyez );
					// This tolerance test assures that we don't produce tiny-tiny triangles.	It is an inscribed circle test.
					if( Computation::triangleArea( p0, p1, p2 ) > Computation::theTrixelizeOverlapTol * Computation::triangleSemiPerimeter( p0, p1, p2 ) )
						{
							triangleQueue->push_back( Computation::ZBufTriangle( this,
																																	 zMap,
																																	 p0, p1, p2 ) );
						}
					p1 = p2;
				}
		}
}

Concrete::Coords3D
Computation::PaintedPolygon3D::computeMean( ) const
{
	Concrete::Coords3D res( 0, 0, 0 );
	typedef typeof points_ ListType;
	for( ListType::const_iterator i = points_.begin( ); i != points_.end( ); ++i )
		{
			res = res + *i;
		}
	return ( 1. / points_.size( ) ) * res;
}

const Computation::FacetLatticeVertex *
Computation::FacetLatticeEdge::getOther( const FacetLatticeEdge * e ) const
{
	if( p0_ == e->p0_ || p0_ == e->p1_ )
		{
			return p1_;
		}
	return p0_;
}

bool
Computation::FacetLatticeEdge::sharePoint( const FacetLatticeEdge * e ) const
{
	return
		p0_ == e->p0_ ||
		p0_ == e->p1_ ||
		p1_ == e->p0_ ||
		p1_ == e->p1_;
}


void
Computation::FacetLatticeEdge::split( const Concrete::Length eyez,
																			PtrOwner_back_Access< std::list< const Computation::FacetLatticeEdge * > > * edgeMem,
																			PtrOwner_back_Access< std::list< const Computation::FacetLatticeVertex * > > * vertexMem,
																			const Computation::FacetLatticeEdge ** child1, const Computation::FacetLatticeEdge ** child2 ) const
{
	if( child1_ != 0 )
		{
			*child1 = child1_;
			*child2 = child2_;
			return;
		}

	if( eyez == Concrete::HUGE_LENGTH )
		{
			FacetLatticeVertex * pNew = new FacetLatticeVertex( 0.5 * ( p0_->p3D_ + p1_->p3D_ ), eyez, vertexMem->size( ) );
			vertexMem->push_back( pNew );
			child1_ = new Computation::FacetLatticeEdge( p0_, pNew );
			edgeMem->push_back( child1_ );
			child2_ = new Computation::FacetLatticeEdge( pNew, p1_ );
			edgeMem->push_back( child2_ );
		}
	else
		{
			// We compute the midpoint _in_view_, and then find where this is along the 3D line.
			Concrete::Coords2D mid2D = 0.5 * ( p0_->p2D_ + p1_->p2D_ );

			// If we write the point we seek in 3D as
			//	 p0_ + lambda * ( p1_ - p0_ )
			// then <lambda> must solve the overdetermined system
			//	 a * lambda == -b
			// where
			const double ra = ( p1_->p3D_.z_ - p0_->p3D_.z_ ) / eyez;
			Concrete::Coords2D a( p1_->p3D_.x_ - p0_->p3D_.x_ + mid2D.x_ * ra,
														p1_->p3D_.y_ - p0_->p3D_.y_ + mid2D.y_ * ra );
			const double rb = static_cast< double >( p0_->p3D_.z_	/ eyez ) - 1;
			Concrete::Coords2D b( p0_->p3D_.x_ + mid2D.x_ * rb,
														p0_->p3D_.y_ + mid2D.y_ * rb );
			const double lambda = - Concrete::innerScalar( a, b ) / Concrete::innerScalar( a, a );

			FacetLatticeVertex * pNew = new FacetLatticeVertex( ( 1 - lambda ) * p0_->p3D_ + lambda * p1_->p3D_, eyez, vertexMem->size( ) );
			vertexMem->push_back( pNew );
			child1_ = new Computation::FacetLatticeEdge( p0_, pNew );
			edgeMem->push_back( child1_ );
			child2_ = new Computation::FacetLatticeEdge( pNew, p1_ );
			edgeMem->push_back( child2_ );
		}

	*child1 = child1_;
	*child2 = child2_;
}


Computation::FacetLatticeVertex::FacetLatticeVertex( const Concrete::Coords3D & p3D, const Concrete::Length eyez, const size_t i )
	: p3D_( p3D ), p2D_( p3D.make2DAutomatic( eyez ) ), i_( i )
{ }

const Computation::FacetLatticeEdge *
Computation::FacetLatticeTriangle::getOther( const Computation::FacetLatticeEdge *ea, const Computation::FacetLatticeEdge *eb ) const
{
	if( ea == e0_ )
		{
			if( eb == e1_ )
				{
					return e2_;
				}
			return e1_;
		}
	else if( ea == e1_ )
		{
			if( eb == e0_ )
				{
					return e2_;
				}
			return e0_;
		}
	else if( ea == e2_ )
		{
			if( eb == e0_ )
				{
					return e1_;
				}
			return e0_;
		}

	throw Exceptions::InternalError( "FacetTriangle::getOther called with alien edge." );
}

bool
Computation::FacetLatticeTriangle::fitsResolution( const Concrete::Length resolution, const Concrete::Length eyez,
																									 PtrOwner_back_Access< std::list< const Computation::FacetLatticeEdge * > > * edgeMem,
																									 PtrOwner_back_Access< std::list< const Computation::FacetLatticeVertex * > > * vertexMem,
																									 const Computation::FacetLatticeTriangle ** child1, const Computation::FacetLatticeTriangle ** child2 ) const
{
	const Computation::FacetLatticeEdge * ec1;
	const Computation::FacetLatticeEdge * ec2;

	Concrete::Length l0 = e0_->length2D( );
	Concrete::Length l1 = e1_->length2D( );
	Concrete::Length l2 = e2_->length2D( );
	Concrete::Length lMax = std::max( l0, std::max( l1, l2 ) );

	if( lMax < resolution )
		{
			return true;
		}

	const Computation::FacetLatticeEdge * ea = e0_;
	const Computation::FacetLatticeEdge * eb = e1_;
	const Computation::FacetLatticeEdge * ec = e2_;
	if( l1 == lMax )
		{
			ea = e1_;
			eb = e2_;
			ec = e0_;
		}
	else if( l2 == lMax )
		{
			ea = e2_;
			eb = e0_;
			ec = e1_;
		}


	ea->split( eyez, edgeMem, vertexMem, & ec1, & ec2 );
	Computation::FacetLatticeEdge * newEdge = new Computation::FacetLatticeEdge( ea->midpoint( ), eb->getOther( ea ) );
	edgeMem->push_back( newEdge );
	if( eb->sharePoint( ec2 ) )
		{
			*child1 = new Computation::FacetLatticeTriangle( ec2, eb, newEdge );
			*child2 = new Computation::FacetLatticeTriangle( newEdge, ec, ec1 );
		}
	else
		{
			*child1 = new Computation::FacetLatticeTriangle( ec1, eb, newEdge );
			*child2 = new Computation::FacetLatticeTriangle( newEdge, ec, ec2 );
		}

	return false;
}


RefCountPtr< const Lang::Drawable2D >
Computation::FacetLatticeTriangle::paint( const RefCountPtr< const Computation::FacetInterpolatorGray > & interpolator, const std::list< RefCountPtr< const Lang::LightSource > > & lights, const Concrete::Length eyez ) const
{
	Kernel::GraphicsState * metaStatePtr = new Kernel::GraphicsState( true );

	metaStatePtr->nonStrokingColor_ = interpolator->compute( Lang::THE_3D_IDENTITY, lights,
																													 Computation::triangleIncenter( v0_->p3D_, v1_->p3D_, v2_->p3D_ ),
																													 eyez );

	RefCountPtr< const Kernel::GraphicsState > metaState( metaStatePtr );

	RefCountPtr< Lang::ElementaryPath2D > path = RefCountPtr< Lang::ElementaryPath2D >( new Lang::ElementaryPath2D( ) );
	path->close( );
	// It's a pity we have to copy duplicate those points...
	path->push_back( new Concrete::PathPoint2D( new Concrete::Coords2D( v0_->p2D_ ) ) );
	path->push_back( new Concrete::PathPoint2D( new Concrete::Coords2D( v1_->p2D_ ) ) );
	path->push_back( new Concrete::PathPoint2D( new Concrete::Coords2D( v2_->p2D_ ) ) );

	return RefCountPtr< const Lang::PaintedPolygon2D >( new Lang::PaintedPolygon2D( metaState, path ) );
}

RefCountPtr< const Lang::Drawable2D >
Computation::FacetLatticeTriangle::paint( const RefCountPtr< const Computation::FacetInterpolatorRGB > & interpolator, const std::list< RefCountPtr< const Lang::LightSource > > & lights, const Concrete::Length eyez ) const
{
	Kernel::GraphicsState * metaStatePtr = new Kernel::GraphicsState( true );

	metaStatePtr->nonStrokingColor_ = interpolator->compute( Lang::THE_3D_IDENTITY, lights,
																													 Computation::triangleIncenter( v0_->p3D_, v1_->p3D_, v2_->p3D_ ),
																													 eyez );

	RefCountPtr< const Kernel::GraphicsState > metaState( metaStatePtr );

	RefCountPtr< Lang::ElementaryPath2D > path = RefCountPtr< Lang::ElementaryPath2D >( new Lang::ElementaryPath2D( ) );
	path->close( );
	// It's a pity we have to copy duplicate those points...
	path->push_back( new Concrete::PathPoint2D( new Concrete::Coords2D( v0_->p2D_ ) ) );
	path->push_back( new Concrete::PathPoint2D( new Concrete::Coords2D( v1_->p2D_ ) ) );
	path->push_back( new Concrete::PathPoint2D( new Concrete::Coords2D( v2_->p2D_ ) ) );

	return RefCountPtr< const Lang::PaintedPolygon2D >( new Lang::PaintedPolygon2D( metaState, path ) );
}

void
Computation::FacetLatticeTriangle::getVertexes( const Computation::FacetLatticeVertex ** va, const Computation::FacetLatticeVertex ** vb, const Computation::FacetLatticeVertex ** vc ) const
{
	*va = v0_;
	*vb = v1_;
	*vc = v2_;
}

// This function is very close to the PDF definition of a free-form triangle mesh.
// If this triangle is an extension of va--vb--vc, then va, vb, vc are updated and the corresponding flag (se PDF spec) is returned.
// If this is not an extension, the value 3 is returned (which is not a legal falg value), and va, vb, vc are left unchanged.
unsigned char
Computation::FacetLatticeTriangle::extendLattice( const Computation::FacetLatticeVertex ** va, const Computation::FacetLatticeVertex ** vb, const Computation::FacetLatticeVertex ** vc ) const
{
	// vc must be part of this triangle if we are to extend va--vb--vc.
	if( v0_ == *vc || v1_ == *vc || v2_ == *vc )
		{
			// First, we try to extend vb--vc (corresponding to flag=1)
			if( v0_ == *vb || v1_ == *vb || v2_ == *vb )
				{
					// We now update va and vb so that they define the edge that is extended.
					*va = *vb;
					*vb = *vc;

					// Now we need to find which of our corners that is the added one.
					if( v0_ != *va && v0_ != *vb )
						{
							*vc = v0_;
						}
					else if( v1_ != *va && v1_ != *vb )
						{
							*vc = v1_;
						}
					else
						{
							*vc = v2_;
						}
					return 1;
				}

			// Then, we try to extend va--vc (corresponding to flag=2)
			if( v0_ == *va || v1_ == *va || v2_ == *va )
				{
					// We now update va and vb so that they define the edge that is extended.
					//					*va = *va;
					*vb = *vc;

					// Now we need to find which of our corners that is the added one.
					if( v0_ != *va && v0_ != *vb )
						{
							*vc = v0_;
						}
					else if( v1_ != *va && v1_ != *vb )
						{
							*vc = v1_;
						}
					else
						{
							*vc = v2_;
						}
					return 2;
				}
		}

	return 3;
}

void
Computation::FacetLatticeTriangle::display2D( std::ostream & os ) const
{
	Concrete::Length lMax = Concrete::ZERO_LENGTH;
	{
		Concrete::Length tmp = v0_->p2D_.distanceTo( v1_->p2D_ );
		if( tmp > lMax )
			{
				lMax = tmp;
			}
	}
	{
		Concrete::Length tmp = v1_->p2D_.distanceTo( v2_->p2D_ );
		if( tmp > lMax )
			{
				lMax = tmp;
			}
	}
	{
		Concrete::Length tmp = v2_->p2D_.distanceTo( v0_->p2D_ );
		if( tmp > lMax )
			{
				lMax = tmp;
			}
	}

	os << "Triangle ( " << v0_->p2D_ << " -- " << v1_->p2D_ << " -- " << v2_->p2D_ << " ) "
		 << " with 2D area " << Concrete::Area::offtype( Computation::triangleArea( v0_->p2D_, v1_->p2D_, v2_->p2D_ ) )
		 << " and longest side " << Lang::Length( lMax ) ;
}

void
Computation::FacetLatticeTriangle::display3D( std::ostream & os ) const
{
	os << "Triangle ( " << v0_->p3D_ << " -- " << v1_->p3D_ << " -- " << v2_->p3D_ << " )" ;
}


void
Computation::PaintedPolygon3D::makeLattice( PtrOwner_back_Access< std::list< const Computation::FacetLatticeTriangle * > > * lattice,
																						PtrOwner_back_Access< std::list< const Computation::FacetLatticeEdge * > > * edgeMem,
																						PtrOwner_back_Access< std::list< const FacetLatticeVertex * > > * vertexMem,
																						const Concrete::Length viewResolution,
																						const Concrete::Length eyez, const Lang::Transform3D & tf ) const
{
	if( points_.size( ) < 3 )
		{
			throw Exceptions::InternalError( "Less than three points in PaintedPolygon3D::makeLattice!" );
		}

	std::list< const Computation::FacetLatticeTriangle * > jobQueue;

	typedef typeof points_ ListType;

	if( points_.size( ) == 3 )
		{
			ListType::const_iterator i = points_.begin( );
			Computation::FacetLatticeVertex * p0 = new Computation::FacetLatticeVertex( i->transformed( tf ), eyez, vertexMem->size( ) );
			vertexMem->push_back( p0 );
			++i;
			Computation::FacetLatticeVertex * p1 = new Computation::FacetLatticeVertex( i->transformed( tf ), eyez, vertexMem->size( ) );
			vertexMem->push_back( p1 );
			++i;
			Computation::FacetLatticeVertex * p2 = new Computation::FacetLatticeVertex( i->transformed( tf ), eyez, vertexMem->size( ) );
			vertexMem->push_back( p2 );

			Computation::FacetLatticeEdge * e0 = new Computation::FacetLatticeEdge( p0, p1 );
			edgeMem->push_back( e0 );
			Computation::FacetLatticeEdge * e1 = new Computation::FacetLatticeEdge( p1, p2 );
			edgeMem->push_back( e1 );
			Computation::FacetLatticeEdge * e2 = new Computation::FacetLatticeEdge( p2, p0 );
			edgeMem->push_back( e2 );

			jobQueue.push_back( new Computation::FacetLatticeTriangle( e0, e1, e2 ) );
		}
	else
		{
			ListType::const_iterator i = points_.begin( );
			Computation::FacetLatticeVertex * p0 = new Computation::FacetLatticeVertex( i->transformed( tf ), eyez, vertexMem->size( ) );
			vertexMem->push_back( p0 );
			++i;
			Computation::FacetLatticeVertex * p1 = new Computation::FacetLatticeVertex( i->transformed( tf ), eyez, vertexMem->size( ) );
			vertexMem->push_back( p1 );
			++i;
 
			Computation::FacetLatticeEdge * e0 = new Computation::FacetLatticeEdge( p0, p1 );
			edgeMem->push_back( e0 );

			for( ; i != points_.end( ); ++i )
				{
					Computation::FacetLatticeVertex * p2 = new Computation::FacetLatticeVertex( i->transformed( tf ), eyez, vertexMem->size( ) );
					vertexMem->push_back( p2 );

					Computation::FacetLatticeEdge * e1 = new Computation::FacetLatticeEdge( p1, p2 );
					edgeMem->push_back( e1 );

					Computation::FacetLatticeEdge * e2 = new Computation::FacetLatticeEdge( p2, p0 );
					edgeMem->push_back( e2 );

					jobQueue.push_back( new Computation::FacetLatticeTriangle( e0, e1, e2 ) );

					p1 = p2;
					e0 = e2;
				}
		}

	while( ! jobQueue.empty( ) )
		{
			//			std::cerr << "Triangles in queue: " << jobQueue.size( ) << "	Triangles in lattice: " << lattice->size( ) << std::endl ;
			const Computation::FacetLatticeTriangle * currentTriangle = jobQueue.front( );
			jobQueue.pop_front( );
			const Computation::FacetLatticeTriangle * child1 = 0;
			const Computation::FacetLatticeTriangle * child2 = 0;
			if( currentTriangle->fitsResolution( viewResolution, eyez, edgeMem, vertexMem, & child1, & child2 ) )
				{
					lattice->push_back( currentTriangle );
				}
			else
				{
					jobQueue.push_back( child1 );
					jobQueue.push_back( child2 );
					delete currentTriangle;
				}
		}
}

Computation::StrokedLine3D::StrokedLine3D( const Concrete::Coords3D & p0, const Concrete::Coords3D & p1, const RefCountPtr< const Kernel::GraphicsState > metaState )
	: p0_( p0 ), p1_( p1 ), metaState_( metaState )
{ }

Computation::StrokedLine3D::~StrokedLine3D( )
{ }

void
Computation::StrokedLine3D::push_zBufLine( const Lang::Transform3D & tf, const Concrete::Length eyez, std::list< const Computation::ZBufLine * > * lineQueue ) const
{
	typedef Computation::ZBufLine::ZMap ZMapType;
	typedef const Bezier::PolyCoeffs< Concrete::Coords2D > ViewType;
	RefCountPtr< ViewType > nullView = RefCountPtr< ViewType >( NullPtr< ViewType >( ) );
	if( tf.isIdentity( ) )
		{
			Concrete::Coords3D d = p1_ - p0_;
			RefCountPtr< const ZMapType > zMap( new ZMapType( p0_, d.directionNoFail( ), eyez ) );
			lineQueue->push_back( new Computation::ZBufLine( this, zMap, nullView, p0_.make2DAutomatic( eyez ), p1_.make2DAutomatic( eyez ) ) );
		}
	else
		{
			Concrete::Coords3D tfp0 = p0_.transformed( tf );
			Concrete::Coords3D tfp1 = p1_.transformed( tf );
			Concrete::Coords3D d = tfp1 - tfp0;
			RefCountPtr< const ZMapType > zMap( new ZMapType( tfp0, d.directionNoFail( ), eyez ) );
			lineQueue->push_back( new Computation::ZBufLine( this, zMap, nullView, tfp0.make2DAutomatic( eyez ), tfp1.make2DAutomatic( eyez ) ) );
		}
}


Computation::StrokedSplineSegment3D::StrokedSplineSegment3D( const Concrete::Coords3D & p0, const Concrete::Coords3D & p0front, const Concrete::Coords3D & p1rear, const Concrete::Coords3D & p1, const RefCountPtr< const Kernel::GraphicsState > metaState )
	: Computation::StrokedLine3D( p0, p1, metaState ),
		p0front_( p0front ),
		p1rear_( p1rear )
{ }

Computation::StrokedSplineSegment3D::~StrokedSplineSegment3D( )
{ }

void
Computation::StrokedSplineSegment3D::push_zBufLine( const Lang::Transform3D & tf, const Concrete::Length eyez, std::list< const Computation::ZBufLine * > * lineQueue ) const
{
	typedef Computation::ZBufLine::ZMap ZMapType;
	typedef const Bezier::PolyCoeffs< Concrete::Coords2D > ViewType;
	if( tf.isIdentity( ) )
		{
			Concrete::Coords3D d = p1_ - p0_;
			RefCountPtr< const ZMapType > zMap( new ZMapType( p0_, d.directionNoFail( ), eyez ) );
			RefCountPtr< ViewType > bezierView = RefCountPtr< ViewType >( new ViewType( Bezier::ControlPoints< Concrete::Coords2D >( p0_.make2DAutomatic( eyez ), p0front_.make2DAutomatic( eyez ), p1rear_.make2DAutomatic( eyez ), p1_.make2DAutomatic( eyez ) ) ) );
			lineQueue->push_back( new Computation::ZBufLine( this, zMap, bezierView, p0_.make2DAutomatic( eyez ), p1_.make2DAutomatic( eyez ) ) );
		}
	else
		{
			Concrete::Coords3D tfp0 = p0_.transformed( tf );
			Concrete::Coords3D tfp0front = p0front_.transformed( tf );
			Concrete::Coords3D tfp1rear = p1rear_.transformed( tf );
			Concrete::Coords3D tfp1 = p1_.transformed( tf );
			Concrete::Coords3D d = tfp1 - tfp0;
			RefCountPtr< const ZMapType > zMap( new ZMapType( tfp0, d.directionNoFail( ), eyez ) );
			RefCountPtr< ViewType > bezierView = RefCountPtr< ViewType >( new ViewType( Bezier::ControlPoints< Concrete::Coords2D >( tfp0.make2DAutomatic( eyez ), tfp0front.make2DAutomatic( eyez ), tfp1rear.make2DAutomatic( eyez ), tfp1.make2DAutomatic( eyez ) ) ) );
			lineQueue->push_back( new Computation::ZBufLine( this, zMap, bezierView, tfp0.make2DAutomatic( eyez ), tfp1.make2DAutomatic( eyez ) ) );
		}
}


Computation::FilledPolygon3D::FilledPolygon3D( const RefCountPtr< const Kernel::GraphicsState > & metaState,
																							 const Concrete::UnitFloatTriple & normal, Concrete::Length m,
																							 Concrete::Length tiebreaker )
	: Computation::PaintedPolygon3D( false, normal, m, tiebreaker ), metaState_( metaState )	// false means not single-sided.
{ }

Computation::FilledPolygon3D::~FilledPolygon3D( )
{ }

RefCountPtr< const Lang::PaintedPolygon2D >
Computation::FilledPolygon3D::polygon_to2D( const Kernel::PassedDyn & dyn, const Lang::Transform3D & tf, const std::list< RefCountPtr< const Lang::LightSource > > & lights ) const
{
	// A FilledPolygon3D is characterized by that it doesn't take notice about the lights or orientation.

	Concrete::Length eyez = dyn->getEyeZ( );

	RefCountPtr< Lang::ElementaryPath2D > path = RefCountPtr< Lang::ElementaryPath2D >( new Lang::ElementaryPath2D( ) );
	path->close( );
	if( tf.isIdentity( ) )
		{
			typedef typeof points_ ListType;
			for( ListType::const_iterator i = points_.begin( ); i != points_.end( ); ++i )
				{
					path->push_back( new Concrete::PathPoint2D( i->make2D( eyez ) ) );
				}
		}
	else
		{
			typedef typeof points_ ListType;
			for( ListType::const_iterator i = points_.begin( ); i != points_.end( ); ++i )
				{
					path->push_back( new Concrete::PathPoint2D( i->transformed( tf ).make2D( eyez ) ) );
				}
		}

	return RefCountPtr< const Lang::PaintedPolygon2D >( new Lang::PaintedPolygon2D( metaState_, path ) );
}

RefCountPtr< const Lang::Color >
Computation::FilledPolygon3D::getColor( ) const
{
	return metaState_->nonStrokingColor_;
}

void
Computation::FilledPolygon3D::gcMark( Kernel::GCMarkedSet & marked )
{
	//	const_cast< Kernel::GraphicsState * >( metaState_.getPtr( ) )->gcMark( marked );
}

Computation::NullPolygon3D::NullPolygon3D(	)
	: Computation::PaintedPolygon3D( true, Concrete::UnitFloatTriple( 1., 0., 0., 1. ), 0, Concrete::ZERO_LENGTH )
{ }

Computation::NullPolygon3D::~NullPolygon3D( )
{ }

RefCountPtr< const Lang::PaintedPolygon2D >
Computation::NullPolygon3D::polygon_to2D( const Kernel::PassedDyn & dyn, const Lang::Transform3D & tf, const std::list< RefCountPtr< const Lang::LightSource > > & lights ) const
{
	throw Exceptions::InternalError( "NullPolygon3D::polygon_to2D was invoked!" );
	//	return Lang::THE_NULL_POLYGON2D;
}

RefCountPtr< const Lang::Color >
Computation::NullPolygon3D::getColor( ) const
{
	return Lang::THE_BLACK;
}

void
Computation::NullPolygon3D::gcMark( Kernel::GCMarkedSet & marked )
{ }


Lang::PaintedPolygon2D::PaintedPolygon2D( RefCountPtr< const Kernel::GraphicsState > metaState, RefCountPtr< const Lang::ElementaryPath2D > path )
	: Lang::PaintedPath2D( metaState, "E" )  // "E" for "extended fill"
{
	if( ! path->isClosed( ) )
		{
			throw Exceptions::InternalError( "Attempt to create PaintedPolygon2D with non-closed path." );
		}
	addSubPath( path );
}

Lang::PaintedPolygon2D::~PaintedPolygon2D( )
{ }

RefCountPtr< const Lang::Drawable2D >
Lang::PaintedPolygon2D::clip( std::list< Computation::ZBufTriangle > * regions, const RefCountPtr< const Lang::PaintedPolygon2D > selfRef ) const
{
	if( regions->empty( ) )
		{
			throw Exceptions::InternalError( "Empty list of regions in PaintedPolygon2D::clip.	(This triangle should not have been generated at all!)" );
		}
	if( regions->size( ) == 1 )
		{
			const Computation::ZBufTriangle & theClip = regions->front( );
			const Lang::ElementaryPath2D & paintPath = * path_.front( );
			bool isInside = true;
			for( Lang::ElementaryPath2D::const_iterator i = paintPath.begin( ); i != paintPath.end( ); ++i )
				{
					if( ! theClip.contains( *((*i)->mid_), Computation::theTrixelizeOverlapTol ) )
						{
							isInside = false;
							break;
						}
				}
			if( isInside )
				{
					return selfRef;
				}
		}

	// If we reach here, clipping shall be done
	Lang::Clipped2D * resPtr = new Lang::Clipped2D( selfRef, "W" );
	Lang::ZBuf::trianglesToPolys( regions, resPtr );
	RefCountPtr< const Lang::Clipped2D > res( resPtr );

	if( selfRef->isContainedIn( resPtr ) )
		{
			return selfRef;
		}

	const bool SHOW_POLYS = false;
	if( SHOW_POLYS )
		{
			return res->debugPolys( );
		}

	return res;
}


bool
Lang::PaintedPath2D::isContainedIn( const Lang::Clipped2D * clipping ) const
{
	if( ! clipping->isSingleConvexPoly( Computation::theTrixelizeOverlapTol ) )
		{
			return false;
		}

	typedef typeof path_ PathType;
	for( PathType::const_iterator subpath = path_.begin( ); subpath != path_.end( ); ++subpath )
		{
			typedef typeof **subpath SubpathType;
			for( SubpathType::const_iterator i = (*subpath)->begin( ); i != (*subpath)->end( ); ++i )
				{
					Concrete::PathPoint2D * pathPoint = *i;
					if( pathPoint->rear_ != pathPoint->mid_ ||
							pathPoint->front_ != pathPoint->mid_ )
						{
							return false;
						}
					if( ! clipping->convexPolyContains( *(pathPoint->mid_), Computation::theTrixelizeOverlapTol ) )
						{
							return false;
						}
				}
		}

	return true;
}


Lang::PaintedPath3D::PaintedPath3D( RefCountPtr< const Kernel::GraphicsState > metaState,
																		const char * paintCmd, Concrete::Length tiebreaker )
	: metaState_( metaState ), paintCmd_( paintCmd ), tiebreaker_( tiebreaker )
{ }

Lang::PaintedPath3D::PaintedPath3D( RefCountPtr< const Kernel::GraphicsState > metaState,
																			 RefCountPtr< const Lang::ElementaryPath3D > path,
																			 const char * paintCmd, Concrete::Length tiebreaker )
	: metaState_( metaState ), paintCmd_( paintCmd ), tiebreaker_( tiebreaker )
{
	addSubPath( path );
}

Lang::PaintedPath3D::PaintedPath3D( RefCountPtr< const Kernel::GraphicsState > metaState,
																			 RefCountPtr< const Lang::MultiPath3D > paths,
																			 const char * paintCmd, Concrete::Length tiebreaker )
	: metaState_( metaState ), paintCmd_( paintCmd ), tiebreaker_( tiebreaker )
{
	for( Lang::MultiPath3D::const_iterator i = paths->begin( ); i != paths->end( ); ++i )
		{
			{
				typedef const Lang::ElementaryPath3D ArgType;
				RefCountPtr< ArgType > subpath = (*i).down_cast< ArgType >( );
				if( subpath != NullPtr< ArgType >( ) )
					{
						addSubPath( subpath );
						continue;
					}
			}

			{
				typedef const Lang::CompositePath3D ArgType;
				ArgType * subpath = dynamic_cast< ArgType * >( (*i).getPtr( ) );
				if( subpath != 0 )
					{
						addSubPath( subpath->getElementaryPath( ) );
						continue;
					}
			}
			throw Exceptions::InternalError( "Painting 3D path: Encountered a subpath of unexpected type" );
		}
}

Lang::PaintedPath3D::~PaintedPath3D( )
{ }

RefCountPtr< const Lang::Class > Lang::PaintedPath3D::TypeID( new Lang::SystemFinalClass( strrefdup( "PaintedPath3D" ) ) );
TYPEINFOIMPL( PaintedPath3D );

void
Lang::PaintedPath3D::addSubPath( RefCountPtr< const Lang::ElementaryPath3D > subpath )
{
	path_.push_back( subpath );
}

RefCountPtr< const Lang::Drawable2D >
Lang::PaintedPath3D::typed_to2D( const Kernel::PassedDyn & dyn, const Lang::Transform3D & tf, const RefCountPtr< const Lang::Drawable3D > & self ) const
{
	Concrete::Length eyez = dyn->getEyeZ( );

	RefCountPtr< const Lang::Group2D > res( Lang::THE_NULL2D );

	if( tf.isIdentity( ) )
		{
			if( ! metaState_->dash_->isSolid( ) && *paintCmd_ == 'S' )
				{
					for( std::list< RefCountPtr< const Lang::ElementaryPath3D > >::const_iterator i = path_.begin( ); i != path_.end( ); ++i )
						{
							(*i)->dashifyIn2D( & res, eyez, metaState_ );
						}
				}
			else
				{
					for( std::list< RefCountPtr< const Lang::ElementaryPath3D > >::const_iterator i = path_.begin( ); i != path_.end( ); ++i )
						{
							res = RefCountPtr< const Lang::Group2D >( new Lang::GroupPair2D( RefCountPtr< const Lang::Drawable2D >( new Lang::PaintedPath2D( metaState_, (*i)->make2D( eyez ), paintCmd_ ) ),
																																										 res,
																																										 metaState_ ) );
						}
				}
		}
	else
		{
			if( ! metaState_->dash_->isSolid( ) && *paintCmd_ == 'S' )
				{
					for( std::list< RefCountPtr< const Lang::ElementaryPath3D > >::const_iterator i = path_.begin( ); i != path_.end( ); ++i )
						{
							(*i)->elementaryTransformed( tf )->dashifyIn2D( & res, eyez, metaState_ );
						}
				}
			else
				{
					for( std::list< RefCountPtr< const Lang::ElementaryPath3D > >::const_iterator i = path_.begin( ); i != path_.end( ); ++i )
						{
							res = RefCountPtr< const Lang::Group2D >( new Lang::GroupPair2D( RefCountPtr< const Lang::Drawable2D >( new Lang::PaintedPath2D( metaState_,
																																																																													 (*i)->elementaryTransformed( tf )->make2D( eyez ),
																																																																													 paintCmd_ ) ),
																																										 res,
																																										 metaState_ ) );
						}
				}
		}
	return res;
}

void
Lang::PaintedPath3D::polygonize( std::list< RefCountPtr< Computation::PaintedPolygon3D > > * zBufPile, std::list< RefCountPtr< Computation::StrokedLine3D > > * linePile, const Kernel::PassedDyn & dyn, const Lang::Transform3D & tf, const RefCountPtr< const Lang::Drawable3D > & self ) const
{
	if( strcmp( paintCmd_, "S" ) == 0 )
		{
			typedef typeof path_ ListType;
			for( ListType::const_iterator subi = path_.begin( ); subi != path_.end( ); ++subi )
				{
					const RefCountPtr< const Lang::ElementaryPath3D > & theSub = *subi;

					if( theSub->size( ) < 2 )
						{
							continue;
						}

					typedef typeof *theSub SubListType;
					SubListType::const_iterator i = theSub->begin( );
					Concrete::Coords3D p0 = (*i)->mid_->transformed( tf );
					SubListType::const_iterator iLast = i;
					++i;
					for( ; i != theSub->end( ); iLast = i, ++i )
						{
							Concrete::Coords3D p1 = (*i)->mid_->transformed( tf );
							if( (*iLast)->front_ != (*iLast)->mid_ || (*i)->rear_ != (*i)->mid_ )
								{
									linePile->push_back( RefCountPtr< Computation::StrokedSplineSegment3D >
																			 ( new Computation::StrokedSplineSegment3D( p0, (*iLast)->front_->transformed( tf ), (*i)->rear_->transformed( tf ), p1, metaState_ ) ) );
								}
							else
								{
									linePile->push_back( RefCountPtr< Computation::StrokedLine3D >
																			 ( new Computation::StrokedLine3D( p0, p1, metaState_ ) ) );
								}
							p0 = p1;
						}
					if( theSub->isClosed( ) )
						{
							i = theSub->begin( );
							Concrete::Coords3D p1 = (*i)->mid_->transformed( tf );
							if( (*iLast)->front_ != (*iLast)->mid_ || (*i)->rear_ != (*i)->mid_ )
								{
									linePile->push_back( RefCountPtr< Computation::StrokedSplineSegment3D >
																			 ( new Computation::StrokedSplineSegment3D( p0, (*iLast)->front_->transformed( tf ), (*i)->rear_->transformed( tf ), p1, metaState_ ) ) );
								}
							else
								{
									linePile->push_back( RefCountPtr< Computation::StrokedLine3D >
																			 ( new Computation::StrokedLine3D( p0, p1, metaState_ ) ) );
								}
						}
				}

			return;
		}

	if( ! ( strcmp( paintCmd_, "f" ) == 0 ||
					strcmp( paintCmd_, "B" ) == 0 ) )
		{
			throw Exceptions::MiscellaneousRequirement( "Only stroked and (non-*) filled polygons can be put i a z buffer." );
		}


	// When we get here we know that the paintCmd_ is "f" or "B", that is, a plain fill.

//	 if( path.size( ) > 1 )
//		 {
//			 std::cerr << "Warning: The result from triangularizing a composite path may not be what is expected." << std::endl ;
//		 }

	typedef typeof path_ ListType;
	for( ListType::const_iterator subi = path_.begin( ); subi != path_.end( ); ++subi )
		{
			const RefCountPtr< const Lang::ElementaryPath3D > & theSub = *subi;

			if( theSub->size( ) < 3 )
				{
					continue;
				}
			if( ! theSub->isClosed( ) )
				{
					throw "Not closed";
				}

			typedef typeof *theSub SubListType;

			RefCountPtr< Computation::FilledPolygon3D > newPoly = RefCountPtr< Computation::FilledPolygon3D >( NullPtr< Computation::FilledPolygon3D >( ) );
			{
				// First we must compute an equation for the polygon!

				Concrete::Coords3D p0( 0, 0, 0 );
				Concrete::Coords3D p1( 0, 0, 0 );
				Concrete::Coords3D p2( 0, 0, 0 );
				theSub->getRepresentativePoints( tf, & p0, & p1, & p2 );

				try
					{
						Concrete::UnitFloatTriple normal = Concrete::crossDirection( p2 - p0, p1 - p0 );
						Concrete::Length m = Concrete::inner( normal, p0 );
						newPoly = RefCountPtr< Computation::FilledPolygon3D >( new Computation::FilledPolygon3D( metaState_, normal, m, tiebreaker_ ) );
					}
				catch( const NonLocalExit::CrossDirectionOfParallel & ball )
					{
						// This means that the crossDirection called failed because the vectors were parallel.
						// A polygon of lower dimension is invisible, so we may just continue
						continue;
					}
			}

			zBufPile->push_back( newPoly );

			for( SubListType::const_iterator i = theSub->begin( ); i != theSub->end( ); ++i )
				{
					if( (*i)->front_ != (*i)->mid_ || (*i)->rear_ != (*i)->mid_ )
						{
							throw "Corner has handle";
						}
					newPoly->pushPoint( (*i)->mid_->transformed( tf ) );
				}
		}
}

void
Lang::PaintedPath3D::gcMark( Kernel::GCMarkedSet & marked )
{ }


Lang::SingleSided3DGray::SingleSided3DGray( const RefCountPtr< const Lang::ElementaryPath3D > & points,
																						const RefCountPtr< const Computation::FacetInterpolatorGray > & interpolator,
																						bool singleSided,
																						const Concrete::UnitFloatTriple & polygonUnitNormal,
																						Concrete::Length m,
																						Concrete::Length tiebreaker,
																						Concrete::Length viewResolution,
																						Computation::FacetShadeOrder shadeOrder )
	: points_( points ), interpolator_( interpolator ), singleSided_( singleSided ), polygonUnitNormal_( polygonUnitNormal ), m_( m ), tiebreaker_( tiebreaker ),
		viewResolution_( viewResolution ), shadeOrder_( shadeOrder )
{ }

Lang::SingleSided3DGray::~SingleSided3DGray( )
{ }

RefCountPtr< const Lang::Class > Lang::SingleSided3DGray::TypeID( new Lang::SystemFinalClass( strrefdup( "SingleSided3D(gray)" ) ) );
TYPEINFOIMPL( SingleSided3DGray );

RefCountPtr< const Lang::Drawable2D >
Lang::SingleSided3DGray::typed_to2D( const Kernel::PassedDyn & dyn, const Lang::Transform3D & tf, const RefCountPtr< const Lang::Drawable3D > & self ) const
{
	throw Exceptions::NotImplemented( "SingleSided3DGray::typed_to2D;	What light scene should be used?" );
}

void
Lang::SingleSided3DGray::polygonize( std::list< RefCountPtr< Computation::PaintedPolygon3D > > * zBufPile, std::list< RefCountPtr< Computation::StrokedLine3D > > * linePile, const Kernel::PassedDyn & dyn, const Lang::Transform3D & tf, const RefCountPtr< const Lang::Drawable3D > & self ) const
{
	Concrete::UnitFloatTriple Tnormal = tf.transformPlaneUnitNormal( polygonUnitNormal_ );

	double ax = fabs( polygonUnitNormal_.x_ );
	double ay = fabs( polygonUnitNormal_.y_ );
	double az = fabs( polygonUnitNormal_.z_ );
	Concrete::Coords3D x0( 0, 0, 0 );
	if( ax >= ay && ax >= az )
		{
			x0 = Concrete::Coords3D( m_ / polygonUnitNormal_.x_, 0, 0 );
		}
	else if( ay >= az )
		{
			x0 = Concrete::Coords3D( 0, m_ / polygonUnitNormal_.y_, 0 );
		}
	else
		{
			x0 = Concrete::Coords3D( 0, 0, m_ / polygonUnitNormal_.z_ );
		}
	Concrete::Length Tm = Concrete::inner( Tnormal, x0.transformed( tf ) );

	RefCountPtr< Computation::SingleSidedPolygon3DGray > res =
		RefCountPtr< Computation::SingleSidedPolygon3DGray >
		( new Computation::SingleSidedPolygon3DGray( interpolator_->transformed( tf ),
																								 singleSided_,
																								 Tnormal,
																								 Tm,
																								 tiebreaker_,
																								 viewResolution_,
																								 shadeOrder_ ) );
	{
		typedef Lang::ElementaryPath3D ListType;
		for( ListType::const_iterator i = points_->begin( ); i != points_->end( ); ++i )
			{
				// The creator of *this is responsible for asserting that there are no handles at the pathpoints.
				res->pushPoint( (*i)->mid_->transformed( tf ) );
			}
	}

	zBufPile->push_back( res );
}

void
Lang::SingleSided3DGray::gcMark( Kernel::GCMarkedSet & marked )
{
	const_cast< Lang::ElementaryPath3D * >( points_.getPtr( ) )->gcMark( marked );
	const_cast< Computation::FacetInterpolatorGray * >( interpolator_.getPtr( ) )->gcMark( marked );
}


Lang::SingleSided3DRGB::SingleSided3DRGB( const RefCountPtr< const Lang::ElementaryPath3D > & points,
																						const RefCountPtr< const Computation::FacetInterpolatorRGB > & interpolator,
																						bool singleSided,
																						const Concrete::UnitFloatTriple & polygonUnitNormal,
																						Concrete::Length m,
																						Concrete::Length tiebreaker,
																						Concrete::Length viewResolution,
																						Computation::FacetShadeOrder shadeOrder )
	: points_( points ), interpolator_( interpolator ), singleSided_( singleSided ), polygonUnitNormal_( polygonUnitNormal ), m_( m ), tiebreaker_( tiebreaker ),
		viewResolution_( viewResolution ), shadeOrder_( shadeOrder )
{ }

Lang::SingleSided3DRGB::~SingleSided3DRGB( )
{ }

RefCountPtr< const Lang::Class > Lang::SingleSided3DRGB::TypeID( new Lang::SystemFinalClass( strrefdup( "SingleSided3D(RGB)" ) ) );
TYPEINFOIMPL( SingleSided3DRGB );

RefCountPtr< const Lang::Drawable2D >
Lang::SingleSided3DRGB::typed_to2D( const Kernel::PassedDyn & dyn, const Lang::Transform3D & tf, const RefCountPtr< const Lang::Drawable3D > & self ) const
{
	throw Exceptions::NotImplemented( "SingleSided3DRGB::typed_to2D;	What light scene should be used?" );
}

void
Lang::SingleSided3DRGB::polygonize( std::list< RefCountPtr< Computation::PaintedPolygon3D > > * zBufPile, std::list< RefCountPtr< Computation::StrokedLine3D > > * linePile, const Kernel::PassedDyn & dyn, const Lang::Transform3D & tf, const RefCountPtr< const Lang::Drawable3D > & self ) const
{
	Concrete::UnitFloatTriple Tnormal = tf.transformPlaneUnitNormal( polygonUnitNormal_ );

	double ax = fabs( polygonUnitNormal_.x_ );
	double ay = fabs( polygonUnitNormal_.y_ );
	double az = fabs( polygonUnitNormal_.z_ );
	Concrete::Coords3D x0( 0, 0, 0 );
	if( ax >= ay && ax >= az )
		{
			x0 = Concrete::Coords3D( m_ / polygonUnitNormal_.x_, 0, 0 );
		}
	else if( ay >= az )
		{
			x0 = Concrete::Coords3D( 0, m_ / polygonUnitNormal_.y_, 0 );
		}
	else
		{
			x0 = Concrete::Coords3D( 0, 0, m_ / polygonUnitNormal_.z_ );
		}
	Concrete::Length Tm = Concrete::inner( Tnormal, x0.transformed( tf ) );

	RefCountPtr< Computation::SingleSidedPolygon3DRGB > res =
		RefCountPtr< Computation::SingleSidedPolygon3DRGB >
		( new Computation::SingleSidedPolygon3DRGB( interpolator_->transformed( tf ),
																								singleSided_,
																								Tnormal,
																								Tm,
																								tiebreaker_,
																								viewResolution_,
																								shadeOrder_ ) );
	{
		typedef Lang::ElementaryPath3D ListType;
		for( ListType::const_iterator i = points_->begin( ); i != points_->end( ); ++i )
			{
				// The creator of *this is responsible for asserting that there are no handles at the pathpoints.
				res->pushPoint( (*i)->mid_->transformed( tf ) );
			}
	}

	zBufPile->push_back( res );
}

void
Lang::SingleSided3DRGB::gcMark( Kernel::GCMarkedSet & marked )
{
	const_cast< Lang::ElementaryPath3D * >( points_.getPtr( ) )->gcMark( marked );
	const_cast< Computation::FacetInterpolatorRGB * >( interpolator_.getPtr( ) )->gcMark( marked );
}



Lang::ZBuf::ZBuf( const RefCountPtr< std::list< RefCountPtr< Computation::PaintedPolygon3D > > > & pile, const RefCountPtr< std::list< RefCountPtr< Computation::StrokedLine3D > > > & linePile, const RefCountPtr< std::list< RefCountPtr< const Lang::LightSource > > > & lightPile, const RefCountPtr< const Kernel::GraphicsState > & metaState )
	: metaState_( metaState ), pile_( pile ), linePile_( linePile ), lightPile_( lightPile )
{ }

Lang::ZBuf::~ZBuf( )
{ }

void
Lang::ZBuf::polygonize( std::list< RefCountPtr< Computation::PaintedPolygon3D > > * zBufPile, std::list< RefCountPtr< Computation::StrokedLine3D > > * linePile, const Kernel::PassedDyn & dyn, const Lang::Transform3D & tf, const RefCountPtr< const Lang::Drawable3D > & self ) const
{
	{
		typedef typeof *pile_ ListType;
		for( ListType::const_iterator i = pile_->begin( ); i != pile_->end( ); ++i )
			{
				zBufPile->push_back( *i );
			}
	}
	{
		typedef typeof *linePile_ ListType;
		for( ListType::const_iterator i = linePile_->begin( ); i != linePile_->end( ); ++i )
			{
				linePile->push_back( *i );
			}
	}
}

void
Lang::ZBuf::gcMark( Kernel::GCMarkedSet & marked )
{
	// At the time of writing, PaintedPolygon3D is not involved in gc.
}


Lang::ZSorter::ZSorter( const RefCountPtr< std::list< RefCountPtr< Computation::PaintedPolygon3D > > > & pile, const RefCountPtr< std::list< RefCountPtr< Computation::StrokedLine3D > > > & linePile, const RefCountPtr< std::list< RefCountPtr< const Lang::LightSource > > > & lightPile, const RefCountPtr< const Kernel::GraphicsState > & metaState )
	: metaState_( metaState ), pile_( pile ), linePile_( linePile ), lightPile_( lightPile )
{ }

Lang::ZSorter::~ZSorter( )
{ }

void
Lang::ZSorter::polygonize( std::list< RefCountPtr< Computation::PaintedPolygon3D > > * zBufPile, std::list< RefCountPtr< Computation::StrokedLine3D > > * linePile, const Kernel::PassedDyn & dyn, const Lang::Transform3D & tf, const RefCountPtr< const Lang::Drawable3D > & self ) const
{
	{
		typedef typeof *pile_ ListType;
		for( ListType::const_iterator i = pile_->begin( ); i != pile_->end( ); ++i )
			{
				zBufPile->push_back( *i );
			}
	}
	{
		typedef typeof *linePile_ ListType;
		for( ListType::const_iterator i = linePile_->begin( ); i != linePile_->end( ); ++i )
			{
				linePile->push_back( *i );
			}
	}
}

void
Lang::ZSorter::gcMark( Kernel::GCMarkedSet & marked )
{
	// At the time of writing, PaintedPolygon3D is not involved in gc.
}


Lang::Transformed3D::Transformed3D( RefCountPtr< const Lang::Drawable3D > element, const Lang::Transform3D & mytf )
	: mytf_( mytf ), element_( element )
{ }

Lang::Transformed3D::~Transformed3D( )
{ }

RefCountPtr< const Lang::Drawable2D >
Lang::Transformed3D::typed_to2D( const Kernel::PassedDyn & dyn, const Lang::Transform3D & tf, const RefCountPtr< const Lang::Drawable3D > & self ) const
{
	return element_->typed_to2D( dyn, Lang::Transform3D( tf, mytf_ ), element_ );
}

void
Lang::Transformed3D::polygonize( std::list< RefCountPtr< Computation::PaintedPolygon3D > > * zBufPile, std::list< RefCountPtr< Computation::StrokedLine3D > > * linePile, const Kernel::PassedDyn & dyn, const Lang::Transform3D & tf, const RefCountPtr< const Lang::Drawable3D > & self ) const
{
	element_->polygonize( zBufPile, linePile, dyn, Lang::Transform3D( tf, mytf_ ), element_ );
}

void
Lang::Transformed3D::findTags( std::vector< Kernel::ValueRef > * dst, const Kernel::PassedDyn & dyn, Lang::Symbol::KeyType key, const Lang::Transform3D & tf ) const
{
	element_->findTags( dst, dyn, key, Lang::Transform3D( tf, mytf_ ) );
}

bool
Lang::Transformed3D::findOneTag( Kernel::EvalState * evalState, Lang::Symbol::KeyType key, const Lang::Transform3D & tf ) const
{
	return
		element_->findOneTag( evalState, key, Lang::Transform3D( tf, mytf_ ) );
}

void
Lang::Transformed3D::gcMark( Kernel::GCMarkedSet & marked )
{
	const_cast< Lang::Drawable3D * >( element_.getPtr( ) )->gcMark( marked );
}

Lang::Text::Text( const RefCountPtr< const Kernel::GraphicsState > & metaState, const RefCountPtr< const Kernel::TextState > & textState, const RefCountPtr< const std::list< RefCountPtr< const Lang::TextOperation > > > & elements, const RefCountPtr< const Lang::ElementaryPath2D > & mybbox )
	: metaState_( metaState ), textState_( textState ), elements_( elements ), mybbox_( mybbox )
{ }

Lang::Text::~Text( )
{ }

void
Lang::Text::shipout( std::ostream & os, Kernel::PageContentStates * pdfState, const Lang::Transform2D & tf ) const
{
	pdfState->resources_->requireProcedureSet( SimplePDF::PDF_Resources::PROC_SET_TEXT );

	pdfState->graphics_.synchForNonStroke( os, metaState_.getPtr( ), pdfState->resources_.getPtr( ) );
	os << "BT" << std::endl ;
	if( ! tf.isIdentity( ) )
		{
			tf.shipout( os );
			os << " Tm" << std::endl ;
		}

	pdfState->text_.synchKnockout( os, textState_.getPtr( ), pdfState->resources_.getPtr( ) );

	typedef typeof *elements_ ListType;
	for( ListType::const_iterator i = elements_->begin( ); i != elements_->end( ); ++i )
		{
			(*i)->shipout( os, pdfState, tf );
		}
	os << "ET" << std::endl ;
}

RefCountPtr< const Lang::ElementaryPath2D >
Lang::Text::bbox( ) const
{
	return mybbox_;
}

void
Lang::Text::gcMark( Kernel::GCMarkedSet & marked )
{
	typedef typeof *elements_ ListType;
	for( ListType::const_iterator i = elements_->begin( ); i != elements_->end( ); ++i )
		{
			const_cast< Lang::TextOperation * >( i->getPtr( ) )->gcMark( marked );
		}
}




RefCountPtr< const Lang::TransparencyGroup >
Helpers::newSolidTransparencyGroup( const RefCountPtr< const Lang::Drawable2D > & obj2, const RefCountPtr< const Lang::Drawable2D > & obj1 )
{
	return Helpers::newTransparencyGroup( Helpers::newGroup2D( Kernel::THE_DEFAULT_STATE, obj2, obj1 ),
																				true,
																				true,
																				Lang::THE_INHERITED_COLOR_SPACE );
}

RefCountPtr< const Lang::TransparencyGroup >
Helpers::newSolidTransparencyGroup( const RefCountPtr< const Lang::Drawable2D > & obj3, const RefCountPtr< const Lang::Drawable2D > & obj2, const RefCountPtr< const Lang::Drawable2D > & obj1 )
{
	return Helpers::newTransparencyGroup( Helpers::newGroup2D( Kernel::THE_DEFAULT_STATE, obj3, obj2, obj1 ),
																				true,
																				true,
																				Lang::THE_INHERITED_COLOR_SPACE );
}


bool
Computation::operator < ( const Computation::UndirectedEdge & p1, const Computation::UndirectedEdge & p2 )
{
	if( p1.low( ) < p2.low( ) )
		{
			return true;
		}
	if( p1.low( ) > p2.low( ) )
		{
			return false;
		}
	return p1.high( ) < p2.high( );
}
