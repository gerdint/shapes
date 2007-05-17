#ifndef metapdfcore_h
#define metapdfcore_h

#include <list>
#include <iostream>
#include <string>

#include "refcount.h"
#include "metapdftypes.h"


namespace MetaPDF
{
  namespace Lang
  {

  class CoreFunction : public Lang::Function
  {
  protected:
    const char * title_;
  public:
    CoreFunction( const char * title );
    CoreFunction( const char * title, Kernel::EvaluatedFormals * formals );
    virtual bool isTransforming( ) const;
    const char * getTitle( ) const;
    virtual void gcMark( Kernel::GCMarkedSet & marked ){ };
  };

  class NullFunction : public Lang::CoreFunction
  {
  public:
    NullFunction( const char * title ) : CoreFunction( title ) { }
    virtual void call( Kernel::EvalState * evalState, Kernel::Arguments & args, const Ast::SourceLocation & callLoc ) const;
  };

  class Core_identity : public Lang::CoreFunction
  {
  public:
    Core_identity( const char * title ) : CoreFunction( title ) { }
    virtual void call( Kernel::EvalState * evalState, Kernel::Arguments & args, const Ast::SourceLocation & callLoc ) const;
  };

  class Core_typeof : public Lang::CoreFunction
  {
  public:
    Core_typeof( const char * title ) : CoreFunction( title ) { }
    virtual void call( Kernel::EvalState * evalState, Kernel::Arguments & args, const Ast::SourceLocation & callLoc ) const;
  };

  class Core_gensym : public Lang::CoreFunction
  {
  public:
    Core_gensym( const char * title ) : CoreFunction( title ) { }
    virtual void call( Kernel::EvalState * evalState, Kernel::Arguments & args, const Ast::SourceLocation & callLoc ) const;
  };

  class Core_tag : public Lang::CoreFunction
  {
  public:
    Core_tag( const char * title );
    virtual void call( Kernel::EvalState * evalState, Kernel::Arguments & args, const Ast::SourceLocation & callLoc ) const;
  };

  class Core_find : public Lang::CoreFunction
  {
  public:
    Core_find( const char * title );
    virtual void call( Kernel::EvalState * evalState, Kernel::Arguments & args, const Ast::SourceLocation & callLoc ) const;
  };

  class Core_findall : public Lang::CoreFunction
  {
  public:
    Core_findall( const char * title );
    virtual void call( Kernel::EvalState * evalState, Kernel::Arguments & args, const Ast::SourceLocation & callLoc ) const;
  };

  class Core_error : public Lang::CoreFunction
  {
  public:
    Core_error( const char * title ) : CoreFunction( title ) { }
    virtual void call( Kernel::EvalState * evalState, Kernel::Arguments & args, const Ast::SourceLocation & callLoc ) const;
  };

  class Core_show : public Lang::CoreFunction
  {
  public:
    Core_show( const char * title ) : CoreFunction( title ) { }
    virtual void call( Kernel::EvalState * evalState, Kernel::Arguments & args, const Ast::SourceLocation & callLoc ) const;
  };

  class Core_if : public Lang::CoreFunction
  {
  public:
    Core_if( const char * title );
    virtual void call( Kernel::EvalState * evalState, Kernel::Arguments & args, const Ast::SourceLocation & callLoc ) const;
  };

  class Core_and : public Lang::CoreFunction
  {
  public:
    Core_and( const char * title );
    virtual void call( Kernel::EvalState * evalState, Kernel::Arguments & args, const Ast::SourceLocation & callLoc ) const;
  };

  class Core_or : public Lang::CoreFunction
  {
  public:
    Core_or( const char * title );
    virtual void call( Kernel::EvalState * evalState, Kernel::Arguments & args, const Ast::SourceLocation & callLoc ) const;
  };

  class Core_memoryinfo : public Lang::CoreFunction
  {
  public:
    Core_memoryinfo( const char * title ) : CoreFunction( title ) { }
    virtual void call( Kernel::EvalState * evalState, Kernel::Arguments & args, const Ast::SourceLocation & callLoc ) const;
  };

  class Core_rectangle : public Lang::CoreFunction
  {
  public:
    Core_rectangle( const char * title ) : CoreFunction( title ) { }
    virtual void call( Kernel::EvalState * evalState, Kernel::Arguments & args, const Ast::SourceLocation & callLoc ) const;
  };

  class Core_stroke : public Lang::CoreFunction
  {
  public:
    Core_stroke( const char * title );
    virtual void call( Kernel::EvalState * evalState, Kernel::Arguments & args, const Ast::SourceLocation & callLoc ) const;
  };

  class Core_fill : public Lang::CoreFunction
  {
  public:
    Core_fill( const char * title );
    virtual void call( Kernel::EvalState * evalState, Kernel::Arguments & args, const Ast::SourceLocation & callLoc ) const;
  };

  class Core_fillstar : public Lang::CoreFunction
  {
  public:
    Core_fillstar( const char * title ) : CoreFunction( title ) { }
    virtual void call( Kernel::EvalState * evalState, Kernel::Arguments & args, const Ast::SourceLocation & callLoc ) const;
  };

  class Core_phong : public Lang::CoreFunction
  {
  public:
    Core_phong( const char * title ) : CoreFunction( title ) { }
    virtual void call( Kernel::EvalState * evalState, Kernel::Arguments & args, const Ast::SourceLocation & callLoc ) const;
  };

  class Core_facetnormal : public Lang::CoreFunction
  {
  public:
    Core_facetnormal( const char * title );
    virtual void call( Kernel::EvalState * evalState, Kernel::Arguments & args, const Ast::SourceLocation & callLoc ) const;
  };

  class Core_facet : public Lang::CoreFunction
  {
  public:
    Core_facet( const char * title );
    virtual void call( Kernel::EvalState * evalState, Kernel::Arguments & args, const Ast::SourceLocation & callLoc ) const;
  };

  class Core_dashpattern : public Lang::CoreFunction
  {
  public:
    Core_dashpattern( const char * title ) : CoreFunction( title ) { }
    virtual void call( Kernel::EvalState * evalState, Kernel::Arguments & args, const Ast::SourceLocation & callLoc ) const;
  };

  class Core_from3Dto2D : public Lang::CoreFunction
  {
  public:
    Core_from3Dto2D( const char * title ) : CoreFunction( title ) { }
    virtual void call( Kernel::EvalState * evalState, Kernel::Arguments & args, const Ast::SourceLocation & callLoc ) const;
  };

  class Core_from2Dto3D : public Lang::CoreFunction
  {
  public:
    Core_from2Dto3D( const char * title ) : CoreFunction( title ) { }
    virtual void call( Kernel::EvalState * evalState, Kernel::Arguments & args, const Ast::SourceLocation & callLoc ) const;
  };

  class Core_facing2Din3D : public Lang::CoreFunction
  {
  public:
    Core_facing2Din3D( const char * title );
    virtual void call( Kernel::EvalState * evalState, Kernel::Arguments & args, const Ast::SourceLocation & callLoc ) const;
  };

  class Core_noArrow : public Lang::CoreFunction
  {
  public:
    Core_noArrow( const char * title ) : CoreFunction( title ) { }
    virtual void call( Kernel::EvalState * evalState, Kernel::Arguments & args, const Ast::SourceLocation & callLoc ) const;
  };

  class Core_gray : public Lang::CoreFunction
  {
  public:
    Core_gray( const char * title ) : CoreFunction( title ) { }
    virtual void call( Kernel::EvalState * evalState, Kernel::Arguments & args, const Ast::SourceLocation & callLoc ) const;
  };

  class Core_rgb : public Lang::CoreFunction
  {
  public:
    Core_rgb( const char * title ) : CoreFunction( title ) { }
    virtual void call( Kernel::EvalState * evalState, Kernel::Arguments & args, const Ast::SourceLocation & callLoc ) const;
  };

  class Core_cmyk : public Lang::CoreFunction
  {
  public:
    Core_cmyk( const char * title ) : CoreFunction( title ) { }
    virtual void call( Kernel::EvalState * evalState, Kernel::Arguments & args, const Ast::SourceLocation & callLoc ) const;
  };

  class Core_shape : public Lang::CoreFunction
  {
  public:
    Core_shape( const char * title ) : CoreFunction( title ) { }
    virtual void call( Kernel::EvalState * evalState, Kernel::Arguments & args, const Ast::SourceLocation & callLoc ) const;
  };

  class Core_opacity : public Lang::CoreFunction
  {
  public:
    Core_opacity( const char * title ) : CoreFunction( title ) { }
    virtual void call( Kernel::EvalState * evalState, Kernel::Arguments & args, const Ast::SourceLocation & callLoc ) const;
  };

  class Core_alphamask : public Lang::CoreFunction
  {
  public:
    Core_alphamask( const char * title );
    virtual void call( Kernel::EvalState * evalState, Kernel::Arguments & args, const Ast::SourceLocation & callLoc ) const;
  };

  class Core_luminositymask : public Lang::CoreFunction
  {
  public:
    Core_luminositymask( const char * title );
    virtual void call( Kernel::EvalState * evalState, Kernel::Arguments & args, const Ast::SourceLocation & callLoc ) const;
  };

  class Core_mod : public Lang::CoreFunction
  {
  public:
    Core_mod( const char * title ) : CoreFunction( title ) { }
    virtual void call( Kernel::EvalState * evalState, Kernel::Arguments & args, const Ast::SourceLocation & callLoc ) const;
  };

  class Core_floor : public Lang::CoreFunction
  {
  public:
    Core_floor( const char * title ) : CoreFunction( title ) { }
    virtual void call( Kernel::EvalState * evalState, Kernel::Arguments & args, const Ast::SourceLocation & callLoc ) const;
  };

  class Core_ceil : public Lang::CoreFunction
  {
  public:
    Core_ceil( const char * title ) : CoreFunction( title ) { }
    virtual void call( Kernel::EvalState * evalState, Kernel::Arguments & args, const Ast::SourceLocation & callLoc ) const;
  };

  class Core_rint : public Lang::CoreFunction
  {
  public:
    Core_rint( const char * title ) : CoreFunction( title ) { }
    virtual void call( Kernel::EvalState * evalState, Kernel::Arguments & args, const Ast::SourceLocation & callLoc ) const;
  };

  class Core_cos : public Lang::CoreFunction
  {
  public:
    Core_cos( const char * title ) : CoreFunction( title ) { }
    virtual void call( Kernel::EvalState * evalState, Kernel::Arguments & args, const Ast::SourceLocation & callLoc ) const;
  };

  class Core_sin : public Lang::CoreFunction
  {
  public:
    Core_sin( const char * title ) : CoreFunction( title ) { }
    virtual void call( Kernel::EvalState * evalState, Kernel::Arguments & args, const Ast::SourceLocation & callLoc ) const;
  };

  class Core_tan : public Lang::CoreFunction
  {
  public:
    Core_tan( const char * title ) : CoreFunction( title ) { }
    virtual void call( Kernel::EvalState * evalState, Kernel::Arguments & args, const Ast::SourceLocation & callLoc ) const;
  };

  class Core_cot : public Lang::CoreFunction
  {
  public:
    Core_cot( const char * title ) : CoreFunction( title ) { }
    virtual void call( Kernel::EvalState * evalState, Kernel::Arguments & args, const Ast::SourceLocation & callLoc ) const;
  };

  class Core_arccos : public Lang::CoreFunction
  {
  public:
    Core_arccos( const char * title ) : CoreFunction( title ) { }
    virtual void call( Kernel::EvalState * evalState, Kernel::Arguments & args, const Ast::SourceLocation & callLoc ) const;
  };

  class Core_arcsin : public Lang::CoreFunction
  {
  public:
    Core_arcsin( const char * title ) : CoreFunction( title ) { }
    virtual void call( Kernel::EvalState * evalState, Kernel::Arguments & args, const Ast::SourceLocation & callLoc ) const;
  };

  class Core_arctan : public Lang::CoreFunction
  {
  public:
    Core_arctan( const char * title ) : CoreFunction( title ) { }
    virtual void call( Kernel::EvalState * evalState, Kernel::Arguments & args, const Ast::SourceLocation & callLoc ) const;
  };

  class Core_min : public Lang::CoreFunction
  {
  public:
    Core_min( const char * title ) : CoreFunction( title ) { }
    virtual void call( Kernel::EvalState * evalState, Kernel::Arguments & args, const Ast::SourceLocation & callLoc ) const;
  };

  class Core_max : public Lang::CoreFunction
  {
  public:
    Core_max( const char * title ) : CoreFunction( title ) { }
    virtual void call( Kernel::EvalState * evalState, Kernel::Arguments & args, const Ast::SourceLocation & callLoc ) const;
  };

  class Core_sqrt : public Lang::CoreFunction
  {
  public:
    Core_sqrt( const char * title ) : CoreFunction( title ) { }
    virtual void call( Kernel::EvalState * evalState, Kernel::Arguments & args, const Ast::SourceLocation & callLoc ) const;
  };

  class Core_angle : public Lang::CoreFunction
  {
  public:
    Core_angle( const char * title ) : CoreFunction( title ) { }
    virtual void call( Kernel::EvalState * evalState, Kernel::Arguments & args, const Ast::SourceLocation & callLoc ) const;
  };

  class Core_dir : public Lang::CoreFunction
  {
  public:
    Core_dir( const char * title ) : CoreFunction( title ) { }
    virtual void call( Kernel::EvalState * evalState, Kernel::Arguments & args, const Ast::SourceLocation & callLoc ) const;
  };

  class Core_abs : public Lang::CoreFunction
  {
  public:
    Core_abs( const char * title ) : CoreFunction( title ) { }
    virtual void call( Kernel::EvalState * evalState, Kernel::Arguments & args, const Ast::SourceLocation & callLoc ) const;
  };

  class Core_normalized : public Lang::CoreFunction
  {
  public:
    Core_normalized( const char * title ) : CoreFunction( title ) { }
    virtual void call( Kernel::EvalState * evalState, Kernel::Arguments & args, const Ast::SourceLocation & callLoc ) const;
  };

  class Core_cross : public Lang::CoreFunction
  {
  public:
    Core_cross( const char * title ) : CoreFunction( title ) { }
    virtual void call( Kernel::EvalState * evalState, Kernel::Arguments & args, const Ast::SourceLocation & callLoc ) const;
  };

  class Core_orthogonal : public Lang::CoreFunction
  {
  public:
    Core_orthogonal( const char * title ) : CoreFunction( title ) { }
    virtual void call( Kernel::EvalState * evalState, Kernel::Arguments & args, const Ast::SourceLocation & callLoc ) const;
  };

  class Core_randomNatural : public Lang::CoreFunction
  {
  public:
    Core_randomNatural( const char * title );
    virtual void call( Kernel::EvalState * evalState, Kernel::Arguments & args, const Ast::SourceLocation & callLoc ) const;
  };

  class Core_randomBall1D : public Lang::CoreFunction
  {
  public:
    Core_randomBall1D( const char * title );
    virtual void call( Kernel::EvalState * evalState, Kernel::Arguments & args, const Ast::SourceLocation & callLoc ) const;
  };

  class Core_randomBall2D : public Lang::CoreFunction
  {
  public:
    Core_randomBall2D( const char * title );
    virtual void call( Kernel::EvalState * evalState, Kernel::Arguments & args, const Ast::SourceLocation & callLoc ) const;
  };

  class Core_randomBall3D : public Lang::CoreFunction
  {
  public:
    Core_randomBall3D( const char * title );
    virtual void call( Kernel::EvalState * evalState, Kernel::Arguments & args, const Ast::SourceLocation & callLoc ) const;
  };

  class Core_coords2D : public Lang::CoreFunction
  {
  public:
    Core_coords2D( const char * title ) : CoreFunction( title ) { }
    virtual void call( Kernel::EvalState * evalState, Kernel::Arguments & args, const Ast::SourceLocation & callLoc ) const;
  };

  class Core_cornercoords2D : public Lang::CoreFunction
  {
  public:
    Core_cornercoords2D( const char * title ) : CoreFunction( title ) { }
    virtual void call( Kernel::EvalState * evalState, Kernel::Arguments & args, const Ast::SourceLocation & callLoc ) const;
  };

  class Core_coords3D : public Lang::CoreFunction
  {
  public:
    Core_coords3D( const char * title ) : CoreFunction( title ) { }
    virtual void call( Kernel::EvalState * evalState, Kernel::Arguments & args, const Ast::SourceLocation & callLoc ) const;
  };

  class Core_polarHandle2DFree_r : public Lang::CoreFunction
  {
  public:
    Core_polarHandle2DFree_r( const char * title ) : CoreFunction( title ) { }
    virtual void call( Kernel::EvalState * evalState, Kernel::Arguments & args, const Ast::SourceLocation & callLoc ) const;
  };

  class Core_polarHandle2DFree_ra : public Lang::CoreFunction
  {
  public:
    Core_polarHandle2DFree_ra( const char * title ) : CoreFunction( title ) { }
    virtual void call( Kernel::EvalState * evalState, Kernel::Arguments & args, const Ast::SourceLocation & callLoc ) const;
  };

  class Core_TeX : public Lang::CoreFunction
  {
  public:
    Core_TeX( const char * title ) : CoreFunction( title ) { }
    virtual void call( Kernel::EvalState * evalState, Kernel::Arguments & args, const Ast::SourceLocation & callLoc ) const;
  };

  class Core_cons : public Lang::CoreFunction
  {
  public:
    Core_cons( const char * title );
    virtual void call( Kernel::EvalState * evalState, Kernel::Arguments & args, const Ast::SourceLocation & callLoc ) const;
  };

  class Core_list : public Lang::CoreFunction
  {
  public:
    Core_list( const char * title ) : CoreFunction( title ) { }
    virtual void call( Kernel::EvalState * evalState, Kernel::Arguments & args, const Ast::SourceLocation & callLoc ) const;
  };

  class Core_isnull : public Lang::CoreFunction
  {
  public:
    Core_isnull( const char * title ) : CoreFunction( title ) { }
    virtual void call( Kernel::EvalState * evalState, Kernel::Arguments & args, const Ast::SourceLocation & callLoc ) const;
  };

  class Core_range : public Lang::CoreFunction
  {
  public:
    Core_range( const char * title );
    virtual void call( Kernel::EvalState * evalState, Kernel::Arguments & args, const Ast::SourceLocation & callLoc ) const;
  };

  class Core_affinetransform : public Lang::CoreFunction
  {
  public:
    Core_affinetransform( const char * title ) : CoreFunction( title ) { }
    virtual void call( Kernel::EvalState * evalState, Kernel::Arguments & args, const Ast::SourceLocation & callLoc ) const;
  };

  class Core_shift : public Lang::CoreFunction
  {
  public:
    Core_shift( const char * title ) : CoreFunction( title ) { }
    virtual void call( Kernel::EvalState * evalState, Kernel::Arguments & args, const Ast::SourceLocation & callLoc ) const;
  };

  class Core_rotate : public Lang::CoreFunction
  {
  public:
    Core_rotate( const char * title );
    virtual void call( Kernel::EvalState * evalState, Kernel::Arguments & args, const Ast::SourceLocation & callLoc ) const;
  };

  class Core_rotate3d : public Lang::CoreFunction
  {
  public:
    Core_rotate3d( const char * title );
    virtual void call( Kernel::EvalState * evalState, Kernel::Arguments & args, const Ast::SourceLocation & callLoc ) const;
  };

  class Core_scale : public Lang::CoreFunction
  {
  public:
    Core_scale( const char * title );
    virtual void call( Kernel::EvalState * evalState, Kernel::Arguments & args, const Ast::SourceLocation & callLoc ) const;
  };

  class Core_scale3d : public Lang::CoreFunction
  {
  public:
    Core_scale3d( const char * title );
    virtual void call( Kernel::EvalState * evalState, Kernel::Arguments & args, const Ast::SourceLocation & callLoc ) const;
  };

  class Core_affinetransform3D : public Lang::CoreFunction
  {
  public:
    Core_affinetransform3D( const char * title ) : CoreFunction( title ) { }
    virtual void call( Kernel::EvalState * evalState, Kernel::Arguments & args, const Ast::SourceLocation & callLoc ) const;
  };

  class Core_inverse : public Lang::CoreFunction
  {
  public:
    Core_inverse( const char * title ) : CoreFunction( title ) { }
    virtual void call( Kernel::EvalState * evalState, Kernel::Arguments & args, const Ast::SourceLocation & callLoc ) const;
  };

  class Core_bbox : public Lang::CoreFunction
  {
  public:
    Core_bbox( const char * title ) : CoreFunction( title ) { }
    virtual void call( Kernel::EvalState * evalState, Kernel::Arguments & args, const Ast::SourceLocation & callLoc ) const;
  };
  
  class Core_bboxed : public Lang::CoreFunction
  {
  public:
    Core_bboxed( const char * title ) : CoreFunction( title ) { }
      virtual void call( Kernel::EvalState * evalState, Kernel::Arguments & args, const Ast::SourceLocation & callLoc ) const;
  };

  class Core_clip : public Lang::CoreFunction
  {
    const char * clipCommand_;
  public:
    Core_clip( const char * title, const char * clipCommand ) : CoreFunction( title ), clipCommand_( clipCommand ) { }
    virtual void call( Kernel::EvalState * evalState, Kernel::Arguments & args, const Ast::SourceLocation & callLoc ) const;
  };

  class Core_controlling : public Lang::CoreFunction
  {
  public:
    Core_controlling( const char * title ) : CoreFunction( title ) { }
    virtual void call( Kernel::EvalState * evalState, Kernel::Arguments & args, const Ast::SourceLocation & callLoc ) const;
  };

  class Core_formxo : public Lang::CoreFunction
  {
  public:
    Core_formxo( const char * title ) : CoreFunction( title ) { }
    virtual void call( Kernel::EvalState * evalState, Kernel::Arguments & args, const Ast::SourceLocation & callLoc ) const;
  };

  class Core_transparencygroup : public Lang::CoreFunction
  {
  public:
    Core_transparencygroup( const char * title );
    virtual void call( Kernel::EvalState * evalState, Kernel::Arguments & args, const Ast::SourceLocation & callLoc ) const;
  };

  class Core_vector : public Lang::CoreFunction
  {
  public:
    Core_vector( const char * title ) : CoreFunction( title ) { }
    virtual void call( Kernel::EvalState * evalState, Kernel::Arguments & args, const Ast::SourceLocation & callLoc ) const;
  };

  class Core_interpolate : public Lang::CoreFunction
  {
  public:
    Core_interpolate( const char * title );
    virtual void call( Kernel::EvalState * evalState, Kernel::Arguments & args, const Ast::SourceLocation & callLoc ) const;
  };

  class Core_importPDFpages : public Lang::CoreFunction
  {
  public:
    Core_importPDFpages( const char * title ) : CoreFunction( title ) { }
    virtual void call( Kernel::EvalState * evalState, Kernel::Arguments & args, const Ast::SourceLocation & callLoc ) const;
  };

  class Core_ambient_light : public Lang::CoreFunction
  {
  public:
    Core_ambient_light( const char * title );
    virtual void call( Kernel::EvalState * evalState, Kernel::Arguments & args, const Ast::SourceLocation & callLoc ) const;
  };

  class Core_specular_light : public Lang::CoreFunction
  {
  public:
    Core_specular_light( const char * title );
    virtual void call( Kernel::EvalState * evalState, Kernel::Arguments & args, const Ast::SourceLocation & callLoc ) const;
  };

  class Core_distant_light : public Lang::CoreFunction
  {
  public:
    Core_distant_light( const char * title );
    virtual void call( Kernel::EvalState * evalState, Kernel::Arguments & args, const Ast::SourceLocation & callLoc ) const;
  };

  class Core_duration : public Lang::CoreFunction
  {
  public:
    Core_duration( const char * title ) : CoreFunction( title ) { }
    virtual void call( Kernel::EvalState * evalState, Kernel::Arguments & args, const Ast::SourceLocation & callLoc ) const;
  };

  class Core_controlling_maximizer : public Lang::CoreFunction
  {
  public:
    Core_controlling_maximizer( const char * title ) : CoreFunction( title ) { }
    virtual void call( Kernel::EvalState * evalState, Kernel::Arguments & args, const Ast::SourceLocation & callLoc ) const;
  };

  class Core_discrete_mean : public Lang::CoreFunction
  {
  public:
    Core_discrete_mean( const char * title ) : CoreFunction( title ) { }
    virtual void call( Kernel::EvalState * evalState, Kernel::Arguments & args, const Ast::SourceLocation & callLoc ) const;
  };

  class Core_discrete_maximizer : public Lang::CoreFunction
  {
  public:
    Core_discrete_maximizer( const char * title ) : CoreFunction( title ) { }
    virtual void call( Kernel::EvalState * evalState, Kernel::Arguments & args, const Ast::SourceLocation & callLoc ) const;
  };

  class Core_discrete_approximator : public Lang::CoreFunction
  {
  public:
    Core_discrete_approximator( const char * title ) : CoreFunction( title ) { }
    virtual void call( Kernel::EvalState * evalState, Kernel::Arguments & args, const Ast::SourceLocation & callLoc ) const;
  };

  class Core_continuous_mean : public Lang::CoreFunction
  {
  public:
    Core_continuous_mean( const char * title ) : CoreFunction( title ) { }
    virtual void call( Kernel::EvalState * evalState, Kernel::Arguments & args, const Ast::SourceLocation & callLoc ) const;
  };

  class Core_continuous_maximizer : public Lang::CoreFunction
  {
  public:
    Core_continuous_maximizer( const char * title ) : CoreFunction( title ) { }
    virtual void call( Kernel::EvalState * evalState, Kernel::Arguments & args, const Ast::SourceLocation & callLoc ) const;
  };

  class Core_continuous_approximator : public Lang::CoreFunction
  {
  public:
    Core_continuous_approximator( const char * title ) : CoreFunction( title ) { }
    virtual void call( Kernel::EvalState * evalState, Kernel::Arguments & args, const Ast::SourceLocation & callLoc ) const;
  };

  class Core_controlling_hull : public Lang::CoreFunction
  {
  public:
    Core_controlling_hull( const char * title ) : CoreFunction( title ) { }
    virtual void call( Kernel::EvalState * evalState, Kernel::Arguments & args, const Ast::SourceLocation & callLoc ) const;
  };

  class Core_subpath : public Lang::CoreFunction
  {
  public:
    Core_subpath( const char * title ) : CoreFunction( title ) { }
    virtual void call( Kernel::EvalState * evalState, Kernel::Arguments & args, const Ast::SourceLocation & callLoc ) const;
  };

  class Core_reverse : public Lang::CoreFunction
  {
  public:
    Core_reverse( const char * title ) : CoreFunction( title ) { }
    virtual void call( Kernel::EvalState * evalState, Kernel::Arguments & args, const Ast::SourceLocation & callLoc ) const;
  };

  class Core_meetpaths : public Lang::CoreFunction
  {
  public:
    Core_meetpaths( const char * title ) : CoreFunction( title ) { }
    virtual void call( Kernel::EvalState * evalState, Kernel::Arguments & args, const Ast::SourceLocation & callLoc ) const;
  };

  class Core_intersection : public Lang::CoreFunction
  {
    mutable Kernel::Environment::LexicalKey * idKey;
  public:
    Core_intersection( const char * title ) : CoreFunction( title ), idKey( 0 ) { }
    virtual ~Core_intersection( ) { if( idKey != 0 ){ delete idKey; } }
    virtual void call( Kernel::EvalState * evalState, Kernel::Arguments & args, const Ast::SourceLocation & callLoc ) const;
  };

  class Core_sprintf : public Lang::CoreFunction
  {
  public:
    Core_sprintf( const char * title ) : CoreFunction( title ) { }
    virtual void call( Kernel::EvalState * evalState, Kernel::Arguments & args, const Ast::SourceLocation & callLoc ) const;
  };

  class Core_strftime : public Lang::CoreFunction
  {
  public:
    Core_strftime( const char * title ) : CoreFunction( title ) { }
    virtual void call( Kernel::EvalState * evalState, Kernel::Arguments & args, const Ast::SourceLocation & callLoc ) const;
  };

  class Core_hot : public Lang::CoreFunction
  {
  public:
    Core_hot( const char * title );
    virtual void call( Kernel::EvalState * evalState, Kernel::Arguments & args, const Ast::SourceLocation & callLoc ) const;
  };

  class Core_textrenderingmode : public Lang::CoreFunction
  {
  public:
    Core_textrenderingmode( const char * title );
    virtual void call( Kernel::EvalState * evalState, Kernel::Arguments & args, const Ast::SourceLocation & callLoc ) const;
  };

    class Core_manualkern : public Lang::CoreFunction
    {
    public:
      Core_manualkern( const char * title ) : CoreFunction( title ) { }
      virtual void call( Kernel::EvalState * evalState, Kernel::Arguments & args, const Ast::SourceLocation & callLoc ) const;
    };

    class Core_automatickern : public Lang::CoreFunction
    {
    public:
      Core_automatickern( const char * title ) : CoreFunction( title ) { }
      virtual void call( Kernel::EvalState * evalState, Kernel::Arguments & args, const Ast::SourceLocation & callLoc ) const;
    };

    class Core_makeglyph : public Lang::CoreFunction
    {
      Lang::Type3Glyph::Kind kind_;
    public:
      Core_makeglyph( const char * title, Lang::Type3Glyph::Kind kind );
      virtual void call( Kernel::EvalState * evalState, Kernel::Arguments & args, const Ast::SourceLocation & callLoc ) const;
    };

    class Core_newrandom : public Lang::CoreFunction
    {
    public:
      Core_newrandom( const char * title );
      virtual void call( Kernel::EvalState * evalState, Kernel::Arguments & args, const Ast::SourceLocation & callLoc ) const;
    };

    class Core_devrandom : public Lang::CoreFunction
    {
    public:
      Core_devrandom( const char * title );
      virtual void call( Kernel::EvalState * evalState, Kernel::Arguments & args, const Ast::SourceLocation & callLoc ) const;
    };

  }
}

#endif

