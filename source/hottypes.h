#ifndef hottypes_h
#define hottypes_h

#include "MetaPDF_Ast_decls.h"
#include "MetaPDF_Lang_decls.h"
#include "FontMetrics_decls.h"

#include "functiontypes.h"
#include "drawabletypes.h"
#include "lighttypes.h"
#include "fonttypes.h"
#include "charptrless.h"

#include <iostream>
#include <sys/time.h>


namespace MetaPDF
{
  namespace Kernel
  {
    class Warm
    {
    public:
      virtual ~Warm( );
      virtual void tackOn( Kernel::EvalState * evalState, const RefCountPtr< const Lang::Value > & piece, const Kernel::PassedDyn & dyn, const Ast::SourceLocation & callLoc ) = 0;
      virtual void freeze( Kernel::EvalState * evalState, const Ast::SourceLocation & callLoc ) = 0;
    };
  }

  namespace Lang
  {

    class Hot : public Lang::NoOperatorOverloadValue
    {
    public:
      Hot( );
      virtual ~Hot( );
      virtual Kernel::Warm * warm( ) const = 0;
      TYPEINFODECL;
    };
    
    
    class HotTriple : public Lang::Hot
    {
      RefCountPtr< const Lang::Value > init_;
      RefCountPtr< const Lang::Function > update_;
      RefCountPtr< const Lang::Function > result_;
    public:
      HotTriple( const RefCountPtr< const Lang::Value > & init, RefCountPtr< const Lang::Function > update, RefCountPtr< const Lang::Function > result );
      virtual ~HotTriple( );
      virtual Kernel::Warm * warm( ) const;
      virtual void gcMark( Kernel::GCMarkedSet & marked );
    };
    
    /* This template can be used to create hot types such as 2D and 3D, where the warm value can be created using a default constructor
     */
    template< class W >
    class HotDefault : public Lang::Hot
    {
    public:
      HotDefault( ) { }
      virtual ~HotDefault( ) { }
      virtual Kernel::Warm * warm( ) const { return new W; }
      virtual void gcMark( Kernel::GCMarkedSet & marked ){ }
    };
    
  }

  namespace Kernel
  {

    class WarmTriple : public Kernel::Warm
    {
      RefCountPtr< const Lang::Value > pile_;
      RefCountPtr< const Lang::Function > update_;
      RefCountPtr< const Lang::Function > result_;
    public:
      WarmTriple( const RefCountPtr< const Lang::Value > & pile, RefCountPtr< const Lang::Function > update, RefCountPtr< const Lang::Function > result );
      virtual ~WarmTriple( );
      virtual void tackOn( Kernel::EvalState * evalState, const RefCountPtr< const Lang::Value > & piece, const Kernel::PassedDyn & dyn, const Ast::SourceLocation & callLoc );
      virtual void freeze( Kernel::EvalState * evalState, const Ast::SourceLocation & callLoc );
    };
    
    class WarmOstream : public Kernel::Warm
    {
      std::ostream & os_;
    public:
      WarmOstream( std::ostream & os );
      virtual ~WarmOstream( );
      virtual void tackOn( Kernel::EvalState * evalState, const RefCountPtr< const Lang::Value > & piece, const Kernel::PassedDyn & dyn, const Ast::SourceLocation & callLoc );
      virtual void freeze( Kernel::EvalState * evalState, const Ast::SourceLocation & callLoc );
    };
    
    class Warm_ostringstream : public Kernel::Warm
    {
      std::ostringstream os_;
    public:
      Warm_ostringstream( );
      virtual ~Warm_ostringstream( );
      virtual void tackOn( Kernel::EvalState * evalState, const RefCountPtr< const Lang::Value > & piece, const Kernel::PassedDyn & dyn, const Ast::SourceLocation & callLoc );
      virtual void freeze( Kernel::EvalState * evalState, const Ast::SourceLocation & callLoc );
    };
    
    class WarmGroup2D : public Kernel::Warm
    {
      RefCountPtr< const Lang::Group2D > pile_;
    public:
      WarmGroup2D( );
      virtual ~WarmGroup2D( );
      virtual void tackOn( Kernel::EvalState * evalState, const RefCountPtr< const Lang::Value > & piece, const Kernel::PassedDyn & dyn, const Ast::SourceLocation & callLoc );
      virtual void freeze( Kernel::EvalState * evalState, const Ast::SourceLocation & callLoc );
      RefCountPtr< const Lang::Group2D > getPile( ){ return pile_; } /* For special use with arrowheads and instances of user classes */
    };
    
    class WarmGroup3D : public Kernel::Warm
    {
      RefCountPtr< const Lang::Group3D > pile_;
    public:
      WarmGroup3D( );
      virtual ~WarmGroup3D( );
      virtual void tackOn( Kernel::EvalState * evalState, const RefCountPtr< const Lang::Value > & piece, const Kernel::PassedDyn & dyn, const Ast::SourceLocation & callLoc );
      virtual void freeze( Kernel::EvalState * evalState, const Ast::SourceLocation & callLoc );
      RefCountPtr< const Lang::Group3D > getPile( ){ return pile_; } /* For special use with arrowheads and instances of user classes */
    };
    
    class WarmGroupLights : public Kernel::Warm
    {
      RefCountPtr< const Lang::LightGroup > pile_;
    public:
      WarmGroupLights( );
      virtual ~WarmGroupLights( );
      virtual void tackOn( Kernel::EvalState * evalState, const RefCountPtr< const Lang::Value > & piece, const Kernel::PassedDyn & dyn, const Ast::SourceLocation & callLoc );
      virtual void freeze( Kernel::EvalState * evalState, const Ast::SourceLocation & callLoc );
      RefCountPtr< const Lang::LightGroup > getPile( ){ return pile_; } /* For special use with arrowheads and instances of user classes */
    };
    
    class WarmZBuf : public Kernel::Warm
    {
      RefCountPtr< std::list< RefCountPtr< Computation::PaintedPolygon3D > > > pile_;
      RefCountPtr< std::list< RefCountPtr< Computation::StrokedLine3D > > > strokePile_;
      RefCountPtr< std::list< RefCountPtr< const Lang::LightSource > > > lightPile_;
    public:
      WarmZBuf( );
      virtual ~WarmZBuf( );
      virtual void tackOn( Kernel::EvalState * evalState, const RefCountPtr< const Lang::Value > & piece, const Kernel::PassedDyn & dyn, const Ast::SourceLocation & callLoc );
      virtual void freeze( Kernel::EvalState * evalState, const Ast::SourceLocation & callLoc );
    };
    
    class WarmZSorter : public Kernel::Warm
    {
      RefCountPtr< std::list< RefCountPtr< Computation::PaintedPolygon3D > > > pile_;
      RefCountPtr< std::list< RefCountPtr< Computation::StrokedLine3D > > > strokePile_;
      RefCountPtr< std::list< RefCountPtr< const Lang::LightSource > > > lightPile_;
    public:
      WarmZSorter( );
      virtual ~WarmZSorter( );
      virtual void tackOn( Kernel::EvalState * evalState, const RefCountPtr< const Lang::Value > & piece, const Kernel::PassedDyn & dyn, const Ast::SourceLocation & callLoc );
      virtual void freeze( Kernel::EvalState * evalState, const Ast::SourceLocation & callLoc );
    };
    
    class WarmTimer : public Kernel::Warm
    {
      timeval start_;
    public:
      WarmTimer( );
      virtual ~WarmTimer( );
      virtual void tackOn( Kernel::EvalState * evalState, const RefCountPtr< const Lang::Value > & piece, const Kernel::PassedDyn & dyn, const Ast::SourceLocation & callLoc );
      virtual void freeze( Kernel::EvalState * evalState, const Ast::SourceLocation & callLoc );
    };

    class WarmText : public Kernel::Warm
    {
      RefCountPtr< std::list< RefCountPtr< const Lang::TextOperation > > > pile_;      
    public:
      WarmText( );
      virtual ~WarmText( );
      virtual void tackOn( Kernel::EvalState * evalState, const RefCountPtr< const Lang::Value > & piece, const Kernel::PassedDyn & dyn, const Ast::SourceLocation & callLoc );
      virtual void freeze( Kernel::EvalState * evalState, const Ast::SourceLocation & callLoc );
    };

    class WarmType3Font : public Kernel::Warm
    {
      std::list< RefCountPtr< const Lang::KernedText > > kernings_;
      std::list< RefCountPtr< const Lang::Type3Glyph > > glyphs_;
      RefCountPtr< FontMetrics::BaseFont > metrics_;
      Concrete::Length size_;
    public:
      WarmType3Font( );
      virtual ~WarmType3Font( );
      virtual void tackOn( Kernel::EvalState * evalState, const RefCountPtr< const Lang::Value > & piece, const Kernel::PassedDyn & dyn, const Ast::SourceLocation & callLoc );
      virtual void freeze( Kernel::EvalState * evalState, const Ast::SourceLocation & callLoc );

    private:
      static void initializeLegalStrechValues( std::set< const char *, charPtrLess > * legalStretchValues );
    };

  }

}

#endif
