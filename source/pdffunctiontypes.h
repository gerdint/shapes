#ifndef pdffunctiontypes_h
#define pdffunctiontypes_h

#include "functiontypes.h"
#include "simplepdfo.h"

namespace Shapes
{
  namespace Lang
  {

    class PDF_Function : public Lang::Function
    {
    public:
      typedef enum { INTERPOLATION = 0, EXPONENTIAL, STITCHING, CALCULATOR } SubType;
    private:
      SubType subType_;
    protected:
      const size_t outputDimension_;
      const size_t inputDimension_;
      const bool rangeIsActive_;
      std::vector< std::pair< double, double > > domain_;
      std::vector< std::pair< double, double > > range_;
      RefCountPtr< SimplePDF::PDF_Indirect_out > storage;

      const char * title_;

      void addCommonFields( SimplePDF::PDF_Dictionary * dst ) const;
      virtual std::auto_ptr< const std::vector< double > > callImpl( const std::vector< double > & arg ) const;
    public:
      PDF_Function( SubType subType, size_t outputDimension, size_t inputDimension, bool rangeIsActive );
      virtual ~PDF_Function( );
      virtual void gcMark( Kernel::GCMarkedSet & marked ){ };
      virtual void call( Kernel::EvalState * evalState, Kernel::Arguments & args, const Ast::SourceLocation & callLoc ) const;
      virtual bool isTransforming( ) const { return false; }
      bool matchesDimensions( size_t outputDimension, size_t inputDimension ) const;
      RefCountPtr< SimplePDF::PDF_Indirect_out > getFunction( ) const;
    };

    class PDF_InterpolationFunction : public Lang::PDF_Function
    {
    public:
      PDF_InterpolationFunction( const std::vector< std::pair< double, double > > & range,
				 const std::vector< std::pair< double, double > > & domain,
				 const std::vector< double > & size,
				 unsigned char bitsPerSample,
				 bool cubic,
				 const std::vector< std::pair< double, double > > & encode,
				 const std::vector< std::pair< double, double > > & decode,
				 const std::vector< double > table );
      ~PDF_InterpolationFunction( );
    };

  }
}

#endif
