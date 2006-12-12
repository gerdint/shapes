#ifndef metapdfastexprs_h
#define metapdfastexprs_h

#include "MetaPDF_Lang_decls.h"

#include "metapdfastexpr.h"

namespace MetaPDF
{
  namespace Ast
  {

  class NegExpr : public UnaryPrefixExpr
  {
  public:
    NegExpr( const Ast::SourceLocation & _opLoc, Ast::Expression * _expr ) : UnaryPrefixExpr( _opLoc, _expr ) { }
    UNARYCALLIMPL( CLASSTREE1_Float );
    UNARYCALLIMPL( CLASSTREE1_Integer );
    UNARYCALLIMPL( CLASSTREE1_Length );
    UNARYCALLIMPL( CLASSTREE1_FloatPair );
    UNARYCALLIMPL( CLASSTREE1_Coords2D );
    UNARYCALLIMPL( CLASSTREE1_FloatTriple );
    UNARYCALLIMPL( CLASSTREE1_Coords3D );
  private:
    RefCountPtr< const Lang::Value > impl( DUMMYANDREF( const Lang::Float ) arg ) const;
    RefCountPtr< const Lang::Value > impl( DUMMYANDREF( const Lang::Integer ) arg ) const;
    RefCountPtr< const Lang::Value > impl( DUMMYANDREF( const Lang::Length ) arg ) const;
    RefCountPtr< const Lang::Value > impl( DUMMYANDREF( const Lang::FloatPair ) arg ) const;
    RefCountPtr< const Lang::Value > impl( DUMMYANDREF( const Lang::Coords2D ) arg ) const;
    RefCountPtr< const Lang::Value > impl( DUMMYANDREF( const Lang::FloatTriple ) arg ) const;
    RefCountPtr< const Lang::Value > impl( DUMMYANDREF( const Lang::Coords3D ) arg ) const;
  };
  
  class RelativeExpr : public UnaryPrefixExpr
  {
  public:
    RelativeExpr( const Ast::SourceLocation & _opLoc, Ast::Expression * _expr ) : UnaryPrefixExpr( _opLoc, _expr ) { }
    UNARYCALLIMPL( CLASSTREE1_Length );
    UNARYCALLIMPL( CLASSTREE1_Coords2D );
    UNARYCALLIMPL( CLASSTREE1_Coords3D );
  private:
    RefCountPtr< const Lang::Value > impl( DUMMYANDREF( const Lang::Length ) arg ) const;
    RefCountPtr< const Lang::Value > impl( DUMMYANDREF( const Lang::Coords2D ) arg ) const;
    RefCountPtr< const Lang::Value > impl( DUMMYANDREF( const Lang::Coords3D ) arg ) const;
  };
  
  class NotExpr : public UnaryPrefixExpr
  {
  public:
    NotExpr( const Ast::SourceLocation & _opLoc, Ast::Expression * _expr ) : UnaryPrefixExpr( _opLoc, _expr ) { }
    UNARYCALLIMPL( CLASSTREE1_Boolean );
  private:
    RefCountPtr< const Lang::Value > impl( DUMMYANDREF( const Lang::Boolean ) arg ) const;
  };
  
  class CycleExpr : public UnaryPostfixExpr
  {
  public:
    CycleExpr( const Ast::SourceLocation & _opLoc, Ast::Expression * _expr ) : UnaryPostfixExpr( _opLoc, _expr ) { }
    UNARYCALLIMPL( CLASSTREE1_PathPoint2D );
    UNARYCALLIMPL( CLASSTREE1_SubPath2D );
    UNARYCALLIMPL( CLASSTREE1_PathPoint3D );
    UNARYCALLIMPL( CLASSTREE1_SubPath3D );
  private:
    RefCountPtr< const Lang::Value > impl( DUMMYANDREF( const Lang::PathPoint2D ) arg ) const;
    RefCountPtr< const Lang::Value > impl( DUMMYANDREF( const Lang::SubPath2D ) arg ) const;
    RefCountPtr< const Lang::Value > impl( DUMMYANDREF( const Lang::PathPoint3D ) arg ) const;
    RefCountPtr< const Lang::Value > impl( DUMMYANDREF( const Lang::SubPath3D ) arg ) const;
  };

  class MinusMinusExpr : public BinaryInfixExpr
  {
  public:
    MinusMinusExpr( const Ast::SourceLocation & _opLoc, Ast::Expression * _expr1, Ast::Expression * _expr2 ) : BinaryInfixExpr( _opLoc, _expr1, _expr2 ) { }
    CALLIMPL( CLASSTREE1_Float, CLASSTREE2_Float );
    CALLIMPL( CLASSTREE1_Length, CLASSTREE2_Length );
    CALLIMPL( CLASSTREE1_Coords2D, CLASSTREE2_Coords2D );
    CALLIMPL( CLASSTREE1_Coords2D, CLASSTREE2_PathPoint2D );
    CALLIMPL( CLASSTREE1_PathPoint2D, CLASSTREE2_Coords2D );
    CALLIMPL( CLASSTREE1_Coords2D, CLASSTREE2_SubPath2D );
    CALLIMPL( CLASSTREE1_SubPath2D, CLASSTREE2_Coords2D );
    CALLIMPL( CLASSTREE1_PathPoint2D, CLASSTREE2_PathPoint2D );
    CALLIMPL( CLASSTREE1_PathPoint2D, CLASSTREE2_SubPath2D );
    CALLIMPL( CLASSTREE1_SubPath2D, CLASSTREE2_PathPoint2D );
    CALLIMPL( CLASSTREE1_SubPath2D, CLASSTREE2_SubPath2D );
    CALLIMPL( CLASSTREE1_Coords3D, CLASSTREE2_Coords3D );
    CALLIMPL( CLASSTREE1_Coords3D, CLASSTREE2_PathPoint3D );
    CALLIMPL( CLASSTREE1_PathPoint3D, CLASSTREE2_Coords3D );
    CALLIMPL( CLASSTREE1_Coords3D, CLASSTREE2_SubPath3D );
    CALLIMPL( CLASSTREE1_SubPath3D, CLASSTREE2_Coords3D );
    CALLIMPL( CLASSTREE1_PathPoint3D, CLASSTREE2_PathPoint3D );
    CALLIMPL( CLASSTREE1_PathPoint3D, CLASSTREE2_SubPath3D );
    CALLIMPL( CLASSTREE1_SubPath3D, CLASSTREE2_PathPoint3D );
    CALLIMPL( CLASSTREE1_SubPath3D, CLASSTREE2_SubPath3D );
    CALLIMPL( CLASSTREE1_PathSlider2D, CLASSTREE2_PathSlider2D );
    CALLIMPL( CLASSTREE1_PathSlider3D, CLASSTREE2_PathSlider3D );
  private:
    RefCountPtr< const Lang::Value > impl( DUMMYANDREF( const Lang::Float ) arg1, DUMMYANDREF( const Lang::Float ) arg2 ) const;
    RefCountPtr< const Lang::Value > impl( DUMMYANDREF( const Lang::Length ) arg1, DUMMYANDREF( const Lang::Length ) arg2 ) const;
    RefCountPtr< const Lang::Value > impl( DUMMYANDREF( const Lang::Coords2D ) arg1, DUMMYANDREF( const Lang::Coords2D ) arg2 ) const;
    RefCountPtr< const Lang::Value > impl( DUMMYANDREF( const Lang::Coords2D ) arg1, DUMMYANDREF( const Lang::PathPoint2D ) arg2 ) const;
    RefCountPtr< const Lang::Value > impl( DUMMYANDREF( const Lang::PathPoint2D ) arg1, DUMMYANDREF( const Lang::Coords2D ) arg2 ) const;
    RefCountPtr< const Lang::Value > impl( DUMMYANDREF( const Lang::Coords2D ) arg1, DUMMYANDREF( const Lang::SubPath2D ) arg2 ) const;
    RefCountPtr< const Lang::Value > impl( DUMMYANDREF( const Lang::SubPath2D ) arg1, DUMMYANDREF( const Lang::Coords2D ) arg2 ) const;
    RefCountPtr< const Lang::Value > impl( DUMMYANDREF( const Lang::PathPoint2D ) arg1, DUMMYANDREF( const Lang::PathPoint2D ) arg2 ) const;
    RefCountPtr< const Lang::Value > impl( DUMMYANDREF( const Lang::PathPoint2D ) arg1, DUMMYANDREF( const Lang::SubPath2D ) arg2 ) const;
    RefCountPtr< const Lang::Value > impl( DUMMYANDREF( const Lang::SubPath2D ) arg1, DUMMYANDREF( const Lang::PathPoint2D ) arg2 ) const;
    RefCountPtr< const Lang::Value > impl( DUMMYANDREF( const Lang::SubPath2D ) arg1, DUMMYANDREF( const Lang::SubPath2D ) arg2 ) const;
    RefCountPtr< const Lang::Value > impl( DUMMYANDREF( const Lang::Coords3D ) arg1, DUMMYANDREF( const Lang::Coords3D ) arg2 ) const;
    RefCountPtr< const Lang::Value > impl( DUMMYANDREF( const Lang::Coords3D ) arg1, DUMMYANDREF( const Lang::PathPoint3D ) arg2 ) const;
    RefCountPtr< const Lang::Value > impl( DUMMYANDREF( const Lang::PathPoint3D ) arg1, DUMMYANDREF( const Lang::Coords3D ) arg2 ) const;
    RefCountPtr< const Lang::Value > impl( DUMMYANDREF( const Lang::Coords3D ) arg1, DUMMYANDREF( const Lang::SubPath3D ) arg2 ) const;
    RefCountPtr< const Lang::Value > impl( DUMMYANDREF( const Lang::SubPath3D ) arg1, DUMMYANDREF( const Lang::Coords3D ) arg2 ) const;
    RefCountPtr< const Lang::Value > impl( DUMMYANDREF( const Lang::PathPoint3D ) arg1, DUMMYANDREF( const Lang::PathPoint3D ) arg2 ) const;
    RefCountPtr< const Lang::Value > impl( DUMMYANDREF( const Lang::PathPoint3D ) arg1, DUMMYANDREF( const Lang::SubPath3D ) arg2 ) const;
    RefCountPtr< const Lang::Value > impl( DUMMYANDREF( const Lang::SubPath3D ) arg1, DUMMYANDREF( const Lang::PathPoint3D ) arg2 ) const;
    RefCountPtr< const Lang::Value > impl( DUMMYANDREF( const Lang::SubPath3D ) arg1, DUMMYANDREF( const Lang::SubPath3D ) arg2 ) const;
    RefCountPtr< const Lang::Value > impl( DUMMYANDREF( const Lang::PathSlider2D ) arg1, DUMMYANDREF( const Lang::PathSlider2D ) arg2 ) const;
    RefCountPtr< const Lang::Value > impl( DUMMYANDREF( const Lang::PathSlider3D ) arg1, DUMMYANDREF( const Lang::PathSlider3D ) arg2 ) const;
  };
  
  class PlusPlusExpr : public BinaryInfixExpr
  {
  public:
    PlusPlusExpr( const Ast::SourceLocation & _opLoc, Ast::Expression * _expr1, Ast::Expression * _expr2 ) : BinaryInfixExpr( _opLoc, _expr1, _expr2 ) { }
    CALLIMPL( CLASSTREE1_Float, CLASSTREE2_Float );
    CALLIMPL( CLASSTREE1_Length, CLASSTREE2_Length );
  private:
    RefCountPtr< const Lang::Value > impl( DUMMYANDREF( const Lang::Float ) arg1, DUMMYANDREF( const Lang::Float ) arg2 ) const;
    RefCountPtr< const Lang::Value > impl( DUMMYANDREF( const Lang::Length ) arg1, DUMMYANDREF( const Lang::Length ) arg2 ) const;
  };
  
  class AmpersandExpr : public BinaryInfixExpr
  {
  public:
    AmpersandExpr( const Ast::SourceLocation & _opLoc, Ast::Expression * _expr1, Ast::Expression * _expr2 ) : BinaryInfixExpr( _opLoc, _expr1, _expr2 ) { }
    CALLIMPL( CLASSTREE1_SubPath2D, CLASSTREE2_SubPath2D );
    CALLIMPL( CLASSTREE1_Path2D, CLASSTREE2_SubPath2D );
    CALLIMPL( CLASSTREE1_SubPath2D, CLASSTREE2_Path2D );
    CALLIMPL( CLASSTREE1_Path2D, CLASSTREE2_Path2D );
    CALLIMPL( CLASSTREE1_SubPath3D, CLASSTREE2_SubPath3D );
    CALLIMPL( CLASSTREE1_Path3D, CLASSTREE2_SubPath3D );
    CALLIMPL( CLASSTREE1_SubPath3D, CLASSTREE2_Path3D );
    CALLIMPL( CLASSTREE1_Path3D, CLASSTREE2_Path3D );
    CALLIMPL( CLASSTREE1_DynamicBindings, CLASSTREE2_DynamicBindings );
  private:
    RefCountPtr< const Lang::Value > impl( DUMMYANDREF( const Lang::SubPath2D ) arg1, DUMMYANDREF( const Lang::SubPath2D ) arg2 ) const;
    RefCountPtr< const Lang::Value > impl( DUMMYANDREF( const Lang::Path2D ) arg1, DUMMYANDREF( const Lang::SubPath2D ) arg2 ) const;
    RefCountPtr< const Lang::Value > impl( DUMMYANDREF( const Lang::SubPath2D ) arg1, DUMMYANDREF( const Lang::Path2D ) arg2 ) const;
    RefCountPtr< const Lang::Value > impl( DUMMYANDREF( const Lang::Path2D ) arg1, DUMMYANDREF( const Lang::Path2D ) arg2 ) const;
    RefCountPtr< const Lang::Value > impl( DUMMYANDREF( const Lang::SubPath3D ) arg1, DUMMYANDREF( const Lang::SubPath3D ) arg2 ) const;
    RefCountPtr< const Lang::Value > impl( DUMMYANDREF( const Lang::Path3D ) arg1, DUMMYANDREF( const Lang::SubPath3D ) arg2 ) const;
    RefCountPtr< const Lang::Value > impl( DUMMYANDREF( const Lang::SubPath3D ) arg1, DUMMYANDREF( const Lang::Path3D ) arg2 ) const;
    RefCountPtr< const Lang::Value > impl( DUMMYANDREF( const Lang::Path3D ) arg1, DUMMYANDREF( const Lang::Path3D ) arg2 ) const;
    RefCountPtr< const Lang::Value > impl( DUMMYANDREF( const Lang::DynamicBindings ) arg1, DUMMYANDREF( const Lang::DynamicBindings ) arg2 ) const;
  };
  
  class PlusExpr : public BinaryInfixExpr
  {
  public:
    PlusExpr( const Ast::SourceLocation & _opLoc, Ast::Expression * _expr1, Ast::Expression * _expr2 ) : BinaryInfixExpr( _opLoc, _expr1, _expr2 ) { }
    CALLIMPL( CLASSTREE1_Float, CLASSTREE2_Float );
    CALLIMPL( CLASSTREE1_Integer, CLASSTREE2_Integer );
    CALLIMPL( CLASSTREE1_Length, CLASSTREE2_Length );
    CALLIMPL( CLASSTREE1_FloatPair, CLASSTREE2_FloatPair );
    CALLIMPL( CLASSTREE1_Coords2D, CLASSTREE2_Coords2D );
    CALLIMPL( CLASSTREE1_FloatTriple, CLASSTREE2_FloatTriple );
    CALLIMPL( CLASSTREE1_Coords3D, CLASSTREE2_Coords3D );
    CALLIMPL( CLASSTREE1_String, CLASSTREE2_String );
    CALLIMPL( CLASSTREE1_Dash, CLASSTREE2_Length );
    CALLIMPL( CLASSTREE1_Length, CLASSTREE2_Dash );
    CALLIMPL( CLASSTREE1_PathSlider2D, CLASSTREE2_Float );
    CALLIMPL( CLASSTREE1_PathSlider2D, CLASSTREE2_Length );
    CALLIMPL( CLASSTREE1_PathSlider3D, CLASSTREE2_Float );
    CALLIMPL( CLASSTREE1_PathSlider3D, CLASSTREE2_Length );
    CALLIMPL( CLASSTREE1_RGB, CLASSTREE2_RGB );
    CALLIMPL( CLASSTREE1_Gray, CLASSTREE2_Gray );
    CALLIMPL( CLASSTREE1_SpecularReflection, CLASSTREE2_SpecularReflection );
  private:
    RefCountPtr< const Lang::Value > impl( DUMMYANDREF( const Lang::Float ) arg1, DUMMYANDREF( const Lang::Float ) arg2 ) const;
    RefCountPtr< const Lang::Value > impl( DUMMYANDREF( const Lang::Integer ) arg1, DUMMYANDREF( const Lang::Integer ) arg2 ) const;
    RefCountPtr< const Lang::Value > impl( DUMMYANDREF( const Lang::Length ) arg1, DUMMYANDREF( const Lang::Length ) arg2 ) const;
    RefCountPtr< const Lang::Value > impl( DUMMYANDREF( const Lang::FloatPair ) arg1, DUMMYANDREF( const Lang::FloatPair ) arg2 ) const;
    RefCountPtr< const Lang::Value > impl( DUMMYANDREF( const Lang::Coords2D ) arg1, DUMMYANDREF( const Lang::Coords2D ) arg2 ) const;
    RefCountPtr< const Lang::Value > impl( DUMMYANDREF( const Lang::FloatTriple ) arg1, DUMMYANDREF( const Lang::FloatTriple ) arg2 ) const;
    RefCountPtr< const Lang::Value > impl( DUMMYANDREF( const Lang::Coords3D ) arg1, DUMMYANDREF( const Lang::Coords3D ) arg2 ) const;
    RefCountPtr< const Lang::Value > impl( DUMMYANDREF( const Lang::String ) arg1, DUMMYANDREF( const Lang::String ) arg2 ) const;
    RefCountPtr< const Lang::Value > impl( DUMMYANDREF( const Lang::Dash ) arg1, DUMMYANDREF( const Lang::Length ) arg2 ) const;
    RefCountPtr< const Lang::Value > impl( DUMMYANDREF( const Lang::Length ) arg1, DUMMYANDREF( const Lang::Dash ) arg2 ) const;
    RefCountPtr< const Lang::Value > impl( DUMMYANDREF( const Lang::PathSlider2D ) arg1, DUMMYANDREF( const Lang::Float ) arg2 ) const;
    RefCountPtr< const Lang::Value > impl( DUMMYANDREF( const Lang::PathSlider2D ) arg1, DUMMYANDREF( const Lang::Length ) arg2 ) const;
    RefCountPtr< const Lang::Value > impl( DUMMYANDREF( const Lang::PathSlider3D ) arg1, DUMMYANDREF( const Lang::Float ) arg2 ) const;
    RefCountPtr< const Lang::Value > impl( DUMMYANDREF( const Lang::PathSlider3D ) arg1, DUMMYANDREF( const Lang::Length ) arg2 ) const;
    RefCountPtr< const Lang::Value > impl( DUMMYANDREF( const Lang::RGB ) arg1, DUMMYANDREF( const Lang::RGB ) arg2 ) const;
    RefCountPtr< const Lang::Value > impl( DUMMYANDREF( const Lang::Gray ) arg1, DUMMYANDREF( const Lang::Gray ) arg2 ) const;
    RefCountPtr< const Lang::Value > impl( DUMMYANDREF( const Lang::SpecularReflection ) arg1, DUMMYANDREF( const Lang::SpecularReflection ) arg2 ) const;
  };
  
  class MinusExpr : public BinaryInfixExpr
  {
  public:
    MinusExpr( const Ast::SourceLocation & _opLoc, Ast::Expression * _expr1, Ast::Expression * _expr2 ) : BinaryInfixExpr( _opLoc, _expr1, _expr2 ) { }
    CALLIMPL( CLASSTREE1_Float, CLASSTREE2_Float );
    CALLIMPL( CLASSTREE1_Integer, CLASSTREE2_Integer );
    CALLIMPL( CLASSTREE1_Length, CLASSTREE2_Length );
    CALLIMPL( CLASSTREE1_FloatPair, CLASSTREE2_FloatPair );
    CALLIMPL( CLASSTREE1_Coords2D, CLASSTREE2_Coords2D );
    CALLIMPL( CLASSTREE1_FloatTriple, CLASSTREE2_FloatTriple );
    CALLIMPL( CLASSTREE1_Coords3D, CLASSTREE2_Coords3D );
    CALLIMPL( CLASSTREE1_PathSlider2D, CLASSTREE2_Float );
    CALLIMPL( CLASSTREE1_PathSlider2D, CLASSTREE2_Length );
    CALLIMPL( CLASSTREE1_PathSlider3D, CLASSTREE2_Float );
    CALLIMPL( CLASSTREE1_PathSlider3D, CLASSTREE2_Length );
  private:
    RefCountPtr< const Lang::Value > impl( DUMMYANDREF( const Lang::Float ) arg1, DUMMYANDREF( const Lang::Float ) arg2 ) const;
    RefCountPtr< const Lang::Value > impl( DUMMYANDREF( const Lang::Integer ) arg1, DUMMYANDREF( const Lang::Integer ) arg2 ) const;
    RefCountPtr< const Lang::Value > impl( DUMMYANDREF( const Lang::Length ) arg1, DUMMYANDREF( const Lang::Length ) arg2 ) const;
    RefCountPtr< const Lang::Value > impl( DUMMYANDREF( const Lang::FloatPair ) arg1, DUMMYANDREF( const Lang::FloatPair ) arg2 ) const;
    RefCountPtr< const Lang::Value > impl( DUMMYANDREF( const Lang::Coords2D ) arg1, DUMMYANDREF( const Lang::Coords2D ) arg2 ) const;
    RefCountPtr< const Lang::Value > impl( DUMMYANDREF( const Lang::FloatTriple ) arg1, DUMMYANDREF( const Lang::FloatTriple ) arg2 ) const;
    RefCountPtr< const Lang::Value > impl( DUMMYANDREF( const Lang::Coords3D ) arg1, DUMMYANDREF( const Lang::Coords3D ) arg2 ) const;
    RefCountPtr< const Lang::Value > impl( DUMMYANDREF( const Lang::PathSlider2D ) arg1, DUMMYANDREF( const Lang::Float ) arg2 ) const;
    RefCountPtr< const Lang::Value > impl( DUMMYANDREF( const Lang::PathSlider2D ) arg1, DUMMYANDREF( const Lang::Length ) arg2 ) const;
    RefCountPtr< const Lang::Value > impl( DUMMYANDREF( const Lang::PathSlider3D ) arg1, DUMMYANDREF( const Lang::Float ) arg2 ) const;
    RefCountPtr< const Lang::Value > impl( DUMMYANDREF( const Lang::PathSlider3D ) arg1, DUMMYANDREF( const Lang::Length ) arg2 ) const;
  };
  
  class AngleExpr : public BinaryInfixExpr
  {
  public:
    AngleExpr( const Ast::SourceLocation & _opLoc, Ast::Expression * _expr1, Ast::Expression * _expr2 ) : BinaryInfixExpr( _opLoc, _expr1, _expr2 ) { }
    CALLIMPL( CLASSTREE1_Float, CLASSTREE2_Float );
    CALLIMPL( CLASSTREE1_Float, CLASSTREE2_Length );
    CALLIMPL( CLASSTREE1_Length, CLASSTREE2_Float );
    CALLIMPL( CLASSTREE1_Length, CLASSTREE2_Length );
    CALLIMPL( CLASSTREE1_FloatPair, CLASSTREE2_FloatPair );
    CALLIMPL( CLASSTREE1_FloatPair, CLASSTREE2_Coords2D );
    CALLIMPL( CLASSTREE1_Coords2D, CLASSTREE2_FloatPair );
    CALLIMPL( CLASSTREE1_Coords2D, CLASSTREE2_Coords2D );
    CALLIMPL( CLASSTREE1_FloatTriple, CLASSTREE2_FloatTriple );
    CALLIMPL( CLASSTREE1_FloatTriple, CLASSTREE2_Coords3D );
    CALLIMPL( CLASSTREE1_Coords3D, CLASSTREE2_FloatTriple );
    CALLIMPL( CLASSTREE1_Coords3D, CLASSTREE2_Coords3D );
  private:
    RefCountPtr< const Lang::Value > impl( DUMMYANDREF( const Lang::Float ) arg1, DUMMYANDREF( const Lang::Float ) arg2 ) const;
    RefCountPtr< const Lang::Value > impl( DUMMYANDREF( const Lang::Float ) arg1, DUMMYANDREF( const Lang::Length ) arg2 ) const;
    RefCountPtr< const Lang::Value > impl( DUMMYANDREF( const Lang::Length ) arg1, DUMMYANDREF( const Lang::Float ) arg2 ) const;
    RefCountPtr< const Lang::Value > impl( DUMMYANDREF( const Lang::Length ) arg1, DUMMYANDREF( const Lang::Length ) arg2 ) const;
    RefCountPtr< const Lang::Value > impl( DUMMYANDREF( const Lang::FloatPair ) arg1, DUMMYANDREF( const Lang::FloatPair ) arg2 ) const;
    RefCountPtr< const Lang::Value > impl( DUMMYANDREF( const Lang::FloatPair ) arg1, DUMMYANDREF( const Lang::Coords2D ) arg2 ) const;
    RefCountPtr< const Lang::Value > impl( DUMMYANDREF( const Lang::Coords2D ) arg1, DUMMYANDREF( const Lang::FloatPair ) arg2 ) const;
    RefCountPtr< const Lang::Value > impl( DUMMYANDREF( const Lang::Coords2D ) arg1, DUMMYANDREF( const Lang::Coords2D ) arg2 ) const;
    RefCountPtr< const Lang::Value > impl( DUMMYANDREF( const Lang::FloatTriple ) arg1, DUMMYANDREF( const Lang::FloatTriple ) arg2 ) const;
    RefCountPtr< const Lang::Value > impl( DUMMYANDREF( const Lang::FloatTriple ) arg1, DUMMYANDREF( const Lang::Coords3D ) arg2 ) const;
    RefCountPtr< const Lang::Value > impl( DUMMYANDREF( const Lang::Coords3D ) arg1, DUMMYANDREF( const Lang::FloatTriple ) arg2 ) const;
    RefCountPtr< const Lang::Value > impl( DUMMYANDREF( const Lang::Coords3D ) arg1, DUMMYANDREF( const Lang::Coords3D ) arg2 ) const;
  };
  
  class StarExpr : public BinaryInfixExpr
  {
  public:
    StarExpr( const Ast::SourceLocation & _opLoc, Ast::Expression * _expr1, Ast::Expression * _expr2 ) : BinaryInfixExpr( _opLoc, _expr1, _expr2 ) { }
    CALLIMPL( CLASSTREE1_Float, CLASSTREE2_Float );
    CALLIMPL( CLASSTREE1_FloatPair, CLASSTREE2_FloatPair );
    CALLIMPL( CLASSTREE1_FloatPair, CLASSTREE2_Float );
    CALLIMPL( CLASSTREE1_Float, CLASSTREE2_FloatPair );
    CALLIMPL( CLASSTREE1_FloatTriple, CLASSTREE2_FloatTriple );
    CALLIMPL( CLASSTREE1_FloatTriple, CLASSTREE2_Float );
    CALLIMPL( CLASSTREE1_Float, CLASSTREE2_FloatTriple );
    CALLIMPL( CLASSTREE1_Length, CLASSTREE2_Float );
    CALLIMPL( CLASSTREE1_Float, CLASSTREE2_Length );
    CALLIMPL( CLASSTREE1_Coords2D, CLASSTREE2_Float );
    CALLIMPL( CLASSTREE1_Float, CLASSTREE2_Coords2D );
    CALLIMPL( CLASSTREE1_Coords2D, CLASSTREE2_FloatPair );
    CALLIMPL( CLASSTREE1_FloatPair, CLASSTREE2_Coords2D );
    CALLIMPL( CLASSTREE1_FloatPair, CLASSTREE2_Length );
    CALLIMPL( CLASSTREE1_Length, CLASSTREE2_FloatPair );
    CALLIMPL( CLASSTREE1_Coords3D, CLASSTREE2_Float );
    CALLIMPL( CLASSTREE1_Float, CLASSTREE2_Coords3D );
    CALLIMPL( CLASSTREE1_Coords3D, CLASSTREE2_FloatTriple );
    CALLIMPL( CLASSTREE1_FloatTriple, CLASSTREE2_Coords3D );
    CALLIMPL( CLASSTREE1_FloatTriple, CLASSTREE2_Length );
    CALLIMPL( CLASSTREE1_Length, CLASSTREE2_FloatTriple );
    CALLIMPL( CLASSTREE1_Dash, CLASSTREE2_Float );
    CALLIMPL( CLASSTREE1_Float, CLASSTREE2_Dash );
    CALLIMPL( CLASSTREE1_Transform2D, CLASSTREE2_Transform2D );
    CALLIMPL( CLASSTREE1_Transform3D, CLASSTREE2_Transform3D );
    CALLIMPL( CLASSTREE1_Float, CLASSTREE2_RGB );
    CALLIMPL( CLASSTREE1_RGB, CLASSTREE2_Float );
    CALLIMPL( CLASSTREE1_Float, CLASSTREE2_Gray );
    CALLIMPL( CLASSTREE1_Gray, CLASSTREE2_Float );
    CALLIMPL( CLASSTREE1_Float, CLASSTREE2_SpecularReflection );
    CALLIMPL( CLASSTREE1_SpecularReflection, CLASSTREE2_Float );
    CALLIMPL( CLASSTREE1_Integer, CLASSTREE2_Integer );
    CALLIMPL( CLASSTREE1_Float, CLASSTREE2_Integer );
    CALLIMPL( CLASSTREE1_Integer, CLASSTREE2_Float );
    CALLIMPL( CLASSTREE1_Length, CLASSTREE2_Integer );
    CALLIMPL( CLASSTREE1_Integer, CLASSTREE2_Length );
  private:
    RefCountPtr< const Lang::Value > impl( DUMMYANDREF( const Lang::Float ) arg1, DUMMYANDREF( const Lang::Float ) arg2 ) const;
    RefCountPtr< const Lang::Value > impl( DUMMYANDREF( const Lang::FloatPair ) arg1, DUMMYANDREF( const Lang::FloatPair ) arg2 ) const;
    RefCountPtr< const Lang::Value > impl( DUMMYANDREF( const Lang::FloatPair ) arg1, DUMMYANDREF( const Lang::Float ) arg2 ) const;
    RefCountPtr< const Lang::Value > impl( DUMMYANDREF( const Lang::Float ) arg1, DUMMYANDREF( const Lang::FloatPair ) arg2 ) const;
    RefCountPtr< const Lang::Value > impl( DUMMYANDREF( const Lang::FloatTriple ) arg1, DUMMYANDREF( const Lang::FloatTriple ) arg2 ) const;
    RefCountPtr< const Lang::Value > impl( DUMMYANDREF( const Lang::FloatTriple ) arg1, DUMMYANDREF( const Lang::Float ) arg2 ) const;
    RefCountPtr< const Lang::Value > impl( DUMMYANDREF( const Lang::Float ) arg1, DUMMYANDREF( const Lang::FloatTriple ) arg2 ) const;
    RefCountPtr< const Lang::Value > impl( DUMMYANDREF( const Lang::Length ) arg1, DUMMYANDREF( const Lang::Float ) arg2 ) const;
    RefCountPtr< const Lang::Value > impl( DUMMYANDREF( const Lang::Float ) arg1, DUMMYANDREF( const Lang::Length ) arg2 ) const;
    RefCountPtr< const Lang::Value > impl( DUMMYANDREF( const Lang::Coords2D ) arg1, DUMMYANDREF( const Lang::Float ) arg2 ) const;
    RefCountPtr< const Lang::Value > impl( DUMMYANDREF( const Lang::Float ) arg1, DUMMYANDREF( const Lang::Coords2D ) arg2 ) const;
    RefCountPtr< const Lang::Value > impl( DUMMYANDREF( const Lang::Coords2D ) arg1, DUMMYANDREF( const Lang::FloatPair ) arg2 ) const;
    RefCountPtr< const Lang::Value > impl( DUMMYANDREF( const Lang::FloatPair ) arg1, DUMMYANDREF( const Lang::Coords2D ) arg2 ) const;
    RefCountPtr< const Lang::Value > impl( DUMMYANDREF( const Lang::FloatPair ) arg1, DUMMYANDREF( const Lang::Length ) arg2 ) const;
    RefCountPtr< const Lang::Value > impl( DUMMYANDREF( const Lang::Length ) arg1, DUMMYANDREF( const Lang::FloatPair ) arg2 ) const;
    RefCountPtr< const Lang::Value > impl( DUMMYANDREF( const Lang::Coords3D ) arg1, DUMMYANDREF( const Lang::Float ) arg2 ) const;
    RefCountPtr< const Lang::Value > impl( DUMMYANDREF( const Lang::Float ) arg1, DUMMYANDREF( const Lang::Coords3D ) arg2 ) const;
    RefCountPtr< const Lang::Value > impl( DUMMYANDREF( const Lang::Coords3D ) arg1, DUMMYANDREF( const Lang::FloatTriple ) arg2 ) const;
    RefCountPtr< const Lang::Value > impl( DUMMYANDREF( const Lang::FloatTriple ) arg1, DUMMYANDREF( const Lang::Coords3D ) arg2 ) const;
    RefCountPtr< const Lang::Value > impl( DUMMYANDREF( const Lang::FloatTriple ) arg1, DUMMYANDREF( const Lang::Length ) arg2 ) const;
    RefCountPtr< const Lang::Value > impl( DUMMYANDREF( const Lang::Length ) arg1, DUMMYANDREF( const Lang::FloatTriple ) arg2 ) const;
    RefCountPtr< const Lang::Value > impl( DUMMYANDREF( const Lang::Dash ) arg1, DUMMYANDREF( const Lang::Float ) arg2 ) const;
    RefCountPtr< const Lang::Value > impl( DUMMYANDREF( const Lang::Float ) arg1, DUMMYANDREF( const Lang::Dash ) arg2 ) const;
    RefCountPtr< const Lang::Value > impl( DUMMYANDREF( const Lang::Transform2D ) arg1, DUMMYANDREF( const Lang::Transform2D ) arg2 ) const;
    RefCountPtr< const Lang::Value > impl( DUMMYANDREF( const Lang::Transform3D ) arg1, DUMMYANDREF( const Lang::Transform3D ) arg2 ) const;
    RefCountPtr< const Lang::Value > impl( DUMMYANDREF( const Lang::Float ) arg1, DUMMYANDREF( const Lang::RGB ) arg2 ) const;
    RefCountPtr< const Lang::Value > impl( DUMMYANDREF( const Lang::RGB ) arg1, DUMMYANDREF( const Lang::Float ) arg2 ) const;
    RefCountPtr< const Lang::Value > impl( DUMMYANDREF( const Lang::Float ) arg1, DUMMYANDREF( const Lang::Gray ) arg2 ) const;
    RefCountPtr< const Lang::Value > impl( DUMMYANDREF( const Lang::Gray ) arg1, DUMMYANDREF( const Lang::Float ) arg2 ) const;
    RefCountPtr< const Lang::Value > impl( DUMMYANDREF( const Lang::Float ) arg1, DUMMYANDREF( const Lang::SpecularReflection ) arg2 ) const;
    RefCountPtr< const Lang::Value > impl( DUMMYANDREF( const Lang::SpecularReflection ) arg1, DUMMYANDREF( const Lang::Float ) arg2 ) const;
    RefCountPtr< const Lang::Value > impl( DUMMYANDREF( const Lang::Integer ) arg1, DUMMYANDREF( const Lang::Integer ) arg2 ) const;
    RefCountPtr< const Lang::Value > impl( DUMMYANDREF( const Lang::Float ) arg1, DUMMYANDREF( const Lang::Integer ) arg2 ) const;
    RefCountPtr< const Lang::Value > impl( DUMMYANDREF( const Lang::Integer ) arg1, DUMMYANDREF( const Lang::Float ) arg2 ) const;
    RefCountPtr< const Lang::Value > impl( DUMMYANDREF( const Lang::Length ) arg1, DUMMYANDREF( const Lang::Integer ) arg2 ) const;
    RefCountPtr< const Lang::Value > impl( DUMMYANDREF( const Lang::Integer ) arg1, DUMMYANDREF( const Lang::Length ) arg2 ) const;
  };
  
  class SlashExpr : public BinaryInfixExpr
  {
  public:
    SlashExpr( const Ast::SourceLocation & _opLoc, Ast::Expression * _expr1, Ast::Expression * _expr2 ) : BinaryInfixExpr( _opLoc, _expr1, _expr2 ) { }
    CALLIMPL( CLASSTREE1_Float, CLASSTREE2_Float );
    CALLIMPL( CLASSTREE1_Length, CLASSTREE2_Length );
    CALLIMPL( CLASSTREE1_Length, CLASSTREE2_Float );
    CALLIMPL( CLASSTREE1_FloatPair, CLASSTREE2_Float );
    CALLIMPL( CLASSTREE1_Coords2D, CLASSTREE2_Length );
    CALLIMPL( CLASSTREE1_Coords2D, CLASSTREE2_Float );
    CALLIMPL( CLASSTREE1_FloatTriple, CLASSTREE2_Float );
    CALLIMPL( CLASSTREE1_Coords3D, CLASSTREE2_Length );
    CALLIMPL( CLASSTREE1_Coords3D, CLASSTREE2_Float );
    CALLIMPL( CLASSTREE1_Integer, CLASSTREE2_Integer );
    CALLIMPL( CLASSTREE1_Float, CLASSTREE2_Integer );
    CALLIMPL( CLASSTREE1_Length, CLASSTREE2_Integer );
  private:
    RefCountPtr< const Lang::Value > impl( DUMMYANDREF( const Lang::Float ) arg1, DUMMYANDREF( const Lang::Float ) arg2 ) const;
    RefCountPtr< const Lang::Value > impl( DUMMYANDREF( const Lang::Length ) arg1, DUMMYANDREF( const Lang::Length ) arg2 ) const;
    RefCountPtr< const Lang::Value > impl( DUMMYANDREF( const Lang::Length ) arg1, DUMMYANDREF( const Lang::Float ) arg2 ) const;
    RefCountPtr< const Lang::Value > impl( DUMMYANDREF( const Lang::FloatPair ) arg1, DUMMYANDREF( const Lang::Float ) arg2 ) const;
    RefCountPtr< const Lang::Value > impl( DUMMYANDREF( const Lang::Coords2D ) arg1, DUMMYANDREF( const Lang::Length ) arg2 ) const;
    RefCountPtr< const Lang::Value > impl( DUMMYANDREF( const Lang::Coords2D ) arg1, DUMMYANDREF( const Lang::Float ) arg2 ) const;
    RefCountPtr< const Lang::Value > impl( DUMMYANDREF( const Lang::FloatTriple ) arg1, DUMMYANDREF( const Lang::Float ) arg2 ) const;
    RefCountPtr< const Lang::Value > impl( DUMMYANDREF( const Lang::Coords3D ) arg1, DUMMYANDREF( const Lang::Length ) arg2 ) const;
    RefCountPtr< const Lang::Value > impl( DUMMYANDREF( const Lang::Coords3D ) arg1, DUMMYANDREF( const Lang::Float ) arg2 ) const;
    RefCountPtr< const Lang::Value > impl( DUMMYANDREF( const Lang::Integer ) arg1, DUMMYANDREF( const Lang::Integer ) arg2 ) const;
    RefCountPtr< const Lang::Value > impl( DUMMYANDREF( const Lang::Float ) arg1, DUMMYANDREF( const Lang::Integer ) arg2 ) const;
    RefCountPtr< const Lang::Value > impl( DUMMYANDREF( const Lang::Length ) arg1, DUMMYANDREF( const Lang::Integer ) arg2 ) const;
  };
  
  class ProjectionExpr : public BinaryInfixExpr
  {
  public:
    ProjectionExpr( const Ast::SourceLocation & _opLoc, Ast::Expression * _expr1, Ast::Expression * _expr2 ) : BinaryInfixExpr( _opLoc, _expr1, _expr2 ) { }
    CALLIMPL( CLASSTREE1_FloatPair, CLASSTREE2_FloatPair );
    CALLIMPL( CLASSTREE1_FloatPair, CLASSTREE2_Coords2D );
    CALLIMPL( CLASSTREE1_Coords2D, CLASSTREE2_FloatPair );
    CALLIMPL( CLASSTREE1_Coords2D, CLASSTREE2_Coords2D );
    CALLIMPL( CLASSTREE1_FloatTriple, CLASSTREE2_FloatTriple );
    CALLIMPL( CLASSTREE1_FloatTriple, CLASSTREE2_Coords3D );
    CALLIMPL( CLASSTREE1_Coords3D, CLASSTREE2_FloatTriple );
    CALLIMPL( CLASSTREE1_Coords3D, CLASSTREE2_Coords3D );
  private:
    RefCountPtr< const Lang::Value > impl( DUMMYANDREF( const Lang::FloatPair ) arg1, DUMMYANDREF( const Lang::FloatPair ) arg2 ) const;
    RefCountPtr< const Lang::Value > impl( DUMMYANDREF( const Lang::FloatPair ) arg1, DUMMYANDREF( const Lang::Coords2D ) arg2 ) const;
    RefCountPtr< const Lang::Value > impl( DUMMYANDREF( const Lang::Coords2D ) arg1, DUMMYANDREF( const Lang::FloatPair ) arg2 ) const;
    RefCountPtr< const Lang::Value > impl( DUMMYANDREF( const Lang::Coords2D ) arg1, DUMMYANDREF( const Lang::Coords2D ) arg2 ) const;
    RefCountPtr< const Lang::Value > impl( DUMMYANDREF( const Lang::FloatTriple ) arg1, DUMMYANDREF( const Lang::FloatTriple ) arg2 ) const;
    RefCountPtr< const Lang::Value > impl( DUMMYANDREF( const Lang::FloatTriple ) arg1, DUMMYANDREF( const Lang::Coords3D ) arg2 ) const;
    RefCountPtr< const Lang::Value > impl( DUMMYANDREF( const Lang::Coords3D ) arg1, DUMMYANDREF( const Lang::FloatTriple ) arg2 ) const;
    RefCountPtr< const Lang::Value > impl( DUMMYANDREF( const Lang::Coords3D ) arg1, DUMMYANDREF( const Lang::Coords3D ) arg2 ) const;
  };
  
  class ComposeExpr : public BinaryInfixExpr
  {
  public:
    ComposeExpr( const Ast::SourceLocation & _opLoc, Ast::Expression * _expr1, Ast::Expression * _expr2 ) : BinaryInfixExpr( _opLoc, _expr1, _expr2 ) { }
    CALLIMPL( CLASSTREE1_Transform2D, CLASSTREE2_Transform2D );
    CALLIMPL( CLASSTREE1_Transform3D, CLASSTREE2_Transform3D );
    CALLIMPL( CLASSTREE1_Function, CLASSTREE2_Function );
  private:
    RefCountPtr< const Lang::Value > impl( DUMMYANDREF( const Lang::Transform2D ) arg1, DUMMYANDREF( const Lang::Transform2D ) arg2 ) const;
    RefCountPtr< const Lang::Value > impl( DUMMYANDREF( const Lang::Transform3D ) arg1, DUMMYANDREF( const Lang::Transform3D ) arg2 ) const;
    RefCountPtr< const Lang::Value > impl( DUMMYANDREF( const Lang::Function ) arg1, DUMMYANDREF( const Lang::Function ) arg2 ) const;
  };
  
  class LessExpr : public BinaryInfixExpr
  {
  public:
    LessExpr( const Ast::SourceLocation & _opLoc, Ast::Expression * _expr1, Ast::Expression * _expr2 ) : BinaryInfixExpr( _opLoc, _expr1, _expr2 ) { }
    CALLIMPL( CLASSTREE1_Symbol, CLASSTREE2_Symbol );
    CALLIMPL( CLASSTREE1_Float, CLASSTREE2_Float );
    CALLIMPL( CLASSTREE1_Integer, CLASSTREE2_Integer );
    CALLIMPL( CLASSTREE1_Length, CLASSTREE2_Length );
    CALLIMPL( CLASSTREE1_Coords2D, CLASSTREE2_Coords2D );
    CALLIMPL( CLASSTREE1_Coords2D, CLASSTREE2_PathPoint2D );
    CALLIMPL( CLASSTREE1_Coords2D, CLASSTREE2_PathSlider2D );
    CALLIMPL( CLASSTREE1_PolarHandleBase, CLASSTREE2_Coords2D );
    CALLIMPL( CLASSTREE1_PolarHandleBase, CLASSTREE2_PathPoint2D );
    CALLIMPL( CLASSTREE1_PolarHandleBase, CLASSTREE2_PathSlider2D );
    CALLIMPL( CLASSTREE1_Coords3D, CLASSTREE2_Coords3D );
    CALLIMPL( CLASSTREE1_Coords3D, CLASSTREE2_PathPoint3D );
    CALLIMPL( CLASSTREE1_Coords3D, CLASSTREE2_PathSlider3D );
  private:
    RefCountPtr< const Lang::Value > impl( DUMMYANDREF( const Lang::Symbol ) arg1, DUMMYANDREF( const Lang::Symbol ) arg2 ) const;
    RefCountPtr< const Lang::Value > impl( DUMMYANDREF( const Lang::Float ) arg1, DUMMYANDREF( const Lang::Float ) arg2 ) const;
    RefCountPtr< const Lang::Value > impl( DUMMYANDREF( const Lang::Integer ) arg1, DUMMYANDREF( const Lang::Integer ) arg2 ) const;
    RefCountPtr< const Lang::Value > impl( DUMMYANDREF( const Lang::Length ) arg1, DUMMYANDREF( const Lang::Length ) arg2 ) const;
    RefCountPtr< const Lang::Value > impl( DUMMYANDREF( const Lang::Coords2D ) arg1, DUMMYANDREF( const Lang::Coords2D ) arg2 ) const;
    RefCountPtr< const Lang::Value > impl( DUMMYANDREF( const Lang::Coords2D ) arg1, DUMMYANDREF( const Lang::PathPoint2D ) arg2 ) const;
    RefCountPtr< const Lang::Value > impl( DUMMYANDREF( const Lang::Coords2D ) arg1, DUMMYANDREF( const Lang::PathSlider2D ) arg2 ) const;
    RefCountPtr< const Lang::Value > impl( DUMMYANDREF( const Lang::PolarHandleBase ) arg1, DUMMYANDREF( const Lang::Coords2D ) arg2 ) const;
    RefCountPtr< const Lang::Value > impl( DUMMYANDREF( const Lang::PolarHandleBase ) arg1, DUMMYANDREF( const Lang::PathPoint2D ) arg2 ) const;
    RefCountPtr< const Lang::Value > impl( DUMMYANDREF( const Lang::PolarHandleBase ) arg1, DUMMYANDREF( const Lang::PathSlider2D ) arg2 ) const;
    RefCountPtr< const Lang::Value > impl( DUMMYANDREF( const Lang::CornerCoords2D ) arg1, DUMMYANDREF( const Lang::Coords2D ) arg2 ) const;
    RefCountPtr< const Lang::Value > impl( DUMMYANDREF( const Lang::CornerCoords2D ) arg1, DUMMYANDREF( const Lang::PathPoint2D ) arg2 ) const;
    RefCountPtr< const Lang::Value > impl( DUMMYANDREF( const Lang::Coords3D ) arg1, DUMMYANDREF( const Lang::Coords3D ) arg2 ) const;
    RefCountPtr< const Lang::Value > impl( DUMMYANDREF( const Lang::Coords3D ) arg1, DUMMYANDREF( const Lang::PathPoint3D ) arg2 ) const;
    RefCountPtr< const Lang::Value > impl( DUMMYANDREF( const Lang::Coords3D ) arg1, DUMMYANDREF( const Lang::PathSlider3D ) arg2 ) const;
  };
  
  class GreaterExpr : public BinaryInfixExpr
  {
  public:
    GreaterExpr( const Ast::SourceLocation & _opLoc, Ast::Expression * _expr1, Ast::Expression * _expr2 ) : BinaryInfixExpr( _opLoc, _expr1, _expr2 ) { }
    CALLIMPL( CLASSTREE1_Symbol, CLASSTREE2_Symbol );
    CALLIMPL( CLASSTREE1_Float, CLASSTREE2_Float );
    CALLIMPL( CLASSTREE1_Integer, CLASSTREE2_Integer );
    CALLIMPL( CLASSTREE1_Length, CLASSTREE2_Length );
    CALLIMPL( CLASSTREE1_Coords2D, CLASSTREE2_Coords2D );
    CALLIMPL( CLASSTREE1_PathPoint2D, CLASSTREE2_Coords2D );
    CALLIMPL( CLASSTREE1_PathSlider2D, CLASSTREE2_Coords2D );
    CALLIMPL( CLASSTREE1_Coords2D, CLASSTREE2_PolarHandleBase );
    CALLIMPL( CLASSTREE1_PathPoint2D, CLASSTREE2_PolarHandleBase );
    CALLIMPL( CLASSTREE1_PathSlider2D, CLASSTREE2_PolarHandleBase );
    CALLIMPL( CLASSTREE1_Coords3D, CLASSTREE2_Coords3D );
    CALLIMPL( CLASSTREE1_PathPoint3D, CLASSTREE2_Coords3D );
    CALLIMPL( CLASSTREE1_PathSlider3D, CLASSTREE2_Coords3D );
  private:
    RefCountPtr< const Lang::Value > impl( DUMMYANDREF( const Lang::Symbol ) arg1, DUMMYANDREF( const Lang::Symbol ) arg2 ) const;
    RefCountPtr< const Lang::Value > impl( DUMMYANDREF( const Lang::Float ) arg1, DUMMYANDREF( const Lang::Float ) arg2 ) const;
    RefCountPtr< const Lang::Value > impl( DUMMYANDREF( const Lang::Integer ) arg1, DUMMYANDREF( const Lang::Integer ) arg2 ) const;
    RefCountPtr< const Lang::Value > impl( DUMMYANDREF( const Lang::Length ) arg1, DUMMYANDREF( const Lang::Length ) arg2 ) const;
    RefCountPtr< const Lang::Value > impl( DUMMYANDREF( const Lang::Coords2D ) arg1, DUMMYANDREF( const Lang::Coords2D ) arg2 ) const;
    RefCountPtr< const Lang::Value > impl( DUMMYANDREF( const Lang::PathPoint2D ) arg1, DUMMYANDREF( const Lang::Coords2D ) arg2 ) const;
    RefCountPtr< const Lang::Value > impl( DUMMYANDREF( const Lang::PathSlider2D ) arg1, DUMMYANDREF( const Lang::Coords2D ) arg2 ) const;
    RefCountPtr< const Lang::Value > impl( DUMMYANDREF( const Lang::Coords2D ) arg1, DUMMYANDREF( const Lang::PolarHandleBase ) arg2 ) const;
    RefCountPtr< const Lang::Value > impl( DUMMYANDREF( const Lang::PathPoint2D ) arg1, DUMMYANDREF( const Lang::PolarHandleBase ) arg2 ) const;
    RefCountPtr< const Lang::Value > impl( DUMMYANDREF( const Lang::PathSlider2D ) arg1, DUMMYANDREF( const Lang::PolarHandleBase ) arg2 ) const;
    RefCountPtr< const Lang::Value > impl( DUMMYANDREF( const Lang::Coords2D ) arg1, DUMMYANDREF( const Lang::CornerCoords2D ) arg2 ) const;
    RefCountPtr< const Lang::Value > impl( DUMMYANDREF( const Lang::PathPoint2D ) arg1, DUMMYANDREF( const Lang::CornerCoords2D ) arg2 ) const;
    RefCountPtr< const Lang::Value > impl( DUMMYANDREF( const Lang::Coords3D ) arg1, DUMMYANDREF( const Lang::Coords3D ) arg2 ) const;
    RefCountPtr< const Lang::Value > impl( DUMMYANDREF( const Lang::PathPoint3D ) arg1, DUMMYANDREF( const Lang::Coords3D ) arg2 ) const;
    RefCountPtr< const Lang::Value > impl( DUMMYANDREF( const Lang::PathSlider3D ) arg1, DUMMYANDREF( const Lang::Coords3D ) arg2 ) const;
  };
  
  class EqualExpr : public BinaryInfixExpr
  {
  public:
    EqualExpr( const Ast::SourceLocation & _opLoc, Ast::Expression * _expr1, Ast::Expression * _expr2 ) : BinaryInfixExpr( _opLoc, _expr1, _expr2 ) { }
    CALLIMPL( CLASSTREE1_Symbol, CLASSTREE2_Symbol );
    CALLIMPL( CLASSTREE1_Float, CLASSTREE2_Float );
    CALLIMPL( CLASSTREE1_Integer, CLASSTREE2_Integer );
    CALLIMPL( CLASSTREE1_Length, CLASSTREE2_Length );
    CALLIMPL( CLASSTREE1_Class, CLASSTREE2_Class );
  private:
    RefCountPtr< const Lang::Value > impl( DUMMYANDREF( const Lang::Symbol ) arg1, DUMMYANDREF( const Lang::Symbol ) arg2 ) const;
    RefCountPtr< const Lang::Value > impl( DUMMYANDREF( const Lang::Float ) arg1, DUMMYANDREF( const Lang::Float ) arg2 ) const;
    RefCountPtr< const Lang::Value > impl( DUMMYANDREF( const Lang::Integer ) arg1, DUMMYANDREF( const Lang::Integer ) arg2 ) const;
    RefCountPtr< const Lang::Value > impl( DUMMYANDREF( const Lang::Length ) arg1, DUMMYANDREF( const Lang::Length ) arg2 ) const;
    RefCountPtr< const Lang::Value > impl( DUMMYANDREF( const Lang::Class ) arg1, DUMMYANDREF( const Lang::Class ) arg2 ) const;
  };
  
  class NotEqualExpr : public BinaryInfixExpr
  {
  public:
    NotEqualExpr( const Ast::SourceLocation & _opLoc, Ast::Expression * _expr1, Ast::Expression * _expr2 ) : BinaryInfixExpr( _opLoc, _expr1, _expr2 ) { }
    CALLIMPL( CLASSTREE1_Symbol, CLASSTREE2_Symbol );
    CALLIMPL( CLASSTREE1_Float, CLASSTREE2_Float );
    CALLIMPL( CLASSTREE1_Integer, CLASSTREE2_Integer );
    CALLIMPL( CLASSTREE1_Length, CLASSTREE2_Length );
    CALLIMPL( CLASSTREE1_Class, CLASSTREE2_Class );
  private:
    RefCountPtr< const Lang::Value > impl( DUMMYANDREF( const Lang::Symbol ) arg1, DUMMYANDREF( const Lang::Symbol ) arg2 ) const;
    RefCountPtr< const Lang::Value > impl( DUMMYANDREF( const Lang::Float ) arg1, DUMMYANDREF( const Lang::Float ) arg2 ) const;
    RefCountPtr< const Lang::Value > impl( DUMMYANDREF( const Lang::Integer ) arg1, DUMMYANDREF( const Lang::Integer ) arg2 ) const;
    RefCountPtr< const Lang::Value > impl( DUMMYANDREF( const Lang::Length ) arg1, DUMMYANDREF( const Lang::Length ) arg2 ) const;
    RefCountPtr< const Lang::Value > impl( DUMMYANDREF( const Lang::Class ) arg1, DUMMYANDREF( const Lang::Class ) arg2 ) const;
  };
  
  class LessEqualExpr : public BinaryInfixExpr
  {
  public:
    LessEqualExpr( const Ast::SourceLocation & _opLoc, Ast::Expression * _expr1, Ast::Expression * _expr2 ) : BinaryInfixExpr( _opLoc, _expr1, _expr2 ) { }
    CALLIMPL( CLASSTREE1_Symbol, CLASSTREE2_Symbol );
    CALLIMPL( CLASSTREE1_Float, CLASSTREE2_Float );
    CALLIMPL( CLASSTREE1_Integer, CLASSTREE2_Integer );
    CALLIMPL( CLASSTREE1_Length, CLASSTREE2_Length );
  private:
    RefCountPtr< const Lang::Value > impl( DUMMYANDREF( const Lang::Symbol ) arg1, DUMMYANDREF( const Lang::Symbol ) arg2 ) const;
    RefCountPtr< const Lang::Value > impl( DUMMYANDREF( const Lang::Float ) arg1, DUMMYANDREF( const Lang::Float ) arg2 ) const;
    RefCountPtr< const Lang::Value > impl( DUMMYANDREF( const Lang::Integer ) arg1, DUMMYANDREF( const Lang::Integer ) arg2 ) const;
    RefCountPtr< const Lang::Value > impl( DUMMYANDREF( const Lang::Length ) arg1, DUMMYANDREF( const Lang::Length ) arg2 ) const;
  };
  
  class GreaterEqualExpr : public BinaryInfixExpr
  {
  public:
    GreaterEqualExpr( const Ast::SourceLocation & _opLoc, Ast::Expression * _expr1, Ast::Expression * _expr2 ) : BinaryInfixExpr( _opLoc, _expr1, _expr2 ) { }
    CALLIMPL( CLASSTREE1_Symbol, CLASSTREE2_Symbol );
    CALLIMPL( CLASSTREE1_Float, CLASSTREE2_Float );
    CALLIMPL( CLASSTREE1_Integer, CLASSTREE2_Integer );
    CALLIMPL( CLASSTREE1_Length, CLASSTREE2_Length );
  private:
    RefCountPtr< const Lang::Value > impl( DUMMYANDREF( const Lang::Symbol ) arg1, DUMMYANDREF( const Lang::Symbol ) arg2 ) const;
    RefCountPtr< const Lang::Value > impl( DUMMYANDREF( const Lang::Float ) arg1, DUMMYANDREF( const Lang::Float ) arg2 ) const;
    RefCountPtr< const Lang::Value > impl( DUMMYANDREF( const Lang::Integer ) arg1, DUMMYANDREF( const Lang::Integer ) arg2 ) const;
    RefCountPtr< const Lang::Value > impl( DUMMYANDREF( const Lang::Length ) arg1, DUMMYANDREF( const Lang::Length ) arg2 ) const;
  };
  
  class AndExpr : public BinaryInfixExpr
  {
  public:
    AndExpr( const Ast::SourceLocation & _opLoc, Ast::Expression * _expr1, Ast::Expression * _expr2 ) : BinaryInfixExpr( _opLoc, _expr1, _expr2 ) { }
    CALLIMPL( CLASSTREE1_Boolean, CLASSTREE2_Boolean );
  private:
    RefCountPtr< const Lang::Value > impl( DUMMYANDREF( const Lang::Boolean ) arg1, DUMMYANDREF( const Lang::Boolean ) arg2 ) const;
  };
  
  class OrExpr : public BinaryInfixExpr
  {
  public:
    OrExpr( const Ast::SourceLocation & _opLoc, Ast::Expression * _expr1, Ast::Expression * _expr2 ) : BinaryInfixExpr( _opLoc, _expr1, _expr2 ) { }
    CALLIMPL( CLASSTREE1_Boolean, CLASSTREE2_Boolean );
  private:
    RefCountPtr< const Lang::Value > impl( DUMMYANDREF( const Lang::Boolean ) arg1, DUMMYANDREF( const Lang::Boolean ) arg2 ) const;
  };
  
  class XorExpr : public BinaryInfixExpr
  {
  public:
    XorExpr( const Ast::SourceLocation & _opLoc, Ast::Expression * _expr1, Ast::Expression * _expr2 ) : BinaryInfixExpr( _opLoc, _expr1, _expr2 ) { }
    CALLIMPL( CLASSTREE1_Boolean, CLASSTREE2_Boolean );
  private:
    RefCountPtr< const Lang::Value > impl( DUMMYANDREF( const Lang::Boolean ) arg1, DUMMYANDREF( const Lang::Boolean ) arg2 ) const;
  };
 
  } 
}


#endif
