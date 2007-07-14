#include "upsamplers.h"

using namespace MetaPDF;

void
Computation::UpsampleInflections::operator () ( std::vector< double > * dst, const Bezier::ControlPoints< Concrete::Coords2D > & controls ) const
{
  if( controls.p1_ == controls.p0_ &&
      controls.p2_ == controls.p3_ )
    {
      // There are no inflections on a straight segment.
      return;
    }

  Bezier::PolyCoeffs< Concrete::Coords2D > coeffs( controls );
  double t[3];
  coeffs.inflections( t );
  // Now, we must just ensure that the times are sorted.
  double * src = & t[0];
  if( *src == HUGE_VAL )
    {
      // There were no inflections.
      return;
    }
  ++src;
  if( *src == HUGE_VAL )
    {
      // There were one inflections.
      dst->push_back( t[0] );
      return;
    }
  // There were two inflections.
  if( t[0] <= t[1] )
    {
      dst->push_back( t[0] );
      dst->push_back( t[1] );
    }
  else
    {
      dst->push_back( t[1] );
      dst->push_back( t[0] );
    }
}

void
Computation::UpsampleBends::operator () ( std::vector< double > * dst, const Bezier::ControlPoints< Concrete::Coords2D > & controls ) const
{
  if( controls.p1_ == controls.p0_ &&
      controls.p2_ == controls.p3_ )
    {
      // There are no bends on a straight segment.
      return;
    }

  throw Exceptions::NotImplemented( "Computation::UpsampleBends" );
}

void
Computation::UpsampleEvery2D::operator () ( std::vector< double > * dst, const Bezier::ControlPoints< Concrete::Coords2D > & controls ) const
{
  throw Exceptions::NotImplemented( "Computation::UpsampleEvery2D" );
}

void
Computation::UpsampleEvery3D::operator () ( std::vector< double > * dst, const Bezier::ControlPoints< Concrete::Coords3D > & controls ) const
{
  throw Exceptions::NotImplemented( "Computation::UpsampleEvery3D" );
}
