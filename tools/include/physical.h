#ifndef physical_h
#define physical_h

#include <iostream>
#include <cmath>

// There is one generic template and one specialization for < 0, 0 >
// The only difference is that Physical< 0, 0 > has a conversion operator to double defined.
// If this difference was not made by a template specialization, I can't see how to avoid
// compiler conversion ambiguities.
// An output operator was also added to the Physical< 0, 0 > specialization.  Other template
// instances must refer to the display method.

template< int L, int T >
class Physical
{
  double value_;

 public:
  explicit Physical( )
    : value_( 0 )
    { }
  Physical( double value )
    : value_( value )
    { }
  Physical( const Physical< L, T > & orig )
    : value_( orig.value_ )
    { }
  Physical< L, T > & operator = ( const Physical< L, T > & orig )
    {
      value_ = orig.value_;
      return *this;
    }
  void operator += ( const Physical< L, T > & p2 )
    {
      value_ += p2.value_;
    }
  void operator -= ( const Physical< L, T > & p2 )
    {
      value_ -= p2.value_;
    }
  void operator *= ( double scalar )
    {
      value_ *= scalar;
    }
  void operator /= ( double scalar )
    {
      value_ /= scalar;
    }
  bool operator == ( const Physical< L, T > & p2 ) const
    {
      return value_ == p2.value_;
    }
  bool operator < ( const Physical< L, T > & p2 ) const
    {
      return value_ < p2.value_;
    }
  bool operator <= ( const Physical< L, T > & p2 ) const
    {
      return value_ <= p2.value_;
    }
  bool operator > ( const Physical< L, T > & p2 ) const
    {
      return value_ > p2.value_;
    }
  bool operator >= ( const Physical< L, T > & p2 ) const
    {
      return value_ >= p2.value_;
    }
  bool operator != ( const Physical< L, T > & p2 ) const
    {
      return value_ != p2.value_;
    }

  Physical< L, T > operator - ( ) const
    {
      return Physical< L, T >( - value_ );
    }
  Physical< L, T > operator + ( const Physical< L, T > & p2 ) const
    {
      return Physical< L, T >( value_ + p2.value_ );
    }
  Physical< L, T > operator - ( const Physical< L, T > & p2 ) const
    {
      return operator + ( - p2 );
    }
  Physical< L, T >
    operator * ( double p2 ) const
    {
      return Physical< L, T >( value_ * p2 );
    }
  template< int L2, int T2 >
    Physical< L + L2, T + T2 >
    operator * ( const Physical< L2, T2 > & p2 ) const
    {
      return Physical< L + L2, T + T2 >( value_ * p2.publicAccessForInternalUse( ) );
    }
  Physical< L, T >
    operator / ( double p2 ) const
    {
      return Physical< L, T >( value_ / p2 );
    }
  template< int L2, int T2 >
    Physical< L - L2, T - T2 >
    operator / ( const Physical< L2, T2 > & p2 ) const
    {
      return Physical< L - L2, T - T2 >( value_ / p2.publicAccessForInternalUse( ) );
    }

  template< int L2, int T2 >
    Physical< L - L2, T - T2 >
    offtype( ) const
    {
      return Physical< L - L2, T - T2 >( value_ );
    }
  
  template< int L2, int T2 >
  static 
  Physical< L2 - L, T2 - T >
  offtype( Physical< L2, T2 > other )
  {
    return Physical< L2 - L, T2 - T >( other.value_ );
  }
  
  
  double publicAccessForInternalUse( ) const
    {
      return value_;
    }

  Physical< L, T > abs( ) const
    {
      return Physical< L, T >( fabs( value_ ) );
    }

  template< int L2, int T2 >
    friend
    Physical< L2, T2 >
    operator * ( double p1, const Physical< L2, T2 > & p2 );

  template< int L2, int T2 >
    friend
    Physical< -L2, -T2 >
    operator / ( double p1, const Physical< L2, T2 > & p2 );

  template< int L2, int T2 >
    friend Physical< L2, T2 > hypotPhysical( const Physical< L2, T2 > & p1, const Physical< L2, T2 > & p2 );
  template< int L2, int T2 >
    friend Physical< L2, T2 > hypotPhysical( const Physical< L2, T2 > & p1, const Physical< L2, T2 > & p2, const Physical< L2, T2 > & p3 );

  template< int L2, int T2 >
    friend Physical< L2, T2 > modPhysical( const Physical< L2, T2 > & p1, const Physical< L2, T2 > & p2 );
  template< int L2, int T2 >
    friend Physical< L2/2, T2/2 > sqrtPhysical( const Physical< L2, T2 > & p );

  template< int L2, int T2 >
    friend bool isinfPhysical( const Physical< L2, T2 > & p1 );

  void display( std::ostream & os ) const
    {
      os << "Physical< " << L << ", " << T << " >( " << value_ << " )" ;
    }

};


template< >
class Physical< 0, 0 >
{
  double value_;

 public:
  explicit Physical( )
    : value_( 0 )
    { }
  Physical( double value )
    : value_( value )
    { }
  Physical( const Physical< 0, 0 > & orig )
    : value_( orig.value_ )
    { }
  Physical< 0, 0 > & operator = ( const Physical< 0, 0 > & orig )
    {
      value_ = orig.value_;
      return *this;
    }
  void operator += ( const Physical< 0, 0 > & p2 )
    {
      value_ += p2.value_;
    }
  void operator -= ( const Physical< 0, 0 > & p2 )
    {
      value_ -= p2.value_;
    }
  void operator *= ( double scalar )
    {
      value_ *= scalar;
    }
  void operator /= ( double scalar )
    {
      value_ /= scalar;
    }
  bool operator == ( const Physical< 0, 0 > & p2 ) const
    {
      return value_ == p2.value_;
    }
  bool operator < ( const Physical< 0, 0 > & p2 ) const
    {
      return value_ < p2.value_;
    }
  bool operator <= ( const Physical< 0, 0 > & p2 ) const
    {
      return value_ <= p2.value_;
    }
  bool operator > ( const Physical< 0, 0 > & p2 ) const
    {
      return value_ > p2.value_;
    }
  bool operator >= ( const Physical< 0, 0 > & p2 ) const
    {
      return value_ >= p2.value_;
    }
  bool operator != ( const Physical< 0, 0 > & p2 ) const
    {
      return value_ != p2.value_;
    }

  Physical< 0, 0 > operator - ( ) const
    {
      return Physical< 0, 0 >( - value_ );
    }
  Physical< 0, 0 > operator + ( const Physical< 0, 0 > & p2 ) const
    {
      return Physical< 0, 0 >( value_ + p2.value_ );
    }
  Physical< 0, 0 > operator - ( const Physical< 0, 0 > & p2 ) const
    {
      return operator + ( - p2 );
    }
  Physical< 0, 0 >
    operator * ( double p2 ) const
    {
      return Physical< 0, 0 >( value_ * p2 );
    }
  template< int L2, int T2 >
    Physical< 0 + L2, 0 + T2 >
    operator * ( const Physical< L2, T2 > & p2 ) const
    {
      return Physical< 0 + L2, 0 + T2 >( value_ * p2.publicAccessForInternalUse( ) );
    }
  Physical< 0, 0 >
    operator / ( double p2 ) const
    {
      return Physical< 0, 0 >( value_ / p2 );
    }
  template< int L2, int T2 >
    Physical< 0 - L2, 0 - T2 >
    operator / ( const Physical< L2, T2 > & p2 ) const
    {
      return Physical< 0 - L2, 0 - T2 >( value_ / p2.publicAccessForInternalUse( ) );
    }

  template< int L2, int T2 >
    Physical< 0 - L2, 0 - T2 >
    offtype( ) const
    {
      return Physical< 0 - L2, 0 - T2 >( value_ );
    }
  
  
  double publicAccessForInternalUse( ) const
    {
      return value_;
    }

  Physical< 0, 0 > abs( ) const
    {
      return Physical< 0, 0 >( fabs( value_ ) );
    }

  template< int L2, int T2 >
    friend
    Physical< L2, T2 >
    operator * ( double p1, const Physical< L2, T2 > & p2 );

  template< int L2, int T2 >
    friend
    Physical< -L2, -T2 >
    operator / ( double p1, const Physical< L2, T2 > & p2 );

  friend std::ostream & operator << ( std::ostream& os, const Physical< 0, 0 > & p );

  operator double( ) const
  {
    return value_;
  }

  template< int L2, int T2 >
    friend Physical< L2, T2 > hypotPhysical( const Physical< L2, T2 > & p1, const Physical< L2, T2 > & p2 );
  template< int L2, int T2 >
    friend Physical< L2, T2 > hypotPhysical( const Physical< L2, T2 > & p1, const Physical< L2, T2 > & p2, const Physical< L2, T2 > & p3 );

  template< int L2, int T2 >
    friend Physical< L2, T2 > modPhysical( const Physical< L2, T2 > & p1, const Physical< L2, T2 > & p2 );
  template< int L2, int T2 >
    friend Physical< L2/2, T2/2 > sqrtPhysical( const Physical< L2, T2 > & p );

  template< int L2, int T2 >
    friend bool isinfPhysical( const Physical< L2, T2 > & p1 );

  void display( std::ostream & os ) const
    {
      os << "Physical< " << 0 << ", " << 0 << " >( " << value_ << " )" ;
    }

  static double hypot3( double x, double y, double z )
  {
    return sqrt( x * x + y * y + z * z );
  }
};


template< int L2, int T2 >
  Physical< L2, T2 >
  operator * ( double p1, const Physical< L2, T2 > & p2 )
{
  return Physical< L2, T2 >( p1 * p2.value_ );
}

template< int L2, int T2 >
  Physical< -L2, -T2 >
  operator / ( double p1, const Physical< L2, T2 > & p2 )
{
  return Physical< -L2, -T2 >( p1 / p2.value_ );
}

template< int L, int T >
  Physical< L, T >
  hypotPhysical( const Physical< L, T > & p1, const Physical< L, T > & p2 )
{
  return Physical< L, T >( hypot( p1.value_, p2.value_ ) );
}

template< int L, int T >
  Physical< L, T >
  hypotPhysical( const Physical< L, T > & p1, const Physical< L, T > & p2, const Physical< L, T > & p3 )
{
  return Physical< L, T >( Physical< 0, 0 >::hypot3( p1.value_, p2.value_, p3.value_ ) );
}

template< int L, int T >
Physical< L, T >
modPhysical( const Physical< L, T > & p1, const Physical< L, T > & p2 )
{
  return Physical< L, T >( fmod( p1.value_, p2.value_ ) );
}

template< int L, int T >
Physical< L/2, T/2 >
sqrtPhysical( const Physical< L, T > & p )
{
  return Physical< L/2, T/2 >( sqrt( p.value_ ) );
}

template< int L2, int T2 >
  bool
  isinfPhysical( const Physical< L2, T2 > & p1 )
{
  return isinf( p1.value_ );
}


#endif
