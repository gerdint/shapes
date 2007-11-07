#ifndef refcount_h
#define refcount_h


#include "nullptr.h"
#include <string>  // for exceptions

/* The type that corresponds to
 *   T *
 * is 
 *   const RefCountPtr<T>
 * and the type that corresponds to
 *   const T *
 * is 
 *   const RefCountPtr<const T>
 *
 * It is important that the added const is supported, so that functions that would
 * take pointers as arguments now can take RefCountPtr objects by const reference.  To this
 * end, many methods need be const, even though they return references to a non-const
 * object.
 */
/* The template specializations seem to follow wrong syntax in gcc
 */
template<class T>
class RefCountPtr
{
 public:
  typedef T ValueType;
 private:
  mutable T * ptr;
  mutable size_t * refCount;
 public:
  RefCountPtr<T>( );
  explicit RefCountPtr<T>( T * _value );
  explicit RefCountPtr<T>( T * _value, size_t * _refCount );
  explicit RefCountPtr<T>( const T& _value );
  RefCountPtr<T>( const NullPtr<T>& null );
  template<class S> // We can build from subclass objects
    RefCountPtr<T>( const RefCountPtr<S>& orig );
#ifdef __SUNPRO_CC
#else
   // ... and from objects of same class, damn it!
  RefCountPtr<T>( const RefCountPtr<T>& orig );
#endif
  /***********************
   * Warning!
   * There is a problem with the type conversion below.
   * Since the class definition does not tell that this method
   * can only be implemented for superclasses, the compiler may
   * too long believe that a cast may be performed to any type,
   * causing ambiguity.
   ***********************/
  //  template<class S> // We can convert to superclass objects
  //      operator RefCountPtr<S> ( ) const;
  template<class S> // We can assign from subclass objects
    RefCountPtr<T>& operator = ( const RefCountPtr<S>& orig );
#ifdef __SUNPRO_CC
#else
   // ... and from objects of same class, damn it!
  RefCountPtr<T>& operator = ( const RefCountPtr<T>& orig );
#endif
  template<class S>
    RefCountPtr<S> down_cast( );
  template<class S>
    RefCountPtr<S> down_cast( ) const;
  template<class S>
    RefCountPtr<S> hard_cast( );
  template<class S>
    RefCountPtr<S> hard_cast( ) const;
  template<class S>
    RefCountPtr<S> unconst_cast( ) const;
  void swap( RefCountPtr<T> & o2 );
  ~RefCountPtr();
  RefCountPtr<T> clone( ) const;
  const T& operator * ( ) const;
  T& operator * ();
  const T * operator -> ( ) const;
  T * operator -> ( );  
  bool operator == ( const RefCountPtr<T>& o2 ) const;
  bool operator != ( const RefCountPtr<T>& o2 ) const;
  bool operator == ( const NullPtr<T>& theNull ) const;
  bool operator != ( const NullPtr<T>& theNull ) const;
  bool operator < ( const RefCountPtr<T>& o2 ) const;
  bool operator > ( const RefCountPtr<T>& o2 ) const;
  //  const T * getPtr( ) const;
  T * getPtr( ) const;
  size_t * getCounterPtr( ) const;
  void abortPtr( );
};

template<class T>
std::ostream &
operator << ( std::ostream & os, const RefCountPtr< T > & self )
{
  os << self.getPtr( ) ;
  return os;
}


template<class T>
RefCountPtr<T>::RefCountPtr( )
  : ptr( new T ), refCount( new size_t( 1 ) )
{ }

template<class T>
RefCountPtr<T>::RefCountPtr( T * _value )
  : ptr( _value ), refCount( new size_t( 1 ) )
{ }

template<class T>
RefCountPtr<T>::RefCountPtr( T * _value, size_t * _refCount )
  : ptr( _value ), refCount( _refCount )
{
  if( ptr != 0 )
    {
      ++(*refCount);
    }
}

template<class T>
RefCountPtr<T>::RefCountPtr( const T & _value )
{
  ptr = new T( _value );
  refCount = new size_t( 1 );
}

template<class T>
RefCountPtr<T>::RefCountPtr( const NullPtr<T> & null )
  : ptr( 0 ), refCount( 0 )
{ }

template<class T>
template<class S>
RefCountPtr<T>::RefCountPtr( const RefCountPtr<S> & orig )
  : ptr( orig.getPtr( ) ), refCount( orig.getCounterPtr( ) )
{
  if( ptr != 0 )
    {
      ++(*refCount);
    }
}

#ifdef __SUNPRO_CC
template<class T>
template<>
RefCountPtr<T>::RefCountPtr<T><T>( const RefCountPtr<T> & orig )
#else
template<class T>
RefCountPtr<T>::RefCountPtr( const RefCountPtr<T> & orig )
#endif
  : ptr( orig.ptr ), refCount( orig.refCount )
{
  if( ptr != 0 )
    {
      ++(*refCount);
    }
}

// template<class T>
// template<class S> // Conversion to superclass
// RefCountPtr<T>::operator RefCountPtr<S> ( ) const
// {
//   return RefCountPtr<S>( *this );
// }

/* Assignment does not change the refCount
 */
template<class T>
template<class S>
RefCountPtr<T>& RefCountPtr<T>::operator = ( const RefCountPtr<S>& orig )
{
  if( ptr == static_cast< T * >( orig.getPtr( ) ) )
    return *this;

  if( ptr != 0 &&
      (--(*refCount)) == 0 )
    {
      delete ptr;
      delete refCount;
    }

  ptr = static_cast< T * >( orig.getPtr( ) );
  refCount = orig.getCounterPtr( );
  
  if( ptr != 0 )
    {
      ++(*refCount);
    }

  return *this;
}

#ifdef __SUNPRO_CC
template<class T>
template<>
RefCountPtr<T>& RefCountPtr<T>::operator = <T> ( const RefCountPtr<T>& orig )
#else
template<class T>
RefCountPtr<T>& RefCountPtr<T>::operator = ( const RefCountPtr<T>& orig )
#endif
{
  if( ptr == orig.ptr )
    return *this;

  if( ptr != 0 &&
      (--(*refCount)) == 0 )
    {
      delete ptr;
      delete refCount;
    }

  ptr = orig.ptr;
  refCount = orig.refCount;
  
  if( ptr != 0 )
    {
      ++(*refCount);
    }

  return *this;
}

template<class T>
template<class S>
RefCountPtr<S> RefCountPtr<T>::down_cast( )
{
  RefCountPtr< S > res( dynamic_cast< S * >( ptr ), refCount );
  if( res.getPtr( ) == 0 )
    {
      return RefCountPtr< S >( NullPtr< S >( ) );
    }
  return res;
}

template<class T>
template<class S>
RefCountPtr<S> RefCountPtr<T>::down_cast( ) const
{
  RefCountPtr< S > res( dynamic_cast< S * >( ptr ), refCount );
  if( res.getPtr( ) == 0 )
    {
      return RefCountPtr< S >( NullPtr< S >( ) );
    }
  return res;
}

template<class T>
template<class S>
RefCountPtr<S> RefCountPtr<T>::hard_cast( )
{
  RefCountPtr< S > res( reinterpret_cast< S * >( ptr ), refCount );
  return res;
}

template<class T>
template<class S>
RefCountPtr<S> RefCountPtr<T>::hard_cast( ) const
{
  RefCountPtr< S > res( reinterpret_cast< S * >( ptr ), refCount );
  return res;
}

template<class T>
template<class S>
RefCountPtr<S> RefCountPtr<T>::unconst_cast( ) const
{
  RefCountPtr< S > res( const_cast< S * >( ptr ), refCount );
  return res;
}


template<class T>
void RefCountPtr<T>::swap( RefCountPtr<T> & o2 )
{
  T * tmpPtr = ptr;
  size_t * tmpRefCount = refCount;
  ptr = o2.ptr;
  refCount = o2.refCount;
  o2.ptr = tmpPtr;
  o2.refCount = tmpRefCount;
}

template<class T>
RefCountPtr<T>::~RefCountPtr( )
{
  if( ptr != 0 &&
      (--(*refCount)) == 0 )
    {
      delete ptr;
      delete refCount;
    }
}

template<class T>
RefCountPtr<T> RefCountPtr<T>::clone( ) const
{
  RefCountPtr<T> copy( *ptr );
  return copy;
}

template<class T>
const T& RefCountPtr<T>::operator * ( ) const
{
  return *ptr;
}

template<class T>
T& RefCountPtr<T>::operator * ( )
{
  return *ptr;
}

template<class T>
const T * RefCountPtr<T>::operator -> ( ) const
{
  return ptr;
}

template<class T>
T *
RefCountPtr<T>::operator -> ( )
{
  return ptr;
}

template<class T>
bool
RefCountPtr<T>::operator == ( const RefCountPtr<T>& o2 ) const
{
  return ptr == o2.ptr;
}

template<class T>
bool
RefCountPtr<T>::operator != ( const RefCountPtr<T>& o2 ) const
{
  return ptr != o2.ptr;
}

template<class T>
bool
RefCountPtr<T>::operator == ( const NullPtr<T>& theNull ) const
{
  return ptr == 0;
}

template<class T>
bool
RefCountPtr<T>::operator != ( const NullPtr<T>& theNull ) const
{
  return ptr != 0;
}

template<class T>
bool
RefCountPtr<T>::operator < ( const RefCountPtr<T>& o2 ) const
{
  return ptr < o2.ptr;
}

template<class T>
bool
RefCountPtr<T>::operator > ( const RefCountPtr<T>& o2 ) const
{
  return ptr > o2.ptr;
}

/*
template<class T>
const T * RefCountPtr<T>::getPtr( ) const
{
  return ptr;
}
*/

template<class T>
T *
RefCountPtr<T>::getPtr( ) const
{
  return reinterpret_cast< T * >( ptr );
}

template<class T>
size_t *
RefCountPtr<T>::getCounterPtr( ) const
{
  return refCount;
}

template<class T>
void
RefCountPtr<T>::abortPtr( )
{
  ptr = 0;
  refCount = 0;
}

#endif
