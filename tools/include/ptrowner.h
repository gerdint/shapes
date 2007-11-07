#ifndef ptrowner_h
#define ptrowner_h


template<class T>
class PtrOwner_top_Access : public T
{
 public:
  PtrOwner_top_Access( )
    { }
  void clear( )
    {
      while( T::size( ) > 0 )
	{
	  delete T::top( );
	  T::pop( );
	}
    }
  ~PtrOwner_top_Access( )
    {
      clear( );
    }
};

template<class T>
class PtrOwner_back_Access : public T
{
 public:
  PtrOwner_back_Access( )
    { }
  void clear( )
    {
      while( T::size( ) > 0 )
	{
	  delete T::back( );
	  T::pop_back( );
	}
    }
  ~PtrOwner_back_Access( )
    {
      clear( );
    }
};

template<class T>
class PtrOwner_front_Access : public T
{
 public:
  PtrOwner_front_Access( )
    { }
  void clear( )
    {
      while( T::size( ) > 0 )
	{
	  delete T::front( );
	  T::pop_front( );
	}
    }
  ~PtrOwner_front_Access( )
    {
      clear( );
    }
};

template<class T>
class PtrOwner_begin_clear_Access : public T
{
 public:
  PtrOwner_begin_clear_Access( )
    { }
  void clear( )
  {
    for( typename T::iterator i = T::begin( ); i != T::end( ); ++i )
      {
	delete *i;
      }
    T::clear( );
  }
  ~PtrOwner_begin_clear_Access( )
    {
      clear( );
    }
};


#endif
