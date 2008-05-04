#ifndef autoonoff_h
#define autoonoff_h

template<class T>
class DeleteOnExit
{
  T *mem;
public:
  DeleteOnExit(T* _mem) : mem(_mem) { }
  ~DeleteOnExit(){ delete mem; }
};

void autoonoff__exitWithMessage( const char * msg );

template<class T>
class PossiblyDeleteOnExit
{
  T *mem;
public:
  PossiblyDeleteOnExit( ) : mem( 0 ) { }
  void activate( T* _mem )
    {
      if( mem != 0 )
	{
	  autoonoff__exitWithMessage( "This PossiblyDeleteOnExit is already in use." );
	}
      mem = _mem;
    }
  ~PossiblyDeleteOnExit( ){ if( mem != 0 ){ delete mem; } }
};

template<class T>
class DeleteArrayOnExit
{
  T *mem;
public:
  DeleteArrayOnExit(T* _mem) : mem(_mem) { }
  ~DeleteArrayOnExit(){ delete [] mem; }
};

template<class T>
class AutoSetReset
{
  T oldVal;
  T* var;
 public:
  AutoSetReset(T* _var, T newVal)
    : oldVal(*_var), var(_var)
    {
      (*var) = newVal;
    }
  ~AutoSetReset()
    {
      (*var) = oldVal;
    }
};

template<class T>
class AutoResetOnExit
{
  T oldVal;
  T* var;
 public:
  AutoResetOnExit(T* _var )
    : oldVal(*_var), var(_var)
    { }
  ~AutoResetOnExit()
    {
      (*var) = oldVal;
    }
};

template<class T>
class AutoSetOnExit
{
  T newVal;
  T* var;
 public:
  AutoSetOnExit(T* _var, T _newVal)
    : newVal(_newVal), var(_var)
    { }
  ~AutoSetOnExit()
    {
      (*var) = newVal;
    }
};

class HelloGoodbye
{
  static int indent;
  static const int TABSIZE = 2;
  const char * mess;
 public:
  HelloGoodbye( const char * _mess );
  ~HelloGoodbye();
};

class Hello
{
 public:
  Hello( const char * mess );
  ~Hello();
};

class Goodbye
{
  const char * mess;
 public:
  Goodbye( const char * _mess );
  ~Goodbye();
};


#endif
