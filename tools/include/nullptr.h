#ifndef nullptr_h
#define nullptr_h


template<class T>
class NullPtr
{
 public:
  NullPtr()
    { }
  operator T * () const { return 0; }
};


#endif
