#ifndef check_h
#define check_h

#ifdef NOCHECK
#define CHECK( expr )
#else
#define CHECK( expr ) expr
#endif

#endif
