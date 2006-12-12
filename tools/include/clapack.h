#ifndef clapack_h
#define clapack_h


typedef double __CLPK_doublereal;
typedef int __CLPK_integer;

// From clapack.h:
int dgesvd_( char *jobu, char *jobvt, __CLPK_integer *m, __CLPK_integer *n, 
	     __CLPK_doublereal *a, __CLPK_integer *lda,
	     __CLPK_doublereal *s,
	     __CLPK_doublereal *u, __CLPK_integer *ldu,
	     __CLPK_doublereal *vt, __CLPK_integer *ldvt,
	     __CLPK_doublereal *work, __CLPK_integer *lwork, 
	     __CLPK_integer *info);


#endif
