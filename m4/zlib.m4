dnl Check to find the zlib headers/libraries

AC_DEFUN([AM_PATH_ZLIB],
[
  AC_ARG_WITH(zlib-include,
    [  --with-zlib-include=DIR Where zlib headers are installed],
    ZLIB_CFLAGS="-I$withval", ZLIB_CFLAGS="" )

  AC_ARG_WITH(zlib-lib,
    [  --with-zlib-lib=DIR     Where the zlib library is installed],
    ZLIB_LIBS="-L$withval", ZLIB_LIBS="" )

  ac_save_CFLAGS="$CFLAGS"
  ac_save_LIBS="$LIBS"

  if test "x${ZLIB_CFLAGS}X" != xX ; then
    CFLAGS = "$ZLIB_CFLAGS"
  fi
  if test "x${ZLIB_LIBS}X" != xX ; then
    LIBS = "$ZLIB_LIBS"
  fi

  AC_CHECK_HEADERS(zlib.h,
    [],
    [AC_MSG_ERROR("ZLIB header files not found.")]
  )

  AC_CHECK_LIB(z, compress2,
    [ZLIB_LIBS="$ZLIB_LIBS -lz"],
    [AC_MSG_ERROR("ZLIB libraries not found.")]
  )
  CFLAGS="$ac_save_CFLAGS"
  LIBS="$ac_save_LIBS"
  AC_SUBST(ZLIB_CFLAGS)
  AC_SUBST(ZLIB_LIBS)
])