#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

AC_PREREQ(2.59)
AC_INIT(Shapes, 0.3.8, tidefelt@isy.liu.se)
AC_CONFIG_AUX_DIR([config])
AM_INIT_AUTOMAKE([foreign])

AC_CONFIG_SRCDIR([source/Shapes_Kernel_decls.h])
AM_CONFIG_HEADER([config.h])

# Checks for programs.
AC_PROG_CXX
AC_PROG_CC
AC_PROG_FLEX(2.5.33)
AC_PROG_BISON(2.3)
AM_PATH_SAXON(9)
AC_PROG_LIBTOOL

# Set compiler options
AC_DEBUG
AC_PROFILE
AC_OPTIMIZATION
AC_CHECK_PROG([COMPARE], [compare], [yes], [no])

# Checks for libraries.
AM_ICONV()
AH_VERBATIM([ICONV_CAST],
[/* Macro to handle different const convention in iconv calls */
#define ICONV_CAST( ptr ) const_cast< ICONV_CONST char ** > ( ptr )
])
AC_CHECK_LIB([c], [close])
AM_PATH_GSL([1.8],, AC_MSG_ERROR(GSL not found))
AC_CHECK_LIB([m], [cos])
AM_PATH_ZLIB()
AC_CHECK_NANSIGNAL()


# Checks for header files.
AC_FUNC_ALLOCA
AC_HEADER_STDC
AC_HEADER_SYS_WAIT
AC_CHECK_HEADERS([inttypes.h libintl.h limits.h malloc.h stddef.h stdlib.h string.h sys/time.h unistd.h])

# Checks for typedefs, structures, and compiler characteristics.
AC_HEADER_STDBOOL
AC_C_CONST
AC_C_INLINE
AC_TYPE_PID_T
AC_C_RESTRICT
AC_TYPE_SIZE_T
AC_HEADER_TIME
AC_STRUCT_TM

# Checks for library functions.
AC_FUNC_ERROR_AT_LINE
AC_FUNC_FORK
AC_FUNC_MALLOC
AC_FUNC_REALLOC
AC_FUNC_STAT
AC_FUNC_STRFTIME
AC_FUNC_STRTOD
AC_CHECK_FUNCS([floor memchr memset pow rint sqrt strchr strdup strrchr strtol])

AC_CONFIG_FILES([Makefile source/Makefile resources/Makefile
                 test/Makefile test/text/Makefile test/graphics/Makefile
                 doc/Makefile doc/output/Makefile doc/tools/Makefile
                 doc/tools/ssi/Makefile doc/tools/postproc/Makefile
                 doc/build/Makefile doc/build/man/Makefile])
AC_OUTPUT

if test "X$COMPARE" != "Xyes"
then
  echo "\
WARNING: The program \`compare' was not found, without it installed
         the tests are likely to fail."
fi

if test "X$HAVE_SAXON" != "X1"
then
  echo "\
WARNING: The program \`Saxon' was not found, without it installed
         the documentation cannot be rebuilt."
fi
