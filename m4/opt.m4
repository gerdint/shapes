AC_DEFUN([AC_DEBUG],
[
  AC_REQUIRE([AC_PROG_CC])
  AC_REQUIRE([AC_PROG_CXX])
  AC_MSG_CHECKING(for debug information level)
  AC_ARG_ENABLE([debug],
    [AC_HELP_STRING([--enable-debug],[Enable debugging symbols in objects])])
  if test "x$enable_debug" = "xno" ; then
    CFLAGS=`echo $CFLAGS | sed 's,-g[[:graph:]]*,,g'`
    CXXFLAGS=`echo $CXXFLAGS | sed 's,-g[[:graph:]]*,,g'`
  elif test "x$enable_debug" = "xyes" ; then
    case $CXXFLAGS in
    *-g*) ;;
    *)    CXXFLAGS="$CXXFLAGS -gdwarf-2" ;;
    esac
    case $CFLAGS in
    *-g*) ;;
    *)    CFLAGS="$CFLAGS -gdwarf-2" ;;
    esac
  else
    test -z $enable_debug && enable_debug=dwarf-2
    CFLAGS=`echo   $CFLAGS   | sed "s,-g[[:graph:]]*,-g$enable_debug,g"`
    CXXFLAGS=`echo $CXXFLAGS | sed "s,-g[[:graph:]]*,-g$enable_debug,g"`
  fi
	  AC_MSG_RESULT($enable_debug 'CFLAGS=$CFLAGS')
])


AC_DEFUN([AC_PROFILE],
[
  AC_REQUIRE([AC_PROG_CC])
  AC_REQUIRE([AC_PROG_CXX])
  AC_MSG_CHECKING(for profiling information)
  AC_ARG_ENABLE([profile],
    [AC_HELP_STRING([--enable-profile],[Enable profiling information])])
  if test "x$enable_profile" = "xno" ; then
    CFLAGS=`echo $CFLAGS | sed 's,-p[[:graph:]]*,,g'`
    CXXFLAGS=`echo $CXXFLAGS | sed 's,-p[[:graph:]]*,,g'`
  elif test "x$enable_profile" = "xyes" ; then
    case $CXXFLAGS in
    *-p*) ;;
    *)    CXXFLAGS="$CXXFLAGS -pg" ;;
    esac
    case $CFLAGS in
    *-p*) ;;
    *)    CFLAGS="$CFLAGS -pg" ;;
    esac
  else
    test -z $enable_profile && enable_profile=g
    CFLAGS=`echo   $CFLAGS   | sed "s,-p[[:graph:]]*,-p$enable_profile,g"`
    CXXFLAGS=`echo $CXXFLAGS | sed "s,-p[[:graph:]]*,-p$enable_profile,g"`
  fi
	  AC_MSG_RESULT($enable_profile 'CFLAGS=$CFLAGS')
])


AC_DEFUN([AC_OPTIMIZATION],
[
  AC_REQUIRE([AC_PROG_CC])
  AC_REQUIRE([AC_PROG_CXX])

  # enable_optimization=yes
  AC_ARG_ENABLE([optimization],
    [AC_HELP_STRING([--enable-optimization],[Enable optimization of objects])])
  AC_MSG_CHECKING(for optimiztion level)

  changequote(<<, >>)dnl
  if test "x$enable_optimization" = "xno" ; then
    CFLAGS=`echo   $CFLAGS   | sed "s,-O[[:graph:]]*,,g"`
    CXXFLAGS=`echo $CXXFLAGS | sed "s,-O[[:graph:]]*,,g"`
  elif test "x$enable_optimization" = "xyes" ; then
    CFLAGS=`echo   $CFLAGS   | sed "s,-O[[:graph:]]*,,g"`
    CXXFLAGS=`echo $CXXFLAGS | sed "s,-O[[:graph:]]*,,g"`
    case $CXXFLAGS in
    *-O*) ;;
    *)    CXXFLAGS="$CXXFLAGS -O3" ;;
    esac
    case $CFLAGS in
    *-O*) ;;
    *)    CFLAGS="$CXXFLAGS -O3" ;;
    esac
  else
    test -z $enable_optimization || enable_optimization=3
    CFLAGS=`echo   "$CFLAGS"   | sed "s,-O[[:graph:]]*,-O$enable_optimization,g"`
    CXXFLAGS=`echo "$CXXFLAGS" | sed "s,-O[[:graph:]]*,-O$enable_optimization,g"`
  fi
  changequote([, ])dnl
  AC_MSG_RESULT($enable_optimization "CFLAGS=$CFLAGS")
])
