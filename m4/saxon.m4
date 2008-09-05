AC_DEFUN([AM_PATH_SAXON],
[
	AC_MSG_CHECKING(for SAXON)
	AC_ARG_WITH(saxon-path,[  --with-saxon-path=PATH   Path where SAXON is installed (optional)],
		saxon_path="$withval", saxon_path="$SAXON_PATH")

	SAXON="java -jar $saxon_path/saxon9.jar"

	have_saxon='no'
	if $SAXON "-?" > /dev/null 2>&1
	then
		saxon_ver=`$SAXON '-?' 2>&1 |
		           sed -n -e '1s/Saxon \([[0-9]]\)\.[[0-9]][[.0-9]]*.*/\1/p'`
		test "$saxon_ver" -ge "$1" && have_saxon='yes'
	fi

	if test $have_saxon = 'yes'
	then
		AC_SUBST(HAVE_SAXON, 1)
	else
		AC_SUBST(HAVE_SAXON, 0)
	fi
	AC_SUBST(SAXON)

	AC_MSG_RESULT($have_saxon)
])
