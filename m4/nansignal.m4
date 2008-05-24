AC_DEFUN([AC_CHECK_NANSIGNAL],
[
	AC_MSG_CHECKING([whether doubles signal NaN])
	AC_LANG_PUSH(C++)
	AC_RUN_IFELSE([
#include <limits>
#include <cstdlib>

int main()
{
	exit(!std::numeric_limits<double>::has_signaling_NaN);
}
], [AC_MSG_RESULT(yes)],
	 [
		AC_MSG_RESULT(no)
		AC_MSG_ERROR([cannot compile Shapes without NaN signals, aborting!])
	 ],
	 [AC_MSG_RESULT(maybe)])

	AC_LANG_POP(C++)
])


