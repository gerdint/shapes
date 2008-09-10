AC_DEFUN([AC_PROG_COMPARE],
[
  AC_ARG_WITH([compare],
              [AS_HELP_STRING([--without-compare],
              [disable use of compare to validate graphical output from tests])],
            [],
            [with_compare=yes])

  if test "X$with_compare" == "Xyes"
  then
    AC_CHECK_PROG([COMPARE], [compare], [yes], [no])
  else
    COMPARE=no
  fi

if test "X$COMPARE" == "Xyes"
then
  min_ImageMagick_version=ifelse([$1], ,6.4.0,$1)
  AC_MSG_CHECKING(for ImageMagick compare - version >= $min_ImageMagick_version)
  ImageMagick_version=`compare -version | sed -e '2,3d' -e 's!.*ImageMagick \([[^ ]]*\).*!\1!'`

  AC_TRY_RUN([
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

char* my_strdup (const char *str);

char* my_strdup (const char *str)
{
  char *new_str;

  if (str)
    {
      new_str = (char *)malloc ((strlen (str) + 1) * sizeof(char));
      strcpy (new_str, str);
    }
  else
    new_str = NULL;

  return new_str;
}

int main (void)
{
  int major = 0, minor = 0, micro = 0;
  int min_major = 0, min_minor = 0, min_micro = 0;
  int n;
  char *tmp_version;

  /* HP/UX 9 (%@#!) writes to sscanf strings */
  tmp_version = my_strdup("$min_ImageMagick_version");
  n = sscanf(tmp_version, "%d.%d.%d", &min_major, &min_minor, &min_micro);

  if (n != 2 && n != 3) {
     printf("%s, bad minimum version string\n", "$min_ImageMagick_version");
     exit(1);
   }

  tmp_version = my_strdup("$ImageMagick_version");
  n = sscanf(tmp_version, "%d.%d.%d", &major, &minor, &micro) ;

  if (n != 2 && n != 3) {
     printf("%d, %s, bad version string\n", n, "$ImageMagick_version");
     exit(1);
   }

  if ((major > min_major) ||
      ((major == min_major) && (minor > min_minor)) ||
      ((major == min_major) && (minor == min_minor) && (micro >= min_micro)))
    {
      exit(0);
    }
  else
    {
      exit(1);
    }
}
    ],
    [
		  COMPARE=yes
      AC_MSG_RESULT(yes)
    ],[
		  COMPARE=no
			AC_MSG_RESULT(no)
		],)
	AC_SUBST([COMPARE])
fi
])
