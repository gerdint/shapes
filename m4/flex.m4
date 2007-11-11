AC_DEFUN([AC_PROG_FLEX],
[
  AM_PROG_LEX
  min_flex_version=ifelse([$1], ,2.5.30,$1)
  AC_MSG_CHECKING(for flex - version >= $min_flex_version)
  if test "$LEX" != flex; then
    LEX="$SHELL $missing_dir/missing flex"
    AC_SUBST([LEX_OUTPUT_ROOT], [lex.yy])
    AC_SUBST([LEXLIB], [''])
#    echo "WARNING: `flex'' is missing on your system.  You should only need it if"
#    echo "         you modified a `.ll' file.  You may need the `Flex'' package"
#    echo "         in order for those modifications to take effect.  You can get"
#    echo "         `Flex'' from any GNU archive site."
    AC_MSG_RESULT(no)
  else
    flex_version=`$LEX --version | sed -e 's/^flex //' -e 's/version //'`

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
  tmp_version = my_strdup("$min_flex_version");
  n = sscanf(tmp_version, "%d.%d.%d", &min_major, &min_minor, &min_micro);

  if (n != 2 && n != 3) {
     printf("%s, bad minimum version string\n", "$min_flex_version");
     exit(1);
   }

  tmp_version = my_strdup("$flex_version");
  n = sscanf(tmp_version, "%d.%d.%d", &major, &minor, &micro) ;

  if (n != 2 && n != 3) {
     printf("%d, %s, bad version string\n", n, "$flex_version");
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
/*
	printf("WARNING: \`flex' on your system reports version %d.%d.%d, but version.\n"
	       "         %d.%d.%d are required.  You should only need it if you\n"
               "         modified a .ll file.  You may need the \`Flex' package\n"
               "         in order for those modifications to take effect.  You can get\n"
               "         \`Flex' from any GNU archive site.",
	       major, minor, micro, min_major, min_minor, min_micro);
*/
      exit(1);
    }
}
    ], AC_MSG_RESULT(yes), [
      LEX="$SHELL $missing_dir/missing flex"
      AC_SUBST([LEX_OUTPUT_ROOT], [lex.yy])
      AC_SUBST([LEXLIB], [''])
      AC_MSG_RESULT(no)
    ],)
  fi
])