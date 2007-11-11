AC_DEFUN([AC_PROG_BISON],
[
  AC_PROG_YACC
  min_bison_version=ifelse([$1], ,2.0,$1)
  AC_MSG_CHECKING(for bison - version >= $min_bison_version)
  if test `echo ${YACC} | sed -e 's/\(bison\).*/\1/'` != bison ; then
    BISON="$SHELL $missing_dir/missing bison"
#    echo "WARNING: `bison'' is missing on your system.  You should only need it if"
#    echo "         you modified a `.yy' file.  You may need the `Bison'' package"
#    echo "         in order for those modifications to take effect.  You can get"
#    echo "         `Bison'' from any GNU archive site."
    AC_MSG_RESULT(no)
  else
    bison_version=`$YACC --version | sed -e '2,$d' -e 's/^bison (GNU Bison) //'`

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
  int major = 0, minor = 0;
  int min_major = 0, min_minor = 0;
  int n;
  char *tmp_version;

  /* HP/UX 9 (%@#!) writes to sscanf strings */
  tmp_version = my_strdup("$min_bison_version");
  n = sscanf(tmp_version, "%d.%d", &min_major, &min_minor);

  if (n != 2) {
     printf("%s, bad minimum version string\n", "$min_bison_version");
     exit(1);
   }

  tmp_version = my_strdup("$bison_version");
  n = sscanf(tmp_version, "%d.%d", &major, &minor) ;

  if (n != 2) {
     printf("%d, %s, bad version string\n", n, "$bison_version");
     exit(1);
   }

  if ((major > min_major) ||
      ((major == min_major) && (minor >= min_minor)))
    {
      exit(0);
    }
  else
    {
/*
	printf("WARNING: \`bison' on your system reports version %d.%d.%d, but version.\n"
	       "         %d.%d.%d are required.  You should only need it if you\n"
               "         modified a .yy file.  You may need the \`Bison' package\n"
               "         in order for those modifications to take effect.  You can get\n"
               "         \`Bison' from any GNU archive site.",
	       major, minor, micro, min_major, min_minor, min_micro);
*/
      exit(1);
    }
}
    ], AC_MSG_RESULT(yes), [
      YACC="$SHELL $missing_dir/missing bison"
      AC_MSG_RESULT(no)
    ],)
  fi
])