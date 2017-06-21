#include <libguile.h>
#include <unistd.h>
#include "utils.h"

SCM
mlg_pagesize ()
{
  return scm_from_long (sysconf (_SC_PAGESIZE));
}

void
init_utils_lib ()
{
  scm_c_define_gsubr ("getpagesize", 0, 0, 0, mlg_pagesize);
}
