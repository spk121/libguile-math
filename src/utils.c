#if HAVE_CONFIG_H
# include "config.h"
#endif
#if HAVE_UNISTD_H
# include <unistd.h>
#endif
#include <libguile.h>
#include "utils.h"

SCM
mlg_pagesize ()
{
#if HAVE_SYSCONF    
  return scm_from_long (sysconf (_SC_PAGESIZE));
#else
  return SCM_BOOL_F;
#endif
}

void
init_utils_lib ()
{
  scm_c_define_gsubr ("getpagesize", 0, 0, 0, mlg_pagesize);
}
