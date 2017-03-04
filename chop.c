#include <libguile.h>
#include "chop.h"

SCM
real_to_small_integer (SCM s_u)
{
  double u;
  SCM_ASSERT (scm_is_real(s_u), s_u, SCM_ARG1, "real->small-integer");

  u = scm_to_double (s_u);
  if (u < (double) (SCM_MOST_POSITIVE_FIXNUM+1)
      && u >= (double) SCM_MOST_NEGATIVE_FIXNUM)
    return SCM_I_MAKINUM ((scm_t_inum) u);

  scm_out_of_range ("real->small-integer", s_u);
}

void
init_chop_lib ()
{
  scm_c_define_gsubr ("real->small-integer", 1, 0, 0, real_to_small_integer);
}
