#include <libguile.h>
#include "math.h"

SCM
mlg_real_to_integer (SCM s_u)
{
  double u;
  SCM_ASSERT (scm_is_real(s_u), s_u, SCM_ARG1, "real->integer");

  u = scm_to_double (s_u);
  if (u < (double) (SCM_MOST_POSITIVE_FIXNUM+1)
      && u >= (double) SCM_MOST_NEGATIVE_FIXNUM)
    return SCM_I_MAKINUM ((long) u);

  scm_out_of_range ("real->integer", s_u);
}

void
init_math_lib ()
{
  scm_c_define_gsubr ("real->integer", 1, 0, 0, mlg_real_to_integer);
}
