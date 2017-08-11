// -*- coding: us-ascii; indent-tabs-mode: nil; -*-
// Modified in 2017 by Michael L. Gran <spk121@yahoo.com>
// from code that, as far as I can tell, is in the public domain.
//
// My modifications to this code are released to the public domain.

#include <libguile.h>
#include <wchar.h>

SCM
mlg_wcwidth (SCM c)
{
  wchar_t c_c = scm_to_uint (scm_char_to_integer(c));
  int width = wcwidth (c_c);
  return scm_from_int (width);
}

void
init_chars_lib ()
{
  scm_c_define_gsubr ("wcwidth", 1, 0, 0, mlg_wcwidth);
}
