// -*- coding: us-ascii; indent-tabs-mode: nil; -*-
// Modified in 2017 by Michael L. Gran <spk121@yahoo.com>
// from code that, as far as I can tell, is in the public domain.
//
// My modifications to this code are released to the public domain.

#include <libguile.h>
#include <stdint.h>
#include "strings.h"

#define POLY 0x8408

// There are many versions of this code out in the public
// domain.  It claims to implement the CCITT X.25 version
// of CRC-16.
SCM
mlg_string_crc16 (SCM s_str)
{
  size_t length;
  char *data_p;
  unsigned char i;
  unsigned int data;
  uint32_t crc = 0xffff;

  SCM_ASSERT (scm_is_string(s_str), s_str, SCM_ARG1, "string-crc16");

  SCM s_bv = scm_string_to_utf8 (s_str);
  length = SCM_BYTEVECTOR_LENGTH (s_bv);
  data_p = (char *)SCM_BYTEVECTOR_CONTENTS (s_bv);

  if (length == 0)
    return scm_from_uint16 (~crc);

  do {
    for (i = 0, data = (uint32_t) 0xff & *data_p++;
         i < 8;
         i ++, data >>= 1)
      {
        if ((crc & 0x0001) ^ (data & 0x0001))
          crc = (crc >> 1) ^ POLY;
        else
          crc >>= 1;
      }
  } while (--length);
  crc = ~crc;
  data = crc;
  crc = (crc << 8) | (data >> 8 & 0xff);

  return scm_from_uint16 (crc);
}

void
init_strings_lib ()
{
  scm_c_define_gsubr ("string-crc16", 1, 0, 0, mlg_string_crc16);
}
