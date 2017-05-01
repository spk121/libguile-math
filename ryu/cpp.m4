Use the '-p' aka '--prefix-builtins' option so that all macros start
with m4.

#include
m4_include(`file')
 - copy file into source file at this location

m4 searches for include files in the current working directory
then in any directories specified by The '--include' or '-I' option.

Finally, it searches the M4PATH environment variable

The #define CONST and #ifdef #endif pattern

m4_define({NAME},{VAL})

m4_ifdef({NAME},
{
  text if true
},
{
  text if false
})


The #define MACRO(VAR) (MACRO_EXPANSION) pattern

m4_define({MACRO},
{
 EXPANSION $1
})

m4_ifdef (`name', `string-1', `string-2')

m4_ifelse (`string-1', `string-2', `equal', [`not-equal'])

#define func(x) (sin(x))

m4_define({func}, {(sin $0)})

func({10})
