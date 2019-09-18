ACLOCAL_AMFLAGS = -I m4 ${ACLOCAL_FLAGS}
AUTOMAKE_OPTIONS = subdir-objects
AM_DISTCHECK_CONFIGURE_FLAGS = --with-gnu-filesystem-hierarchy

info_TEXINFOS = doc/mlg_lib.texi

guileextensiondir = $(GUILE_EXTENSION)
guileextension_LTLIBRARIES = libmlg.la

AM_CFLAGS = $(GUILE_CFLAGS)
AM_LIBS = $(GUILE_LTLIBS)
AM_LDFLAGS = $(GUILE_LDFLAGS) -avoid-version

libmlg_la_SOURCES = \
  src/chars.c \
  src/journal.c \
  src/journal.h \
  src/math.c \
  src/math.h \
  src/strings.c \
  src/strings.h \
  src/utils.c \
  src/utils.h

guilesitedir = $(GUILE_SITE)
guileobjectdir = $(GUILE_SITE_CCACHE)
mlgguilesitedir = $(guilesitedir)/mlg
ryuguilesitedir = $(guilesitedir)/ryu
mlgguileobjectdir = $(guileobjectdir)/mlg
ryuguileobjectdir = $(guileobjectdir)/ryu

dist_mlgguilesite_DATA = \
 module/mlg/assert.scm \
 module/mlg/bytevectors.scm \
 module/mlg/characters.scm \
 module/mlg/debug.scm \
 module/mlg/journal.scm \
 module/mlg/lists.scm \
 module/mlg/logging.scm \
 module/mlg/math.scm \
 module/mlg/numval.scm \
 module/mlg/port.scm \
 module/mlg/procedure.scm \
 module/mlg/strings.scm \
 module/mlg/time.scm \
 module/mlg/typechecking.scm \
 module/mlg/utils.scm

nodist_data_DATA = \
 module/mlg/regex.scm \
 module/mlg/spreadsheet.scm \
 module/mlg/state.scm

dist_ryuguilesite_DATA = \
 module/ryu/core.scm \
 module/ryu/printf.scm

mlgguileobject_DATA = \
  $(dist_mlgguilesite_DATA:%.scm=%.go)

ryuguileobject_DATA = \
  $(dist_ryuguilesite_DATA:%.scm=%.go)

GUILEC_FLAGS = \
 -O2 \
 -Warity-mismatch \
 -Wformat \
 -Wmacro-use-before-definition \
 -Wunbound-variable \
 --load-path=$(abs_srcdir)/module \
 --load-path=$(abs_builddir)/module

.scm.go:
	GUILE_AUTO_COMPILE=0 \
	LTDL_LIBRARY_PATH=$(abs_builddir)/.libs \
	$(GUILE_TOOLS) compile --target="$(host)" $(GUILEC_FLAGS) \
	-o "$@" "$<"

$(mlgguileobject_DATA) $(ryuguileobject_DATA): $(guileextension_LTLIBRARIES)
