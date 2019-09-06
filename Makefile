SCM_SRC = \
 mlg/assert.scm \
 mlg/bytevectors.scm \
 mlg/characters.scm \
 mlg/debug.scm \
 mlg/journal.scm \
 mlg/lists.scm \
 mlg/logging.scm \
 mlg/math.scm \
 mlg/numval.scm \
 mlg/port.scm \
 mlg/procedure.scm \
 mlg/strings.scm \
 mlg/time.scm \
 mlg/typechecking.scm \
 mlg/utils.scm

BROKEN_SCM_SRC = \
 mlg/io.scm \
 mlg/regex.scm \
 mlg/spreadsheet.scm

SCM_SCRIPTS =

SCM_OBJ := $(patsubst %.scm,%.go,$(SCM_SRC))
SCM_SCRIPT_OBJ := $(patsubst %,%.go,$(SCM_SCRIPTS))

C_SRC = \
 mlg/chars.c \
 mlg/math.c \
 mlg/journal.c \
 mlg/strings.c \
 mlg/utils.c

C_OBJ := $(patsubst %.c,%.o,$(C_SRC))

.SUFFIXES: .c .o .scm .go

#GUILE_VERSION = $(shell guile --version | head -1 | awk '{print $4}')
GUILE_VERSION = 2.2

CFLAGS = -g -O1 \
  -Wall `pkg-config --cflags guile-$(GUILE_VERSION) libsystemd` -fpic
LIBS = `pkg-config --libs guile-$(GUILE_VERSION) libsystemd`
SCM_CFLAGS = -Wunsupported-warning -Wunused-variable -Wunused-toplevel\
 -Warity-mismatch -Wduplicate-case-datum \
 -Wbad-case-datum -Wformat
ifeq '$(GUILE_VERSION)' '2.2'
SCM_CFLAGS += -Wmacro-use-before-definition
endif
SCM_LDFLAGS = -L . 

all: libguile-mlg.so mlg.go.tar

mlg.go.tar: $(SCM_OBJ) $(SCM_SCRIPT_OBJ)
	tar -c -H ustar -f $@ $(SCM_OBJ) $(SCM_SCRIPT_OBJ)
  # pax -w -x ustar -f $@ $(SCM_OBJ)

libguile-mlg.so: $(C_OBJ)
	$(CC) -O1 -shared -o $@ $(C_OBJ) $(LIBS)

.scm.go:
	unset GUILE_LOAD_COMPILED_PATH ; \
	GUILE_WARN_DEPRECATED=detailed \
	LC_ALL=C \
	LD_LIBRARY_PATH=. \
	guild compile $(SCM_LDFLAGS) $(SCM_CFLAGS) -o "$@" "$<"

clean:
	rm -f $(SCM_OBJ) $(SCM_SCRIPT_OBJ)
	rm -f mlg.go.tar mlg.go.tar.Z
	rm -f $(C_OBJ)
	rm -f libguile-mlg.so

install:
	mkdir -p `pkg-config guile-$(GUILE_VERSION) --variable=extensiondir`
	mkdir -p `pkg-config guile-$(GUILE_VERSION) --variable=sitedir`/mlg
	cp libguile-mlg.so `pkg-config guile-$(GUILE_VERSION) --variable=extensiondir`
	cp $(SCM_SRC) `pkg-config guile-$(GUILE_VERSION) --variable=sitedir`/mlg
