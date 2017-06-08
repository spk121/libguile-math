SCM_SRC = \
 assert.scm \
 buf.scm \
 bytevectors.scm \
 characters.scm \
 containers.scm \
 debug.scm \
 journal.scm \
 lists.scm \
 logging.scm \
 math.scm \
 numval.scm \
 port.scm \
 procedure.scm \
 re.scm \
 strings.scm \
 time.scm \
 typechecking.scm \
 utils.scm

BROKEN_SCM_SRC = \
 ed.scm \
 io.scm \
 regex.scm \
 spreadsheet.scm

SCM_OBJ := $(patsubst %.scm,%.go,$(SCM_SRC))

C_SRC = math.c journal.c strings.c

C_OBJ := $(patsubst %.c,%.o,$(C_SRC))

.SUFFIXES: .c .o .scm .go

GUILE_VERSION = 2.2

CFLAGS = -g -O1 \
  -Wall `pkg-config --cflags guile-$(GUILE_VERSION) libsystemd` -fpic
LIBS = `pkg-config --libs guile-$(GUILE_VERSION) libsystemd`
SCM_CFLAGS = -Wunsupported-warning -Wunused-variable -Wunused-toplevel\
 -Wmacro-use-before-definition -Warity-mismatch -Wduplicate-case-datum \
 -Wbad-case-datum -Wformat
SCM_LDFLAGS = -L .. -L ../../gano-gano/newsrc

all: libguile-mlg.so mlg_compiled.tar.Z

mlg_compiled.tar.Z: mlg_compiled.tar
	compress -f $<

mlg_compiled.tar: $(SCM_OBJ)
	tar -c -H ustar -f $@ $(SCM_OBJ)
  # pax -w -x ustar -f $@ $(SCM_OBJ)


libguile-mlg.so: $(C_OBJ)
	$(CC) -O1 -shared -o $@ $^ $(LIBS)

.scm.go:
	unset GUILE_LOAD_COMPILED_PATH ; \
	GUILE_WARN_DEPRECATED=detailed \
	LC_ALL=C \
	guild compile $(SCM_LDFLAGS) $(SCM_CFLAGS) -o "$@" "$<"

clean:
	rm -f *.go
	rm -f mlg_compiled.tar
	rm -f *.o
	rm -f *.so

install:
	mkdir -p `pkg-config guile-$(GUILE_VERSION) --variable=extensiondir`
	mkdir -p `pkg-config guile-$(GUILE_VERSION) --variable=sitedir`/mlg
	cp libguile-mlg.so `pkg-config guile-$(GUILE_VERSION) --variable=extensiondir`
	cp $(SCM_SRC) `pkg-config guile-$(GUILE_VERSION) --variable=sitedir`/mlg
