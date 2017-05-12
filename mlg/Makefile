SCM_SRC = math.scm utils.scm
C_SRC = math.c
C_OBJ := $(patsubst %.c,%.o,$(C_SRC))
CFLAGS = -g -Wall `pkg-config --cflags guile-2.0` -fpic
LIBS = `pkg-config --libs guile-2.0`

ed.go: ed.scm
	unset GUILE_LOAD_COMPILED_PATH ; \
	LC_ALL=C \
	guild compile \
	  -L .. -L ../../gano-gano/newsrc \
	  -Wformat -Wunbound-variable -Warity-mismatch -o "$@" "$<"

all: libguile-mlg.so ed.go

libguile-mlg.so: $(C_OBJ)
	$(CC) -shared -o $@ $^ $(LIBS)

install:
	mkdir -p `pkg-config guile-2.2 --variable=extensiondir`
	mkdir -p `pkg-config guile-2.2 --variable=sitedir`/mlg
	cp libguile-mlg.so `pkg-config guile-2.2 --variable=extensiondir`
	cp $(SCM_SRC) `pkg-config guile-2.2 --variable=sitedir`/mlg
