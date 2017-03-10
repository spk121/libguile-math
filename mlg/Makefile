SCM_SRC = math.scm utils.scm
C_SRC = math.c debug.c
C_OBJ := $(patsubst %.c,%.o,$(C_SRC))
CFLAGS = -g -Wall `pkg-config --cflags guile-2.2` -fpic
LIBS = `pkg-config --libs guile-2.2`

all: libguile-mlg.so 

libguile-mlg.so: $(C_OBJ)
	$(CC) -shared -o $@ $^ $(LIBS)

install:
	mkdir -p `pkg-config guile-2.2 --variable=extensiondir`
	mkdir -p `pkg-config guile-2.2 --variable=sitedir`/mlg
	cp libguile-mlg.so `pkg-config guile-2.2 --variable=extensiondir`
	cp $(SCM_SRC) `pkg-config guile-2.2 --variable=sitedir`/mlg
