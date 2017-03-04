C_SRC = chop.c
C_OBJ := $(patsubst %.c,%.o,$(C_SRC))
CFLAGS = -g -Wall `pkg-config --cflags guile-2.2` -fpic
LIBS = `pkg-config --libs guile-2.2`

all: libguile-chop.so 

libguile-chop.so: $(C_OBJ)
	$(CC) -shared -o $@ $^ $(LIBS)

install:
	mkdir -p `pkg-config guile-2.2 --variable=extensiondir`
	mkdir -p `pkg-config guile-2.2 --variable=sitedir`
	cp libguile-chop.so `pkg-config guile-2.2 --variable=extensiondir`
	cp chop.scm `pkg-config guile-2.2 --variable=sitedir`
