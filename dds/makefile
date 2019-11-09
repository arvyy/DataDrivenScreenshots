CC=gcc

# Tell the C compiler where to find <libguile.h>
CFLAGS=`pkg-config --cflags guile-2.2 gl` -g -I/usr/include/GL

# Tell the linker what libraries to use and where to find them.
LIBS=`pkg-config --libs guile-2.2 gl`

clean:
	rm main.o ../main dds_input.so dds_native.so

main: main.o dds_native.so dds_input.so
	${CC} main.o -Wl,-rpath='dds' dds_native.so libraylib.so.2.5.0 ${LIBS} -o ../main

main.o: c/main.c
	${CC} -c ${CFLAGS} c/main.c 

dds_input.so: c/input.c
	${CC} ${CFLAGS} -shared -o dds_input.so -fPIC c/input.c

dds_native.so: c/base.c c/base.h
	${CC} ${CFLAGS} -shared -o dds_native.so -fPIC c/base.c
