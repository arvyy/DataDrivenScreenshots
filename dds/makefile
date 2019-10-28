CC=gcc

# Tell the C compiler where to find <libguile.h>
CFLAGS=`pkg-config --cflags guile-2.2 gl` -g -I/usr/include/GL

# Tell the linker what libraries to use and where to find them.
LIBS=`pkg-config --libs guile-2.2 gl`

clean:
	rm main.o ../main

main: main.o
	${CC} main.o -Wl,-rpath='dds' libraylib.so.2.5.0 ${LIBS} -o ../main

main.o: c/main.c
	${CC} -c ${CFLAGS} c/main.c 
