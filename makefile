CC=gcc

# Tell the C compiler where to find <libguile.h>
CFLAGS=`pkg-config --cflags guile-2.2 raylib gl` -g -I/usr/include/GL

# Tell the linker what libraries to use and where to find them.
LIBS=`pkg-config --libs guile-2.2 raylib gl` 

main: main.o
	${CC} main.o ${LIBS} -o main

main.o: main.c
	${CC} -c ${CFLAGS} main.c 
