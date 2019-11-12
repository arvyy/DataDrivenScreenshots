CC=gcc

# Tell the C compiler where to find <libguile.h>
CFLAGS=`pkg-config --cflags guile-2.2 gl` -g -I/usr/include/GL

# Tell the linker what libraries to use and where to find them.
LIBS=`pkg-config --libs guile-2.2 gl`
RAYLIB=`pkg-config --libs raylib`

clean:
	rm dds_native.so

dds_native.so: c/base.c c/base.h c/input.c c/boot.c
	${CC} ${CFLAGS} ${RAYLIB} -shared -o dds_native.so -fPIC c/*.c

dds_install: dds_native.so
	cp dds_native.so `pkg-config guile-2.2 --variable=extensiondir`
	cp libraylib.so.2.5.0 `pkg-config guile-2.2 --variable=extensiondir`
	mkdir `pkg-config guile-2.2 --variable=sitedir` -p
	cp guile/dds `pkg-config guile-2.2 --variable=sitedir` -r

#main: main.o dds_native.so dds_input.so
#	${CC} main.o -Wl,-rpath='dds' dds_native.so libraylib.so.2.5.0 ${LIBS} -o ../main
#
#main.o: c/main.c
#	${CC} -c ${CFLAGS} c/main.c 
#
#dds_input.so: c/input.c
#	${CC} ${CFLAGS} -shared -o dds_input.so -fPIC c/input.c
#
#dds_native.so: c/base.c c/base.h
#	${CC} ${CFLAGS} -shared -o dds_native.so -fPIC c/base.c
