CC=gcc

# Tell the C compiler where to find <libguile.h>
CFLAGS=`pkg-config --cflags guile-2.2 gl` -g -I/usr/include/GL -L . -lraylib

clean:
	rm dds_native.so
	rm dds.run
	rm *.html

dds_native.so: c/base.c c/base.h c/input.c c/boot.c
	${CC} ${CFLAGS} -shared -o dds_native.so -fPIC c/*.c

dds_docs:
	asciidoctor -D . asciidoc/*.adoc

dist: clean dds_docs dds_native.so
	mkdir -p dist
	mkdir dist/native
	mkdir dist/docs
	cp libraylib.so.2.5.0 dist/native/libraylib.so
	cp dds_native.so dist/native
	cp -r guile dist
	cp bash/ddsrunner.sh dist/dds.sh
	cp bash/installer.sh dist/installer.sh
	cp bash/uninstaller.sh dist/uninstaller.sh
	cp asciidoc/*.html dist/docs
	cp -r examples dist
	makeself dist dds.run 'Guile DDS' ./installer.sh
	rm -r dist
