#/bin/sh
rm `pkg-config guile-2.2 --variable=extensiondir`/libraylib.so
rm `pkg-config guile-2.2 --variable=extensiondir`/dds_native.so
rm -r `pkg-config guile-2.2 --variable=sitedir`/dds
