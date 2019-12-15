#/bin/sh
sudo cp -a native/* `pkg-config guile-2.2 --variable=extensiondir`
sudo cp -a guile/dds `pkg-config guile-2.2 --variable=sitedir`
mkdir $USER_PWD/dds
cp -r docs $USER_PWD/dds
cp -r examples $USER_PWD/dds
cp dds.sh $USER_PWD/dds
cp uninstaller.sh $USER_PWD/dds
