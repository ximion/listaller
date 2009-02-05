#!/bin/bash
# get latest from http://users.telenet.be/Jan.Van.hijfte/qtforfpc/fpcqt4.html
# for more info, you can visit the links on above page to FreePascal and Lazarus wiki

# download corresponding qt source
# unpack qt source -> the created directory (Qt4 dir) will be used below
# do configure and gmake in that Qt4 dir

NAME=libkbind.so
QTDIR=/usr/share/qt4
INCLUDE_PATH="-I. -I$QTDIR/include -I$QTDIR/include/Qt -I$QTDIR/include/QtGui -I$QTDIR/include/QtCore -I/usr/include/KDE -Iqlcl "
LIB_PATH=$QTDIR/lib
export LD_LIBRARY_PATH=$LIB_PATH
if [ -e "$LIB_PATH/libQtCore.so.4" ]
then
  echo please wait for compile to finish ...
  g++ -D BINUX $INCLUDE_PATH libkbind.cpp -o libkbind.so -shared -fPIC -lQtCore -lQtGui  -Xlinker -soname=$NAME -Xlinker --library-path -Xlinker $LIB_PATH
  echo Showing used Qt libraries when LD_LIBRARY_PATH=$LD_LIBRARY_PATH
  ldd $NAME | grep libk
  echo stripping library
  strip --strip-all $NAME
  echo Done
else
  echo "Please Modify location of Qt4 in this script"
fi
