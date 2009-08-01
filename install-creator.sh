#!/usr/bin/env bash
# Parses command line options. Currently supported options are:
#
# DESTDIR		Destination root directory
# WIDGET                Widgetset the binary should be installed for
set -e

for arg; do
  case $arg in
    DESTDIR=*) DESTDIR=${arg#DESTDIR=};;
    WIDGET=*) WIDGET=${arg#WIDGET=};;
  esac;
done

ARCH=$(uname -m)
case "$ARCH" in

 "i686") ARCH="i386";;

 "i586") ARCH="i386";;

 "i486") ARCH="i386";;
esac
if [ $ARCH = "x86_64" ]; then TDir="/usr/lib64/"
else TDir="/usr/lib/"
fi
#
# Does the install
#
# "mkdir -p" is equivalent to ForceDirectories pascal function
#

mkdir -p $DESTDIR/usr/bin/
mkdir -p $DESTDIR/usr/appfiles/
mkdir -p $DESTDIR/usr/appfiles/liCreator
mkdir -p $DESTDIR/usr/share/applications

if [ "$WIDGET" == "qt4" ]; then
cp './bin/qt4/licreator' $DESTDIR/usr/appfiles/liCreator/
else
cp './bin/gtk2/licreator' $DESTDIR/usr/appfiles/liCreator/
fi

cp './graphics/listaller_creator.png' $DESTDIR/usr/appfiles/liCreator/
cp "./liCreator/licreator.desktop" $DESTDIR"/usr/share/applications/"

#Create symlink
cd $DESTDIR/usr/bin
ln -s /usr/appfiles/liCreator/licreator licreator

echo "Installation done. (liCreator)"
