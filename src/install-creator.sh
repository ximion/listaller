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
    prefix=*) prefix=${arg#prefix=};;
  esac;
done

if [ -z "$prefix" ]; then
   export prefix="/usr"
fi

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

mkdir -p $DESTDIR$prefix/bin/
mkdir -p $DESTDIR/opt/appfiles/ #NEEDS to be edited for prefix
mkdir -p $DESTDIR/opt/appfiles/liCreator
mkdir -p $DESTDIR$prefix/share/applications

if [ "$WIDGET" == "qt4" ]; then
cp '../build/qt4/licreator' $DESTDIR/opt/appfiles/liCreator/
else
cp '../build/gtk2/licreator' $DESTDIR/opt/appfiles/liCreator/
fi

cp '../graphics/listaller_creator.png' $DESTDIR/opt/appfiles/liCreator/
cp "../licreator/licreator.desktop" $DESTDIR$prefix/share/applications/

#Create symlink
if [ ! -e "$DESTDIR$prefix/bin/licreator" ]; then
 cd $DESTDIR$prefix/bin
 ln -s /opt/appfiles/liCreator/licreator licreator
fi

echo "Installation done. (liCreator)"
