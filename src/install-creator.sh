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
    libdir=*) libdir=${arg#libdir=};;
  esac;
done

if [ -z "$prefix" ]; then
   export prefix="/usr"
fi
if [ -z "$libdir" ]; then
   export libdir="$prefix/lib"
fi

#
# Does the install
#

mkdir -p $DESTDIR$prefix/bin/
mkdir -p $DESTDIR/opt/appfiles/ #NEEDS to be edited for prefix
mkdir -p $DESTDIR/opt/appfiles/liCreator
mkdir -p $DESTDIR$prefix/share/applications
mkdir -p $DESTDIR$prefix/share/pixmaps

if [ "$WIDGET" == "qt4" ]; then
cp '../build/qt4/licreator-qt' $DESTDIR/opt/appfiles/liCreator/
cp "../data/applications/licreator-qt.desktop" $DESTDIR$prefix/share/applications/
cp '../graphics/listaller_creator.png' $DESTDIR/opt/appfiles/liCreator/listaller_creator-qt.png
#Create symlink
if [ ! -e "$DESTDIR$prefix/bin/licreator-qt" ]; then
 cd $DESTDIR$prefix/bin
 ln -s /opt/appfiles/liCreator/licreator-qt licreator-qt
fi
else
cp '../build/gtk2/licreator-gtk' $DESTDIR/opt/appfiles/liCreator/
cp "../data/applications/licreator-gtk.desktop" $DESTDIR$prefix/share/applications/
cp '../graphics/listaller_creator.png' $DESTDIR/opt/appfiles/liCreator/listaller_creator-gtk.png
#Create symlink
if [ ! -e "$DESTDIR$prefix/bin/licreator-gtk" ]; then
 cd $DESTDIR$prefix/bin
 ln -s /opt/appfiles/liCreator/licreator-gtk licreator-gtk
fi
fi


echo "Installation done. (liCreator)"
