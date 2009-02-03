#!/usr/bin/env bash
# Parses command line options. Currently supported options are:
# DESTDIR		Destination root directory
for arg; do

  case $arg in

    DESTDIR=*) DESTDIR=${arg#DESTDIR=};;

  esac;

done

#DESTDIR=""

#Strips the debug-infos
strip --strip-all "./bin/lipa" "./bin/unibuild"

# Does the install
#
# "mkdir -p" is equivalent to ForceDirectories pascal function

mkdir -p $DESTDIR/usr/share/listaller
mkdir -p $DESTDIR/usr/share/listaller/graphics
mkdir -p $DESTDIR/usr/share/listaller/graphics/libutton
mkdir -p $DESTDIR/usr/bin

cp ./bin/lipa $DESTDIR/usr/bin/
cp ./bin/unibuild $DESTDIR/usr/share/listaller
#Copy graphics
cp ./graphics/libutton/left.png $DESTDIR/usr/share/listaller/graphics/libutton/
cp ./graphics/libutton/firstblock.png $DESTDIR/usr/share/listaller/graphics/libutton/
cp ./graphics/libutton/block.png $DESTDIR/usr/share/listaller/graphics/libutton/
cp ./graphics/libutton/lastblock.png $DESTDIR/usr/share/listaller/graphics/libutton/
cp -dpr ./graphics/libutton/distro/ $DESTDIR/usr/share/listaller/graphics/libutton/distro/
rm -rf $DESTDIR/usr/share/listaller/graphics/libutton/distro/.svn
rm -rf $DESTDIR/usr/share/listaller/graphics/libutton/distro/.directory

echo "Installation done."
