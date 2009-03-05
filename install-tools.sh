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
#strip --strip-all "./bin/lipa" "./bin/unibuild"
ARCH=$(uname -m)
case "$ARCH" in

 "i686") ARCH="i386";;

 "i586") ARCH="i386";;

 "i486") ARCH="i386";;

esac
if [ $ARCH = "x86_64" ]; then TDir="/usr/lib64/"
else TDir="/usr/lib/"
fi

# Does the install
#
# "mkdir -p" is equivalent to ForceDirectories pascal function

mkdir -p $DESTDIR/$TDir/listaller
mkdir -p $DESTDIR/$TDir/listaller/graphics
mkdir -p $DESTDIR/$TDir/listaller/graphics/libutton
mkdir -p $DESTDIR/usr/bin

cp ./bin/lipa $DESTDIR/usr/bin/
cp ./bin/unibuild $DESTDIR/$TDir/listaller
#Copy graphics
cp ./graphics/libutton/left.png $DESTDIR/$TDir/listaller/graphics/libutton/
cp ./graphics/libutton/firstblock.png $DESTDIR/$TDir/listaller/graphics/libutton/
cp ./graphics/libutton/block.png $DESTDIR/$TDir/listaller/graphics/libutton/
cp ./graphics/libutton/lastblock.png $DESTDIR/$TDir/listaller/graphics/libutton/
cp -dpr ./graphics/libutton/distro/ $DESTDIR/$TDir/listaller/graphics/libutton/distro/
rm -rf $DESTDIR/$TDir/listaller/graphics/libutton/distro/.svn
rm -rf $DESTDIR/$TDir/listaller/graphics/libutton/distro/.directory

echo "Installation done."
