#!/usr/bin/env bash
# Parses command line options. Currently supported options are:
#
# DESTDIR		Destination root directory
for arg; do

  case $arg in

    DESTDIR=*) DESTDIR=${arg#DESTDIR=};;

  esac;

done

#DESTDIR=""

#Strips the debug-infos
strip "./bin/unibuild"
strip "./bin/licreator"

#
# Does the install
#
# "mkdir -p" is equivalent to ForceDirectories pascal function
#

mkdir -p $DESTDIR/usr/bin/
mkdir -p $DESTDIR/usr/appfiles/
mkdir -p $DESTDIR/usr/appfiles/liCreator
mkdir -p $DESTDIR/usr/share/applications

cp './bin/licreator' $DESTDIR/usr/appfiles/liCreator/
cp './graphics/listaller_creator.png' $DESTDIR/usr/appfiles/liCreator/

cp "./liCreator/licreator.desktop" $DESTDIR"/usr/share/applications/"

#Create symlink
cd $DESTDIR/usr/bin
ln -s /usr/appfiles/liCreator/licreator licreator

echo "Installation done. (liCreator)"
