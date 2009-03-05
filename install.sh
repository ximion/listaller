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
#strip --strip-all "./bin/listallmgr" "./bin/listallgo" "./bin/lipa" "./bin/liupdate"

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

mkdir -p $DESTDIR/usr/bin
mkdir -p $DESTDIR/$TDir//pixmaps
mkdir -p $DESTDIR/$TDir/listaller
mkdir -p $DESTDIR/$TDir//listaller/mime
mkdir -p $DESTDIR/$TDir//listaller/graphics
mkdir -p $DESTDIR/$TDir//listaller/graphics/categories
#mkdir -p $DESTDIR/$TDir//listaller/lang
mkdir -p $DESTDIR/$TDir//listaller/pkitbind
#Copy graphics
cp ./graphics/header.png $DESTDIR/$TDir//listaller/graphics/
cp ./graphics/mime-ipk.png $DESTDIR/$TDir//listaller/graphics/
cp ./graphics/mime-ips.png $DESTDIR/$TDir//listaller/graphics/
cp ./graphics/listaller.png $DESTDIR/$TDir//pixmaps
cp ./graphics/wizardimage.png $DESTDIR/$TDir//listaller/graphics/
cp ./graphics/spackage.png $DESTDIR/$TDir//listaller/graphics/
cp ./graphics/throbber.gif $DESTDIR/$TDir//listaller/graphics/
cp ./graphics/categories/accessories.png $DESTDIR/$TDir//listaller/graphics/categories/
cp ./graphics/categories/all.png $DESTDIR/$TDir//listaller/graphics/categories/
cp ./graphics/categories/development.png $DESTDIR/$TDir//listaller/graphics/categories/
cp ./graphics/categories/games.png $DESTDIR/$TDir//listaller/graphics/categories/
cp ./graphics/categories/graphics.png $DESTDIR/$TDir//listaller/graphics/categories/
cp ./graphics/categories/internet.png $DESTDIR/$TDir//listaller/graphics/categories/
cp ./graphics/categories/multimedia.png $DESTDIR/$TDir//listaller/graphics/categories/
cp ./graphics/categories/office.png $DESTDIR/$TDir//listaller/graphics/categories/
cp ./graphics/categories/other.png $DESTDIR/$TDir//listaller/graphics/categories/
cp ./graphics/categories/science.png $DESTDIR/$TDir//listaller/graphics/categories/
cp ./graphics/categories/system.png $DESTDIR/$TDir//listaller/graphics/categories/
#Copy other files
cp ./bin/listallgo $DESTDIR/$TDir//listaller/
cp ./bin/listallmgr $DESTDIR/$TDir//listaller/
cp ./bin/liupdate $DESTDIR/$TDir//listaller/
cp ./bindings/pkitbind.py $DESTDIR/$TDir//listaller/pkitbind/
cp -dpr ./lang/ $DESTDIR/$TDir//listaller/
rm -rf $DESTDIR/$TDir//listaller/lang/.svn
rm -rf $DESTDIR/$TDir//listaller/lang/.directory

mkdir -p $DESTDIR/$TDir//applications
mkdir -p $DESTDIR/$TDir//mime
mkdir -p $DESTDIR/$TDir//mime/packages
mkdir -p $DESTDIR/$TDir//mime/text
mkdir -p $DESTDIR/$TDir//mime-info
mkdir -p $DESTDIR/etc/lipa
mkdir -p $DESTDIR/etc/lipa/app-reg

cp ./additional/blacklist $DESTDIR/etc/lipa/

cp "./additional/applications/listaller-manager.desktop" $DESTDIR/usr/share/applications
cp ./additional/mime/packages/x-ipk.xml $DESTDIR/$TDir/listaller/mime
cp ./additional/mime/text/x-ips.xml $DESTDIR/$TDir/listaller/mime
cp ./additional/mime-info/listaller-pack.mime $DESTDIR/usr/share/mime-info

#mkdir -p $DESTDIR/usr/bin
#cp ./bin/lipa $DESTDIR/usr/bin/

#Execute installscript
sh ./additional/scripts/postinst

#Create symlink
cd $DESTDIR/usr/bin
ln -s /$TDir/listaller/listallmgr listallmgr

echo "Installation done."
