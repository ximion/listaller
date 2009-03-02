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
strip --strip-all "./bin/listallmgr" "./bin/listallgo" "./bin/lipa" "./bin/liupdate"

#
# Does the install
#
# "mkdir -p" is equivalent to ForceDirectories pascal function
#

mkdir -p $DESTDIR/usr/bin
mkdir -p $DESTDIR/usr/share/pixmaps
mkdir -p $DESTDIR/usr/share/listaller
mkdir -p $DESTDIR/usr/share/listaller/mime
mkdir -p $DESTDIR/usr/share/listaller/graphics
mkdir -p $DESTDIR/usr/share/listaller/graphics/categories
#mkdir -p $DESTDIR/usr/share/listaller/lang
mkdir -p $DESTDIR/usr/share/listaller/pkitbind
#Copy graphics
cp ./graphics/header.png $DESTDIR/usr/share/listaller/graphics/
cp ./graphics/mime-ipk.png $DESTDIR/usr/share/listaller/graphics/
cp ./graphics/mime-ips.png $DESTDIR/usr/share/listaller/graphics/
cp ./graphics/listaller.png $DESTDIR/usr/share/pixmaps
cp ./graphics/wizardimage.png $DESTDIR/usr/share/listaller/graphics/
cp ./graphics/spackage.png $DESTDIR/usr/share/listaller/graphics/
cp ./graphics/throbber.gif $DESTDIR/usr/share/listaller/graphics/
cp ./graphics/categories/accessories.png $DESTDIR/usr/share/listaller/graphics/categories/
cp ./graphics/categories/all.png $DESTDIR/usr/share/listaller/graphics/categories/
cp ./graphics/categories/development.png $DESTDIR/usr/share/listaller/graphics/categories/
cp ./graphics/categories/games.png $DESTDIR/usr/share/listaller/graphics/categories/
cp ./graphics/categories/graphics.png $DESTDIR/usr/share/listaller/graphics/categories/
cp ./graphics/categories/internet.png $DESTDIR/usr/share/listaller/graphics/categories/
cp ./graphics/categories/multimedia.png $DESTDIR/usr/share/listaller/graphics/categories/
cp ./graphics/categories/office.png $DESTDIR/usr/share/listaller/graphics/categories/
cp ./graphics/categories/other.png $DESTDIR/usr/share/listaller/graphics/categories/
cp ./graphics/categories/science.png $DESTDIR/usr/share/listaller/graphics/categories/
cp ./graphics/categories/system.png $DESTDIR/usr/share/listaller/graphics/categories/
#Copy other files
cp ./bin/listallgo $DESTDIR/usr/share/listaller/
cp ./bin/listallmgr $DESTDIR/usr/share/listaller/
cp ./bin/liupdate $DESTDIR/usr/share/listaller/
cp ./bindings/pkitbind.py $DESTDIR/usr/share/listaller/pkitbind/
cp -dpr ./lang/ $DESTDIR/usr/share/listaller/
rm -rf $DESTDIR/usr/share/listaller/lang/.svn
rm -rf $DESTDIR/usr/share/listaller/lang/.directory

mkdir -p $DESTDIR/usr/share/applications
mkdir -p $DESTDIR/usr/share/mime
mkdir -p $DESTDIR/usr/share/mime/packages
mkdir -p $DESTDIR/usr/share/mime/text
mkdir -p $DESTDIR/usr/share/mime-info
mkdir -p $DESTDIR/etc/lipa
mkdir -p $DESTDIR/etc/lipa/app-reg

cp ./additional/blacklist $DESTDIR/etc/lipa/

cp "./additional/applications/listaller-manager.desktop" $DESTDIR/usr/share/applications
cp ./additional/mime/packages/x-ipk.xml $DESTDIR/usr/share/listaller/mime
cp ./additional/mime/text/x-ips.xml $DESTDIR/usr/share/listaller/mime
cp ./additional/mime-info/listaller-pack.mime $DESTDIR/usr/share/mime-info

#mkdir -p $DESTDIR/usr/bin
#cp ./bin/lipa $DESTDIR/usr/bin/

#Execute installscript
sh ./additional/scripts/postinst

#Create symlink
cd $DESTDIR/usr/bin
ln -s /usr/share/listaller/listallmgr listallmgr

echo "Installation done."
