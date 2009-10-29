#!/usr/bin/env bash
# Parses command line options. Currently supported options are:
#
# DESTDIR		Destination root directory
set -e

for arg; do
  case $arg in
    DESTDIR=*) DESTDIR=${arg#DESTDIR=};;
  esac;
done

echo "Installing Listaller's platform/widgetset independent data..."
mkdir -p $DESTDIR/usr/share/pixmaps
mkdir -p $DESTDIR/usr/share/listaller/mime
mkdir -p $DESTDIR/usr/share/listaller/graphics
mkdir -p $DESTDIR/usr/share/listaller/graphics/categories
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
cp -dpr ./build/locale/ $DESTDIR/usr/share/listaller/
rm -f $DESTDIR/usr/share/listaller/lang/.svn
rm -f $DESTDIR/usr/share/listaller/lang/.directory

chmod -R 0755 $DESTDIR/usr/share/listaller/graphics

mkdir -p $DESTDIR/usr/share/mime
mkdir -p $DESTDIR/usr/share/mime-info
mkdir -p $DESTDIR/etc/lipa
mkdir -p $DESTDIR/etc/lipa/app-reg

cp ./data/blacklist $DESTDIR/etc/lipa/
cp ./data/mime/x-ipk.xml $DESTDIR/usr/share/listaller/mime
cp ./data/mime/x-ips.xml $DESTDIR/usr/share/listaller/mime

#mkdir -p $DESTDIR/usr/bin
#cp ./bin/lipa $DESTDIR/usr/bin/

#Execute installscript
sh ./data/scripts/postinst

echo "liData installation done."