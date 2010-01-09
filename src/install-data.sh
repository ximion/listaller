#!/usr/bin/env bash
# Parses command line options. Currently supported options are:
#
# DESTDIR		Destination root directory
set -e

for arg; do
  case $arg in
    DESTDIR=*) DESTDIR=${arg#DESTDIR=};;
    prefix=*) prefix=${arg#prefix=};;
  esac;
done

if [ -z "$prefix" ]; then
   export prefix="/usr"
fi

echo "Installing Listaller's platform/widgetset independent data..."
mkdir -p $DESTDIR$prefix/share/pixmaps
mkdir -p $DESTDIR$prefix/share/listaller/mime
mkdir -p $DESTDIR$prefix/share/listaller/graphics
mkdir -p $DESTDIR$prefix/share/listaller/graphics/categories
#Copy graphics
cp ../graphics/header.png $DESTDIR$prefix/share/listaller/graphics/
cp ../graphics/mime-ipk.png $DESTDIR$prefix/share/listaller/graphics/
cp ../graphics/mime-ips.png $DESTDIR$prefix/share/listaller/graphics/
cp ../graphics/listaller.png $DESTDIR$prefix/share/pixmaps
cp ../graphics/wizardimage.png $DESTDIR$prefix/share/listaller/graphics/
cp ../graphics/spackage.png $DESTDIR$prefix/share/listaller/graphics/
cp ../graphics/throbber.gif $DESTDIR$prefix/share/listaller/graphics/
cp ../graphics/categories/accessories.png $DESTDIR$prefix/share/listaller/graphics/categories/
cp ../graphics/categories/all.png $DESTDIR$prefix/share/listaller/graphics/categories/
cp ../graphics/categories/development.png $DESTDIR$prefix/share/listaller/graphics/categories/
cp ../graphics/categories/games.png $DESTDIR$prefix/share/listaller/graphics/categories/
cp ../graphics/categories/graphics.png $DESTDIR$prefix/share/listaller/graphics/categories/
cp ../graphics/categories/internet.png $DESTDIR$prefix/share/listaller/graphics/categories/
cp ../graphics/categories/multimedia.png $DESTDIR$prefix/share/listaller/graphics/categories/
cp ../graphics/categories/office.png $DESTDIR$prefix/share/listaller/graphics/categories/
cp ../graphics/categories/other.png $DESTDIR$prefix/share/listaller/graphics/categories/
cp ../graphics/categories/science.png $DESTDIR$prefix/share/listaller/graphics/categories/
cp ../graphics/categories/system.png $DESTDIR$prefix/share/listaller/graphics/categories/
#Copy other files
cp -dpr ../build/locale/ $DESTDIR$prefix/share/listaller/
rm -f $DESTDIR$prefix/share/listaller/lang/.svn
rm -f $DESTDIR$prefix/share/listaller/lang/.directory

chmod -R 0755 $DESTDIR$prefix/share/listaller/graphics

mkdir -p $DESTDIR$prefix/share/mime
mkdir -p $DESTDIR$prefix/share/mime-info
mkdir -p $DESTDIR/etc/lipa
mkdir -p $DESTDIR/etc/lipa/app-reg

cp ../data/blacklist $DESTDIR/etc/lipa/
cp ../data/mime/x-ipk.xml $DESTDIR$prefix/share/listaller/mime
cp ../data/mime/x-ips.xml $DESTDIR$prefix/share/listaller/mime

#mkdir -p $DESTDIR$prefix/bin
#cp ./build/lipa $DESTDIR$prefix/bin/

#Execute installscript
sh ../data/scripts/postinst

echo "liData installation done."
