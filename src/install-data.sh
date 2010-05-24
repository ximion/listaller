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
cp ../graphics/mime-ipk.png $DESTDIR$prefix/share/listaller/graphics/
cp ../graphics/mime-ips.png $DESTDIR$prefix/share/listaller/graphics/
cp ../graphics/listaller.png $DESTDIR$prefix/share/pixmaps
cp ../graphics/wizardimage.png $DESTDIR$prefix/share/listaller/graphics/
cp ../graphics/spackage.png $DESTDIR$prefix/share/listaller/graphics/
cp ../graphics/throbber.gif $DESTDIR$prefix/share/listaller/graphics/
cp ../graphics/icon48-appremove.png $DESTDIR$prefix/share/listaller/graphics/
cp ../graphics/icon48-catalog.png $DESTDIR$prefix/share/listaller/graphics/
cp ../graphics/icon48-repository.png $DESTDIR$prefix/share/listaller/graphics/
cp ../graphics/icon48-settings.png $DESTDIR$prefix/share/listaller/graphics/
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
rm -f $DESTDIR$prefix/share/listaller/locale/.directory

#chmod -R 0755 $DESTDIR$prefix/share/listaller/graphics

cp ../data/mime/x-ipk.xml $DESTDIR$prefix/share/listaller/mime
cp ../data/mime/x-ips.xml $DESTDIR$prefix/share/listaller/mime

#mkdir -p $DESTDIR$prefix/bin
#cp ./build/lipa $DESTDIR$prefix/bin/

#Execute installscript if root
if [[ $EUID -ne 0 ]]; then
 sh ../data/scripts/postinst
else
 echo "Not running as root: Did not run postinst script."
fi

echo "liData installation done."
