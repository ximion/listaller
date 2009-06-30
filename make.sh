#!/usr/bin/env bash
# Enable halt on error
 #set -e
#
# DESTDIR		Destination root directory
# WIDGET                Widgetset the binary should be installed for
for arg; do
  case $arg in
    DESTDIR=*) DESTDIR=${arg#DESTDIR=};;
    WIDGET=*) WIDGET=${arg#WIDGET=};;
  esac;
done

# Detects and parses the architecture
ARCH=$(uname -m)

case "$ARCH" in
 "i686") ARCH="i386";;
 "i586") ARCH="i386";;
 "i486") ARCH="i386";;
esac

echo "Target architecture: $ARCH"

if [ $ARCH = "x86_64" ]; then LCLDir="/usr/lib64/lazarus/"
else LCLDir="/usr/lib/lazarus"
fi

LCLDir="/usr/lib/lazarus"

echo "LAZTarget: $LCLDir"

OS="linux"

echo "Target operating system: $OS"
echo "Active widgetset: $WIDGET"

# Command line to build the sofware
# Create the binary dir
# Create necessary dirs
mkdir -p ./bin
mkdir -p ./bin/locale
mkdir -p ./bin/qt4
mkdir -p ./bin/gtk2

#Create symlinks for GTK+ libraries
ln -s -f /usr/lib/libgdk_pixbuf-2.0.so libgdk_pixbuf.so
ln -s -f /usr/lib/libgtk-x11-2.0.so libgtk.so
ln -s -f /usr/lib/libglib-2.0.so libglib.so
ln -s -f /usr/lib/libgdk-x11-2.0.so libgdk.so

VER=$(fpc -iW)
echo "Compiler version: $VER"

#Workaround for Lazarus-Alpha-packaging bug
#echo "LAZBuild version: $($LCLDir/lazbuild -v)"

echo "Creating installer..."
if [ "$WIDGET" == "qt4" ]; then
#./libuild PR=listallgo.lpr O=listallgo WIDGET=qt4
$LCLDir/lazbuild -B --ws=qt listallgo.lpr
mv ./bin/listallgo ./bin/qt4/
else
#./libuild PR=listallgo.lpr O=listallgo WIDGET=gtk2
$LCLDir/lazbuild -B --ws=gtk2 listallgo.lpr
mv ./bin/listallgo ./bin/gtk2/
fi
echo "Creating software-manager..."
if [ "$WIDGET" == "qt4" ]; then
#./libuild PR=listallmngr.lpr O=listallmgr WIDGET=qt4
$LCLDir/lazbuild -B --ws=qt listallmgr.lpr
mv ./bin/listallmgr ./bin/qt4/
else
#./libuild PR=listallmngr.lpr O=listallmgr WIDGET=gtk2
$LCLDir/lazbuild -B --ws=gtk2 listallmgr.lpr
mv ./bin/listallmgr ./bin/gtk2/
fi
echo "Creating updater..."
if [ "$WIDGET" == "qt4" ]; then
#./libuild PR=liupdate.lpr O=liupdate WIDGET=qt4
$LCLDir/lazbuild -B --ws=qt liupdate.lpr
mv ./bin/liupdate ./bin/qt4/
else
#./libuild PR=liupdate.lpr O=liupdate WIDGET=gtk2
$LCLDir/lazbuild -B --ws=gtk2 liupdate.lpr
mv ./bin/liupdate ./bin/gtk2/
fi

#Also workarount for lazbuild bug in Lazarus-Alpha
WIDGET="gtk2"

# We have to use GTK2 widgeset instead of the "nogui" widget untill all bugs in LazSVn are fixed.
echo "Creating command-line tool..."
#$LCLDir/lazbuild -B --ws=nogui lipa.lpr
fpc -MObjFPC -Sgi -CX -O1 -gl -XX -vewhi -l -Fuopbitmap/ -Fuabbrevia/ -Fu$LCLDir/lcl/units/$ARCH-$OS/ -Fu$LCLDir/lcl/units/$ARCH-$OS/nogui/ -Fu. -FUbin/ -FEbin/ -olipa -dOpbCompat lipa.lpr
echo "Creating package build tool..."
#$LCLDir/lazbuild -B --ws=nogui libuild.lpr
fpc  -MObjFPC -Sgi -CX -O1 -gl -XX -vewnhi -l -Fuopbitmap/ -Fuabbrevia/ -Fu$LCLDir/lcl/units/$ARCH-$OS/ -Fu$LCLDir/lcl/units/$ARCH-$OS/nogui/ -Fu. -FUbin/ -FEbin/ -olibuild -dOpbCompat libuild.lpr
echo "Creating unified build tool..."
#$LCLDir/lazbuild -B --ws=nogui unibuild.lpr
fpc  -MObjFPC -Sgi -CX -O1 -gl -XX -vewnhi -l -Fuopbitmap/ -Fuabbrevia/ -Fu$LCLDir/lcl/units/$ARCH-$OS/ -Fu$LCLDir/lcl/units/$ARCH-$OS/nogui/ -Fu. -FUbin/ -FEbin/ -ounibuild -dOpbCompat unibuild.lpr

#Compiling lanuage files
echo "Compiling language files..."
for i in `find ./locale -name "*.po"`
do
echo "Compiling $i"
msgfmt -o `expr substr $i 1 $(( ${#i} - 3 ))`.mo $i
mv `expr substr $i 1 $(( ${#i} - 3 ))`.mo ./bin/locale/
done

echo "Listaller build completed."
