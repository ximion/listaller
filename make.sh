#!/usr/bin/env bash
#
# DESTDIR		Destination root directory
set -e

for arg; do
  case $arg in
    DESTDIR=*) DESTDIR=${arg#DESTDIR=};;
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

if [ "$ARCH" == "x86_64" ]; then
LCLDir="/usr/lib64/lazarus"
else
LCLDir="/usr/lib/lazarus"
fi

echo "LAZTarget: $LCLDir"

OS="linux"

echo "Target operating system: $OS"
echo "Active widgetset: $WIDGET"

# Command line to build the sofware
# Create necessary dirs
mkdir -p ./bin
mkdir -p ./bin/locale

echo "Building libraries..."
$LCLDir/lazbuild -B --ws=nogui ./libs/libinstaller.lpr
$LCLDir/lazbuild -B --ws=nogui ./libs/libappmanager.lpr

ln -s ./bin/libinstaller.so.0.4 libinstaller.so
ln -s ./bin/libappmanager.so.0.4 libappmanager.so

echo "Creating command-line tool..."
$LCLDir/lazbuild -B --ws=nogui lipa.lpr
#fpc -MObjFPC -Sgi -CX -O1 -gl -XX -vewhi -l -Fuabbrevia/ -Fusynapse/ -Fu$LCLDir/lcl/units/$ARCH-$OS/ -Fu$LCLDir/lcl/units/$ARCH-$OS/nogui/ -Fu. -FUbin/ -vm5024 -FEbin/ -olipa -dOpbCompat lipa.lpr
echo "Creating package build tool..."
$LCLDir/lazbuild -B --ws=nogui libuild.lpr
#fpc -MObjFPC -Sgi -CX -O1 -gl -XX -vewnhi -l -Fuopbitmap/ -Fuabbrevia/ -Fu$LCLDir/lcl/units/$ARCH-$OS/ -Fu$LCLDir/lcl/units/$ARCH-$OS/nogui/ -Fu. -FUbin/ -FEbin/ -olibuild -dOpbCompat libuild.lpr
echo "Creating unified build tool..."
$LCLDir/lazbuild -B --ws=nogui unibuild.lpr
#fpc -MObjFPC -Sgi -CX -O1 -gl -XX -vewnhi -l -Fuopbitmap/ -Fuabbrevia/ -Fu$LCLDir/lcl/units/$ARCH-$OS/ -Fu$LCLDir/lcl/units/$ARCH-$OS/nogui/ -Fu. -FUbin/ -FEbin/ -ounibuild -dOpbCompat unibuild.lpr

#Compiling lanuage files
echo "Compiling language files..."
for i in `find ./locale -name "*.po"`
do
echo "Compiling $i"
msgfmt -o `expr substr $i 1 $(( ${#i} - 3 ))`.mo $i
mv `expr substr $i 1 $(( ${#i} - 3 ))`.mo ./bin/locale/
done

echo "Listaller build completed."
