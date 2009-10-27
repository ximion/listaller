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

echo "Starting new Listaller build..."
# Command line to build the sofware
# Create necessary dirs
mkdir -p ./bin
mkdir -p ./bin/locale

echo "Compiling libinstaller library..."
lazbuild -B --ws=nogui ./libs/libinstaller.lpr
ln -s ./bin/libinstaller.so.0.4.0 libinstaller.so.0.4
ln -s libinstaller.so.0.4 libinstaller.so

echo "Compiling command-line tool..."
lazbuild -B --ws=nogui lipa.lpr
#fpc -MObjFPC -Sgi -CX -O1 -gl -XX -vewhi -l -Fuabbrevia/ -Fusynapse/ -Fu$LCLDir/lcl/units/$ARCH-$OS/ -Fu$LCLDir/lcl/units/$ARCH-$OS/nogui/ -Fu. -FUbin/ -vm5024 -FEbin/ -olipa -dOpbCompat lipa.lpr
echo "Compiling package build tool..."
lazbuild -B --ws=nogui libuild.lpr
#fpc -MObjFPC -Sgi -CX -O1 -gl -XX -vewnhi -l -Fuopbitmap/ -Fuabbrevia/ -Fu$LCLDir/lcl/units/$ARCH-$OS/ -Fu$LCLDir/lcl/units/$ARCH-$OS/nogui/ -Fu. -FUbin/ -FEbin/ -olibuild -dOpbCompat libuild.lpr

#Compiling lanuage files
echo "Generating language files..."
for i in `find ./locale -name "*.po"`
do
echo "Format $i"
msgfmt -o `expr substr $i 1 $(( ${#i} - 3 ))`.mo $i
mv `expr substr $i 1 $(( ${#i} - 3 ))`.mo ./bin/locale/
done

echo "Listaller build completed."
