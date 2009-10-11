#!/bin/bash
# WIDGET                Widgetset the binary should be installed for
set -e

for arg; do
  case $arg in
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

OS="linux"

if [ "$ARCH" == "x86_64" ]; then
LCLDir="/usr/lib64/lazarus"
else
LCLDir="/usr/lib/lazarus"
fi

echo "LAZTarget: $LCLDir"

# Command line to build the sofware
echo "Creating liCreator..."
if [ "$WIDGET" == "qt4" ]; then
#fpc  -MObjFPC -Sgi -CX -O1 -gl -XX -WG -vewni -l -Fu../ -Fu$LCLDir/lcl/units/$ARCH-$OS/ -Fu$LCLDir/components/synedit/ -Fu$LCLDir/components/synedit/units/$ARCH-$OS/ -Fu$LCLDir/ideintf/units/$ARCH-$OS/ -Fu$LCLDir/lcl/units/$ARCH-$OS/qt/ -Fu$LCLDir/packager/units/$ARCH-$OS/ -Fu. -FE../bin/ -olicreator -dUSE_QT_45 -dLCL -dLCLqt lipkgcreator.lpr
lazbuild -B --ws=qt licreator.lpr
mv ../bin/licreator ../bin/qt4/
else
#fpc -MObjFPC -Sgi -CX -O1 -gl -XX -WG -vewni -l -Fu../ -Fu$LCLDir/lcl/units/$ARCH-$OS/ -Fu$LCLDir/components/synedit/ -Fu$LCLDir/components/synedit/units/$ARCH-$OS/ -Fu$LCLDir/ideintf/units/$ARCH-$OS/ -Fu$LCLDir/lcl/units/$ARCH-$OS/ -Fu$LCLDir/lcl/units/$ARCH-$OS/gtk2/ -Fu$LCLDir/packager/units/$ARCH-$OS/ -Fu. -FE../bin/ -olicreator -dUSE_QT_45 -dLCL -dLCLgtk2 lipkgcreator.lpr
lazbuild -B --ws=gtk2 licreator.lpr
mv ../bin/licreator ../bin/gtk2/
fi