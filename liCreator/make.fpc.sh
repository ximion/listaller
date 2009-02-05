#! /bin/sh
#
# Detects and parses the architecture
#

ARCH=$(uname -m)

case "$ARCH" in

 "i686") ARCH="i386";;

 "i586") ARCH="i386";;

 "i486") ARCH="i386";;

esac

if [ $ARCH = "x86_64" ]
then LCLDir="/usr/lib64/lazarus/"
else LCLDir="/usr/lib/lazarus"
fi

echo "LAZTarget: $LCLDir"

# Building the IDE and LCL
#
# Detects and parses the OS
#

OS="linux"

#
# Command line to build the sofware
#
# Create the binary dir

echo "Creating liCreator..."
fpc -Sgi -MObjFPC -O1 -gl -WG -CX -XX -vewnhi -l -Fu../ -Fu$LCLDir/components/synedit/ -Fu$LCLDir/lcl/units/$ARCH-$OS/ -Fu$LCLDir/lcl/units/$ARCH-$OS/gtk2/ -Fu$LCLDir/packager/units/$ARCH-$OS/ -Fu. -o../bin/licreator -dLCL -dLCLgtk2 lipkgcreator.lpr