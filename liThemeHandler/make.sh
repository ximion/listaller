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

echo "Building liThemeMgr..."
$LCLDir/lazbuild -B --ws=gtk2 litheme.lpr