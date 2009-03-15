#! /bin/sh

# Detects and parses the architecture
ARCH=$(uname -m)

case "$ARCH" in
 "i686") ARCH="i386";;
 "i586") ARCH="i386";;
 "i486") ARCH="i386";;
esac

#Detects path to LCL
if [ $ARCH = "x86_64" ]
then LCLDir="/usr/lib64/lazarus/"
else LCLDir="/usr/lib/lazarus"
fi

echo "LAZTarget: $LCLDir"

# Command line to build the sofware
echo "Creating liCreator..."
$LCLDir/lazbuild -B --ws=gtk2 lipkgcreator.lpr