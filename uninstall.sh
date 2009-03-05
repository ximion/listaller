#!/bin/bash
echo "Uninstallation started."
ARCH=$(uname -m)
case "$ARCH" in

 "i686") ARCH="i386";;

 "i586") ARCH="i386";;

 "i486") ARCH="i386";;
esac
if [ $ARCH = "x86_64" ]; then TDir="/usr/lib64/"
else TDir="/usr/lib/"
fi

rm -f /$TDir/listaller/graphics/header.png
rm -f /$TDir/listaller/graphics/mime-ipk.png
rm -f /$TDir/listaller/graphics/wizardimage.png
rm -f /$TDir/listaller/graphics/spackage.png
rm -f /$TDir/listaller/listallgo
rm -f /$TDir/listaller/listallmngr
rm -f /$TDir/listaller/liupdate
rm -f /$TDir/listaller/lang/lclstrconsts.de.po
rm -f /$TDir/listaller/lang/lclstrconsts.fr.po
rm -f /$TDir/listaller/lang/listaller.de.po

rm -f "/usr/share/applications/Listaller manager.desktop"
rm -f /$TDir/mime/packages/x-ipk.xml
rm -f /$TDir/mime-info/listaller-pack.mime
rm -f /usr/bin/listaller

rmdir /$TDir/listaller

echo "Uninstalled."
