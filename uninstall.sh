echo "Uninstallation started."
rm -f /usr/share/listaller/graphics/header.png
rm -f /usr/share/listaller/graphics/mime-ipk.png
rm -f /usr/share/listaller/graphics/wizardimage.png
rm -f /usr/share/listaller/graphics/spackage.png
rm -f /usr/share/listaller/listallgo
rm -f /usr/share/listaller/listallmngr
rm -f /usr/share/listaller/liupdate
rm -f /usr/share/listaller/lang/lclstrconsts.de.po
rm -f /usr/share/listaller/lang/lclstrconsts.fr.po
rm -f /usr/share/listaller/lang/listaller.de.po

rm -f /usr/share/application-registry/listaller.applications
rm -f "/usr/share/applications/Listaller manager.desktop"
rm -f /usr/share/mime/packages/x-ipk.xml
rm -f /usr/share/mime-info/listaller-pack.mime
rm -f /usr/bin/listaller

rmdir /usr/share/listaller

echo "Uninstalled."
