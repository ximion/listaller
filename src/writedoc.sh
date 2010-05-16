#!/bin/bash

#Looking for pasdoc
PDP=$(which pasdoc)
if ([ -e $PDP ]&&[ "$PDP" != '' ]); then
mkdir -p ../docs/html
cd ../docs/html/
pasdoc --write-uses-list --staronly --format html \
../../src/listallgo.lpr \
../../src/lipa.lpr \
../../src/listallmgr.lpr \
../../src/liupdate.lpr \
../../src/libuild.lpr \
../../src/unibuild.pas \
../../src/litray.lpr \
../../src/igobase.pas \
../../src/distri.pas \
../../src/dgfrm.pas \
../../src/litranslator.pas \
../../src/ipkbuild.pas \
../../src/ipkdef.pas \
../../src/manager.pas \
../../src/updatefrm.pas \
../../src/liutils.pas \
../../src/appitem.pas \
../../src/applist.pas \
../../src/listallerd.lpr \
../../src/djobs.pas \
../../src/litypes.pas \
../../src/RegExpr.pas \
../../src/xtypefm.pas \
../../src/strlocale.pas \
../../src/uninstall.pas \
../../src/simdbus.pas \
../../src/updexecfrm.pas \
../../src/xtypefm.pas \
../../src/linotify.pas \
../../src/callbackprocess.pas \
../../intf/polkit.pas \
../../intf/pktypes.pas \
../../intf/appupdate.pas \
../../intf/gext.pas \
../../intf/packagekit.pas \
../../intf/appman.pas \
../../intf/installer.pas \
../../packager/ipkpackage.pas \
../../packager/gpgsign.pas \
../../packager/tarfile.pas \
../../lib/ipkinstall.pas \
../../lib/limanageapp.pas \
../../lib/mtprocs.pas \
../../lib/libinstaller.lpr \
../../lib/lidbusproc.pas \
../../licreator/editor.pas \
../../licreator/prjwizard.pas \
../../licreator/licreator.lpr \
../../licreator/editor.pas \
../../licreator/prjwizard.pas \
../../opbitmap/gifanimator.pas \
../../synapse/httpsend.pas \
../../synapse/ftpsend.pas \
../../synapse/blcksock.pas
else
  echo " PasDoc was not found. Please install PasDoc."
  exit 8
fi
