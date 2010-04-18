#!/bin/bash

#Looking for pasdoc
PDP=$(which pasdoc)
if ([ -e $PDP ]&&[ "$PDP" != '' ]); then
mkdir -p ../docs/html
cd ../docs/html/
pasdoc --write-uses-list --staronly --format html \
../../src/listallgo.lpr ../../src/lipa.lpr ../../src/listallmgr.lpr ../../src/liupdate.lpr \
../../src/libuild.lpr ../../src/unibuild.pas ../../src/litray.lpr \
../../licreator/licreator.lpr ../../licreator/editor.pas \
../../licreator/prjwizard.pas ../../src/igobase.pas ../../src/distri.pas ../../src/dgfrm.pas \
../../src/litranslator.pas ../../lib/ipkinstall.pas ../../lib/limanageapp.pas \
../../src/ipkbuild.pas ../../src/ipkdef.pas ../../synapse/httpsend.pas \
../../synapse/ftpsend.pas ../../synapse/blcksock.pas ../../src/manager.pas \
../../src/updatefrm.pas ../../src/licommon.pas ../../src/appitem.pas ../../src/applist.pas \
../../intf/packagekit.pas ../../intf/appman.pas ../../src/litypes.pas ../../src/libasic.pas \
../../intf/installer.pas ../../src/RegExpr.pas ../../src/xtypefm.pas \
../../src/trstrings.pas ../../src/uninstall.pas ../../src/updexecfrm.pas ../../src/xtypefm.pas \
../../src/linotify.pas ../../licreator/editor.pas ../../licreator/prjwizard.pas \
../../opbitmap/gifanimator.pas ../../lib/mtprocs.pas ../../lib/libinstaller.lpr ../../lib/lidbusproc.pas \
../../intf/polkit.pas ../../intf/pkdesktop.pas ../../intf/pkenum.pas ../../intf/appupdate.pas \
../../intf/gext.pas ../../src/listallerd.lpr ../../src/djobs.pas \
../../packager/ipkpackage.pas ../../packager/gpgsign.pas ../../packager/callbackprocess.pas \
../../packager/tarfile.pas ../../src/simdbus.pas
else
  echo " PasDoc was not found. Please install PasDoc."
  exit 8
fi
