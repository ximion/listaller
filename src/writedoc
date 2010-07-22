#!/bin/bash

#Looking for pasdoc
PDP=$(which pasdoc)
if ([ -e $PDP ]&&[ "$PDP" != '' ]); then
mkdir -p ../docs/html
cd ../docs/html/

SRCDIR=$SRCDIR
FRONTDIR=$SRCDIR/frontends
TOOLSDIR=$SRCDIR/cmdtools
DAEMONDIR=$SRCDIR/daemon
BINDDIR=$SRCDIR/bind
CREATORDIR=$SRCDIR/licreator
LIBDIR=$LIBDIR

pasdoc --write-uses-list --staronly --format html \
$SRCDIR/listallgo.lpr \
$SRCDIR/lipa.lpr \
$SRCDIR/listallmgr.lpr \
$SRCDIR/liupdate.lpr \
$SRCDIR/libuild.lpr \
$SRCDIR/unibuild.pas \
$SRCDIR/litray.lpr \
$SRCDIR/igobase.pas \
$SRCDIR/distri.pas \
$SRCDIR/dgfrm.pas \
$SRCDIR/litranslator.pas \
$SRCDIR/ipkbuild.pas \
$SRCDIR/ipkdef.pas \
$SRCDIR/manager.pas \
$SRCDIR/updatefrm.pas \
$SRCDIR/liutils.pas \
$SRCDIR/appitem.pas \
$SRCDIR/applist.pas \
$SRCDIR/listallerd.lpr \
$SRCDIR/djobs.pas \
$SRCDIR/litypes.pas \
$SRCDIR/RegExpr.pas \
$SRCDIR/xtypefm.pas \
$SRCDIR/strlocale.pas \
$SRCDIR/uninstall.pas \
$SRCDIR/simdbus.pas \
$SRCDIR/updexecfrm.pas \
$SRCDIR/xtypefm.pas \
$SRCDIR/linotify.pas \
$SRCDIR/callbackprocess.pas \
$BINDDIR/polkit.pas \
$BINDDIR/pktypes.pas \
$BINDDIR/appupdate.pas \
$BINDDIR/gext.pas \
$BINDDIR/packagekit.pas \
$BINDDIR/appman.pas \
$BINDDIR/installer.pas \
$SRCDIR/ipkpackage.pas \
$SRCDIR/gpgsign.pas \
$SRCDIR/tararchive.pas \
$LIBDIR/ipkinstall.pas \
$LIBDIR/limanageapp.pas \
$LIBDIR/mtprocs.pas \
$LIBDIR/softwaredb.pas \
$LIBDIR/libinstaller.lpr \
$LIBDIR/lidbusproc.pas \
$LIBDIR/dderesolve.pas \
$LIBDIR/slibmanage.pas \
$CREATORDIR/editor.pas \
$CREATORDIR/prjwizard.pas \
$CREATORDIR/licreator.lpr \
$CREATORDIR/editor.pas \
$CREATORDIR/prjwizard.pas \
../../opbitmap/gifanimator.pas \
../../synapse/httpsend.pas \
../../synapse/ftpsend.pas \
../../synapse/blcksock.pas
else
  echo " PasDoc was not found. Please install PasDoc."
  exit 8
fi
