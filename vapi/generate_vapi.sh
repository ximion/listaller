#!/bin/sh
# Small helper script to generate vapi files from GIR

vapigen --metadatadir=. -d . \
	/usr/share/gir-1.0/PackageKitGlib-1.0.gir \
	--library=packagekit-glib2 \
	--pkg glib-2.0 --pkg gio-2.0 --pkg gobject-2.0
