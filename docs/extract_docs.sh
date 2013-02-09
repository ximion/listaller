#!/bin/sh
set -e

rm -rf ./api
cd ../src
valadoc -b . -o ../docs/api --vapidir=../vapi \
	--pkg=config \
	--pkg=gee-1.0 \
	--pkg=gio-2.0 \
	--pkg=libsoup-2.4 \
	--pkg=sqlite3 \
	--pkg=libarchive \
	--pkg=gpgme \
	--pkg=libxml-2.0 \
	--pkg=rdf-minimal \
	--pkg=packagekit-glib2 \
	--package-name=listaller \
	--package-version=0.5.7 \
	*.vala
cd ../docs
