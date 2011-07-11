/* -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*-
 *
 * Copyright (C) 2011 Richard Hughes <richard@hughsie.com>
 *
 * Licensed under the GNU General Public License Version 2
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
 */

/* COMPILETLY UNTESTED */

/* You need to compile this with the folowing commands:
 * {
libtool --mode=compile gcc -Wall -Werror -g -O `pkg-config --cflags --libs packagekit-plugin` -c pk-plugin-external.c
libtool --mode=link gcc -g -O -module -avoid-version -o libpk-plugin-external.la -rpath /home/hughsie/.root/lib/packagekit-plugins pk-plugin-external.lo
libtool --mode=install cp libpk-plugin-external.la /home/hughsie/.root/lib/packagekit-plugins/libpk-plugin-external.la
 *
 * or, if using automake:
 * {
plugindir = $(LIBDIR)/packagekit-plugins
plugin_LTLIBRARIES = libpk_plugin_external.la
libpk_plugin_external_la_SOURCES = pk-plugin-external.c
libpk_plugin_external_la_LIBADD = $(PK_PLUGIN_LIBS)
libpk_plugin_external_la_LDFLAGS = -module -avoid-version
libpk_plugin_external_la_CFLAGS = $(PK_PLUGIN_CFLAGS)
 * } 
 */

#define I_KNOW_THE_PACKAGEKIT_PLUGIN_API_IS_SUBJECT_TO_CHANGE
#define I_KNOW_THE_PACKAGEKIT_GLIB2_API_IS_SUBJECT_TO_CHANGE

#include <stdlib.h>
#include <PackageKit/plugin/packagekit-plugin.h>

#include "pk-listaller.h"

typedef struct {
	ListallerManager	*mgr;
	ListallerSettings	*conf;
} PluginPrivate;

static PluginPrivate *priv;

/* include all of pk-listaller.c here */

/* to emit packages, just do:
 *
 *  PkBackend *backend;
 *  backend = pk_transaction_get_backend (transaction);
 *  pk_backend_package (PK_INFO_ENUM_AVAILABLE, "dave;0.0.1;i386;fedora", "Dave");
 */

/**
 * pk_transaction_plugin_get_description:
 */
const gchar *
pk_transaction_plugin_get_description (void)
{
	return "An external plugin that compiles outside of PK";
}

/**
 * pk_transaction_plugin_initialize:
 */
void
pk_transaction_plugin_initialize (PkTransaction *transaction)
{
	/* create private area */
	priv = g_new0 (PluginPrivate, 1);
	priv->conf = listaller_settings_new (TRUE);
	priv->mgr = listaller_manager_new (priv->conf);
	pk_transaction_add_supported_mime_type (transaction,
						"application/x-installation");
}

/**
 * pk_transaction_plugin_destroy:
 */
void
pk_transaction_plugin_destroy (PkTransaction *transaction)
{
	g_object_unref (priv->conf);
	g_object_unref (priv->mgr);
	g_free (priv);
}

/**
 * pk_transaction_plugin_started:
 */
void
pk_transaction_plugin_started (PkTransaction *transaction)
{
	gboolean ret;
	PkRoleEnum role;
	gchar **values;
	gchar **package_ids;
	gchar **full_paths;
	PkBackend *backend;
	PkLiStatus listatus;

	/* reset the Listaller fake-backend */
	pk_listaller_reset (priv->pkli);
	backend = pk_transaction_get_backend (transaction);
	pk_backend_set_status (backend, PK_STATUS_ENUM_SETUP);

	/* handle these before the transaction has been run */
	role = pk_transaction_get_role (transaction);
	if (role == PK_ROLE_ENUM_SEARCH_NAME ||
	    role == PK_ROLE_ENUM_SEARCH_DETAILS) {
		values = pk_transaction_get_values (transaction);
		pk_listaller_find_applications (priv->pkli, values);
		goto out;
	}

	if (role == PK_ROLE_ENUM_GET_DETAILS) {
		package_ids = pk_transaction_get_package_ids (transaction);
		pk_listaller_get_details (priv->pkli, &package_ids);
		goto out;
	}

	/* remove Listaller-specific package ids simulate-* actions */
	if (role == PK_ROLE_ENUM_SIMULATE_REMOVE_PACKAGES ||
	    role == PK_ROLE_ENUM_SIMULATE_INSTALL_PACKAGES) {
		pk_listaller_delete_app_ids (priv->pkli, &package_ids);
	}

	/* handle Listaller transactions before the backend gets initialized */
	if (role == PK_ROLE_ENUM_INSTALL_FILES) {
		full_paths = pk_transaction_get_full_paths (transaction);
		pk_listaller_install_files (priv->pkli, &full_paths);
		goto out;
	}
	if (role == PK_ROLE_ENUM_REMOVE_PACKAGES) {
		package_ids = pk_transaction_get_package_ids (transaction);
		pk_listaller_remove_applications (priv->pkli, &package_ids);
		goto out;
	}

	listatus = pk_listaller_get_status (priv->pkli);
	if (listatus == PK_LISTALLER_STATUS_FAILED) {
		pk_backend_error_code (backend, PK_ERROR_ENUM_FAILED, "failed to do something");
		goto out;
	}
out:
	return;
}

/**
 * pk_transaction_plugin_finished_end:
 */
void
pk_transaction_plugin_finished_end (PkTransaction *transaction)
{
	/* update application databases */
	pk_listaller_scan_applications (priv->pkli);
}
