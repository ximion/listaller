/* -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*-
 *
 * Copyright (C) 2010-2011 Matthias Klumpp <matthias@tenstral.net>
 *                    2011 Richard Hughes <richard@hughsie.com>
 * Licensed under the GNU Lesser General Public License Version 3
 *
 * This library is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this library.  If not, see <http://www.gnu.org/licenses/>.
 */

#include <stdlib.h>
#include <packagekit-glib2/packagekit.h>
#include <plugin/packagekit-plugin.h>
#include "listaller_internal.h"

struct PkPluginPrivate {
	ListallerManager	*mgr;
	ListallerSettings	*conf;
	ListallerPkBackendProxy *pkbproxy;
};

/**
 * pk_listaller_reset:
 */
void
pk_listaller_reset (PkPlugin *plugin)
{
	/* reset the native backend */
	pk_backend_reset (plugin->backend);
}

/**
 * pk_listaller_native_backend_skip:
 */
static void
pk_listaller_native_backend_skip (PkPlugin *plugin)
{
	PkExitEnum exit;
	exit = pk_backend_get_exit_code (plugin->backend);

	/* only skip transaction if we don't have an error already */
	if (!pk_backend_get_is_error_set (plugin->backend)) {
		pk_backend_set_exit_code (plugin->backend, PK_EXIT_ENUM_SKIP_TRANSACTION);
	}
}

/**
 * pk_listaller_scan_applications:
 *
 * Updates the current application database
 *
 * Return value: True if successful
 */
gboolean
pk_listaller_scan_applications (PkPlugin *plugin)
{
	gboolean res;

	/* run it */
	res = listaller_manager_scan_applications (plugin->priv->mgr);

	/* print warning if scan fails */
	if (!res) {
		g_warning ("listaller: unable to update application database.");
	} else {
		g_debug ("listaller: application database update finished.");
	}

	return res;
}

/**
 * pk_listaller_find_applications:
 *
 * Find Listaller apps by name
 */
void
pk_listaller_find_applications (PkPlugin *plugin, gchar **values)
{
	g_debug ("listaller: searching for applications: %s", values[0]);
	listaller_manager_find_applications_by_values (plugin->priv->mgr,
						       LISTALLER_APP_SOURCE_EXTERN,
						       values,
						       NULL);
}

/**
 * pk_packages_get_listaller_file:
 *
 * Get a IPK files from file list and remove it from list
 **/
gchar*
pk_packages_get_listaller_file (gchar ***full_paths)
{
	guint i;
	GPtrArray *pkarray;
	gchar *res = NULL;

	pkarray = g_ptr_array_new_with_free_func (g_free);;
	for (i = 0; i < g_strv_length(*full_paths); i++) {
		if (g_str_has_suffix (*full_paths[i], ".ipk")) {
			res = g_strdup (*full_paths[i]);
			break;
		} else {
			g_ptr_array_add (pkarray, g_strdup (*full_paths[i]));
		}
	}

	g_strfreev (*full_paths);
	*full_paths = pk_ptr_array_to_strv (pkarray);
	g_ptr_array_unref (pkarray);

	return res;
}

/**
 * pk_listaller_contains_listaller_files:
 *
 * Checks if there are Listaller packages in full_paths
 **/
gboolean
pk_listaller_contains_listaller_files (gchar **full_paths)
{
	gboolean ret = FALSE;
	guint i;

	for (i=0; i<g_strv_length (full_paths); i++) {
		if (g_str_has_suffix (full_paths[i], ".ipk")) {
			ret = TRUE;
			break;
		}
	}
	return ret;
}

/**
 * pk_listaller_appitem_from_pkid:
 *
 * Receive status change messages
 **/
static ListallerAppItem*
pk_listaller_appitem_from_pkid (const gchar *package_id)
{
	gchar **parts = NULL;
	gchar **tmp = NULL;
	ListallerAppItem *item = NULL;

	parts = pk_package_id_split (package_id);
	tmp = g_strsplit (parts[3], "%", 2);
	if (g_strcmp0 (tmp[0], "local:listaller") != 0)
		goto out;

	item = listaller_app_item_new_blank ();
	listaller_app_item_set_idname (item, parts[0]);
	listaller_app_item_set_version (item, parts[1]);
	listaller_app_item_set_desktop_file (item, tmp[1]);
	listaller_app_item_set_shared (item, TRUE);

	g_debug ("listaller: <appid> %s %s %s", parts[0], parts[1], tmp[1]);

out:
	g_strfreev (tmp);
	g_strfreev (parts);
	return item;
}

/**
 * pk_listaller_setup_ready:
 *
 * Return value: The generated package ID or NULL
 **/
static gchar*
pk_listaller_pkid_from_appitem (ListallerAppItem *item)
{
	const gchar *appid;
	const gchar *version;
	const gchar *desktop_file;
	gchar *data;
	gchar *package_id;
	g_return_val_if_fail (LISTALLER_IS_APP_ITEM (item), NULL);

	appid = listaller_app_item_get_idname (item);
	version = listaller_app_item_get_version (item);
	desktop_file = listaller_app_item_get_desktop_file (item);

	data = g_strconcat ("local:listaller%", desktop_file, NULL);

	package_id = pk_package_id_build (appid, version, "current", data);

	g_free (data);
	return package_id;
}

/**
 * pk_listaller_remove_applications:
 *
 * Remove applications which are managed by Listaller.
 **/
void
pk_listaller_remove_applications (PkPlugin *plugin, gchar **package_ids)
{
	ListallerAppItem *app = NULL;
	guint i;

	g_debug ("listaller: remove applications");

	for (i=0; package_ids[i] != NULL; i++) {
		app = pk_listaller_appitem_from_pkid (package_ids[i]);
		if (app == NULL)
			continue;

		listaller_manager_remove_application (plugin->priv->mgr, app);
		g_object_unref (app);
	};
}

/**
 * pk_listaller_get_details:
 */
void
pk_listaller_get_details (PkPlugin *plugin, gchar **package_ids)
{
	const gchar *description;
	const gchar *license;
	const gchar *url;
	ListallerAppItem *app;
	guint i;

	g_debug ("listaller: running get_details ()");
	pk_listaller_reset (plugin);

	for (i=0; package_ids[i] != NULL; i++) {
		app = pk_listaller_appitem_from_pkid (package_ids[i]);

		description = listaller_app_item_get_description (app);
		license = listaller_app_item_get_license_name (app);
		url = listaller_app_item_get_url (app);

		/* emit */
		pk_backend_details (plugin->backend, package_ids[i],
					license,
					PK_GROUP_ENUM_UNKNOWN,
					description,
					url,
					0);
	};
}

static void listaller_application_cb (GObject *sender, ListallerAppItem *item, PkPlugin *plugin)
{
	gchar *package_id;

	package_id = pk_listaller_pkid_from_appitem (item);
	if (package_id == NULL) {
		g_debug ("listaller: <error> generated PK package-id was NULL, ignoring entry.");
		return;
	}
	g_debug ("listaller: new app found -> %s", listaller_app_item_get_appid (item));

	/* emit */
	pk_backend_package (plugin->backend, PK_INFO_ENUM_INSTALLED, package_id,
			    listaller_app_item_get_summary (item));

	g_free (package_id);
}

static void listaller_error_code_cb (GObject *sender, ListallerErrorItem *error, PkPlugin *plugin)
{
	g_return_if_fail (error != NULL);

	/* don't try to set errors twice */
	if (pk_backend_get_is_error_set (plugin->backend))
		return;

	/* emit */
	pk_backend_error_code (plugin->backend, PK_ERROR_ENUM_INTERNAL_ERROR,
				listaller_error_item_get_details (error));
}

static void listaller_message_cb (GObject *sender, ListallerMessageItem *message, PkPlugin *plugin)
{
	ListallerMessageEnum mtype;
	gchar *text;

	g_return_if_fail (message != NULL);

	/* PackageKit won't forward these messages to the frontend */
	mtype = listaller_message_item_get_mtype (message);
	text = g_strconcat ("listaller:", " ", listaller_message_item_get_details (message), NULL);
	switch (mtype) {
		case LISTALLER_MESSAGE_ENUM_INFO: g_message ("%s\n", text);
						break;
		case LISTALLER_MESSAGE_ENUM_WARNING: g_warning ("listaller: %s\n", text);
						break;
		case LISTALLER_MESSAGE_ENUM_CRITICAL: g_critical ("listaller: %s\n", text);
						break;
		default:
			g_message ("<unknown type> %s", text);
			break;
	}
	g_free (text);
}


static void listaller_progress_change_cb (GObject* sender, gint progress, gint subprogress, PkPlugin *plugin)
{
	/* emit */
	pk_backend_set_percentage (plugin->backend, progress);
	pk_backend_set_sub_percentage (plugin->backend, subprogress);
}

static void listaller_status_change_cb (GObject *sender, ListallerStatusItem *status, PkPlugin *plugin)
{
	ListallerStatusEnum listatus;
	PkStatusEnum pkstatus;
	g_return_if_fail (status != NULL);

	listatus = listaller_status_item_get_status (status);
	pkstatus = PK_STATUS_ENUM_UNKNOWN;

	if (listatus == LISTALLER_STATUS_ENUM_ACTION_STARTED) {
		pkstatus = PK_STATUS_ENUM_RUNNING;
	} else if (listatus == LISTALLER_STATUS_ENUM_INSTALLATION_FINISHED) {
		pkstatus = PK_STATUS_ENUM_FINISHED;
	}

	g_debug ("listaller: <status-info> %s", listaller_status_item_get_info (status));

	/* emit */
	if (pkstatus != PK_STATUS_ENUM_UNKNOWN)
		pk_backend_set_status (plugin->backend, pkstatus);
}

/**
 * pk_listaller_install_file:
 *
 * Install the IPK package in filename
 *
 * Return value: True if successful
 **/
static gboolean
pk_listaller_install_file (PkPlugin *plugin, const gchar *filename)
{
	gboolean ret = FALSE;
	ListallerSetup *setup;
	gchar* package_id;
	ListallerAppItem *app = NULL;

	setup = listaller_setup_new (filename, plugin->priv->conf);
	g_signal_connect (setup, "error-code",
			  G_CALLBACK (listaller_error_code_cb), plugin);
	g_signal_connect (setup, "message",
			  G_CALLBACK (listaller_message_cb), plugin);
	g_signal_connect (setup, "status-changed",
			  G_CALLBACK (listaller_status_change_cb), plugin);
	g_signal_connect (setup, "progress-changed",
			  G_CALLBACK (listaller_progress_change_cb), plugin);

	pk_listaller_reset (plugin);

	/* now intialize the new setup */
	ret = listaller_setup_initialize (setup);
	if (!ret)
		goto out;

	/* install the application! */
	ret = listaller_setup_run_installation (setup);

	/* fetch installed application */
	app = listaller_setup_get_current_application (setup);

	package_id = pk_listaller_pkid_from_appitem (app);
	if (package_id == NULL) {
		g_debug ("listaller: <error> Unable to build package-id from app-id!");
	} else if (!pk_backend_get_is_error_set (plugin->backend)) {
		/* emit */
		pk_backend_package (plugin->backend, PK_INFO_ENUM_INSTALLED,
					package_id,
					listaller_app_item_get_summary (app));
		g_free (package_id);
	}
	g_object_unref (app);

	/* close setup */
	g_object_unref (setup);

out:
	return ret;
}

/**
 * pk_listaller_install_files:
 *
 * Install the IPK packages in file_paths
 **/
void
pk_listaller_install_files (PkPlugin *plugin, gchar **filenames)
{
	gboolean ret = FALSE;
	guint i;

	for (i=0; filenames[i] != NULL; i++) {

		g_debug ("listaller: Current path is: %s", filenames[i]);
		ret = pk_listaller_install_file (plugin, filenames[i]);
		if (!ret)
			break;
	}
}

/**
 * pk_listaller_is_package:
 */
static gboolean
pk_listaller_is_package (const gchar *package_id)
{
	return (g_strstr_len (package_id, -1,
			     "local:listaller") != NULL);
}

/**
 * pk_listaller_filter_listaller_packages:
 */
static gchar **
pk_listaller_filter_listaller_packages (PkTransaction *transaction,
					gchar **package_ids)
{
	gboolean ret = FALSE;
	gchar **package_ids_new = NULL;
	gchar **retval = NULL;
	GPtrArray *listaller = NULL;
	GPtrArray *native = NULL;
	guint i;

	/* just do a quick pass as an optimisation for the common case */
	for (i=0; package_ids[i] != NULL; i++) {
		ret = pk_listaller_is_package (package_ids[i]);
		if (ret)
			break;
	}
	if (!ret)
		goto out;

	/* find and filter listaller packages */
	native = g_ptr_array_new_with_free_func (g_free);
	listaller = g_ptr_array_new_with_free_func (g_free);

	for (i=0; package_ids[i] != NULL; i++) {
		ret = pk_listaller_is_package (package_ids[i]);
		if (ret) {
			g_ptr_array_add (listaller,
					 g_strdup (package_ids[i]));
		} else {
			g_ptr_array_add (native,
					 g_strdup (package_ids[i]));
		}
	}

	/* pickle the arrays */
	retval = pk_ptr_array_to_strv (listaller);
	package_ids_new = pk_ptr_array_to_strv (native);
	pk_transaction_set_package_ids (transaction, package_ids_new);
out:
	g_strfreev (package_ids_new);
	if (native != NULL)
		g_ptr_array_unref (native);
	if (listaller != NULL)
		g_ptr_array_unref (listaller);
	return retval;
}

/**
 * pk_listaller_is_file:
 *
 * TODO: should get content type (instead of just checking file extension)...
 */
static gboolean
pk_listaller_is_file (const gchar *filename)
{
	return g_str_has_suffix (filename, ".ipk");
}

/**
 * pk_listaller_filter_listaller_files:
 */
static gchar **
pk_listaller_filter_listaller_files (PkTransaction *transaction,
				     gchar **files)
{
	gchar **files_new = NULL;
	gchar **retval = NULL;
	GPtrArray *native = NULL;
	GPtrArray *listaller = NULL;
	guint i;
	gboolean ret = FALSE;

	/* just do a quick pass as an optimisation for the common case */
	for (i=0; files[i] != NULL; i++) {
		ret = pk_listaller_is_file (files[i]);
		if (ret)
			break;
	}
	if (!ret)
		goto out;

	/* find and filter listaller packages */
	native = g_ptr_array_new_with_free_func (g_free);
	listaller = g_ptr_array_new_with_free_func (g_free);

	for (i=0; files[i] != NULL; i++) {
		ret = pk_listaller_is_file (files[i]);
		if (ret) {
			g_ptr_array_add (listaller,
					 g_strdup (files[i]));
		} else {
			g_ptr_array_add (native,
					 g_strdup (files[i]));
		}
	}

	/* pickle the arrays */
	retval = pk_ptr_array_to_strv (listaller);
	files_new = pk_ptr_array_to_strv (native);
	pk_transaction_set_full_paths (transaction, files_new);
out:
	g_strfreev (files_new);
	if (native != NULL)
		g_ptr_array_unref (native);
	if (listaller != NULL)
		g_ptr_array_unref (listaller);
	return retval;
}

/**
 * pk_plugin_started:
 */
void
pk_plugin_transaction_started (PkPlugin *plugin,
			       PkTransaction *transaction)
{
	PkRoleEnum role;
	gchar **values;
	gchar **package_ids;
	gchar **data = NULL;
	gchar **full_paths;

	/* reset the Listaller fake-backend */
	pk_listaller_reset (plugin);
	pk_backend_set_status (plugin->backend, PK_STATUS_ENUM_SETUP);

	/* handle these before the transaction has been run */
	role = pk_transaction_get_role (transaction);
	if (role == PK_ROLE_ENUM_SEARCH_NAME ||
	    role == PK_ROLE_ENUM_SEARCH_DETAILS) {
		values = pk_transaction_get_values (transaction);
		pk_listaller_find_applications (plugin, values);
		goto out;
	}

	//TODO: PK_ROLE_ENUM_GET_PACKAGES
	//TODO: PK_ROLE_ENUM_RESOLVE

	if (role == PK_ROLE_ENUM_GET_DETAILS) {
		package_ids = pk_transaction_get_package_ids (transaction);
		data = pk_listaller_filter_listaller_packages (transaction,
							       package_ids);
		if (data != NULL)
			pk_listaller_get_details (plugin, data);

		/* nothing more to process */
		package_ids = pk_transaction_get_package_ids (transaction);
		if (g_strv_length (package_ids) == 0)
			pk_listaller_native_backend_skip (plugin);
		goto out;
	}

	if (role == PK_ROLE_ENUM_SIMULATE_REMOVE_PACKAGES ||
	    role == PK_ROLE_ENUM_SIMULATE_INSTALL_PACKAGES) {

		/* ignore the return value, we can't sensibly do anything */
		package_ids = pk_transaction_get_package_ids (transaction);
		data = pk_listaller_filter_listaller_packages (transaction,
							       package_ids);

		/* nothing more to process */
		package_ids = pk_transaction_get_package_ids (transaction);
		if (g_strv_length (package_ids) == 0)
			pk_listaller_native_backend_skip (plugin);
		goto out;
	}

	if (role == PK_ROLE_ENUM_SIMULATE_INSTALL_FILES) {
		full_paths = pk_transaction_get_full_paths (transaction);
		data = pk_listaller_filter_listaller_files (transaction,
							    full_paths);

		/* We have Listaller packages, so skip this! */
		/* FIXME: This needs to be smarter - backend needs to Simulate() with remaining pkgs */
		if (data != NULL)
			pk_listaller_native_backend_skip (plugin);
		goto out;
	}

	if (role == PK_ROLE_ENUM_INSTALL_FILES) {
		full_paths = pk_transaction_get_full_paths (transaction);
		data = pk_listaller_filter_listaller_files (transaction,
							    full_paths);

		if (data != NULL) {
			pk_listaller_install_files (plugin, data);
		}

		/* nothing more to process */
		full_paths = pk_transaction_get_full_paths (transaction);
		if (g_strv_length (full_paths) == 0)
			pk_listaller_native_backend_skip (plugin);
		goto out;
	}
	if (role == PK_ROLE_ENUM_REMOVE_PACKAGES) {
		package_ids = pk_transaction_get_package_ids (transaction);
		data = pk_listaller_filter_listaller_packages (transaction,
							       package_ids);

		if (data != NULL)
			pk_listaller_remove_applications (plugin, data);

		/* nothing more to process */
		package_ids = pk_transaction_get_package_ids (transaction);
		if (g_strv_length (package_ids) == 0)
			pk_listaller_native_backend_skip (plugin);
		goto out;
	}

out:
	g_strfreev (data);
}

/**
 * pk_plugin_transaction_finished_end:
 */
void
pk_plugin_transaction_finished_end (PkPlugin *plugin,
				    PkTransaction *transaction)
{
	PkRoleEnum role;

	/* update application databases */
	role = pk_transaction_get_role (transaction);
	if (role == PK_ROLE_ENUM_INSTALL_FILES ||
	    role == PK_ROLE_ENUM_REMOVE_PACKAGES ||
	    role == PK_ROLE_ENUM_REFRESH_CACHE) {
		pk_listaller_scan_applications (plugin);
	}
}

/**
 * pk_plugin_get_description:
 */
const gchar *
pk_plugin_get_description (void)
{
	// TODO: Think of a better description
	return "Listaller support for PackageKit";
}

PkResults *
pkbackend_request_whatprovides (PkBitfield filters, PkProvidesEnum provides, gchar** search, PkPlugin *plugin)
{
	/* query the native backend for a package provinding X */
	pk_backend_what_provides (plugin->backend, filters, provides, search);
	return NULL;
}

/**
 * pk_plugin_initialize:
 */
void
pk_plugin_initialize (PkPlugin *plugin)
{
	/* create private area */
	plugin->priv = PK_TRANSACTION_PLUGIN_GET_PRIVATE (PkPluginPrivate);
	plugin->priv->conf = listaller_settings_new (TRUE);
	plugin->priv->mgr = listaller_manager_new (plugin->priv->conf);

	/* tell PK we might be able to handle these */
	pk_backend_implement (plugin->backend, PK_ROLE_ENUM_GET_DETAILS);
	pk_backend_implement (plugin->backend, PK_ROLE_ENUM_INSTALL_FILES);
	pk_backend_implement (plugin->backend, PK_ROLE_ENUM_SIMULATE_INSTALL_FILES);
	pk_backend_implement (plugin->backend, PK_ROLE_ENUM_REMOVE_PACKAGES);

	g_signal_connect (plugin->priv->mgr, "error-code", (GCallback) listaller_error_code_cb, plugin);
	g_signal_connect (plugin->priv->mgr, "message", (GCallback) listaller_message_cb, plugin);
	g_signal_connect (plugin->priv->mgr, "status-changed", (GCallback) listaller_status_change_cb, plugin);
	g_signal_connect (plugin->priv->mgr, "progress-changed", (GCallback) listaller_progress_change_cb, plugin);
	g_signal_connect (plugin->priv->mgr, "application", (GCallback) listaller_application_cb, plugin);

	/* create a backend proxy and connect it, so Listaller can acces parts of PkBackend */
	plugin->priv->pkbproxy = listaller_pk_backend_proxy_new ();

	g_signal_connect (plugin->priv->pkbproxy, "request-whatprovides", (GCallback) pkbackend_request_whatprovides, plugin);

	listaller_set_backend_proxy (plugin->priv->pkbproxy);
}

/**
 * pk_plugin_destroy:
 */
void
pk_plugin_destroy (PkPlugin *plugin)
{
	listaller_set_backend_proxy (NULL);
	g_object_unref (plugin->priv->pkbproxy);
	g_object_unref (plugin->priv->conf);
	g_object_unref (plugin->priv->mgr);
}

/**
 * pk_plugin_transaction_content_types:
 */
void
pk_plugin_transaction_content_types (PkPlugin *plugin,
				     PkTransaction *transaction)
{
	pk_transaction_add_supported_content_type (transaction,
						   "application/x-installation");
}
