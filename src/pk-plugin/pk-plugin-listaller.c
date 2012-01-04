/* -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*-
 *
 * Copyright (C) 2010-2012 Matthias Klumpp <matthias@tenstral.net>
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
#include <glib.h>
#include <packagekit-glib2/packagekit.h>
#include <plugin/packagekit-plugin.h>

#include "listaller_internal.h"
#include "li-plugin-utils.h"

struct PkPluginPrivate {
	PkTransaction		*current_transaction;
	ListallerManager	*mgr;
	ListallerSettings	*conf;
	PkResults		*backend_results;
	GMainLoop		*loop;
};

typedef struct {
	guint finished_id;
	guint package_id;
	guint error_code_id;
} PkPluginSignalData;

/**
 * pk_plugin_reset:
 */
void
pk_plugin_reset (PkPlugin *plugin)
{
	/* reset the native backend */
	pk_backend_reset (plugin->backend);

	/* recreate PkResults object */
	g_object_unref (plugin->priv->backend_results);
	plugin->priv->backend_results = pk_results_new ();
}

/**
 * pk_plugin_skip_native_backend:
 *
 * Tell PackageKit to skip execution of native backend.
 */
static void
pk_plugin_skip_native_backend (PkPlugin *plugin)
{
	PkExitEnum exit;
	exit = pk_backend_get_exit_code (plugin->backend);

	/* only skip transaction if we don't have an error already */
	if (!pk_backend_get_is_error_set (plugin->backend)) {
		pk_backend_set_exit_code (plugin->backend, PK_EXIT_ENUM_SKIP_TRANSACTION);
	}
}

/**
 * pk_plugin_listaller_scan_applications:
 *
 * Updates the current application database
 *
 * Return value: True if successful
 */
gboolean
pk_plugin_listaller_scan_applications (PkPlugin *plugin)
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
	ListallerAppLicense license;
	const gchar *url;
	ListallerAppItem *app;
	guint i;

	g_debug ("listaller: running get_details ()");
	pk_backend_reset (plugin->backend);

	for (i=0; package_ids[i] != NULL; i++) {
		app = pk_listaller_appitem_from_pkid (package_ids[i]);

		description = listaller_app_item_get_description (app);
		listaller_app_item_get_license (app, &license);
		url = listaller_app_item_get_website (app);

		/* emit */
		pk_backend_details (plugin->backend, package_ids[i],
					license.name,
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


static void listaller_progress_change_cb (GObject *sender, gint progress, gint subprogress, PkPlugin *plugin)
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
 * pk_plugin_backend_package_cb:
 **/
static void
pk_plugin_backend_package_cb (PkBackend *backend,
		      PkPackage *package,
		      PkPlugin *plugin)
{
	pk_results_add_package (plugin->priv->backend_results, package);
}

/**
 * pk_plugin_backend_finished_cb:
 **/
static void
pk_plugin_backend_finished_cb (PkBackend *backend,
		       PkExitEnum exit_enum,
		       PkPlugin *plugin)
{
	if (!g_main_loop_is_running (plugin->priv->loop))
		return;
	if (exit_enum != PK_EXIT_ENUM_SUCCESS) {
		g_warning ("%s failed with exit code: %s",
			   pk_role_enum_to_string (pk_backend_get_role (backend)),
			   pk_exit_enum_to_string (exit_enum));
	}

	/* save exit code */
	pk_results_set_exit_code (plugin->priv->backend_results, exit_enum);

	/* quit the loop */
	g_main_loop_quit (plugin->priv->loop);
}

/**
 * pk_plugin_backend_error_code_cb:
 **/
static void
pk_plugin_backend_error_code_cb (PkBackend *backend,
				PkError *item,
				PkPlugin *plugin)
{
	gchar *details;
	PkErrorEnum code;

	/* get data */
	g_object_get (item,
		      "code", &code,
		      "details", &details,
		      NULL);

	/* add to results */
	pk_results_set_error_code (plugin->priv->backend_results, item);
	g_debug ("Native backend failed with detail error message: %s", details);

	g_free (details);
}

/**
 * pk_plugin_prepare_backend_call:
 *
 **/
static PkPluginSignalData*
pk_plugin_prepare_backend_call (PkPlugin *plugin)
{
	PkPluginSignalData *siginfo;
	PkBitfield	    backend_signals;

	siginfo = g_slice_new (PkPluginSignalData);

	/* connect backend signals to Listaller PkPlugin */
	siginfo->finished_id = g_signal_connect (plugin->backend, "finished",
					G_CALLBACK (pk_plugin_backend_finished_cb), plugin);
	siginfo->package_id = g_signal_connect (plugin->backend, "package",
				       G_CALLBACK (pk_plugin_backend_package_cb), plugin);
	siginfo->error_code_id = g_signal_connect (plugin->backend, "error-code",
				       G_CALLBACK (pk_plugin_backend_error_code_cb), plugin);

	/* don't forward some events to the transaction, only Listaller should see them */
	backend_signals = PK_TRANSACTION_ALL_BACKEND_SIGNALS;
	pk_bitfield_remove (backend_signals, PK_BACKEND_SIGNAL_ERROR_CODE);
	pk_bitfield_remove (backend_signals, PK_BACKEND_SIGNAL_PACKAGE);
	pk_bitfield_remove (backend_signals, PK_BACKEND_SIGNAL_FINISHED);
	pk_transaction_set_signals (plugin->priv->current_transaction, backend_signals);

	return siginfo;
}

/**
 * pk_plugin_restore_backend:
 *
 **/
static void
pk_plugin_restore_backend (PkPlugin *plugin, PkPluginSignalData *siginfo)
{
	/* disconnect the native backend */
	if (siginfo->package_id > 0)
		g_signal_handler_disconnect (plugin->backend, siginfo->package_id);
	if (siginfo->error_code_id > 0)
		g_signal_handler_disconnect (plugin->backend, siginfo->error_code_id);
	if (siginfo->finished_id > 0)
		g_signal_handler_disconnect (plugin->backend, siginfo->finished_id);

	/* connect backend again */
	pk_transaction_connect_backend_signals (plugin->priv->current_transaction, PK_TRANSACTION_ALL_BACKEND_SIGNALS);
	pk_backend_reset (plugin->backend);

	g_slice_free (PkPluginSignalData, siginfo);
}

/**
 * pk_backend_request_whatprovides_cb:
 *
 * Helper method for a Listaller native PkBackend proxy to do a what-provides request.
 **/
static PkResults*
pk_backend_request_whatprovides_cb (PkBitfield filters,
				PkProvidesEnum provides,
				gchar** search,
				PkPlugin *plugin)
{
	PkResults *results;
	PkPluginSignalData *backend_siginfo;

	/* query the native backend for a package provinding X */
	if (plugin == NULL)
		g_debug ("<liplugin-dbg> PLUGIN was NULL!");

	g_debug ("Running what-provides on native backend!");

	/* prepare for native backend call */
	pk_plugin_reset (plugin);
	backend_siginfo = pk_plugin_prepare_backend_call (plugin);

	/* query the native backend */
	pk_backend_what_provides (plugin->backend, filters, provides, search);

	/* wait for finished */
	g_main_loop_run (plugin->priv->loop);

	results = plugin->priv->backend_results;
	pk_plugin_restore_backend (plugin, backend_siginfo);

	g_debug ("Results exit code is %s", pk_exit_enum_to_string (pk_results_get_exit_code (results)));

	return results;
}

/**
 * pk_backend_request_installpackages_cb:
 *
 * Helper method for a Listaller native PkBackend proxy to do a install-packages request.
 **/
static PkResults*
pk_backend_request_installpackages_cb (gboolean only_trusted,
					gchar **package_ids,
					PkPlugin *plugin)
{
	PkResults *results;
	PkPluginSignalData *backend_siginfo;

	g_debug ("Running install-packages on native backend!");

	/* prepare the backend */
	pk_plugin_reset (plugin);
	backend_siginfo = pk_plugin_prepare_backend_call (plugin);

	/* query the native backend */
	pk_backend_install_packages (plugin->backend, only_trusted, package_ids);

	/* wait for finished */
	g_main_loop_run (plugin->priv->loop);

	results = plugin->priv->backend_results;
	pk_plugin_restore_backend (plugin, backend_siginfo);

	g_debug ("Results exit code is %s", pk_exit_enum_to_string (pk_results_get_exit_code (results)));

	return results;
}

/**
 * pk_plugin_started:
 *
 * Hook for the beginning of a new PkTransaction (where it is not completely set-up)
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

	ListallerPkBackendProxy *pkbproxy;

	/* reset the native-backend */
	pk_backend_reset (plugin->backend);
	pk_backend_set_status (plugin->backend, PK_STATUS_ENUM_SETUP);

	/* set the transaction */
	plugin->priv->current_transaction = transaction;

	/* create a backend proxy and connect it, so Listaller can acces parts of PkBackend */
	pkbproxy = listaller_pk_backend_proxy_new ();
	listaller_pk_backend_proxy_set_what_provides (pkbproxy,
						      (ListallerPkBackendProxyWhatProvidesCB) pk_backend_request_whatprovides_cb,
						      plugin);
	listaller_pk_backend_proxy_set_install_packages (pkbproxy,
							 (ListallerPkBackendProxyInstallPackagesCB) pk_backend_request_installpackages_cb,
							 plugin);
	listaller_set_backend_proxy (pkbproxy);

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
		data = pk_transaction_filter_listaller_packages (transaction,
							       package_ids);
		if (data != NULL)
			pk_listaller_get_details (plugin, data);

		/* nothing more to process */
		package_ids = pk_transaction_get_package_ids (transaction);
		if (g_strv_length (package_ids) == 0)
			pk_plugin_skip_native_backend (plugin);
		goto out;
	}

	if (role == PK_ROLE_ENUM_SIMULATE_REMOVE_PACKAGES ||
	    role == PK_ROLE_ENUM_SIMULATE_INSTALL_PACKAGES) {

		/* ignore the return value, we can't sensibly do anything */
		package_ids = pk_transaction_get_package_ids (transaction);
		data = pk_transaction_filter_listaller_packages (transaction,
							       package_ids);

		/* nothing more to process */
		package_ids = pk_transaction_get_package_ids (transaction);
		if (g_strv_length (package_ids) == 0)
			pk_plugin_skip_native_backend (plugin);
		goto out;
	}

	if (role == PK_ROLE_ENUM_SIMULATE_INSTALL_FILES) {
		full_paths = pk_transaction_get_full_paths (transaction);
		data = pk_transaction_filter_listaller_files (transaction,
							    full_paths);

		/* We have Listaller packages, so skip this! */
		/* FIXME: This needs to be smarter - backend needs to Simulate() with remaining pkgs */
		if (data != NULL)
			pk_plugin_skip_native_backend (plugin);
		goto out;
	}

	if (role == PK_ROLE_ENUM_INSTALL_FILES) {
		full_paths = pk_transaction_get_full_paths (transaction);
		data = pk_transaction_filter_listaller_files (transaction,
							    full_paths);

		if (data != NULL) {
			pk_listaller_install_files (plugin, data);
		}

		/* nothing more to process */
		full_paths = pk_transaction_get_full_paths (transaction);
		if (g_strv_length (full_paths) == 0)
			pk_plugin_skip_native_backend (plugin);
		goto out;
	}
	if (role == PK_ROLE_ENUM_REMOVE_PACKAGES) {
		package_ids = pk_transaction_get_package_ids (transaction);
		data = pk_transaction_filter_listaller_packages (transaction,
							       package_ids);

		if (data != NULL)
			pk_listaller_remove_applications (plugin, data);

		/* nothing more to process */
		package_ids = pk_transaction_get_package_ids (transaction);
		if (g_strv_length (package_ids) == 0)
			pk_plugin_skip_native_backend (plugin);
		goto out;
	}

out:
	g_strfreev (data);

	/* remove the backend proxy */
	listaller_set_backend_proxy (NULL);
	g_object_unref (pkbproxy);
	plugin->priv->current_transaction = NULL;
}

/**
 * pk_plugin_transaction_finished_end:
 *
 * Hook for the end of a PkTransaction
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
		pk_plugin_listaller_scan_applications (plugin);
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
	plugin->priv->loop = g_main_loop_new (NULL, FALSE);
	plugin->priv->backend_results = pk_results_new ();

	/* tell PK we might be able to handle these */
	pk_backend_implement (plugin->backend, PK_ROLE_ENUM_GET_DETAILS);
	pk_backend_implement (plugin->backend, PK_ROLE_ENUM_INSTALL_FILES);
	pk_backend_implement (plugin->backend, PK_ROLE_ENUM_SIMULATE_INSTALL_FILES);
	pk_backend_implement (plugin->backend, PK_ROLE_ENUM_REMOVE_PACKAGES);

	/* connect Listaller manager signals */
	g_signal_connect (plugin->priv->mgr, "error-code", G_CALLBACK (listaller_error_code_cb), plugin);
	g_signal_connect (plugin->priv->mgr, "message", G_CALLBACK (listaller_message_cb), plugin);
	g_signal_connect (plugin->priv->mgr, "status-changed", G_CALLBACK (listaller_status_change_cb), plugin);
	g_signal_connect (plugin->priv->mgr, "progress-changed", G_CALLBACK (listaller_progress_change_cb), plugin);
	g_signal_connect (plugin->priv->mgr, "application", G_CALLBACK (listaller_application_cb), plugin);
}

/**
 * pk_plugin_destroy:
 */
void
pk_plugin_destroy (PkPlugin *plugin)
{
	g_main_loop_unref (plugin->priv->loop);
	g_object_unref (plugin->priv->conf);
	g_object_unref (plugin->priv->mgr);
	g_object_unref (plugin->priv->backend_results);
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
