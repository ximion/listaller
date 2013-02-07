/* -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*-
 *
 * Copyright (C) 2010-2013 Matthias Klumpp <matthias@tenstral.net>
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
	ListallerSetupSettings	*setup_settings;
	PkResults		*backend_results;
	GMainLoop		*loop;
};

static void pk_plugin_restore_backend (PkPlugin *plugin);
static void pk_plugin_redirect_backend_signals (PkPlugin *plugin);

/**
 * pk_plugin_reset:
 */
void
pk_plugin_reset (PkPlugin *plugin)
{
	/* reset the native backend */
	pk_plugin_restore_backend (plugin);

	/* recreate PkResults object */
	g_object_unref (plugin->priv->backend_results);
	plugin->priv->backend_results = pk_results_new ();

	/* reset the job */
	pk_backend_reset_job (plugin->backend, plugin->job);
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
	exit = pk_backend_job_get_exit_code (plugin->job);

	/* only skip transaction if we don't have an error already */
	if (!pk_backend_job_get_is_error_set (plugin->job)) {
		pk_backend_job_set_exit_code (plugin->job, PK_EXIT_ENUM_SKIP_TRANSACTION);
	}
}

/**
 * pk_plugin_listaller_refresh_repos:
 *
 * Updates the current application caches
 *
 * Return value: True if successful
 */
gboolean
pk_plugin_listaller_refresh_repos (PkPlugin *plugin)
{
	gboolean res;
	ListallerManager *limgr;

	/* create new Listaller task */
	limgr = listaller_manager_new (TRUE);

	/* run it */
	res = listaller_manager_refresh_repository_cache (limgr);

	g_object_unref (limgr);

	/* print warning if refresh fails */
	if (!res) {
		g_warning ("listaller: unable to update application data from repositories.");
	} else {
		g_debug ("listaller: application cache update finished.");
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
						       "*",
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
	pk_backend_job_reset (plugin->job);

	for (i=0; package_ids[i] != NULL; i++) {
		app = pk_listaller_appitem_from_pkid (package_ids[i]);

		/* update AppItem with database data */
		listaller_manager_refresh_appitem (plugin->priv->mgr, &app);

		description = listaller_app_item_get_description (app);
		listaller_app_item_get_license (app, &license);
		url = listaller_app_item_get_website (app);

		//TODO: Fetch size of installed application too

		/* emit */
		pk_backend_job_details (plugin->job, package_ids[i],
					license.name,
					PK_GROUP_ENUM_UNKNOWN,
					description,
					url,
					0);
	};
}

/**
 * pk_listaller_get_filelist:
 */
void
pk_listaller_get_filelist (PkPlugin *plugin, gchar **package_ids)
{
	const gchar *filelist;
	ListallerAppItem *app;
	guint i;

	g_debug ("listaller: running get_filelist ()");
	pk_backend_job_reset (plugin->job);

	for (i=0; package_ids[i] != NULL; i++) {
		app = pk_listaller_appitem_from_pkid (package_ids[i]);

		/* update AppItem with database data */
		listaller_manager_refresh_appitem (plugin->priv->mgr, &app);

		filelist = listaller_manager_get_application_filelist_as_string (plugin->priv->mgr, app);
		if (filelist == NULL)
			filelist = "ERROR while fetching list of files. (Please report this issue)";

		/* emit */
		pk_backend_job_files (plugin->job, package_ids[i], filelist);
	};
}

static void
listaller_application_cb (GObject *sender, ListallerAppItem *item, PkPlugin *plugin)
{
	gchar *package_id;

	package_id = pk_listaller_pkid_from_appitem (item);
	if (package_id == NULL) {
		g_debug ("listaller: <error> generated PK package-id was NULL, ignoring entry.");
		return;
	}
	g_debug ("listaller: new app found -> %s", listaller_app_item_get_appid (item));

	pk_plugin_restore_backend (plugin);

	/* emit */
	pk_backend_job_package (plugin->job, PK_INFO_ENUM_INSTALLED, package_id,
			    listaller_app_item_get_summary (item));

	pk_plugin_redirect_backend_signals (plugin);

	g_free (package_id);
}

static void
listaller_error_code_cb (GObject *sender, ListallerErrorItem *error, PkPlugin *plugin)
{
	g_return_if_fail (error != NULL);

	/* don't try to set errors twice */
	if (pk_backend_job_get_is_error_set (plugin->job))
		return;

	pk_plugin_restore_backend (plugin);

	/* emit */
	pk_backend_job_error_code (plugin->job, PK_ERROR_ENUM_INTERNAL_ERROR,
				listaller_error_item_get_details (error));
}

static void
listaller_message_cb (GObject *sender, ListallerMessageItem *message, PkPlugin *plugin)
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


static void
listaller_progress_cb (GObject *sender, ListallerProgressItem *item, PkPlugin *plugin)
{
	gint value;
	value = listaller_progress_item_get_value (item);

	if (listaller_progress_item_get_prog_type (item) != LISTALLER_PROGRESS_ENUM_MAIN_PROGRESS)
		return;

	pk_plugin_restore_backend (plugin);

	/* emit */
	if (value > 0)
		pk_backend_job_set_percentage (plugin->job, value);

	pk_plugin_redirect_backend_signals (plugin);
}

static void
listaller_status_change_cb (GObject *sender, ListallerStatusItem *status, PkPlugin *plugin)
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

	pk_plugin_restore_backend (plugin);

	/* emit */
	if (pkstatus != PK_STATUS_ENUM_UNKNOWN)
		pk_backend_job_set_status (plugin->job, pkstatus);

	pk_plugin_redirect_backend_signals (plugin);
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

	setup = listaller_setup_new (filename);
	g_signal_connect (setup, "error-code",
			  G_CALLBACK (listaller_error_code_cb), plugin);
	g_signal_connect (setup, "message",
			  G_CALLBACK (listaller_message_cb), plugin);
	g_signal_connect (setup, "status-changed",
			  G_CALLBACK (listaller_status_change_cb), plugin);
	g_signal_connect (setup, "progress",
			  G_CALLBACK (listaller_progress_cb), plugin);

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
	} else if (!pk_backend_job_get_is_error_set (plugin->job)) {
		/* emit */
		pk_backend_job_package (plugin->job, PK_INFO_ENUM_INSTALLED,
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
 * pk_plugin_backend_job_package_cb:
 **/
static void
pk_plugin_backend_job_package_cb (PkBackendJob *job,
		      PkPackage *package,
		      PkPlugin *plugin)
{
	pk_results_add_package (plugin->priv->backend_results, package);
}

/**
 * pk_plugin_backend_job_finished_cb:
 **/
static void
pk_plugin_backend_job_finished_cb (PkBackendJob *job,
		       PkExitEnum exit_enum,
		       PkPlugin *plugin)
{
	if (!g_main_loop_is_running (plugin->priv->loop))
		return;
	if (exit_enum != PK_EXIT_ENUM_SUCCESS) {
		g_warning ("%s failed with exit code: %s",
			   pk_role_enum_to_string (pk_backend_job_get_role (job)),
			   pk_exit_enum_to_string (exit_enum));
	}

	/* save exit code */
	pk_results_set_exit_code (plugin->priv->backend_results, exit_enum);

	/* quit the loop */
	g_main_loop_quit (plugin->priv->loop);
}

/**
 * pk_plugin_backend_job_error_code_cb:
 **/
static void
pk_plugin_backend_job_error_code_cb (PkBackendJob *job,
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
 * pk_plugin_redirect_backend_signals:
 *
 **/
static void
pk_plugin_redirect_backend_signals (PkPlugin *plugin)
{
	/* connect (used) backend signals to Listaller PkPlugin */
	pk_backend_job_set_vfunc (plugin->job,
				  PK_BACKEND_SIGNAL_FINISHED,
				  (PkBackendJobVFunc) pk_plugin_backend_job_finished_cb,
				  plugin);
	pk_backend_job_set_vfunc (plugin->job,
				  PK_BACKEND_SIGNAL_PACKAGE,
				  (PkBackendJobVFunc) pk_plugin_backend_job_package_cb,
				  plugin);
	pk_backend_job_set_vfunc (plugin->job,
				  PK_BACKEND_SIGNAL_ERROR_CODE,
				  (PkBackendJobVFunc) pk_plugin_backend_job_error_code_cb,
				  plugin);
}

/**
 * pk_plugin_restore_backend:
 *
 **/
static void
pk_plugin_restore_backend (PkPlugin *plugin)
{
	pk_backend_job_reset (plugin->job);
	/* connect backend-job to current backend again */
	pk_transaction_signals_reset (plugin->priv->current_transaction,
				     plugin->job);
}

/**
 * pk_backend_job_request_whatprovides_cb:
 *
 * Helper method for a Listaller native PkBackend proxy to do a what-provides request.
 **/
static PkResults*
pk_backend_job_request_whatprovides_cb (PkBitfield filters,
				PkProvidesEnum provides,
				gchar** search,
				PkPlugin *plugin)
{
	PkResults *results;

	/* query the native backend for a package provinding X */
	if (plugin == NULL)
		g_debug ("<liplugin-dbg> PLUGIN was NULL!");

	g_debug ("Running what-provides on native backend!");

	/* prepare for native backend call */
	pk_plugin_reset (plugin);
	pk_plugin_redirect_backend_signals (plugin);

	/* query the native backend */
	pk_backend_what_provides (plugin->backend,
				   plugin->job,
				   filters,
				   provides,
				   search);

	/* wait for finished */
	g_main_loop_run (plugin->priv->loop);

	results = plugin->priv->backend_results;
	pk_plugin_restore_backend (plugin);

	g_debug ("Results exit code is %s", pk_exit_enum_to_string (pk_results_get_exit_code (results)));

	return results;
}

/**
 * pk_backend_job_request_installpackages_cb:
 *
 * Helper method for a Listaller native PkBackend proxy to do a install-packages request.
 **/
static PkResults*
pk_backend_job_request_installpackages_cb (PkBitfield transaction_flags,
					gchar **package_ids,
					PkPlugin *plugin)
{
	PkResults *results;

	g_debug ("Running install-packages on native backend!");

	/* prepare the backend */
	pk_plugin_reset (plugin);
	pk_plugin_redirect_backend_signals (plugin);

	/* query the native backend */
	pk_backend_install_packages (plugin->backend,
				      plugin->job,
				      transaction_flags,
				      package_ids);

	/* wait for finished */
	g_main_loop_run (plugin->priv->loop);

	results = plugin->priv->backend_results;
	pk_plugin_restore_backend (plugin);

	g_debug ("Results exit code is %s", pk_exit_enum_to_string (pk_results_get_exit_code (results)));

	return results;
}

/**
 * pk_plugin_transaction_started:
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

	/* reset the native-backend job */
	pk_backend_job_reset (plugin->job);
	pk_backend_job_set_status (plugin->job, PK_STATUS_ENUM_SETUP);

	/* set the transaction */
	plugin->priv->current_transaction = transaction;

	/* create a backend proxy and connect it, so Listaller can acces parts of PkBackend */
	pkbproxy = listaller_pk_backend_proxy_new ();
	listaller_pk_backend_proxy_set_what_provides (pkbproxy,
						(ListallerPkBackendProxyWhatProvidesCB) pk_backend_job_request_whatprovides_cb,
						plugin);
	listaller_pk_backend_proxy_set_install_packages (pkbproxy,
						(ListallerPkBackendProxyInstallPackagesCB) pk_backend_job_request_installpackages_cb,
						plugin);
	listaller_set_backend_proxy (pkbproxy);

	/* get transaction role */
	role = pk_transaction_get_role (transaction);

	/* if we're only simulation, skip Listaller packages */
	if (pk_bitfield_contain (pk_transaction_get_transaction_flags (transaction),
				   PK_TRANSACTION_FLAG_ENUM_SIMULATE)) {

		if (role == PK_ROLE_ENUM_INSTALL_FILES) {
			full_paths = pk_transaction_get_full_paths (transaction);
			data = pk_transaction_filter_listaller_files (transaction,
									full_paths);

			/* We have Listaller packages, so skip this! */
			/* FIXME: This needs to be smarter - backend needs to Simulate() with remaining pkgs */
			if (data != NULL)
				pk_plugin_skip_native_backend (plugin);
		} else {
			/* ignore the return value, we can't sensibly do anything */
			package_ids = pk_transaction_get_package_ids (transaction);
			data = pk_transaction_filter_listaller_packages (transaction,
								package_ids);

			/* nothing more to process */
			if (data != 0)
				pk_plugin_skip_native_backend (plugin);
		}

		goto out;
	}

	g_debug ("Processing transaction");

	/* handle these before the transaction has been run */
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

	if (role == PK_ROLE_ENUM_GET_FILES) {
		package_ids = pk_transaction_get_package_ids (transaction);
		data = pk_transaction_filter_listaller_packages (transaction,
							       package_ids);
		if (data != NULL)
			pk_listaller_get_filelist (plugin, data);

		/* nothing more to process */
		package_ids = pk_transaction_get_package_ids (transaction);
		if (g_strv_length (package_ids) == 0)
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
 * pk_plugin_transaction_finished_results:
 *
 * Hook for the end (results) of a PkTransaction
 */
void
pk_plugin_transaction_finished_results (PkPlugin *plugin,
				    PkTransaction *transaction)
{
	PkRoleEnum role;

	if (pk_backend_job_get_exit_code (plugin->job) == PK_EXIT_ENUM_CANCELLED) {
		g_debug ("skipping finished_results() because transaction was cancelled");
		return;
	}

	/* skip simulate actions */
	if (pk_bitfield_contain (pk_transaction_get_transaction_flags (transaction),
				 PK_TRANSACTION_FLAG_ENUM_SIMULATE)) {
		return;
	}

	/* skip only-download */
	if (pk_bitfield_contain (pk_transaction_get_transaction_flags (transaction),
				 PK_TRANSACTION_FLAG_ENUM_ONLY_DOWNLOAD)) {
		return;
	}

	/* update application databases */
	role = pk_transaction_get_role (transaction);
	if (role == PK_ROLE_ENUM_REFRESH_CACHE) {
		pk_plugin_listaller_refresh_repos (plugin);
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
	plugin->priv->mgr = listaller_manager_new (TRUE);
	plugin->priv->loop = g_main_loop_new (NULL, FALSE);
	plugin->priv->backend_results = pk_results_new ();
	plugin->priv->setup_settings = NULL;

	/* tell PK we might be able to handle these */
	pk_backend_implement (plugin->backend, PK_ROLE_ENUM_GET_DETAILS);
	pk_backend_implement (plugin->backend, PK_ROLE_ENUM_GET_FILES);
	pk_backend_implement (plugin->backend, PK_ROLE_ENUM_INSTALL_FILES);
	pk_backend_implement (plugin->backend, PK_ROLE_ENUM_REMOVE_PACKAGES);

	/* connect Listaller manager signals */
	g_signal_connect (plugin->priv->mgr, "error-code", G_CALLBACK (listaller_error_code_cb), plugin);
	g_signal_connect (plugin->priv->mgr, "message", G_CALLBACK (listaller_message_cb), plugin);
	g_signal_connect (plugin->priv->mgr, "status-changed", G_CALLBACK (listaller_status_change_cb), plugin);
	g_signal_connect (plugin->priv->mgr, "progress", G_CALLBACK (listaller_progress_cb), plugin);
	g_signal_connect (plugin->priv->mgr, "application", G_CALLBACK (listaller_application_cb), plugin);

	/* We want verbose mode! */
	listaller_set_verbose_mode (TRUE);
	listaller_add_log_domain ("PkListaller");
}

/**
 * pk_plugin_destroy:
 */
void
pk_plugin_destroy (PkPlugin *plugin)
{
	g_main_loop_unref (plugin->priv->loop);
	if (plugin->priv->setup_settings != NULL)
		g_object_unref (plugin->priv->setup_settings);
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
						   "application/x-app-package");
}
