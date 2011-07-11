/* -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*-
 *
 * Copyright (C) 2010-2011 Matthias Klumpp <matthias@nlinux.org>
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

#ifdef HAVE_CONFIG_H
#  include <config.h>
#endif
#define PK_COMPILATION

#include <glib/gi18n.h>
#include <glib.h>
#include <gmodule.h>

#include <plugin/pk-backend.h>
#include <packagekit-glib2/packagekit.h>
#include "listaller.h"
#include "pk-listaller.h"

#define PK_LISTALLER_GET_PRIVATE(o) (G_TYPE_INSTANCE_GET_PRIVATE ((o), PK_TYPE_LISTALLER, PkListallerPrivate))

static void     pk_listaller_finalize		(GObject	    *object);

struct PkListallerPrivate
{
	ListallerManager	*mgr;
	ListallerSettings	*conf;
	PkResults		*results;
	PkLiStatus		 status;
};

enum {
	SIGNAL_STATUS_CHANGED,
	SIGNAL_PROGRESS_CHANGED,
	SIGNAL_APPLICATION,
	SIGNAL_DETAILS,
	SIGNAL_MESSAGE,
	SIGNAL_ERROR_CODE,
	SIGNAL_FINISHED,
	SIGNAL_LAST
};

static guint signals[SIGNAL_LAST] = { 0 };

G_DEFINE_TYPE (PkListaller, pk_listaller, G_TYPE_OBJECT)
static gpointer pk_listaller_object = NULL;

/**
 * pk_listaller_reset:
 */
void
pk_listaller_reset (PkListaller *pkli)
{
	g_return_if_fail (PK_IS_LISTALLER (pkli));
	/* unref then create rather then set zero size, as another object
	 * might have a reference on the data */
	g_object_unref (pkli->priv->results);
	pkli->priv->results = pk_results_new ();

	pkli->priv->status = PK_LISTALLER_STATUS_UNKNOWN;
}

/**
 * pk_listaller_scan_applications:
 *
 * Updates the current application database
 *
 * Return value: True if successful
 */
gboolean
pk_listaller_scan_applications (PkListaller *pkli)
{
	gboolean res;
	g_return_val_if_fail (PK_IS_LISTALLER (pkli), FALSE);

	/* run it */
	res = listaller_manager_scan_applications (pkli->priv->mgr);

	/* print warning if scan fails */
	if (!res) {
		g_warning ("listaller: unable to update application database.");
	} else {
		g_debug ("listaller: application database update finished.");
	}

	if (res)
		pkli->priv->status = PK_LISTALLER_STATUS_FINISHED;
	else
		pkli->priv->status = PK_LISTALLER_STATUS_FAILED;

	return res;
}

/**
 * pk_listaller_find_applications:
 *
 * Find Listaller apps by name
 */
void
pk_listaller_find_applications (PkListaller *pkli, gchar **values)
{
	g_return_if_fail (PK_IS_LISTALLER (pkli));

	g_debug ("listaller: searching for applications.");
	listaller_manager_find_applications_by_values (pkli->priv->mgr, LISTALLER_APP_SOURCE_EXTERN, values, NULL);
}

/**
 * pk_packages_get_listaller_pkg:
 *
 * Get a Listaller fake package from package-id list and delete its entry
 **/
static gchar*
pk_packages_get_listaller_pkg (gchar ***package_ids)
{
	guint i;
	gchar **parts = NULL;
	GPtrArray *pkarray;
	gchar *res = NULL;

	pkarray = g_ptr_array_new_with_free_func (g_free);
	for (i = 0; i < g_strv_length(*package_ids); i++) {
		// FIXME: *package_ids[i] is sometimes invalid - needs further thinking
		parts = pk_package_id_split (*package_ids[i]);
		if (parts == NULL)
			break;
		if ((g_str_has_prefix (parts[3], "local:listaller%") == TRUE) && (res == NULL)) {
			res = g_strdup (*package_ids[i]);
		} else {
			g_ptr_array_add (pkarray, g_strdup (*package_ids[i]));
		}
		g_strfreev (parts);
		if (res != NULL)
			break;
	}

	g_strfreev (*package_ids);
	*package_ids = pk_ptr_array_to_strv (pkarray);
	g_ptr_array_free (pkarray, TRUE);

	return res;
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
 * pk_listaller_delete_app_ids:
 *
 * Remove package-ids which belong to Listaller
 **/
void
pk_listaller_delete_app_ids (PkListaller *pkli, gchar ***package_ids)
{
	gchar *pkid = NULL;

	pkid = pk_packages_get_listaller_pkg (package_ids);
	while (pkid != NULL) {
		g_free (pkid);
		pkid = pk_packages_get_listaller_pkg (package_ids);
	}

	pkli->priv->status = PK_LISTALLER_STATUS_FINISHED;
}

/**
 * pk_listaller_get_status:
 *
 * Get Listaller's current fake-backend status
 */
PkLiStatus
pk_listaller_get_status (PkListaller *pkli)
{
	g_return_val_if_fail (PK_IS_LISTALLER (pkli), FALSE);

	return pkli->priv->status;
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
		return NULL;

	item = listaller_app_item_new_blank ();
	listaller_app_item_set_idname (item, parts[0]);
	listaller_app_item_set_version (item, parts[1]);
	listaller_app_item_set_desktop_file (item, tmp[1]);
	listaller_app_item_set_shared (item, TRUE);

	g_debug ("listaller: <appid> %s %s %s", parts[0], parts[1], tmp[1]);

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
pk_listaller_remove_applications (PkListaller *pkli, gchar ***package_ids)
{
	gchar *pkid = NULL;
	ListallerAppItem *app = NULL;
	PkListallerPrivate *priv = PK_LISTALLER_GET_PRIVATE (pkli);
	g_return_if_fail (PK_IS_LISTALLER (pkli));

	g_debug ("listaller: remove applications");

	pkid = pk_packages_get_listaller_pkg (package_ids);
	while (pkid != NULL) {
		app = pk_listaller_appitem_from_pkid (pkid);
		if (app == NULL)
			continue;

		listaller_manager_remove_application (priv->mgr, app);
		g_free (pkid);
		pkid = pk_packages_get_listaller_pkg (package_ids);
		g_object_unref (app);
	};

	/* Is there something left to do? */
	pkli->priv->status = PK_LISTALLER_STATUS_ENTRIES_LEFT;
	if ((*package_ids == NULL) || (g_strv_length (*package_ids) == 0))
		pkli->priv->status = PK_LISTALLER_STATUS_FINISHED;		
}

/**
 * pk_listaller_get_details:
 */
void
pk_listaller_get_details (PkListaller *pkli, gchar ***package_ids)
{
	gchar *pkid = NULL;
	gchar *description;
	const gchar *license;
	const gchar *url;
	ListallerAppItem *app;
	PkDetails *item;
	g_return_if_fail (PK_IS_LISTALLER (pkli));

	g_debug ("listaller: running get_details ()");
	pk_listaller_reset (pkli);

	pkid = pk_packages_get_listaller_pkg (package_ids);
	while (pkid != NULL) {
		app = pk_listaller_appitem_from_pkid (pkid);

		description = listaller_manager_get_app_description (pkli->priv->mgr, app);
		license = listaller_app_item_get_license_name (app);
		url = listaller_app_item_get_url (app);

		/* form PkDetails struct */
		item = pk_details_new ();
		g_object_set (item,
			"package-id", pkid,
		"license", license,
		"group", PK_GROUP_ENUM_UNKNOWN,
		"description", description,
		"url", url,
		"size", 0,
			NULL);

		/* emit */
		g_signal_emit (pkli, signals[SIGNAL_DETAILS], 0, item);
		pk_results_add_details (pkli->priv->results, item);

		g_free (description);
		g_free (pkid);
		pkid = pk_packages_get_listaller_pkg (package_ids);
	};

	/* Is there something left to do? */
	pkli->priv->status = PK_LISTALLER_STATUS_ENTRIES_LEFT;
	if ((*package_ids == NULL) || (g_strv_length (*package_ids) == 0))
		pkli->priv->status = PK_LISTALLER_STATUS_FINISHED;
}

static void listaller_application_cb (GObject *sender, ListallerAppItem *item, PkListaller *pkli)
{
	gchar *package_id;
	PkPackage *pkg = NULL;

	g_return_if_fail (PK_IS_LISTALLER (pkli));

	package_id = pk_listaller_pkid_from_appitem (item);
	if (package_id == NULL) {
		g_debug ("listaller: <error> generated PK package-id was NULL, ignoring entry.");
		return;
	}
	g_debug ("listaller: new app found -> %s", listaller_app_item_get_appid (item));

	/* create a new package object AFTER we emulate the info value */
	pkg = pk_package_new ();
	g_object_set (pkg,
		      "info", PK_INFO_ENUM_INSTALLED,
	       "summary", listaller_app_item_get_summary (item),
	       NULL);
	pk_package_set_id (pkg, package_id, NULL);

	/* emit */
	g_signal_emit (pkli, signals[SIGNAL_APPLICATION], 0, pkg);

	g_free (package_id);
}

static void listaller_error_code_cb (GObject *sender, ListallerErrorItem *error, PkListaller *pkli)
{
	PkError *item = NULL;

	g_return_if_fail (PK_IS_LISTALLER (pkli));
	g_return_if_fail (error != NULL);

	/* form PkError struct */
	item = pk_error_new ();
	g_object_set (item,
		      "code", PK_ERROR_ENUM_INTERNAL_ERROR,
	       "details", listaller_error_item_get_details (error),
	       NULL);

	/* emit */
	g_signal_emit (pkli, signals[SIGNAL_ERROR_CODE], 0, item);
}



static void listaller_message_cb (GObject *sender, ListallerMessageItem *message, PkListaller *pkli)
{
	ListallerMessageEnum mtype;
	gchar *text;

	g_return_if_fail (PK_IS_LISTALLER (pkli));
	g_return_if_fail (message != NULL);

	/* PackageKit won't forward this messages to the frontend */
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


static void listaller_progress_change_cb (GObject* sender, gint progress, gint subprogress, PkListaller *pkli)
{
	g_return_if_fail (PK_IS_LISTALLER (pkli));

	/* emit */
	g_signal_emit (pkli, signals[SIGNAL_PROGRESS_CHANGED], 0, progress, subprogress, -1, -1);
}

static void listaller_status_change_cb (GObject *sender, ListallerStatusItem *status, PkListaller *pkli)
{
	ListallerStatusEnum listatus;
	PkStatusEnum pkstatus;
	g_return_if_fail (PK_IS_LISTALLER (pkli));
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
		g_signal_emit (pkli, signals[SIGNAL_STATUS_CHANGED], 0, pkstatus);
}

/**
 * pk_listaller_install_file:
 *
 * Install the IPK package in filename
 *
 * Return value: True if successful
 **/
static gboolean
pk_listaller_install_file (PkListaller *pkli, const gchar *filename)
{
	gboolean ret = FALSE;
	ListallerSetup *setup;
	gchar* package_id;
	PkPackage *pkg = NULL;
	ListallerAppItem *app = NULL;

	PkListallerPrivate *priv = PK_LISTALLER_GET_PRIVATE (pkli);
	g_return_val_if_fail (PK_IS_LISTALLER (pkli), FALSE);

	setup = listaller_setup_new (filename, priv->conf);
	g_signal_connect_object (setup, "error-code", (GCallback) listaller_error_code_cb, pkli, 0);
	g_signal_connect_object (setup, "message", (GCallback) listaller_message_cb, pkli, 0);
	g_signal_connect_object (setup, "status-changed", (GCallback) listaller_status_change_cb, pkli, 0);
	g_signal_connect_object (setup, "progress-changed", (GCallback) listaller_progress_change_cb, pkli, 0);

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
	} else {
		/* create a new package object */
		pkg = pk_package_new ();
		g_object_set (pkg,
				"info", PK_INFO_ENUM_INSTALLED,
				"summary", listaller_app_item_get_summary (app),
				NULL);
		pk_package_set_id (pkg, package_id, NULL);

		/* emit */
		g_signal_emit (pkli, signals[SIGNAL_APPLICATION], 0, pkg);
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
pk_listaller_install_files (PkListaller *pkli, gchar ***full_paths)
{
	gboolean ret = FALSE;
	gchar *ipkpath = NULL;
	PkListallerPrivate *priv = PK_LISTALLER_GET_PRIVATE (pkli);
	g_return_if_fail (PK_IS_LISTALLER (pkli));

	ipkpath = pk_packages_get_listaller_file (full_paths);
	if (ipkpath != NULL)  {
		while (ipkpath != NULL) {
			g_debug ("listaller: Current path is: %s", ipkpath);

			ret = pk_listaller_install_file (pkli, ipkpath);
			if (!ret)
				goto out;

			ipkpath = pk_packages_get_listaller_file (full_paths);
		}
		/* as we connot install native and Listaller packages at the same time,we
		 set the "no packages left" mark here */
		priv->status = PK_LISTALLER_STATUS_FINISHED;
	}
out:
	if (!ret)
		priv->status = PK_LISTALLER_STATUS_FAILED;
}

/**
 * pk_listaller_init:
 **/
static void
pk_listaller_init (PkListaller *pkli)
{
	pkli->priv = PK_LISTALLER_GET_PRIVATE (pkli);

	pkli->priv->conf = listaller_settings_new (TRUE);
	pkli->priv->mgr = listaller_manager_new (pkli->priv->conf);

	pkli->priv->results = pk_results_new ();

	pkli->priv->status = PK_LISTALLER_STATUS_UNKNOWN;

	g_signal_connect_object (pkli->priv->mgr, "error-code", (GCallback) listaller_error_code_cb, pkli, 0);
	g_signal_connect_object (pkli->priv->mgr, "message", (GCallback) listaller_message_cb, pkli, 0);
	g_signal_connect_object (pkli->priv->mgr, "status-changed", (GCallback) listaller_status_change_cb, pkli, 0);
	g_signal_connect_object (pkli->priv->mgr, "progress-changed", (GCallback) listaller_progress_change_cb, pkli, 0);
	g_signal_connect_object (pkli->priv->mgr, "application", (GCallback) listaller_application_cb, pkli, 0);
}

/**
 * pk_listaller_finalize:
 **/
static void
pk_listaller_finalize (GObject *object)
{
	PkListaller *pkli;

	g_return_if_fail (PK_IS_LISTALLER (object));

	pkli = PK_LISTALLER (object);

	g_object_unref (pkli->priv->mgr);
	g_object_unref (pkli->priv->conf);
	g_object_unref (pkli->priv->results);

	G_OBJECT_CLASS (pk_listaller_parent_class)->finalize (object);
}

/**
 * pk_listaller_class_init:
 * @klass: The PkListallerClass
 **/
static void
pk_listaller_class_init (PkListallerClass *klass)
{
	GObjectClass *object_class = G_OBJECT_CLASS (klass);
	object_class->finalize = pk_listaller_finalize;

	/* signals */
	signals[SIGNAL_APPLICATION] =
		g_signal_new ("application",
			      G_TYPE_FROM_CLASS (object_class), G_SIGNAL_RUN_LAST,
			      0, NULL, NULL, g_cclosure_marshal_VOID__POINTER,
			      G_TYPE_NONE, 1, G_TYPE_POINTER);
	signals[SIGNAL_DETAILS] =
		g_signal_new ("details",
			      G_TYPE_FROM_CLASS (object_class), G_SIGNAL_RUN_LAST,
			      0, NULL, NULL, g_cclosure_marshal_VOID__POINTER,
			      G_TYPE_NONE, 1, G_TYPE_POINTER);
	signals[SIGNAL_STATUS_CHANGED] =
		g_signal_new ("status-changed",
			      G_TYPE_FROM_CLASS (object_class), G_SIGNAL_RUN_LAST,
			      0, NULL, NULL, g_cclosure_marshal_VOID__UINT,
			      G_TYPE_NONE, 1, G_TYPE_UINT);
	signals[SIGNAL_PROGRESS_CHANGED] =
		g_signal_new ("progress-changed",
			      G_TYPE_FROM_CLASS (object_class), G_SIGNAL_RUN_LAST,
			      0, NULL, NULL, pk_marshal_VOID__UINT_UINT_UINT_UINT,
			      G_TYPE_NONE, 4, G_TYPE_UINT, G_TYPE_UINT, G_TYPE_UINT, G_TYPE_UINT);
	signals[SIGNAL_MESSAGE] =
		g_signal_new ("message",
			      G_TYPE_FROM_CLASS (object_class), G_SIGNAL_RUN_LAST,
			      0, NULL, NULL, g_cclosure_marshal_VOID__POINTER,
			      G_TYPE_NONE, 1, G_TYPE_POINTER);
	signals[SIGNAL_FINISHED] =
		g_signal_new ("finished",
			      G_TYPE_FROM_CLASS (object_class), G_SIGNAL_RUN_LAST,
			      0, NULL, NULL, g_cclosure_marshal_VOID__UINT,
		G_TYPE_NONE, 1, G_TYPE_UINT);
	signals[SIGNAL_ERROR_CODE] =
		g_signal_new ("error-code",
			      G_TYPE_FROM_CLASS (object_class), G_SIGNAL_RUN_LAST,
			      0, NULL, NULL, g_cclosure_marshal_VOID__POINTER,
			      G_TYPE_NONE, 1, G_TYPE_POINTER);

	g_type_class_add_private (klass, sizeof (PkListallerPrivate));
}

/**
 * pk_listaller_new:
 * Return value: A new PkListaller object to access Listaller.
 **/
PkListaller *
pk_listaller_new (void)
{
	if (pk_listaller_object != NULL) {
		g_object_ref (pk_listaller_object);
	} else {
		pk_listaller_object = g_object_new (PK_TYPE_LISTALLER, NULL);
		g_object_add_weak_pointer (pk_listaller_object, &pk_listaller_object);
	}
	return PK_LISTALLER (pk_listaller_object);
}
