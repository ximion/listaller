/* -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*-
 *
 * Copyright (C) 2010-2012 Matthias Klumpp <matthias@tenstral.net>
 *
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

#include "li-plugin-utils.h"

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
 * Create a Listaller AppItem from a PackageKit package-id
 **/
ListallerAppItem*
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
	listaller_app_item_set_state (item, LISTALLER_APP_STATE_INSTALLED_SHARED);

	g_debug ("listaller: <appid> %s %s %s", parts[0], parts[1], tmp[1]);

out:
	g_strfreev (tmp);
	g_strfreev (parts);
	return item;
}

/**
 * pk_listaller_pkid_from_appitem:
 *
 * Generate a PackageKit package-id from a Listaller AppItem.
 * Return value: The resulting package ID or NULL
 **/
gchar*
pk_listaller_pkid_from_appitem (ListallerAppItem *item)
{
	gchar *package_id;
	g_return_val_if_fail (LISTALLER_IS_APP_ITEM (item), NULL);

	package_id = listaller_app_item_build_pk_package_id (item);

	return package_id;
}

/**
 * pk_listaller_is_setup_file:
 *
 * Check if the given filename is a Listaller setup package
 */
gboolean
pk_listaller_is_setup_file (const gchar *filename)
{
	/* TODO: should get content type (instead of just checking file extension)... */
	return g_str_has_suffix (filename, ".ipk");
}

/**
 * pk_listaller_is_package:
 */
gboolean
pk_listaller_is_package (const gchar *package_id)
{
	return (g_strstr_len (package_id, -1,
			     "local:listaller") != NULL);
}

/**
 * pk_transaction_filter_listaller_packages:
 */
gchar **
pk_transaction_filter_listaller_packages (PkTransaction *transaction,
					gchar **package_ids)
{
	gboolean ret = FALSE;
	gchar **package_ids_new = NULL;
	gchar **retval = NULL;
	GPtrArray *listaller = NULL;
	GPtrArray *native = NULL;
	guint i;

	if (package_ids == NULL)
		return NULL;

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
 * pk_listaller_filter_listaller_files:
 */
gchar **
pk_transaction_filter_listaller_files (PkTransaction *transaction,
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
		ret = pk_listaller_is_setup_file (files[i]);
		if (ret)
			break;
	}
	if (!ret)
		goto out;

	/* find and filter listaller packages */
	native = g_ptr_array_new_with_free_func (g_free);
	listaller = g_ptr_array_new_with_free_func (g_free);

	for (i=0; files[i] != NULL; i++) {
		ret = pk_listaller_is_setup_file (files[i]);
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
