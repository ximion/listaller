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

#ifndef __LI_PLUGIN_UTILS_H
#define __LI_PLUGIN_UTILS_H

#include <packagekit-glib2/packagekit.h>
#include <plugin/packagekit-plugin.h>
#include "listaller_internal.h"

G_BEGIN_DECLS

gchar			*pk_packages_get_listaller_file			(gchar ***full_paths);
gboolean	 	 pk_listaller_contains_listaller_files		(gchar **full_paths);

ListallerAppItem	*pk_listaller_appitem_from_pkid			(const gchar *package_id);
gchar			*pk_listaller_pkid_from_appitem			(ListallerAppItem *item);

gboolean		 pk_listaller_is_setup_file			(const gchar *filename);
gboolean		 pk_listaller_is_package			(const gchar *package_id);

gchar			**pk_transaction_filter_listaller_packages	(PkTransaction *transaction,
										gchar **package_ids);
gchar			**pk_transaction_filter_listaller_files		(PkTransaction *transaction,
										gchar **files);

G_END_DECLS

#endif /* __LI_PLUGIN_UTILS_H */
