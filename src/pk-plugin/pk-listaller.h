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

#ifndef __PK_LISTALLER_H
#define __PK_LISTALLER_H

#include <glib.h>
#include <glib-object.h>
#include <gmodule.h>

G_BEGIN_DECLS

#define PK_TYPE_LISTALLER		(pk_listaller_get_type ())
#define PK_LISTALLER(o)			(G_TYPE_CHECK_INSTANCE_CAST ((o), PK_TYPE_LISTALLER, PkListaller))
#define PK_LISTALLER_CLASS(k)		(G_TYPE_CHECK_CLASS_CAST((k), PK_TYPE_LISTALLER, PkListallerClass))
#define PK_IS_LISTALLER(o)		(G_TYPE_CHECK_INSTANCE_TYPE ((o), PK_TYPE_LISTALLER))
#define PK_IS_LISTALLER_CLASS(k)	(G_TYPE_CHECK_CLASS_TYPE ((k), PK_TYPE_LISTALLER))
#define PK_LISTALLER_GET_CLASS(o)	(G_TYPE_INSTANCE_GET_CLASS ((o), PK_TYPE_LISTALLER, PkListallerClass))

typedef struct PkListallerPrivate PkListallerPrivate;

typedef struct
{
	 GObject		 parent;
	 PkListallerPrivate	*priv;
} PkListaller;

typedef struct
{
	GObjectClass		 parent_class;
} PkListallerClass;

/**
 * PkListallerStatus:
 *
 * Status of the Listaller fake backend
 **/
typedef enum {
	PK_LISTALLER_STATUS_UNKNOWN,
	PK_LISTALLER_STATUS_ENTRIES_LEFT, /* action successful, but backend entries left */
	PK_LISTALLER_STATUS_BROKEN,
	PK_LISTALLER_STATUS_FINISHED, /* action successful, no work left */
	PK_LISTALLER_STATUS_FAILED
} PkLiStatus;

GType		 pk_listaller_get_type			(void);
PkListaller	*pk_listaller_new			(void);

gboolean	 pk_listaller_scan_applications		(PkListaller *pkli);
void		 pk_listaller_find_applications		(PkListaller *pkli, gchar **values);

void		 pk_listaller_remove_applications	(PkListaller *pkli, gchar ***package_ids);
void		 pk_listaller_get_details 		(PkListaller *pkli, gchar ***package_ids);

void		 pk_listaller_install_files 		(PkListaller *pkli, gchar ***full_paths);

PkLiStatus	 pk_listaller_get_status		(PkListaller *pkli);
void		 pk_listaller_reset 			(PkListaller *pkli);

void		 pk_listaller_delete_app_ids 		(PkListaller *pkli, gchar ***package_ids);
gchar		*pk_packages_get_listaller_file 	(gchar ***full_paths);
gboolean	 pk_listaller_contains_listaller_files (gchar **full_paths);

G_END_DECLS

#endif /* __PK_LISTALLER_H */
