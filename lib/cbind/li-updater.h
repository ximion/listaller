/* -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*-
 *
 * Copyright (C) 2010 Matthias Klumpp <matthias@nlinux.org>
 *
 * Licensed under the GNU General Public License Version 3
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 3 of the License, or
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

#if !defined (__LISTALLER_H_INSIDE__)
#error "Only <listaller.h> can be included directly."
#endif

#ifndef __LI_UPDATER
#define __LI_UPDATER

#include <glib.h>
#include <stdio.h>


gpointer li_updater_new(void);

void li_updater_free(void);

void li_updater_set_sumode(void);

gboolean li_updater_register_status_call(gpointer upd,TLiStatusChangeCall call,gpointer user_data);

gboolean li_updater_register_request_call(gpointer upd,TRequestCall call,gpointer user_data);

gboolean li_updater_register_newupdate_call(gpointer upd,TNewUpdateEvent call,gpointer user_data);

gboolean li_updater_search_updates(gpointer upd);

gchar *li_updater_updateid_oldversion(gpointer upd,gint32 uid);

gchar *li_updater_updateid_newversion(gpointer upd,gint32 uid);


#endif /* __LI_UPDATER */
