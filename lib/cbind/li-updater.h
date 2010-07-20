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

#include <glib-object.h>


GPointer li_updater_new(void);

void li_updater_free(void);

void li_updater_set_sumode(void);

GBoolean li_updater_register_status_call(GPointer upd,call TLiStatusChangeCall,GPointer user_data);

GBoolean li_updater_register_request_call(GPointer upd,call TRequestCall,GPointer user_data);

GBoolean li_updater_register_newupdate_call(GPointer upd,call TNewUpdateEvent,GPointer user_data);

GBoolean li_updater_search_updates(GPointer upd);

GChar *li_updater_updateid_oldversion(GPointer upd,uid GInt32);

GChar *li_updater_updateid_newversion(GPointer upd,uid GInt32);


#endif /* __LI_UPDATER */
