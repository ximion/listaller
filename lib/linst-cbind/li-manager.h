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

#ifndef __LI_MANAGER
#define __LI_MANAGER

#include<stdio.h>
#include<iostream>


void* li_mgr_new(void);

void li_mgr_free(void* mgr);

bool li_mgr_load_apps(void* mgr);

bool li_mgr_register_status_call(void* mgr,TLiStatusChangeCall call,void* user_data);

TRqResult li_mgr_register_request_call(void* mgr,TRequestCall call,void* user_data);

bool li_mgr_register_app_call(void* mgr,TAppEvent call);

void li_mgr_set_sumode(void* mgr,bool md);

bool li_mgr_remove_app(void* mgr,AppInfo obj);

bool li_remove_ipk_installed_app(char *appname,char *appid,TLiStatusChangeCall scall,bool fastmode);

bool li_mgr_check_apps(void* mgr,void* log,bool root);


#endif /* __LI_MANAGER */
