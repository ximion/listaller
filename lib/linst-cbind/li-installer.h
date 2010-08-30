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

#ifndef __LI_INSTALLER
#define __LI_INSTALLER

#include<stdio.h>
#include<iostream>


bool li_remove_ipk_installed_app(char *appname,char *appid,StatusChangeEvent statuscall,bool fastmode);

void* li_setup_new(void);

void li_setup_free(Installation setup);

bool li_setup_init(Installation setup,char *pkname);

bool li_setup_register_status_call(Installation setup,StatusChangeEvent call,void* user_data);

bool li_setup_register_user_request_call(Installation setup,UserRequestCall call,void* user_data);

kgType li_setup_get_pkgtype(Installation setup);

void li_set_testmode(bool st);

void li_setup_set_forced(Installation setup,char *str);

void li_setup_set_sumode(Installation setup,bool b);

char *li_setup_get_disallows(Installation setup);

char *li_setup_get_supported_distributions(Installation setup);

bool li_get_ipk_app_installed(char *appname,char *appid,bool sumode);

char *li_setup_get_appname(Installation setup);

char *li_setup_get_appversion(Installation setup);

char *li_setup_get_pkgid(Installation setup);

kgSignatureState li_setup_get_signature_state(Installation setup);

bool li_setup_get_long_description(Installation setup,void* list);

char *li_setup_get_wizard_image_path(Installation setup);

bool li_setup_get_license(Installation setup,void* list);

bool li_setup_get_profiles_list(Installation setup,void* list);

void li_setup_set_profileid(Installation setup,int id);

void li_setup_enable_usource_registering(Installation setup,bool b);

char *li_setup_get_appicon(Installation setup);

char *li_setup_get_desktopfiles(Installation setup);

char *li_setup_get_app_exec_command(Installation setup);

char *li_setup_get_current_profile_filelist(Installation setup);

bool li_setup_execute(Installation setup);

void li_setup_exec_by_daemon(Installation setup,bool b);


#endif /* __LI_INSTALLER */
