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


void* li_setup_new(void);

void li_setup_free(void* setup);

void li_setup_set_sumode(void* setup,bool b);

char *li_setup_init(void* setup,char *pkname);

kgType li_setup_get_pkgtype(void* setup);

char *li_setup_get_disallows(void* setup);

char *li_setup_get_supported_distributions(void* setup);

char *li_setup_get_appname(void* setup);

char *li_setup_get_appversion(void* setup);

char *li_setup_get_pkgid(void* setup);

bool li_setup_get_long_description(void* setup,void* list);

char *li_setup_get_wizard_image_path(void* setup);

bool li_setup_get_license(void* setup,void* list);

bool li_setup_get_profiles_list(void* setup,void* list);

char *li_setup_get_appicon(void* setup);

char *li_setup_get_desktopfiles(void* setup);

char *li_setup_get_app_exec_command(void* setup);

char *li_setup_get_current_profile_filelist(void* setup);

void li_setup_enable_usource_registering(void* setup,bool b);

bool li_setup_register_status_call(void* setup,StatusChangeEvent call,void* user_data);

bool li_setup_register_user_request_call(void* setup,UserRequestCall call,void* user_data);

bool li_setup_execute(void* setup);

void li_setup_set_forced(void* setup,char *str);

bool li_setup_get_dependencies(void* setup,void* list);

bool li_setup_set_profileid(void* setup,int id);

kgSignatureState li_setup_get_signature_state(void* setup);

bool li_get_ipk_app_installed(char *appname,char *appid,bool sumode);

void li_set_testmode(bool st);


#endif /* __LI_INSTALLER */
