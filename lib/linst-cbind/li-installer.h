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

void li_setup_free(LiInstallation setup);

bool li_setup_init(LiInstallation setup,char *pkname);

bool li_setup_register_status_call(LiInstallation setup,StatusChangeEvent call,void* user_data);

bool li_setup_register_user_request_call(LiInstallation setup,UserRequestCall call,void* user_data);

PkgType li_setup_pkgtype(LiInstallation setup);

void li_set_testmode(bool st);

void li_setup_set_forced(LiInstallation setup,char *str);

void li_setup_set_sumode(LiInstallation setup,bool b);

bool li_setup_sumode(LiInstallation setup);

char *li_setup_disallows(LiInstallation setup);

char *li_setup_supported_distros(LiInstallation setup);

bool li_ipk_app_is_installed(char *appname,char *appid,bool sumode);

char *li_setup_appname(LiInstallation setup);

char *li_setup_appversion(LiInstallation setup);

char *li_setup_pkgid(LiInstallation setup);

PkgSignatureState li_setup_signature_state(LiInstallation setup);

bool li_setup_long_description(LiInstallation setup,void* list);

char *li_setup_wizard_image_path(LiInstallation setup);

bool li_setup_license(LiInstallation setup,void* list);

bool li_setup_profiles_list(LiInstallation setup,void* list);

void li_setup_set_profileid(LiInstallation setup,int id);

void li_setup_enable_usource_registering(LiInstallation setup,bool b);

char *li_setup_appicon(LiInstallation setup);

char *li_setup_desktopfiles(LiInstallation setup);

char *li_setup_app_exec_command(LiInstallation setup);

char *li_setup_current_profile_filelist(LiInstallation setup);

bool li_setup_execute(LiInstallation setup);

void li_setup_exec_by_daemon(LiInstallation setup,bool b);

#endif /* __LI_INSTALLER */
