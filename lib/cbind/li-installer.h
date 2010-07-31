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

#include <glib.h>
#include <stdio.h>


gpointer li_setup_new(void);

void li_setup_free(void);

void li_setup_set_sumode(void);

char *li_setup_init(gpointer setup,gchar *pkname);

TPkgType li_setup_get_pkgtype(gpointer setup);

gchar *li_setup_get_disallows(gpointer setup);

gchar *li_setup_get_supported_distributions(gpointer setup);

gchar *li_setup_get_appname(gpointer setup);

gchar *li_setup_get_appversion(gpointer setup);

gchar *li_setup_get_pkgid(gpointer setup);

gboolean li_setup_get_long_description(gpointer setup,gpointer list);

gchar *li_setup_get_wizard_image_path(gpointer setup);

gboolean li_setup_get_license(gpointer setup,gpointer list);

gboolean li_setup_get_profiles_list(gpointer setup,gpointer list);

gchar *li_setup_get_appicon(gpointer setup);

gchar *li_setup_get_desktopfiles(gpointer setup);

gchar *li_setup_get_app_exec_command(gpointer setup);

gchar *li_setup_get_current_profile_filelist(gpointer setup);

void li_setup_enable_usource_registering(gpointer setup);

gboolean li_setup_register_status_call(gpointer setup,TLiStatusChangeCall call,gpointer user_data);

gboolean li_setup_register_user_request_call(gpointer setup,TRequestCall call,gpointer user_data);

gboolean li_setup_execute(gpointer setup);

void li_setup_set_forced(gpointer setup);

gboolean li_setup_get_dependencies(gpointer setup,stringlist *list);

gboolean li_setup_set_profileid(gpointer setup,gint16 id);

TPkgSigState li_setup_get_signature_state(gpointer setup);

gboolean li_get_ipk_app_installed(gchar *appname,gchar *appid,gboolean sumode);

void li_set_testmode(gchar *appname,gchar *appid,gboolean sumode);


#endif /* __LI_INSTALLER */
