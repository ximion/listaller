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

#include <glib-object.h>


GPointer li_setup_new(void);

void li_setup_free(void);

void li_setup_set_sumode(void);

Char *li_setup_init(GPointer setup,pkname PGChar);

TPkgType li_setup_get_pkgtype(GPointer setup);

GChar *li_setup_get_disallows(GPointer setup);

GChar *li_setup_get_supported_distributions(GPointer setup);

GChar *li_setup_get_appname(GPointer setup);

GChar *li_setup_get_appversion(GPointer setup);

GChar *li_setup_get_pkgid(GPointer setup);

GBoolean li_setup_get_long_description(GPointer setup,list GPointer);

GChar *li_setup_get_wizard_image_path(GPointer setup);

GBoolean li_setup_get_license(GPointer setup,list GPointer);

GBoolean li_setup_get_profiles_list(GPointer setup,list GPointer);

GChar *li_setup_get_appicon(GPointer setup);

GChar *li_setup_get_desktopfiles(GPointer setup);

GChar *li_setup_get_app_exec_command(GPointer setup);

GChar *li_setup_get_current_profile_filelist(GPointer setup);

void li_setup_enable_usource_registering(GPointer setup);

GBoolean li_setup_register_status_call(GPointer setup,call TLiStatusChangeCall,GPointer user_data);

GBoolean li_setup_register_user_request_call(GPointer setup,call TRequestCall,GPointer user_data);

GBoolean li_setup_execute(GPointer setup);

void li_setup_set_forced(GPointer setup);

GBoolean li_setup_get_dependencies(GPointer setup,list PStringList);

GBoolean li_setup_set_profileid(GPointer setup,id GInt16);

TPkgSigState li_setup_get_signature_state(GPointer setup);

GBoolean li_get_ipk_app_installed(PGChar appname,appid PGChar,GBoolean sumode);

void li_set_testmode(PGChar appname,appid PGChar,GBoolean sumode);


#endif /* __LI_INSTALLER */
