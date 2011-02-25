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

#ifndef __LI_APPITEM
#define __LI_APPITEM

#include <stdlib.h>
#include <stdio.h>

LiAppItem *li_appitem_new(void);

LiAppItem *li_appitem_new_from_appid(char *appID);

char *li_appitem_name(LiAppItem *item);

char *li_appitem_id(LiAppItem *item);

char *li_appitem_version(LiAppItem *item);

char *li_appitem_summary(LiAppItem *item);

char *li_appitem_author(LiAppItem *item);

char *li_appitem_publisher(LiAppItem *item);

char *li_appitem_iconname(LiAppItem *item);

char *li_appitem_categories(LiAppItem *item);

double li_appitem_timestamp(LiAppItem *item);

char *li_appitem_dependencies(LiAppItem *item);


#endif /* __LI_APPITEM */
