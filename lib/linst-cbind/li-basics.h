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

#ifndef __LI_BASICS
#define __LI_BASICS

#include <stdlib.h>
#include <stdio.h>

void* li_new_stringlist(void);

bool li_free_stringlist(StringList lst);

char *li_stringlist_read_line(StringList lst,int ln);

bool li_stringlist_write_line(StringList lst,int ln,char *val);

char *li_stringlist_to_text(StringList lst);

char *li_current_regdir(void);

char *li_global_regdir(void);


#endif /* __LI_BASICS */