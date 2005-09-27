/*  $Id$
 *
 *  Copyright 2002-2005 Jasper Huijsmans (jasper@xfce.org)
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU Library General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
*/

#ifndef __XFCE_SETTINGS_H
#define __XFCE_SETTINGS_H

#define CHANNEL "xfce"

/* IMPORTANT: keep this in sync with mcs_client.c */
enum
{
    XFCE_ORIENTATION,
    XFCE_SIZE,
    XFCE_THEME,
    XFCE_AUTOHIDE,
    XFCE_FULLWIDTH,
    XFCE_HANDLESTYLE,
    XFCE_OPTIONS
};

static char *xfce_settings_names[] = {
    "orientation",
    "size",
    "theme",
    "autohide",
    "fullwidth",
    "handlestyle"
};

#endif /* __XFCE_SETTINGS_H */
