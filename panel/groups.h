/*  xfce4
 *  
 *  Copyright (C) 2002 Jasper Huijsmans (huysmans@users.sourceforge.net)
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
*/

#ifndef __XFCE_GROUPS_H__
#define __XFCE_GROUPS_H__

#include <panel/global.h>

void groups_init (GtkBox * box);
void groups_pack (GtkBox * box);
void groups_unpack (void);
void groups_cleanup (void);

void old_groups_set_from_xml (int side, xmlNodePtr node);
void groups_set_from_xml (xmlNodePtr node);
void groups_write_xml (xmlNodePtr root);

void groups_register_control (Control * control);
Control *groups_get_control (int index);
PanelPopup *groups_get_popup (int index);

void groups_move (int from, int to);
void groups_remove (int index);
void groups_show_popup (int index, gboolean show);

/* settings */
void groups_set_orientation (int orientation);
void groups_set_layer (int layer);

void groups_set_size (int size);
void groups_set_popup_position (int position);
void groups_set_theme (const char *theme);

/* add/remove */
void groups_set_num_groups (int n);
void groups_add_control (int id, const char *filename, int index);

/* arrow direction */
void groups_set_arrow_direction (GtkArrowType type);
GtkArrowType groups_get_arrow_direction (void);

#endif /* __XFCE_GROUPS_H__ */
