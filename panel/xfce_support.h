/*  $Id$
 *  
 *  Copyright 2002-2004 Jasper Huijsmans (jasper@xfce.org)
 *  
 *  Startup notification added by Olivier Fourdan based on gnome-desktop
 *  developed by Elliot Lee <sopwith@redhat.com> and Sid Vicious
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

#include <gmodule.h>

#ifndef __XFCE_SUPPORT_H__
#define __XFCE_SUPPORT_H__

/* dnd */
typedef void (*DragCallback) (GtkWidget * widget, gpointer data);

typedef void (*DropCallback) (GtkWidget * widget, GList * drop_data,
			      gpointer data);

#define DRAG_CALLBACK(f) (DragCallback)f

#define DROP_CALLBACK(f) (DropCallback)f

G_MODULE_IMPORT void dnd_set_drag_dest (GtkWidget * widget);

G_MODULE_IMPORT void dnd_set_drag_callback (GtkWidget * widget, 
                       			    DragCallback function, 
					    gpointer data);

G_MODULE_IMPORT void dnd_set_callback (GtkWidget * widget, 
    				       DropCallback function, 
				       gpointer data);

G_MODULE_IMPORT void gnome_uri_list_free_strings (GList * list);

G_MODULE_IMPORT GList *gnome_uri_list_extract_uris (const gchar * uri_list);

G_MODULE_IMPORT GList *
	gnome_uri_list_extract_filenames (const gchar * uri_list);

	
/* tooltips */
G_MODULE_IMPORT void add_tooltip (GtkWidget * widget, const char *tip);


/* file open dialog */
G_MODULE_IMPORT char *select_file_name (const char *title, const char *path,
			GtkWidget * parent);

G_MODULE_IMPORT char *select_file_with_preview (const char *title, 
    						const char *path,
						GtkWidget * parent);


/* executing programs */
G_MODULE_IMPORT void exec_cmd (const char *cmd, gboolean in_terminal, 
    			       gboolean use_sn);

G_MODULE_IMPORT void exec_cmd_silent (const char *cmd, gboolean in_terminal, 
    			              gboolean use_sn);

#endif /* __XFCE_SUPPORT_H__ */
