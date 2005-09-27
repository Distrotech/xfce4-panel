/*  $Id$
 *  
 *  Copyright (C) 2002-2004 Jasper Huijsmans (jasper@xfce.org)
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

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <gmodule.h>
#include <libxfce4util/libxfce4util.h>
#include <libxfcegui4/xfce-icontheme.h>

#include "xfce.h"

#ifdef HAVE_GDK_PIXBUF_NEW_FROM_STREAM
#define gdk_pixbuf_new_from_inline gdk_pixbuf_new_from_stream
#endif

static XfceIconTheme *global_icon_theme = NULL;

G_MODULE_EXPORT /* EXPORT:icon_names */
const char *icon_names[NUM_ICONS];


static void
theme_changed (XfceIconTheme * icontheme)
{
#if 0
    char *theme;

    g_object_get (gtk_settings_get_default (), "gtk-icon-theme-name",
		  &theme, NULL);

    DBG ("Theme: %s\n", theme);

    g_free (theme);
#endif
    
    panel_set_theme (NULL);
}

G_MODULE_EXPORT /* EXPORT:icons_init */
void
icons_init (void)
{
    int i = 0;

    icon_names[0]   = _("Default");
    icon_names[++i] = _("Editor");
    icon_names[++i] = _("File management");
    icon_names[++i] = _("Utilities");
    icon_names[++i] = _("Games");
    icon_names[++i] = _("Help browser");
    icon_names[++i] = _("Multimedia");
    icon_names[++i] = _("Network");
    icon_names[++i] = _("Graphics");
    icon_names[++i] = _("Printer");
    icon_names[++i] = _("Productivity");
    icon_names[++i] = _("Sound");
    /* Translation with context. 
     * Translators only need to translate 'Terminal' */
    icon_names[++i] = Q_("icons|Terminal");

    global_icon_theme = 
        xfce_icon_theme_get_for_screen (gdk_screen_get_default());

    g_signal_connect (global_icon_theme, "changed",
		      G_CALLBACK (theme_changed), NULL);
}

G_MODULE_EXPORT /* EXPORT:themed_pixbuf_from_name_list */
GdkPixbuf *
themed_pixbuf_from_name_list (char **namelist, int size)
{
    GdkPixbuf *pb;
    char *icon = NULL, *fallback = NULL;

    for (; namelist[0] != NULL; ++namelist)
    {
	icon = xfce_icon_theme_lookup (global_icon_theme, namelist[0], size);

	if (icon && (strstr (icon, "hicolor") || strstr (icon, "pixmaps")))
	{
	    if (!fallback)
		fallback = icon;
	    else
		g_free (icon);

	    icon = NULL;
	}

	if (icon)
	    break;
    }

    if (icon)
    {
	pb = gdk_pixbuf_new_from_file (icon, NULL);
    }
    else if (fallback)
    {
	pb = gdk_pixbuf_new_from_file (fallback, NULL);
    }
    else
    {
	pb = xfce_icon_theme_load_category (global_icon_theme,
					    XFCE_ICON_CATEGORY_UNKNOWN, size);
    }

    g_free (icon);
    g_free (fallback);

    return pb;
}

G_MODULE_EXPORT /* EXPORT:get_pixbuf_by_id */
GdkPixbuf *
get_pixbuf_by_id (int id)
{
    char *icon = NULL;
    GdkPixbuf *pb = NULL;

    switch (id)
    {
	case EDIT_ICON:
	    icon =
		xfce_icon_theme_lookup_category (global_icon_theme,
						 XFCE_ICON_CATEGORY_EDITOR,
						 icon_size[settings.size]);
	    break;
	case FILE1_ICON:
	    icon =
		xfce_icon_theme_lookup_category (global_icon_theme,
						 XFCE_ICON_CATEGORY_FILEMAN,
						 icon_size[settings.size]);
	    break;
	case FILE2_ICON:
	    icon =
		xfce_icon_theme_lookup_category (global_icon_theme,
						 XFCE_ICON_CATEGORY_UTILITY,
						 icon_size[settings.size]);
	    break;
	case GAMES_ICON:
	    icon =
		xfce_icon_theme_lookup_category (global_icon_theme,
						 XFCE_ICON_CATEGORY_GAME,
						 icon_size[settings.size]);
	    break;
	case MAN_ICON:
	    icon =
		xfce_icon_theme_lookup_category (global_icon_theme,
						 XFCE_ICON_CATEGORY_HELP,
						 icon_size[settings.size]);
	    break;
	case MULTIMEDIA_ICON:
	    icon =
		xfce_icon_theme_lookup_category (global_icon_theme,
						 XFCE_ICON_CATEGORY_MULTIMEDIA,
						 icon_size[settings.size]);
	    break;
	case NETWORK_ICON:
	    icon =
		xfce_icon_theme_lookup_category (global_icon_theme,
						 XFCE_ICON_CATEGORY_NETWORK,
						 icon_size[settings.size]);
	    break;
	case PAINT_ICON:
	    icon =
		xfce_icon_theme_lookup_category (global_icon_theme,
						 XFCE_ICON_CATEGORY_GRAPHICS,
						 icon_size[settings.size]);
	    break;
	case PRINT_ICON:
	    icon =
		xfce_icon_theme_lookup_category (global_icon_theme,
						 XFCE_ICON_CATEGORY_PRINTER,
						 icon_size[settings.size]);
	    break;
	case SCHEDULE_ICON:
	    icon =
		xfce_icon_theme_lookup_category (global_icon_theme,
						 XFCE_ICON_CATEGORY_PRODUCTIVITY,
						 icon_size[settings.size]);
	    break;
	case SOUND_ICON:
	    icon =
		xfce_icon_theme_lookup_category (global_icon_theme,
						 XFCE_ICON_CATEGORY_SOUND,
						 icon_size[settings.size]);
	    break;
	case TERMINAL_ICON:
	    icon =
		xfce_icon_theme_lookup_category (global_icon_theme,
						 XFCE_ICON_CATEGORY_TERMINAL,
						 icon_size[settings.size]);
	    break;
    }

    if (!icon)
    {
	icon =
	    xfce_icon_theme_lookup_category (global_icon_theme,
					     XFCE_ICON_CATEGORY_UNKNOWN,
					     icon_size[settings.size]);
    }

    if (icon)
    {
	pb = gdk_pixbuf_new_from_file (icon, NULL);

	g_free (icon);
    }

    return pb;
}

G_MODULE_EXPORT /* EXPORT:get_pixbuf_from_file */
GdkPixbuf *
get_pixbuf_from_file (const char *path)
{
    GdkPixbuf *pb = NULL;

    if (g_file_test (path, G_FILE_TEST_EXISTS))
    {
	pb = gdk_pixbuf_new_from_file (path, NULL);
    }
    else
    {
        char *icon = NULL;

	icon = xfce_icon_theme_lookup (global_icon_theme, path,
				       icon_size[settings.size]);
        
        if (!icon)
        {
            icon =
                xfce_icon_theme_lookup_category (global_icon_theme,
                                                 XFCE_ICON_CATEGORY_UNKNOWN,
                                                 icon_size[settings.size]);
        }

        if (icon)
        {
            pb = gdk_pixbuf_new_from_file (icon, NULL);

            g_free (icon);
        }
    }

    return pb;
}

G_MODULE_EXPORT /* EXPORT:get_panel_pixbuf */
GdkPixbuf *
get_panel_pixbuf (void)
{
    char *icon = NULL;
    GdkPixbuf *pb = NULL;

    icon = xfce_icon_theme_lookup (global_icon_theme, "xfce4-panel", 48);

    if (icon)
    {
	pb = xfce_pixbuf_new_from_file_at_size (icon, 48, 48, NULL);

	g_free (icon);
    }

    return pb;
}

G_MODULE_EXPORT /* EXPORT:get_scaled_pixbuf */
GdkPixbuf *
get_scaled_pixbuf (GdkPixbuf * pb, int size)
{
    int w, h, neww, newh;
    GdkPixbuf *newpb;

    if (!pb || !GDK_IS_PIXBUF (pb))
    {
	GdkPixbuf *tmp = get_pixbuf_by_id (UNKNOWN_ICON);

	newpb = get_scaled_pixbuf (tmp, size);
	g_object_unref (tmp);

	return newpb;
    }

    w = gdk_pixbuf_get_width (pb);
    h = gdk_pixbuf_get_height (pb);

    if (size > w && size > h)
    {
	newpb = pb;
	g_object_ref (newpb);
	return newpb;
    }
    else if (h > w)
    {
	newh = size;
	neww = ((double) w * (double) size / (double) h);
    }
    else
    {
	neww = size;
	newh = ((double) h * (double) size / (double) w);
    }

    return gdk_pixbuf_scale_simple (pb, neww, newh, GDK_INTERP_BILINEAR);
}

G_MODULE_EXPORT /* EXPORT:get_themed_pixbuf */
GdkPixbuf *
get_themed_pixbuf (const char *name)
{
    char *icon = NULL;
    GdkPixbuf *pb = NULL;

    icon = xfce_icon_theme_lookup (global_icon_theme, name, 48);

    if (!icon)
    {
	icon =
	    xfce_icon_theme_lookup_category (global_icon_theme,
					     XFCE_ICON_CATEGORY_UNKNOWN, 48);
    }

    if (icon)
    {
	pb = gdk_pixbuf_new_from_file (icon, NULL);

	g_free (icon);
    }

    return pb;
}
