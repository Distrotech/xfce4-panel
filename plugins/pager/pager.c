/*  xfce4
 *
 *  Copyright (C) 2002 Jasper Huijsmans(huysmans@users.sourceforge.net)
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

#ifdef GDK_MULTIHEAD_SAFE
#undef GDK_MULTIHEAD_SAFE
#endif

#include <math.h>

#include <libxfce4util/i18n.h>
#include <libxfcegui4/libxfcegui4.h>

#include <panel/xfce.h>
#include <panel/popup.h>
#include <panel/settings.h>
#include <panel/plugins.h>
#include <panel/mcs_client.h>

typedef struct
{
    const char *signal;
    GCallback callback;
    gpointer data;
}
SignalCallback;

typedef struct
{
    NetkScreen *screen;

    int ws_created_id;
    int ws_destroyed_id;

    GtkWidget *base;

    /* graphical pager */
    GtkWidget *netk_pager;

    /* callback(s) we have to save for reorientation */
    GList *callbacks;
}
t_pager;

/*  callback structure
 *  ------------------
 *  we want to keep track of callbacks to be able to 
 *  add them to new desktop buttons
*/
SignalCallback *
signal_callback_new (const char *signal, GCallback callback, gpointer data)
{
    SignalCallback *sc = g_new0 (SignalCallback, 1);

    sc->signal = signal;
    sc->callback = callback;
    sc->data = data;

    return sc;
}

/*  Graphical pager
 *  ---------------
*/
static void
netk_pager_update_size (GtkWidget * pager, NetkScreen * screen)
{
    int s = icon_size[settings.size] + border_width;

    if (settings.orientation == HORIZONTAL)
    {
	gtk_widget_set_size_request (pager, -1, s);
    }
    else
    {
	gtk_widget_set_size_request (pager, s, -1);
    }
}

GtkWidget *
create_netk_pager (NetkScreen * screen)
{
    GtkWidget *pager;
    GtkOrientation gor = (settings.orientation == VERTICAL) ?
	GTK_ORIENTATION_VERTICAL : GTK_ORIENTATION_HORIZONTAL;

    pager = netk_pager_new (screen);
    netk_pager_set_n_rows (NETK_PAGER (pager), 1);
    netk_pager_set_orientation (NETK_PAGER (pager), gor);
    gtk_widget_show (pager);

    netk_pager_update_size (pager, screen);

    return pager;
}

/* static prototype */
static void arrange_pager (t_pager * pager);

/* settings */
static void
pager_set_size (Control * control, int size)
{
    t_pager *pager;

    gtk_widget_set_size_request (control->base, -1, -1);

    pager = control->data;

    netk_pager_update_size (pager->netk_pager, pager->screen);
}

static void
pager_set_orientation (Control * control, int orientation)
{
    t_pager *pager;

    pager = control->data;

    arrange_pager (pager);
    pager_set_size (control, settings.size);
}

/*  creation, destruction and configuration 
 *  ---------------------------------------
*/
static void
pager_attach_callback (Control * control, const char *signal,
		       GCallback callback, gpointer data)
{
    SignalCallback *sc;
    t_pager *pager;

    pager = control->data;

    sc = signal_callback_new (signal, callback, data);
    pager->callbacks = g_list_append (pager->callbacks, sc);

    g_signal_connect (pager->netk_pager, signal, callback, data);
}

static void
arrange_pager (t_pager * pager)
{
    GList *li;

    if (pager->netk_pager)
	gtk_widget_destroy(pager->netk_pager);
    
    pager->netk_pager = create_netk_pager (pager->screen);

    /* packing the widgets */
    gtk_container_add (GTK_CONTAINER (pager->base), pager->netk_pager);

    /* attach callbacks */
    for (li = pager->callbacks; li; li = li->next)
    {
	SignalCallback *cb = li->data;

	g_signal_connect (pager->netk_pager,
			  cb->signal, cb->callback, cb->data);
    }
}

/* callbacks */
static void
pager_screen_created (NetkScreen * screen, NetkWorkspace * ws,
		      t_pager * pager)
{
    netk_pager_update_size (pager->netk_pager, pager->screen);
}

static void
pager_screen_destroyed (NetkScreen * screen, NetkWorkspace * ws,
			t_pager * pager)
{
    netk_pager_update_size (pager->netk_pager, pager->screen);
}

t_pager *
pager_new (NetkScreen * screen)
{
    t_pager *pager = g_new0 (t_pager, 1);

    pager->screen = screen;

    pager->base = gtk_alignment_new (0.5, 0.5, 0, 0); 
    gtk_widget_show (pager->base); 

    /* this creates all widgets */
    arrange_pager (pager);

    pager->ws_created_id =
	g_signal_connect (pager->screen, "workspace-created",
			  G_CALLBACK (pager_screen_created), pager);

    pager->ws_destroyed_id =
	g_signal_connect (pager->screen, "workspace-destroyed",
			  G_CALLBACK (pager_screen_destroyed), pager);

    return pager;
}

static void
pager_free (Control * control)
{
    GList *li;
    t_pager *pager;

    pager = control->data;

    g_signal_handler_disconnect (pager->screen, pager->ws_created_id);
    g_signal_handler_disconnect (pager->screen, pager->ws_destroyed_id);

    for (li = pager->callbacks; li; li = li->next)
	g_free (li->data);

    g_list_free (pager->callbacks);

    g_free (pager);
}

/*  Switcher panel control
 *  ----------------------
*/
gboolean
create_pager_control (Control * control)
{
    t_pager *pager;
    NetkScreen *screen;

    screen = netk_screen_get_default ();

    netk_screen_force_update (screen);
    pager = pager_new (screen);
    netk_screen_force_update (screen);

    gtk_container_add (GTK_CONTAINER (control->base), pager->base);

    control->data = pager;
    control->with_popup = FALSE;

    pager_set_size (control, settings.size);

    return TRUE;
}

G_MODULE_EXPORT void
xfce_control_class_init (ControlClass * cc)
{
    cc->name = "pager";
    cc->caption = _("Graphical pager");

    cc->create_control = (CreateControlFunc) create_pager_control;

    cc->free = pager_free;
    cc->attach_callback = pager_attach_callback;

    cc->set_orientation = pager_set_orientation;
    cc->set_size = pager_set_size;
}

XFCE_PLUGIN_CHECK_INIT
