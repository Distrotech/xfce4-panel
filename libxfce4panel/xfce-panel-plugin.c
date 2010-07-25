/*
 * Copyright (C) 2008-2010 Nick Schermer <nick@xfce.org>
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
 */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <gtk/gtk.h>
#include <glib.h>
#include <libxfce4util/libxfce4util.h>

#include <common/panel-private.h>
#include <libxfce4panel/xfce-panel-macros.h>
#include <libxfce4panel/xfce-panel-plugin.h>
#include <libxfce4panel/xfce-panel-plugin-provider.h>
#include <libxfce4panel/libxfce4panel-marshal.h>
#include <libxfce4panel/libxfce4panel-alias.h>



/**
 * SECTION: xfce-panel-plugin
 * @title: XfcePanelPlugin
 * @short_description: Interface for panel plugins
 * @include: libxfce4panel/libxfce4panel.h
 *
 * The interface plugin developers used to interact with the plugin and
 * the panel.
 **/



#define XFCE_PANEL_PLUGIN_CONSTRUCTED(plugin) \
  PANEL_HAS_FLAG (XFCE_PANEL_PLUGIN (plugin)->priv->flags, \
                  PLUGIN_FLAG_CONSTRUCTED)



typedef const gchar *(*ProviderToPluginChar) (XfcePanelPluginProvider *provider);
typedef gint         (*ProviderToPluginInt)  (XfcePanelPluginProvider *provider);



static void          xfce_panel_plugin_provider_init          (XfcePanelPluginProviderInterface *iface);
static GObject      *xfce_panel_plugin_constructor            (GType                             type,
                                                               guint                             n_props,
                                                               GObjectConstructParam            *props);
static void          xfce_panel_plugin_get_property           (GObject                          *object,
                                                               guint                             prop_id,
                                                               GValue                           *value,
                                                               GParamSpec                       *pspec);
static void          xfce_panel_plugin_set_property           (GObject                          *object,
                                                               guint                             prop_id,
                                                               const GValue                     *value,
                                                               GParamSpec                       *pspec);
static void          xfce_panel_plugin_dispose                (GObject                          *object);
static void          xfce_panel_plugin_finalize               (GObject                          *object);
static void          xfce_panel_plugin_realize                (GtkWidget                        *widget);
static gboolean      xfce_panel_plugin_button_press_event     (GtkWidget                        *widget,
                                                               GdkEventButton                   *event);
static void          xfce_panel_plugin_menu_move              (XfcePanelPlugin                  *plugin);
static void          xfce_panel_plugin_menu_remove            (XfcePanelPlugin                  *plugin);
static void          xfce_panel_plugin_menu_add_items         (XfcePanelPlugin                  *plugin);
static void          xfce_panel_plugin_menu_panel_preferences (XfcePanelPlugin                  *plugin);
static GtkMenu      *xfce_panel_plugin_menu_get               (XfcePanelPlugin                  *plugin);
static inline gchar *xfce_panel_plugin_relative_filename      (XfcePanelPlugin                  *plugin);
static void          xfce_panel_plugin_unregister_menu        (GtkMenu                          *menu,
                                                               XfcePanelPlugin                  *plugin);
static void          xfce_panel_plugin_set_size               (XfcePanelPluginProvider          *provider,
                                                               gint                              size);
static void          xfce_panel_plugin_set_orientation        (XfcePanelPluginProvider          *provider,
                                                               GtkOrientation                    orientation);
static void          xfce_panel_plugin_set_screen_position    (XfcePanelPluginProvider          *provider,
                                                               XfceScreenPosition                screen_position);
static void          xfce_panel_plugin_save                   (XfcePanelPluginProvider          *provider);
static gboolean      xfce_panel_plugin_get_show_configure     (XfcePanelPluginProvider          *provider);
static void          xfce_panel_plugin_show_configure         (XfcePanelPluginProvider          *provider);
static gboolean      xfce_panel_plugin_get_show_about         (XfcePanelPluginProvider          *provider);
static void          xfce_panel_plugin_show_about             (XfcePanelPluginProvider          *provider);
static void          xfce_panel_plugin_removed                (XfcePanelPluginProvider          *provider);
static gboolean      xfce_panel_plugin_remote_event           (XfcePanelPluginProvider          *provider,
                                                               const gchar                      *name,
                                                               const GValue                     *value,
                                                               guint                            *handle);
static void          xfce_panel_plugin_set_locked             (XfcePanelPluginProvider          *provider,
                                                               gboolean                          locked);
static void          xfce_panel_plugin_take_window_notify     (gpointer                          data,
                                                               GObject                          *where_the_object_was);
static void          xfce_panel_plugin_menu_item_destroy      (GtkWidget                        *item,
                                                               XfcePanelPlugin                  *plugin);



enum
{
  PROP_0,
  PROP_NAME,
  PROP_DISPLAY_NAME,
  PROP_COMMENT,
  PROP_ARGUMENTS,
  PROP_UNIQUE_ID,
  PROP_ORIENTATION,
  PROP_SIZE,
  PROP_SCREEN_POSITION,
  PROP_EXPAND
};

enum
{
  ABOUT,
  CONFIGURE_PLUGIN,
  FREE_DATA,
  ORIENTATION_CHANGED,
  REMOTE_EVENT,
  REMOVED,
  SAVE,
  SIZE_CHANGED,
  SCREEN_POSITION_CHANGED,
  LAST_SIGNAL
};

typedef enum
{
  PLUGIN_FLAG_DISPOSED       = 1 << 0,
  PLUGIN_FLAG_CONSTRUCTED    = 1 << 1,
  PLUGIN_FLAG_REALIZED       = 1 << 2,
  PLUGIN_FLAG_SHOW_CONFIGURE = 1 << 3,
  PLUGIN_FLAG_SHOW_ABOUT     = 1 << 4,
  PLUGIN_FLAG_BLOCK_AUTOHIDE = 1 << 5
}
PluginFlags;

struct _XfcePanelPluginPrivate
{
  /* plugin information */
  gchar               *name;
  gchar               *display_name;
  gchar               *comment;
  gint                 unique_id;
  gchar               *property_base;
  gchar              **arguments;
  gint                 size;
  guint                expand : 1;
  GtkOrientation       orientation;
  XfceScreenPosition   screen_position;
  guint                locked : 1;
  GSList              *menu_items;

  /* flags for rembering states */
  PluginFlags          flags;

  /* plugin right-click menu */
  GtkMenu             *menu;

  /* menu block counter (configure insensitive) */
  gint                 menu_blocked;

  /* autohide block counter */
  gint                 panel_lock;
};



static guint  plugin_signals[LAST_SIGNAL];
static GQuark item_properties = 0;
static GQuark item_about = 0;



G_DEFINE_TYPE_WITH_CODE (XfcePanelPlugin, xfce_panel_plugin, GTK_TYPE_EVENT_BOX,
                         G_IMPLEMENT_INTERFACE (XFCE_TYPE_PANEL_PLUGIN_PROVIDER,
                         xfce_panel_plugin_provider_init));



static void
xfce_panel_plugin_class_init (XfcePanelPluginClass *klass)
{
  GObjectClass   *gobject_class;
  GtkWidgetClass *gtkwidget_class;

  g_type_class_add_private (klass, sizeof (XfcePanelPluginPrivate));

  klass->construct = NULL;

  gobject_class = G_OBJECT_CLASS (klass);
  gobject_class->constructor = xfce_panel_plugin_constructor;
  gobject_class->get_property = xfce_panel_plugin_get_property;
  gobject_class->set_property = xfce_panel_plugin_set_property;
  gobject_class->dispose = xfce_panel_plugin_dispose;
  gobject_class->finalize = xfce_panel_plugin_finalize;

  gtkwidget_class = GTK_WIDGET_CLASS (klass);
  gtkwidget_class->realize = xfce_panel_plugin_realize;
  gtkwidget_class->button_press_event = xfce_panel_plugin_button_press_event;

  /**
   * XfcePanelPlugin::about
   * @plugin : an #XfcePanelPlugin.
   *
   * This signal is emmitted when the About entry in the right-click
   * menu is clicked. Plugin writes can use it to show information
   * about the plugin and display credits of the developers, translators
   * and other contributors.
   *
   * See also: xfce_panel_plugin_menu_show_about().
   **/
  plugin_signals[ABOUT] =
    g_signal_new (g_intern_static_string ("about"),
                  G_TYPE_FROM_CLASS (klass),
                  G_SIGNAL_RUN_LAST,
                  G_STRUCT_OFFSET (XfcePanelPluginClass, about),
                  NULL, NULL,
                  g_cclosure_marshal_VOID__VOID,
                  G_TYPE_NONE, 0);

  /**
   * XfcePanelPlugin::configure-plugin
   * @plugin : an #XfcePanelPlugin.
   *
   * This signal is emmitted when the Properties entry in the right-click
   * menu is clicked. Plugin writes can use this signal to open a
   * plugin settings dialog.
   *
   * See also: xfce_panel_plugin_menu_show_configure() and
   *           xfce_titled_dialog_new ().
   **/
  plugin_signals[CONFIGURE_PLUGIN] =
    g_signal_new (g_intern_static_string ("configure-plugin"),
                  G_TYPE_FROM_CLASS (klass),
                  G_SIGNAL_RUN_LAST,
                  G_STRUCT_OFFSET (XfcePanelPluginClass, configure_plugin),
                  NULL, NULL,
                  g_cclosure_marshal_VOID__VOID,
                  G_TYPE_NONE, 0);

  /**
   * XfcePanelPlugin::free-data
   * @plugin : an #XfcePanelPlugin.
   *
   * This signal is emmitted when the plugin is closing. Plugin
   * writers should use this signal to free any allocated resources.
   *
   * See also #XfceHVBox.
   **/
  plugin_signals[FREE_DATA] =
    g_signal_new (g_intern_static_string ("free-data"),
                  G_TYPE_FROM_CLASS (klass),
                  G_SIGNAL_RUN_LAST,
                  G_STRUCT_OFFSET (XfcePanelPluginClass, free_data),
                  NULL, NULL,
                  g_cclosure_marshal_VOID__VOID,
                  G_TYPE_NONE, 0);

  /**
   * XfcePanelPlugin::orientation-changed
   * @plugin      : an #XfcePanelPlugin.
   * @orientation : new #GtkOrientation of the panel.
   *
   * This signal is emmitted whenever the orientation of the panel
   * the @plugin is on changes. Plugins writers can for example use
   * this signal to change the order of widgets in the plugin.
   *
   * See also: #XfceHVBox.
   **/
  plugin_signals[ORIENTATION_CHANGED] =
    g_signal_new (g_intern_static_string ("orientation-changed"),
                  G_TYPE_FROM_CLASS (klass),
                  G_SIGNAL_RUN_LAST,
                  G_STRUCT_OFFSET (XfcePanelPluginClass, orientation_changed),
                  NULL, NULL,
                  g_cclosure_marshal_VOID__ENUM,
                  G_TYPE_NONE, 1, GTK_TYPE_ORIENTATION);

  /**
   * XfcePanelPlugin::remote-event
   * @plugin : an #XfcePanelPlugin.
   * @name   : name of the signal.
   * @value  : value of the signal.
   *
   * This signal is emmitted by the user by running
   * xfce4-panel --plugin-event=plugin-name:name:type:value. It can be
   * used for remote communication, like for example to popup a menu.
   *
   * Returns: %TRUE to stop signal emission to other plugins, %FALSE
   *          to send the signal also to other plugins with the same
   *          name.
   **/
  plugin_signals[REMOTE_EVENT] =
    g_signal_new (g_intern_static_string ("remote-event"),
                  G_TYPE_FROM_CLASS (klass),
                  G_SIGNAL_RUN_LAST,
                  G_STRUCT_OFFSET (XfcePanelPluginClass, remote_event),
                  NULL, NULL,
                  _libxfce4panel_marshal_BOOLEAN__STRING_BOXED,
                  G_TYPE_BOOLEAN, 2, G_TYPE_STRING, G_TYPE_VALUE);

  /**
   * XfcePanelPlugin::removed
   * @plugin : an #XfcePanelPlugin.
   *
   * This signal is emmitted when the plugin is permanently removed from
   * the panel configuration by the user. Developers can use this signal
   * to cleanup custom setting locations that for example store passwords.
   *
   * The free-data signal is emitted after this signal!
   *
   * Note that if you use the xfconf channel and base property provided
   * by xfce_panel_plugin_get_property_base() or the rc file location
   * returned by xfce_panel_plugin_save_location(), the panel will take
   * care of removing those settings.
   *
   * Since: 4.8
   **/
  plugin_signals[REMOVED] =
    g_signal_new (g_intern_static_string ("removed"),
                  G_TYPE_FROM_CLASS (klass),
                  G_SIGNAL_RUN_LAST,
                  G_STRUCT_OFFSET (XfcePanelPluginClass, removed),
                  NULL, NULL,
                  g_cclosure_marshal_VOID__VOID,
                  G_TYPE_NONE, 0);

  /**
   * XfcePanelPlugin::save
   * @plugin : an #XfcePanelPlugin.
   *
   * This signal is emitted when the plugin should save it's
   * configuration. The signal is always emmitted before the plugin
   * closes (before the "free-data" signal) and also once in 10
   * minutes or so.
   *
   * See also: xfce_panel_plugin_save_location().
   **/
  plugin_signals[SAVE] =
    g_signal_new (g_intern_static_string ("save"),
                  G_TYPE_FROM_CLASS (klass),
                  G_SIGNAL_RUN_LAST,
                  G_STRUCT_OFFSET (XfcePanelPluginClass, save),
                  NULL, NULL,
                  g_cclosure_marshal_VOID__VOID,
                  G_TYPE_NONE, 0);

  /**
   * XfcePanelPlugin::size-changed
   * @plugin : an #XfcePanelPlugin.
   * @size   : the new size of the panel.
   *
   * This signal is emmitted whenever the size of the panel
   * the @plugin is on changes. Plugins writers can for example use
   * this signal to update their icon size.
   *
   * If the function returns %FALSE or is not used, the panel will force
   * a square size to the plugin. If you want non-square plugins and you
   * don't need this signal you can use something like this:
   *
   * g_signal_connect (plugin, "size-changed", G_CALLBACK (gtk_true), NULL);
   **/
  plugin_signals[SIZE_CHANGED] =
    g_signal_new (g_intern_static_string ("size-changed"),
                  G_TYPE_FROM_CLASS (klass),
                  G_SIGNAL_RUN_LAST,
                  G_STRUCT_OFFSET (XfcePanelPluginClass, size_changed),
                  g_signal_accumulator_true_handled, NULL,
                  _libxfce4panel_marshal_BOOLEAN__INT,
                  G_TYPE_BOOLEAN, 1, G_TYPE_INT);

  /**
   * XfcePanelPlugin::screen-position-changed
   * @plugin   : an #XfcePanelPlugin.
   * @position : the new #XfceScreenPosition of the panel.
   *
   * This signal is emmitted whenever the screen position of the panel
   * the @plugin is on changes. Plugins writers can for example use
   * this signal to change the arrow direction of buttons.
   **/
  plugin_signals[SCREEN_POSITION_CHANGED] =
    g_signal_new (g_intern_static_string ("screen-position-changed"),
                  G_TYPE_FROM_CLASS (klass),
                  G_SIGNAL_RUN_LAST,
                  G_STRUCT_OFFSET (XfcePanelPluginClass, screen_position_changed),
                  NULL, NULL,
                  g_cclosure_marshal_VOID__ENUM,
                  G_TYPE_NONE, 1, XFCE_TYPE_SCREEN_POSITION);

  /**
   * XfcePanelPlugin:name:
   *
   * The internal, unstranslated, name of the #XfcePanelPlugin. Plugin
   * writer can use it to read the plugin name, but
   * xfce_panel_plugin_get_name() is recommended since that returns
   * a const string.
   **/
  g_object_class_install_property (gobject_class,
                                   PROP_NAME,
                                   g_param_spec_string ("name",
                                                        "Name",
                                                        "Plugin internal name",
                                                        NULL,
                                                        G_PARAM_READWRITE
                                                        | G_PARAM_STATIC_STRINGS
                                                        | G_PARAM_CONSTRUCT_ONLY));

  /**
   * XfcePanelPlugin:display-name:
   *
   * The translated display name of the #XfcePanelPlugin. This property is set
   * during plugin construction and can't be set twice. Plugin writer can use
   * it to read the plugin display name, but xfce_panel_plugin_get_display_name()
   * is recommended.
   **/
  g_object_class_install_property (gobject_class,
                                   PROP_DISPLAY_NAME,
                                   g_param_spec_string ("display-name",
                                                        "Display Name",
                                                        "Plugin display name",
                                                        NULL,
                                                        G_PARAM_READWRITE
                                                        | G_PARAM_STATIC_STRINGS
                                                        | G_PARAM_CONSTRUCT_ONLY));

  /**
   * XfcePanelPlugin:comment:
   *
   * The translated description of the #XfcePanelPlugin. This property is set
   * during plugin construction and can't be set twice. Plugin writer can use
   * it to read the plugin description, but xfce_panel_plugin_get_comment()
   * is recommended.
   *
   * Since: 4.8
   **/
  g_object_class_install_property (gobject_class,
                                   PROP_COMMENT,
                                   g_param_spec_string ("comment",
                                                        "Comment",
                                                        "Plugin comment",
                                                        NULL,
                                                        G_PARAM_READWRITE
                                                        | G_PARAM_STATIC_STRINGS
                                                        | G_PARAM_CONSTRUCT_ONLY));

  /**
   * XfcePanelPlugin:id:
   *
   * The unique id of the #XfcePanelPlugin. This property is set during plugin
   * construction and can't be set twice. Plugin writer can use it to read the
   * plugin display name, but xfce_panel_plugin_get_unique_id() is recommended.
   *
   * Since: 4.8
   **/
  g_object_class_install_property (gobject_class,
                                   PROP_UNIQUE_ID,
                                   g_param_spec_int ("unique-id",
                                                     "Unique ID",
                                                     "Unique plugin ID",
                                                     -1, G_MAXINT, -1,
                                                     G_PARAM_READWRITE
                                                     | G_PARAM_STATIC_STRINGS
                                                     | G_PARAM_CONSTRUCT_ONLY));

  /**
   * XfcePanelPlugin:arguments:
   *
   * The arguments the plugin was started with. If the plugin was not
   * started with any arguments this value is %NULL. Plugin writer can
   * use it to read the arguments array, but
   * xfce_panel_plugin_get_arguments() is recommended.
   **/
  g_object_class_install_property (gobject_class,
                                   PROP_ARGUMENTS,
                                   g_param_spec_boxed ("arguments",
                                                       "Arguments",
                                                       "Startup arguments for the plugin",
                                                       G_TYPE_STRV,
                                                       G_PARAM_READWRITE
                                                       | G_PARAM_STATIC_STRINGS
                                                       | G_PARAM_CONSTRUCT_ONLY));

  /**
   * XfcePanelPlugin:orientation:
   *
   * The #GtkOrientation of the #XfcePanelPlugin. Plugin writer can use it to read the
   * plugin orientation, but xfce_panel_plugin_get_orientation() is recommended.
   **/
  g_object_class_install_property (gobject_class,
                                   PROP_ORIENTATION,
                                   g_param_spec_enum ("orientation",
                                                      "Orientation",
                                                      "Orientation of the plugin's panel",
                                                      GTK_TYPE_ORIENTATION,
                                                      GTK_ORIENTATION_HORIZONTAL,
                                                      G_PARAM_READABLE
                                                      | G_PARAM_STATIC_STRINGS));

  /**
   * XfcePanelPlugin:size:
   *
   * The size in pixels of the #XfcePanelPlugin. Plugin writer can use it to read the
   * plugin size, but xfce_panel_plugin_get_size() is recommended.
   **/
  g_object_class_install_property (gobject_class,
                                   PROP_SIZE,
                                   g_param_spec_int ("size",
                                                     "Size",
                                                     "Size of the plugin's panel",
                                                     0, 128, 0,
                                                     G_PARAM_READABLE
                                                     | G_PARAM_STATIC_STRINGS));

  /**
   * XfcePanelPlugin:screen-position:
   *
   * The #XfceScreenPosition of the #XfcePanelPlugin. Plugin writer can use it
   * to read the plugin's screen position, but xfce_panel_plugin_get_screen_psotion()
   * is recommended.
   **/
  g_object_class_install_property (gobject_class,
                                   PROP_SCREEN_POSITION,
                                   g_param_spec_enum  ("screen-position",
                                                       "Screen Position",
                                                       "Screen position of the plugin's panel",
                                                       XFCE_TYPE_SCREEN_POSITION,
                                                       XFCE_SCREEN_POSITION_NONE,
                                                       G_PARAM_READABLE
                                                       | G_PARAM_STATIC_STRINGS));

  /**
   * XfcePanelPlugin:expand:
   *
   * Wether the #XfcePanelPlugin expands on the panel. Plugin writes can use it
   * to read or set this property, but xfce_panel_plugin_set_expand()
   * is recommended.
   **/
  g_object_class_install_property (gobject_class,
                                   PROP_EXPAND,
                                   g_param_spec_boolean ("expand",
                                                         "Expand",
                                                         "Whether this plugin is expanded",
                                                         FALSE,
                                                         G_PARAM_READWRITE
                                                         | G_PARAM_STATIC_STRINGS));

  item_properties = g_quark_from_static_string ("item-properties");
  item_about = g_quark_from_static_string ("item-about");
}



static void
xfce_panel_plugin_init (XfcePanelPlugin *plugin)
{
  plugin->priv = G_TYPE_INSTANCE_GET_PRIVATE (plugin, XFCE_TYPE_PANEL_PLUGIN, XfcePanelPluginPrivate);

  plugin->priv->name = NULL;
  plugin->priv->display_name = NULL;
  plugin->priv->comment = NULL;
  plugin->priv->unique_id = -1;
  plugin->priv->property_base = NULL;
  plugin->priv->arguments = NULL;
  plugin->priv->size = 0;
  plugin->priv->expand = FALSE;
  plugin->priv->orientation = GTK_ORIENTATION_HORIZONTAL;
  plugin->priv->screen_position = XFCE_SCREEN_POSITION_NONE;
  plugin->priv->menu = NULL;
  plugin->priv->menu_blocked = 0;
  plugin->priv->panel_lock = 0;
  plugin->priv->flags = 0;
  plugin->priv->locked = TRUE;
  plugin->priv->menu_items = NULL;

  /* hide the event box window to make the plugin transparent */
  gtk_event_box_set_visible_window (GTK_EVENT_BOX (plugin), FALSE);
}



static void
xfce_panel_plugin_provider_init (XfcePanelPluginProviderInterface *iface)
{
  iface->get_name = (ProviderToPluginChar) xfce_panel_plugin_get_name;
  iface->get_unique_id = (ProviderToPluginInt) xfce_panel_plugin_get_unique_id;
  iface->set_size = xfce_panel_plugin_set_size;
  iface->set_orientation = xfce_panel_plugin_set_orientation;
  iface->set_screen_position = xfce_panel_plugin_set_screen_position;
  iface->save = xfce_panel_plugin_save;
  iface->get_show_configure = xfce_panel_plugin_get_show_configure;
  iface->show_configure = xfce_panel_plugin_show_configure;
  iface->get_show_about = xfce_panel_plugin_get_show_about;
  iface->show_about = xfce_panel_plugin_show_about;
  iface->removed = xfce_panel_plugin_removed;
  iface->remote_event = xfce_panel_plugin_remote_event;
  iface->set_locked = xfce_panel_plugin_set_locked;
}



static GObject *
xfce_panel_plugin_constructor (GType                  type,
                               guint                  n_props,
                               GObjectConstructParam *props)
{
  GObject *plugin;

  plugin = G_OBJECT_CLASS (xfce_panel_plugin_parent_class)->constructor (type, n_props, props);

  /* all the properties are set and can be used in public */
  PANEL_SET_FLAG (XFCE_PANEL_PLUGIN (plugin)->priv->flags, PLUGIN_FLAG_CONSTRUCTED);

  return plugin;
}



static void
xfce_panel_plugin_get_property (GObject    *object,
                                guint       prop_id,
                                GValue     *value,
                                GParamSpec *pspec)
{
  XfcePanelPluginPrivate *private = XFCE_PANEL_PLUGIN (object)->priv;

  switch (prop_id)
    {
    case PROP_NAME:
      g_value_set_static_string (value, private->name);
      break;

    case PROP_DISPLAY_NAME:
      g_value_set_static_string (value, private->display_name);
      break;

    case PROP_COMMENT:
      g_value_set_static_string (value, private->comment);
      break;

    case PROP_UNIQUE_ID:
      g_value_set_int (value, private->unique_id);
      break;

    case PROP_ARGUMENTS:
      g_value_set_boxed (value, private->arguments);
      break;

    case PROP_ORIENTATION:
      g_value_set_enum (value, private->orientation);
      break;

    case PROP_SIZE:
      g_value_set_int (value, private->size);
      break;

    case PROP_SCREEN_POSITION:
      g_value_set_enum (value, private->screen_position);
      break;

    case PROP_EXPAND:
      g_value_set_boolean (value, private->expand);
      break;

    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
      break;
    }
}



static void
xfce_panel_plugin_set_property (GObject      *object,
                                guint         prop_id,
                                const GValue *value,
                                GParamSpec   *pspec)
{
  XfcePanelPluginPrivate *private = XFCE_PANEL_PLUGIN (object)->priv;
  gchar                  *name;

  switch (prop_id)
    {
    case PROP_NAME:
    case PROP_UNIQUE_ID:
      if (prop_id == PROP_NAME)
        private->name = g_value_dup_string (value);
      else
        private->unique_id = g_value_get_int (value);

      if (private->unique_id != -1 && private->name != NULL)
        {
          /* give the widget a unique name for theming */
          name = g_strdup_printf ("%s-%d", private->name, private->unique_id);
          gtk_widget_set_name (GTK_WIDGET (object), name);
          g_free (name);
        }
      break;

    case PROP_DISPLAY_NAME:
      private->display_name = g_value_dup_string (value);
      break;

    case PROP_COMMENT:
      private->comment = g_value_dup_string (value);
      break;

    case PROP_ARGUMENTS:
      private->arguments = g_value_dup_boxed (value);
      break;

    case PROP_EXPAND:
      xfce_panel_plugin_set_expand (XFCE_PANEL_PLUGIN (object),
                                    g_value_get_boolean (value));
      break;

    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
      break;
    }
}



static void
xfce_panel_plugin_dispose (GObject *object)
{
  XfcePanelPlugin *plugin = XFCE_PANEL_PLUGIN (object);

  if (!PANEL_HAS_FLAG (plugin->priv->flags, PLUGIN_FLAG_DISPOSED))
    {
      /* allow the plugin to cleanup */
      g_signal_emit (G_OBJECT (object), plugin_signals[FREE_DATA], 0);

      /* plugin disposed, don't try this again */
      PANEL_SET_FLAG (plugin->priv->flags, PLUGIN_FLAG_DISPOSED);
    }

  (*G_OBJECT_CLASS (xfce_panel_plugin_parent_class)->dispose) (object);
}



static void
xfce_panel_plugin_finalize (GObject *object)
{
  XfcePanelPlugin *plugin = XFCE_PANEL_PLUGIN (object);
  GSList          *li;

  /* destroy the menu */
  if (plugin->priv->menu != NULL)
    {
      gtk_widget_destroy (GTK_WIDGET (plugin->priv->menu));
      panel_assert (plugin->priv->menu_items == NULL);
    }
  else
    {
      /* release custom menu items */
      for (li = plugin->priv->menu_items; li != NULL; li = li->next)
        {
          g_signal_handlers_disconnect_by_func (G_OBJECT (li->data),
              G_CALLBACK (xfce_panel_plugin_menu_item_destroy), plugin);
          g_object_unref (G_OBJECT (li->data));
        }
      g_slist_free (plugin->priv->menu_items);
    }

  g_free (plugin->priv->name);
  g_free (plugin->priv->display_name);
  g_free (plugin->priv->comment);
  g_free (plugin->priv->property_base);
  g_strfreev (plugin->priv->arguments);

  (*G_OBJECT_CLASS (xfce_panel_plugin_parent_class)->finalize) (object);
}



static void
xfce_panel_plugin_realize (GtkWidget *widget)
{
  XfcePanelPluginClass *klass;
  XfcePanelPlugin      *plugin = XFCE_PANEL_PLUGIN (widget);

  /* let gtk realize the plugin */
  (*GTK_WIDGET_CLASS (xfce_panel_plugin_parent_class)->realize) (widget);

  /* launch the construct function for object oriented plugins, but
   * do this only once */
  if (!PANEL_HAS_FLAG (plugin->priv->flags, PLUGIN_FLAG_REALIZED))
    {
      PANEL_SET_FLAG (plugin->priv->flags, PLUGIN_FLAG_REALIZED);

      /* whether this is an object plugin */
      klass = XFCE_PANEL_PLUGIN_GET_CLASS (widget);
      if (klass->construct != NULL)
        (*klass->construct) (XFCE_PANEL_PLUGIN (widget));
    }
}



static gboolean
xfce_panel_plugin_button_press_event (GtkWidget      *widget,
                                      GdkEventButton *event)
{
  XfcePanelPlugin *plugin = XFCE_PANEL_PLUGIN (widget);
  guint            modifiers;
  GtkMenu         *menu;
  GtkWidget       *item;

  panel_return_val_if_fail (XFCE_IS_PANEL_PLUGIN (widget), FALSE);

  /* get the default accelerator modifier mask */
  modifiers = event->state & gtk_accelerator_get_default_mod_mask ();

  if (event->button == 3
      || (event->button == 1 && modifiers == GDK_CONTROL_MASK))
    {
      /* get the panel menu */
      menu = xfce_panel_plugin_menu_get (plugin);

      /* if the menu is block, some items are insensitive */
      item = g_object_get_qdata (G_OBJECT (menu), item_properties);
      if (item != NULL)
        gtk_widget_set_sensitive (item, plugin->priv->menu_blocked == 0);

      /* popup the menu */
      gtk_menu_popup (menu, NULL, NULL, NULL, NULL, event->button, event->time);

      return TRUE;
    }

  return FALSE;
}



static void
xfce_panel_plugin_menu_move (XfcePanelPlugin *plugin)
{
  panel_return_if_fail (XFCE_IS_PANEL_PLUGIN (plugin));
  panel_return_if_fail (XFCE_IS_PANEL_PLUGIN_PROVIDER (plugin));

  /* move the plugin */
  xfce_panel_plugin_provider_emit_signal (XFCE_PANEL_PLUGIN_PROVIDER (plugin),
                                          PROVIDER_SIGNAL_MOVE_PLUGIN);
}



static void
xfce_panel_plugin_menu_remove (XfcePanelPlugin *plugin)
{
  GtkWidget *dialog;

  panel_return_if_fail (XFCE_IS_PANEL_PLUGIN (plugin));

  /* create question dialog (same code is also in panel-preferences-dialog.c) */
  dialog = gtk_message_dialog_new (NULL, GTK_DIALOG_MODAL,
      GTK_MESSAGE_QUESTION, GTK_BUTTONS_NONE,
      /* I18N: %s is the name of the plugin */
      _("Are you sure that you want to remove \"%s\"?"),
      xfce_panel_plugin_get_display_name (plugin));
  gtk_window_set_screen (GTK_WINDOW (dialog),
      gtk_widget_get_screen (GTK_WIDGET (plugin)));
  gtk_message_dialog_format_secondary_text (GTK_MESSAGE_DIALOG (dialog),
      _("If you remove the item from the panel, it is permanently lost."));
  gtk_dialog_add_buttons (GTK_DIALOG (dialog), GTK_STOCK_CANCEL,
      GTK_RESPONSE_NO, GTK_STOCK_REMOVE, GTK_RESPONSE_YES, NULL);
  gtk_dialog_set_default_response (GTK_DIALOG (dialog), GTK_RESPONSE_NO);

  if (gtk_dialog_run (GTK_DIALOG (dialog)) == GTK_RESPONSE_YES)
    {
      gtk_widget_hide (dialog);

      /* ask the panel or wrapper to remove the plugin */
      xfce_panel_plugin_remove (plugin);
    }

  gtk_widget_destroy (dialog);
}



static void
xfce_panel_plugin_menu_add_items (XfcePanelPlugin *plugin)
{
  panel_return_if_fail (XFCE_IS_PANEL_PLUGIN (plugin));
  panel_return_if_fail (XFCE_IS_PANEL_PLUGIN_PROVIDER (plugin));
  panel_return_if_fail (XFCE_PANEL_PLUGIN_CONSTRUCTED (plugin));

  /* open items dialog */
  if (!xfce_panel_plugin_get_locked (plugin))
    xfce_panel_plugin_provider_emit_signal (XFCE_PANEL_PLUGIN_PROVIDER (plugin),
                                            PROVIDER_SIGNAL_ADD_NEW_ITEMS);
}



static void
xfce_panel_plugin_menu_panel_preferences (XfcePanelPlugin *plugin)
{
  panel_return_if_fail (XFCE_IS_PANEL_PLUGIN (plugin));
  panel_return_if_fail (XFCE_IS_PANEL_PLUGIN_PROVIDER (plugin));
  panel_return_if_fail (XFCE_PANEL_PLUGIN_CONSTRUCTED (plugin));

  /* open preferences dialog */
  if (!xfce_panel_plugin_get_locked (plugin))
    xfce_panel_plugin_provider_emit_signal (XFCE_PANEL_PLUGIN_PROVIDER (plugin),
                                            PROVIDER_SIGNAL_PANEL_PREFERENCES);
}



static void
xfce_panel_plugin_menu_panel_logout (XfcePanelPlugin *plugin)
{
  panel_return_if_fail (XFCE_IS_PANEL_PLUGIN (plugin));
  panel_return_if_fail (XFCE_IS_PANEL_PLUGIN_PROVIDER (plugin));
  panel_return_if_fail (XFCE_PANEL_PLUGIN_CONSTRUCTED (plugin));

  /* logout the session */
  xfce_panel_plugin_provider_emit_signal (XFCE_PANEL_PLUGIN_PROVIDER (plugin),
                                          PROVIDER_SIGNAL_PANEL_LOGOUT);
}



static void
xfce_panel_plugin_menu_panel_about (XfcePanelPlugin *plugin)
{
  panel_return_if_fail (XFCE_IS_PANEL_PLUGIN (plugin));
  panel_return_if_fail (XFCE_IS_PANEL_PLUGIN_PROVIDER (plugin));
  panel_return_if_fail (XFCE_PANEL_PLUGIN_CONSTRUCTED (plugin));

  /* open the about dialog of the panel */
  xfce_panel_plugin_provider_emit_signal (XFCE_PANEL_PLUGIN_PROVIDER (plugin),
                                          PROVIDER_SIGNAL_PANEL_ABOUT);
}



static void
xfce_panel_plugin_menu_panel_help (XfcePanelPlugin *plugin)
{
  panel_return_if_fail (XFCE_IS_PANEL_PLUGIN (plugin));
  panel_return_if_fail (XFCE_IS_PANEL_PLUGIN_PROVIDER (plugin));
  panel_return_if_fail (XFCE_PANEL_PLUGIN_CONSTRUCTED (plugin));

  /* open the manual of the panel */
  xfce_panel_plugin_provider_emit_signal (XFCE_PANEL_PLUGIN_PROVIDER (plugin),
                                          PROVIDER_SIGNAL_PANEL_HELP);
}



static void
xfce_panel_plugin_menu_destroy (XfcePanelPlugin *plugin)
{
  GSList *li;

  panel_return_if_fail (XFCE_IS_PANEL_PLUGIN (plugin));

  if (plugin->priv->menu != NULL)
    {
      /* remove custom items before they get destroyed */
      for (li = plugin->priv->menu_items; li != NULL; li = li->next)
        gtk_container_remove (GTK_CONTAINER (plugin->priv->menu), GTK_WIDGET (li->data));

      gtk_widget_destroy (GTK_WIDGET (plugin->priv->menu));
      plugin->priv->menu = NULL;
    }
}



static GtkMenu *
xfce_panel_plugin_menu_get (XfcePanelPlugin *plugin)
{
  GtkWidget *menu, *submenu;
  GtkWidget *item;
  GtkWidget *image;
  gboolean   locked;
  GSList    *li;

  panel_return_val_if_fail (XFCE_IS_PANEL_PLUGIN (plugin), NULL);

  if (G_UNLIKELY (plugin->priv->menu == NULL))
    {
      locked = xfce_panel_plugin_get_locked (plugin);

      menu = gtk_menu_new ();
      gtk_menu_attach_to_widget (GTK_MENU (menu), GTK_WIDGET (plugin), NULL);

      /* item with plugin name */
      item = gtk_menu_item_new_with_label (xfce_panel_plugin_get_display_name (plugin));
      gtk_menu_shell_append (GTK_MENU_SHELL (menu), item);
      gtk_widget_set_sensitive (item, FALSE);
      gtk_widget_show (item);

      /* separator */
      item = gtk_separator_menu_item_new ();
      gtk_menu_shell_append (GTK_MENU_SHELL (menu), item);
      gtk_widget_show (item);

      if (!locked)
        {
          /* properties item */
          item = gtk_image_menu_item_new_from_stock (GTK_STOCK_PROPERTIES, NULL);
          g_signal_connect_swapped (G_OBJECT (item), "activate",
              G_CALLBACK (xfce_panel_plugin_show_configure), plugin);
          g_object_set_qdata (G_OBJECT (menu), item_properties, item);
          gtk_menu_shell_append (GTK_MENU_SHELL (menu), item);
          if (PANEL_HAS_FLAG (plugin->priv->flags, PLUGIN_FLAG_SHOW_CONFIGURE))
            gtk_widget_show (item);

          /* about item */
          item = gtk_image_menu_item_new_from_stock (GTK_STOCK_ABOUT, NULL);
          g_signal_connect_swapped (G_OBJECT (item), "activate",
              G_CALLBACK (xfce_panel_plugin_show_about), plugin);
          g_object_set_qdata (G_OBJECT (menu), item_about, item);
          gtk_menu_shell_append (GTK_MENU_SHELL (menu), item);
          if (PANEL_HAS_FLAG (plugin->priv->flags, PLUGIN_FLAG_SHOW_ABOUT))
            gtk_widget_show (item);

          /* move item */
          item = gtk_image_menu_item_new_with_mnemonic (_("_Move"));
          g_signal_connect_swapped (G_OBJECT (item), "activate",
              G_CALLBACK (xfce_panel_plugin_menu_move), plugin);
          gtk_menu_shell_append (GTK_MENU_SHELL (menu), item);
          gtk_widget_show (item);

          image = gtk_image_new_from_stock (GTK_STOCK_GO_FORWARD, GTK_ICON_SIZE_MENU);
          gtk_image_menu_item_set_image (GTK_IMAGE_MENU_ITEM (item), image);
          gtk_widget_show (image);

          /* add custom menu items */
          for (li = plugin->priv->menu_items; li != NULL; li = li->next)
            gtk_menu_shell_append (GTK_MENU_SHELL (menu), GTK_WIDGET (li->data));

          /* separator */
          item = gtk_separator_menu_item_new ();
          gtk_menu_shell_append (GTK_MENU_SHELL (menu), item);
          gtk_widget_show (item);

          /* remove */
          item = gtk_image_menu_item_new_from_stock (GTK_STOCK_REMOVE, NULL);
          g_signal_connect_swapped (G_OBJECT (item), "activate",
              G_CALLBACK (xfce_panel_plugin_menu_remove), plugin);
          gtk_menu_shell_append (GTK_MENU_SHELL (menu), item);
          gtk_widget_show (item);

          /* separator */
          item = gtk_separator_menu_item_new ();
          gtk_menu_shell_append (GTK_MENU_SHELL (menu), item);
          gtk_widget_show (item);
        }

      /* create a panel submenu item */
      submenu = gtk_menu_new ();
      item = gtk_menu_item_new_with_mnemonic ("_Xfce Panel");
      gtk_menu_shell_append (GTK_MENU_SHELL (menu), item);
      gtk_menu_item_set_submenu (GTK_MENU_ITEM (item), submenu);
      gtk_widget_show (item);

      if (!locked)
        {
          /* add new items */
          item = gtk_image_menu_item_new_with_mnemonic (_("Add _New Items..."));
          g_signal_connect_swapped (G_OBJECT (item), "activate",
              G_CALLBACK (xfce_panel_plugin_menu_add_items), plugin);
          gtk_menu_shell_append (GTK_MENU_SHELL (submenu), item);
          gtk_widget_show (item);

          image = gtk_image_new_from_stock (GTK_STOCK_ADD, GTK_ICON_SIZE_MENU);
          gtk_image_menu_item_set_image (GTK_IMAGE_MENU_ITEM (item), image);
          gtk_widget_show (image);

          /* customize panel */
          item = gtk_image_menu_item_new_with_mnemonic (_("Panel Pr_eferences..."));
          g_signal_connect_swapped (G_OBJECT (item), "activate",
              G_CALLBACK (xfce_panel_plugin_menu_panel_preferences), plugin);
          gtk_menu_shell_append (GTK_MENU_SHELL (submenu), item);
          gtk_widget_show (item);

          image = gtk_image_new_from_stock (GTK_STOCK_PREFERENCES, GTK_ICON_SIZE_MENU);
          gtk_image_menu_item_set_image (GTK_IMAGE_MENU_ITEM (item), image);
          gtk_widget_show (image);

          /* separator */
          item = gtk_separator_menu_item_new ();
          gtk_menu_shell_append (GTK_MENU_SHELL (submenu), item);
          gtk_widget_show (item);
        }

      /* logout item */
      item = gtk_image_menu_item_new_with_mnemonic (_("Log _Out"));
      g_signal_connect_swapped (G_OBJECT (item), "activate",
          G_CALLBACK (xfce_panel_plugin_menu_panel_logout), plugin);
      gtk_menu_shell_append (GTK_MENU_SHELL (submenu), item);
      gtk_widget_show (item);

      image = gtk_image_new_from_icon_name ("system-log-out", GTK_ICON_SIZE_MENU);
      gtk_image_menu_item_set_image (GTK_IMAGE_MENU_ITEM (item), image);
      gtk_widget_show (image);

      /* separator */
      item = gtk_separator_menu_item_new ();
      gtk_menu_shell_append (GTK_MENU_SHELL (submenu), item);
      gtk_widget_show (item);

      /* help item */
      item = gtk_image_menu_item_new_from_stock (GTK_STOCK_HELP, NULL);
      g_signal_connect_swapped (G_OBJECT (item), "activate",
          G_CALLBACK (xfce_panel_plugin_menu_panel_help), plugin);
      gtk_menu_shell_append (GTK_MENU_SHELL (submenu), item);
      gtk_widget_show (item);

      /* about item */
      item = gtk_image_menu_item_new_from_stock (GTK_STOCK_ABOUT, NULL);
      g_signal_connect_swapped (G_OBJECT (item), "activate",
          G_CALLBACK (xfce_panel_plugin_menu_panel_about), plugin);
      gtk_menu_shell_append (GTK_MENU_SHELL (submenu), item);
      gtk_widget_show (item);

      /* set panel menu */
      plugin->priv->menu = GTK_MENU (menu);
    }

  /* block autohide when this menu is shown */
  xfce_panel_plugin_register_menu (plugin, GTK_MENU (plugin->priv->menu));

  return plugin->priv->menu;
}



static inline gchar *
xfce_panel_plugin_relative_filename (XfcePanelPlugin *plugin)
{
  panel_return_val_if_fail (XFCE_IS_PANEL_PLUGIN (plugin), NULL);
  panel_return_val_if_fail (xfce_panel_plugin_get_name (plugin) != NULL, NULL);
  panel_return_val_if_fail (xfce_panel_plugin_get_unique_id (plugin) != -1, NULL);

  /* return the relative configuration filename */
  return g_strdup_printf (PANEL_PLUGIN_RC_RELATIVE_PATH,
                          plugin->priv->name, plugin->priv->unique_id);
}



static void
xfce_panel_plugin_unregister_menu (GtkMenu         *menu,
                                   XfcePanelPlugin *plugin)
{
  panel_return_if_fail (XFCE_IS_PANEL_PLUGIN (plugin));
  panel_return_if_fail (plugin->priv->panel_lock > 0);
  panel_return_if_fail (GTK_IS_MENU (menu));

  /* disconnect this signal */
  g_signal_handlers_disconnect_by_func (G_OBJECT (menu),
      G_CALLBACK (xfce_panel_plugin_unregister_menu), plugin);

  if (G_LIKELY (plugin->priv->panel_lock > 0))
    {
      /* decrease the counter */
      plugin->priv->panel_lock--;

      /* emit signal to unlock the panel */
      if (plugin->priv->panel_lock == 0)
        xfce_panel_plugin_provider_emit_signal (XFCE_PANEL_PLUGIN_PROVIDER (plugin),
                                                PROVIDER_SIGNAL_UNLOCK_PANEL);
    }
}



static void
xfce_panel_plugin_set_size (XfcePanelPluginProvider *provider,
                            gint                     size)
{
  XfcePanelPlugin *plugin = XFCE_PANEL_PLUGIN (provider);
  gboolean         handled = FALSE;

  panel_return_if_fail (XFCE_IS_PANEL_PLUGIN (provider));

  /* check if update is required */
  if (G_LIKELY (plugin->priv->size != size))
    {
      plugin->priv->size = size;

      g_signal_emit (G_OBJECT (plugin),
                     plugin_signals[SIZE_CHANGED], 0, size, &handled);

      /* handle the size when not done by the plugin */
      if (!handled)
        gtk_widget_set_size_request (GTK_WIDGET (plugin), size, size);

      g_object_notify (G_OBJECT (plugin), "size");
    }
}



static void
xfce_panel_plugin_set_orientation (XfcePanelPluginProvider *provider,
                                   GtkOrientation           orientation)
{
  XfcePanelPlugin *plugin = XFCE_PANEL_PLUGIN (provider);

  panel_return_if_fail (XFCE_IS_PANEL_PLUGIN (provider));

  /* check if update is required */
  if (G_LIKELY (plugin->priv->orientation != orientation))
    {
      plugin->priv->orientation = orientation;

      g_signal_emit (G_OBJECT (plugin),
                     plugin_signals[ORIENTATION_CHANGED], 0, orientation);

      g_object_notify (G_OBJECT (plugin), "orientation");
    }
}



static void
xfce_panel_plugin_set_screen_position (XfcePanelPluginProvider *provider,
                                       XfceScreenPosition       screen_position)
{
  XfcePanelPlugin *plugin = XFCE_PANEL_PLUGIN (provider);

  panel_return_if_fail (XFCE_IS_PANEL_PLUGIN (provider));

  /* check if update is required */
  if (G_LIKELY (plugin->priv->screen_position != screen_position
      || xfce_screen_position_is_floating (screen_position)))
    {
      plugin->priv->screen_position = screen_position;

      g_signal_emit (G_OBJECT (plugin),
                     plugin_signals[SCREEN_POSITION_CHANGED], 0,
                     screen_position);

      g_object_notify (G_OBJECT (plugin), "screen-position");
    }
}



static void
xfce_panel_plugin_save (XfcePanelPluginProvider *provider)
{
  XfcePanelPlugin *plugin = XFCE_PANEL_PLUGIN (provider);

  panel_return_if_fail (XFCE_IS_PANEL_PLUGIN (provider));

  /* only send the save signal if the plugin is not locked */
  if (XFCE_PANEL_PLUGIN (provider)->priv->menu_blocked == 0
      && !xfce_panel_plugin_get_locked (plugin))
    g_signal_emit (G_OBJECT (provider), plugin_signals[SAVE], 0);
}



static gboolean
xfce_panel_plugin_get_show_configure (XfcePanelPluginProvider *provider)
{
  panel_return_val_if_fail (XFCE_IS_PANEL_PLUGIN (provider), FALSE);

  /* TODO, not sure, but maybe return FALSE when menu_blocked > 0 */

  return PANEL_HAS_FLAG (XFCE_PANEL_PLUGIN (provider)->priv->flags,
                         PLUGIN_FLAG_SHOW_CONFIGURE);
}



static void
xfce_panel_plugin_show_configure (XfcePanelPluginProvider *provider)
{
  XfcePanelPlugin *plugin = XFCE_PANEL_PLUGIN (provider);

  panel_return_if_fail (XFCE_IS_PANEL_PLUGIN (provider));

  if (plugin->priv->menu_blocked == 0
      && !xfce_panel_plugin_get_locked (plugin))
    g_signal_emit (G_OBJECT (plugin), plugin_signals[CONFIGURE_PLUGIN], 0);
}



static gboolean
xfce_panel_plugin_get_show_about (XfcePanelPluginProvider *provider)
{
  panel_return_val_if_fail (XFCE_IS_PANEL_PLUGIN (provider), FALSE);

  /* TODO, not sure, but maybe return FALSE when menu_blocked > 0 */

  return PANEL_HAS_FLAG (XFCE_PANEL_PLUGIN (provider)->priv->flags,
                         PLUGIN_FLAG_SHOW_ABOUT);
}



static void
xfce_panel_plugin_show_about (XfcePanelPluginProvider *provider)
{
  XfcePanelPlugin *plugin = XFCE_PANEL_PLUGIN (provider);

  panel_return_if_fail (XFCE_IS_PANEL_PLUGIN (provider));

  if (G_LIKELY (plugin->priv->menu_blocked == 0))
    g_signal_emit (G_OBJECT (provider), plugin_signals[ABOUT], 0);
}



static void
xfce_panel_plugin_removed (XfcePanelPluginProvider *provider)
{
  panel_return_if_fail (XFCE_IS_PANEL_PLUGIN (provider));

  if (!xfce_panel_plugin_get_locked (XFCE_PANEL_PLUGIN (provider)))
    g_signal_emit (G_OBJECT (provider), plugin_signals[REMOVED], 0);
}



static gboolean
xfce_panel_plugin_remote_event (XfcePanelPluginProvider *provider,
                                const gchar             *name,
                                const GValue            *value,
                                guint                   *handle)
{
  gboolean stop_emission;

  panel_return_val_if_fail (XFCE_IS_PANEL_PLUGIN (provider), TRUE);
  panel_return_val_if_fail (name != NULL, TRUE);
  panel_return_val_if_fail (value == NULL || G_IS_VALUE (value), TRUE);

  g_signal_emit (G_OBJECT (provider), plugin_signals[REMOTE_EVENT], 0,
                 name, value, &stop_emission);

  return stop_emission;
}



static void
xfce_panel_plugin_set_locked (XfcePanelPluginProvider *provider,
                              gboolean                 locked)
{
  XfcePanelPlugin *plugin = XFCE_PANEL_PLUGIN (provider);

  panel_return_if_fail (XFCE_IS_PANEL_PLUGIN (provider));

  if (G_LIKELY (plugin->priv->locked != locked))
    {
      plugin->priv->locked = locked;

      /* destroy the menu if it exists */
      xfce_panel_plugin_menu_destroy (plugin);
    }
}



static void
xfce_panel_plugin_take_window_notify (gpointer  data,
                                      GObject  *where_the_object_was)
{
  panel_return_if_fail (GTK_IS_WINDOW (data) || XFCE_IS_PANEL_PLUGIN (data));

  /* release the opposite weak ref */
  g_object_weak_unref (G_OBJECT (data),
      xfce_panel_plugin_take_window_notify, where_the_object_was);

  /* destroy the dialog if the plugin was finalized */
  if (GTK_IS_WINDOW (data))
    gtk_widget_destroy (GTK_WIDGET (data));
}



static void
xfce_panel_plugin_menu_item_destroy (GtkWidget       *item,
                                     XfcePanelPlugin *plugin)
{
  panel_return_if_fail (XFCE_IS_PANEL_PLUGIN (plugin));
  panel_return_if_fail (GTK_IS_MENU_ITEM (item));
  panel_return_if_fail (g_slist_find (plugin->priv->menu_items, item) != NULL);

  /* remote the item from the list and release it */
  plugin->priv->menu_items = g_slist_remove (plugin->priv->menu_items, item);
  g_object_unref (G_OBJECT (item));
}



/**
 * xfce_panel_plugin_get_name:
 * @plugin : an #XfcePanelPlugin.
 *
 * The internal name of the panel plugin.
 *
 * Returns: the name of the panel plugin.
 **/
G_CONST_RETURN gchar *
xfce_panel_plugin_get_name (XfcePanelPlugin *plugin)
{
  g_return_val_if_fail (XFCE_IS_PANEL_PLUGIN (plugin), NULL);
  g_return_val_if_fail (XFCE_PANEL_PLUGIN_CONSTRUCTED (plugin), NULL);

  return plugin->priv->name;
}



/**
 * xfce_panel_plugin_get_display_name:
 * @plugin : an #XfcePanelPlugin.
 *
 * This returns the translated name of the plugin set in the .desktop
 * file of the plugin.
 *
 * Returns: the (translated) display name of the plugin.
 **/
G_CONST_RETURN gchar *
xfce_panel_plugin_get_display_name (XfcePanelPlugin *plugin)
{
  g_return_val_if_fail (XFCE_IS_PANEL_PLUGIN (plugin), NULL);
  g_return_val_if_fail (XFCE_PANEL_PLUGIN_CONSTRUCTED (plugin), NULL);

  if (G_LIKELY (plugin->priv->display_name))
    return plugin->priv->display_name;

  return plugin->priv->name;
}



/**
 * xfce_panel_plugin_get_comment:
 * @plugin : an #XfcePanelPlugin.
 *
 * This returns the translated comment of the plugin set in
 * the .desktop file of the plugin.
 *
 * Returns: the (translated) comment of the plugin.
 *
 * Since: 4.8
 **/
G_CONST_RETURN gchar *
xfce_panel_plugin_get_comment (XfcePanelPlugin *plugin)
{
  g_return_val_if_fail (XFCE_IS_PANEL_PLUGIN (plugin), NULL);
  g_return_val_if_fail (XFCE_PANEL_PLUGIN_CONSTRUCTED (plugin), NULL);

  return plugin->priv->comment;
}



/**
 * xfce_panel_plugin_get_unique_id:
 * @plugin : an #XfcePanelPlugin.
 *
 * The internal unique id of the plugin. Each plugin in the panel has
 * a unique number that is for example used for the config file name
 * or property base in the xfconf channel.
 *
 * Returns: the unique id of the plugin.
 *
 * Since 4.8
 **/
gint
xfce_panel_plugin_get_unique_id (XfcePanelPlugin *plugin)
{
  g_return_val_if_fail (XFCE_IS_PANEL_PLUGIN (plugin), -1);
  g_return_val_if_fail (XFCE_PANEL_PLUGIN_CONSTRUCTED (plugin), -1);

  return plugin->priv->unique_id;
}



/**
 * xfce_panel_plugin_get_property_base:
 * @plugin : an #XfcePanelPlugin.
 *
 * The property base for this plugin in the xfce4-panel XfconfChannel,
 * this name is something like /plugins/plugin-1.
 *
 * Returns: the property base for the xfconf channel userd by a plugin.
 *
 * See also: xfconf_channel_new_with_property_base() and
 *           XFCE_PANEL_PLUGIN_CHANNEL_NAME.
 **/
G_CONST_RETURN gchar *
xfce_panel_plugin_get_property_base (XfcePanelPlugin *plugin)
{
  g_return_val_if_fail (XFCE_IS_PANEL_PLUGIN (plugin), NULL);
  g_return_val_if_fail (XFCE_PANEL_PLUGIN_CONSTRUCTED (plugin), NULL);

  /* create the propert if needed */
  if (plugin->priv->property_base == NULL)
    plugin->priv->property_base = g_strdup_printf (PANEL_PLUGIN_PROPERTY_BASE,
                                                   plugin->priv->unique_id);

  return plugin->priv->property_base;
}



/**
 * xfce_panel_plugin_get_arguments:
 * @plugin : an #XfcePanelPlugin.
 *
 * Argument vector passed to the plugin when it was added. Most of the
 * time the return value will be %NULL, but if could for example contain
 * a list of filenames when the user added the plugin with
 *
 * xfce4-panel --add=launcher *.desktop
 *
 * see the code of the launcher plugin how to use this.
 *
 * Returns: the argument vector. The vector is owned by the plugin and
 *          should not be freed.
 *
 * Since: 4.8
 **/
G_CONST_RETURN gchar * G_CONST_RETURN *
xfce_panel_plugin_get_arguments (XfcePanelPlugin *plugin)
{
  g_return_val_if_fail (XFCE_IS_PANEL_PLUGIN (plugin), NULL);
  g_return_val_if_fail (XFCE_PANEL_PLUGIN_CONSTRUCTED (plugin), NULL);

  return (const gchar * const *) plugin->priv->arguments;
}



/**
 * xfce_panel_plugin_get_size:
 * @plugin : an #XfcePanelPlugin.
 *
 * The size of the panel in which the plugin is embedded.
 *
 * Returns: the current size of the panel.
 **/
gint
xfce_panel_plugin_get_size (XfcePanelPlugin *plugin)
{
  g_return_val_if_fail (XFCE_IS_PANEL_PLUGIN (plugin), -1);
  g_return_val_if_fail (XFCE_PANEL_PLUGIN_CONSTRUCTED (plugin), -1);

  /* always return a 'positive' size that makes sence */
  return MAX (16, plugin->priv->size);
}



/**
 * xfce_panel_plugin_get_expand:
 * @plugin : an #XfcePanelPlugin.
 *
 * Whether the plugin is expanded or not. This set by the plugin using
 * xfce_panel_plugin_set_expand().
 *
 * Returns: %TRUE when the plugin should expand,
 *          %FALSE otherwise.
 **/
gboolean
xfce_panel_plugin_get_expand (XfcePanelPlugin *plugin)
{
  g_return_val_if_fail (XFCE_IS_PANEL_PLUGIN (plugin), FALSE);
  g_return_val_if_fail (XFCE_PANEL_PLUGIN_CONSTRUCTED (plugin), FALSE);

  return plugin->priv->expand;
}



/**
 * xfce_panel_plugin_set_expand:
 * @plugin : an #XfcePanelPlugin.
 * @expand : whether to expand the plugin.
 *
 * Wether the plugin should expand of not
 **/
void
xfce_panel_plugin_set_expand (XfcePanelPlugin *plugin,
                              gboolean         expand)
{
  g_return_if_fail (XFCE_IS_PANEL_PLUGIN (plugin));
  g_return_if_fail (XFCE_PANEL_PLUGIN_CONSTRUCTED (plugin));

  /* normalize the value */
  expand = !!expand;

  /* check if update is required */
  if (G_LIKELY (xfce_panel_plugin_get_expand (plugin) != expand))
    {
      plugin->priv->expand = expand;

      /* emit signal (in provider) */
      xfce_panel_plugin_provider_emit_signal (XFCE_PANEL_PLUGIN_PROVIDER (plugin),
                                              expand ? PROVIDER_SIGNAL_EXPAND_PLUGIN :
                                                  PROVIDER_SIGNAL_COLLAPSE_PLUGIN);

      g_object_notify (G_OBJECT (plugin), "expand");
    }
}



/**
 * xfce_panel_plugin_get_orientation:
 * @plugin : an #XfcePanelPlugin.
 *
 * The orientation of the panel in which the plugin is embedded.
 *
 * Returns: the current #GtkOrientation of the panel.
 **/
GtkOrientation
xfce_panel_plugin_get_orientation (XfcePanelPlugin *plugin)
{
  g_return_val_if_fail (XFCE_IS_PANEL_PLUGIN (plugin), GTK_ORIENTATION_HORIZONTAL);
  g_return_val_if_fail (XFCE_PANEL_PLUGIN_CONSTRUCTED (plugin), GTK_ORIENTATION_HORIZONTAL);

  return plugin->priv->orientation;
}



/**
 * xfce_panel_plugin_get_screen_position:
 * @plugin : an #XfcePanelPlugin.
 *
 * The screen position of the panel in which the plugin is embedded.
 *
 * Returns: the current #XfceScreenPosition of the panel.
 **/
XfceScreenPosition
xfce_panel_plugin_get_screen_position (XfcePanelPlugin *plugin)
{
  g_return_val_if_fail (XFCE_IS_PANEL_PLUGIN (plugin), XFCE_SCREEN_POSITION_NONE);
  g_return_val_if_fail (XFCE_PANEL_PLUGIN_CONSTRUCTED (plugin), XFCE_SCREEN_POSITION_NONE);

  return plugin->priv->screen_position;
}



/**
 * xfce_panel_plugin_take_window:
 * @plugin : an #XfcePanelPlugin.
 * @window : a #GtkWindow.
 *
 * Connect a dialog to a plugin. When the @plugin is closed, it will
 * destroy the @window.
 *
 * Since: 4.8
 **/
void
xfce_panel_plugin_take_window (XfcePanelPlugin *plugin,
                               GtkWindow       *window)
{
  g_return_if_fail (XFCE_IS_PANEL_PLUGIN (plugin));
  g_return_if_fail (GTK_IS_WINDOW (window));

  /* monitor both objects */
  g_object_weak_ref (G_OBJECT (plugin),
      xfce_panel_plugin_take_window_notify, window);
  g_object_weak_ref (G_OBJECT (window),
      xfce_panel_plugin_take_window_notify, plugin);
}



/**
 * xfce_panel_plugin_add_action_widget:
 * @plugin : an #XfcePanelPlugin.
 * @widget : a #GtkWidget that receives mouse events.
 *
 * Attach the plugin menu to this widget. Plugin writers should call this
 * for every widget that can receive mouse events. If you forget to call this
 * the plugin will not have a right-click menu and the user won't be able to
 * remove it.
 **/
void
xfce_panel_plugin_add_action_widget (XfcePanelPlugin *plugin,
                                     GtkWidget       *widget)
{
  g_return_if_fail (XFCE_IS_PANEL_PLUGIN (plugin));
  g_return_if_fail (GTK_IS_WIDGET (widget));

  g_signal_connect_swapped (G_OBJECT (widget), "button-press-event",
      G_CALLBACK (xfce_panel_plugin_button_press_event), plugin);
}



/**
 * xfce_panel_plugin_menu_insert_item:
 * @plugin : an #XfcePanelPlugin.
 * @item   : a #GtkMenuItem.
 *
 * Insert a custom menu item to the plugin's right click menu. This item
 * is packed below the "Move" menu item.
 **/
void
xfce_panel_plugin_menu_insert_item (XfcePanelPlugin *plugin,
                                    GtkMenuItem     *item)
{
  g_return_if_fail (XFCE_IS_PANEL_PLUGIN (plugin));
  g_return_if_fail (GTK_IS_MENU_ITEM (item));

  /* take the item and add to internal list */
  plugin->priv->menu_items = g_slist_prepend (plugin->priv->menu_items,
                                              g_object_ref_sink (item));
  g_signal_connect (G_OBJECT (item), "destroy",
      G_CALLBACK (xfce_panel_plugin_menu_item_destroy), plugin);

  /* destroy the menu */
  xfce_panel_plugin_menu_destroy (plugin);
}



/**
 * xfce_panel_plugin_menu_show_configure:
 * @plugin : an #XfcePanelPlugin.
 *
 * Show the "Properties" item in the menu. Clicking on the menu item
 * will emit the "configure-plugin" signal.
 **/
void
xfce_panel_plugin_menu_show_configure (XfcePanelPlugin *plugin)
{
  GtkMenu   *menu;
  GtkWidget *item;

  g_return_if_fail (XFCE_IS_PANEL_PLUGIN (plugin));
  g_return_if_fail (XFCE_PANEL_PLUGIN_CONSTRUCTED (plugin));

  PANEL_SET_FLAG (plugin->priv->flags, PLUGIN_FLAG_SHOW_CONFIGURE);

  /* show the menu item if the menu is already generated */
  if (G_UNLIKELY (plugin->priv->menu != NULL))
    {
       /* get and show the properties item */
       menu = xfce_panel_plugin_menu_get (plugin);
       item = g_object_get_qdata (G_OBJECT (menu), item_properties);
       if (G_LIKELY (item != NULL))
         gtk_widget_show (item);
    }

  /* emit signal, used by the external plugin */
  xfce_panel_plugin_provider_emit_signal (XFCE_PANEL_PLUGIN_PROVIDER (plugin),
                                          PROVIDER_SIGNAL_SHOW_CONFIGURE);
}



/**
 * xfce_panel_plugin_menu_show_about:
 * @plugin : an #XfcePanelPlugin.
 *
 * Show the "About" item in the menu. Clicking on the menu item
 * will emit the "about" signal.
 **/
void
xfce_panel_plugin_menu_show_about (XfcePanelPlugin *plugin)
{
  GtkMenu   *menu;
  GtkWidget *item;

  g_return_if_fail (XFCE_IS_PANEL_PLUGIN (plugin));
  g_return_if_fail (XFCE_PANEL_PLUGIN_CONSTRUCTED (plugin));

  PANEL_SET_FLAG (plugin->priv->flags, PLUGIN_FLAG_SHOW_ABOUT);

  /* show the menu item if the menu is already generated */
  if (G_UNLIKELY (plugin->priv->menu != NULL))
    {
       /* get and show the about item */
       menu = xfce_panel_plugin_menu_get (plugin);
       item = g_object_get_qdata (G_OBJECT (menu), item_about);
       if (G_LIKELY (item != NULL))
         gtk_widget_show (item);
    }

  /* emit signal, used by the external plugin */
  xfce_panel_plugin_provider_emit_signal (XFCE_PANEL_PLUGIN_PROVIDER (plugin),
                                          PROVIDER_SIGNAL_SHOW_ABOUT);
}



/**
 * xfce_panel_plugin_get_locked:
 * @plugin : an #XfcePanelPlugin.
 *
 * Whether the plugin is locked (not allowing customization). This
 * is emitted through the panel based on the Xfconf locking of the
 * panel window the plugin is embedded on.
 *
 * It is however possible to send a fake signal to the plugin to
 * override this propery, so you should only use this for interface
 * elements and (if you use Xfconf) check the locking yourself
 * before you write any values or query the kiosk mode using the
 * api in libxfce4util.
 *
 * Returns: %TRUE if the user is not allowed to modify the plugin,
 *          %FALSE is customization is allowed.
 *
 * Since: 4.8
 **/
gboolean
xfce_panel_plugin_get_locked (XfcePanelPlugin *plugin)
{
  g_return_val_if_fail (XFCE_IS_PANEL_PLUGIN (plugin), TRUE);

  return plugin->priv->locked;
}



/**
 * xfce_panel_plugin_remove:
 * @plugin : an #XfcePanelPlugin.
 *
 * Remove this plugin from the panel and remove all its configuration.
 *
 * Plugins should not use this function to implement their own
 * menu item or button to remove theirselfs from the panel, but only
 * in case the there are problems with the plugin in the panel. Always
 * try to inform the user why this occured.
 *
 * Since: 4.8
 **/
void
xfce_panel_plugin_remove (XfcePanelPlugin *plugin)
{
  g_return_if_fail (XFCE_IS_PANEL_PLUGIN (plugin));

  /* ask the panel or wrapper to remove the plugin */
  xfce_panel_plugin_provider_emit_signal (XFCE_PANEL_PLUGIN_PROVIDER (plugin),
                                          PROVIDER_SIGNAL_REMOVE_PLUGIN);
}



/**
 * xfce_panel_plugin_block_menu:
 * @plugin : an #XfcePanelPlugin.
 *
 * Block configuring the plugin. This will make the "Properties" menu
 * item insensitive.
 **/
void
xfce_panel_plugin_block_menu (XfcePanelPlugin *plugin)
{
  g_return_if_fail (XFCE_IS_PANEL_PLUGIN (plugin));

  /* increase block counter */
  plugin->priv->menu_blocked++;
}



/**
 * xfce_panel_plugin_unblock_menu:
 * @plugin : an #XfcePanelPlugin.
 *
 * Unblock configuring the plugin. This will make the "Properties" menu
 * item sensitive.
 **/
void
xfce_panel_plugin_unblock_menu (XfcePanelPlugin *plugin)
{
  g_return_if_fail (XFCE_IS_PANEL_PLUGIN (plugin));
  g_return_if_fail (plugin->priv->menu_blocked > 0);

  /* decrease block counter */
  if (G_LIKELY (plugin->priv->menu_blocked > 0))
    plugin->priv->menu_blocked--;
}



/**
 * xfce_panel_plugin_register_menu:
 * @plugin : an #XfcePanelPlugin.
 * @menu   : a #GtkMenu that will be opened
 *
 * Register a menu that is about to popup. This will make sure the panel
 * will properly handle its autohide behaviour. You have to call this
 * function every time the menu is opened (e.g. using gtk_popup_menu()).
 *
 * If you want to open the menu aligned to the side of the panel (and the
 * plugin), you should use xfce_panel_plugin_position_menu() as
 * #GtkMenuPositionFunc. This callback function will take care of calling
 * xfce_panel_plugin_register_menu() as well.
 *
 * See also: xfce_panel_plugin_position_menu() and xfce_panel_plugin_block_autohide().
 **/
void
xfce_panel_plugin_register_menu (XfcePanelPlugin *plugin,
                                 GtkMenu         *menu)
{
  g_return_if_fail (XFCE_IS_PANEL_PLUGIN (plugin));
  g_return_if_fail (GTK_IS_MENU (menu));
  g_return_if_fail (XFCE_PANEL_PLUGIN_CONSTRUCTED (plugin));

  /* increase the counter */
  plugin->priv->panel_lock++;

  /* connect signal to menu to decrease counter */
  g_signal_connect (G_OBJECT (menu), "deactivate",
      G_CALLBACK (xfce_panel_plugin_unregister_menu), plugin);
  g_signal_connect (G_OBJECT (menu), "destroy",
      G_CALLBACK (xfce_panel_plugin_unregister_menu), plugin);

  /* tell panel it needs to lock */
  if (plugin->priv->panel_lock == 1)
    xfce_panel_plugin_provider_emit_signal (XFCE_PANEL_PLUGIN_PROVIDER (plugin),
                                            PROVIDER_SIGNAL_LOCK_PANEL);
}



/**
 * xfce_panel_plugin_arrow_type:
 * @plugin : an #XfcePanelPlugin.
 *
 * Determine the #GtkArrowType for a widget that opens a menu and uses
 * xfce_panel_plugin_position_menu() to position the menu.
 *
 * Returns: the #GtkArrowType to use.
 **/
GtkArrowType
xfce_panel_plugin_arrow_type (XfcePanelPlugin *plugin)
{
  XfceScreenPosition  screen_position;
  GdkScreen          *screen;
  gint                monitor_num;
  GdkRectangle        monitor;
  gint                x, y;

  g_return_val_if_fail (XFCE_IS_PANEL_PLUGIN (plugin), GTK_ARROW_NONE);
  g_return_val_if_fail (XFCE_PANEL_PLUGIN_CONSTRUCTED (plugin), GTK_ARROW_NONE);

  /* get the plugin screen position */
  screen_position = xfce_panel_plugin_get_screen_position (plugin);

  /* detect the arrow type */
  if (xfce_screen_position_is_top (screen_position))
    return GTK_ARROW_DOWN;
  else if (xfce_screen_position_is_bottom (screen_position))
    return GTK_ARROW_UP;
  else if (xfce_screen_position_is_left (screen_position))
    return GTK_ARROW_RIGHT;
  else if (xfce_screen_position_is_right (screen_position))
    return GTK_ARROW_LEFT;
  else /* floating */
    {
      /* get the monitor geometry */
      screen = gtk_widget_get_screen (GTK_WIDGET (plugin));
      monitor_num = gdk_screen_get_monitor_at_window (screen, GTK_WIDGET (plugin)->window);
      gdk_screen_get_monitor_geometry (screen, monitor_num, &monitor);

      /* get the plugin root origin */
      gdk_window_get_root_origin (GTK_WIDGET (plugin)->window, &x, &y);

      /* detect arrow type */
      if (screen_position == XFCE_SCREEN_POSITION_FLOATING_H)
        return (y < (monitor.y + monitor.height / 2)) ? GTK_ARROW_DOWN : GTK_ARROW_UP;
      else
        return (x < (monitor.x + monitor.width / 2)) ? GTK_ARROW_RIGHT : GTK_ARROW_LEFT;
    }
}



/**
 * xfce_panel_plugin_position_widget:
 * @plugin        : an #XfcePanelPlugin.
 * @menu_widget   : a #GtkWidget that will be used as popup menu.
 * @attach_widget : a #GtkWidget relative to which the menu should be positioned.
 * @x             : return location for the x coordinate.
 * @y             : return location for the x coordinate.
 *
 * The menu widget is positioned relative to @attach_widget.
 * If @attach_widget is NULL, the menu widget is instead positioned
 * relative to @panel_plugin.
 *
 * This function is intended for custom menu widgets.
 * For a regular #GtkMenu you should use xfce_panel_plugin_position_menu()
 * instead (as callback argument to gtk_menu_popup()).
 *
 * See also: xfce_panel_plugin_position_menu().
 **/
void
xfce_panel_plugin_position_widget (XfcePanelPlugin *plugin,
                                   GtkWidget       *menu_widget,
                                   GtkWidget       *attach_widget,
                                   gint            *x,
                                   gint            *y)
{
  GtkRequisition  requisition;
  GdkScreen      *screen;
  GdkRectangle    monitor;
  gint            monitor_num;
  GTimeVal        now_t, end_t;

  g_return_if_fail (XFCE_IS_PANEL_PLUGIN (plugin));
  g_return_if_fail (GTK_IS_WIDGET (menu_widget));
  g_return_if_fail (attach_widget == NULL || GTK_IS_WIDGET (attach_widget));
  g_return_if_fail (XFCE_PANEL_PLUGIN_CONSTRUCTED (plugin));

  /* if the attach widget is null, use the panel plugin */
  if (attach_widget == NULL)
    attach_widget = GTK_WIDGET (plugin);

  /* make sure the menu is realized to get valid rectangle sizes */
  if (!GTK_WIDGET_REALIZED (menu_widget))
    gtk_widget_realize (menu_widget);

  /* make sure the attach widget is realized for the gdkwindow */
  if (!GTK_WIDGET_REALIZED (attach_widget))
    gtk_widget_realize (attach_widget);

  /* get the menu/widget size request */
  gtk_widget_size_request (menu_widget, &requisition);

  /* get the root position of the attach widget */
  gdk_window_get_position (GDK_WINDOW (attach_widget->window), x, y);

  /* if the panel is hidden (auto hide is enabled) and we requested a
   * panel lock, wait for gtk to position the panel before we actually
   * use the coordinates */
  if (plugin->priv->panel_lock > 0)
    {
      g_get_current_time (&end_t);
      g_time_val_add (&end_t, G_USEC_PER_SEC / 2);

      while (*x == -9999 && *y == -9999)
        {
          while (gtk_events_pending ())
            gtk_main_iteration ();

          gdk_window_get_position (GDK_WINDOW (attach_widget->window), x, y);

          /* don't try longer then 1/2 a second */
          g_get_current_time (&now_t);
          if (now_t.tv_sec > end_t.tv_sec
              || (now_t.tv_sec == end_t.tv_sec
                  && now_t.tv_usec > end_t.tv_usec))
            break;
        }
    }

  /* add the widgets allocation */
  *x += attach_widget->allocation.x;
  *y += attach_widget->allocation.y;

  switch (xfce_panel_plugin_arrow_type (plugin))
    {
    case GTK_ARROW_UP:
      *y -= requisition.height;
      break;

    case GTK_ARROW_DOWN:
      *y += attach_widget->allocation.height;
      break;

    case GTK_ARROW_LEFT:
      *x -= requisition.width;
      break;

    default: /* GTK_ARROW_RIGHT and GTK_ARROW_NONE */
      *x += attach_widget->allocation.width;
      break;
    }

  /* get the monitor geometry */
  screen = gtk_widget_get_screen (attach_widget);
  monitor_num = gdk_screen_get_monitor_at_window (screen, attach_widget->window);
  gdk_screen_get_monitor_geometry (screen, monitor_num, &monitor);

  /* keep the menu inside the screen */
  if (*x > monitor.x + monitor.width - requisition.width)
    *x = monitor.x + monitor.width - requisition.width;
  if (*x < monitor.x)
    *x = monitor.x;
  if (*y > monitor.y + monitor.height - requisition.height)
    *y = monitor.y + monitor.height - requisition.height;
  if (*y < monitor.y)
    *y = monitor.y;

  /* popup on the correct screen */
  if (G_LIKELY (GTK_IS_MENU (menu_widget)))
    gtk_menu_set_screen (GTK_MENU (menu_widget), screen);
  else if (GTK_IS_WINDOW (menu_widget))
    gtk_window_set_screen (GTK_WINDOW (menu_widget), screen);
}



/**
 * xfce_panel_plugin_position_menu:
 * @menu         : a #GtkMenu.
 * @x            : return location for the x coordinate.
 * @y            : return location for the y coordinate.
 * @push_in      : keep inside the screen (see #GtkMenuPositionFunc)
 * @panel_plugin : an #XfcePanelPlugin.
 *
 * Function to be used as #GtkMenuPositionFunc in a call to gtk_menu_popup().
 * As data argument it needs an #XfcePanelPlugin.
 *
 * The menu is normally positioned relative to @panel_plugin. If you want the
 * menu to be positioned relative to another widget, you can use
 * gtk_menu_attach_to_widget() to explicitly set a 'parent' widget.
 *
 * As a convenience, xfce_panel_plugin_position_menu() calls
 * xfce_panel_plugin_register_menu() for the menu.
 *
 * <example>
 * void
 * myplugin_popup_menu (XfcePanelPlugin *plugin,
 *                      GtkMenu         *menu,
 *                      GdkEventButton  *ev)
 * {
 *     gtk_menu_popup (menu, NULL, NULL,
 *                     xfce_panel_plugin_position_menu, plugin,
 *                     ev->button, ev->time );
 * }
 * </example>
 *
 * For a custom widget that will be used as a popup menu, use
 * xfce_panel_plugin_position_widget() instead.
 *
 * See also: gtk_menu_popup().
 **/
void
xfce_panel_plugin_position_menu (GtkMenu  *menu,
                                 gint     *x,
                                 gint     *y,
                                 gboolean *push_in,
                                 gpointer  panel_plugin)
{
  GtkWidget *attach_widget;

  g_return_if_fail (XFCE_IS_PANEL_PLUGIN (panel_plugin));
  g_return_if_fail (GTK_IS_MENU (menu));
  g_return_if_fail (XFCE_PANEL_PLUGIN_CONSTRUCTED (panel_plugin));

  /* register the menu */
  xfce_panel_plugin_register_menu (XFCE_PANEL_PLUGIN (panel_plugin), menu);

  /* calculate the coordinates */
  attach_widget = gtk_menu_get_attach_widget (menu);
  xfce_panel_plugin_position_widget (XFCE_PANEL_PLUGIN (panel_plugin),
                                     GTK_WIDGET (menu), attach_widget, x, y);

  /* keep the menu inside screen */
  *push_in = TRUE;
}



/**
 * xfce_panel_plugin_focus_widget:
 * @plugin : an #XfcePanelPlugin.
 * @widget : a #GtkWidget inside the plugins that should be focussed.
 *
 * Grab the focus on @widget. Asks the panel to allow focus on its items
 * and set the focus to the requested widget.
 **/
void
xfce_panel_plugin_focus_widget (XfcePanelPlugin *plugin,
                                GtkWidget       *widget)
{
  g_return_if_fail (XFCE_IS_PANEL_PLUGIN (plugin));
  g_return_if_fail (GTK_IS_WIDGET (widget));
  g_return_if_fail (XFCE_PANEL_PLUGIN_CONSTRUCTED (plugin));

  /* focus the panel window */
  xfce_panel_plugin_provider_emit_signal (XFCE_PANEL_PLUGIN_PROVIDER (plugin),
                                          PROVIDER_SIGNAL_FOCUS_PLUGIN);

  /* let the widget grab focus */
  gtk_widget_grab_focus (widget);
}



/**
 * xfce_panel_plugin_block_autohide:
 * @plugin  : an #XfcePanelPlugin.
 * @blocked : new blocking state of this plugin.
 *
 * Wether this plugin blocks the autohide functality of the panel. Use
 * this when you 'popup' something that is visually attached to the
 * plugin at it will look weird for a user if the panel will hide while
 * he/she is working in the popup.
 *
 * For menus it there is xfce_panel_plugin_register_menu() which will
 * take care of this.
 **/
void
xfce_panel_plugin_block_autohide (XfcePanelPlugin *plugin,
                                  gboolean         blocked)
{
  g_return_if_fail (XFCE_IS_PANEL_PLUGIN (plugin));
  g_return_if_fail (XFCE_PANEL_PLUGIN_CONSTRUCTED (plugin));

  /* leave when requesting the same block state */
  if (PANEL_HAS_FLAG (plugin->priv->flags, PLUGIN_FLAG_BLOCK_AUTOHIDE) == blocked)
    return;

  if (blocked)
    {
      /* increase the counter */
      panel_return_if_fail (plugin->priv->panel_lock >= 0);
      plugin->priv->panel_lock++;

      PANEL_SET_FLAG (plugin->priv->flags, PLUGIN_FLAG_BLOCK_AUTOHIDE);

      /* tell panel it needs to lock */
      if (plugin->priv->panel_lock == 1)
        xfce_panel_plugin_provider_emit_signal (XFCE_PANEL_PLUGIN_PROVIDER (plugin),
                                                PROVIDER_SIGNAL_LOCK_PANEL);
    }
  else
    {
      /* decrease the counter */
      panel_return_if_fail (plugin->priv->panel_lock > 0);
      plugin->priv->panel_lock--;

      PANEL_UNSET_FLAG (plugin->priv->flags, PLUGIN_FLAG_BLOCK_AUTOHIDE);

      /* tell panel it needs to unlock */
      if (plugin->priv->panel_lock == 0)
        xfce_panel_plugin_provider_emit_signal (XFCE_PANEL_PLUGIN_PROVIDER (plugin),
                                                PROVIDER_SIGNAL_UNLOCK_PANEL);
    }
}



/**
 * xfce_panel_plugin_lookup_rc_file:
 * @plugin : an #XfcePanelPlugin.
 *
 * Looks for the plugin resource file. This should be used to get the
 * plugin read location of the config file. You should only use the
 * returned path to read information from, since it might point to a
 * not-writable file (in kiosk mode for example).
 *
 * See also: xfce_panel_plugin_save_location() and xfce_resource_lookup()
 *
 * Returns: The path to a config file or %NULL if no file was found.
 *          The returned string must be freed using g_free()
 **/
gchar *
xfce_panel_plugin_lookup_rc_file (XfcePanelPlugin *plugin)
{
  gchar *filename, *path;

  g_return_val_if_fail (XFCE_IS_PANEL_PLUGIN (plugin), NULL);
  g_return_val_if_fail (XFCE_PANEL_PLUGIN_CONSTRUCTED (plugin), NULL);

  filename = xfce_panel_plugin_relative_filename (plugin);
  path = xfce_resource_lookup (XFCE_RESOURCE_CONFIG, filename);
  g_free (filename);

  return path;
}



/**
 * xfce_panel_plugin_save_location:
 * @plugin : an #XfcePanelPlugin.
 * @create : whether to create missing directories.
 *
 * Returns the path that can be used to store configuration information.
 * Don't use this function if you want to read from the config file, but
 * use xfce_panel_plugin_rc_location() instead.
 *
 * See also: xfce_panel_plugin_rc_location() and xfce_resource_save_location()
 *
 * Returns: The path to a config file or %NULL if no file was found.
 *          The returned string must be freed u sing g_free().
 **/
gchar *
xfce_panel_plugin_save_location (XfcePanelPlugin *plugin,
                                 gboolean         create)
{
  gchar *filename, *path;

  g_return_val_if_fail (XFCE_IS_PANEL_PLUGIN (plugin), NULL);

  filename = xfce_panel_plugin_relative_filename (plugin);
  path = xfce_resource_save_location (XFCE_RESOURCE_CONFIG, filename, create);
  g_free (filename);

  return path;
}



#define __XFCE_PANEL_PLUGIN_C__
#include <libxfce4panel/libxfce4panel-aliasdef.c>
