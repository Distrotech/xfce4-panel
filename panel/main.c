/*  xfce4
 *
 *  Copyright (c) 2002 Jasper Huijsmans <huysmans@users.sourceforge.net>
 *  Copyright (c) 2003 Benedikt Meurer <benedikt.meurer@unix-ag.uni-siegen.de>
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

/*  main.c 
 *  ------
 *  Contains 'main' function, quit and restart functions
 *  and session management.
*/

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif

#ifdef HAVE_SIGNAL_H
#include <signal.h>
#endif
#ifdef HAVE_STDDEF_H
#include <stddef.h>
#endif
#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#include <libxfce4util/i18n.h>
#include <libxfce4util/debug.h>
#include <libxfcegui4/session-client.h>
#include <libxfcegui4/netk-util.h>

#include "xfce.h"
#include "settings.h"
#include "mcs_client.h"
#include "controls_dialog.h"
#include "item_dialog.h"
#include "popup.h"

/* signal handling */
typedef enum
{
    NOSIGNAL,
    RESTART,
    QUIT,
    QUIT_CONFIRM,
    NUM_SIGNALS,
}
SignalState;

static SignalState sigstate = NOSIGNAL;

static char *progname = NULL;

/*  session management
 *  ------------------
*/
static SessionClient *client_session;
static gboolean session_managed = FALSE;

static void
save_panel (void)
{
    if (!disable_user_config)
	write_panel_config ();
}

static void
save_yourself (gpointer data, int save_style, gboolean shutdown,
	       int interact_style, gboolean fast)
{
    save_panel ();
}

static void
die (gpointer client_data)
{
    quit (TRUE);
}

/*  Exported interface
 *  ------------------
*/
void
quit (gboolean force)
{
    if (!force)
    {
	if (session_managed)
	{
	    logout_session (client_session);
	    return;
	}
	else if (!confirm
		 (_("Are you sure you want to exit?"), GTK_STOCK_QUIT, NULL))
	{
	    return;
	}
    }

    mcs_stop_watch ();

    save_panel ();

#ifdef HAVE_LIBSTARTUP_NOTIFICATION
    free_startup_timeout ();
#endif

    panel_cleanup ();

    if (gtk_main_level ())
	gtk_main_quit ();

    DBG("sigstate: %d", sigstate);
    if (sigstate != RESTART)
    {
	g_message("%s: Exit", PACKAGE);
	exit (0);
    }
}

/*  Main program
 *  ------------
*/
static gboolean
check_signal_state (void)
{
    static gboolean restarting = FALSE;

    if (sigstate != NOSIGNAL)
    {
	/* close open dialogs */
	destroy_controls_dialog();
	destroy_menu_dialog();
	
	if (sigstate == RESTART && !restarting)
	{
	    restarting = TRUE;
	    
	    /* calls gtk_main_quit() */
	    quit(TRUE);

	    /* progname is saved on startup 
	     * TODO: do we need to pass on arguments? */
	    g_message("%s: restarting %s ...", PACKAGE, progname);
	    execlp(progname,progname,NULL);
	}
	else if (sigstate == QUIT)
	{
	    quit (TRUE);
	}
	else if (sigstate == QUIT_CONFIRM)
	{
	    sigstate = NOSIGNAL;
	    quit (FALSE);
	}
    }

    /* keep running */
    return TRUE;
}

static void
sighandler (int sig)
{
    /* Don't do any reall stuff here.
     * Only set a signal state flag. There's a timeout in the main loop
     * that tests the flag.
     * This will prevent problems with gtk main loop threads and stuff
     */
    switch (sig)
    {
	case SIGUSR1:
	    sigstate = RESTART;
	    break;

	case SIGUSR2:
	    sigstate = QUIT_CONFIRM;
	    break;
	    
	case SIGINT:
	    /* hack: prevent the panel from saving config on ^C */
	    disable_user_config = TRUE;
	    /* fall through */

	default:
	    sigstate = QUIT;
    }
}

int
main (int argc, char **argv)
{
#ifdef HAVE_SIGACTION
    struct sigaction act;
#endif
    gboolean net_wm_support;
    int i;

    progname = argv[0];
    
    net_wm_support = FALSE;

#if 0
#ifdef ENABLE_NLS
    bindtextdomain (GETTEXT_PACKAGE, LOCALEDIR);
#ifdef HAVE_BIND_TEXTDOMAIN_CODESET
    bind_textdomain_codeset (GETTEXT_PACKAGE, "UTF-8");
#endif
    textdomain (GETTEXT_PACKAGE);
#endif
#else
    xfce_textdomain(GETTEXT_PACKAGE, LOCALEDIR, "UTF-8");
#endif

    gtk_set_locale ();
    gtk_init (&argc, &argv);

    if (argc == 2 &&
	(strequal (argv[1], "-v") || strequal (argv[1], "--version") ||
	 strequal (argv[1], "-h") || strequal (argv[1], "--help")))
    {
	g_print (_("%s, version %s\n"
		   "Part of the XFce Desktop Environment\n"
		   "http://www.xfce.org\n"), PACKAGE, VERSION);
	return 0;
    }

    for (i = 0; i < 5; i++)
    {
	if ((net_wm_support = check_net_wm_support ()) == TRUE)
	    break;

	g_usleep (2000 * 1000);
    }

    if (!net_wm_support)
    {
	xfce_err (_("Your window manager does not seem to support "
		    "the new window manager hints as defined on "
		    "http://www.freedesktop.org. \n"
		    "Some XFce features may not work as intended."));
    }

    client_session = client_session_new (argc, argv, NULL /* data */ ,
					 SESSION_RESTART_IF_RUNNING, 40);

    client_session->save_yourself = save_yourself;
    client_session->die = die;

    if (!(session_managed = session_init (client_session)))
	g_message ("%s: Running without session manager", PACKAGE);

#ifdef HAVE_SIGACTION
    act.sa_handler = sighandler;
    sigemptyset (&act.sa_mask);
#ifdef SA_RESTART
    act.sa_flags = SA_RESTART;
#else
    act.sa_flags = 0;
#endif
    sigaction (SIGHUP, &act, NULL);
    sigaction (SIGUSR1, &act, NULL);
    sigaction (SIGUSR2, &act, NULL);
    sigaction (SIGINT, &act, NULL);
    sigaction (SIGTERM, &act, NULL);
#else
    signal (SIGHUP, sighandler);
    signal (SIGUSR1, sighandler);
    signal (SIGUSR2, sighandler);
    signal (SIGINT, sighandler);
    signal (SIGTERM, sighandler);
#endif

    /* icon framework: names and id's */
    icons_init ();

    create_panel ();

    /* signal state */
    g_timeout_add (500, (GSourceFunc) check_signal_state, NULL);

    gtk_main ();

    return 0;
}
