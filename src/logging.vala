/* logging.vala
 *
 * Copyright (C) 2012-2013 Matthias Klumpp <matthias@tenstral.net>
 *
 * Licensed under the GNU Lesser General Public License Version 3
 *
 * This library is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this library.  If not, see <http://www.gnu.org/licenses/>.
 */

using GLib;
using Gee;
using Listaller;

private static const int CONSOLE_RESET   = 0;
private static const int CONSOLE_BLACK   = 30;
private static const int CONSOLE_RED     = 31;
private static const int CONSOLE_GREEN   = 32;
private static const int CONSOLE_YELLOW  = 33;
private static const int CONSOLE_BLUE    = 34;
private static const int CONSOLE_MAGENTA = 35;
private static const int CONSOLE_CYAN    = 36;
private static const int CONSOLE_WHITE   = 37;

private static const int DEBUG_LOG_DOMAIN_LENGTH = 20;

extern static const string? G_LOG_DOMAIN;

namespace Listaller {

private static bool __debug_errors_fatal = false;
private static bool verbose_mode = false;
private static bool _console = true;
private static bool _clitool = false;

} // End of namespace: Listaller

private static void li_log_handler_cb (string? log_domain, LogLevelFlags log_level, string message) {
	var str_time_buf = new char[255];
	uint len;
	uint i;

	if (log_domain == null)
		log_domain = "";

	string msg_text = message;

	if (!verbose_mode) {
		// we ignore debug info if not in verbose mode
		if (log_level == LogLevelFlags.LEVEL_DEBUG)
			return;

		// remove the source-code information from output, when not in verbose mode
		if (msg_text.index_of (".vala") > 0) {
			string msg = msg_text;
			msg_text = msg.substring (msg.index_of (":", msg.index_of (":") + 1) + 2);
		}
	}

	// time header
	var the_time = Time.local (time_t ());
	the_time.strftime (str_time_buf, "%H:%M:%S");
	string str_time = (string) str_time_buf;

	// don't colour the output
	if (!_console) {
		if (log_level == LogLevelFlags.LEVEL_DEBUG) {
			stdout.printf ("%s\t%s\t%s\n", str_time, log_domain, msg_text);
		} else {
			stdout.printf ("***\n%s\t%s\t%s\n***\n", str_time, log_domain, msg_text);
		}
		return;
	}

	// in a command-line utility, we don't print additional information which crowds the output
	if ((_clitool) && (!verbose_mode)) {
		// debug-output is never shown if we're not in verbose mode
		//stdout.printf ("%c[%dm%s\t", 0x1B, CONSOLE_RESET, log_domain);
		switch (log_level) {
			case LogLevelFlags.LEVEL_CRITICAL:
				stdout.printf ("%c[%dm%s %c[%dm", 0x1B, CONSOLE_RED, "CRITICAL", 0x1B, CONSOLE_RESET);
				break;
			case LogLevelFlags.LEVEL_ERROR:
				stdout.printf ("%c[%dm%s %c[%dm", 0x1B, CONSOLE_RED, "ERROR", 0x1B, CONSOLE_RESET);
				break;
			case LogLevelFlags.LEVEL_WARNING:
				stdout.printf ("%c[%dm%s %c[%dm", 0x1B, CONSOLE_YELLOW, "WARNING", 0x1B, CONSOLE_RESET);
				break;
			default: break;
		}
		stdout.printf ("%c[%dm%s\n%c[%dm", 0x1B, CONSOLE_RESET, msg_text, 0x1B, CONSOLE_RESET);
		return;
	}

	// time in green
	stdout.printf ("%c[%dm%s\t", 0x1B, CONSOLE_GREEN, str_time);

	// log domain in either white/black or cyan
	if (log_domain == G_LOG_DOMAIN)
		stdout.printf ("%c[%dm%s%c[%dm", 0x1B, CONSOLE_RESET, log_domain, 0x1B, CONSOLE_RESET);
	else
		stdout.printf ("%c[%dm%s%c[%dm", 0x1B, CONSOLE_CYAN, log_domain, 0x1B, CONSOLE_RESET);

	// pad with spaces
	len = log_domain.length;
	for (i = len; i < DEBUG_LOG_DOMAIN_LENGTH; i++)
		stdout.printf (" ");

	// critical is also in red
	if (log_level == LogLevelFlags.LEVEL_CRITICAL ||
	    log_level == LogLevelFlags.LEVEL_WARNING ||
	    log_level == LogLevelFlags.LEVEL_ERROR) {
		stdout.printf ("%c[%dm%s\n%c[%dm", 0x1B, CONSOLE_RED, msg_text, 0x1B, CONSOLE_RESET);
		// stderr.printf ("%c[%dm%s%c[%dm %s\n", 0x1B, CONSOLE_RED, "[error]:", 0x1B, CONSOLE_RESET, msg);
	} else {
		// debug in standard colour
		stdout.printf ("%c[%dm%s\n%c[%dm", 0x1B, CONSOLE_RESET, msg_text, 0x1B, CONSOLE_RESET);
	}
}

private static void
li_log_ignore_cb (string? log_domain, LogLevelFlags log_level,
		    string message) { }

namespace Listaller {

public void set_verbose_mode (bool enabled) {
	verbose_mode = enabled;
}

public bool get_verbose_mode () {
	return verbose_mode;
}

public void set_console_mode (bool enabled) {
	_console = enabled;
}

public void set_clitool_mode (bool enabled) {
	_clitool = enabled;
	set_console_mode (enabled);
}

public bool get_clitool_mode () {
	return _clitool;
}

public void add_log_domain (string log_domain)
{
	Log.set_fatal_mask (log_domain, LogLevelFlags.LEVEL_ERROR | LogLevelFlags.LEVEL_CRITICAL);
	Log.set_handler (log_domain,
			   LogLevelFlags.LEVEL_ERROR |
			   LogLevelFlags.LEVEL_CRITICAL |
			   LogLevelFlags.LEVEL_DEBUG |
			   LogLevelFlags.LEVEL_WARNING,
			   li_log_handler_cb);
	Log.set_default_handler (li_log_handler_cb);
}

} // End of namespace: Listaller
