/* toolshared.vala -- Functions which are useful for all Listaller cmdline programs
 *
 * Copyright (C) 2010-2013 Matthias Klumpp
 *
 * Licensed under the GNU General Public License Version 3
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

using GLib;
using Posix;

public static extern unowned char* ctermid (char *__s);

/**
* cmd_strpad:
* @data: the input string
* @length: the desired length of the output string, with padding
*
* Returns the text padded to a length with spaces. If the string is
* longer than length then a longer string is returned.
*
* Return value: The padded string
**/
public string cmd_strpad (string? data, uint length) {
	int size;
	uint data_len;
	string text;
	string padding;

	if (data == null)
		return string.nfill (length, ' ');

	// ITS4: ignore, only used for formatting
	data_len = data.length;

	// calculate
	size = (int) (length - data_len);
	if (size <= 0)
		return data;

	padding = string.nfill (size, ' ');
	text = "%s%s".printf (data, padding);

	return text;
}

private string? console_get_tty_name () {
	string? tty_name;
	tty_name = (string) ctermid (null);
	if (tty_name == null) {
		warning ("Cannot get terminal: %s",
			   Posix.strerror (Posix.errno));
		return null;
	}
	return tty_name;
}

private FILE? console_open_tty (string tty_name) {
	FILE tty;
	tty = FILE.open (tty_name, "r+");
	if (tty == null) {
		warning ("Error opening terminal for the process (`%s'): %s",
			   tty_name, Posix.strerror (Posix.errno));
		return null;
	}
	return tty;
}

public FILE? console_get_tty () {
	string? tty_name;
	FILE tty;
	tty_name = console_get_tty_name ();
	if (tty_name == null)
		return null;

	tty = console_open_tty (tty_name);
	if (tty == null)
		return null;

	return tty;
}

public string? console_readline_unbuffered (string prompt)
{
	FILE tty;
	string? str = null;
	termios ts, ots;

	tty = console_get_tty ();
	if (tty == null)
		return null;

	tty.printf ("%s", prompt);
	tty.flush ();
	//! setbuf (tty, null);

	// taken from polkitagenttextlistener.c
	Posix.tcgetattr (tty.fileno (), out ts);
	ots = ts;
	ts.c_lflag &= ~(ECHONL);
	Posix.tcsetattr (tty.fileno (), TCSAFLUSH, ts);

	str = "";
	while (true) {
		int c;
		c = tty.getc ();
		if (c == '\n') {
			// ok, done
			break;
		} else if (c == FILE.EOF) {
			warning ("Got unexpected EOF.");
			break;
		} else {
			str = "%s%c".printf (str, (char) c);
		}
	}
	Posix.tcsetattr (tty.fileno (), TCSAFLUSH, ots);
	tty.putc ('\n');

	if (str == "")
		return null;
	return str;
}

public void console_termios_noecho (termios old) {
	old.c_lflag &= ~ICANON;
	old.c_lflag &= ~ECHO;
	old.c_cc[VMIN] = 1;
	old.c_cc[VTIME] = 0;
	if (Posix.tcsetattr (0, TCSANOW, old) < 0)
		GLib.stderr.printf ("tcsetattr ~ICANON\n");
}

public void console_termios_restore (termios old) {
	old.c_lflag |= ICANON;
	old.c_lflag |= ECHO;
	if (Posix.tcsetattr (0, TCSADRAIN, old) < 0)
		GLib.stderr.printf ("tcsetattr ICANON\n");
}

public void console_wait_for_enter (FILE? atty = null) {
	FILE tty;

	tty = console_get_tty ();
	if (tty == null)
		return;

	// prepare termios
	termios old = {0};
	if (Posix.tcgetattr (0, out old) < 0)
		GLib.stderr.printf ("tcgetattr()\n");
	console_termios_noecho (old);

	while (true) {
		int c;
		if (atty == null)
			c = tty.getc ();
		else
			c = atty.getc ();
		if (c == '\n') {
			// ok, done
			break;
		} else if (c == FILE.EOF) {
			warning ("Got unexpected EOF.");
			break;
		}
	}

	console_termios_restore (old);
}

public bool console_get_prompt (string question, bool defaultyes, bool forceanswer = false)
{
	bool ret = false;
	bool valid = false;
	string prompt;
	string? str;

	if (!forceanswer)
		prompt = "%s %s ".printf (question, defaultyes ? "[Y/n]" : "[N/y]");
	else
		prompt = "%s %s ".printf (question, "[y/n]");

	while (!valid) {
		str = console_readline_unbuffered (prompt);

		if ((!forceanswer) && (str == null))
			str = "";
		else if (str == null)
			break;

		if (str.length == 0) {
			if (defaultyes) {
				valid = true;
				ret = true;
			} else {
				valid = true;
				ret = false;
			}
		}
		str = str.down ();
		if ((str == "y") ||
		    (str == "yes")) {
			valid = true;
			ret = true;
		}
		if ((str == "n") ||
		    (str == "no")) {
			valid = true;
			ret = false;
		}
	}
	if ((!valid) && (forceanswer)) {
		ret = console_get_prompt (question, defaultyes, true);
	}

	return ret;
}

public bool is_root () {
	if (Posix.getuid () == 0) {
		return true;
	} else {
		return false;
	}
}
