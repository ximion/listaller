/* lipa.vala -- Listaller command-line tool, main class
 *
 * Copyright (C) 2010-2012 Matthias Klumpp
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

string? console_readline_unbuffered (string prompt)
{
	string? tty_name;
	FILE tty;
	string? str = null;
	termios ts, ots;

	tty_name = (string) ctermid (null);
	if (tty_name == null) {
		warning ("Cannot get terminal: %s",
			   Posix.strerror (Posix.errno));
		return null;
	}

	tty = FILE.open (tty_name, "r+");
	if (tty == null) {
		warning ("Error opening terminal for the process (`%s'): %s",
			   tty_name, Posix.strerror (Posix.errno));
		return null;
	}

	tty.printf ("%s", prompt);
	tty.flush ();
	//! setbuf (tty, null);

	// taken from polkitagenttextlistener.c
	tcgetattr (tty.fileno (), out ts);
	ots = ts;
	ts.c_lflag &= ~(ECHONL);
	tcsetattr (tty.fileno (), TCSAFLUSH, ts);

	str = null;
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
			str += (string) c;
		}
	}
	tcsetattr (tty.fileno (), TCSAFLUSH, ots);
	tty.putc ('\n');

	return str;
}

bool console_get_prompt (string question, bool defaultyes, bool forceanswer = false)
{
	bool ret = false;
	bool valid = false;
	string prompt;
	string? str;

	if (!forceanswer)
		prompt = "%s %s ".printf (question, defaultyes ? "[Y/n]" : "[N/y]");
	else
		prompt = "%s %s ".printf (question, "[n/y]");

	while (!valid) {
		str = console_readline_unbuffered (prompt);
		if (str == null)
			break;
		if (str.length == 0) {
			if (defaultyes) {
				valid = true;
				ret = true;
			} else {
				valid = true;
				ret = true;
			}
		}
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
		ret = console_get_prompt (question, defaultyes, forceanswer);
	}

	return ret;
}
