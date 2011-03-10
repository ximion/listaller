/* util.vala
 *
 * Copyright (C) 2010-2011  Matthias Klumpp
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
 *
 * Author:
 * 	Matthias Klumpp <matthias@nlinux.org>
 */

using GLib;

public ulong timeval_to_ms (TimeVal time_val) {
	return (((ulong) time_val.tv_sec) * 1000) + (((ulong) time_val.tv_usec) / 1000);
}

public ulong now_ms () {
	return timeval_to_ms (TimeVal());
}

public ulong now_sec () {
	TimeVal time_val = TimeVal ();

	return time_val.tv_sec;
}

private string string_replace (string str, string regex_str, string replace_str) {
	string res = str;
	try {
		var regex = new Regex (regex_str);
		res = regex.replace (str, -1, 0, replace_str);
	} catch (RegexError e) {
		warning ("%s", e.message);
	}
	return res;
}

/*
 * Count the appearance of string b in a
 */
private int count_str (string a, string b) {
	if (!(b in a))
		return 0;

	int count = -1;
	int last_index = 0;

	while (last_index > 0) {
		count++;
		last_index = a.index_of (b, last_index);
	}

	return count;
}

/*
 * Calculate checksum for file
 */
private string compute_checksum_for_file (string fname, ChecksumType cstype = ChecksumType.SHA1) {
	Checksum cs;
	uchar data [1024];
	size_t size = 0;

	cs = new Checksum (cstype);
	Posix.FILE input = Posix.FILE.open (fname, "rb" );

	// Return empty string if we were unable to open the file
	if (input == null) {
		return "";
	}

	// Build the checksum
	do {
		size = Posix.read (input.fileno (), (void*) data, 1024);
		cs.update (data, size);
	} while (size == 1024);
	Posix.close (input.fileno ());

	string sum = cs.get_string ();
	return sum;
}
