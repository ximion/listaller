/* version.vala
 *
 * Copyright (C) 2011 Matthias Klumpp <matthias@nlinux.org>
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
using Posix;
using Listaller;
using Listaller.Utils;

namespace Listaller {

class VersionNumber : Object {
	private int major;    // x.0.0
	private int minor;    // 0.x.0
	private int revision; // 0.0.x
	private string originalString;

	public VersionNumber (string version)
	{
		originalString = version;
		try
		{
			var regex = new Regex("([[:digit:]]*)\\.([[:digit:]]*)\\.*([[:digit:]]*)");
			var split = regex.split (version);
			GLib.assert (split.length > 1); // TODO: Don't use assert, print a nice error message instead
			major = int.parse (split[1]);
			if (split.length > 2)
			{
				minor = int.parse (split[2]);
			}
			else
			{
				minor = 0;
			}
			if (split.length > 3)
			{
				revision = int.parse (split[3]);
			}
			else
			{
				revision = 0;
			}
		}
		catch (GLib.RegexError e)
		{
			warning ("Error compiling regular expression!");
		}
	}

	public bool newerThan (VersionNumber other)
	{
		if (major > other.major)
		{
			return true;
		}
		else if (major == other.major)
		{
			if (minor > other.minor)
			{
				return true;
			}
			else if (minor == other.minor)
			{
				if (revision > other.revision)
				{
					return true;
				}
			}
		}
		return false;
	}

	public string to_string ()
	{
		return originalString;
	}
}

/* compare alpha and numeric segments of two versions
 * return 1: a is newer than b
 *        0: a and b are the same version
 *       -1: b is newer than a
 *
 * NOTE: This algorithm is used by RPM too [GPLv2]
 */
public int compare_versions (string a, string b) {
	char oldch1, oldch2;
	char[strlen (a) + 1] abuf = { 0 };
	char[strlen (b) + 1] bbuf = { 0 };
	char *str1 = abuf;
	char *str2 = bbuf;
	char *one;
	char *two;

	// easy comparison to see if versions are identical
	if (a == b) return 0;

	strcpy ((string*) str1, a);
	strcpy ((string*) str2, b);

	one = str1;
	two = str2;

	// loop through each version segment of str1 and str2 and compare them
	while ((*one != 0) && (*two != 0)) {
		while ((*one != 0) && (!isalnum (*one))) one++;
		while ((*two != 0) && (!isalnum (*two))) two++;

		// If we ran to the end of either, we are finished with the loop
		if (!((*one != 0) && (*two != 0))) break;

		str1 = one;
		str2 = two;

		/* grab first completely alpha or completely numeric segment
		   leave one and two pointing to the start of the alpha or numeric
		   segment and walk str1 and str2 to end of segment */
		bool isnum = false;
		if (isdigit (*str1)) {
			while ((*str1 != 0) && isdigit (*str1)) str1++;
			while ((*str2 != 0) && isdigit (*str2)) str2++;
			isnum = true;
		} else {
			while ((*str1 != 0) && isalpha (*str1)) str1++;
			while ((*str2 != 0) && isalpha (*str2)) str2++;
			isnum = false;
		}

		/* save character at the end of the alpha or numeric segment
		   so that they can be restored after the comparison */
		oldch1 = *str1;
		*str1 = '\0';
		oldch2 = *str2;
		*str2 = '\0';

		/* this cannot happen, as we previously tested to make sure that
		   the first string has a non-null segment */
		if (one == str1) return -1;	// arbitrary

		/* take care of the case where the two version segments are
		   different types: one numeric, the other alpha (i.e. empty)
		   numeric segments are always newer than alpha segments
		   XXX See RH bugzilla #50977. */
		if (two == str2) return (isnum ? 1 : -1);

		if (isnum) {
			/* this used to be done by converting the digit segments
			   to ints using atoi() - it's changed because long
			   digit segments can overflow an int - this should fix that. */

			// throw away any leading zeros - it's a number, right?
			while (*one == '0') one++;
			while (*two == '0') two++;

			// whichever number has more digits wins
			if (strlen ((string*) one) > strlen ((string*) two)) return 1;
			if (strlen ((string*) two) > strlen ((string*) one)) return -1;
		}

		/* strcmp will return which one is greater - even if the two
		   segments are alpha or if they are numeric.  don't return
		   if they are equal because there might be more segments to
		   compare */
		int rc = Posix.strcmp ((string*) one, (string*) two);
		if (rc != 0) return (rc < 1 ? -1 : 1);

		// restore character that was replaced by null above
		*str1 = oldch1;
		one = str1;
		*str2 = oldch2;
		two = str2;
	}

	/* this catches the case where all numeric and alpha segments have
	   compared identically but the segment sepparating characters were
	   different */
	if ((*one == 0) && (*two == 0)) return 0;

	// whichever version still has characters left over wins
	if (*one == 0) return -1; else return 1;
}

} // End of namespace
