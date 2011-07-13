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
			assert (split.length > 1); // TODO: Don't use assert, print a nice error message instead
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
 */
public int compare_versions (string a, string b) {
	// easy comparison to see if versions are identical
	if (a == b)
		return 0;

	string? one = a;
	string? two = b;
	uint i = 0;
	uint j = 0;

	while ((i < one.length) && (!one.get (i).isalnum ())) i++;
	while ((j < two.length) && (!two.get (j).isalnum ())) j++;

	/* If we ran to the end of either, we are finished with the loop */
	//if ((i >= one.length) || (j >= two.length)) break;

	/* grab first completely alpha or completely numeric segment */
	/* leave one and two pointing to the start of the alpha or numeric */
	/* segment and walk str1 and str2 to end of segment */
	bool isnum;
	i = 0;
	j = 0;
	if (one.get (0).isdigit ()) {
		while ((i < one.length) && (one.get (i).isdigit ())) i++;
		while ((j < two.length) && (two.get (j).isdigit ())) j++;
		isnum = true;
	} else {
		while ((i < one.length) && (one.get (i).isalpha ())) i++;
		while ((j < two.length) && (two.get (j).isalpha ())) j++;
		isnum = false;
	}

	if (isnum) {
		// throw away any leading zeros - it's a number, right?
		while (one.get (i) == '0') i++;
		while (two.get (j) == '0') j++;

		string str1 = one.substring (i);
		string str2 = two.substring (j);
		/* whichever number has more digits wins */
		if (str1.length > str2.length) return 1;
		if (str2.length > str1.length) return -1;
	}

	/* strcmp will return which one is greater - even if the two */
	/* segments are alpha or if they are numeric.  don't return  */
	/* if they are equal because there might be more segments to */
	/* compare */
	int rc = strcmp (one, two);
	if (rc != 0) return (rc < 1 ? -1 : 1);

	return 0;
}

} // End of namespace
