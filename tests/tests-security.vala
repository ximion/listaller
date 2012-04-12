/* tests-security.vala
 *
 * Copyright (C) 2011-2012 Matthias Klumpp
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
 * 	Matthias Klumpp <matthias@tenstral.net>
 */

using GLib;
using Gee;
using Listaller;

private string datadir;
private string foobar_dir;

void msg (string s) {
	stdout.printf ("%s\n", s);
}

void test_sign_working () {
	GPGSign gsig = new GPGSign ();

	string sign_text;
	gsig.sign_package (Path.build_filename (datadir, "xfile1.bin", null), out sign_text);

	msg ("=========WORKING============\n");
	msg (sign_text);
	msg ("=========WORKING============");

	GPGSignature sigverify = new GPGSignature (sign_text);
	sigverify.verify_package (Path.build_filename (datadir, "xfile1.bin", null));

	debug (sigverify.sigstatus.to_string ());
	assert (sigverify.sigstatus == SignStatus.VALID);
}

void test_sign_failing () {
	GPGSign gsig = new GPGSign ();

	string sign_text;
	gsig.sign_package (Path.build_filename (datadir, "FooBar-1.0_install.ipk", null), out sign_text);

	msg ("============FAILING============");
	msg (sign_text);
	msg ("============FAILING============");

	// We just use some random binary stuff here instaled of a "real" IPK package
	GPGSignature sigverify = new GPGSignature (sign_text);
	sigverify.verify_package (Path.build_filename (datadir, "xfile1.bin", null));

	debug (sigverify.sigstatus.to_string ());
	assert (sigverify.sigstatus == SignStatus.BAD);
}

int main (string[] args) {
	msg ("=== Running Security Tests ===");
	datadir = args[1];
	assert (datadir != null);
	foobar_dir = Path.build_filename (datadir, "foobar", null);
	datadir = Path.build_filename (datadir, "testdata", null);
	assert (FileUtils.test (datadir, FileTest.EXISTS) != false);

	__unittestmode = true;

	Test.init (ref args);
	set_console_mode (true);
	set_verbose_mode (true);
	add_log_domain ("LiTest");

	test_sign_working ();
	test_sign_failing ();

	Test.run ();
	return 0;
}
