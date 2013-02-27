/* tests-signing.vala
 *
 * Copyright (C) 2011-2013 Matthias Klumpp <matthias@tenstral.net>
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
	gsig.sign_package (Path.build_filename (datadir, "FooBar-1.0_%s.ipk".printf (Utils.system_machine_generic ()), null), out sign_text);

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
	msg ("=== Running Signature Tests ===");
	datadir = args[1];
	assert (datadir != null);
	foobar_dir = Path.build_filename (datadir, "foobar", null);
	datadir = Path.build_filename (datadir, "testdata", null);
	assert (FileUtils.test (datadir, FileTest.EXISTS) != false);

	var tenv = new TestEnvironment ("signing");
	tenv.init (ref args);
	// NOTE We don't create the testing environment here, because we need the real one to test GPG
	//      Maybe we can set-up a new GPG testing env later, but for now this test is better than none.

	test_sign_working ();
	test_sign_failing ();

	tenv.run ();

	return 0;
}
