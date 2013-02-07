/* tests-keydb.vala
 *
 * Copyright (C) 2012-2013 Matthias Klumpp <matthias@tenstral.net>
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

void test_keydb () {
	msg ("~~ Testing KeyDB ~~");
	KeyManager keymgr = new KeyManager ();

	var k = keymgr.lookup_key ("0xBF4DECEB");
	assert (k != null);
	msg (k.uids->name);
	assert (k.uids->name == "Matthias Klumpp");
}

void test_signature_validation () {
	msg ("~~ Testing signature validation ~~");
	bool ret;

	string sigtext = Utils.load_file_to_string (Path.build_filename (datadir, "sigtest-signature.asc", null));
	string pkgdata_path = Path.build_filename (datadir, "sigtest-control.tar.xz", null);

	var sign = new GPGSignature (sigtext);
	ret = sign.verify_package (pkgdata_path);
	assert (ret == true);
	assert (sign.valid == true);
	assert (sign.sigstatus == SignStatus.VALID);

	debug ("Signature key fingerprint: %s", sign.key_fpr);
	assert (sign.key_fpr == "D33A3F0CA16B0ACC51A60738494C8A5FBF4DECEB");

	debug ("Signature signer user-ids: \n%s", sign.user_names);
	debug ("Signature trust level: %s", sign.trust_level.to_string ());
}

int main (string[] args) {
	msg ("=== Running KeyDB Tests ===");
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

	test_keydb ();
	test_signature_validation ();

	Test.run ();
	return 0;
}
