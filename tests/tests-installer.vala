/* tests-installer.vala
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

void msg (string s) {
	stdout.printf (s + "\n");
}

void test_setup_message_cb (MessageItem item) {
	msg ("Received message:");
	msg (" " + item.to_string ());
	assert (item.mtype == MessageEnum.INFO);
}

void test_setup_error_code_cb (ErrorItem item) {
	msg ("Received error:");
	msg (" " + item.to_string ());
	// skip all permission-errors
	if (item.error != ErrorEnum.ACTION_NOT_ALLOWED)
		error (item.details);
}

void test_install_package () {
	bool ret = false;
	msg ("Installer tests");

	string ipkfilename = Path.build_filename (datadir, "FooBar-1.0_install.ipk", null);

	// Excludes stuff like PK dependency installing from testing
	__unittestmode = true;
	Report.set_print_fatal_msg (false);

	Setup setup = new Setup (ipkfilename);
	setup.error_code.connect (test_setup_error_code_cb);
	setup.message.connect (test_setup_message_cb);

	ret = setup.initialize ();
	assert (ret == true);

	ret = setup.set_install_mode (IPK.InstallMode.TEST);
	assert (ret == true);

	ret = setup.run_installation ();
	assert (ret == true);
}

int main (string[] args) {
	msg ("=== Running IPK Installer Tests ===");
	datadir = args[1];
	assert (datadir != null);
	datadir = Path.build_filename (datadir, "testdata", null);
	assert (FileUtils.test (datadir, FileTest.EXISTS) != false);

	Test.init (ref args);
	set_console_mode (true);
	set_verbose_mode (true);
	add_log_domain ("LiTest");

	test_install_package ();

	Test.run ();
	return 0;
}
