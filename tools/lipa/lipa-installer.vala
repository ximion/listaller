/* lipa-installer.vala -- Application setup handling in Listaller command-line tool
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
using Config;
using Listaller;

public class LipaInstaller : LipaModule {
	private Setup inst;

	public LipaInstaller () {
		base ();
	}

	public void setup_error_code (ErrorItem error) {
		stderr.printf (error.details);
		error_code = (int) error.error;
	}

	public void setup_progress_changed (int progress, int sub_progress) {
		progress_bar.set_percentage (progress);
		//stdout.printf ("%c8", 0x1B);
		//stdout.printf ("[%i]   ", progress);
		// TODO: Show sub-progress
	}

	public void setup_status_changed (StatusItem status) {
		if (status.status == StatusEnum.INSTALLATION_FINISHED) {
			progress_bar.end ();
			stdout.printf ("Installation completed!\n");
		}
	}

	public void setup_message (MessageItem message) {
		stdout.printf ("%s\n", message.details);
	}

	public void install_package (string ipkfname) {
		bool ret;
		stdout.printf ("Preparing... Please wait!");

		inst = new Setup (ipkfname, liconf);
		inst.message.connect (setup_message);
		inst.status_changed.connect (setup_status_changed);
		inst.progress_changed.connect (setup_progress_changed);
		inst.error_code.connect (setup_error_code);

		ret = inst.initialize ();
		if (!ret) {
			error_code = 8;
			return;
		}

		IPK.Control ipkmeta = inst.control;
		if (ipkmeta == null) {
			error_code = 6;
			return;
		}

		AppItem appID = ipkmeta.get_application ();
		stdout.printf ("%c", 0xD);
		stdout.printf ("==== %s ====\n\n", _("Installation of %s").printf (appID.full_name));

		stdout.printf ("%s\n\n%s\n", _("Description:"), appID.description);
		string[] licenseLines = appID.license.text.split ("\n");
		if (licenseLines.length > 1) {
			stdout.printf ("%s\n\n", _("License:"));
			for (int i = 0; i < licenseLines.length; i++) {
				stdout.printf ("%s\n", licenseLines[i]);
				if ((i % 2) == 1) {
					Posix.FILE? tty = console_get_tty ();
					tty.printf (" <<< Press ENTER to continue! >>>\r");
					tty.flush ();
					// FIXME: For some reason, old console content gets not overridden
					tty.printf ("                                 \r");
					console_wait_for_enter (tty);
					//tty.printf ("                                 \r");
					tty.flush ();
				}
			}
			console_get_prompt ("Do you accept these terms and conditions?", false, true);
		}

		// Display security info
		IPK.PackSecurity sec = inst.get_security_info ();
		SecurityLevel secLev = sec.get_level ();
		if (secLev == SecurityLevel.HIGH)
			stdout.printf ("%s %c[%dm%s\n%c[%dm", _("Security is:"), 0x1B, CONSOLE_GREEN, "HIGH", 0x1B, CONSOLE_RESET);
		else if (secLev == SecurityLevel.MEDIUM)
			stdout.printf ("%s %c[%dm%s\n%c[%dm", _("Security is:"), 0x1B, CONSOLE_YELLOW, "MEDIUM", 0x1B, CONSOLE_RESET);
		else if (secLev <= SecurityLevel.LOW)
			stdout.printf ("%s %c[%dm%s\n%c[%dm", _("Security is:"), 0x1B, CONSOLE_RED, "LOW", 0x1B, CONSOLE_RESET);

		// Make sure color is reset...
		print ("%c[%dm", 0x1B, CONSOLE_RESET);

		AppItem? app = inst.get_current_application ();
		if (app == null)
			error ("Did not receive valid application information!");

		ret = console_get_prompt ("Do you want to install %s now?".printf (app.full_name), true);
		// If user doesn't want to install the application, exit
		if (!ret)
			return;

		progress_bar.start (_("Installing"));
		// Go!
		ret = inst.run_installation ();
		progress_bar.end ();

		if (ret) {
			print ("Installation of %s completed successfully!\n", app.full_name);
		} else {
			print ("Installation of %s failed!", app.full_name);
			error_code = 3;
		}

		inst = null;
	}

	public override void terminate_action () {
		if (inst != null) {
			inst.kill_installation_process ();
			inst = null;
		}
	}

}
