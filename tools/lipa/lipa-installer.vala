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
using Listaller;

public class LipaInstaller : LipaModule {
	private Setup inst;
	private bool setup_running = false;

	public LipaInstaller () {
		base ();
	}

	public void setup_error_code (ErrorItem error) {
		// End progress, if any
		progress_bar.end ();

		stderr.printf (error.details);
		error_code = (int) error.error;
	}

	public void setup_progress_changed (int progress) {
		if (setup_running)
			progress_bar.set_percentage (progress);

		// TODO: Show item-progress too
	}

	public void setup_status_changed (StatusItem status) {
		if (status.status == StatusEnum.INSTALLATION_FINISHED) {
			progress_bar.end ();
			print ("Installation completed!\n");
			setup_running = false;
		} else if (status.status == StatusEnum.ACTION_STARTED) {
			setup_running = true;
		}
	}

	public void setup_message (MessageItem message) {
		stdout.printf ("%s\n", message.details);
	}

	public void install_package (string ipkfname) {
		bool ret;
		print ("Preparing... Please wait!");

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
		//print ("%c8", 0x1B);
		print ("==== %s ====\n\n", _("Installation of %s").printf (appID.full_name));

		print ("%s\n\n%s\n", _("Description:"), appID.description);
		string[] licenseLines = appID.license.text.split ("\n");

		// save cursor in new position
		//print ("%c7", 0x1B);

		if (licenseLines.length > 1) {
			print ("%s\n\n", _("License:"));
			for (int i = 0; i < licenseLines.length; i++) {
				print ("%s\r\r", licenseLines[i]);
				if ((i % 2) == 1) {
					Posix.FILE? tty = console_get_tty ();
					// TODO: Text does not get erasted for some reason...
					// => Implement this properly!
					//print (" <<< Press ENTER to continue! >>>\r");
					console_wait_for_enter (tty);
				}
			}
			console_get_prompt (_("Do you accept these terms and conditions?"), false, true);
		}

		// Display security info
		IPK.PackSecurity sec = inst.get_security_info ();
		SecurityLevel secLev = sec.get_level ();
		if (secLev == SecurityLevel.HIGH)
			print ("%s %c[%dm%s\n%c[%dm", _("Security is:"), 0x1B, CONSOLE_GREEN, "HIGH", 0x1B, CONSOLE_RESET);
		else if (secLev == SecurityLevel.MEDIUM)
			print ("%s %c[%dm%s\n%c[%dm", _("Security is:"), 0x1B, CONSOLE_YELLOW, "MEDIUM", 0x1B, CONSOLE_RESET);
		else if (secLev <= SecurityLevel.LOW)
			print ("%s %c[%dm%s\n%c[%dm", _("Security is:"), 0x1B, CONSOLE_RED, "LOW", 0x1B, CONSOLE_RESET);

		// Make sure color is reset...
		print ("%c[%dm", 0x1B, CONSOLE_RESET);

		AppItem? app = inst.get_current_application ();
		if (app == null)
			error ("Did not receive valid application information!");

		ret = console_get_prompt (_("Do you want to install %s now?").printf (app.full_name), true);
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
			if (setup_running)
				inst.kill_installation_process ();
			inst = null;
		}
	}

}
