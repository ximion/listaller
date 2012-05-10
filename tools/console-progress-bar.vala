/* console-progress-bar.vala -- Show a progress bar in text-terminal
 *
 * Copyright (C) 2012 Matthias Klumpp
 *                    Richard Hughes <richard@hughsie.com>
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

public static const int CMD_PROGRESS_BAR_PERCENTAGE_INVALID = 101;
public static const int CMD_PROGRESS_BAR_PULSE_TIMEOUT = 40; // milliseconds

public class CmdProgressBar : Object {
	private struct PulseState {
		uint position;
		bool move_forward;
	}

	private uint size;
	private uint padding;
	private uint timer_id;
	private int percentage;
	private PulseState pulse_state;

	public CmdProgressBar () {
		size = 10;
		percentage = -1;
		padding = 0;
		timer_id = 0;
		pulse_state = PulseState ();
	}

	public bool set_size (uint size) {
		return_val_if_fail (size < 100, false);
		this.size = size;
		return true;
	}

	public bool set_padding (uint padding) {
		return_val_if_fail (padding < 100, false);
		this.padding = padding;
		return true;
	}

	private bool draw (int pvalue) {
		// no value yet
		if (pvalue == -1)
			return false;

		uint section;

		// restore cursor
		print ("%c8", 0x1B);
		section = (uint) ((float) size / (float) 100.0 * (float) pvalue);
		print ("[");

		int i;
		for (i=0; i < section; i++)
			print ("=");
		for (i=0; i < size - section; i++)
			print (" ");
		print ("] ");
		if (pvalue >= 0 && pvalue < 100)
			print ("(%i%%)  ", percentage);
		else
			print ("        ");

		return true;
	}

	private bool pulse_bar () {
		// restore cursor
		print ("%c8", 0x1B);

		if (pulse_state.move_forward) {
			if (pulse_state.position == size - 1)
				pulse_state.move_forward = false;
			else
				pulse_state.position++;
		} else if (!pulse_state.move_forward) {
			if (pulse_state.position == 1)
				pulse_state.move_forward = true;
			else
				pulse_state.position--;
		}

		print ("[");
		int i;
		for (i=0; i<(int) pulse_state.position-1; i++)
			print (" ");
		print ("==");
		for (i=0; i<(int) (size - pulse_state.position - 1); i++)
			print (" ");
		print ("] ");
		if (percentage >= 0 && percentage != CMD_PROGRESS_BAR_PERCENTAGE_INVALID)
			print ("(%i%%)  ", percentage);
		else
			print ("        ");

		return true;
	}

	private void draw_pulse_bar () {
		// have we already got zero percent?
		if (timer_id != 0)
			return;
		if (true) {
			pulse_state.position = 1;
			pulse_state.move_forward = true;
			timer_id = Timeout.add (CMD_PROGRESS_BAR_PULSE_TIMEOUT, pulse_bar);
			// FIXME: Fix Vala bug to make this work again (g_source_set_name_by_id)
			// MainContext.set_name_by_id (timer_id, "[CmdProgressBar] pulse");
		}
	}

	public bool set_percentage (int percentage) {
		return_val_if_fail (percentage <= CMD_PROGRESS_BAR_PERCENTAGE_INVALID, false);

		// never called start()
		if (this.percentage == -1)
			start ("FIXME: need to call pk_progress_bar_start() earlier!");

		// check for old percentage
		if (percentage == this.percentage) {
			return true;
		}

		// save
		this.percentage = percentage;

		// either pulse or display
		if (percentage < 0 || percentage > 100) {
			draw (0);
			draw_pulse_bar ();
		} else {
			if (timer_id != 0) {
				Source.remove (timer_id);
				timer_id = 0;
			}
			draw (percentage);
		}

		return true;
	}

	public bool start (string text) {
		string text_pad;

		// finish old value
		if (percentage != -1) {
			draw (100);
			print ("\n");
		}

		// make these all the same length
		text_pad = cmd_strpad (text, padding);
		print ("%s", text_pad);

		// save cursor in new position
		print ("%c7", 0x1B);

		// reset
		if (percentage == -1)
			percentage = 0;
		draw (0);

		return true;
	}

	public bool end () {
		// never drawn
		if (percentage == -1)
			return false;

		percentage = -1;
		draw (100);
		print ("\n");

		return true;
	}
}