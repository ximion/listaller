#!/bin/bash

# Setting up directory where the Package is installed
MYDIR=$1

# Setting up default preferences 
PAP_ENABLE_SOUND="yes"
PAP_ENABLE_MUSIC="yes"
PAP_DISPLAY_MODE="window"
PAP_KEYBOARD="with_num_pad"
PAP_USE_JOYSTIKS="auto"

# Loading users prefrences
if [ -f "$HOME/.PersonalAppPreferences" ]; then
	source "$HOME/.PersonalAppPreferences"
fi

# Entering Application Directory
cd $MYDIR

# ===========
# Main Script
# ===========

# Enabeling preferences

case $PAP_ENABLE_SOUND in
	"yes")
		# Snip 8< --------------------------------------------------------------
		# Replace this code by the parameter setting for your application
		echo "Setting sound enabled"
		# Snap 8< --------------------------------------------------------------
	;;
	"no")
		# Snip 8< --------------------------------------------------------------
		# Replace this code by the parameter setting for your application
		echo "Setting sound disabled"
		# Snap 8< --------------------------------------------------------------
	;;
	*)
		# Snip 8< --------------------------------------------------------------
		# Replace this code by the parameter setting for your application
		echo "Setting sound enabled, because it is the default setting"
		# Snap 8< --------------------------------------------------------------
	;;
esac

case $PAP_ENABLE_MUSIC in
	"yes")
		# Snip 8< --------------------------------------------------------------
		# Replace this code by the parameter setting for your application
		echo "Setting background music enabled"
		# Snap 8< --------------------------------------------------------------
	;;
	"no")
		# Snip 8< --------------------------------------------------------------
		# Replace this code by the parameter setting for your application
		echo "Setting background music enabled"
		# Snap 8< --------------------------------------------------------------
	;;
	*)
		# Snip 8< --------------------------------------------------------------
		# Replace this code by the parameter setting for your application
		echo "Setting background music enabled, because it is the default setting"
		# Snap 8< --------------------------------------------------------------
	;;
esac

case $PAP_DISPLAY_MODE in
	"window")
		# Snip 8< --------------------------------------------------------------
		# Replace this code by the parameter setting for your application
		echo "Taking care that the app starts in a window"
		# Snap 8< --------------------------------------------------------------
	;;
	"fullscreen")
		# Snip 8< --------------------------------------------------------------
		# Replace this code by the parameter setting for your application
		echo "Taking care that the app starts fullscreen"
		# Snap 8< --------------------------------------------------------------
	;;
	*)
		# Snip 8< --------------------------------------------------------------
		# Replace this code by the parameter setting for your application
		echo "Taking care that the app starts in a window, because it is the default setting"
		# Snap 8< --------------------------------------------------------------
	;;
esac

case $PAP_KEYBOARD in
	"with_num_pad")
		# Snip 8< --------------------------------------------------------------
		# Replace this code by the parameter setting for your application
		echo "There is a full equipted keyboard, that can be used to emulate joystics."
		# Snap 8< --------------------------------------------------------------
	;;
	"without_num_pad")
		# Snip 8< --------------------------------------------------------------
		# Replace this code by the parameter setting for your application
		echo "The keyboard has no numpad. That fact has to be considered."
		# Snap 8< --------------------------------------------------------------
	;;
	*)
		# Snip 8< --------------------------------------------------------------
		# Replace this code by the parameter setting for your application
		echo "There is a full equipted keyboard expected by default."
		# Snap 8< --------------------------------------------------------------
	;;
esac

case $PAP_USE_JOYSTIKS in
	"none")
		# Snip 8< --------------------------------------------------------------
		# Replace this code by the parameter setting for your application
		echo "No joysticks will be used."
		# Snap 8< --------------------------------------------------------------
	;;
	"one")
		# Snip 8< --------------------------------------------------------------
		# Replace this code by the parameter setting for your application
		echo "Reqiering and using one joystick"
		# Snap 8< --------------------------------------------------------------
	;;
	"two")
		# Snip 8< --------------------------------------------------------------
		# Replace this code by the parameter setting for your application
		echo "Requierering and using two joysticks."
		# Snap 8< --------------------------------------------------------------
	;;
	"auto")
		# Snip 8< --------------------------------------------------------------
		# Replace this code by the parameter setting for your application
		echo "Detecting how many joystiks are connected and useing as much as possible by default."
		# Snap 8< --------------------------------------------------------------
	;;
	*)
		# Snip 8< --------------------------------------------------------------
		# Replace this code by the parameter setting for your application
		echo "Detecting how many joystiks are connected and useing as much as possible by default."
		# Snap 8< --------------------------------------------------------------
	;;
esac

# Running the application

# Snip 8< --------------------------------------------------------------
# Replace this code by the parameter setting for your application
zenity --info --title="Testing run.sh" --text="MYDIR=$MYDIR\nPAP_ENABLE_SOUND=$PAP_ENABLE_SOUND\nPAP_ENABLE_MUSIC=$PAP_ENABLE_MUSIC\nPAP_DISPLAY_MODE=$PAP_DISPLAY_MODE\nPAP_KEYBOARD=$PAP_KEYBOARD\nPAP_USE_JOYSTIKS=$PAP_USE_JOYSTIKS"
# Snap 8< --------------------------------------------------------------
