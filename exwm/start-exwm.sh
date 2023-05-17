#!/bin/bash

# run command 'xinput list' to determine the ID/name of touchpad device
# run command 'xinput -list-props ID' to list properties of device
# enable natural scrolling
xinput set-prop "pointer:Synaptics TM3276-022" "libinput Natural Scrolling Enabled" 1
xinput set-prop "pointer:Synaptics TM3276-022" "libinput Tapping Enabled" 1

# start picom first
picom &

# enable screen locking
xss-lock -- slock &

# start emacs
emacs -mm -l ~/.config/doom/desktop.el
