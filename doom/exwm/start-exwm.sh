#!/bin/bash

# Start ssh-agent
# if [ -z "$SSH_AUTH_SOCK" ] ; then
#    eval `ssh-agent -s`
# fi

# Set up ssh-agent and keychain in Emacs
# if [ -n "$DISPLAY" ] && [ -z "$SSH_AUTH_SOCK" ] && [ -f "/usr/bin/gnome-keyring-daemon" ]; then
#   eval $(/usr/bin/gnome-keyring-daemon --start --components=ssh)
#   export SSH_AUTH_SOCK=$(echo $SSH_AUTH_SOCK | sed "s:/run/user/$UID/keyring/ssh$::")
#   emacsclient -e "(setenv \"SSH_AUTH_SOCK\" \"$SSH_AUTH_SOCK\")" >/dev/null
# fi

# run command 'xinput list' to determine the ID/name of touchpad device
# run command 'xinput -list-props ID' to list properties of device
# enable natural scrolling
# xinput set-prop "pointer:Synaptics TM3276-022" "libinput Natural Scrolling Enabled" 1
# xinput set-prop "pointer:Synaptics TM3276-022" "libinput Tapping Enabled" 1

# start picom first
picom &

# enable screen locking
xss-lock -- slock &

# start emacs
emacs -mm -l ~/.config/doom/exwm/desktop.el
