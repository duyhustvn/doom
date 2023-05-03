#!/bin/bash

# start picom first
picom &

# enable screen locking
xss-lock -- slock &

# start emacs
emacs -mm
