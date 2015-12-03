#!/bin/sh

i3-msg 'workspace org'
if ! xwininfo -name org
then
    i3-msg 'append_layout /home/reuben/.i3/org-layout.json'
    emacs --name org ~/org/main.org
fi
