#!/bin/sh

i3-msg "workspace org"
if ! xwininfo -name org
then
    em -c $HOME/org/main.org
fi
