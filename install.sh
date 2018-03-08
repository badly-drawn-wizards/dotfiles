#!/bin/sh

command -v stow 2&>1 > /dev/null || echo "GNU Stow is required"

stow -S git i3 spacemacs vimperator utilities X11 zsh ssh pulseaudio
