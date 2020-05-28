#!/bin/sh

command -v stow 2&>1 > /dev/null || echo "GNU Stow is required"

stow -S git emacs utilities zsh ssh home-manager
