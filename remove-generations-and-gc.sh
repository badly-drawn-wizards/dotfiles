#!/usr/bin/env sh
nix-env --delete-generations +3 --profile /nix/var/nix/profiles/system
nix-collect-garbage
