#!/usr/bin/env bash

set -euo pipefail

PHASE=${PHASE:-0}
NIX="nix --extra-experimental-features nix-command --extra-experimental-features flakes"

if [ $PHASE -e 1 ]
  # TODO Git clone
  # TODO Setup filesystem

  # Symlink flake
  ln -st /etc/nixos /workspace/dotfiles/flake.nix
fi


if [ "$EUID" -ne 0 ]
then 
  echo "Please run as root"
  exit
fi

exec $NIX shell nixpkgs#bash nixpkgs#git --command bash -c "PHASE=1 $0"



