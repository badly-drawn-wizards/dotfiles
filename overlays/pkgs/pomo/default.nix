# https://github.com/kevinschoon/pomo
{ lib, buildGoModule, fetchFromGitHub }:

buildGoModule rec {
  pname = "pomo";
  version = "0.8.1";

  src = fetchFromGitHub {
    owner = "kevinschoon";
    repo = "pomo";
    rev = "${version}";
    hash = "sha256-w9H6i46Fv5qB3nbl0j7/LvmMt+Dgvlg1fzAL1vtETX0=";
  };

  vendorHash = "sha256-i97+VWrCA+1r5u8fXc0fZ2shl1lSDfgr6PUohjrBI30=";

  doCheck = false;
}
