{ pkgs, mach-nix, fetchFromGitHub, applyPatches }:

mach-nix.buildPythonApplication rec {
  pname = "lightnovel-crawler";
  version = "2.28.11";
  src = applyPatches {
      src = fetchFromGitHub {
        owner = "dipu-bd";
        repo = pname;
        rev = "v${version}";
        sha256 = "1jqxad820pzl6ddql5kd5s4cq6jkywb8xbj824wypnc0sv61fjfg";
      };
      patches = [ ./1247.patch ];
  };
}
