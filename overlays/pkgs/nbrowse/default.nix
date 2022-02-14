{ buildDotnetModule
, dotnetCorePackages
, autoPatchelfHook
, fetchFromGitHub
, gcc-unwrapped
, libstdcxx5
, stdenv
, ... }:

assert stdenv.isLinux;
buildDotnetModule rec {
  pname = "nbrowse";
  version = "0.5.1";

  nativeBuildInputs = [ autoPatchelfHook ];
  propagatedBuildInputs = [ gcc-unwrapped.lib ];

  src = fetchFromGitHub {
    owner = "r3c";
    repo = pname;
    rev = "356b85dfcb92c5e89d96971b3e24ed22e8d57f79";
    sha256 = "WbCk6g3qh9F9mRbXkj4C31GxLiIFIm2UyaiVhgKGamo=";
  };

  nugetDeps = ./deps.nix;
  dotnet-runtime = dotnetCorePackages.runtime_3_1;
  projectFile = ["./NBrowse/NBrowse.csproj" "./NBrowse.CLI/NBrowse.CLI.csproj"];
  executables = [ "NBrowse.CLI" ];

  postFixup = ''
  mv "$out/bin/NBrowse.CLI" "$out/bin/nbrowse"
  '';
}
