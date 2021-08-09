self: super: {
  rot8 = super.rustPlatform.buildRustPackage {
    pname = "rot8";
    version = "0.1.3";

    cargoSha256 = "sMFa3Pe/JC1sWeeN40RBXNRV02jDyhDlb+eTvATJLfM=";

    src = super.fetchFromGitHub {
      owner = "efernau";
      repo = "rot8";
      rev = "e8957f0c6035a8873a968e7a13a43790f149ce89";
      sha256 = "jel8TO/QbBtS1+o3N6qvp8ju9B9w15uSBJOooVKLCWU=";
      fetchSubmodules = true;
    };
  };
}
