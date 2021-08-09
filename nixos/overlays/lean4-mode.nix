self: super:
{
  emacsPackagesFor = emacs: (super.emacsPackagesFor emacs).overrideScope' (eself: esuper: {
    lean4-mode = esuper.trivialBuild {
      pname = "lean4-mode";
      version = "v4.0.0-m2";
      src = super.pkgs.fetchFromGitHub {
        owner = "leanprover";
        repo = "lean4";
        rev = "26dda3f63d885e8c22888926bdea0d99f58bf444";
        sha256 = "e0bDkcyd8PYzU1KuPkgZFgC/bPTC9fuFQzc6mMzL9LY=";
        fetchSubmodules = true;
      };
      sourceRoot = "source/lean4-mode";
      packageRequires = with eself; [ dash dash-functional f flycheck lsp-mode magit-section s ];
    };
  });
}
