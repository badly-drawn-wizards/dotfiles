self: super:
{
  vimPlugins = super.vimPlugins.extend (vself: vsuper: {
    nvim-transparent = self.vimUtils.buildVimPlugin {
      pname = "nvim-transparent";
      version = "2023-11-01";
      src = self.fetchFromGitHub {
        owner = "xiyaowong";
        repo = "transparent.nvim";
        rev = "3af6232c8d39d51062702e875ff6407c1eeb0391";
        sha256 = "sha256-1JyfwHBCtNCPmsOLzJRWBtg1u9uApQZY4fn2mTL3NZ4=";
      };
      meta.homepage = "https://github.com/xiyaowong/transparent.nvim";
    };
    
    nvim-focus = self.vimUtils.buildVimPlugin {
      pname = "nvim-focus";
      version = "1.0.2";
      src = self.fetchFromGitHub {
        owner = "nvim-focus";
        repo = "focus.nvim";
        rev = "c9bc6a969c3ff0d682f389129961c9e71ff2c918";
        sha256 = "sha256-Ak9NZhsPJTZGrxM3jjA5oYMKEsx2uj/Hi/KjGCDFBrI=";
      };
      meta.homepage = "https://github.com/nvim-focus/focus.nvim";
    };
  });
}
