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
  });
}
