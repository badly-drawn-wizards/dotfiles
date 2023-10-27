self: super:
{
  vimPlugins = super.vimPlugins.extend (vself: vsuper: {
    # nvim-session-lens = self.vimUtils.buildVimPlugin {
    #   pname = "nvim-session-lens";
    #   version = "2023-10-26";
    #   src = self.fetchFromGitHub {
    #     owner = "rmagatti";
    #     repo = "session-lens";
    #     rev = "1b65d8e1bcd1836c5135cce118ba18d662a9dabd";
    #     sha256 = "sha256-ZSzUp3i3JZMwzN2f9nG5zS+qWq0qE2J+djEv042IMI0=";
    #   };
    #   meta.homepage = "https://github.com/rmagatti/session-lens/";
    # };
  });
}
