{ pkgs ? import <nixpkgs>
, config
, lib
, ...
}:
{
  programs.zsh = {
    enable = true;
    enableAutosuggestions = true;
    shellAliases = {
      t = "tmux attach || tmux new";
      vnix = "ranger $HOME/.config/nixpkgs/";
      vnixos = "vim /etc/nixos/configuration.nix";
      doom-install = "nix-env -iA nixos.doom-emacs";
    };
    plugins = [
      {
        name = "zsh-completions";
        src = pkgs.fetchFromGitHub {
          owner = "zsh-users";
          repo = "zsh-completions";
          rev = "cfb2dc47dbfeb68e7bfef2d5a0be9e433fc7bae5";
          sha256 = "17ifhfwn4lgy7899mnqz1g8s2izk0qbi4bphgxzdaz8fk6kavkrl";
        };
      }
      {
        name = "zsh-syntax-highlighting";
        src = pkgs.fetchFromGitHub {
          owner = "zsh-users";
          repo = "zsh-syntax-highlighting";
          rev = "00c0c765509538de207173fc787d364eedd48d6b";
          sha256 = "07carys84ap0s4l7klvnnqaqjfabr281fsbx760hfm6w2x0y1bvm";
        };
      }
      {
        name = "nix-zsh-completions";
        src = pkgs.fetchFromGitHub {
          owner = "spwhitt";
          repo = "nix-zsh-completions";
          rev = "fbbbe6ddedb233d0d0fcc8d665750d183c2b57f8";
          sha256 = "0kn039nv9rcbfvqlzf5vkgg9b14fxm3lvmnnw0rd25rr11mmz85x";
        };
      }
    ];
    oh-my-zsh = {
      enable = true;
      custom = "${./ohmyzsh}";
      theme = "candy";
      plugins = [
        "direnv"
        "thefuck"
        "git"
      ];

    };

    initExtra = ''
    stty -ixon
    zstyle ':completion:*' list-colors
    bindkey -v
    bindkey "^P" history-search-backward
    bindkey "^N" history-search-forward
    bindkey "^R" history-incremental-pattern-search-backward
    bindkey "^S" history-incremental-pattern-search-forward
  '';
  };
}
