{ pkgs
, config
, lib
, ...
}:
{
  programs.zsh = {
    enable = true;
    plugins = with pkgs; [
      {
        name = "nix-shell";
        src = "${zsh-nix-shell}/share/zsh-nix-shell";
      }
      {
        name = "fast-syntax-highlighting";
        src = "${zsh-fast-syntax-highlighting}/share/zsh";
      }
      {
        name = "fzf-tab";
        src = "${zsh-fzf-tab}/share/fzf-tab";
      }
      {
        name = "spaceship-prompt";
        src = "${spaceship-prompt}/share/zsh";
      }
    ];
    package = pkgs.buildEnv {
      name = "zsh-packages";
      paths = with pkgs; [
      ];
    };
    enableCompletion = true;
    enableAutosuggestions = true;
    shellAliases = {
      t = "tmux attach || tmux new";
      ls = "exa";
      dc = "docker-compose";
      nr = "nix repl dot#os";
    };
    dirHashes = {
      org = "$HOME/org";
      ws = "/workspace";
      dot = "/workspace/dotfiles";
    };
    oh-my-zsh = {
      enable = true;
      theme = "spaceship";
      custom = "${config.home.homeDirectory}/.oh-my-zsh/custom";
      plugins = [
        "direnv"
        "thefuck"
        "git"
      ];

    };

    initExtra = ''
    stty -ixon
    zstyle ':completion:*' list-colors
    setopt autocd cdable_vars
    bindkey -v
    bindkey "^P" history-search-backward
    bindkey "^N" history-search-forward
    bindkey "^R" history-incremental-pattern-search-backward
    bindkey "^S" history-incremental-pattern-search-forward

    export MCFLY_KEY_SCHEME=vim
    eval "$(mcfly init zsh)"
  '';
  };

  home.file = {
    ".oh-my-zsh/custom/themes".source = "${pkgs.spaceship-prompt}/share/zsh/themes";
    ".spaceshiprc.zsh".text = ''
    '';
  };

  home.packages = with pkgs; [
    thefuck
    mcfly
    exa
  ];
}
