{ pkgs
, config
, lib
, ...
}:
{
  programs.zsh = {
    enable = true;
    enableCompletion = true;
    enableAutosuggestions = true;
    shellAliases = {
      t = "tmux attach || tmux new";
      ls = "exa";
      dc = "docker-compose";
    };
    plugins = [
      {
        name = "zsh-syntax-highlighting";
        src = "${pkgs.zsh-syntax-highlighting}/share/zsh-syntax-highlighting";
        file = "zsh-syntax-highlighting.zsh";
      }
    ];
    dirHashes = {
      org = "$HOME/org";
      ws = "/workspace";
      nixos = "/workspace/dotfiles/nixos";
    };
    oh-my-zsh = {
      enable = true;
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

  home.packages = with pkgs; [
    zsh-syntax-highlighting
    thefuck
    mcfly
    exa
  ];
}
