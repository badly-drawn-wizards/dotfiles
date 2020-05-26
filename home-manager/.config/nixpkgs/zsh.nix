{
  enable = true;
  defaultKeymap = "viins";
  shellAliases = {
    t = "tmux attach || tmux new";
  };
  oh-my-zsh = {
    enable = true;
    theme = "candy";
    plugins = [ 
      "direnv" 
      "thefuck" 
      "git"
      #"zsh-users/zsh-completions"
      #"zsh-users/zsh-syntax-highlighting"
      #"spwhitt/nix-zsh-completions"
    ];

  };

  initExtra = ''
    stty -ixon
    zstyle ':completion:*' list-colors
    bindkey "^P" history-search-backward
    bindkey "^N" history-search-forward
    bindkey "^R" history-incremental-pattern-search-backward
    bindkey "^S" history-incremental-pattern-search-forward
  '';
}
