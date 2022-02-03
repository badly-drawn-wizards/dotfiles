{ pkgs
, config
, lib
, ...
}:
let
  typewritten = pkgs.fetchFromGitHub {
    owner = "reobin";
    repo = "typewritten";
    rev = "v1.4.5";
    sha256 = "C3wpfv9qHvmAPxbC00uMdPMwAYpdaf+Ro5ydphgJgBo=";
  };
in
{
  programs.zsh = {
    enable = true;
    enableCompletion = true;
    enableAutosuggestions = true;
    shellAliases = {
      t = "tmux attach || tmux new";
      ls = "exa";
      dc = "docker-compose";
      ne = "nix-eval";
      nattr = "nix-apply builtins.attrNames";
    };
    plugins = [
      {
        name = "typewritten";
        src = typewritten;
        file = "typewritten.zsh";
      }
      {
        name = "zsh-syntax-highlighting";
        src = "${pkgs.zsh-syntax-highlighting}/share/zsh-syntax-highlighting";
        file = "zsh-syntax-highlighting.zsh";
      }
    ];
    dirHashes = {
      org = "$HOME/org";
      ws = "/workspace";
      dot = "/workspace/dotfiles";
    };
    oh-my-zsh = {
      enable = true;
      extraConfig = builtins.readFile ./zsh-theme-extra.zsh;
      theme = "";
      plugins = [
        "direnv"
        "thefuck"
        "git"
      ];

    };

    initExtra = let dollar = "$"; in ''
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

    function nix-apply() {
      local f=$1
      local args=${dollar}{*[2,-2]}
      local x=${dollar}{*[-1]}
      nix-eval $args "($f) ($x)"
    }

    function nix-eval() {
      local expr="${dollar}{*[-1]}"
      local args="${dollar}{*[1,-2]}"
      nix eval /dot#repl --apply "repl: with repl; $expr" $args
    }
  '';
  };

  home.packages = with pkgs; [
    zsh-syntax-highlighting
    thefuck
    mcfly
    exa
  ];
}
