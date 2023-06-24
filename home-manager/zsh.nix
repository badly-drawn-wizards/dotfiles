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
        name = "fast-syntax-highlighting";
        src = "${zsh-fast-syntax-highlighting}/share/zsh/site-functions";
      }
      {
        name = "fzf-tab";
        src = "${zsh-fzf-tab}/share/fzf-tab";
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
        "thefuck"
        "git"
      ];

    };

    initExtraFirst = ''
      SPACESHIP_TIME_SHOW=true
      SPACESHIP_BATTERY_SHOW=true
      SPACESHIP_CHAR_SYMBOL="λ "
      SPACESHIP_PROMPT_ORDER=(
          time           # Time stamps section
          user           # Username section
          dir            # Current directory section
          host           # Hostname section
          git            # Git section (git_branch + git_status)
          hg             # Mercurial section (hg_branch  + hg_status)
          package        # Package version
          node           # Node.js section
          bun            # Bun section
          deno           # Deno section
          ruby           # Ruby section
          python         # Python section
          elm            # Elm section
          elixir         # Elixir section
          xcode          # Xcode section
          swift          # Swift section
          golang         # Go section
          perl           # Perl section
          php            # PHP section
          rust           # Rust section
          haskell        # Haskell Stack section
          scala          # Scala section
          kotlin         # Kotlin section
          java           # Java section
          lua            # Lua section
          dart           # Dart section
          julia          # Julia section
          crystal        # Crystal section
          docker         # Docker section
          docker_compose # Docker section
          aws            # Amazon Web Services section
          gcloud         # Google Cloud Platform section
          azure          # Azure section
          venv           # virtualenv section
          conda          # conda virtualenv section
          dotnet         # .NET section
          ocaml          # OCaml section
          vlang          # V section
          zig            # Zig section
          purescript     # PureScript section
          erlang         # Erlang section
          kubectl        # Kubectl context section
          ansible        # Ansible section
          terraform      # Terraform workspace section
          pulumi         # Pulumi stack section
          ibmcloud       # IBM Cloud section
          nix_shell      # Nix shell
          gnu_screen     # GNU Screen section
          exec_time      # Execution time
          async          # Async jobs indicator
          line_sep       # Line break
          battery        # Battery level and status
          jobs           # Background jobs indicator
          exit_code      # Exit code section
          sudo           # Sudo indicator
          char           # Prompt character
      )
      # Interferes with spaceship-prompt when loaded after it for some reason
      source ${pkgs.fzf-zsh}/share/zsh/plugins/fzf-zsh/fzf-zsh.plugin.zsh

      bindkey -v
      bindkey "^K" history-search-backward
      bindkey "^J" history-search-forward
      bindkey "^P" history-search-backward
      bindkey "^N" history-search-forward
    '';

    initExtra = ''
      stty -ixon
      zstyle ':completion:*' list-colors
      setopt autocd cdable_vars

      function hm-cat() {
        local file="$HOME/$1"
        nix eval "dot#os.hm.home.file.\"$file\".source" --apply builtins.readFile --raw
      }
    '';
  };

  home.file = {
    ".oh-my-zsh/custom/themes".source = pkgs.runCommandLocal "omz-custom-themes" {}
      ''
        mkdir -p $out/spaceship-prompt
        cd $out
        cp ${pkgs.spaceship-prompt}/share/zsh/themes/spaceship.zsh-theme ./spaceship-prompt/
        cp -R ${pkgs.spaceship-prompt}/lib/spaceship-prompt/* ./spaceship-prompt/
        ln -s ./spaceship-prompt/spaceship.zsh-theme
      '';
  };

  home.packages = with pkgs; [
    thefuck
    exa
  ];
}
