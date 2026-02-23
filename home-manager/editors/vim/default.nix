{ config, lib, pkgs, ... }:

let
  helpers = config.lib.nixvim;
  inherit (helpers) mkRaw;
in
{
  imports = [
    ./keybinds.nix
    ./plugins.nix
    ./lsp.nix
  ];

  home.shellAliases = {
    vim = "nvim";
    v = "vim";
  };

  programs.nixvim = {
    enable = true;

    # Core Neovim settings
    clipboard = {
      register = "unnamedplus";
      providers.wl-copy.enable = true;
    };

    colorscheme = "dracula";

    # Custom highlight groups using dracula theme colors
    highlight = {
      DraculaPink = mkRaw ''{["fg"] = (require('dracula').colors().pink)}'';
      DraculaPurple = mkRaw ''{["fg"] = (require('dracula').colors().purple)}'';
    };

    editorconfig.enable = true;

    opts = {
      # Display
      number = true;
      relativenumber = false;
      signcolumn = "yes";
      cursorline = true;

      # Behavior
      mouse = "a";

      # Session options for auto-session
      sessionoptions = "blank,buffers,curdir,folds,help,tabpages,winsize,winpos,terminal,localoptions";

      # Folding - treesitter defines folds, but start open
      foldenable = false;

      # Indentation
      expandtab = true;
      shiftwidth = 2;
      tabstop = 2;

      # Search
      ignorecase = true;
      smartcase = true;

      # Performance
      updatetime = 250;
      timeoutlen = 300;
    };

    # Global variables
    globals = {
      mapleader = " ";
      maplocalleader = " m";
    };

    # Terminal escape with timeout
    extraConfigLua = builtins.readFile ./terminal-escape.lua;
  };
}
