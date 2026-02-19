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
      autochdir = true;
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
    extraConfigLua = ''
      _G.terminal_escape = function()
        local timeout = 300 -- milliseconds
        local timer_expired = false

        -- Start timer
        local timer = vim.fn.timer_start(timeout, function()
          timer_expired = true
        end)

        -- Wait for next character (non-blocking with timeout)
        local char = vim.fn.getchar(0)

        if char == 0 then
          -- No character pressed, wait for timer
          vim.wait(timeout, function() return timer_expired end)
          vim.fn.timer_stop(timer)
          -- Send escape to terminal
          vim.api.nvim_feedkeys(vim.api.nvim_replace_termcodes('<Esc>', true, false, true), 't', false)
        else
          -- Character pressed, stop timer
          vim.fn.timer_stop(timer)

          -- Convert character code to string if needed
          local key = type(char) == 'number' and vim.fn.nr2char(char) or char

          -- Check if it's another escape (27 is ESC key code)
          if char == 27 then
            -- Double escape - exit terminal mode
            vim.cmd('stopinsert')
          else
            -- Other key - send escape + key to terminal
            vim.api.nvim_feedkeys(vim.api.nvim_replace_termcodes('<Esc>' .. key, true, false, true), 't', false)
          end
        end
      end
    '';
  };
}
