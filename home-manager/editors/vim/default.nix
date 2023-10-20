{ config, lib, pkgs, ... }:

{
  programs.nixvim = {
    enable = true;
    clipboard.providers.wl-copy.enable = true;

    colorschemes.dracula.enable = true;
    colorscheme = "dracula";

    editorconfig.enable = true;

    plugins = {
      lsp = {
        enable = true;
        servers = {
          clangd.enable = true;
          nixd.enable = true;
          pylsp.enable = true;
          omnisharp.enable = true;
          terraformls.enable = true;
          tsserver.enable = true;
          cssls.enable = true;
          tailwindcss.enable = true;
          yamlls.enable = true;
          hls.enable = true;
          lua-ls.enable = true;
          metals.enable = true;
          rust-analyzer.enable = true;
        };
      };
      nix.enable = true;
      treesitter.enable = true;
      treesitter-refactor.enable = true;
      rainbow-delimiters.enable = true;
      surround.enable = true;
      telescope.enable = true;
      toggleterm.enable = true;

      neogit.enable = true;
      diffview.enable = true;

      trouble.enable = true;
      gitgutter.enable = true;
      neorg.enable = true;
      lightline = {
        enable = true;
      };
      alpha = {
        enable = true;
        iconsEnabled = true;
        layout = [
          {
            type = "padding";
            val = 4;
          }
          {
            type = "text";
            val = [
              "   _    _    __                                  _   "
              "  F L  J J   LJ   _____     ___ _    _ ___    ___FJ  "
              " J J .. L L      [__   F   F __` L  J '__ \", F __  L "
              " | |/  \| |  FJ  `-.'.'/  | |--| |  | |__|-J| |--| | "
              " F   /\   J J  L .' (_(_  F L__J J  F L  `-'F L__J J "
              "J___//\\___LJ__LJ_______LJ\____,__LJ__L    J\____,__L"
              "|___/  \___||__||_______| J____,__F|__L     J____,__F"
            ];

            opts = {
              position = "center";
              hl = "Type";
            };
          }
        ];
      };

      which-key = {
        enable = true;
      };
      project-nvim = {
        enable = true;
      };
    };

    extraPlugins = with pkgs.vimPlugins; [
      lean-nvim
    ];

    extraConfigLuaPre = ''
      vim.mapleader   = ' '
      vim.g.mapleader = ' '
      vim.go.autochdir = true
    '';

    keymaps = let
      leader = { key, action, desc ? null, lua ? true }: {
        inherit action lua;
        mode = "n";
        key = "<leader>${key}";
        options = {
          inherit desc;
          silent = true;
        };
      };
      tele = fn: "require('telescope.builtin').${fn}";
    in
      [
        (leader {
          key = "ff";
          action = tele "find_files";
          desc = "Find file";
        })
        (leader {
          key = "bb";
          action = tele "buffers";
          desc = "Find buffer";
        })
        (leader {
          key = "bd";
          action = "vim.cmd.bdelete";
          desc = "Delete buffer";
        })
        (leader {
          key = "q";
          action = "vim.cmd.quitall";
          desc = "Quit nvim";
        })
    ];
  };

  home.file = {
  };

  home.packages = with pkgs; [
    neovim-remote
  ];
}
