{ config, lib, pkgs, ... }:

let
  fn = config.programs.nixvim.extraFunction;

  reload-signal = "69";
  reloadable-nvim = pkgs.writeScriptBin "reloadable-nvim" ''
  #!${pkgs.bash}/bin/bash
  last=${reload-signal}
  while [ $last = ${reload-signal} ] 
  do
    nvim
    last=$?
  done
  '';
in
{
  imports = [
    ./extra-functions.nix
    ./which-key-alias.nix
  ];

  programs.nixvim = {
    enable = true;
    clipboard.providers.wl-copy.enable = true;

    colorschemes.dracula.enable = true;
    colorscheme = "dracula";

    editorconfig.enable = true;

    extraFunctions = {
      leaderbdelete = ''
        local bufnr = vim.api.nvim_get_current_buf()
        local modified = vim.api.nvim_get_option_value("modified", { buf = bufnr })
        if modified then
          vim.ui.input({
            prompt = "... unwritten changes, want to delete buffer? (y/n) ",
          }, function(input)
            if input == "y" then
              vim.cmd("bdelete!")
            end
          end)
        else
          vim.cmd("bdelete")
        end
      '';
    };

    plugins = {
      lsp = {
        enable = true;
        keymaps = {
          lspBuf = {
            K = "hover";
            gD = "references";
            gd = "definition";
            gi = "implementation";
            gt = "type_definition";
          };
          diagnostic = {
            "<leader>cj" = "goto_next";
            "<leader>ck" = "goto_prev";
          };
        };
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
          texlab.enable = true;
        };
      };
      cmp-fuzzy-buffer.enable = true;
      cmp-treesitter.enable = true;
      cmp-nvim-lsp.enable = true;
      cmp-nvim-lsp-document-symbol.enable = true;
      cmp-nvim-lsp-signature-help.enable = true;
      cmp-nvim-lua.enable = true;
      cmp-latex-symbols.enable = true;


      trouble.enable = true;

      nix.enable = true;

      treesitter.enable = true;
      treesitter-refactor.enable = true;

      surround.enable = true;
      undotree.enable = true;
      vimtex.enable = true;
      mini = {
        enable = true;
        modules = {
          ai = {};
          jump2d = {
            mappings = {
              start_jumping = "<leader>jj";
            };
            labels = "asdfjkl;qweruiopzxcvm,./ghbn";
          };
          comment = {};
        };
      };

      rainbow-delimiters.enable = true;

      project-nvim.enable = true;
      telescope = {
        enable = true;
        extensions = {
          file_browser = {
            enable = true;
            hidden = true;
          };
          media_files.enable = true;
          project-nvim.enable = true;
        };
      };
      which-key.enable = true;
      toggleterm.enable = true;

      neogit.enable = true;
      diffview.enable = true;
      gitgutter.enable = true;

      dap.enable = true;

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
              hl = "DraculaPink";
            };
          }
          {
            type = "padding";
            val = 8;
          }
          {
            type = "text";
            val = with builtins; filter isString (split "\n" (import ./wizard.nix));
            opts = {
              position = "center";
              hl = "DraculaPurple";
            };
          }
        ];
      };
    };

    extraPlugins = with pkgs.vimPlugins; [
      lean-nvim
    ];

    extraConfigLuaPre = ''
      vim.mapleader   = ' '
      vim.g.mapleader = ' '
    '';

    keymaps = let
      leader = { key, action, desc ? null, lua ? true, options ? {} }: {
        inherit action lua;
        mode = "n";
        key = "<leader>${key}";
        options = { silent = true; } // options // { inherit desc; };
      };

      defer = body: "(function () return (${body}) end)";
      cmd = fn: defer "vim.cmd('${fn}')";
      tele = fn: cmd "Telescope ${fn}";
      nop = defer "nil";
    in (
      [
        (leader {
          key = " ";
          action = tele "commands";
        })

        (leader {
          key = "ff";
          action = tele "file_browser";
        })

        (leader {
          key = "bb";
          action = tele "buffers";
          desc = "Find buffer";
        })
        (leader {
          key = "bd";
          action = fn.leaderbdelete;
          desc = "Delete buffer";
        })
        (leader {
          key = "bD";
          action = cmd "bdelete!";
          desc = "Force delete buffer";
        })

        (leader {
          key = "w";
          action = defer "wk_alias('<C-w>')";
          desc = "+window";
        })
        {
          mode = "n";
          key = "<C-w>d";
          lua = false;
          action = "<C-w>q";
          options.desc = "Quit window";
        }

        (leader {
          key = "ot";
          action = cmd "ToggleTerm";
          desc = "Toggle terminal";
        })
        {
          mode = "t";
          key = "<Esc><Esc>";
          action = "<C-\\><C-n>";
          lua = false;
          options.desc = "Escape terminal mode";
          options.remap = false;
        }

        (leader {
          key = "sp";
          action = tele "live_grep";
          desc = "Grep";
        })
        (leader {
          key = "sd";
          action = tele "live_grep";
          desc = "Grep";
        })
        (leader {
          key = "sc";
          action = cmd "noh";
          desc = "Clear high";
        })

        (leader {
          key = "jj";
          action = cmd "";
          desc = "Mini jump";
        })

        (leader {
          key = "pp";
          action = tele "projects";
          desc = "Open project";
        })
        (leader {
          key = "pf";
          action = tele "find_files";
          desc = "Find file";
        })

        (leader {
          key = "gg";
          action = cmd "Neogit";
        })
        (leader {
          key = "gs";
          action = cmd "GitGutterStageHunk";
        })


        (leader {
          key = "qq";
          action = cmd "quitall";
          desc = "Quit nvim";
        })
        (leader {
          key = "qr";
          action = cmd "cq ${reload-signal}";
        })
      ]
    );
  };

  home.file = {
  };

  home.packages = with pkgs; [
    neovim-remote
    reloadable-nvim
  ];
}
