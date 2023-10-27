{ config, lib, pkgs, ... }:

let
  inherit (config.nixvim.helpers) mkRaw;
  fn = config.programs.nixvim.extraFunction;

  reload-signal = "69";
  reloadable-session = "reloadable-session";
  reloadable-nvim = pkgs.writeScriptBin "reloadable-nvim" ''
  #!${pkgs.bash}/bin/bash
  nvim "$@"
  if [ $? = ${reload-signal} ] 
  then
    exec reloadable-nvim -c "lua ${fn.reloadRestore}()"
  fi
  '';
in
{
  imports = [
    ./extra-functions.nix
    ./which-key-alias.nix
  ];

  programs.nixvim = {
    enable = true;
    luaLoader.enable = true;

    clipboard.providers.wl-copy.enable = true;

    colorschemes.dracula.enable = true;
    colorscheme = "dracula";

    editorconfig.enable = true;

    autoCmd = [
      {
        event = ["VimLeavePre"];
        command = "silent lua ${fn.reloadVimLeaveHook}()";
      }
    ];

    extraFunctions = {
      reload = ''
        isReloading = true
        require('neo-tree').close_all()
        require("auto-session").SaveSession("${reloadable-session}", false)
        vim.cmd("confirm qa")
        isReloading = false
      '';
      reloadVimLeaveHook = ''
        if isReloading then
          vim.cmd("cq ${reload-signal}")
        end
      '';
      reloadRestore = ''
        require('auto-session').RestoreSessionFromFile('${reloadable-session}')
      '';
      cmp_tab_trigger = ["fallback" ''
        local c = require('cmp')
        if c.visible() then
          c.select_next_item()
        else
          fallback()
        end
      ''];
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
            "<M-CR>" = "code_action";
          };
          diagnostic = {
            "<leader>cj" = "goto_next";
            "<leader>ck" = "goto_prev";
          };
        };
        servers = {
          clangd.enable = true;
          nixd = {
            enable = true;
            settings = {
              options.enable = true;
              formatting.command = "nixpkgs-fmt";
            };
            rootDir = "require('lspconfig.util').root_pattern('.nixd.json', '.git')";
          };
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

      vimtex.enable = true;

      nvim-cmp = {
        enable = true;
        autoEnableSources = true;
        mapping = {
          "<CR>" = "cmp.mapping.confirm({ select = true })";
          "<Tab>" = {
            modes = ["i" "s"];
            action = fn.cmp_tab_trigger;
          };
          "<C-Space>" = "cmp.mapping.complete()";
        };
        sorting.comparators = let
          uglyRaw = expr: "order or (${expr})";
        in [ 
          (uglyRaw "require('cmp_fuzzy_buffer.compare')")
          "offset" "exact" "score" "recently_used" "locality" "kind" "length" "order" 
        ];
        sources = [
          { name = "nvim_lsp"; groupIndex = 1; }
          { name = "nvim_lsp_document_symbol"; groupIndex = 1; }
          { name = "nvim_lsp_signature_help"; groupIndex = 1; }
          { name = "nvim_lua"; groupIndex = 2; }
          { name = "latex_symbols"; groupIndex = 2; }
          { name = "tree_sitter"; groupIndex = 2; }
          { name = "fuzzy_buffer"; groupIndex = 3; keywordLength = 4; }
        ];
      };

      trouble.enable = true;

      nix.enable = true;

      treesitter = {
        enable = true;
        package = pkgs.vimPlugins.nvim-treesitter;
        folding = true;
      };
      treesitter-refactor.enable = true;

      surround.enable = true;
      undotree.enable = true;
      mini = {
        enable = true;
        modules = {
          ai = {};
          jump2d = {
            mappings = {
              start_jumping = "";
            };
            labels = "asdfjkl;qweruiopzxcvm,./ghbn";
          };
          comment = {};
        };
      };

      rainbow-delimiters.enable = true;

      project-nvim.enable = true;
      auto-session = {
        enable = true;
        logLevel = "error";
        autoSession = {
          enabled = true;
          enableLastSession = true;
        };
        autoSave.enabled = true;
        autoRestore.enabled = false;
        sessionLens = {
          loadOnSetup = true;
        };
      };

      telescope = {
        enable = true;
        extraOptions.defaults.mappings.i = {
          "<C-j>" = mkRaw "require('telescope.actions').move_selection_next";
          "<C-k>" = mkRaw "require('telescope.actions').move_selection_previous";
        };
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

      neo-tree.enable = true;

      neogit.enable = true;
      diffview.enable = true;
      gitgutter.enable = true;

      toggleterm.enable = true;

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
      nvim-web-devicons
      vim-autoswap
    ];

    extraConfigLuaPre = ''
      vim.mapleader   = ' '
      vim.g.mapleader = ' '
      vim.o.sessionoptions="blank,buffers,curdir,folds,help,tabpages,winsize,winpos,terminal,localoptions"
      vim.o.foldenable = false
      vim.g.editorconfig = {
        indent_style = "space",
        indent_size = 2
      }
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
          key = ".";
          action = cmd "Neotree position=current dir=%:p:h";
        })
        (leader {
          key = "ff";
          action = tele "file_browser";
        })
        (leader {
          key = "fed";
          action = cmd "e ~/.config/nvim/init.lua";
        })

        (leader {
          key = "bb";
          action = tele "buffers";
          desc = "Find buffer";
        })
        (leader {
          key = "bd";
          action = cmd "confirm bdelete";
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
          key = "sc";
          action = cmd "nohlsearch";
          desc = "Clear search highlight";
        })
        (leader {
          key = "ss";
          action = tele "treesitter";
          desc = "LSP treesitter";
        })
        (leader {
          key = "sD";
          action = tele "lsp_references";
          desc = "LSP references";
        })
        (leader {
          key = "sld";
          action = tele "lsp_definitions";
          desc = "LSP definitions";
        })
        (leader {
          key = "sli";
          action = tele "lsp_implmentations";
          desc = "LSP implementations";
        })
        (leader {
          key = "slt";
          action = tele "lsp_type_definitions";
          desc = "LSP type definitions";
        })
        (leader {
          key = "sls";
          action = tele "lsp_workspace_symbols";
          desc = "LSP symbols";
        })
        (leader {
          key = "slci";
          action = tele "lsp_incoming_calls";
          desc = "LSP incoming calls";
        })
        (leader {
          key = "slco";
          action = tele "lsp_outgoing_calls";
          desc = "LSP outgoing calls";
        })

        (leader {
          key = "jj";
          action = defer "MiniJump2d.start(MiniJump2d.builtin_opts.word_start)";
          desc = "Mini jump";
        })
        (leader {
          key = "jl";
          action = defer "MiniJump2d.start(MiniJump2d.builtin_opts.line_start)";
          desc = "Mini jump";
        })

        (leader {
          key = "pp";
          action = tele "projects";
          desc = "Open project";
        })
        (leader {
          key = "pa";
          action = cmd "AddProject";
          desc = "Open project";
        })
        (leader {
          key = "ps";
          action = defer "require('auto-session.session-lens').search_session()";
          desc = "Search session";
        })
        (leader {
          key = "pf";
          action = tele "find_files hidden=true";
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
          key = "gS";
          action = cmd "!git stage %";
        })

        (leader {
          key = "qq";
          action = cmd "confirm qa";
          desc = "Quit nvim";
        })
        (leader {
          key = "qQ";
          action = cmd "qa!";
          desc = "Quit nvim";
        })
        (leader {
          key = "qr";
          action = defer "reload()";
        })
      ]
    );
  };

  programs.zsh.shellAliases = {
    vim = "reloadable-nvim";
    v = "vim";
  };

  home.file = {
  };

  home.packages = with pkgs; [
    neovim-remote
    reloadable-nvim
    chafa
  ];
}
