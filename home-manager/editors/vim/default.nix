{ config, lib, pkgs, ... }:

let
  cfg = config.nixvim;
  inherit (cfg.helpers) mkRaw toLuaObject;
  fn = config.programs.nixvim.extraFunction;

in
{
  imports = [
    ./extra-functions.nix
    ./which-key-alias.nix
    ./reloadable-session.nix
    ./transparent.nix
  ];

  programs.nixvim = {
    enable = true;

    luaLoader.enable = true;


    clipboard = {
      register = "unnamedplus";
      providers.wl-copy.enable = true;
    };

    colorscheme = "dracula";
    highlight = {
      DraculaPink = mkRaw ''{["fg"] = (require('dracula').colors().pink)}'';
      DraculaPurple = mkRaw ''{["fg"] = (require('dracula').colors().purple)}'';
    };

    editorconfig.enable = true;


    autoGroups = {
      nvim_metals = {
        clear = true;
      };
    };
    autoCmd = [
      {
        event = [ "TermEnter" "TermLeave" ];
        callback = mkRaw fn.toggleterm_autoscroll;
      }
      {
        event = [ "FileType" ];
        pattern = [ "scala" "sbt" "java" ];
        callback = mkRaw fn.nvim_metals_attach;
        group = "nvim_metals";
      }
    ];

    userCommands = {
      Metals = {
        desc = "Metals command palette";
        command = "lua require('telescope').extensions.metals.commands()";
      };
    };

    extraFunctions = {
      lightline_lsp_status = ''
        return vim.g['metals_status']
      '';
      nvim_metals_attach = ''
        require("metals").initialize_or_attach(metals_config)
      '';
      cmp_tab_trigger = [
        "fallback"
        ''
          local c = require('cmp')
          if c.visible() then
            c.select_next_item()
          else
            fallback()
          end
        ''
      ];
      toggleterm_autoscroll = ''
        local tt = require('toggleterm.terminal')
        local term = tt.get(tt.get_focused_id())
        if term then
          term.auto_scroll = vim.api.nvim_get_mode() == "t"
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
          pylsp = {
            enable = true;
            settings.plugins = {
              pylsp_mypy.enabled = true;
            };
          };
          omnisharp.enable = true;
          terraformls.enable = true;
          tsserver.enable = true;
          cssls.enable = true;
          tailwindcss.enable = true;
          yamlls.enable = true;
          hls.enable = true;
          lua-ls.enable = true;
          metals.enable = false; # conflicts with nvim-metals
          rust-analyzer = {
            enable = true;
            installCargo = true;
            installRustc = true;
          };
          texlab.enable = true;
          gopls.enable = true;
        };
      };
      lsp-format.enable = true;
      lspkind.enable = true;
      nvim-lightbulb.enable = true;
      fidget.enable = true;

      vimtex.enable = true;

      cmp = {
        enable = true;
        autoEnableSources = true;
        settings.mapping = {
          __raw = ''
          cmp.mapping.preset.insert({
            ['<C-b>'] = cmp.mapping.scroll_docs(-4),
            ['<C-f>'] = cmp.mapping.scroll_docs(4),
            ['<C-Space>'] = cmp.mapping.complete(),
            ['<C-e>'] = cmp.mapping.abort(),
            ['<CR>'] = cmp.mapping.confirm({ select = true }),
            ['<C-j>'] = cmp.mapping.select_next_item(),
            ['<C-k>'] = cmp.mapping.select_prev_item(),
          })
          '';
          # "<Tab>" = {
          #   modes = [ "i" "s" ];
          #   action = fn.cmp_tab_trigger;
          # };
        };

        settings.sorting.comparators = 
            [
              "require('cmp.config.compare').offset"
              "require('cmp.config.compare').exact"
              "require('cmp.config.compare').score"
              "require('cmp.config.compare').recently_used"
              "require('cmp.config.compare').locality"
              "require('cmp.config.compare').kind"
              "require('cmp.config.compare').length"
              "require('cmp.config.compare').order"
              "require('cmp_fuzzy_buffer.compare')"
            ];
        settings.snippet.expand = "luasnip";
        settings.sources = [
          { name = "nvim_lsp"; groupIndex = 1; }
          { name = "nvim_lsp_document_symbol"; groupIndex = 1; }
          { name = "nvim_lsp_signature_help"; groupIndex = 1; }
          { name = "nvim_lua"; groupIndex = 2; }
          { name = "latex_symbols"; groupIndex = 2; }
          { name = "tree_sitter"; groupIndex = 2; }
          { name = "fuzzy_buffer"; groupIndex = 3; keywordLength = 4; }
        ];
      };
      luasnip = {
        enable = true;
      };

      trouble.enable = true;

      nix.enable = true;

      lean = {
        enable = true;
        leanPackage = null;
        mappings = true;
      };

      treesitter = {
        enable = true;
        folding = true;
        indent = true;
      };
      treesitter-textobjects.enable = true;
      treesitter-refactor.enable = true;

      surround.enable = true;
      undotree.enable = true;
      yanky = {
        enable = true;
        picker.telescope = {
          enable = true;
          useDefaultMappings = true;
        };
      };
      mini = {
        enable = true;
        modules = {
          # ai = { };
          jump2d = {
            mappings = {
              start_jumping = "";
            };
            labels = "asdfjkl;qweruiopzxcvm,./ghbn";
          };
          comment = { };
        };
      };
      rainbow-delimiters.enable = true;
      harpoon = {
        enable = true;
        enableTelescope = true;
      };

      project-nvim =
        {
          enable = true;
          extraOptions = {
            detection_methods = mkRaw "({})";
          };
        };

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
        defaults.mappings.i = {
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
          frecency.enable = true;
        };
      };
      which-key.enable = true;

      neo-tree = {
        enable = true;
        filesystem = {
          bindToCwd = false;
          hijackNetrwBehavior = "open_current";
          useLibuvFileWatcher = true;
        };
      };

      neogit = {
        enable = true;
        settings.integrations.diffview = true;
      };
      diffview.enable = true;
      gitsigns.enable = true;

      toggleterm = {
        enable = true;
        autoScroll = false;
      };

      dap = {
        enable = true;
        extensions = {
          dap-ui.enable = true;
          dap-virtual-text.enable = true;
        };
        configurations = {
          scala = [{
            type = "scala";
            request = "launch";
            name = "Run test or target";
            metals =
              {
                runType = "runOrTest";
              };
          }];
        };
      };

      neorg = {
        enable = true;
        lazyLoading = false;
        modules = {
          "core.dirman".config = {
            workspaces = {
              notes = "~/org/notes";
            };
          };

          "core.defaults" = { __empty = null; };
          "core.concealer" = { __empty = null; };
          "core.completion".config = {
            engine = "nvim-cmp";
            name = "[Norg]";
          };
          "core.keybinds".config = {
            default_keybinds = true;
            neorg_leader = "<LocalLeader>";
          };
          "core.integrations.nvim-cmp" = { __empty = null; };
          "core.integrations.telescope" = { __empty = null; };
        };
      };

      lualine = {
        enable = true;
        sections.lualine_x = [ 
          "g:metals_status" "encoding" "fileformat" "filetype" 
        ];
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

      transparent.enable = true;
    };

    extraPlugins = with pkgs.vimPlugins; [
      nvim-web-devicons
      nvim-metals
      vim-autoswap
      dracula-nvim
      nvim-focus
    ];

    extraConfigLuaPre = ''
      vim.g.mapleader = ' '
      vim.g.maplocalleader = ' m'
      vim.o.sessionoptions="blank,buffers,curdir,folds,help,tabpages,winsize,winpos,terminal,localoptions"
      vim.o.foldenable = false
      vim.o.signcolumn="yes"

      require('dracula').setup({})

      metals_config = require("metals").bare_config()
      metals_config.init_options.statusBarProvider = "off"

      metals_config.find_root_dir_max_project_nesting = 3
      metals_config.settings = {
        showImplicitArguments = true,
        metalsBinaryPath = "${pkgs.metals}/bin/metals",
        excludedPackages = { "akka.actor.typed.javadsl", "com.github.swagger.akka.javadsl" },
      }

      -- Example if you are using cmp how to make sure the correct capabilities for snippets are set
      metals_config.capabilities = require("cmp_nvim_lsp").default_capabilities()

      metals_config.on_attach = function(client, bufnr)
        require("metals").setup_dap()
      end

      require("focus").setup()
    '';

    keymaps =
      let
        leader = { key, action, desc ? null, lua ? true, options ? { } }: {
          inherit action lua;
          mode = "n";
          key = "<leader>${key}";
          options = { silent = true; } // options // { inherit desc; };
        };
        strEscape = lib.escape [ "'" "\"" ];
        defer = body: "(function () return (${body}) end)";
        cmd = fn: defer "vim.cmd('${strEscape fn}')";
        tele = fn: cmd "Telescope ${fn}";
        nop = defer "nil";
      in
      (
        [
          {
            mode = "i";
            key = "<M-h>";
            lua = true;
            action = defer "vim.api.nvim_feedkeys(vim.api.nvim_replace_termcodes('<C-d>', true, false, true), 'i', false)";
          }
          {
            mode = "i";
            key = "<M-l>";
            lua = true;
            action = defer "vim.api.nvim_feedkeys(vim.api.nvim_replace_termcodes('<C-t>', true, false, true), 'i', false)";
          }

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
            key = "fr";
            action = tele "frecency";
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
            key = "l";
            action = defer "vim.api.nvim_input('\"+')";
            options.desc = "Use clipboard register";
          })

          (leader {
            key = "ot";
            action = cmd "exe v:count1 . 'ToggleTerm'";
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
            key = "cl";
            action = defer "vim.lsp.codelens.run()";
            desc = "Run codelens";
          })
          (leader {
            key = "cL";
            action = defer "vim.lsp.codelens.refresh()";
            desc = "Refresh codelens";
          })

          (leader {
            key = "dt";
            action = defer "require('dapui').toggle()";
            desc = "REPL";
          })
          (leader {
            key = "dr";
            action = defer "require('dap').repl.toggle()";
            desc = "REPL";
          })
          (leader {
            key = "dK";
            action = defer "require('dap.ui.widgets').hover()";
            desc = "Hover";
          })
          (leader {
            key = "db";
            action = defer "require('dap').toggle_breakpoint()";
            desc = "Toggle breakpoint";
          })
          (leader {
            key = "dc";
            action = defer "require('dap').continue()";
            desc = "Continue";
          })
          (leader {
            key = "dC";
            action = defer "require('dap').run_to_cursor()";
            desc = "Run to cursor";
          })
          (leader {
            key = "ds";
            action = defer "require('dap').terminate()";
            desc = "Terminate";
          })
          (leader {
            key = "dn";
            action = defer "require('dap').step_over()";
            desc = "Step over";
          })
          (leader {
            key = "di";
            action = defer "require('dap').step_into()";
            desc = "Step into";
          })
          (leader {
            key = "dl";
            action = defer "require('dap').run_last()";
            desc = "Run last";
          })

          (leader {
            key = "sp";
            action = tele "live_grep";
            desc = "Grep";
          })
          (leader {
            key = "sd";
            action = defer "require('telescope.builtin').live_grep({cwd = require('telescope.utils').buffer_dir()})";
            desc = "Grep buffer dir";
          })
          (leader {
            key = "sc";
            action = cmd "nohlsearch";
            desc = "Clear search highlight";
          })
          (leader {
            key = "se";
            action = defer "vim.lsp.buf.rename()";
            desc = "Rename LSP symbol";
          })
          (leader {
            key = "ss";
            action = tele "treesitter";
            desc = "Treesitter symbols";
          })
          (leader {
            key = "sr";
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
            action = tele "lsp_implementations";
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
            key = "hh";
            action = tele "harpoon marks";
            desc = "Marks";
          })
          (leader {
            key = "ha";
            action = defer "require('harpoon.mark').add_file()";
            desc = "Add mark";
          })
          (leader {
            key = "ht";
            action = defer "require('harpoon.ui').toggle_quick_menu()";
            desc = "Toggle quick menu";
          })
          (leader {
            key = "hj";
            action = defer "require('harpoon.ui').nav_next()";
            desc = "Next mark";
          })
          (leader {
            key = "hk";
            action = defer "require('harpoon.ui').nav_prev()";
            desc = "Previous mark";
          })

          (leader {
            key = "nsh";
            action = cmd "Neorg keybind neorg core.integrations.telescope.search_headings";
            desc = "Search headings";
          })
          (leader {
            key = "nsf";
            action = cmd "Neorg keybind neorg core.integrations.telescope.find_norg_files";
            desc = "Find norg files";
          })
          (leader {
            key = "np";
            action = cmd "Neorg keybind neorg core.integrations.telescope.switch_workspace";
            desc = "Switch workspace";
          })
          (leader {
            key = "nc";
            action = cmd "Neorg keybind neorg core.dirman.new_note";
            desc = "New note";
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
            desc = "Quit nvim without saving";
          })
          (leader {
            key = "qr";
            action = defer "${fn.reload}()";
          })
        ]
      );
  };

  programs.zsh.shellAliases = {
    vim = "reloadable-nvim";
    v = "vim";
  };

  home.file = { };

  home.packages = with pkgs; [
    neovim-remote
    chafa
    metals
    coursier
  ];
}
