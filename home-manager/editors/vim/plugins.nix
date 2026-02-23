{ config, lib, pkgs, ... }:

let
  helpers = config.lib.nixvim;
  inherit (helpers) mkRaw;
in
{
  programs.nixvim = {
    # Colorscheme
    colorschemes.dracula-nvim = {
      enable = true;
      settings = { };
    };

    plugins = {
      # Icons (required by telescope, neo-tree, trouble)
      web-devicons.enable = true;

      # File explorer - enabled but doesn't hijack netrw
      neo-tree = {
        enable = true;
        settings = {
          filesystem = {
            hijack_netrw_behavior = "disabled";
            follow_current_file = {
              enabled = true;
              leave_dirs_open = false;
            };
          };
        };
      };

      # Fuzzy finder
      telescope = {
        enable = true;
        extensions = {
          fzf-native.enable = true;
          frecency.enable = true;
          file-browser.enable = true;
        };
        settings = {
          defaults.mappings.i = {
            "<C-j>" = mkRaw "require('telescope.actions').move_selection_next";
            "<C-k>" = mkRaw "require('telescope.actions').move_selection_previous";
          };
        };
      };

      # Treesitter
      treesitter = {
        enable = true;
        folding.enable = true;
        settings = {
          highlight.enable = true;
          indent.enable = true;
        };
      };
      treesitter-context = {
        enable = true;
        settings = {
          separator = "─";
        };
      };
      treesitter-textobjects = {
        enable = true;
        settings = {
          select = {
            enable = true;
            lookahead = true;
            keymaps = {
              af = "@function.outer";
              "if" = "@function.inner";
              ac = "@class.outer";
              ic = "@class.inner";
              aa = "@parameter.outer";
              ia = "@parameter.inner";
              ab = "@block.outer";
              ib = "@block.inner";
            };
          };
          move = {
            enable = true;
            goto_next_start = {
              "]f" = "@function.outer";
              "]c" = "@class.outer";
              "]a" = "@parameter.inner";
            };
            goto_previous_start = {
              "[f" = "@function.outer";
              "[c" = "@class.outer";
              "[a" = "@parameter.inner";
            };
          };
        };
      };

      # Rainbow delimiters
      rainbow-delimiters.enable = true;

      # Git integration
      gitsigns = {
        enable = true;
        settings = {
          current_line_blame = true;
          current_line_blame_opts = {
            delay = 500;
            virt_text_pos = "eol";
          };
        };
      };
      neogit = {
        enable = true;
        settings.integrations.diffview = true;
      };
      diffview.enable = true;

      # Session management - auto-save but not auto-restore
      auto-session = {
        enable = true;
        settings = {
          enabled = true;
          auto_restore = false;
          auto_save = true;
          auto_create = true;
          log_level = "error";
          session_lens = {
            load_on_setup = true;
          };
        };
      };

      # Project management
      project-nvim = {
        enable = true;
        enableTelescope = true;
        settings = { };
      };

      # Yank history
      yanky = {
        enable = true;
        enableTelescope = true;
        settings.picker.telescope.use_default_mappings = true;
      };

      # Mini modules
      mini = {
        enable = true;
        modules = {
          jump2d = {
            mappings = {
              start_jumping = "";
            };
            labels = "asdfjkl;qweruiopzxcvm,./ghbn";
          };
        };
      };

      # Terminal
      toggleterm = {
        enable = true;
        settings = {
          autoscroll = true;
          direction = "horizontal";
        };
      };

      # Notifications
      notify = {
        enable = true;
        settings = {
          timeout = 3000;
          render = "default";
          stages = "fade";
          background_colour = "#000000";
        };
      };

      # Debug Adapter Protocol
      dap = {
        enable = true;
        configurations = {
          scala = [{
            type = "scala";
            request = "launch";
            name = "Run test or target";
            metals = {
              runType = "runOrTest";
            };
          }];
        };
      };
      dap-ui = {
        enable = true;
        floating.mappings = {
          close = [ "<ESC>" "q" ];
        };
      };
      dap-virtual-text.enable = true;

      # Status line
      lualine = {
        enable = true;
        settings = {
          options = {
            theme = "dracula-nvim";
            section_separators = {
              left = "";
              right = "";
            };
            component_separators = {
              left = "";
              right = "";
            };
          };
          sections = {
            lualine_x = [
              "encoding"
              "fileformat"
              "filetype"
            ];
          };
        };
      };

      # Completion
      cmp = {
        enable = true;
        settings = {
          mapping = {
            "<C-Space>" = "cmp.mapping.complete()";
            "<C-d>" = "cmp.mapping.scroll_docs(-4)";
            "<C-e>" = "cmp.mapping.close()";
            "<C-f>" = "cmp.mapping.scroll_docs(4)";
            "<CR>" = "cmp.mapping.confirm({ select = true })";
            "<S-Tab>" = "cmp.mapping(cmp.mapping.select_prev_item(), {'i', 's'})";
            "<Tab>" = "cmp.mapping(cmp.mapping.select_next_item(), {'i', 's'})";
          };
          sources = [
            { name = "nvim_lsp"; }
            { name = "luasnip"; }
            { name = "path"; }
            { name = "buffer"; }
          ];
        };
      };
      cmp-nvim-lsp.enable = true;
      cmp-buffer.enable = true;
      cmp-path.enable = true;

      # Snippets
      luasnip.enable = true;
      cmp_luasnip.enable = true;

      # Editing enhancements
      sleuth.enable = true;
      comment.enable = true;
      nvim-surround.enable = true;
      nvim-autopairs.enable = true;
      undotree.enable = true;

      # Visual enhancements
      indent-blankline = {
        enable = true;
        settings = {
          scope.enabled = true;
        };
      };
      nvim-colorizer = {
        enable = true;
        settings = {
          user_default_options = {
            names = false;
            rgb_fn = true;
            hsl_fn = true;
          };
        };
      };
      todo-comments = {
        enable = true;
        settings = {
          signs = true;
        };
      };
      fidget = {
        enable = true;
        settings = {
          notification = {
            window = {
              winblend = 0;
            };
          };
        };
      };
      nvim-lightbulb = {
        enable = true;
        settings = {
          autocmd = {
            enabled = true;
          };
          sign = {
            enabled = true;
            text = "💡";
          };
        };
      };

      # Better UI/UX
      trouble = {
        enable = true;
        settings = {
          auto_close = true;
          focus = true;
        };
      };
      inc-rename = {
        enable = true;
        settings = {
          input_buffer_type = "dressing";
        };
      };
      lsp-lines = {
        enable = true;
      };

      # Startup screen with wizard
      alpha = {
        enable = true;
        settings.layout = [
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
              " | |/  \\| |  FJ  `-.'.'/  | |--| |  | |__|-J| |--| | "
              " F   /\\   J J  L .' (_(_  F L__J J  F L  `-'F L__J J "
              "J___//\\___LJ__LJ_______LJ\\____,__LJ__L    J\\____,__L"
              "|___/  \\___||__||_______| J____,__F|__L     J____,__F"
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

      # Transparent background
      transparent = {
        enable = true;
        settings = {
          extra_groups = [
            "NormalFloat"
            "NvimTreeNormal"
          ];
        };
      };

      # Lean theorem prover (handles LSP setup internally)
      lean = {
        enable = true;
        settings = {
          mappings = true;
        };
      };
    };
  };
}
