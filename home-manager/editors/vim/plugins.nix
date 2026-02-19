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
      settings = {};
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
      treesitter-context.enable = true;

      # Rainbow delimiters
      rainbow-delimiters.enable = true;

      # Git integration
      gitsigns = {
        enable = true;
        settings.current_line_blame = false;
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
        settings = {
          detection_methods = [ "pattern" ];
          patterns = [ ".git" "package.json" "Cargo.toml" "go.mod" ];
        };
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
          autoscroll = false;
          direction = "float";
        };
      };

      # Notifications
      notify = {
        enable = true;
        settings = {
          timeout = 3000;
          max_width = 60;
          max_height = 10;
          render = "default";
          stages = "fade";
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

      # Startup screen with wizard
      alpha = {
        enable = true;
        settings.layout = [
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
  };
}
