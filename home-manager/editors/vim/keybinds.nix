{ config, lib, pkgs, ... }:

let
  cfg = config.programs.nixvim.keybindGroups;

  # Keybind type - unified submodule for both groups and actions
  # Either has action (leaf) or keybinds (group), enforced by assertion during processing
  keybindType = lib.types.submodule {
    options = {
      key = lib.mkOption {
        type = lib.types.str;
        description = "The key or key sequence for this binding";
        example = "<leader>f";
      };

      desc = lib.mkOption {
        type = lib.types.nullOr lib.types.str;
        default = null;
        description = "Description/label for which-key (group name for groups, action description for actions)";
        example = "Files";
      };

      icon = lib.mkOption {
        type = lib.types.nullOr lib.types.str;
        default = null;
        description = "Icon to display in which-key";
        example = "󰈔";
      };

      mode = lib.mkOption {
        type = lib.types.either lib.types.str (lib.types.listOf lib.types.str);
        default = "n";
        description = "Vim mode(s) for this keybind";
        example = "n";
      };

      silent = lib.mkOption {
        type = lib.types.bool;
        default = true;
        description = "Whether to suppress output";
      };

      # For actions: action to execute
      action = lib.mkOption {
        type = lib.types.nullOr lib.types.anything;
        default = null;
        description = "Action to execute (string command or raw Lua via config.lib.nixvim.mkRaw)";
        example = "<cmd>Telescope find_files<CR>";
      };

      # For groups: nested keybinds
      keybinds = lib.mkOption {
        type = lib.types.listOf keybindType;
        default = [];
        description = "Nested keybinds within this group";
      };
    };
  };

  # Convert our ergonomic format to nixvim keymaps and which-key spec
  processKeybind = prefix: bind:
    let
      key = "${prefix}${bind.key}";
      hasAction = bind.action != null;
      hasKeybinds = bind.keybinds != [];
      isGroup = hasKeybinds;
      isAction = hasAction;
    in
    assert !(hasAction && hasKeybinds)
      || throw "Keybind '${key}' cannot have both 'action' and 'keybinds'. Use 'action' for leaf nodes or 'keybinds' for groups.";
    {
      keymap = lib.optional isAction {
        inherit key;
        mode = bind.mode;
        action = bind.action;
        options = {
          inherit (bind) silent;
          desc = bind.desc;
        };
      };

      whichKeySpec =
        if isGroup then
          {
            __unkeyed-1 = key;
            group = bind.desc;
            icon = bind.icon;
          }
        else
          null;

      # Recursively process nested keybinds
      nested =
        if isGroup && bind.keybinds != []
        then lib.flatten (map (processKeybind key) bind.keybinds)
        else [];
    };

  # Process all keybind groups recursively
  processAll = binds: prefix:
    lib.flatten (map (bind:
      let processed = processKeybind prefix bind;
      in [ processed ] ++ processed.nested
    ) binds);

  allProcessed = processAll cfg "";
  allKeymaps = lib.flatten (map (p: p.keymap) allProcessed);
  allWhichKeySpecs = lib.filter (s: s != null) (map (p: p.whichKeySpec) allProcessed);

in
{
  options.programs.nixvim.keybindGroups = lib.mkOption {
    type = lib.types.listOf keybindType;
    default = [];
    description = ''
      Ergonomic keybind configuration with integrated which-key support.

      Each entry is either:
      - A group: { key = "<leader>b"; desc = "Buffers"; icon = "󰓩"; keybinds = [...]; }
      - An action: { key = "<leader>bb"; action = "..."; desc = "List buffers"; }

      Nested keybinds are supported via the keybinds attribute on groups.
      For Lua actions, use config.lib.nixvim.mkRaw in your configuration.
    '';
    example = lib.literalExpression ''
      [
        {
          key = "<leader>f";
          desc = "Files";
          icon = "󰈔";
          keybinds = [
            {
              key = "f";
              action = "<cmd>Telescope find_files<CR>";
              desc = "Find file";
            }
          ];
        }
      ]
    '';
  };

  config = {
    programs.nixvim = {
      # Apply generated keymaps
      keymaps = allKeymaps;

      # Configure which-key with generated spec
      plugins.which-key = {
        enable = true;
        settings = {
          preset = false;
          delay = 200;
          spec = allWhichKeySpecs;
        };
      };
    };

    programs.nixvim.keybindGroups = let
      helpers = config.lib.nixvim;
      inherit (helpers) mkRaw;
    in [
      # Leader + space: Command palette
      {
        key = "<leader> ";
        action = "<cmd>Telescope commands<CR>";
        desc = "Command palette";
      }

      # Leader + .: Open directory of current buffer in netrw
      {
        key = "<leader>.";
        action = mkRaw ''
          function()
            local bufname = vim.api.nvim_buf_get_name(0)
            if bufname == "" then
              vim.notify("Buffer not linked to a file", vim.log.levels.WARN, { title = "Netrw" })
              return
            end
            local dir = vim.fn.fnamemodify(bufname, ":p:h")
            vim.cmd("edit " .. vim.fn.fnameescape(dir))
          end
        '';
        desc = "Open directory in netrw";
      }

      # Quit
      {
        key = "<leader>q";
        desc = "Quit";
        icon = "󰗼";
        keybinds = [
          {
            key = "q";
            action = "<cmd>confirm qa<CR>";
            desc = "Quit nvim";
          }
          {
            key = "Q";
            action = "<cmd>qa!<CR>";
            desc = "Quit without saving";
          }
        ];
      }

      # Windows
      {
        key = "<leader>w";
        desc = "Windows";
        icon = "󰖲";
        keybinds = [
          {
            key = "h";
            action = "<C-w>h";
            desc = "Move left";
          }
          {
            key = "j";
            action = "<C-w>j";
            desc = "Move down";
          }
          {
            key = "k";
            action = "<C-w>k";
            desc = "Move up";
          }
          {
            key = "l";
            action = "<C-w>l";
            desc = "Move right";
          }
          {
            key = "s";
            action = "<cmd>split<CR>";
            desc = "Split horizontal";
          }
          {
            key = "v";
            action = "<cmd>vsplit<CR>";
            desc = "Split vertical";
          }
          {
            key = "d";
            action = "<C-w>q";
            desc = "Close window";
          }
          {
            key = "=";
            action = "<C-w>=";
            desc = "Balance windows";
          }
          {
            key = "m";
            action = "<C-w>_<C-w>|";
            desc = "Maximize window";
          }
          {
            key = "<";
            action = "<C-w>5<";
            desc = "Decrease width";
          }
          {
            key = ">";
            action = "<C-w>5>";
            desc = "Increase width";
          }
        ];
      }

      # Project
      {
        key = "<leader>p";
        desc = "Project";
        icon = "󰉋";
        keybinds = [
          {
            key = "p";
            action = "<cmd>Telescope projects<CR>";
            desc = "Switch project";
          }
          {
            key = "f";
            action = "<cmd>Telescope find_files hidden=true<CR>";
            desc = "Find file";
          }
          {
            key = "s";
            action = mkRaw "function() require('auto-session.session-lens').search_session() end";
            desc = "Search session";
          }
          {
            key = "a";
            action = "<cmd>AddProject<CR>";
            desc = "Add project";
          }
        ];
      }

      # Files
      {
        key = "<leader>f";
        desc = "Files";
        icon = "󰈔";
        keybinds = [
          {
            key = "f";
            action = "<cmd>Telescope file_browser<CR>";
            desc = "File browser";
          }
          {
            key = "r";
            action = "<cmd>Telescope frecency<CR>";
            desc = "Recent files";
          }
          {
            key = "s";
            action = "<cmd>write<CR>";
            desc = "Save file";
          }
          {
            key = "S";
            action = "<cmd>wall<CR>";
            desc = "Save all files";
          }
          {
            key = "R";
            action = "<cmd>e!<CR>";
            desc = "Reload file";
          }
          {
            key = "e";
            desc = "Edit";
            keybinds = [
              {
                key = "d";
                action = "<cmd>e ~/.config/nvim/init.lua<CR>";
                desc = "Edit dotfiles";
              }
            ];
          }
        ];
      }

      # Search
      {
        key = "<leader>s";
        desc = "Search";
        icon = "";
        keybinds = [
          {
            key = "p";
            action = "<cmd>Telescope live_grep<CR>";
            desc = "Grep project";
          }
          {
            key = "d";
            action = mkRaw "function() require('telescope.builtin').live_grep({cwd = require('telescope.utils').buffer_dir()}) end";
            desc = "Grep directory";
          }
          {
            key = "s";
            action = "<cmd>Telescope treesitter<CR>";
            desc = "Treesitter symbols";
          }
          {
            key = "c";
            action = "<cmd>nohlsearch<CR>";
            desc = "Clear highlight";
          }
          {
            key = "e";
            action = mkRaw "vim.lsp.buf.rename";
            desc = "Rename symbol";
          }
          {
            key = "r";
            action = "<cmd>Telescope lsp_references<CR>";
            desc = "LSP references";
          }
          {
            key = "l";
            desc = "LSP";
            keybinds = [
              {
                key = "d";
                action = "<cmd>Telescope lsp_definitions<CR>";
                desc = "Definitions";
              }
              {
                key = "i";
                action = "<cmd>Telescope lsp_implementations<CR>";
                desc = "Implementations";
              }
              {
                key = "t";
                action = "<cmd>Telescope lsp_type_definitions<CR>";
                desc = "Type definitions";
              }
              {
                key = "s";
                action = "<cmd>Telescope lsp_workspace_symbols<CR>";
                desc = "Workspace symbols";
              }
              {
                key = "c";
                desc = "Calls";
                keybinds = [
                  {
                    key = "i";
                    action = "<cmd>Telescope lsp_incoming_calls<CR>";
                    desc = "Incoming calls";
                  }
                  {
                    key = "o";
                    action = "<cmd>Telescope lsp_outgoing_calls<CR>";
                    desc = "Outgoing calls";
                  }
                ];
              }
            ];
          }
        ];
      }

      # Code & LSP
      {
        key = "<leader>c";
        desc = "Code";
        icon = "";
        keybinds = [
          {
            key = "a";
            action = mkRaw "vim.lsp.buf.code_action";
            desc = "Code action";
          }
          {
            key = "r";
            action = "<cmd>IncRename <CR>";
            desc = "Rename (inc)";
          }
          {
            key = "f";
            action = mkRaw "vim.lsp.buf.format";
            desc = "Format";
          }
          {
            key = "l";
            action = mkRaw "vim.lsp.codelens.run";
            desc = "Run codelens";
          }
          {
            key = "L";
            action = mkRaw "vim.lsp.codelens.refresh";
            desc = "Refresh codelens";
          }
        ];
      }

      # Diagnostics/Errors
      {
        key = "<leader>e";
        desc = "Diagnostics";
        icon = "";
        keybinds = [
          {
            key = "e";
            action = "<cmd>Trouble diagnostics toggle<CR>";
            desc = "Diagnostics (Trouble)";
          }
          {
            key = "E";
            action = "<cmd>Trouble diagnostics toggle filter.buf=0<CR>";
            desc = "Buffer diagnostics";
          }
          {
            key = "l";
            action = mkRaw "vim.diagnostic.open_float";
            desc = "Line diagnostics";
          }
          {
            key = "n";
            action = mkRaw "vim.diagnostic.goto_next";
            desc = "Next diagnostic";
          }
          {
            key = "p";
            action = mkRaw "vim.diagnostic.goto_prev";
            desc = "Previous diagnostic";
          }
          {
            key = "q";
            action = "<cmd>Trouble qflist toggle<CR>";
            desc = "Quickfix list";
          }
          {
            key = "s";
            action = "<cmd>Trouble symbols toggle focus=false<CR>";
            desc = "Symbols (Trouble)";
          }
          {
            key = "r";
            action = "<cmd>Trouble lsp toggle focus=false win.position=right<CR>";
            desc = "LSP references";
          }
        ];
      }

      # Debug
      {
        key = "<leader>d";
        desc = "Debug";
        icon = "";
        keybinds = [
          {
            key = "t";
            action = mkRaw "function() require('dapui').toggle() end";
            desc = "Toggle DAP UI";
          }
          {
            key = "r";
            action = mkRaw "function() require('dap').repl.toggle() end";
            desc = "Toggle REPL";
          }
          {
            key = "b";
            action = mkRaw "function() require('dap').toggle_breakpoint() end";
            desc = "Toggle breakpoint";
          }
          {
            key = "B";
            action = mkRaw "function() require('dap').set_breakpoint(vim.fn.input('Breakpoint condition: ')) end";
            desc = "Conditional breakpoint";
          }
          {
            key = "c";
            action = mkRaw "function() require('dap').continue() end";
            desc = "Continue";
          }
          {
            key = "C";
            action = mkRaw "function() require('dap').run_to_cursor() end";
            desc = "Run to cursor";
          }
          {
            key = "s";
            action = mkRaw "function() require('dap').terminate() end";
            desc = "Terminate";
          }
          {
            key = "n";
            action = mkRaw "function() require('dap').step_over() end";
            desc = "Step over";
          }
          {
            key = "i";
            action = mkRaw "function() require('dap').step_into() end";
            desc = "Step into";
          }
          {
            key = "o";
            action = mkRaw "function() require('dap').step_out() end";
            desc = "Step out";
          }
          {
            key = "l";
            action = mkRaw "function() require('dap').run_last() end";
            desc = "Run last";
          }
          {
            key = "K";
            action = mkRaw "function() require('dap.ui.widgets').hover() end";
            desc = "Hover";
          }
        ];
      }

      # Jump
      {
        key = "<leader>j";
        desc = "Jump";
        icon = "󰘎";
        keybinds = [
          {
            key = "j";
            action = mkRaw "function() MiniJump2d.start(MiniJump2d.builtin_opts.word_start) end";
            desc = "Jump to word";
          }
          {
            key = "l";
            action = mkRaw "function() MiniJump2d.start(MiniJump2d.builtin_opts.line_start) end";
            desc = "Jump to line";
          }
        ];
      }

      # Notifications
      {
        key = "<leader>n";
        desc = "Notifications";
        icon = "󰵅";
        keybinds = [
          {
            key = "n";
            action = "<cmd>Telescope notify<CR>";
            desc = "Show history";
          }
          {
            key = "d";
            action = mkRaw "function() require('notify').dismiss({ silent = true, pending = true }) end";
            desc = "Dismiss all";
          }
        ];
      }

      # Application specific
      {
        key = "<leader>a";
        desc = "Apps";
        icon = "󰀻";
        keybinds = [
          # Git
          {
            key = "g";
            desc = "Git";
            icon = "";
            keybinds = [
              {
                key = "g";
                action = "<cmd>Neogit<CR>";
                desc = "Neogit";
              }
              {
                key = "s";
                action = mkRaw "function() require('gitsigns').stage_hunk() end";
                desc = "Stage hunk";
              }
              {
                key = "S";
                action = "<cmd>!git add %<CR>";
                desc = "Stage file";
              }
              {
                key = "u";
                action = mkRaw "function() require('gitsigns').undo_stage_hunk() end";
                desc = "Undo stage hunk";
              }
              {
                key = "c";
                action = "<cmd>Telescope git_commits<CR>";
                desc = "Commits";
              }
              {
                key = "C";
                action = "<cmd>Telescope git_bcommits<CR>";
                desc = "Buffer commits";
              }
              {
                key = "b";
                action = "<cmd>Telescope git_branches<CR>";
                desc = "Branches";
              }
              {
                key = "t";
                action = "<cmd>Telescope git_status<CR>";
                desc = "Status";
              }
              {
                key = "h";
                action = "<cmd>Telescope git_stash<CR>";
                desc = "Stash";
              }
            ];
          }
          # Terminal
          {
            key = "t";
            desc = "Terminal";
            keybinds = [
              {
                key = "t";
                action = "<cmd>exe v:count1 . 'ToggleTerm'<CR>";
                desc = "Toggle terminal";
              }
            ];
          }
          # Clipboard
          {
            key = "l";
            desc = "Clipboard";
            keybinds = [
              {
                key = "l";
                action = mkRaw "function() vim.api.nvim_input('\"+') end";
                desc = "Use + register";
              }
            ];
          }
          # Yank history
          {
            key = "y";
            desc = "Yank";
            keybinds = [
              {
                key = "y";
                action = "<cmd>Telescope yank_history<CR>";
                desc = "Yank history";
              }
            ];
          }
        ];
      }

      # Buffers
      {
        key = "<leader>b";
        desc = "Buffers";
        icon = "󰓩";
        keybinds = [
          {
            key = "b";
            action = "<cmd>Telescope buffers<CR>";
            desc = "List buffers";
          }
          {
            key = "d";
            action = "<cmd>confirm bdelete<CR>";
            desc = "Delete buffer";
          }
          {
            key = "D";
            action = "<cmd>bdelete!<CR>";
            desc = "Force delete";
          }
          {
            key = "n";
            action = "<cmd>bnext<CR>";
            desc = "Next buffer";
          }
          {
            key = "p";
            action = "<cmd>bprevious<CR>";
            desc = "Previous buffer";
          }
        ];
      }

      # Git hunk navigation
      {
        key = "<leader>]";
        action = mkRaw "function() require('gitsigns').nav_hunk('next') end";
        desc = "Next git hunk";
      }
      {
        key = "<leader>[";
        action = mkRaw "function() require('gitsigns').nav_hunk('prev') end";
        desc = "Previous git hunk";
      }

      # Additional non-leader keybinds
      {
        key = "gd";
        action = mkRaw "vim.lsp.buf.definition";
        desc = "Go to definition";
      }
      {
        key = "gD";
        action = mkRaw "vim.lsp.buf.declaration";
        desc = "Go to declaration";
      }
      {
        key = "gr";
        action = mkRaw "vim.lsp.buf.references";
        desc = "Go to references";
      }
      {
        key = "gi";
        action = mkRaw "vim.lsp.buf.implementation";
        desc = "Go to implementation";
      }
      {
        key = "[d";
        action = mkRaw "vim.diagnostic.goto_prev";
        desc = "Previous diagnostic";
      }
      {
        key = "]d";
        action = mkRaw "vim.diagnostic.goto_next";
        desc = "Next diagnostic";
      }

      # Window close alias
      {
        key = "<C-w>d";
        action = "<C-w>q";
        desc = "Quit window";
      }

      # Insert mode indent
      {
        key = "<M-h>";
        mode = "i";
        action = mkRaw "function() vim.api.nvim_feedkeys(vim.api.nvim_replace_termcodes('<C-d>', true, false, true), 'i', false) end";
        desc = "Decrease indent";
      }
      {
        key = "<M-l>";
        mode = "i";
        action = mkRaw "function() vim.api.nvim_feedkeys(vim.api.nvim_replace_termcodes('<C-t>', true, false, true), 'i', false) end";
        desc = "Increase indent";
      }
    ];
  };
}
