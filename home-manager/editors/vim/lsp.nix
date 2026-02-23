{ config, lib, pkgs, ... }:

{
  programs.nixvim = {
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
            "<leader>ca" = "code_action";
            "<leader>cr" = "rename";
          };
          diagnostic = {
            "<leader>cj" = "goto_next";
            "<leader>ck" = "goto_prev";
          };
        };

        servers = {
          # Nix
          nixd = {
            enable = true;
            settings = {
              formatting.command = [ "nixpkgs-fmt" ];
              nixpkgs.expr = "import <nixpkgs> { }";
              options = {
                nixos.expr = "(builtins.getFlake (toString ./.)).nixosConfigurations.noobnoob.options";
                home_manager.expr = "(builtins.getFlake (builtins.toString ./.)).nixosConfigurations.noobnoob.options.home-manager.users.type.getSubOptions []";
              };
            };
          };

          # C/C++
          clangd.enable = true;

          # Python
          pylsp = {
            enable = true;
            settings.plugins = {
              pylsp_mypy.enabled = true;
            };
          };

          # C# - using csharp-ls (Roslyn-based LSP)
          csharp_ls.enable = true;

          # Rust
          rust_analyzer = {
            enable = true;
            installCargo = false;
            installRustc = false;
          };

          # Haskell
          hls = {
            enable = true;
            installGhc = false;
          };

          # Lua
          lua_ls.enable = true;

          # Go
          gopls.enable = true;

          # Web
          ts_ls.enable = true;
          cssls.enable = true;
          tailwindcss.enable = true;

          # LaTeX
          texlab.enable = true;

          # YAML
          yamlls.enable = true;

          # Terraform
          terraformls.enable = true;
        };
      };

      # LSP UI enhancements
      lsp-format.enable = true;
      lspkind.enable = true;
      fidget.enable = true;
      nvim-lightbulb.enable = true;

      # Diagnostics
      trouble.enable = true;
    };
  };
}
