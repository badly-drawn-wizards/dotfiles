{ config, lib, pkgs, ... }:

{
  programs.nixvim = {
    extraConfigLuaPre = ''
      vim.g.WK_shown = false;
      local function wk_alias(keys)
        local timeout = vim.o.timeoutlen
        if vim.g.WK_shown then
          vim.o.timeoutlen = 0
        end
        local key_codes = vim.api.nvim_replace_termcodes(keys, true, false, true)
        vim.api.nvim_feedkeys(key_codes, "m", false)
        vim.defer_fn(function()
          vim.o.timeoutlen = timeout
          vim.g.WK_shown = false
        end, 10)
      end

      vim.api.nvim_create_autocmd({ "Filetype" }, {
        pattern = "WhichKey",
        callback = function()
          vim.g.WK_shown = true
        end,
      })
    '';
  };
}
