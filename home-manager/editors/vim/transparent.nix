{ config
, lib
, ...
}:
with lib; let
  helpers = config.nixvim.helpers;
  module = { pkgs, ... }@args:
    let
      cfg = args.config.plugins.transparent;
    in
    {
      options = {
        plugins.transparent = {
          enable = mkEnableOption "transparent";
          package = helpers.mkPackageOption "transparent" pkgs.vimPlugins.nvim-transparent;
        };
      };
      config =
        let
          setupOptions = with cfg;
            helpers.toLuaObject { };
        in
        mkIf cfg.enable {
          extraPlugins = [ cfg.package ];

          extraConfigLua = ''
            require('transparent').setup(${setupOptions})
          '';
        };
    };
in
{
  options.programs.nixvim = mkOption {
    type = types.submodule module;
  };
}
