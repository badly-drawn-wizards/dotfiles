{ config, lib, pkgs, ... }:

let
  defn = name: rest:
    let
      rest' = if builtins.isList rest then rest else [rest];
      params = lib.init rest';
      body = lib.last rest';
    in ''
      function ${name}(${lib.concatStringsSep ", " params})
      ${body}
      end
    '';

  inherit (config.programs.nixvim) extraFunctions;
in
{
  options.programs.nixvim = {
    extraFunctions = lib.mkOption {
      type = with lib.types; attrsOf (either str (listOf str));
      default = {};
      description = ''
        Extra functions to define. Either function body or list of parameters
        followed by function body.
      '';
    };
    extraFunction = lib.mkOption {
      type = with lib.types; attrsOf str;
      default =
        builtins.mapAttrs (name: _: name) extraFunctions;
      readOnly = true;
      description = "The names of defined extraFunctions.";
    };
  };

  config = {
    programs.nixvim.extraConfigLuaPre =
      lib.concatLines (lib.mapAttrsToList defn extraFunctions);
  };

}
