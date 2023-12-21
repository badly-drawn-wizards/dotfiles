{ config, lib, pkgs, ... }:

{
  wayland.windowManager.hyprland = {
    enable = true;
    systemd.enable = true;
    extraConfig = ''
      $mod = SUPER
      bind $mod, i, exec, em

      ${builtins.concatStringsSep "\n" (builtins.genList (
            x: let
              ws = let
                c = (x + 1) / 10;
              in
                builtins.toString (x + 1 - (c * 10));
            in ''
              bind = $mod, ${ws}, workspace, ${toString (x + 1)}
              bind = $mod SHIFT, ${ws}, movetoworkspace, ${toString (x + 1)}
            ''
          )
          10)}
    '';
  };
}
