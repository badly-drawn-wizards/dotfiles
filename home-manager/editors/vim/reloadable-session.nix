{ config, lib, pkgs, ... }:
let
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
  programs.nixvim = {
    autoCmd = [
      {
        event = [ "VimLeavePre" ];
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
    };
  };

  home.packages = [
    reloadable-nvim
  ];

}
