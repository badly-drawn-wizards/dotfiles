{ config, lib, pkgs, inputs, ... }:

let
  configPath = ".emacs.d/lisp";
  cleanConfig = ''
  (setq doom-font (font-spec :family "${config.fontMono}" :size 16))
  '';
  cleanConfigName = "config.clean.el";
  dirtyConfigName = "config.dirty.el";

  inherit (pkgs) jq writeScript;
  inherit (config.programs.doom-emacs) eval-elisp;
  
  org-clock-format-jq = writeScript "org-clock-jq" ''
    #!/usr/bin/env jq
    # https://stackoverflow.com/q/64957982
    def lpad(n):
      tostring
      | if (n > length) then ((n - length) * "0") + . else . end;

    def bold:
      "<b>\(.)</b>";
    def timeFormat:
      "[\(. / 60 | floor | lpad(2)):\(. % 60 | lpad(2))]";
    def when(flag; filter):
      if flag then filter else . end;
    def format:
      "\(.active as $active | if $active then .clockedMinutes else .totalMinutes end | timeFormat | when($active; bold)) \(.heading)";
    def tooltip:
      "Current clock: \(.clockedMinutes | timeFormat)\nTotal: \(.totalMinutes | timeFormat)";

    . | ({
      text: (if . then format else "<i>Emacs down</i>" end),
      tooltip: (if . then tooltip else "Long live emacs" end),
      class: (if . then (if .active then "active" else "" end) else "missing" end),
      percentage: 0
    })
  '';

  org-clock = writeScript "org-clock" ''
    #!/usr/bin/env bash
    function status() {
      ${eval-elisp} "(my/org-clock-info)" | ${jq}/bin/jq -r 'if . == "" then null else . end'
    }
    if [ $# -eq 0 ]
    then
      status | ${jq}/bin/jq -c --from-file ${org-clock-format-jq}
    else
      case $1 in
        toggle-last-clock)
          ${eval-elisp} "(+org/toggle-last-clock nil)"
          ;;
        recent-clock)
          i=$(\
            status \
            | jq -r ".recent[]" \
            | ${config.programs.rofi.cmd.dmenu "org-clock-recent"} -no-custom -format i)
          [$i = ""] && ${eval-elisp} "(my/org-clock-info-clock-in $i)"
          ;;
      esac
    fi
  '';
in
{
  options = with lib; with types; {
    programs.doom-emacs = {
      eval-elisp = mkOption {
        type = str;
        readOnly = true;
        default = "${config.programs.doom-emacs.package}/bin/emacsclient --no-wait -qa ${pkgs.coreutils}/bin/true --eval";
      };
      org-clock = mkOption {
        type = package;
        readOnly = true;
        default = org-clock;
      };
    };
  };
  config = {
    home.sessionVariables = {
      EDITOR = "em";
    };

    home.file = {
      "${configPath}/${cleanConfigName}".text = cleanConfig;
      "${configPath}/${dirtyConfigName}".source = config.lib.file.mkOutOfStoreSymlink "${import ./path-to-this-directory.nix}/${dirtyConfigName}";

      ".local/bin/em" = {
        text = ''
          #!/usr/bin/env /bin/sh
          emacsclient -c -a "" $@
        '';
        executable = true;
      };

      # https://github.com/Mic92/dotfiles/commit/87d5c4af6ffd190daa92eea153fee77c7cdeb661
      ".tree-sitter".source = (pkgs.runCommand "grammars" {} ''
        mkdir -p $out/bin
        ${lib.concatStringsSep "\n"
          (lib.mapAttrsToList (name: src: "name=${name}; ln -s ${src}/parser $out/bin/\${name#tree-sitter-}.so") pkgs.tree-sitter.builtGrammars)};
      '');
    };

    # A marriage made in hell
    programs.doom-emacs =
      let
        emacs = pkgs.emacs;
        epkgs = pkgs.emacsPackagesFor emacs;
        doomPrivateDir = ./doom;
      in {
        enable = true;
        doomPackageDir =
          let
            filteredPath = builtins.path {
              path = doomPrivateDir;
              name = "doom-private-dir-filtered";
              filter = path: type:
                builtins.elem (baseNameOf path) [ "init.el" "packages.el" ];
            };
          in
            pkgs.linkFarm "doom-packages-dir" [
            {
              name = "init.el";
              path = "${filteredPath}/init.el";
            }
            {
              name = "packages.el";
              path = "${filteredPath}/packages.el";
            }
            {
              name = "config.el";
              path = pkgs.emptyFile;
            }];
        inherit doomPrivateDir;
        extraConfig = ''
        (add-to-list 'load-path "${config.home.homeDirectory}/${configPath}")
        (load "${cleanConfigName}")
        (load "${dirtyConfigName}")
        '';
        extraPackages = [
          epkgs.tsc
          pkgs.cargo
        ];
        emacsPackage = emacs;
        emacsPackagesOverlay = eself: esuper: {
          # irony = esuper.irony.overrideAttrs (_: { doCheck = false; });
          lean4-mode = pkgs.lean4.lean4-mode;
        };
      };

    home.packages = with pkgs; [
      # So emacs can compliment me
      espeak
      ripgrep
      tree-sitter

      nixpkgs-fmt
    ];
  };
}
