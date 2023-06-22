{ config, lib, pkgs, inputs, ... }:

let
  configPath = ".emacs.d/lisp";
  cleanConfig = ''
  '';
  cleanConfigName = "config.clean.el";
  dirtyConfigName = "config.dirty.el";

  inherit (pkgs) jq writeScript symlinkJoin;
  inherit (config.programs.doom-emacs) elisp-client elisp-batch;
  
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
      ${elisp-client} "(my/org-clock-info)" \
        | ${jq}/bin/jq -sr '.[] // null'
    }
    if [ $# -eq 0 ]
    then
      status | ${jq}/bin/jq -c --from-file ${org-clock-format-jq}
    else
      case $1 in
        toggle-last-clock)
          ${elisp-client} "(+org/toggle-last-clock nil)"
          ;;
        recent-clock)
          i=$(\
            status \
            | ${jq}/bin/jq -r ".recent[]" \
            | ${config.programs.rofi.cmd.dmenu "org-clock-recent"} -no-custom -format i)
          [ -n $i ] && ${elisp-client} "(my/org-clock-info-clock-in $i)"
          ;;
      esac
    fi
  '';

  org-agenda = writeScript "org-agenda" ''
    #!/usr/bin/env bash
    function agenda() {
      ${elisp-batch} '(progn (require (quote org-utils)) (my/org-batch-agenda))'
    }
    agenda | ${jq}/bin/jq -sRc '{
      text: "",
      tooltip: .,
      class: "agenda",
      percentage: 0
    }'
  '';
  dirty-elisp-dir = config.lib.file.mkOutOfStoreSymlink
    "${import ./path-to-this-directory.nix}/lisp";
in
{
  options = with lib; with types; {
    programs.doom-emacs = {
      elisp-client = mkOption {
        type = str;
        readOnly = true;
        default = "${config.programs.doom-emacs.package}/bin/emacsclient --no-wait -qa ${pkgs.coreutils}/bin/true --eval";
      };
      elisp-batch = mkOption {
        type = str;
        readOnly = true;
        default = ''EMACSLOADPATH="${dirty-elisp-dir}:$EMACSLOADPATH" ${config.programs.doom-emacs.package}/bin/emacs --batch -q --eval'';
      };
      org-clock = mkOption {
        type = package;
        readOnly = true;
        default = org-clock;
      };
      org-agenda = mkOption {
        type = package;
        readOnly = true;
        default = org-agenda;
      };
    };
  };
  config = {
    home.sessionVariables = {
      EDITOR = "em";
    };

    home.file = {
      "${configPath}".source = dirty-elisp-dir;
      ".local/bin/em" = {
        text = ''
          #!/usr/bin/env sh
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
        inherit (pkgs) emacs emacsPackagesFor;
        oepkgs = emacsPackagesFor emacs;
        doomPrivateDir = ./doom;

        straightBuild = { pname, src, ... }@args: pkgs.trivialBuild ({
          ename = pname;
          version = "1";
          buildPhase = ":";
        } // args);
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
        (setq doom-font (font-spec :family "${config.fontMono}" :size 16))
        (load "${dirtyConfigName}")
        '';
        extraPackages = [
          pkgs.cargo
        ];
        emacsPackage = emacs;
        emacsPackagesOverlay = eself: esuper: {
          # irony = esuper.irony.overrideAttrs (_: { doCheck = false; });
          inherit (pkgs.lean4) lean4-mode;
          inherit (oepkgs) evil-collection;
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
