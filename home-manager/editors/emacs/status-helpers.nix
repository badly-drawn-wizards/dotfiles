{ config, lib, pkgs, ... }:

let
  inherit (pkgs) jq writeScript symlinkJoin;
  inherit (config.programs.doom-emacs) package elisp-client doomscript extraSiteLisp;
  inherit (extraSiteLisp) site-lisp;

  org-agenda-doomscript = writeScript "org-agenda-doomscript" ''
    #!${package}/bin/doomscript
    (require 'org-utils)
    (my/org-batch-agenda)
  '';

  org-clock-format-jq = writeScript "org-clock-jq" ''
    #!${pkgs.jq}/bin/jq
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
in
{
  options = with lib; {
    programs.doom-emacs = {
      elisp-client = mkOption {
        type = types.str;
        readOnly = true;
        default = "${package}/bin/emacsclient --no-wait -qa ${pkgs.coreutils}/bin/true --eval";
      };
      doomscript = mkOption {
        type = types.str;
        readOnly = true;
        default = ''EMACSLOADPATH="${site-lisp}:$EMACSLOADPATH" ${package}/bin/doomscript'';
      };
      org-clock = mkOption {
        type = types.package;
        readOnly = true;
        default = writeScript "org-clock" ''
          #!${pkgs.bash}/bin/bash
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
      };
      org-agenda = mkOption {
        type = types.package;
        readOnly = true;
        default = writeScript "org-agenda" ''
          #!${pkgs.bash}/bin/bash
          function agenda() {
            ${doomscript} ${org-agenda-doomscript}
          }
          agenda | ${jq}/bin/jq -sRc '{
            text: "",
            tooltip: .,
            class: "agenda",
            percentage: 0
          }'
        '';
      };
    };
  };
}
