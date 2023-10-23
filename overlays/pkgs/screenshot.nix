{ writeScriptBin, grim, slurp, bash }:

writeScriptBin "screenshot" ''
  #!${bash}/bin/bash
  ${grim}/bin/grim -t png -g "$(${slurp}/bin/slurp)" ''${HOME}/screenshots/$(date +%Y-%m-%d_%H-%m-%s).png
''
