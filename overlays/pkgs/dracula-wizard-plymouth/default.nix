{ stdenvNoCC, ... }:

stdenvNoCC.mkDerivation {
  name = "dracula-wizard-plymouth";

  dontUnpack = true;
  dontBuild = true;

  installPhase = ''
    runHook preInstall

    theme=$out/share/plymouth/themes/dracula-wizard
    mkdir -p $theme
    # Same wizard as the GRUB theme
    cp ${../dracula-grub-theme/wizard/logo.png} $theme/logo.png
    cp ${./dracula-wizard.script} $theme/dracula-wizard.script
    substitute ${./dracula-wizard.plymouth} $theme/dracula-wizard.plymouth --subst-var out

    runHook postInstall
  '';
}
