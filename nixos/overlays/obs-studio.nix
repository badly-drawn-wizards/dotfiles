self: super:
{
  obs-studio = super.obs-studio.overrideAttrs (old: {
    # src = super.fetchFromGitHub {
    #   owner = "badly-drawn-wizards";
    #   repo = "obs-studio";
    #   rev = "9efe3ecded505f41086bad9fbb01dd959cdb1484";
    #   sha256 = "18har81mg5vywyv96rcxadwlc8zgs6sj28fkf4y86p1vnm4m1rx6";
    # };
    buildInputs = with self; old.buildInputs ++ [
      wayland
    ];
    postInstall = with self; ''
      wrapProgram $out/bin/obs \
        --prefix "LD_LIBRARY_PATH" : "${xorg.libX11.out}/lib:${vlc}/lib" \
        --set OBS_USE_EGL 1 \
        --set QT_QPA_PLATFORM wayland
    '';
  });
}
