{ stdenv, fira-code, otf2bdf, bdf2psf, ... }:

stdenv.mkDerivation {
  src = fira-code;
  name = "fira-consolefont";
  buildInputs = [ fira-code otf2bdf bdf2psf ];
  BDF_ARGS = ["-p 20" "-r 96" "-c C" ];
  convertToBdf = "${otf2bdf}/bin/otf2bdf $BDF_ARGS -o FiraCode.bdf $src/share/fonts/truetype/FiraCode-VF.ttf";
  roundBdfDown = ''sed -i 's/^\(AVERAGE_WIDTH [0-9]\+\)[0-9]/\10/' FiraCode.bdf'';
  convertToPsf = "${bdf2psf}/bin/bdf2psf --fb FiraCode.bdf ${bdf2psf}/share/bdf2psf/standard.equivalents ${bdf2psf}/share/bdf2psf/fontsets/Uni2.512 512 firacode.psf";
  buildPhase = ''
  eval "$convertToBdf"
  eval "$convertToPsf"
  gzip -k firacode.psf
  '';
  installPhase = ''
  mkdir -p "$out/share/consolefonts"
  cp firacode.psf.gz "$out/share/consolefonts/"
  '';
}
