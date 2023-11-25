{ lib, fetchzip }:

let
  version = "3.2.0";
in
fetchzip {
  name = "kata-images-${version}";
  url = "https://github.com/kata-containers/kata-containers/releases/download/${version}/kata-static-${version}-amd64.tar.xz";
  sha256 = "sha256-u5BjtAmuS1xfp2DLy1J3wywkwJ/23UCafvhxxj0OnLA=";
  # remove unrelated files
  postFetch = ''
    mv $out/kata/share/kata-containers kata-containers
    rm -r $out
    mkdir -p $out/share
    mv kata-containers $out/share/kata-containers
  '';
}
