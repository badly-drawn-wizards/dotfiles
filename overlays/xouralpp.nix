self: super: with super; {
  xournalpp = xournalpp.overrideAttrs (attrs: {
    src = fetchFromGitHub {
      owner = "xournalpp";
      repo = "xournalpp";
      rev = "59f4f22a482b99801b286b4c784a8919373b9589";
      sha256 = "07wnw57gx4ks7sibrhv2xaai5g3gbd2d01x5k09d7z6l667h8vif";
    };
  });
}
