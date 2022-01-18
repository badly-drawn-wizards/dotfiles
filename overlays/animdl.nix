self: super:
{
  animdl = super.python3.buildPythonPackage {
    pname = "animdl";
    version = "1.4.19";
    src = super.fetchFromGitHub {
      owner = "justfoolingaround";
      repo = "animdl";
      rev = "52b1bfd9aeefaeb63ec826f9b078665737ab9a53";
      sha256 = "0iw3yw9nlbshnnz0dm38jx60w45jg0q4xkjclz3q28ma707nijk7";
    };
  };
}
