let pkgs = import <nixpkgs> {};
in pkgs.stdenv.mkDerivation {
  name = "futhark-sml";
  buildInputs = [
    pkgs.mlton
    pkgs.mlkit
    pkgs.smlfmt
    pkgs.smlpkg
  ];
}
